/-
  Seascape Demo
  Demonstrates Gerstner waves for realistic ocean simulation with a procedural overcast sky.
  Features interactive FPS camera controls (WASD + mouse look).
-/
import Afferent

open Afferent Afferent.FFI Afferent.Render

namespace Demos

/-- Pi constant for wave calculations. -/
private def pi : Float := 3.14159265358979

/-! ## Gerstner Wave Parameters -/

/-- A single Gerstner wave component. -/
structure GerstnerWave where
  amplitude : Float    -- Wave height
  wavelength : Float   -- Distance between crests
  direction : Float    -- Direction angle in radians
  speed : Float        -- Wave speed multiplier
  deriving Inhabited

/-- Default wave set for moderate ocean conditions. -/
def defaultWaves : Array GerstnerWave := #[
  { amplitude := 0.8, wavelength := 20.0, direction := 0.0, speed := 1.0 },
  { amplitude := 0.5, wavelength := 15.0, direction := pi / 4.0, speed := 0.8 },
  { amplitude := 0.3, wavelength := 10.0, direction := -pi / 6.0, speed := 1.2 },
  { amplitude := 0.2, wavelength := 7.0, direction := pi * 0.39, speed := 1.5 }
]

/-- Compute Gerstner wave displacement for a single point.
    Returns (dx, dy, dz) displacement. -/
def gerstnerDisplacement (waves : Array GerstnerWave) (x z t : Float) : Float × Float × Float :=
  let gravity := 9.8
  waves.foldl (init := (0.0, 0.0, 0.0)) fun (dx, dy, dz) wave =>
    let k := 2.0 * pi / wave.wavelength
    let omega := Float.sqrt (gravity * k)
    let dirX := Float.cos wave.direction
    let dirZ := Float.sin wave.direction
    let phase := k * (dirX * x + dirZ * z) - omega * wave.speed * t
    let cosPhase := Float.cos phase
    let sinPhase := Float.sin phase
    (dx + wave.amplitude * dirX * cosPhase,
     dy + wave.amplitude * sinPhase,
     dz + wave.amplitude * dirZ * cosPhase)

/-! ## Ocean Mesh -/

/-- Ocean mesh state with pre-allocated arrays. -/
structure OceanMesh where
  gridSize : Nat           -- Number of vertices per side
  extent : Float           -- Half-width of the ocean (e.g., 50 means -50 to +50)
  basePositions : Array Float  -- Original XZ positions (2 floats per vertex)
  vertices : Array Float   -- Current displaced vertices (10 floats per vertex)
  indices : Array UInt32   -- Triangle indices
  deriving Inhabited

/-- Create a flat ocean mesh grid. -/
def OceanMesh.create (gridSize : Nat) (extent : Float) : OceanMesh :=
  let numVertices := gridSize * gridSize
  let numQuads := (gridSize - 1) * (gridSize - 1)
  let numTriangles := numQuads * 2
  let numIndices := numTriangles * 3
  let spacing := (extent * 2.0) / (gridSize - 1).toFloat

  -- Generate base positions and initial vertices
  let (basePositions, vertices) := Id.run do
    let mut basePositions := Array.mkEmpty (numVertices * 2)
    let mut vertices := Array.mkEmpty (numVertices * 10)

    for row in [:gridSize] do
      for col in [:gridSize] do
        let x := -extent + col.toFloat * spacing
        let z := -extent + row.toFloat * spacing

        -- Store base position
        basePositions := basePositions.push x
        basePositions := basePositions.push z

        -- Initial vertex: position(3) + normal(3) + color(4)
        -- Position
        vertices := vertices.push x
        vertices := vertices.push 0.0  -- Y = 0 initially
        vertices := vertices.push z
        -- Normal (pointing up initially)
        vertices := vertices.push 0.0
        vertices := vertices.push 1.0
        vertices := vertices.push 0.0
        -- Color (ocean base color)
        vertices := vertices.push 0.20
        vertices := vertices.push 0.35
        vertices := vertices.push 0.40
        vertices := vertices.push 1.0

    return (basePositions, vertices)

  -- Generate indices for triangle strips
  let indices := Id.run do
    let mut indices := Array.mkEmpty numIndices
    for row in [:(gridSize - 1)] do
      for col in [:(gridSize - 1)] do
        let topLeft := (row * gridSize + col).toUInt32
        let topRight := topLeft + 1
        let bottomLeft := ((row + 1) * gridSize + col).toUInt32
        let bottomRight := bottomLeft + 1

        -- First triangle (top-left, bottom-left, top-right)
        indices := indices.push topLeft
        indices := indices.push bottomLeft
        indices := indices.push topRight

        -- Second triangle (top-right, bottom-left, bottom-right)
        indices := indices.push topRight
        indices := indices.push bottomLeft
        indices := indices.push bottomRight

    return indices

  { gridSize, extent, basePositions, vertices, indices }

/-- Create an annular (ring-shaped) ocean mesh for LOD.
    `radialSteps`: number of samples from inner → outer radius
    `angularSteps`: number of samples around the circle
    `innerExtent`: inner radius (hole in the ring)
    `outerExtent`: outer radius -/
def OceanMesh.createRing (radialSteps angularSteps : Nat) (innerExtent outerExtent : Float) : OceanMesh :=
  -- For a ring, we create a grid where radius varies from inner to outer
  -- and angle varies around the full circle.
  let numVertices := radialSteps * angularSteps

  -- Generate base positions and initial vertices
  let (basePositions, vertices) := Id.run do
    let mut basePositions := Array.mkEmpty (numVertices * 2)
    let mut vertices := Array.mkEmpty (numVertices * 10)

    for radialIdx in [:radialSteps] do
      -- Radius from inner to outer
      let t := radialIdx.toFloat / (radialSteps - 1).toFloat
      let radius := innerExtent + t * (outerExtent - innerExtent)

      for angularIdx in [:angularSteps] do
        let angle := 2.0 * pi * angularIdx.toFloat / angularSteps.toFloat
        let x := radius * Float.cos angle
        let z := radius * Float.sin angle

        -- Store base position
        basePositions := basePositions.push x
        basePositions := basePositions.push z

        -- Initial vertex
        vertices := vertices.push x
        vertices := vertices.push 0.0
        vertices := vertices.push z
        -- Normal (pointing up)
        vertices := vertices.push 0.0
        vertices := vertices.push 1.0
        vertices := vertices.push 0.0
        -- Color (ocean base color)
        vertices := vertices.push 0.20
        vertices := vertices.push 0.35
        vertices := vertices.push 0.40
        vertices := vertices.push 1.0

    return (basePositions, vertices)

  -- Generate indices for the ring
  let indices := Id.run do
    let mut indices := Array.mkEmpty ((radialSteps - 1) * angularSteps * 6)
    for radialIdx in [:(radialSteps - 1)] do
      for angularIdx in [:angularSteps] do
        let nextAngular := (angularIdx + 1) % angularSteps
        let topLeft := (radialIdx * angularSteps + angularIdx).toUInt32
        let topRight := (radialIdx * angularSteps + nextAngular).toUInt32
        let bottomLeft := ((radialIdx + 1) * angularSteps + angularIdx).toUInt32
        let bottomRight := ((radialIdx + 1) * angularSteps + nextAngular).toUInt32

        -- Two triangles per quad
        indices := indices.push topLeft
        indices := indices.push bottomLeft
        indices := indices.push topRight

        indices := indices.push topRight
        indices := indices.push bottomLeft
        indices := indices.push bottomRight

    return indices

  { gridSize := radialSteps, extent := outerExtent, basePositions, vertices, indices }

/-- Apply Gerstner waves to the ocean mesh and recompute normals. -/
def OceanMesh.applyWaves (mesh : OceanMesh) (waves : Array GerstnerWave) (t : Float) : OceanMesh :=
  let numVertices := mesh.basePositions.size / 2

  -- First pass: compute displaced positions (x,y,z) for every base (x,z).
  let displacedPositions := Id.run do
    let mut displacedPositions := Array.mkEmpty (numVertices * 3)
    for i in [:numVertices] do
      let baseX := mesh.basePositions.getD (i * 2) 0.0
      let baseZ := mesh.basePositions.getD (i * 2 + 1) 0.0
      let (dx, dy, dz) := gerstnerDisplacement waves baseX baseZ t
      displacedPositions := displacedPositions.push (baseX + dx)
      displacedPositions := displacedPositions.push dy
      displacedPositions := displacedPositions.push (baseZ + dz)
    return displacedPositions

  -- Second pass: accumulate normals from triangles (works for any topology / LOD layout).
  let normalSums := Id.run do
    let mut normalSums := Array.replicate (numVertices * 3) 0.0
    let triCount := mesh.indices.size / 3
    for triIdx in [:triCount] do
      let i0 := (mesh.indices.getD (triIdx * 3) (0 : UInt32)).toNat
      let i1 := (mesh.indices.getD (triIdx * 3 + 1) (0 : UInt32)).toNat
      let i2 := (mesh.indices.getD (triIdx * 3 + 2) (0 : UInt32)).toNat

      let p0x := displacedPositions.getD (i0 * 3) 0.0
      let p0y := displacedPositions.getD (i0 * 3 + 1) 0.0
      let p0z := displacedPositions.getD (i0 * 3 + 2) 0.0

      let p1x := displacedPositions.getD (i1 * 3) 0.0
      let p1y := displacedPositions.getD (i1 * 3 + 1) 0.0
      let p1z := displacedPositions.getD (i1 * 3 + 2) 0.0

      let p2x := displacedPositions.getD (i2 * 3) 0.0
      let p2y := displacedPositions.getD (i2 * 3 + 1) 0.0
      let p2z := displacedPositions.getD (i2 * 3 + 2) 0.0

      let e1x := p1x - p0x
      let e1y := p1y - p0y
      let e1z := p1z - p0z

      let e2x := p2x - p0x
      let e2y := p2y - p0y
      let e2z := p2z - p0z

      -- Face normal (e1 x e2)
      let nx := e1y * e2z - e1z * e2y
      let ny := e1z * e2x - e1x * e2z
      let nz := e1x * e2y - e1y * e2x

      let o0 := i0 * 3
      let o1 := i1 * 3
      let o2 := i2 * 3

      normalSums := normalSums.set! o0 (normalSums.getD o0 0.0 + nx)
      normalSums := normalSums.set! (o0 + 1) (normalSums.getD (o0 + 1) 0.0 + ny)
      normalSums := normalSums.set! (o0 + 2) (normalSums.getD (o0 + 2) 0.0 + nz)

      normalSums := normalSums.set! o1 (normalSums.getD o1 0.0 + nx)
      normalSums := normalSums.set! (o1 + 1) (normalSums.getD (o1 + 1) 0.0 + ny)
      normalSums := normalSums.set! (o1 + 2) (normalSums.getD (o1 + 2) 0.0 + nz)

      normalSums := normalSums.set! o2 (normalSums.getD o2 0.0 + nx)
      normalSums := normalSums.set! (o2 + 1) (normalSums.getD (o2 + 1) 0.0 + ny)
      normalSums := normalSums.set! (o2 + 2) (normalSums.getD (o2 + 2) 0.0 + nz)

    return normalSums

  -- Third pass: normalize normals and update vertex buffer (position, normal, color).
  let vertices := Id.run do
    let mut vertices := mesh.vertices
    for i in [:numVertices] do
      let vertexOffset := i * 10

      let x := displacedPositions.getD (i * 3) 0.0
      let y := displacedPositions.getD (i * 3 + 1) 0.0
      let z := displacedPositions.getD (i * 3 + 2) 0.0

      let nx0 := normalSums.getD (i * 3) 0.0
      let ny0 := normalSums.getD (i * 3 + 1) 0.0
      let nz0 := normalSums.getD (i * 3 + 2) 0.0

      let nLen := Float.sqrt (nx0 * nx0 + ny0 * ny0 + nz0 * nz0)
      let (nx, ny, nz) :=
        if nLen < 0.0001 then
          (0.0, 1.0, 0.0)
        else
          (nx0 / nLen, ny0 / nLen, nz0 / nLen)

      -- Color based on wave height (y displacement)
      let heightFactor := (y + 2.0) / 4.0
      let heightFactor :=
        if heightFactor < 0.0 then 0.0 else if heightFactor > 1.0 then 1.0 else heightFactor

      let r := 0.15 + heightFactor * 0.35
      let g := 0.25 + heightFactor * 0.30
      let b := 0.30 + heightFactor * 0.30

      vertices := vertices.set! vertexOffset x
      vertices := vertices.set! (vertexOffset + 1) y
      vertices := vertices.set! (vertexOffset + 2) z
      vertices := vertices.set! (vertexOffset + 3) nx
      vertices := vertices.set! (vertexOffset + 4) ny
      vertices := vertices.set! (vertexOffset + 5) nz
      vertices := vertices.set! (vertexOffset + 6) r
      vertices := vertices.set! (vertexOffset + 7) g
      vertices := vertices.set! (vertexOffset + 8) b

    return vertices

  { mesh with vertices }

/-! ## Sky Dome -/

/-- Sky dome mesh for procedural overcast sky. -/
structure SkyDome where
  vertices : Array Float   -- 10 floats per vertex
  indices : Array UInt32
  deriving Inhabited

/-- Create a full sky sphere with gradient coloring.
    Upper hemisphere: gradient from zenith to horizon.
    Lower hemisphere: constant horizon color (matches fog). -/
def SkyDome.create (radius : Float) (segments : Nat) (rings : Nat) : SkyDome :=
  -- Total rings: upper hemisphere (rings) + lower hemisphere (rings/2)
  let lowerRings := rings / 2
  let totalRings := rings + lowerRings
  let (vertices, indices) := Id.run do
    let mut vertices := Array.mkEmpty ((segments * totalRings + 2) * 10)
    let mut indices := Array.mkEmpty (segments * totalRings * 6)

    -- Zenith vertex (top of dome)
    vertices := vertices.push 0.0
    vertices := vertices.push radius
    vertices := vertices.push 0.0
    -- Normal (pointing inward)
    vertices := vertices.push 0.0
    vertices := vertices.push (-1.0)
    vertices := vertices.push 0.0
    -- Color (zenith - darker gray)
    vertices := vertices.push 0.35
    vertices := vertices.push 0.38
    vertices := vertices.push 0.42
    vertices := vertices.push 1.0

    -- Generate upper hemisphere rings (zenith to horizon)
    for ring in [:rings] do
      let phi := (pi / 2.0) * (1.0 - (ring + 1).toFloat / rings.toFloat)
      let y := radius * Float.sin phi
      let ringRadius := radius * Float.cos phi

      -- Color gradient from zenith to horizon
      let t := (ring + 1).toFloat / rings.toFloat
      let r := 0.35 + t * 0.20  -- 0.35 to 0.55
      let g := 0.38 + t * 0.20  -- 0.38 to 0.58
      let b := 0.42 + t * 0.20  -- 0.42 to 0.62

      for seg in [:segments] do
        let theta := 2.0 * pi * seg.toFloat / segments.toFloat
        let x := ringRadius * Float.cos theta
        let z := ringRadius * Float.sin theta

        vertices := vertices.push x
        vertices := vertices.push y
        vertices := vertices.push z
        let len := Float.sqrt (x * x + y * y + z * z)
        vertices := vertices.push (-x / len)
        vertices := vertices.push (-y / len)
        vertices := vertices.push (-z / len)
        vertices := vertices.push r
        vertices := vertices.push g
        vertices := vertices.push b
        vertices := vertices.push 1.0

    -- Generate lower hemisphere rings (horizon downward, constant color)
    for ring in [:lowerRings] do
      let phi := -(pi / 2.0) * (ring + 1).toFloat / lowerRings.toFloat  -- 0 to -pi/2
      let y := radius * Float.sin phi
      let ringRadius := radius * Float.cos phi

      -- Constant horizon color (matches fog)
      let r := 0.55
      let g := 0.58
      let b := 0.62

      for seg in [:segments] do
        let theta := 2.0 * pi * seg.toFloat / segments.toFloat
        let x := ringRadius * Float.cos theta
        let z := ringRadius * Float.sin theta

        vertices := vertices.push x
        vertices := vertices.push y
        vertices := vertices.push z
        let len := Float.sqrt (x * x + y * y + z * z)
        vertices := vertices.push (-x / len)
        vertices := vertices.push (-y / len)
        vertices := vertices.push (-z / len)
        vertices := vertices.push r
        vertices := vertices.push g
        vertices := vertices.push b
        vertices := vertices.push 1.0

    -- Nadir vertex (bottom of sphere)
    vertices := vertices.push 0.0
    vertices := vertices.push (-radius)
    vertices := vertices.push 0.0
    vertices := vertices.push 0.0
    vertices := vertices.push 1.0
    vertices := vertices.push 0.0
    vertices := vertices.push 0.55
    vertices := vertices.push 0.58
    vertices := vertices.push 0.62
    vertices := vertices.push 1.0

    let nadirIdx := (1 + totalRings * segments).toUInt32

    -- Generate indices
    -- Connect zenith to first ring
    for seg in [:segments] do
      let next := (seg + 1) % segments
      indices := indices.push 0
      indices := indices.push (seg + 1).toUInt32
      indices := indices.push (next + 1).toUInt32

    -- Connect all rings (upper + lower)
    for ring in [:(totalRings - 1)] do
      let ringStart := 1 + ring * segments
      let nextRingStart := ringStart + segments
      for seg in [:segments] do
        let next := (seg + 1) % segments
        let tl := (ringStart + seg).toUInt32
        let tr := (ringStart + next).toUInt32
        let bl := (nextRingStart + seg).toUInt32
        let br := (nextRingStart + next).toUInt32

        indices := indices.push tl
        indices := indices.push bl
        indices := indices.push tr

        indices := indices.push tr
        indices := indices.push bl
        indices := indices.push br

    -- Connect last ring to nadir
    let lastRingStart := 1 + (totalRings - 1) * segments
    for seg in [:segments] do
      let next := (seg + 1) % segments
      indices := indices.push (lastRingStart + seg).toUInt32
      indices := indices.push nadirIdx
      indices := indices.push (lastRingStart + next).toUInt32

    return (vertices, indices)

  { vertices, indices }

/-! ## Seascape Rendering -/

/-- Fog parameters for the seascape. -/
structure FogParams where
  color : Array Float     -- RGB color (3 floats)
  start : Float           -- Distance where fog begins
  endDist : Float         -- Distance where fog is fully opaque
  deriving Inhabited

/-- Default fog parameters for infinite ocean effect.
    Fog color exactly matches sky horizon for seamless blend. -/
def defaultFog : FogParams :=
  { color := #[0.55, 0.58, 0.62]  -- Exactly match sky horizon color
  , start := 80.0                  -- Fog begins at moderate distance
  , endDist := 350.0 }             -- Fully fogged before mesh edge at 500

/-- Render the seascape with the given camera.
    t: elapsed time in seconds
    renderer: FFI renderer
    screenWidth/screenHeight: for aspect ratio
    camera: FPS camera state -/
def renderSeascape (renderer : Renderer) (t : Float)
    (screenWidth screenHeight : Float) (camera : FPSCamera) : IO Unit := do
  let aspect := screenWidth / screenHeight
  let fovY := pi / 3.0  -- 60 degrees for wide ocean vista
  let proj := Matrix4.perspective fovY aspect 0.1 1000.0
  let view := camera.viewMatrix

  -- Light direction (from above-left, softer for overcast)
  let lx := -0.3
  let ly := 0.7
  let lz := -0.5
  let len := Float.sqrt (lx * lx + ly * ly + lz * lz)
  let lightDir := #[lx / len, ly / len, lz / len]
  let ambient := 0.5  -- Higher ambient for overcast lighting

  -- Camera position for fog calculation
  let cameraPos := #[camera.x, camera.y, camera.z]

  -- Fog parameters
  let fog := defaultFog

  -- Create and render sky dome (large, centered on camera)
  let skyDome := SkyDome.create 600.0 32 16

  -- Sky model matrix - translate to camera position
  let skyModel := Matrix4.translate camera.x camera.y camera.z
  let skyMvp := Matrix4.multiply proj (Matrix4.multiply view skyModel)

  -- Render sky first (it's at far distance) - no fog for sky
  Renderer.drawMesh3D renderer
    skyDome.vertices
    skyDome.indices
    skyMvp.toArray
    skyModel.toArray
    lightDir
    1.0  -- Full ambient for sky (no directional lighting)

  -- Ocean model matrix (identity - ocean is at world origin)
  let model := Matrix4.identity
  let mvp := Matrix4.multiply proj (Matrix4.multiply view model)

  -- Near ocean mesh (high detail disc, matches ring seam at radius 50)
  let nearMesh := OceanMesh.createRing 64 64 0.0 50.0
  let nearMesh := nearMesh.applyWaves defaultWaves t
  Renderer.drawMesh3DWithFog renderer
    nearMesh.vertices
    nearMesh.indices
    mvp.toArray
    model.toArray
    lightDir
    ambient
    cameraPos
    fog.color
    fog.start
    fog.endDist

  -- Mid ring (medium detail: fewer radial steps, same angular steps for crack-free seam)
  let midMesh := OceanMesh.createRing 32 64 50.0 150.0
  let midMesh := midMesh.applyWaves defaultWaves t
  Renderer.drawMesh3DWithFog renderer
    midMesh.vertices
    midMesh.indices
    mvp.toArray
    model.toArray
    lightDir
    ambient
    cameraPos
    fog.color
    fog.start
    fog.endDist

  -- Far ring (low detail: even fewer radial steps, same angular steps for crack-free seam)
  let farMesh := OceanMesh.createRing 16 64 150.0 500.0
  let farMesh := farMesh.applyWaves defaultWaves t
  Renderer.drawMesh3DWithFog renderer
    farMesh.vertices
    farMesh.indices
    mvp.toArray
    model.toArray
    lightDir
    ambient
    cameraPos
    fog.color
    fog.start
    fog.endDist

/-- Create initial FPS camera for seascape viewing.
    Positioned above and behind the ocean, looking forward. -/
def seascapeCamera : FPSCamera :=
  { x := 0.0
  , y := 8.0
  , z := 30.0
  , yaw := pi  -- Facing negative Z (into the ocean)
  , pitch := -0.15   -- Slightly angled down
  , moveSpeed := 10.0
  , lookSensitivity := 0.003 }

end Demos

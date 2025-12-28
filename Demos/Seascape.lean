/-
  Seascape Demo
  Demonstrates Gerstner waves for realistic ocean simulation with a procedural overcast sky.
  Features interactive FPS camera controls (WASD + mouse look).
-/
import Afferent
import Assimptor

open Afferent Afferent.FFI Afferent.Render Assimptor
open Linalg

namespace Demos

-- Use Float.pi from Linalg.Core

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
  { amplitude := 0.5, wavelength := 15.0, direction := Float.pi / 4.0, speed := 0.8 },
  { amplitude := 0.3, wavelength := 10.0, direction := -Float.pi / 6.0, speed := 1.2 },
  { amplitude := 0.2, wavelength := 7.0, direction := Float.pi * 0.39, speed := 1.5 }
]

/-- Precomputed wave constants to reduce per-vertex work. -/
private structure PreparedWave where
  amplitude : Float
  dirX : Float
  dirZ : Float
  k : Float
  omegaSpeed : Float
  ak : Float
  deriving Inhabited

private def prepareWaves (waves : Array GerstnerWave) : Array PreparedWave :=
  let gravity := 9.8
  waves.map fun w =>
    let k := Float.twoPi / w.wavelength
    let omega := Float.sqrt (gravity * k)
    let dirX := Float.cos w.direction
    let dirZ := Float.sin w.direction
    { amplitude := w.amplitude
      dirX
      dirZ
      k
      omegaSpeed := omega * w.speed
      ak := w.amplitude * k }

private def defaultWavesGpuParams : Array Float :=
  let prepared := prepareWaves defaultWaves
  Id.run do
    let mut a := Array.mkEmpty (4 * 4)
    let mut b := Array.mkEmpty (4 * 4)
    for i in [:4] do
      let w := prepared.getD i default
      -- waveA: (dirX, dirZ, k, omegaSpeed)
      a := a.push w.dirX
      a := a.push w.dirZ
      a := a.push w.k
      a := a.push w.omegaSpeed
      -- waveB: (amplitude, ak, 0, 0)
      b := b.push w.amplitude
      b := b.push w.ak
      b := b.push 0.0
      b := b.push 0.0
    return a ++ b

/-- Compute Gerstner wave displacement for a single point.
    Returns (dx, dy, dz) displacement. -/
def gerstnerDisplacement (waves : Array GerstnerWave) (x z t : Float) : Float × Float × Float :=
  let gravity := 9.8
  waves.foldl (init := (0.0, 0.0, 0.0)) fun (dx, dy, dz) wave =>
    let k := Float.twoPi / wave.wavelength
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
        let angle := Float.twoPi * angularIdx.toFloat / angularSteps.toFloat
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
  let prepared := prepareWaves waves
  let numVertices := mesh.basePositions.size / 2
  let vertices := Id.run do
    let mut vertices := Array.mkEmpty (numVertices * 10)

    for i in [:numVertices] do
      let baseX := mesh.basePositions.getD (i * 2) 0.0
      let baseZ := mesh.basePositions.getD (i * 2 + 1) 0.0

      -- Displacement + partials for analytic normal (no per-triangle accumulation).
      let mut dx := 0.0
      let mut dy := 0.0
      let mut dz := 0.0
      let mut sx := 0.0
      let mut sz := 0.0
      let mut sxx := 0.0
      let mut szz := 0.0
      let mut sxz := 0.0
      for w in prepared do
        let phase := w.k * (w.dirX * baseX + w.dirZ * baseZ) - w.omegaSpeed * t
        let cosPhase := Float.cos phase
        let sinPhase := Float.sin phase
        dx := dx + w.amplitude * w.dirX * cosPhase
        dy := dy + w.amplitude * sinPhase
        dz := dz + w.amplitude * w.dirZ * cosPhase

        sx := sx + w.ak * w.dirX * cosPhase
        sz := sz + w.ak * w.dirZ * cosPhase
        sxx := sxx + w.ak * w.dirX * w.dirX * sinPhase
        szz := szz + w.ak * w.dirZ * w.dirZ * sinPhase
        sxz := sxz + w.ak * w.dirX * w.dirZ * sinPhase

      let x := baseX + dx
      let y := dy
      let z := baseZ + dz

      let dPdxX := 1.0 - sxx
      let dPdxY := sx
      let dPdxZ := -sxz

      let dPdzX := -sxz
      let dPdzY := sz
      let dPdzZ := 1.0 - szz

      let dPdz := Vec3.mk dPdzX dPdzY dPdzZ
      let dPdx := Vec3.mk dPdxX dPdxY dPdxZ
      let normal := (dPdz.cross dPdx).normalize
      let (nx, ny, nz) :=
        if normal.length < 0.000001 then
          (0.0, 1.0, 0.0)
        else
          (normal.x, normal.y, normal.z)

      -- Color based on wave height (y displacement)
      let heightFactor := (y + 2.0) / 4.0
      let heightFactor :=
        if heightFactor < 0.0 then 0.0 else if heightFactor > 1.0 then 1.0 else heightFactor

      let r := 0.15 + heightFactor * 0.35
      let g := 0.25 + heightFactor * 0.30
      let b := 0.30 + heightFactor * 0.30

      vertices := vertices.push x
      vertices := vertices.push y
      vertices := vertices.push z
      vertices := vertices.push nx
      vertices := vertices.push ny
      vertices := vertices.push nz
      vertices := vertices.push r
      vertices := vertices.push g
      vertices := vertices.push b
      vertices := vertices.push 1.0

    return vertices

  { mesh with vertices }

/-! ## Projected Grid Ocean -/

/-- Create an ocean mesh using a projected grid (screen-space grid projected onto y=0 plane).
    This keeps vertex density high near the camera and low toward the horizon. -/
def OceanMesh.createProjectedGrid (gridSize : Nat) (fovY aspect : Float)
    (camera : FPSCamera) (maxDistance snapSize overscanNdc : Float) (indices : Array UInt32) : OceanMesh :=
  let numVertices := gridSize * gridSize

  let overscanNdc := if overscanNdc < 0.0 then 0.0 else overscanNdc

  -- Snap the grid in world XZ to reduce "swimming" as the camera translates.
  let snapEps := 0.00001
  let useSnap := snapSize > snapEps
  let originX := if useSnap then Float.floor (camera.x / snapSize) * snapSize else camera.x
  let originZ := if useSnap then Float.floor (camera.z / snapSize) * snapSize else camera.z

  -- Camera basis (world space)
  let cosPitch := Float.cos camera.pitch
  let sinPitch := Float.sin camera.pitch
  let cosYaw := Float.cos camera.yaw
  let sinYaw := Float.sin camera.yaw

  let fwd := Vec3.mk (cosPitch * sinYaw) sinPitch (-cosPitch * cosYaw)

  let right := (fwd.cross Vec3.unitY).normalize
  let up := (right.cross fwd).normalize
  let (rightX, rightY, rightZ) := (right.x, right.y, right.z)
  let (upX, upY, upZ) := (up.x, up.y, up.z)
  let (fwdX, fwdY, fwdZ) := (fwd.x, fwd.y, fwd.z)

  -- Projection parameters (camera-space rays)
  let tanHalfFovY := Float.tan (fovY / 2.0)
  let tanHalfFovX := tanHalfFovY * aspect

  -- Restrict the grid to below the horizon so we don't generate a hard "cap" at `maxDistance`
  -- near the upper corners of the viewport.
  let eps := 0.00001
  let horizonSy := if Float.abs upY < eps then 0.0 else (-fwdY) / upY
  let horizonNdcY := horizonSy / tanHalfFovY
  let horizonMargin := 0.05
  let ndcBottom := -1.0 - overscanNdc
  let ndcTop0 := horizonNdcY - horizonMargin
  let ndcTop :=
    if ndcTop0 < ndcBottom then ndcBottom
    else if ndcTop0 > 1.0 + overscanNdc then 1.0 + overscanNdc
    else ndcTop0
  let ndcLeft := -1.0 - overscanNdc
  let ndcRight := 1.0 + overscanNdc

  let basePositions := Id.run do
    let mut basePositions := Array.mkEmpty (numVertices * 2)

    let denom := (gridSize - 1).toFloat
    for row in [:gridSize] do
      for col in [:gridSize] do
        -- NDC coordinates [-1, 1]
        let ndcX := ndcLeft + (col.toFloat / denom) * (ndcRight - ndcLeft)
        let ndcY := ndcTop - (row.toFloat / denom) * (ndcTop - ndcBottom)

        -- World ray direction through this screen sample
        let sx := ndcX * tanHalfFovX
        let sy := ndcY * tanHalfFovY
        let dirX0 := rightX * sx + upX * sy + fwdX
        let dirY0 := rightY * sx + upY * sy + fwdY
        let dirZ0 := rightZ * sx + upZ * sy + fwdZ
        -- No need to normalize: plane intersection uses the ray parameter t = -originY/dirY.
        let dirX := dirX0
        let dirY := dirY0
        let dirZ := dirZ0

        -- Intersect ray with ocean plane y=0. Clamp to a max distance for stability.
        let t :=
          if Float.abs dirY < eps then
            maxDistance
          else
            (-camera.y) / dirY
        let t :=
          if t < 0.0 then maxDistance else if t > maxDistance then maxDistance else t

        let x := originX + dirX * t
        let z := originZ + dirZ * t

        basePositions := basePositions.push x
        basePositions := basePositions.push z
    return basePositions

  { gridSize, extent := maxDistance, basePositions, vertices := #[], indices }

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
      let phi := Float.halfPi * (1.0 - (ring + 1).toFloat / rings.toFloat)
      let y := radius * Float.sin phi
      let ringRadius := radius * Float.cos phi

      -- Color gradient from zenith to horizon
      let t := (ring + 1).toFloat / rings.toFloat
      let r := 0.35 + t * 0.20  -- 0.35 to 0.55
      let g := 0.38 + t * 0.20  -- 0.38 to 0.58
      let b := 0.42 + t * 0.20  -- 0.42 to 0.62

      for seg in [:segments] do
        let theta := Float.twoPi * seg.toFloat / segments.toFloat
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
      let phi := -Float.halfPi * (ring + 1).toFloat / lowerRings.toFloat  -- 0 to -pi/2
      let y := radius * Float.sin phi
      let ringRadius := radius * Float.cos phi

      -- Constant horizon color (matches fog)
      let r := 0.55
      let g := 0.58
      let b := 0.62

      for seg in [:segments] do
        let theta := Float.twoPi * seg.toFloat / segments.toFloat
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

private initialize seascapeSkyDomeCache : IO.Ref (Option SkyDome) ← IO.mkRef none

private def getSeascapeSkyDome : IO SkyDome := do
  match (← seascapeSkyDomeCache.get) with
  | some dome => return dome
  | none =>
      let dome := SkyDome.create 600.0 32 16
      seascapeSkyDomeCache.set (some dome)
      return dome

/-! ## Frigate Ship Asset -/

/-- Cached frigate asset data (loaded once). -/
private structure FrigateCache where
  asset : LoadedAsset
  texture : Texture

private initialize seascapeFrigateCache : IO.Ref (Option FrigateCache) ← IO.mkRef none

/-- Load the frigate ship asset (cached). -/
def loadFrigate : IO FrigateCache := do
  match (← seascapeFrigateCache.get) with
  | some cache => return cache
  | none =>
      -- Load the FBX model
      let asset ← loadAsset
        "assets/fictional-frigate/source/frigateUn1.fbx"
        "assets/fictional-frigate/textures"

      -- Load the base color texture
      let texturePath := if h : 0 < asset.texturePaths.size then
          s!"assets/fictional-frigate/textures/{asset.texturePaths[0]}"
        else
          "assets/fictional-frigate/textures/frigate6_lambert2_BaseColor.png"
      let texture ← Texture.load texturePath

      let cache := { asset, texture }
      seascapeFrigateCache.set (some cache)
      IO.println s!"Frigate loaded: {asset.vertices.size / 12} vertices, {asset.indices.size / 3} triangles"
      return cache

/-! ## Seascape Rendering -/

/-- Fog parameters for the seascape. -/
structure FogParams where
  color : Array Float     -- RGB color (3 floats)
  start : Float           -- Distance where fog begins
  endDist : Float         -- Distance where fog is fully opaque
  deriving Inhabited

private structure ProjectedGridIndexCache where
  gridSize : Nat
  indices : Array UInt32
  deriving Inhabited

private initialize seascapeProjectedGridIndexCache : IO.Ref (Option ProjectedGridIndexCache) ← IO.mkRef none

private def buildGridIndices (gridSize : Nat) : Array UInt32 :=
  let numQuads := (gridSize - 1) * (gridSize - 1)
  let numIndices := (numQuads * 2 * 3)
  Id.run do
    let mut indices := Array.mkEmpty numIndices
    for row in [:(gridSize - 1)] do
      for col in [:(gridSize - 1)] do
        let topLeft := (row * gridSize + col).toUInt32
        let topRight := topLeft + 1
        let bottomLeft := ((row + 1) * gridSize + col).toUInt32
        let bottomRight := bottomLeft + 1

        indices := indices.push topLeft
        indices := indices.push bottomLeft
        indices := indices.push topRight

        indices := indices.push topRight
        indices := indices.push bottomLeft
        indices := indices.push bottomRight
    return indices

private def getProjectedGridIndices (gridSize : Nat) : IO (Array UInt32) := do
  match (← seascapeProjectedGridIndexCache.get) with
  | some cached =>
      if cached.gridSize == gridSize then
        return cached.indices
      else
        let indices := buildGridIndices gridSize
        seascapeProjectedGridIndexCache.set (some { gridSize, indices })
        return indices
  | none =>
      let indices := buildGridIndices gridSize
      seascapeProjectedGridIndexCache.set (some { gridSize, indices })
      return indices

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
  let fovY := Float.pi / 3.0  -- 60 degrees for wide ocean vista
  let proj := Mat4.perspective fovY aspect 0.1 1000.0
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

  -- Create and render sky dome (cached; large, centered on camera)
  let skyDome ← getSeascapeSkyDome

  -- Sky model matrix - translate to camera position
  let skyModel := Mat4.translation camera.x camera.y camera.z
  let skyMvp := proj * view * skyModel

  -- Render sky first (it's at far distance) - no fog for sky
  Renderer.drawMesh3D renderer
    skyDome.vertices
    skyDome.indices
    skyMvp.toArray
    skyModel.toArray
    lightDir
    1.0  -- Full ambient for sky (no directional lighting)

  -- Ocean model matrix (identity - ocean is at world origin)
  let model := Mat4.identity
  let mvp := proj * view * model

  -- Ocean via GPU projected grid + GPU Gerstner waves (fast path).
  -- `maxDistance` should extend past fog end distance so the edge stays hidden.
  Renderer.drawOceanProjectedGridWithFog renderer
    128
    mvp.toArray
    model.toArray
    lightDir
    ambient
    cameraPos
    fog.color
    fog.start
    fog.endDist
    t
    fovY
    aspect
    800.0  -- maxDistance
    0.0    -- snapSize (disable snapping to avoid visible "pops" while moving)
    0.25   -- overscanNdc
    0.05   -- horizonMargin
    camera.yaw
    camera.pitch
    defaultWavesGpuParams

  -- Render the frigate ship, bobbing with the waves
  let frigate ← loadFrigate

  -- Frigate base position (in front of the starting camera)
  let frigateBaseX := 0.0
  let frigateBaseZ := -30.0  -- In front of camera (camera faces -Z)

  -- Apply Gerstner wave displacement to make the ship bob with the ocean
  let (dx, dy, dz) := gerstnerDisplacement defaultWaves frigateBaseX frigateBaseZ t

  -- Build model matrix: translate to wave position, then scale
  -- FBX models often need scaling (try different scales if needed)
  let frigateScale := 0.02  -- Scale down the model (adjust as needed)
  let frigateY := dy - 1.0  -- Offset to sit at water level (adjust based on model)

  -- Model matrix: first scale, then translate
  let scaleMatrix := Mat4.scaling frigateScale frigateScale frigateScale
  let translateMatrix := Mat4.translation (frigateBaseX + dx) frigateY (frigateBaseZ + dz)
  let frigateModel := translateMatrix * scaleMatrix
  let frigateMvp := proj * view * frigateModel

  -- Draw each submesh of the frigate
  for submesh in frigate.asset.subMeshes do
    Renderer.drawMesh3DTextured renderer
      frigate.asset.vertices
      frigate.asset.indices
      submesh.indexOffset
      submesh.indexCount
      frigateMvp.toArray
      frigateModel.toArray
      lightDir
      ambient
      cameraPos
      fog.color
      fog.start
      fog.endDist
      frigate.texture

/-- Create initial FPS camera for seascape viewing.
    Positioned above and behind the ocean, looking forward. -/
def seascapeCamera : FPSCamera :=
  { x := 0.0
  , y := 8.0
  , z := 30.0
  , yaw := Float.pi  -- Facing negative Z (into the ocean)
  , pitch := -0.15   -- Slightly angled down
  , moveSpeed := 10.0
  , lookSensitivity := 0.003 }

end Demos

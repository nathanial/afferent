/-
  Afferent Dynamic Rendering Module

  Generic dynamic shape rendering where:
  - CPU controls: position, size, rotation, base hue
  - GPU computes: HSV→RGB, pixel→NDC conversion, time-based color animation

  This pattern cuts bandwidth in half (4-5 floats vs 8) while giving full CPU control.

  Data formats:
  - Circle: [x, y, hue, radius] × count (4 floats)
  - Rect:   [x, y, hue, halfSize, rotation] × count (5 floats)
  - Triangle: [x, y, hue, halfSize, rotation] × count (5 floats)
-/

import Afferent.FFI.Metal

namespace Afferent.Render.Dynamic

/-! ## Generic Dynamic Particle Data

These structures hold particle state that can be updated by CPU each frame.
The GPU then handles color computation and coordinate conversion. -/

/-- Generic particle data for dynamic rendering.
    Each particle has position, velocity, and a base hue. -/
structure ParticleState where
  /-- Per-particle: x, y, vx, vy, hueBase (5 floats) -/
  data : Array Float
  /-- Number of particles -/
  count : Nat
  /-- Screen bounds for collision detection -/
  screenWidth : Float
  screenHeight : Float
  deriving Inhabited

/-- Create initial particle state with random positions and velocities. -/
def ParticleState.create (count : Nat) (screenWidth screenHeight : Float) (seed : Nat) : ParticleState :=
  let data := Id.run do
    let mut arr := Array.mkEmpty (count * 5)
    let mut s := seed
    for i in [:count] do
      -- Simple LCG for deterministic randomness
      s := (s * 1103515245 + 12345) % (2^31)
      let x := (s.toFloat / 2147483648.0) * screenWidth
      s := (s * 1103515245 + 12345) % (2^31)
      let y := (s.toFloat / 2147483648.0) * screenHeight
      s := (s * 1103515245 + 12345) % (2^31)
      let vx := (s.toFloat / 2147483648.0 - 0.5) * 400.0
      s := (s * 1103515245 + 12345) % (2^31)
      let vy := (s.toFloat / 2147483648.0 - 0.5) * 400.0
      let hue := i.toFloat / count.toFloat
      arr := arr.push x
      arr := arr.push y
      arr := arr.push vx
      arr := arr.push vy
      arr := arr.push hue
    arr
  { data, count, screenWidth, screenHeight }

/-- Update particle positions with simple bouncing physics. -/
def ParticleState.updateBouncing (p : ParticleState) (dt : Float) (shapeRadius : Float) : ParticleState :=
  let data := Id.run do
    let mut arr := p.data
    for i in [:p.count] do
      let base := i * 5
      let x := arr[base]!
      let y := arr[base + 1]!
      let vx := arr[base + 2]!
      let vy := arr[base + 3]!

      -- Update position
      let x' := x + vx * dt
      let y' := y + vy * dt

      -- Bounce off walls
      let (x'', vx') :=
        if x' < shapeRadius then (shapeRadius, -vx)
        else if x' > p.screenWidth - shapeRadius then (p.screenWidth - shapeRadius, -vx)
        else (x', vx)
      let (y'', vy') :=
        if y' < shapeRadius then (shapeRadius, -vy)
        else if y' > p.screenHeight - shapeRadius then (p.screenHeight - shapeRadius, -vy)
        else (y', vy)

      arr := arr.set! base x''
      arr := arr.set! (base + 1) y''
      arr := arr.set! (base + 2) vx'
      arr := arr.set! (base + 3) vy'
    arr
  { p with data }

/-- Create particles in a grid layout with zero velocity. -/
def ParticleState.createGrid (cols rows : Nat) (startX startY spacing : Float)
    (screenWidth screenHeight : Float) : ParticleState :=
  let count := cols * rows
  let data := Id.run do
    let mut arr := Array.mkEmpty (count * 5)
    for row in [:rows] do
      for col in [:cols] do
        let x := startX + col.toFloat * spacing
        let y := startY + row.toFloat * spacing
        let hue := (row * cols + col).toFloat / count.toFloat
        arr := arr.push x
        arr := arr.push y
        arr := arr.push 0.0  -- vx (unused for grid)
        arr := arr.push 0.0  -- vy (unused for grid)
        arr := arr.push hue
    arr
  { data, count, screenWidth, screenHeight }

/-! ## Data Builders

These functions build the packed float arrays that get sent to the GPU.
They extract position and hue from ParticleState and pack it into the
format expected by each shader. -/

/-- Build dynamic circle data from particle state.
    Format: [pixelX, pixelY, hueBase, radiusPixels] × count (4 floats per circle) -/
def buildCircleData (particles : ParticleState) (radius : Float) : Array Float := Id.run do
  let mut data := Array.mkEmpty (particles.count * 4)
  for i in [:particles.count] do
    let base := i * 5
    let x := particles.data[base]!
    let y := particles.data[base + 1]!
    let hue := particles.data[base + 4]!
    data := data.push x
    data := data.push y
    data := data.push hue
    data := data.push radius
  data

/-- Build dynamic rect data from particle state.
    Format: [pixelX, pixelY, hueBase, halfSizePixels, rotation] × count (5 floats per rect)
    Rotation can be time-based or per-particle stored. -/
def buildRectData (particles : ParticleState) (halfSize : Float) (getRotation : Nat → Float) : Array Float := Id.run do
  let mut data := Array.mkEmpty (particles.count * 5)
  for i in [:particles.count] do
    let base := i * 5
    let x := particles.data[base]!
    let y := particles.data[base + 1]!
    let hue := particles.data[base + 4]!
    data := data.push x
    data := data.push y
    data := data.push hue
    data := data.push halfSize
    data := data.push (getRotation i)
  data

/-- Build dynamic rect data with uniform rotation for all particles. -/
def buildRectDataUniform (particles : ParticleState) (halfSize rotation : Float) : Array Float :=
  buildRectData particles halfSize (fun _ => rotation)

/-- Build dynamic rect data with time-based per-particle rotation. -/
def buildRectDataAnimated (particles : ParticleState) (halfSize t spinSpeed : Float) : Array Float :=
  buildRectData particles halfSize (fun i =>
    let hue := particles.data[i * 5 + 4]!
    t * spinSpeed + hue * 6.28)

/-- Build dynamic triangle data from particle state.
    Format: [pixelX, pixelY, hueBase, halfSizePixels, rotation] × count (5 floats per triangle) -/
def buildTriangleData (particles : ParticleState) (halfSize : Float) (getRotation : Nat → Float) : Array Float := Id.run do
  let mut data := Array.mkEmpty (particles.count * 5)
  for i in [:particles.count] do
    let base := i * 5
    let x := particles.data[base]!
    let y := particles.data[base + 1]!
    let hue := particles.data[base + 4]!
    data := data.push x
    data := data.push y
    data := data.push hue
    data := data.push halfSize
    data := data.push (getRotation i)
  data

/-- Build dynamic triangle data with uniform rotation. -/
def buildTriangleDataUniform (particles : ParticleState) (halfSize rotation : Float) : Array Float :=
  buildTriangleData particles halfSize (fun _ => rotation)

/-- Build dynamic triangle data with time-based per-particle rotation. -/
def buildTriangleDataAnimated (particles : ParticleState) (halfSize t spinSpeed : Float) : Array Float :=
  buildTriangleData particles halfSize (fun i =>
    let hue := particles.data[i * 5 + 4]!
    t * spinSpeed + hue * 6.28)

/-! ## Draw Functions

These wrap the FFI calls with a cleaner interface.
Canvas width/height are the logical (not physical) dimensions for coordinate conversion. -/

/-- Draw dynamic circles. GPU computes color + NDC conversion. -/
def drawCircles (renderer : FFI.Renderer) (particles : ParticleState) (radius t : Float) : IO Unit := do
  let data := buildCircleData particles radius
  FFI.Renderer.drawDynamicCircles renderer data particles.count.toUInt32 t particles.screenWidth particles.screenHeight

/-- Draw dynamic rects with time-based rotation. GPU computes color + NDC. -/
def drawRectsAnimated (renderer : FFI.Renderer) (particles : ParticleState)
    (halfSize t spinSpeed : Float) : IO Unit := do
  let data := buildRectDataAnimated particles halfSize t spinSpeed
  FFI.Renderer.drawDynamicRects renderer data particles.count.toUInt32 t particles.screenWidth particles.screenHeight

/-- Draw dynamic rects with uniform rotation. GPU computes color + NDC. -/
def drawRectsUniform (renderer : FFI.Renderer) (particles : ParticleState)
    (halfSize rotation t : Float) : IO Unit := do
  let data := buildRectDataUniform particles halfSize rotation
  FFI.Renderer.drawDynamicRects renderer data particles.count.toUInt32 t particles.screenWidth particles.screenHeight

/-- Draw dynamic triangles with time-based rotation. GPU computes color + NDC. -/
def drawTrianglesAnimated (renderer : FFI.Renderer) (particles : ParticleState)
    (halfSize t spinSpeed : Float) : IO Unit := do
  let data := buildTriangleDataAnimated particles halfSize t spinSpeed
  FFI.Renderer.drawDynamicTriangles renderer data particles.count.toUInt32 t particles.screenWidth particles.screenHeight

/-- Draw dynamic triangles with uniform rotation. GPU computes color + NDC. -/
def drawTrianglesUniform (renderer : FFI.Renderer) (particles : ParticleState)
    (halfSize rotation t : Float) : IO Unit := do
  let data := buildTriangleDataUniform particles halfSize rotation
  FFI.Renderer.drawDynamicTriangles renderer data particles.count.toUInt32 t particles.screenWidth particles.screenHeight

end Afferent.Render.Dynamic

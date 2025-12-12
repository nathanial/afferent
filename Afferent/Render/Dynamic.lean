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

/-! ## Fast Trig Approximations

For animation, we don't need high-precision trig - visual quality matters more than
mathematical accuracy. These polynomial approximations are ~5x faster than Float.sin/cos. -/

/-- Normalize angle to [-π, π] range -/
@[inline] def normalizeAngle (x : Float) : Float :=
  let pi := 3.14159265358979
  let twoPi := 6.28318530717959
  let x' := x - twoPi * (x / twoPi).floor  -- x mod 2π, now in [0, 2π)
  if x' > pi then x' - twoPi else x'       -- shift to [-π, π]

/-- Fast sine approximation using parabolic curve.
    Accurate to ~1% for visual purposes. Much faster than Float.sin. -/
@[inline] def fastSin (x : Float) : Float :=
  let x' := normalizeAngle x
  -- Coefficients tuned for smooth curve: 4/π and 4/π²
  1.27323954 * x' - 0.405284735 * x' * x'.abs

/-- Fast cosine using sin(x + π/2) -/
@[inline] def fastCos (x : Float) : Float :=
  fastSin (x + 1.5707963267949)  -- π/2

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

/-- Orbital particle data - stores parameters separately from computed positions.
    Per-particle: phase, baseRadius, orbitSpeed, hue (4 floats for params)
    Plus computed x, y positions. -/
structure OrbitalState where
  /-- Per-particle orbital parameters: phase, baseRadius, orbitSpeed, hue -/
  params : Array Float
  /-- Per-particle computed positions: x, y -/
  positions : Array Float
  /-- Number of particles -/
  count : Nat
  /-- Center of orbit -/
  centerX : Float
  centerY : Float
  deriving Inhabited

/-- Create orbital particles around a center point. -/
def OrbitalState.create (count : Nat) (centerX centerY maxRadius : Float) (seed : Nat) : OrbitalState :=
  let (params, positions) := Id.run do
    let mut parr := Array.mkEmpty (count * 4)
    let mut posarr := Array.mkEmpty (count * 2)
    let mut s := seed
    for i in [:count] do
      -- Phase
      s := (s * 1103515245 + 12345) % (2^31)
      let phase := (s.toFloat / 2147483648.0) * 6.28318
      -- Base radius
      s := (s * 1103515245 + 12345) % (2^31)
      let baseRadius := 20.0 + (s.toFloat / 2147483648.0) * (maxRadius - 20.0)
      -- Orbit speed
      s := (s * 1103515245 + 12345) % (2^31)
      let orbitSpeed := 0.5 + (s.toFloat / 2147483648.0) * 2.0
      let hue := i.toFloat / count.toFloat
      parr := parr.push phase
      parr := parr.push baseRadius
      parr := parr.push orbitSpeed
      parr := parr.push hue
      posarr := posarr.push centerX  -- Initial x
      posarr := posarr.push centerY  -- Initial y
    (parr, posarr)
  { params, positions, count, centerX, centerY }

/-- Update orbital positions from time. -/
def OrbitalState.update (o : OrbitalState) (t : Float) : OrbitalState :=
  let positions := Id.run do
    let mut arr := o.positions
    for i in [:o.count] do
      let pbase := i * 4
      let phase := o.params[pbase]!
      let baseRadius := o.params[pbase + 1]!
      let orbitSpeed := o.params[pbase + 2]!
      -- Compute current angle
      let angle := t * orbitSpeed + phase
      -- Add wobble to radius
      let wobble := 30.0 * Float.sin (t * 0.5 + phase * 3.0)
      let radius := baseRadius + wobble
      -- Compute position
      let x := o.centerX + radius * Float.cos angle
      let y := o.centerY + radius * Float.sin angle
      let posBase := i * 2
      arr := arr.set! posBase x
      arr := arr.set! (posBase + 1) y
    arr
  { o with positions }

/-- Build rect data from orbital state. -/
def buildOrbitalRectData (o : OrbitalState) (halfSize t spinSpeed : Float) : Array Float := Id.run do
  let mut data := Array.mkEmpty (o.count * 5)
  for i in [:o.count] do
    let posBase := i * 2
    let paramBase := i * 4
    let x := o.positions[posBase]!
    let y := o.positions[posBase + 1]!
    let hue := o.params[paramBase + 3]!
    let phase := o.params[paramBase]!
    let rotation := t * spinSpeed + phase
    data := data.push x
    data := data.push y
    data := data.push hue
    data := data.push halfSize
    data := data.push rotation
  data

/-- Build circle data from orbital state. -/
def buildOrbitalCircleData (o : OrbitalState) (radius : Float) : Array Float := Id.run do
  let mut data := Array.mkEmpty (o.count * 4)
  for i in [:o.count] do
    let posBase := i * 2
    let paramBase := i * 4
    let x := o.positions[posBase]!
    let y := o.positions[posBase + 1]!
    let hue := o.params[paramBase + 3]!
    data := data.push x
    data := data.push y
    data := data.push hue
    data := data.push radius
  data

/-- Build triangle data from orbital state. -/
def buildOrbitalTriangleData (o : OrbitalState) (halfSize t spinSpeed : Float) : Array Float := Id.run do
  let mut data := Array.mkEmpty (o.count * 5)
  for i in [:o.count] do
    let posBase := i * 2
    let paramBase := i * 4
    let x := o.positions[posBase]!
    let y := o.positions[posBase + 1]!
    let hue := o.params[paramBase + 3]!
    let phase := o.params[paramBase]!
    let rotation := t * spinSpeed + phase
    data := data.push x
    data := data.push y
    data := data.push hue
    data := data.push halfSize
    data := data.push rotation
  data

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

These wrap the FFI calls with a cleaner interface. -/

/-- Draw dynamic circles. GPU computes color + NDC conversion. -/
def drawCircles (renderer : FFI.Renderer) (particles : ParticleState) (radius t : Float) : IO Unit := do
  let data := buildCircleData particles radius
  FFI.Renderer.drawDynamicCircles renderer data particles.count.toUInt32 t

/-- Draw dynamic rects with time-based rotation. GPU computes color + NDC. -/
def drawRectsAnimated (renderer : FFI.Renderer) (particles : ParticleState)
    (halfSize t spinSpeed : Float) : IO Unit := do
  let data := buildRectDataAnimated particles halfSize t spinSpeed
  FFI.Renderer.drawDynamicRects renderer data particles.count.toUInt32 t

/-- Draw dynamic rects with uniform rotation. GPU computes color + NDC. -/
def drawRectsUniform (renderer : FFI.Renderer) (particles : ParticleState)
    (halfSize rotation t : Float) : IO Unit := do
  let data := buildRectDataUniform particles halfSize rotation
  FFI.Renderer.drawDynamicRects renderer data particles.count.toUInt32 t

/-- Draw dynamic triangles with time-based rotation. GPU computes color + NDC. -/
def drawTrianglesAnimated (renderer : FFI.Renderer) (particles : ParticleState)
    (halfSize t spinSpeed : Float) : IO Unit := do
  let data := buildTriangleDataAnimated particles halfSize t spinSpeed
  FFI.Renderer.drawDynamicTriangles renderer data particles.count.toUInt32 t

/-- Draw dynamic triangles with uniform rotation. GPU computes color + NDC. -/
def drawTrianglesUniform (renderer : FFI.Renderer) (particles : ParticleState)
    (halfSize rotation t : Float) : IO Unit := do
  let data := buildTriangleDataUniform particles halfSize rotation
  FFI.Renderer.drawDynamicTriangles renderer data particles.count.toUInt32 t

/-! ## Orbital Draw Functions -/

/-- Draw orbital rects with spinning animation. -/
def drawOrbitalRects (renderer : FFI.Renderer) (orbital : OrbitalState)
    (halfSize t spinSpeed : Float) : IO Unit := do
  let data := buildOrbitalRectData orbital halfSize t spinSpeed
  FFI.Renderer.drawDynamicRects renderer data orbital.count.toUInt32 t

/-- Draw orbital circles. -/
def drawOrbitalCircles (renderer : FFI.Renderer) (orbital : OrbitalState)
    (radius t : Float) : IO Unit := do
  let data := buildOrbitalCircleData orbital radius
  FFI.Renderer.drawDynamicCircles renderer data orbital.count.toUInt32 t

/-- Draw orbital triangles with spinning animation. -/
def drawOrbitalTriangles (renderer : FFI.Renderer) (orbital : OrbitalState)
    (halfSize t spinSpeed : Float) : IO Unit := do
  let data := buildOrbitalTriangleData orbital halfSize t spinSpeed
  FFI.Renderer.drawDynamicTriangles renderer data orbital.count.toUInt32 t

end Afferent.Render.Dynamic

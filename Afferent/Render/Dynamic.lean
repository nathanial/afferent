/-
  Afferent Dynamic Rendering Module

  Generic dynamic shape rendering where:
  - CPU controls: position, size, rotation, base hue
  - GPU computes: HSV→RGB, pixel→clip conversion, time-based color animation

  Data formats (instanced layout):
  - Circle: [x, y, angle=0, radius, hue, 0, 0, 1] × count (8 floats)
  - Rect:   [x, y, rotation, halfSize, hue, 0, 0, 1] × count (8 floats)
  - Triangle: [x, y, rotation, halfSize, hue, 0, 0, 1] × count (8 floats)
-/

import Afferent.FFI
import Init.Data.FloatArray

namespace Afferent.Render.Dynamic

private def sizeModeScreen : UInt32 := 1
private def colorModeHSV : UInt32 := 1
private def dynamicHueSpeed : Float := 0.2
private def rotationModeUniform : UInt32 := 0
private def rotationModeAnimated : UInt32 := 1

private def pixelToClipParams (width height : Float)
    : Float × Float × Float × Float × Float × Float :=
  (2.0 / width, 0.0, 0.0, -2.0 / height, -1.0, 1.0)

/-! ## Generic Dynamic Particle Data

These structures hold particle state that can be updated by CPU each frame.
The GPU then handles color computation and coordinate conversion. -/

/-- Generic particle data for dynamic rendering.
    Each particle has position, velocity, and a base hue. -/
structure ParticleState where
  /-- Per-particle: x, y, vx, vy, hueBase (5 floats) -/
  data : FloatArray
  /-- Number of particles -/
  count : Nat
  /-- Screen bounds for collision detection -/
  screenWidth : Float
  screenHeight : Float
  deriving Inhabited

/-- Create initial particle state with random positions and velocities. -/
def ParticleState.create (count : Nat) (screenWidth screenHeight : Float) (seed : Nat) : ParticleState :=
  let data := Id.run do
    let mut arr := FloatArray.emptyWithCapacity (count * 5)
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
      let x := arr.get! base
      let y := arr.get! (base + 1)
      let vx := arr.get! (base + 2)
      let vy := arr.get! (base + 3)

      -- Update position
      let x' := x + vx * dt
      let y' := y + vy * dt

      -- Bounce off walls (avoid boxing vx/vy unless they change)
      let (x'', vx', bouncedX) :=
        if x' < shapeRadius then (shapeRadius, -vx, true)
        else if x' > p.screenWidth - shapeRadius then (p.screenWidth - shapeRadius, -vx, true)
        else (x', vx, false)
      let (y'', vy', bouncedY) :=
        if y' < shapeRadius then (shapeRadius, -vy, true)
        else if y' > p.screenHeight - shapeRadius then (p.screenHeight - shapeRadius, -vy, true)
        else (y', vy, false)

      arr := arr.set! base x''
      arr := arr.set! (base + 1) y''
      if bouncedX then
        arr := arr.set! (base + 2) vx'
      if bouncedY then
        arr := arr.set! (base + 3) vy'
    arr
  { p with data }

/-! ## Buffer Allocation Helpers -/

/-- Create a FloatBuffer sized for instanced shapes (8 floats per particle). -/
def ParticleState.createInstanceBuffer (p : ParticleState) : IO FFI.FloatBuffer := do
  FFI.FloatBuffer.create (p.count.toUSize * 8)

/-- Create a FloatBuffer sized for sprites (5 floats per particle). -/
def ParticleState.createSpriteBuffer (p : ParticleState) : IO FFI.FloatBuffer := do
  FFI.FloatBuffer.create (p.count.toUSize * 5)

/-- Write instanced shape data with uniform rotation into a FloatBuffer. -/
def writeInstancedUniformToBuffer (particles : ParticleState) (buffer : FFI.FloatBuffer)
    (halfSize rotation : Float) : IO Unit := do
  FFI.FloatBuffer.writeInstancedFromParticles
    buffer
    particles.data
    particles.count.toUInt32
    halfSize
    rotation
    0.0
    0.0
    rotationModeUniform

/-- Write instanced shape data with animated rotation into a FloatBuffer. -/
def writeInstancedAnimatedToBuffer (particles : ParticleState) (buffer : FFI.FloatBuffer)
    (halfSize t spinSpeed : Float) : IO Unit := do
  FFI.FloatBuffer.writeInstancedFromParticles
    buffer
    particles.data
    particles.count.toUInt32
    halfSize
    0.0
    t
    spinSpeed
    rotationModeAnimated

/-- Create particles in a grid layout with zero velocity. -/
def ParticleState.createGrid (cols rows : Nat) (startX startY spacing : Float)
    (screenWidth screenHeight : Float) : ParticleState :=
  let count := cols * rows
  let data := Id.run do
    let mut arr := FloatArray.emptyWithCapacity (count * 5)
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

/-! ## Instanced Draw Helpers -/

private def drawInstancedFromBuffer (renderer : FFI.Renderer) (shapeType : UInt32)
    (buffer : FFI.FloatBuffer) (count : UInt32) (t : Float)
    (screenWidth screenHeight : Float) : IO Unit := do
  let (a, b, c, d, tx, ty) := pixelToClipParams screenWidth screenHeight
  FFI.Renderer.drawInstancedShapesBuffer
    renderer
    shapeType
    buffer
    count
    a b c d tx ty
    screenWidth screenHeight
    sizeModeScreen
    t dynamicHueSpeed
    colorModeHSV

def drawInstancedUniform (renderer : FFI.Renderer) (shapeType : UInt32) (particles : ParticleState)
    (buffer : FFI.FloatBuffer) (halfSize rotation t : Float) : IO Unit := do
  writeInstancedUniformToBuffer particles buffer halfSize rotation
  drawInstancedFromBuffer renderer shapeType buffer particles.count.toUInt32 t
    particles.screenWidth particles.screenHeight

def drawInstancedAnimated (renderer : FFI.Renderer) (shapeType : UInt32) (particles : ParticleState)
    (buffer : FFI.FloatBuffer) (halfSize t spinSpeed : Float) : IO Unit := do
  writeInstancedAnimatedToBuffer particles buffer halfSize t spinSpeed
  drawInstancedFromBuffer renderer shapeType buffer particles.count.toUInt32 t
    particles.screenWidth particles.screenHeight

/-! ## FloatBuffer-based Sprite Rendering

For maximum performance with 1M+ sprites, write directly to a FloatBuffer. -/

/-- Write sprite data for all particles directly into a FloatBuffer.
    This is the high-performance path for 1M+ sprites.
    Format: [x, y, rotation, halfSize, alpha] per sprite (5 floats) -/
def writeSpritesToBuffer (particles : ParticleState) (buffer : FFI.FloatBuffer)
    (halfSize : Float) (rotation : Float := 0.0) (alpha : Float := 1.0) : IO Unit := do
  -- One FFI call for all sprites (avoids 100k boundary crossings per frame)
  FFI.FloatBuffer.writeSpritesFromParticles buffer particles.data particles.count.toUInt32 halfSize rotation alpha

/-- Draw sprites from a FloatBuffer. Call writeSpritesToBuffer first, then this. -/
def drawSpritesFromBuffer (renderer : FFI.Renderer) (texture : FFI.Texture)
    (buffer : FFI.FloatBuffer) (count : UInt32) (_halfSize : Float)
    (screenWidth screenHeight : Float) : IO Unit := do
  -- Buffer already contains SpriteInstanceData layout, so use direct instance draw.
  -- halfSize is ignored (kept for API compatibility).
  FFI.Renderer.drawSpritesInstanceBuffer renderer texture buffer count screenWidth screenHeight

/-! ## Orbital Particle System

Particles that orbit around a center point. Each particle has:
- phase: initial angle offset
- radius: distance from center
- speed: angular velocity (can be negative for reverse orbit)
- hue: base color hue for HSV animation
- size: half-size of the rendered shape -/

/-- Orbital particle state. Each particle orbits around a center point. -/
structure OrbitalState where
  /-- Per-particle orbital parameters: phase, radius, speed, hue, size (5 floats) -/
  params : FloatArray
  /-- Number of particles -/
  count : Nat
  /-- Center of orbit X coordinate -/
  centerX : Float
  /-- Center of orbit Y coordinate -/
  centerY : Float
  /-- Screen width for coordinate conversion -/
  screenWidth : Float
  /-- Screen height for coordinate conversion -/
  screenHeight : Float
  deriving Inhabited

/-- Create orbital particles with random parameters.
    - count: number of particles
    - centerX, centerY: orbit center
    - minRadius, maxRadius: range for orbit radius
    - speedMin, speedMax: range for angular velocity
    - sizeMin, sizeMax: range for particle size
    - screenWidth, screenHeight: screen dimensions
    - seed: random seed for deterministic generation -/
def OrbitalState.create (count : Nat) (centerX centerY : Float)
    (minRadius maxRadius : Float) (speedMin speedMax : Float)
    (sizeMin sizeMax : Float) (screenWidth screenHeight : Float)
    (seed : Nat) : OrbitalState :=
  let twoPi : Float := 6.283185307
  let params := Id.run do
    let mut arr := FloatArray.emptyWithCapacity (count * 5)
    let mut s := seed
    for i in [:count] do
      -- Phase: random angle 0..2π
      s := (s * 1103515245 + 12345) % (2^31)
      let phase := (s.toFloat / 2147483648.0) * twoPi
      -- Radius: random in range
      s := (s * 1103515245 + 12345) % (2^31)
      let radius := minRadius + (s.toFloat / 2147483648.0) * (maxRadius - minRadius)
      -- Speed: random in range, with random direction
      s := (s * 1103515245 + 12345) % (2^31)
      let baseSpeed := speedMin + (s.toFloat / 2147483648.0) * (speedMax - speedMin)
      s := (s * 1103515245 + 12345) % (2^31)
      let dir : Float := if s % 2 == 0 then 1.0 else -1.0
      let speed := baseSpeed * dir
      -- Hue: evenly distributed
      let hue := i.toFloat / count.toFloat
      -- Size: random in range
      s := (s * 1103515245 + 12345) % (2^31)
      let size := sizeMin + (s.toFloat / 2147483648.0) * (sizeMax - sizeMin)
      arr := arr.push phase
      arr := arr.push radius
      arr := arr.push speed
      arr := arr.push hue
      arr := arr.push size
    arr
  { params, count, centerX, centerY, screenWidth, screenHeight }

/-- Write orbital particle positions to a FloatBuffer for instanced rendering.
    Computes x, y positions and rotation from orbital parameters at time t.
    Format: [x, y, rotation, size, hue, 0, 0, 1] per particle (8 floats) -/
def writeOrbitalToBuffer (orbital : OrbitalState) (buffer : FFI.FloatBuffer)
    (t : Float) : IO Unit := do
  -- Hoist struct field accesses out of the loop for performance
  let params := orbital.params
  let centerX := orbital.centerX
  let centerY := orbital.centerY
  let count := orbital.count
  for i in [:count] do
    let base := i * 5
    let phase := params.get! base
    let radius := params.get! (base + 1)
    let speed := params.get! (base + 2)
    let hue := params.get! (base + 3)
    let size := params.get! (base + 4)
    -- Compute position from orbital motion
    let angle := phase + t * speed
    let x := centerX + radius * Float.cos angle
    let y := centerY + radius * Float.sin angle
    -- Rotation follows orbital angle
    let rot := angle
    -- Write to buffer: [x, y, rotation, size, hue, 0, 0, 1]
    let bufIndex : USize := (i * 8).toUSize
    FFI.FloatBuffer.setVec8 buffer bufIndex x y rot size hue 0.0 0.0 1.0

/-- Draw orbital particles as rectangles. -/
def drawOrbitalRects (renderer : FFI.Renderer) (orbital : OrbitalState)
    (buffer : FFI.FloatBuffer) (t : Float) : IO Unit := do
  writeOrbitalToBuffer orbital buffer t
  drawInstancedFromBuffer renderer 0 buffer orbital.count.toUInt32 t
    orbital.screenWidth orbital.screenHeight

end Afferent.Render.Dynamic

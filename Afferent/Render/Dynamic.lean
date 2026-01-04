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

/-! ## Fused Update + Packing (High Performance)

These functions update the particle simulation and write the render instance
buffers in a single pass to reduce memory bandwidth at 1M+ particles. -/

/-- Update bouncing physics and write sprite instance buffer in one pass. -/
def ParticleState.updateBouncingAndWriteSprites (p : ParticleState)
    (dt halfSize : Float) (spriteBuffer : FFI.FloatBuffer) : IO ParticleState := do
  let data ← FFI.Particles.updateBouncingAndWriteSprites
    p.data p.count.toUInt32 dt halfSize p.screenWidth p.screenHeight spriteBuffer
  pure { p with data }

/-- Update bouncing physics and write instanced circle buffer in one pass. -/
def ParticleState.updateBouncingAndWriteCircles (p : ParticleState)
    (dt radius : Float) (circleBuffer : FFI.FloatBuffer) : IO ParticleState := do
  let data ← FFI.Particles.updateBouncingAndWriteCircles
    p.data p.count.toUInt32 dt radius p.screenWidth p.screenHeight circleBuffer
  pure { p with data }

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

/-- Draw dynamic circles from a FloatBuffer containing instanced circle data. -/
def drawCirclesFromBuffer (renderer : FFI.Renderer) (circleBuffer : FFI.FloatBuffer)
    (count : UInt32) (t : Float) (screenWidth screenHeight : Float) : IO Unit := do
  let (a, b, c, d, tx, ty) := pixelToClipParams screenWidth screenHeight
  FFI.Renderer.drawInstancedCirclesBuffer
    renderer
    circleBuffer
    count
    a b c d tx ty
    screenWidth screenHeight
    sizeModeScreen
    t dynamicHueSpeed
    colorModeHSV

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

/-! ## Data Builders (Array-Based Slow Path)

These build boxed Array Float payloads and are convenient for small counts.
For high-performance streaming, prefer the FloatBuffer-based writers. -/

/-- Build instanced circle data from particle state.
    Format: [pixelX, pixelY, angle=0, radiusPixels, hueBase, 0, 0, 1] × count. -/
def buildCircleData (particles : ParticleState) (radius : Float) : Array Float := Id.run do
  let mut data := Array.mkEmpty (particles.count * 8)
  for i in [:particles.count] do
    let base := i * 5
    let x := particles.data.get! base
    let y := particles.data.get! (base + 1)
    let hue := particles.data.get! (base + 4)
    data := data.push x
    data := data.push y
    data := data.push 0.0
    data := data.push radius
    data := data.push hue
    data := data.push 0.0
    data := data.push 0.0
    data := data.push 1.0
  data

/-- Build instanced rect data from particle state.
    Format: [pixelX, pixelY, rotation, halfSizePixels, hueBase, 0, 0, 1] × count. -/
def buildRectData (particles : ParticleState) (halfSize : Float) (getRotation : Nat → Float) : Array Float := Id.run do
  let mut data := Array.mkEmpty (particles.count * 8)
  for i in [:particles.count] do
    let base := i * 5
    let x := particles.data.get! base
    let y := particles.data.get! (base + 1)
    let hue := particles.data.get! (base + 4)
    data := data.push x
    data := data.push y
    data := data.push (getRotation i)
    data := data.push halfSize
    data := data.push hue
    data := data.push 0.0
    data := data.push 0.0
    data := data.push 1.0
  data

/-- Build dynamic rect data with uniform rotation for all particles. -/
def buildRectDataUniform (particles : ParticleState) (halfSize rotation : Float) : Array Float :=
  buildRectData particles halfSize (fun _ => rotation)

/-- Build dynamic rect data with time-based per-particle rotation. -/
def buildRectDataAnimated (particles : ParticleState) (halfSize t spinSpeed : Float) : Array Float :=
  buildRectData particles halfSize (fun i =>
    let hue := particles.data.get! (i * 5 + 4)
    t * spinSpeed + hue * 6.28)

/-- Build instanced triangle data from particle state.
    Format: [pixelX, pixelY, rotation, halfSizePixels, hueBase, 0, 0, 1] × count. -/
def buildTriangleData (particles : ParticleState) (halfSize : Float) (getRotation : Nat → Float) : Array Float := Id.run do
  let mut data := Array.mkEmpty (particles.count * 8)
  for i in [:particles.count] do
    let base := i * 5
    let x := particles.data.get! base
    let y := particles.data.get! (base + 1)
    let hue := particles.data.get! (base + 4)
    data := data.push x
    data := data.push y
    data := data.push (getRotation i)
    data := data.push halfSize
    data := data.push hue
    data := data.push 0.0
    data := data.push 0.0
    data := data.push 1.0
  data

/-- Build dynamic triangle data with uniform rotation. -/
def buildTriangleDataUniform (particles : ParticleState) (halfSize rotation : Float) : Array Float :=
  buildTriangleData particles halfSize (fun _ => rotation)

/-- Build dynamic triangle data with time-based per-particle rotation. -/
def buildTriangleDataAnimated (particles : ParticleState) (halfSize t spinSpeed : Float) : Array Float :=
  buildTriangleData particles halfSize (fun i =>
    let hue := particles.data.get! (i * 5 + 4)
    t * spinSpeed + hue * 6.28)

/-! ## Draw Functions (Buffer-First)

These are the preferred paths for high-volume instanced rendering.
They stream instance data into a FloatBuffer each frame (no boxed Arrays). -/

/-- Draw dynamic circles using a FloatBuffer. -/
def drawCircles (renderer : FFI.Renderer) (particles : ParticleState)
    (buffer : FFI.FloatBuffer) (radius t : Float) : IO Unit := do
  writeInstancedUniformToBuffer particles buffer radius 0.0
  drawCirclesFromBuffer renderer buffer particles.count.toUInt32 t
    particles.screenWidth particles.screenHeight

/-- Draw dynamic rects with time-based rotation using a FloatBuffer. -/
def drawRectsAnimated (renderer : FFI.Renderer) (particles : ParticleState)
    (buffer : FFI.FloatBuffer) (halfSize t spinSpeed : Float) : IO Unit := do
  writeInstancedAnimatedToBuffer particles buffer halfSize t spinSpeed
  let (a, b, c, d, tx, ty) := pixelToClipParams particles.screenWidth particles.screenHeight
  FFI.Renderer.drawInstancedRectsBuffer
    renderer
    buffer
    particles.count.toUInt32
    a b c d tx ty
    particles.screenWidth particles.screenHeight
    sizeModeScreen
    t dynamicHueSpeed
    colorModeHSV

/-- Draw dynamic rects with uniform rotation using a FloatBuffer. -/
def drawRectsUniform (renderer : FFI.Renderer) (particles : ParticleState)
    (buffer : FFI.FloatBuffer) (halfSize rotation t : Float) : IO Unit := do
  writeInstancedUniformToBuffer particles buffer halfSize rotation
  let (a, b, c, d, tx, ty) := pixelToClipParams particles.screenWidth particles.screenHeight
  FFI.Renderer.drawInstancedRectsBuffer
    renderer
    buffer
    particles.count.toUInt32
    a b c d tx ty
    particles.screenWidth particles.screenHeight
    sizeModeScreen
    t dynamicHueSpeed
    colorModeHSV

/-- Draw dynamic triangles with time-based rotation using a FloatBuffer. -/
def drawTrianglesAnimated (renderer : FFI.Renderer) (particles : ParticleState)
    (buffer : FFI.FloatBuffer) (halfSize t spinSpeed : Float) : IO Unit := do
  writeInstancedAnimatedToBuffer particles buffer halfSize t spinSpeed
  let (a, b, c, d, tx, ty) := pixelToClipParams particles.screenWidth particles.screenHeight
  FFI.Renderer.drawInstancedTrianglesBuffer
    renderer
    buffer
    particles.count.toUInt32
    a b c d tx ty
    particles.screenWidth particles.screenHeight
    sizeModeScreen
    t dynamicHueSpeed
    colorModeHSV

/-- Draw dynamic triangles with uniform rotation using a FloatBuffer. -/
def drawTrianglesUniform (renderer : FFI.Renderer) (particles : ParticleState)
    (buffer : FFI.FloatBuffer) (halfSize rotation t : Float) : IO Unit := do
  writeInstancedUniformToBuffer particles buffer halfSize rotation
  let (a, b, c, d, tx, ty) := pixelToClipParams particles.screenWidth particles.screenHeight
  FFI.Renderer.drawInstancedTrianglesBuffer
    renderer
    buffer
    particles.count.toUInt32
    a b c d tx ty
    particles.screenWidth particles.screenHeight
    sizeModeScreen
    t dynamicHueSpeed
    colorModeHSV

/-! ## Draw Functions (Array-Based Slow Path)

These allocate boxed Arrays and should be used only for small counts. -/

/-- Draw dynamic circles using a boxed Array Float (slow path). -/
def drawCirclesArray (renderer : FFI.Renderer) (particles : ParticleState) (radius t : Float) : IO Unit := do
  let data := buildCircleData particles radius
  let (a, b, c, d, tx, ty) := pixelToClipParams particles.screenWidth particles.screenHeight
  FFI.Renderer.drawInstancedCircles
    renderer
    data
    particles.count.toUInt32
    a b c d tx ty
    particles.screenWidth particles.screenHeight
    sizeModeScreen
    t dynamicHueSpeed
    colorModeHSV

/-- Draw dynamic rects with time-based rotation using a boxed Array Float (slow path). -/
def drawRectsAnimatedArray (renderer : FFI.Renderer) (particles : ParticleState)
    (halfSize t spinSpeed : Float) : IO Unit := do
  let data := buildRectDataAnimated particles halfSize t spinSpeed
  let (a, b, c, d, tx, ty) := pixelToClipParams particles.screenWidth particles.screenHeight
  FFI.Renderer.drawInstancedRects
    renderer
    data
    particles.count.toUInt32
    a b c d tx ty
    particles.screenWidth particles.screenHeight
    sizeModeScreen
    t dynamicHueSpeed
    colorModeHSV

/-- Draw dynamic rects with uniform rotation using a boxed Array Float (slow path). -/
def drawRectsUniformArray (renderer : FFI.Renderer) (particles : ParticleState)
    (halfSize rotation t : Float) : IO Unit := do
  let data := buildRectDataUniform particles halfSize rotation
  let (a, b, c, d, tx, ty) := pixelToClipParams particles.screenWidth particles.screenHeight
  FFI.Renderer.drawInstancedRects
    renderer
    data
    particles.count.toUInt32
    a b c d tx ty
    particles.screenWidth particles.screenHeight
    sizeModeScreen
    t dynamicHueSpeed
    colorModeHSV

/-- Draw dynamic triangles with time-based rotation using a boxed Array Float (slow path). -/
def drawTrianglesAnimatedArray (renderer : FFI.Renderer) (particles : ParticleState)
    (halfSize t spinSpeed : Float) : IO Unit := do
  let data := buildTriangleDataAnimated particles halfSize t spinSpeed
  let (a, b, c, d, tx, ty) := pixelToClipParams particles.screenWidth particles.screenHeight
  FFI.Renderer.drawInstancedTriangles
    renderer
    data
    particles.count.toUInt32
    a b c d tx ty
    particles.screenWidth particles.screenHeight
    sizeModeScreen
    t dynamicHueSpeed
    colorModeHSV

/-- Draw dynamic triangles with uniform rotation using a boxed Array Float (slow path). -/
def drawTrianglesUniformArray (renderer : FFI.Renderer) (particles : ParticleState)
    (halfSize rotation t : Float) : IO Unit := do
  let data := buildTriangleDataUniform particles halfSize rotation
  let (a, b, c, d, tx, ty) := pixelToClipParams particles.screenWidth particles.screenHeight
  FFI.Renderer.drawInstancedTriangles
    renderer
    data
    particles.count.toUInt32
    a b c d tx ty
    particles.screenWidth particles.screenHeight
    sizeModeScreen
    t dynamicHueSpeed
    colorModeHSV

/-! ## Sprite Data Builders

Build packed float arrays for sprite rendering (textured quads).
Format: [pixelX, pixelY, rotation, halfSize, alpha] × count (5 floats per sprite) -/

/-- Build sprite data from particle state.
    Format: [pixelX, pixelY, rotation, halfSize, alpha] × count (5 floats per sprite) -/
def buildSpriteData (particles : ParticleState) (halfSize : Float)
    (getRotation : Nat → Float) (getAlpha : Nat → Float) : Array Float := Id.run do
  let mut data := Array.mkEmpty (particles.count * 5)
  for i in [:particles.count] do
    let base := i * 5
    let x := particles.data.get! base
    let y := particles.data.get! (base + 1)
    data := data.push x
    data := data.push y
    data := data.push (getRotation i)
    data := data.push halfSize
    data := data.push (getAlpha i)
  data

/-- Build sprite data with uniform rotation and full opacity. -/
def buildSpriteDataUniform (particles : ParticleState) (halfSize rotation : Float) : Array Float :=
  buildSpriteData particles halfSize (fun _ => rotation) (fun _ => 1.0)

/-- Build sprite data with time-based per-particle rotation. -/
def buildSpriteDataAnimated (particles : ParticleState) (halfSize t spinSpeed : Float) : Array Float :=
  buildSpriteData particles halfSize
    (fun i =>
      let hue := particles.data.get! (i * 5 + 4)
      t * spinSpeed + hue * 6.28)
    (fun _ => 1.0)

/-- Draw sprites with texture. Position from particle state, GPU handles NDC conversion. -/
def drawSprites (renderer : FFI.Renderer) (texture : FFI.Texture) (particles : ParticleState)
    (halfSize : Float) (rotation : Float := 0.0) : IO Unit := do
  let data := buildSpriteDataUniform particles halfSize rotation
  FFI.Renderer.drawSprites renderer texture data particles.count.toUInt32 particles.screenWidth particles.screenHeight

/-- Draw sprites with time-based rotation animation. -/
def drawSpritesAnimated (renderer : FFI.Renderer) (texture : FFI.Texture) (particles : ParticleState)
    (halfSize t spinSpeed : Float) : IO Unit := do
  let data := buildSpriteDataAnimated particles halfSize t spinSpeed
  FFI.Renderer.drawSprites renderer texture data particles.count.toUInt32 particles.screenWidth particles.screenHeight

/-! ## FloatBuffer-based Sprite Rendering

For maximum performance with 1M+ sprites, write directly to a FloatBuffer
instead of building a Lean Array. This eliminates:
- Lean array allocation (copy-on-write overhead)
- FFI array-to-C conversion (5M unboxing operations)

The FloatBuffer approach: 1 FFI call per sprite (setVec5) vs 5M unbox calls. -/

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

end Afferent.Render.Dynamic

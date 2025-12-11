/-
  Afferent Canvas Context
  High-level drawing API similar to HTML5 Canvas.
-/
import Afferent.Core.Types
import Afferent.Core.Path
import Afferent.Core.Transform
import Afferent.Core.Paint
import Afferent.Canvas.State
import Afferent.Render.Tessellation
import Afferent.Text.Font
import Afferent.FFI.Metal

namespace Afferent

/-- Drawing context that wraps FFI renderer with high-level API. -/
structure DrawContext where
  window : FFI.Window
  renderer : FFI.Renderer
  /-- Initial/logical canvas width (used as reference for coordinate system) -/
  baseWidth : Float
  /-- Initial/logical canvas height (used as reference for coordinate system) -/
  baseHeight : Float

namespace DrawContext

/-- Create a new drawing context with a window. -/
def create (width height : UInt32) (title : String) : IO DrawContext := do
  FFI.init
  let window ← FFI.Window.create width height title
  let renderer ← FFI.Renderer.create window
  pure {
    window
    renderer
    baseWidth := width.toFloat
    baseHeight := height.toFloat
  }

/-- Get the current drawable size (may differ from base size due to window resize or Retina scaling). -/
def getCurrentSize (ctx : DrawContext) : IO (Float × Float) := do
  let (w, h) ← ctx.window.getSize
  pure (w.toFloat, h.toFloat)

/-- Get width for coordinate calculations (uses current drawable size). -/
def width (ctx : DrawContext) : IO Float := do
  let (w, _) ← ctx.getCurrentSize
  pure w

/-- Get height for coordinate calculations (uses current drawable size). -/
def height (ctx : DrawContext) : IO Float := do
  let (_, h) ← ctx.getCurrentSize
  pure h

/-- Check if the window should close. -/
def shouldClose (ctx : DrawContext) : IO Bool :=
  ctx.window.shouldClose

/-- Poll window events. -/
def pollEvents (ctx : DrawContext) : IO Unit :=
  ctx.window.pollEvents

/-- Get the last key code pressed (0 if none). -/
def getKeyCode (ctx : DrawContext) : IO UInt16 :=
  ctx.window.getKeyCode

/-- Clear the key pressed state (call after handling). -/
def clearKey (ctx : DrawContext) : IO Unit :=
  ctx.window.clearKey

/-- Begin a new frame with a clear color. -/
def beginFrame (ctx : DrawContext) (clearColor : Color) : IO Bool :=
  ctx.renderer.beginFrame clearColor.r clearColor.g clearColor.b clearColor.a

/-- End the current frame and present. -/
def endFrame (ctx : DrawContext) : IO Unit :=
  ctx.renderer.endFrame

/-- Clean up resources. -/
def destroy (ctx : DrawContext) : IO Unit := do
  FFI.Renderer.destroy ctx.renderer
  FFI.Window.destroy ctx.window

/-- Set a scissor rectangle for clipping. Coordinates are in pixels. -/
def setScissor (ctx : DrawContext) (x y width height : UInt32) : IO Unit :=
  ctx.renderer.setScissor x y width height

/-- Reset scissor to full viewport (disable clipping). -/
def resetScissor (ctx : DrawContext) : IO Unit :=
  ctx.renderer.resetScissor

/-- Fill a rectangle with a solid color (pixel coordinates). -/
def fillRect (ctx : DrawContext) (rect : Rect) (color : Color) : IO Unit := do
  -- Use base (logical) canvas size for NDC conversion to maintain coordinate system
  let result := Tessellation.tessellateRectNDC rect color ctx.baseWidth ctx.baseHeight
  if result.vertices.size > 0 && result.indices.size > 0 then
    let vertexBuffer ← FFI.Buffer.createVertex ctx.renderer result.vertices
    let indexBuffer ← FFI.Buffer.createIndex ctx.renderer result.indices
    ctx.renderer.drawTriangles vertexBuffer indexBuffer result.indices.size.toUInt32
    FFI.Buffer.destroy indexBuffer
    FFI.Buffer.destroy vertexBuffer

/-- Fill a rectangle specified by x, y, width, height. -/
def fillRectXYWH (ctx : DrawContext) (x y w h : Float) (color : Color) : IO Unit :=
  ctx.fillRect (Rect.mk' x y w h) color

/-- Fill a convex path with a solid color (pixel coordinates). -/
def fillPath (ctx : DrawContext) (path : Path) (color : Color) : IO Unit := do
  -- Use base (logical) canvas size for NDC conversion to maintain coordinate system
  let result := Tessellation.tessellateConvexPathNDC path color ctx.baseWidth ctx.baseHeight
  if result.vertices.size > 0 && result.indices.size > 0 then
    let vertexBuffer ← FFI.Buffer.createVertex ctx.renderer result.vertices
    let indexBuffer ← FFI.Buffer.createIndex ctx.renderer result.indices
    ctx.renderer.drawTriangles vertexBuffer indexBuffer result.indices.size.toUInt32
    FFI.Buffer.destroy indexBuffer
    FFI.Buffer.destroy vertexBuffer

/-- Fill a circle with a solid color. -/
def fillCircle (ctx : DrawContext) (center : Point) (radius : Float) (color : Color) : IO Unit :=
  ctx.fillPath (Path.circle center radius) color

/-- Fill an ellipse with a solid color. -/
def fillEllipse (ctx : DrawContext) (center : Point) (radiusX radiusY : Float) (color : Color) : IO Unit :=
  ctx.fillPath (Path.ellipse center radiusX radiusY) color

/-- Fill a rounded rectangle with a solid color. -/
def fillRoundedRect (ctx : DrawContext) (rect : Rect) (cornerRadius : Float) (color : Color) : IO Unit :=
  ctx.fillPath (Path.roundedRect rect cornerRadius) color

/-! ## Gradient Fill API -/

/-- Fill a rectangle with a fill style (solid color or gradient). -/
def fillRectWithStyle (ctx : DrawContext) (rect : Rect) (style : FillStyle) : IO Unit := do
  -- Use base (logical) canvas size for NDC conversion to maintain coordinate system
  let result := Tessellation.tessellateRectFillNDC rect style ctx.baseWidth ctx.baseHeight
  if result.vertices.size > 0 && result.indices.size > 0 then
    let vertexBuffer ← FFI.Buffer.createVertex ctx.renderer result.vertices
    let indexBuffer ← FFI.Buffer.createIndex ctx.renderer result.indices
    ctx.renderer.drawTriangles vertexBuffer indexBuffer result.indices.size.toUInt32
    FFI.Buffer.destroy indexBuffer
    FFI.Buffer.destroy vertexBuffer

/-- Fill a transformed rectangle with a fill style (fast path - no Path allocation). -/
def fillTransformedRectWithStyle (ctx : DrawContext) (rect : Rect) (transform : Transform) (style : FillStyle) : IO Unit := do
  let result := Tessellation.tessellateTransformedRectNDC rect transform style ctx.baseWidth ctx.baseHeight
  if result.vertices.size > 0 && result.indices.size > 0 then
    let vertexBuffer ← FFI.Buffer.createVertex ctx.renderer result.vertices
    let indexBuffer ← FFI.Buffer.createIndex ctx.renderer result.indices
    ctx.renderer.drawTriangles vertexBuffer indexBuffer result.indices.size.toUInt32
    FFI.Buffer.destroy indexBuffer
    FFI.Buffer.destroy vertexBuffer

/-- Fill a convex path with a fill style (solid color or gradient). -/
def fillPathWithStyle (ctx : DrawContext) (path : Path) (style : FillStyle) : IO Unit := do
  -- Use base (logical) canvas size for NDC conversion to maintain coordinate system
  let result := Tessellation.tessellateConvexPathFillNDC path style ctx.baseWidth ctx.baseHeight
  if result.vertices.size > 0 && result.indices.size > 0 then
    let vertexBuffer ← FFI.Buffer.createVertex ctx.renderer result.vertices
    let indexBuffer ← FFI.Buffer.createIndex ctx.renderer result.indices
    ctx.renderer.drawTriangles vertexBuffer indexBuffer result.indices.size.toUInt32
    FFI.Buffer.destroy indexBuffer
    FFI.Buffer.destroy vertexBuffer

/-- Fill a rectangle with a linear gradient. -/
def fillRectLinearGradient (ctx : DrawContext) (rect : Rect)
    (start finish : Point) (stops : Array GradientStop) : IO Unit :=
  ctx.fillRectWithStyle rect (.gradient (.linear start finish stops))

/-- Fill a rectangle with a radial gradient. -/
def fillRectRadialGradient (ctx : DrawContext) (rect : Rect)
    (center : Point) (radius : Float) (stops : Array GradientStop) : IO Unit :=
  ctx.fillRectWithStyle rect (.gradient (.radial center radius stops))

/-- Fill a circle with a radial gradient. -/
def fillCircleRadialGradient (ctx : DrawContext) (center : Point) (radius : Float)
    (stops : Array GradientStop) : IO Unit :=
  ctx.fillPathWithStyle (Path.circle center radius) (.gradient (.radial center radius stops))

/-- Fill an ellipse with a fill style. -/
def fillEllipseWithStyle (ctx : DrawContext) (center : Point) (radiusX radiusY : Float)
    (style : FillStyle) : IO Unit :=
  ctx.fillPathWithStyle (Path.ellipse center radiusX radiusY) style

/-- Fill a rounded rectangle with a fill style. -/
def fillRoundedRectWithStyle (ctx : DrawContext) (rect : Rect) (cornerRadius : Float)
    (style : FillStyle) : IO Unit :=
  ctx.fillPathWithStyle (Path.roundedRect rect cornerRadius) style

/-! ## Stroke Drawing (Simple API) -/

/-- Stroke a path with a given style (pixel coordinates). -/
def strokePath (ctx : DrawContext) (path : Path) (style : StrokeStyle) : IO Unit := do
  -- Use base (logical) canvas size for NDC conversion to maintain coordinate system
  let result := Tessellation.tessellateStrokeNDC path style ctx.baseWidth ctx.baseHeight
  if result.vertices.size > 0 && result.indices.size > 0 then
    let vertexBuffer ← FFI.Buffer.createVertex ctx.renderer result.vertices
    let indexBuffer ← FFI.Buffer.createIndex ctx.renderer result.indices
    ctx.renderer.drawTriangles vertexBuffer indexBuffer result.indices.size.toUInt32
    FFI.Buffer.destroy indexBuffer
    FFI.Buffer.destroy vertexBuffer

/-- Stroke a path with a color and line width. -/
def strokePathSimple (ctx : DrawContext) (path : Path) (color : Color) (lineWidth : Float := 1.0) : IO Unit :=
  ctx.strokePath path { StrokeStyle.default with color, lineWidth }

/-- Stroke a rectangle outline. -/
def strokeRect (ctx : DrawContext) (rect : Rect) (style : StrokeStyle) : IO Unit :=
  ctx.strokePath (Path.rectangle rect) style

/-- Stroke a rectangle with x, y, width, height and simple style. -/
def strokeRectXYWH (ctx : DrawContext) (x y width height : Float) (color : Color) (lineWidth : Float := 1.0) : IO Unit :=
  ctx.strokePathSimple (Path.rectangle (Rect.mk' x y width height)) color lineWidth

/-- Stroke a circle outline. -/
def strokeCircle (ctx : DrawContext) (center : Point) (radius : Float) (color : Color) (lineWidth : Float := 1.0) : IO Unit :=
  ctx.strokePathSimple (Path.circle center radius) color lineWidth

/-- Stroke an ellipse outline. -/
def strokeEllipse (ctx : DrawContext) (center : Point) (radiusX radiusY : Float) (color : Color) (lineWidth : Float := 1.0) : IO Unit :=
  ctx.strokePathSimple (Path.ellipse center radiusX radiusY) color lineWidth

/-- Stroke a rounded rectangle outline. -/
def strokeRoundedRect (ctx : DrawContext) (rect : Rect) (cornerRadius : Float) (color : Color) (lineWidth : Float := 1.0) : IO Unit :=
  ctx.strokePathSimple (Path.roundedRect rect cornerRadius) color lineWidth

/-- Draw a line from p1 to p2. -/
def drawLine (ctx : DrawContext) (p1 p2 : Point) (color : Color) (lineWidth : Float := 1.0) : IO Unit :=
  ctx.strokePathSimple (Path.empty |>.moveTo p1 |>.lineTo p2) color lineWidth

/-! ## Batch Drawing -/

/-- Draw all geometry accumulated in a batch with a single draw call.
    This is much faster than issuing separate draw calls for each shape. -/
def drawBatch (ctx : DrawContext) (batch : Batch) : IO Unit := do
  if batch.isEmpty then return
  let vertexBuffer ← FFI.Buffer.createVertex ctx.renderer batch.vertices
  let indexBuffer ← FFI.Buffer.createIndex ctx.renderer batch.indices
  ctx.renderer.drawTriangles vertexBuffer indexBuffer batch.indexCount.toUInt32
  FFI.Buffer.destroy indexBuffer
  FFI.Buffer.destroy vertexBuffer

/-! ## Text Rendering -/

/-- Draw text at a position with a font, color, and transform.
    Uses the base (logical) canvas size for NDC conversion to maintain coordinate system. -/
def fillTextTransformed (ctx : DrawContext) (text : String) (pos : Point) (font : Font) (color : Color) (transform : Transform) : IO Unit :=
  FFI.Text.render ctx.renderer font.handle text pos.x pos.y color.r color.g color.b color.a transform.toArray ctx.baseWidth ctx.baseHeight

/-- Draw text at a position with a font and color (identity transform). -/
def fillText (ctx : DrawContext) (text : String) (pos : Point) (font : Font) (color : Color) : IO Unit :=
  ctx.fillTextTransformed text pos font color Transform.identity

/-- Draw text at x, y coordinates with a font and color (identity transform). -/
def fillTextXY (ctx : DrawContext) (text : String) (x y : Float) (font : Font) (color : Color) : IO Unit :=
  ctx.fillText text ⟨x, y⟩ font color

/-- Measure the dimensions of text. Returns (width, height). -/
def measureText (_ : DrawContext) (text : String) (font : Font) : IO (Float × Float) :=
  Font.measureText font text

/-- Run a render loop until the window is closed. -/
def runLoop (ctx : DrawContext) (clearColor : Color) (draw : DrawContext → IO Unit) : IO Unit := do
  while !(← ctx.shouldClose) do
    ctx.pollEvents
    let ok ← ctx.beginFrame clearColor
    if ok then
      draw ctx
      ctx.endFrame

/-! ## Stateful Drawing API -/

/-- Fill a path using the current state (applies transform and uses state's fill style). -/
def fillPathWithState (ctx : DrawContext) (path : Path) (state : CanvasState) : IO Unit := do
  let transformedPath := state.transformPath path
  let style := state.effectiveFillStyle
  ctx.fillPathWithStyle transformedPath style

/-- Fill a rectangle using the current state. -/
def fillRectWithState (ctx : DrawContext) (rect : Rect) (state : CanvasState) : IO Unit := do
  let transformedPath := state.transformPath (Path.rectangle rect)
  let style := state.effectiveFillStyle
  ctx.fillPathWithStyle transformedPath style

/-- Fill a circle using the current state. -/
def fillCircleWithState (ctx : DrawContext) (center : Point) (radius : Float) (state : CanvasState) : IO Unit := do
  ctx.fillPathWithState (Path.circle center radius) state

/-- Run a stateful render loop with save/restore support.
    The draw function receives a mutable StateStack reference. -/
def runStatefulLoop (ctx : DrawContext) (clearColor : Color)
    (draw : DrawContext → StateStack → IO StateStack) : IO Unit := do
  let mut stack := StateStack.new
  while !(← ctx.shouldClose) do
    ctx.pollEvents
    let ok ← ctx.beginFrame clearColor
    if ok then
      stack ← draw ctx stack
      ctx.endFrame

end DrawContext

/-! ## Stateful Canvas - Higher-level API with automatic state management -/

/-- A canvas with built-in state management and optional batching. -/
structure Canvas where
  ctx : DrawContext
  stateStack : StateStack
  /-- Active batch accumulator. When Some, drawing ops add to batch instead of drawing immediately. -/
  batch : Option Batch := none
  /-- Pre-allocated buffer for instanced rendering (avoids per-frame allocation). -/
  instanceBuffer : Array Float := #[]
  /-- Capacity of instance buffer (in number of instances, not floats). -/
  instanceBufferCapacity : Nat := 0
  /-- High-performance mutable FloatBuffer for zero-copy instanced rendering. -/
  floatBuffer : Option FFI.FloatBuffer := none
  /-- Capacity of FloatBuffer (in floats). -/
  floatBufferCapacity : Nat := 0

namespace Canvas

/-- Create a new canvas with a window. -/
def create (width height : UInt32) (title : String) : IO Canvas := do
  let ctx ← DrawContext.create width height title
  pure { ctx, stateStack := StateStack.new }

/-- Get the current state. -/
def state (c : Canvas) : CanvasState :=
  c.stateStack.current

/-- Save the current state. -/
def save (c : Canvas) : Canvas :=
  { c with stateStack := c.stateStack.save }

/-- Restore the most recently saved state. -/
def restore (c : Canvas) : Canvas :=
  { c with stateStack := c.stateStack.restore }

/-- Modify the current state. -/
def modifyState (f : CanvasState → CanvasState) (c : Canvas) : Canvas :=
  { c with stateStack := c.stateStack.modify f }

/-! ## Transform operations -/

def translate (dx dy : Float) (c : Canvas) : Canvas :=
  { c with stateStack := c.stateStack.translate dx dy }

def rotate (angle : Float) (c : Canvas) : Canvas :=
  { c with stateStack := c.stateStack.rotate angle }

def scale (sx sy : Float) (c : Canvas) : Canvas :=
  { c with stateStack := c.stateStack.scale sx sy }

def scaleUniform (s : Float) (c : Canvas) : Canvas :=
  { c with stateStack := c.stateStack.scaleUniform s }

def resetTransform (c : Canvas) : Canvas :=
  { c with stateStack := c.stateStack.resetTransform }

/-! ## Style operations -/

def setFillColor (color : Color) (c : Canvas) : Canvas :=
  { c with stateStack := c.stateStack.setFillColor color }

def setStrokeColor (color : Color) (c : Canvas) : Canvas :=
  { c with stateStack := c.stateStack.setStrokeColor color }

def setLineWidth (w : Float) (c : Canvas) : Canvas :=
  { c with stateStack := c.stateStack.setLineWidth w }

def setGlobalAlpha (a : Float) (c : Canvas) : Canvas :=
  { c with stateStack := c.stateStack.setGlobalAlpha a }

def setFillStyle (style : FillStyle) (c : Canvas) : Canvas :=
  { c with stateStack := c.stateStack.setFillStyle style }

def setFillLinearGradient (start finish : Point) (stops : Array GradientStop) (c : Canvas) : Canvas :=
  { c with stateStack := c.stateStack.setFillLinearGradient start finish stops }

def setFillRadialGradient (center : Point) (radius : Float) (stops : Array GradientStop) (c : Canvas) : Canvas :=
  { c with stateStack := c.stateStack.setFillRadialGradient center radius stops }

/-! ## Batching API -/

/-- Start accumulating shapes into a batch instead of drawing them immediately.
    Use `flushBatch` to draw all accumulated shapes with a single draw call. -/
def beginBatch (c : Canvas) (capacityHint : Nat := 1000) : Canvas :=
  { c with batch := some (Batch.withCapacity capacityHint) }

/-- Flush the current batch, drawing all accumulated shapes with a single draw call.
    Returns the canvas with no active batch. -/
def flushBatch (c : Canvas) : IO Canvas := do
  match c.batch with
  | none => pure c
  | some batch =>
    c.ctx.drawBatch batch
    pure { c with batch := none }

/-- Check if batching is currently active. -/
def isBatching (c : Canvas) : Bool :=
  c.batch.isSome

/-- Execute an action with batching enabled.
    All shapes drawn within the action are batched and drawn with a single draw call at the end. -/
def batched (capacityHint : Nat := 1000) (action : Canvas → IO Canvas) (c : Canvas) : IO Canvas := do
  let c := c.beginBatch capacityHint
  let c ← action c
  c.flushBatch

/-- FASTEST PATH: Batch many rectangles with a pure function that computes geometry directly.
    The generator function takes an index and returns (x, y, angle, halfSize, color).
    This bypasses Canvas state entirely - no save/restore, no Transform allocations. -/
def batchRectsBy (count : Nat)
    (generator : Nat → Float × Float × Float × Float × Color)
    (c : Canvas) : IO Canvas := do
  let mut batch := Batch.withCapacity count
  for i in [:count] do
    let (x, y, angle, halfSize, color) := generator i
    batch := batch.addRectDirect x y angle halfSize color c.ctx.baseWidth c.ctx.baseHeight
  c.ctx.drawBatch batch
  pure c

/-- GPU INSTANCED: Render many rectangles with GPU-computed transforms.
    The generator function takes an index and returns (x, y, angle, halfSize, color).
    Transforms are computed on the GPU - maximum parallelism for large counts.
    Use this for 1000+ rectangles for best performance.
    Reuses a pre-allocated buffer to avoid per-frame allocation. -/
def batchInstancedRectsBy (count : Nat)
    (generator : Nat → Float × Float × Float × Float × Color)
    (c : Canvas) : IO Canvas := do
  let floatCount := count * 8
  -- Reuse existing buffer if large enough, otherwise grow it
  let data := if c.instanceBufferCapacity >= count then
      c.instanceBuffer
    else
      -- Allocate with some headroom to avoid frequent reallocation
      Array.replicate floatCount 0.0
  -- Fill instance data using set! for in-place mutation (8 floats per instance)
  let mut data := data
  for i in [:count] do
    let (x, y, angle, halfSize, color) := generator i
    -- Convert position to NDC
    let ndcX := (x / c.ctx.baseWidth) * 2.0 - 1.0
    let ndcY := 1.0 - (y / c.ctx.baseHeight) * 2.0
    -- Convert halfSize to NDC (use width for uniform scale)
    let ndcHalfSize := halfSize / c.ctx.baseWidth * 2.0
    -- Pack instance data using set! (in-place mutation)
    let base := i * 8
    data := data.set! base ndcX
    data := data.set! (base + 1) ndcY
    data := data.set! (base + 2) angle
    data := data.set! (base + 3) ndcHalfSize
    data := data.set! (base + 4) color.r
    data := data.set! (base + 5) color.g
    data := data.set! (base + 6) color.b
    data := data.set! (base + 7) color.a
  -- Single GPU draw call with instancing
  FFI.Renderer.drawInstancedRects c.ctx.renderer data count.toUInt32
  -- Return canvas with buffer for reuse next frame
  pure { c with instanceBuffer := data, instanceBufferCapacity := count }

/-- Pre-computed particle data for ultra-fast instanced rendering.
    Store static per-particle values that don't change each frame. -/
structure ParticleData where
  /-- Pre-computed: phase, baseRadius, orbitSpeed, phaseX3, phase2, hueBase (6 floats per particle) -/
  staticData : Array Float
  /-- Number of particles -/
  count : Nat
  /-- Half size of each particle (uniform) -/
  halfSize : Float
  /-- Center X coordinate -/
  centerX : Float
  /-- Center Y coordinate -/
  centerY : Float

/-- Create pre-computed particle data for orbital animation.
    Call once at startup, then use renderParticles each frame. -/
def ParticleData.create (count : Nat) (centerX centerY halfSize : Float) : ParticleData :=
  let pi := 3.14159265358979323846
  let countF := count.toFloat
  -- 6 floats per particle: phase, baseRadius, orbitSpeed, phaseX3, phase2, hueBase
  let data := Id.run do
    let mut data := Array.replicate (count * 6) 0.0
    for i in [:count] do
      let phase := i.toFloat * (2.0 * pi / countF)
      let ring := i % 10
      let baseRadius := 50.0 + ring.toFloat * 45.0
      let orbitSpeed := 1.0 + ring.toFloat * 0.2
      let base := i * 6
      data := data.set! base phase
      data := data.set! (base + 1) baseRadius
      data := data.set! (base + 2) orbitSpeed
      data := data.set! (base + 3) (phase * 3.0)      -- pre-multiply for orbit pulsing
      data := data.set! (base + 4) (phase * 2.0)      -- pre-multiply for rotation
      data := data.set! (base + 5) (i.toFloat / countF)  -- hue base (0 to 1)
    pure data
  { staticData := data, count, halfSize, centerX, centerY }

/-- Pre-computed grid particle data - even simpler, just positions. -/
structure GridParticleData where
  /-- Pre-computed: x, y, hueBase (3 floats per particle) -/
  staticData : Array Float
  /-- Number of particles -/
  count : Nat
  /-- Half size of each particle -/
  halfSize : Float

/-- Create pre-computed grid particle data.
    Particles arranged in a grid, each spinning in place. -/
def GridParticleData.create (cols rows : Nat) (startX startY spacing halfSize : Float) : GridParticleData :=
  let count := cols * rows
  let countF := count.toFloat
  let data := Id.run do
    let mut data := Array.replicate (count * 3) 0.0
    for row in [:rows] do
      for col in [:cols] do
        let i := row * cols + col
        let x := startX + col.toFloat * spacing
        let y := startY + row.toFloat * spacing
        let base := i * 3
        data := data.set! base x
        data := data.set! (base + 1) y
        data := data.set! (base + 2) (i.toFloat / countF)  -- hue base
    pure data
  { staticData := data, count, halfSize }

/-- ULTRA-FAST: Render grid particles - just spinning in place, no orbital motion.
    Only 1 angle computation per particle (no trig in inner loop - GPU does it). -/
def batchInstancedGridParticles (particles : GridParticleData) (t : Float)
    (c : Canvas) : IO Canvas := do
  let count := particles.count
  let floatCount := count * 8
  let data := if c.instanceBufferCapacity >= count then
      c.instanceBuffer
    else
      Array.replicate floatCount 0.0
  let tHue := t * 0.3
  let tSpin := t * 3.0
  let mut data := data
  for i in [:count] do
    let sbase := i * 3
    let x := particles.staticData[sbase]!
    let y := particles.staticData[sbase + 1]!
    let hueBase := particles.staticData[sbase + 2]!
    -- Simple spin - angle varies by position for wave effect
    let angle := tSpin + hueBase * 6.28318  -- Each particle has different phase
    -- Rainbow color
    let hue := (tHue + hueBase) - (tHue + hueBase).floor
    let h6 := hue * 6.0
    let sector := h6.floor
    let f := h6 - sector
    let q := 1.0 - 0.9 * f
    let t' := 1.0 - 0.9 * (1.0 - f)
    let (r, g, b) := match sector.toUInt8 % 6 with
      | 0 => (1.0, t', 0.1)
      | 1 => (q, 1.0, 0.1)
      | 2 => (0.1, 1.0, t')
      | 3 => (0.1, q, 1.0)
      | 4 => (t', 0.1, 1.0)
      | _ => (1.0, 0.1, q)
    let ndcX := (x / c.ctx.baseWidth) * 2.0 - 1.0
    let ndcY := 1.0 - (y / c.ctx.baseHeight) * 2.0
    let ndcHalfSize := particles.halfSize / c.ctx.baseWidth * 2.0
    let base := i * 8
    data := data.set! base ndcX
    data := data.set! (base + 1) ndcY
    data := data.set! (base + 2) angle
    data := data.set! (base + 3) ndcHalfSize
    data := data.set! (base + 4) r
    data := data.set! (base + 5) g
    data := data.set! (base + 6) b
    data := data.set! (base + 7) 1.0
  FFI.Renderer.drawInstancedRects c.ctx.renderer data count.toUInt32
  pure { c with instanceBuffer := data, instanceBufferCapacity := count }

/-- ULTRA-FAST: Render particles using pre-computed static data.
    Only computes time-varying values each frame. Minimal trig calls. -/
def batchInstancedParticles (particles : ParticleData) (t : Float)
    (c : Canvas) : IO Canvas := do
  let count := particles.count
  let floatCount := count * 8
  -- Reuse existing buffer if large enough
  let data := if c.instanceBufferCapacity >= count then
      c.instanceBuffer
    else
      Array.replicate floatCount 0.0
  -- Pre-compute time-based values (computed once, not per-particle)
  let tHalf := t * 0.5
  let tTriple := t * 3.0
  let tHue := t * 0.3
  -- Fill instance data
  let mut data := data
  for i in [:count] do
    let sbase := i * 6
    let phase := particles.staticData[sbase]!
    let baseRadius := particles.staticData[sbase + 1]!
    let orbitSpeed := particles.staticData[sbase + 2]!
    let phaseX3 := particles.staticData[sbase + 3]!
    let phase2 := particles.staticData[sbase + 4]!
    let hueBase := particles.staticData[sbase + 5]!
    -- Time-varying computations (3 trig calls per particle instead of 5)
    let orbitAngle := t * orbitSpeed + phase
    let orbitRadius := baseRadius + 30.0 * Float.sin (tHalf + phaseX3)
    let x := particles.centerX + orbitRadius * Float.cos orbitAngle
    let y := particles.centerY + orbitRadius * Float.sin orbitAngle
    let angle := tTriple + phase2
    -- Rainbow color (no trig - just hue cycling)
    let hue := (tHue + hueBase) - (tHue + hueBase).floor
    -- Inline HSV to RGB (avoid function call overhead)
    let h6 := hue * 6.0
    let sector := h6.floor
    let f := h6 - sector
    let q := 1.0 - 0.9 * f
    let t' := 1.0 - 0.9 * (1.0 - f)
    let (r, g, b) := match sector.toUInt8 % 6 with
      | 0 => (1.0, t', 0.1)
      | 1 => (q, 1.0, 0.1)
      | 2 => (0.1, 1.0, t')
      | 3 => (0.1, q, 1.0)
      | 4 => (t', 0.1, 1.0)
      | _ => (1.0, 0.1, q)
    -- Convert to NDC and pack
    let ndcX := (x / c.ctx.baseWidth) * 2.0 - 1.0
    let ndcY := 1.0 - (y / c.ctx.baseHeight) * 2.0
    let ndcHalfSize := particles.halfSize / c.ctx.baseWidth * 2.0
    let base := i * 8
    data := data.set! base ndcX
    data := data.set! (base + 1) ndcY
    data := data.set! (base + 2) angle
    data := data.set! (base + 3) ndcHalfSize
    data := data.set! (base + 4) r
    data := data.set! (base + 5) g
    data := data.set! (base + 6) b
    data := data.set! (base + 7) 1.0
  FFI.Renderer.drawInstancedRects c.ctx.renderer data count.toUInt32
  pure { c with instanceBuffer := data, instanceBufferCapacity := count }

/-- ULTRA-FAST: Render grid of spinning triangles.
    Same as grid particles but renders triangles instead of squares. -/
def batchInstancedGridTriangles (particles : GridParticleData) (t : Float)
    (c : Canvas) : IO Canvas := do
  let count := particles.count
  let floatCount := count * 8
  let data := if c.instanceBufferCapacity >= count then
      c.instanceBuffer
    else
      Array.replicate floatCount 0.0
  let tHue := t * 0.3
  let tSpin := t * 2.0  -- Slower spin for triangles
  let mut data := data
  for i in [:count] do
    let sbase := i * 3
    let x := particles.staticData[sbase]!
    let y := particles.staticData[sbase + 1]!
    let hueBase := particles.staticData[sbase + 2]!
    let angle := tSpin + hueBase * 6.28318
    let hue := (tHue + hueBase) - (tHue + hueBase).floor
    let h6 := hue * 6.0
    let sector := h6.floor
    let f := h6 - sector
    let q := 1.0 - 0.9 * f
    let t' := 1.0 - 0.9 * (1.0 - f)
    let (r, g, b) := match sector.toUInt8 % 6 with
      | 0 => (1.0, t', 0.1)
      | 1 => (q, 1.0, 0.1)
      | 2 => (0.1, 1.0, t')
      | 3 => (0.1, q, 1.0)
      | 4 => (t', 0.1, 1.0)
      | _ => (1.0, 0.1, q)
    let ndcX := (x / c.ctx.baseWidth) * 2.0 - 1.0
    let ndcY := 1.0 - (y / c.ctx.baseHeight) * 2.0
    let ndcHalfSize := particles.halfSize / c.ctx.baseWidth * 2.0
    let base := i * 8
    data := data.set! base ndcX
    data := data.set! (base + 1) ndcY
    data := data.set! (base + 2) angle
    data := data.set! (base + 3) ndcHalfSize
    data := data.set! (base + 4) r
    data := data.set! (base + 5) g
    data := data.set! (base + 6) b
    data := data.set! (base + 7) 1.0
  FFI.Renderer.drawInstancedTriangles c.ctx.renderer data count.toUInt32
  pure { c with instanceBuffer := data, instanceBufferCapacity := count }

/-- Bouncing particle data - stores position, velocity, and color info. -/
structure BouncingParticleData where
  /-- Per-particle: x, y, vx, vy, hueBase (5 floats) -/
  particleState : Array Float
  /-- Number of particles -/
  count : Nat
  /-- Radius of each circle -/
  radius : Float
  /-- Screen bounds -/
  screenWidth : Float
  screenHeight : Float

/-- Create bouncing particles with random initial positions and velocities. -/
def BouncingParticleData.create (count : Nat) (screenWidth screenHeight radius : Float) (seed : Nat) : BouncingParticleData :=
  let data := Id.run do
    let mut data := Array.replicate (count * 5) 0.0
    let mut rng := seed
    for i in [:count] do
      -- Simple LCG random number generator
      rng := (rng * 1103515245 + 12345) % (2^31)
      let rx := (rng % 1000).toFloat / 1000.0
      rng := (rng * 1103515245 + 12345) % (2^31)
      let ry := (rng % 1000).toFloat / 1000.0
      rng := (rng * 1103515245 + 12345) % (2^31)
      let rvx := ((rng % 1000).toFloat / 1000.0 - 0.5) * 2.0
      rng := (rng * 1103515245 + 12345) % (2^31)
      let rvy := ((rng % 1000).toFloat / 1000.0 - 0.5) * 2.0
      let base := i * 5
      -- Position (with margin from edges)
      data := data.set! base (radius + rx * (screenWidth - 2.0 * radius))
      data := data.set! (base + 1) (radius + ry * (screenHeight - 2.0 * radius))
      -- Velocity (pixels per second)
      data := data.set! (base + 2) (rvx * 300.0)
      data := data.set! (base + 3) (rvy * 300.0)
      -- Hue
      data := data.set! (base + 4) (i.toFloat / count.toFloat)
    pure data
  { particleState := data, count, radius, screenWidth, screenHeight }

/-- Update bouncing particles (call each frame with delta time).
    Returns updated particle data with new positions. -/
def BouncingParticleData.update (p : BouncingParticleData) (dt : Float) : BouncingParticleData :=
  let data := Id.run do
    let mut data := p.particleState
    for i in [:p.count] do
      let base := i * 5
      let mut x := data[base]!
      let mut y := data[base + 1]!
      let mut vx := data[base + 2]!
      let mut vy := data[base + 3]!
      -- Update position
      x := x + vx * dt
      y := y + vy * dt
      -- Bounce off walls
      if x < p.radius then
        x := p.radius
        vx := -vx
      if x > p.screenWidth - p.radius then
        x := p.screenWidth - p.radius
        vx := -vx
      if y < p.radius then
        y := p.radius
        vy := -vy
      if y > p.screenHeight - p.radius then
        y := p.screenHeight - p.radius
        vy := -vy
      data := data.set! base x
      data := data.set! (base + 1) y
      data := data.set! (base + 2) vx
      data := data.set! (base + 3) vy
    pure data
  { p with particleState := data }

/-- Render bouncing circles. -/
def batchInstancedBouncingCircles (particles : BouncingParticleData) (t : Float)
    (c : Canvas) : IO Canvas := do
  let count := particles.count
  let floatCount := count * 8
  let data := if c.instanceBufferCapacity >= count then
      c.instanceBuffer
    else
      Array.replicate floatCount 0.0
  let tHue := t * 0.2
  let mut data := data
  for i in [:count] do
    let pbase := i * 5
    let x := particles.particleState[pbase]!
    let y := particles.particleState[pbase + 1]!
    let hueBase := particles.particleState[pbase + 4]!
    let hue := (tHue + hueBase) - (tHue + hueBase).floor
    let h6 := hue * 6.0
    let sector := h6.floor
    let f := h6 - sector
    let q := 1.0 - 0.9 * f
    let t' := 1.0 - 0.9 * (1.0 - f)
    let (r, g, b) := match sector.toUInt8 % 6 with
      | 0 => (1.0, t', 0.1)
      | 1 => (q, 1.0, 0.1)
      | 2 => (0.1, 1.0, t')
      | 3 => (0.1, q, 1.0)
      | 4 => (t', 0.1, 1.0)
      | _ => (1.0, 0.1, q)
    let ndcX := (x / c.ctx.baseWidth) * 2.0 - 1.0
    let ndcY := 1.0 - (y / c.ctx.baseHeight) * 2.0
    let ndcRadius := particles.radius / c.ctx.baseWidth * 2.0
    let base := i * 8
    data := data.set! base ndcX
    data := data.set! (base + 1) ndcY
    data := data.set! (base + 2) 0.0  -- angle (unused for circles)
    data := data.set! (base + 3) ndcRadius
    data := data.set! (base + 4) r
    data := data.set! (base + 5) g
    data := data.set! (base + 6) b
    data := data.set! (base + 7) 1.0
  FFI.Renderer.drawInstancedCircles c.ctx.renderer data count.toUInt32
  pure { c with instanceBuffer := data, instanceBufferCapacity := count }

/-! ## Zero-Copy FloatBuffer API (Maximum Performance) -/

/-- Ensure the FloatBuffer exists with sufficient capacity.
    Creates or grows the buffer as needed. -/
def ensureFloatBuffer (neededFloats : Nat) (c : Canvas) : IO Canvas := do
  if c.floatBufferCapacity >= neededFloats then
    pure c
  else
    -- Destroy old buffer if exists
    if let some buf := c.floatBuffer then
      FFI.FloatBuffer.destroy buf
    -- Create new larger buffer (with some headroom)
    let capacity := neededFloats + neededFloats / 4  -- 25% headroom
    let buf ← FFI.FloatBuffer.create capacity.toUSize
    pure { c with floatBuffer := some buf, floatBufferCapacity := capacity }

/-- MAXIMUM PERFORMANCE: Render grid particles using zero-copy FloatBuffer.
    Eliminates 80,000 array allocations per frame for 10k particles. -/
def batchInstancedGridParticlesFast (particles : GridParticleData) (t : Float)
    (c : Canvas) : IO Canvas := do
  let count := particles.count
  let floatCount := count * 8
  let c ← c.ensureFloatBuffer floatCount
  let some buf := c.floatBuffer | pure c  -- Should never fail after ensureFloatBuffer
  let tHue := t * 0.3
  let tSpin := t * 3.0
  for i in [:count] do
    let sbase := i * 3
    let x := particles.staticData[sbase]!
    let y := particles.staticData[sbase + 1]!
    let hueBase := particles.staticData[sbase + 2]!
    let angle := tSpin + hueBase * 6.28318
    let hue := (tHue + hueBase) - (tHue + hueBase).floor
    let h6 := hue * 6.0
    let sector := h6.floor
    let f := h6 - sector
    let q := 1.0 - 0.9 * f
    let t' := 1.0 - 0.9 * (1.0 - f)
    let (r, g, b) := match sector.toUInt8 % 6 with
      | 0 => (1.0, t', 0.1)
      | 1 => (q, 1.0, 0.1)
      | 2 => (0.1, 1.0, t')
      | 3 => (0.1, q, 1.0)
      | 4 => (t', 0.1, 1.0)
      | _ => (1.0, 0.1, q)
    let ndcX := (x / c.ctx.baseWidth) * 2.0 - 1.0
    let ndcY := 1.0 - (y / c.ctx.baseHeight) * 2.0
    let ndcHalfSize := particles.halfSize / c.ctx.baseWidth * 2.0
    let base := (i * 8).toUSize
    -- Single FFI call for 8 floats - 8x less overhead!
    FFI.FloatBuffer.setVec8 buf base ndcX ndcY angle ndcHalfSize r g b 1.0
  FFI.Renderer.drawInstancedRectsBuffer c.ctx.renderer buf count.toUInt32
  pure c

/-- MAXIMUM PERFORMANCE: Render orbital particles using zero-copy FloatBuffer. -/
def batchInstancedParticlesFast (particles : ParticleData) (t : Float)
    (c : Canvas) : IO Canvas := do
  let count := particles.count
  let floatCount := count * 8
  let c ← c.ensureFloatBuffer floatCount
  let some buf := c.floatBuffer | pure c
  let tHalf := t * 0.5
  let tTriple := t * 3.0
  let tHue := t * 0.3
  for i in [:count] do
    let sbase := i * 6
    let phase := particles.staticData[sbase]!
    let baseRadius := particles.staticData[sbase + 1]!
    let orbitSpeed := particles.staticData[sbase + 2]!
    let phaseX3 := particles.staticData[sbase + 3]!
    let phase2 := particles.staticData[sbase + 4]!
    let hueBase := particles.staticData[sbase + 5]!
    let orbitAngle := t * orbitSpeed + phase
    let orbitRadius := baseRadius + 30.0 * Float.sin (tHalf + phaseX3)
    let x := particles.centerX + orbitRadius * Float.cos orbitAngle
    let y := particles.centerY + orbitRadius * Float.sin orbitAngle
    let angle := tTriple + phase2
    let hue := (tHue + hueBase) - (tHue + hueBase).floor
    let h6 := hue * 6.0
    let sector := h6.floor
    let f := h6 - sector
    let q := 1.0 - 0.9 * f
    let t' := 1.0 - 0.9 * (1.0 - f)
    let (r, g, b) := match sector.toUInt8 % 6 with
      | 0 => (1.0, t', 0.1)
      | 1 => (q, 1.0, 0.1)
      | 2 => (0.1, 1.0, t')
      | 3 => (0.1, q, 1.0)
      | 4 => (t', 0.1, 1.0)
      | _ => (1.0, 0.1, q)
    let ndcX := (x / c.ctx.baseWidth) * 2.0 - 1.0
    let ndcY := 1.0 - (y / c.ctx.baseHeight) * 2.0
    let ndcHalfSize := particles.halfSize / c.ctx.baseWidth * 2.0
    let base := (i * 8).toUSize
    FFI.FloatBuffer.setVec8 buf base ndcX ndcY angle ndcHalfSize r g b 1.0
  FFI.Renderer.drawInstancedRectsBuffer c.ctx.renderer buf count.toUInt32
  pure c

/-- MAXIMUM PERFORMANCE: Render grid triangles using zero-copy FloatBuffer. -/
def batchInstancedGridTrianglesFast (particles : GridParticleData) (t : Float)
    (c : Canvas) : IO Canvas := do
  let count := particles.count
  let floatCount := count * 8
  let c ← c.ensureFloatBuffer floatCount
  let some buf := c.floatBuffer | pure c
  let tHue := t * 0.3
  let tSpin := t * 2.0
  for i in [:count] do
    let sbase := i * 3
    let x := particles.staticData[sbase]!
    let y := particles.staticData[sbase + 1]!
    let hueBase := particles.staticData[sbase + 2]!
    let angle := tSpin + hueBase * 6.28318
    let hue := (tHue + hueBase) - (tHue + hueBase).floor
    let h6 := hue * 6.0
    let sector := h6.floor
    let f := h6 - sector
    let q := 1.0 - 0.9 * f
    let t' := 1.0 - 0.9 * (1.0 - f)
    let (r, g, b) := match sector.toUInt8 % 6 with
      | 0 => (1.0, t', 0.1)
      | 1 => (q, 1.0, 0.1)
      | 2 => (0.1, 1.0, t')
      | 3 => (0.1, q, 1.0)
      | 4 => (t', 0.1, 1.0)
      | _ => (1.0, 0.1, q)
    let ndcX := (x / c.ctx.baseWidth) * 2.0 - 1.0
    let ndcY := 1.0 - (y / c.ctx.baseHeight) * 2.0
    let ndcHalfSize := particles.halfSize / c.ctx.baseWidth * 2.0
    let base := (i * 8).toUSize
    FFI.FloatBuffer.setVec8 buf base ndcX ndcY angle ndcHalfSize r g b 1.0
  FFI.Renderer.drawInstancedTrianglesBuffer c.ctx.renderer buf count.toUInt32
  pure c

/-- MAXIMUM PERFORMANCE: Render bouncing circles using zero-copy FloatBuffer. -/
def batchInstancedBouncingCirclesFast (particles : BouncingParticleData) (t : Float)
    (c : Canvas) : IO Canvas := do
  let count := particles.count
  let floatCount := count * 8
  let c ← c.ensureFloatBuffer floatCount
  let some buf := c.floatBuffer | pure c
  let tHue := t * 0.2
  for i in [:count] do
    let pbase := i * 5
    let x := particles.particleState[pbase]!
    let y := particles.particleState[pbase + 1]!
    let hueBase := particles.particleState[pbase + 4]!
    let hue := (tHue + hueBase) - (tHue + hueBase).floor
    let h6 := hue * 6.0
    let sector := h6.floor
    let f := h6 - sector
    let q := 1.0 - 0.9 * f
    let t' := 1.0 - 0.9 * (1.0 - f)
    let (r, g, b) := match sector.toUInt8 % 6 with
      | 0 => (1.0, t', 0.1)
      | 1 => (q, 1.0, 0.1)
      | 2 => (0.1, 1.0, t')
      | 3 => (0.1, q, 1.0)
      | 4 => (t', 0.1, 1.0)
      | _ => (1.0, 0.1, q)
    let ndcX := (x / c.ctx.baseWidth) * 2.0 - 1.0
    let ndcY := 1.0 - (y / c.ctx.baseHeight) * 2.0
    let ndcRadius := particles.radius / c.ctx.baseWidth * 2.0
    let base := (i * 8).toUSize
    FFI.FloatBuffer.setVec8 buf base ndcX ndcY 0.0 ndcRadius r g b 1.0
  FFI.Renderer.drawInstancedCirclesBuffer c.ctx.renderer buf count.toUInt32
  pure c

/-- Clean up FloatBuffer when destroying canvas. -/
def destroyFloatBuffer (c : Canvas) : IO Unit := do
  if let some buf := c.floatBuffer then
    FFI.FloatBuffer.destroy buf

/-! ## GPU-Animated Rendering (Pixi.js Style)
    Static data uploaded once at startup, only time sent per frame.
    GPU computes: angle, HSV→RGB, pixel→NDC conversion -/

/-- Build static data array for animated grid particles.
    Format: [pixelX, pixelY, hueBase, halfSizePixels, phaseOffset, spinSpeed] × count -/
def buildAnimatedGridData (particles : GridParticleData) (spinSpeed : Float := 3.0) : Array Float := Id.run do
  let mut data := Array.mkEmpty (particles.count * 6)
  for i in [:particles.count] do
    let sbase := i * 3
    let x := particles.staticData[sbase]!
    let y := particles.staticData[sbase + 1]!
    let hueBase := particles.staticData[sbase + 2]!
    data := data.push x
    data := data.push y
    data := data.push hueBase
    data := data.push particles.halfSize
    data := data.push (hueBase * 6.28318)  -- phase offset
    data := data.push spinSpeed
  data

/-- Build static data array for animated orbital particles.
    Note: This needs special handling since orbitals have dynamic positions.
    For now, we use grid layout instead (positions won't orbit). -/
def buildAnimatedOrbitalData (particles : ParticleData) (spinSpeed : Float := 3.0) : Array Float := Id.run do
  -- For orbital particles, we need to compute initial positions
  -- Since position depends on time, animated orbital would need a different shader
  -- For now, fall back to using the center position (will just spin in place)
  let mut data := Array.mkEmpty (particles.count * 6)
  for i in [:particles.count] do
    let sbase := i * 6
    let phase := particles.staticData[sbase]!
    let baseRadius := particles.staticData[sbase + 1]!
    let hueBase := particles.staticData[sbase + 5]!
    -- Use initial orbit position (t=0)
    let x := particles.centerX + baseRadius * Float.cos phase
    let y := particles.centerY + baseRadius * Float.sin phase
    data := data.push x
    data := data.push y
    data := data.push hueBase
    data := data.push particles.halfSize
    data := data.push (phase)  -- phase offset
    data := data.push spinSpeed
  data

/-- Build static data array for animated circles (bouncing circles need special handling).
    For static circles, we just use fixed positions with color animation. -/
def buildAnimatedCircleData (particles : BouncingParticleData) : Array Float := Id.run do
  let mut data := Array.mkEmpty (particles.count * 6)
  for i in [:particles.count] do
    let pbase := i * 5
    let x := particles.particleState[pbase]!
    let y := particles.particleState[pbase + 1]!
    let hueBase := particles.particleState[pbase + 4]!
    data := data.push x
    data := data.push y
    data := data.push hueBase
    data := data.push particles.radius
    data := data.push 0.0  -- no rotation for circles
    data := data.push 0.0  -- no spin speed for circles
  data

/-- Upload animated grid particle data to GPU (call once at startup).
    After this, use drawAnimatedRects to render with just a time value. -/
def uploadAnimatedGridRects (particles : GridParticleData) (spinSpeed : Float := 3.0) (c : Canvas) : IO Unit := do
  let data := buildAnimatedGridData particles spinSpeed
  FFI.Renderer.uploadAnimatedRects c.ctx.renderer data particles.count.toUInt32

/-- Upload animated grid triangles to GPU (call once at startup). -/
def uploadAnimatedGridTriangles (particles : GridParticleData) (spinSpeed : Float := 2.0) (c : Canvas) : IO Unit := do
  let data := buildAnimatedGridData particles spinSpeed
  FFI.Renderer.uploadAnimatedTriangles c.ctx.renderer data particles.count.toUInt32

/-- Upload animated circles to GPU (call once at startup). -/
def uploadAnimatedCircles (particles : BouncingParticleData) (c : Canvas) : IO Unit := do
  let data := buildAnimatedCircleData particles
  FFI.Renderer.uploadAnimatedCircles c.ctx.renderer data particles.count.toUInt32

/-- Draw animated rects - GPU does all animation! Only sends time value.
    Call uploadAnimatedGridRects once first, then call this every frame. -/
def drawAnimatedRects (t : Float) (c : Canvas) : IO Canvas := do
  FFI.Renderer.drawAnimatedRects c.ctx.renderer t
  pure c

/-- Draw animated triangles - GPU does all animation! Only sends time value. -/
def drawAnimatedTriangles (t : Float) (c : Canvas) : IO Canvas := do
  FFI.Renderer.drawAnimatedTriangles c.ctx.renderer t
  pure c

/-- Draw animated circles - GPU does all animation! Only sends time value. -/
def drawAnimatedCircles (t : Float) (c : Canvas) : IO Canvas := do
  FFI.Renderer.drawAnimatedCircles c.ctx.renderer t
  pure c

/-! ## GPU-Animated Orbital Particles
    Particles orbit around a center point with position computed on GPU.
    Static data uploaded once at startup, only time sent per frame. -/

/-- Build static data array for GPU-animated orbital particles.
    Format: [phase, baseRadius, orbitSpeed, phaseX3, phase2, hueBase, halfSizePixels, padding] × count
    This data is uploaded once and the GPU computes position, rotation, and color each frame. -/
def buildOrbitalData (particles : ParticleData) : Array Float := Id.run do
  let mut data := Array.mkEmpty (particles.count * 8)
  for i in [:particles.count] do
    let sbase := i * 6
    let phase := particles.staticData[sbase]!
    let baseRadius := particles.staticData[sbase + 1]!
    let orbitSpeed := particles.staticData[sbase + 2]!
    let phaseX3 := particles.staticData[sbase + 3]!
    let phase2 := particles.staticData[sbase + 4]!
    let hueBase := particles.staticData[sbase + 5]!
    data := data.push phase
    data := data.push baseRadius
    data := data.push orbitSpeed
    data := data.push phaseX3
    data := data.push phase2
    data := data.push hueBase
    data := data.push particles.halfSize
    data := data.push 0.0  -- padding
  data

/-- Upload orbital particle data to GPU (call once at startup).
    After this, use drawOrbitalParticles to render with just a time value.
    GPU computes: orbital position, spin angle, HSV→RGB, pixel→NDC. -/
def uploadOrbitalParticles (particles : ParticleData) (c : Canvas) : IO Unit := do
  let data := buildOrbitalData particles
  FFI.Renderer.uploadOrbitalParticles c.ctx.renderer data particles.count.toUInt32 particles.centerX particles.centerY

/-- Draw orbital particles - GPU does all animation! Only sends time value.
    Call uploadOrbitalParticles once first, then call this every frame. -/
def drawOrbitalParticles (t : Float) (c : Canvas) : IO Canvas := do
  FFI.Renderer.drawOrbitalParticles c.ctx.renderer t
  pure c

/-! ## Dynamic Circle Rendering
    CPU updates positions each frame, GPU does HSV->RGB and pixel->NDC.
    Cuts per-frame data transfer in half (4 floats vs 8 floats per circle). -/

/-- Build dynamic circle data from bouncing particles.
    Format: [pixelX, pixelY, hueBase, radiusPixels] × count (4 floats per circle) -/
def buildDynamicCircleData (particles : BouncingParticleData) : Array Float := Id.run do
  let mut data := Array.mkEmpty (particles.count * 4)
  for i in [:particles.count] do
    let pbase := i * 5
    let x := particles.particleState[pbase]!
    let y := particles.particleState[pbase + 1]!
    let hueBase := particles.particleState[pbase + 4]!
    data := data.push x
    data := data.push y
    data := data.push hueBase
    data := data.push particles.radius
  data

/-- Draw dynamic circles - GPU does color + NDC conversion!
    Positions updated each frame from CPU, but HSV->RGB and pixel->NDC done on GPU.
    Half the data transfer compared to full instance data. -/
def drawDynamicCircles (particles : BouncingParticleData) (t : Float) (c : Canvas) : IO Canvas := do
  let data := buildDynamicCircleData particles
  FFI.Renderer.drawDynamicCircles c.ctx.renderer data particles.count.toUInt32 t
  pure c

/-! ## Drawing operations -/

/-- Fill a path using the current state. Batch-aware: adds to batch if active. -/
def fillPath (path : Path) (c : Canvas) : IO Canvas := do
  let transformedPath := c.state.transformPath path
  let style := c.state.effectiveFillStyle
  let result := Tessellation.tessellateConvexPathFillNDC transformedPath style c.ctx.baseWidth c.ctx.baseHeight
  match c.batch with
  | some batch =>
    pure { c with batch := some (batch.add result) }
  | none =>
    c.ctx.fillPathWithStyle transformedPath style
    pure c

/-- Fill a rectangle using the current state. Batch-aware: adds to batch if active.
    Uses fast path that skips Path allocation - just transforms 4 corners directly. -/
def fillRect (rect : Rect) (c : Canvas) : IO Canvas := do
  let transform := c.state.transform
  let style := c.state.effectiveFillStyle
  match c.batch with
  | some batch =>
    -- FAST PATH: write directly into batch arrays, no intermediate allocation
    let batch' := batch.addTransformedRect rect transform style c.ctx.baseWidth c.ctx.baseHeight
    pure { c with batch := some batch' }
  | none =>
    -- Non-batched: use normal tessellation
    c.ctx.fillTransformedRectWithStyle rect transform style
    pure c

/-- Fill a rectangle specified by x, y, width, height using current state. -/
def fillRectXYWH (x y width height : Float) (c : Canvas) : IO Canvas :=
  c.fillRect (Rect.mk' x y width height)

/-- Fill a circle using the current state. Batch-aware: adds to batch if active. -/
def fillCircle (center : Point) (radius : Float) (c : Canvas) : IO Canvas :=
  c.fillPath (Path.circle center radius)

/-- Fill an ellipse using the current state. Batch-aware: adds to batch if active. -/
def fillEllipse (center : Point) (radiusX radiusY : Float) (c : Canvas) : IO Canvas :=
  c.fillPath (Path.ellipse center radiusX radiusY)

/-- Fill a rounded rectangle using the current state. Batch-aware: adds to batch if active. -/
def fillRoundedRect (rect : Rect) (cornerRadius : Float) (c : Canvas) : IO Canvas :=
  c.fillPath (Path.roundedRect rect cornerRadius)

/-! ## Stroke operations -/

/-- Get the effective stroke style with transform and global alpha applied. -/
private def effectiveStrokeStyle (c : Canvas) : StrokeStyle :=
  let state := c.state
  { state.strokeStyle with
    color := state.effectiveStrokeColor }

/-- Stroke a path using the current state. Batch-aware: adds to batch if active. -/
def strokePath (path : Path) (c : Canvas) : IO Canvas := do
  let transformedPath := c.state.transformPath path
  let style := c.effectiveStrokeStyle
  let result := Tessellation.tessellateStrokeNDC transformedPath style c.ctx.baseWidth c.ctx.baseHeight
  match c.batch with
  | some batch =>
    pure { c with batch := some (batch.add result) }
  | none =>
    c.ctx.strokePath transformedPath style
    pure c

/-- Stroke a rectangle using the current state. -/
def strokeRect (rect : Rect) (c : Canvas) : IO Canvas :=
  c.strokePath (Path.rectangle rect)

/-- Stroke a rectangle specified by x, y, width, height using current state. -/
def strokeRectXYWH (x y width height : Float) (c : Canvas) : IO Canvas :=
  c.strokeRect (Rect.mk' x y width height)

/-- Stroke a circle using the current state. -/
def strokeCircle (center : Point) (radius : Float) (c : Canvas) : IO Canvas :=
  c.strokePath (Path.circle center radius)

/-- Stroke an ellipse using the current state. -/
def strokeEllipse (center : Point) (radiusX radiusY : Float) (c : Canvas) : IO Canvas :=
  c.strokePath (Path.ellipse center radiusX radiusY)

/-- Stroke a rounded rectangle using the current state. -/
def strokeRoundedRect (rect : Rect) (cornerRadius : Float) (c : Canvas) : IO Canvas :=
  c.strokePath (Path.roundedRect rect cornerRadius)

/-- Draw a line from p1 to p2 using the current state. -/
def drawLine (p1 p2 : Point) (c : Canvas) : IO Canvas :=
  c.strokePath (Path.empty |>.moveTo p1 |>.lineTo p2)

/-! ## Text operations -/

/-- Draw text at a position with a font using the current fill color and transform.
    Note: Text uses a different shader and cannot be batched with shapes.
    If batching is active, the batch is flushed before drawing text. -/
def fillText (text : String) (pos : Point) (font : Font) (c : Canvas) : IO Canvas := do
  -- Flush any pending batch since text uses different pipeline
  let c ← c.flushBatch
  let color := c.state.effectiveFillColor
  let transform := c.state.transform
  c.ctx.fillTextTransformed text pos font color transform
  pure c

/-- Draw text at x, y coordinates with a font using the current fill color and transform. -/
def fillTextXY (text : String) (x y : Float) (font : Font) (c : Canvas) : IO Canvas :=
  c.fillText text ⟨x, y⟩ font

/-- Draw text with an explicit color (still uses current transform). -/
def fillTextColor (text : String) (pos : Point) (font : Font) (color : Color) (c : Canvas) : IO Canvas := do
  -- Flush any pending batch since text uses different pipeline
  let c ← c.flushBatch
  let transform := c.state.transform
  c.ctx.fillTextTransformed text pos font color transform
  pure c

/-- Measure text dimensions. Returns (width, height). -/
def measureText (text : String) (font : Font) (c : Canvas) : IO (Float × Float) :=
  c.ctx.measureText text font

/-! ## Window operations -/

def shouldClose (c : Canvas) : IO Bool :=
  c.ctx.shouldClose

def pollEvents (c : Canvas) : IO Unit :=
  c.ctx.pollEvents

/-- Get the last key code pressed (0 if none). Common codes: Space=49, Escape=53, P=35 -/
def getKeyCode (c : Canvas) : IO UInt16 :=
  c.ctx.getKeyCode

/-- Clear the key pressed state (call after handling the key). -/
def clearKey (c : Canvas) : IO Unit :=
  c.ctx.clearKey

def beginFrame (clearColor : Color) (c : Canvas) : IO Bool :=
  c.ctx.beginFrame clearColor

def endFrame (c : Canvas) : IO Unit :=
  c.ctx.endFrame

def destroy (c : Canvas) : IO Unit :=
  c.ctx.destroy

def width (c : Canvas) : IO Float := c.ctx.width
def height (c : Canvas) : IO Float := c.ctx.height
def baseWidth (c : Canvas) : Float := c.ctx.baseWidth
def baseHeight (c : Canvas) : Float := c.ctx.baseHeight

/-- Set a scissor rectangle for clipping in pixel coordinates.
    Note: Scissor coordinates are in actual pixel space, not logical canvas coordinates. -/
def setScissor (x y width height : UInt32) (c : Canvas) : IO Unit :=
  c.ctx.setScissor x y width height

/-- Reset scissor to full viewport (disable clipping). -/
def resetScissor (c : Canvas) : IO Unit :=
  c.ctx.resetScissor

/-- Set a clip rectangle in logical canvas coordinates.
    The coordinates will be scaled to match the current drawable size. -/
def clip (rect : Rect) (c : Canvas) : IO Unit := do
  let (drawW, drawH) ← c.ctx.getCurrentSize
  let scaleX := drawW / c.ctx.baseWidth
  let scaleY := drawH / c.ctx.baseHeight
  let x := (rect.x * scaleX).toUInt32
  let y := (rect.y * scaleY).toUInt32
  let w := (rect.width * scaleX).toUInt32
  let h := (rect.height * scaleY).toUInt32
  c.ctx.setScissor x y w h

/-- Remove clipping and restore full viewport. -/
def unclip (c : Canvas) : IO Unit :=
  c.ctx.resetScissor

/-- Run a render loop with a Canvas that maintains state across frames.
    The draw function can return a modified Canvas with updated state. -/
def runLoop (c : Canvas) (clearColor : Color) (draw : Canvas → IO Canvas) : IO Unit := do
  let mut canvas := c
  while !(← canvas.shouldClose) do
    canvas.pollEvents
    let ok ← canvas.beginFrame clearColor
    if ok then
      canvas ← draw canvas
      canvas.endFrame

/-- Run a render loop with time parameter (in seconds since start).
    The draw function receives canvas and elapsed time. -/
def runLoopWithTime (c : Canvas) (clearColor : Color) (draw : Canvas → Float → IO Canvas) : IO Unit := do
  let startTime ← IO.monoMsNow
  let mut canvas := c
  while !(← canvas.shouldClose) do
    canvas.pollEvents
    let ok ← canvas.beginFrame clearColor
    if ok then
      let now ← IO.monoMsNow
      let elapsed := (now - startTime).toFloat / 1000.0  -- Convert ms to seconds
      canvas ← draw canvas elapsed
      canvas.endFrame

end Canvas

end Afferent

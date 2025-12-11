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

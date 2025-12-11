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
import Afferent.FFI.Metal

namespace Afferent

/-- Drawing context that wraps FFI renderer with high-level API. -/
structure DrawContext where
  window : FFI.Window
  renderer : FFI.Renderer
  width : Float
  height : Float

namespace DrawContext

/-- Create a new drawing context with a window. -/
def create (width height : UInt32) (title : String) : IO DrawContext := do
  FFI.init
  let window ← FFI.Window.create width height title
  let renderer ← FFI.Renderer.create window
  pure {
    window
    renderer
    width := width.toFloat
    height := height.toFloat
  }

/-- Check if the window should close. -/
def shouldClose (ctx : DrawContext) : IO Bool :=
  ctx.window.shouldClose

/-- Poll window events. -/
def pollEvents (ctx : DrawContext) : IO Unit :=
  ctx.window.pollEvents

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

/-- Fill a rectangle with a solid color (pixel coordinates). -/
def fillRect (ctx : DrawContext) (rect : Rect) (color : Color) : IO Unit := do
  let result := Tessellation.tessellateRectNDC rect color ctx.width ctx.height
  if result.vertices.size > 0 && result.indices.size > 0 then
    let vertexBuffer ← FFI.Buffer.createVertex ctx.renderer result.vertices
    let indexBuffer ← FFI.Buffer.createIndex ctx.renderer result.indices
    ctx.renderer.drawTriangles vertexBuffer indexBuffer result.indices.size.toUInt32
    FFI.Buffer.destroy indexBuffer
    FFI.Buffer.destroy vertexBuffer

/-- Fill a rectangle specified by x, y, width, height. -/
def fillRectXYWH (ctx : DrawContext) (x y width height : Float) (color : Color) : IO Unit :=
  ctx.fillRect (Rect.mk' x y width height) color

/-- Fill a convex path with a solid color (pixel coordinates). -/
def fillPath (ctx : DrawContext) (path : Path) (color : Color) : IO Unit := do
  let result := Tessellation.tessellateConvexPathNDC path color ctx.width ctx.height
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
  let result := Tessellation.tessellateRectFillNDC rect style ctx.width ctx.height
  if result.vertices.size > 0 && result.indices.size > 0 then
    let vertexBuffer ← FFI.Buffer.createVertex ctx.renderer result.vertices
    let indexBuffer ← FFI.Buffer.createIndex ctx.renderer result.indices
    ctx.renderer.drawTriangles vertexBuffer indexBuffer result.indices.size.toUInt32
    FFI.Buffer.destroy indexBuffer
    FFI.Buffer.destroy vertexBuffer

/-- Fill a convex path with a fill style (solid color or gradient). -/
def fillPathWithStyle (ctx : DrawContext) (path : Path) (style : FillStyle) : IO Unit := do
  let result := Tessellation.tessellateConvexPathFillNDC path style ctx.width ctx.height
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
  let result := Tessellation.tessellateStrokeNDC path style ctx.width ctx.height
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

/-- A canvas with built-in state management. -/
structure Canvas where
  ctx : DrawContext
  stateStack : StateStack

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

/-! ## Drawing operations -/

/-- Fill a path using the current state. -/
def fillPath (path : Path) (c : Canvas) : IO Unit :=
  c.ctx.fillPathWithState path c.state

/-- Fill a rectangle using the current state. -/
def fillRect (rect : Rect) (c : Canvas) : IO Unit :=
  c.ctx.fillRectWithState rect c.state

/-- Fill a rectangle specified by x, y, width, height using current state. -/
def fillRectXYWH (x y width height : Float) (c : Canvas) : IO Unit :=
  c.fillRect (Rect.mk' x y width height)

/-- Fill a circle using the current state. -/
def fillCircle (center : Point) (radius : Float) (c : Canvas) : IO Unit :=
  c.ctx.fillCircleWithState center radius c.state

/-- Fill an ellipse using the current state. -/
def fillEllipse (center : Point) (radiusX radiusY : Float) (c : Canvas) : IO Unit :=
  c.ctx.fillPathWithState (Path.ellipse center radiusX radiusY) c.state

/-- Fill a rounded rectangle using the current state. -/
def fillRoundedRect (rect : Rect) (cornerRadius : Float) (c : Canvas) : IO Unit :=
  c.ctx.fillPathWithState (Path.roundedRect rect cornerRadius) c.state

/-! ## Stroke operations -/

/-- Get the effective stroke style with transform and global alpha applied. -/
private def effectiveStrokeStyle (c : Canvas) : StrokeStyle :=
  let state := c.state
  { state.strokeStyle with
    color := state.effectiveStrokeColor }

/-- Stroke a path using the current state (applies transform and uses state's stroke style). -/
def strokePath (path : Path) (c : Canvas) : IO Unit := do
  let transformedPath := c.state.transformPath path
  let style := c.effectiveStrokeStyle
  c.ctx.strokePath transformedPath style

/-- Stroke a rectangle using the current state. -/
def strokeRect (rect : Rect) (c : Canvas) : IO Unit :=
  c.strokePath (Path.rectangle rect)

/-- Stroke a rectangle specified by x, y, width, height using current state. -/
def strokeRectXYWH (x y width height : Float) (c : Canvas) : IO Unit :=
  c.strokeRect (Rect.mk' x y width height)

/-- Stroke a circle using the current state. -/
def strokeCircle (center : Point) (radius : Float) (c : Canvas) : IO Unit :=
  c.strokePath (Path.circle center radius)

/-- Stroke an ellipse using the current state. -/
def strokeEllipse (center : Point) (radiusX radiusY : Float) (c : Canvas) : IO Unit :=
  c.strokePath (Path.ellipse center radiusX radiusY)

/-- Stroke a rounded rectangle using the current state. -/
def strokeRoundedRect (rect : Rect) (cornerRadius : Float) (c : Canvas) : IO Unit :=
  c.strokePath (Path.roundedRect rect cornerRadius)

/-- Draw a line from p1 to p2 using the current state. -/
def drawLine (p1 p2 : Point) (c : Canvas) : IO Unit :=
  c.strokePath (Path.empty |>.moveTo p1 |>.lineTo p2)

/-! ## Window operations -/

def shouldClose (c : Canvas) : IO Bool :=
  c.ctx.shouldClose

def pollEvents (c : Canvas) : IO Unit :=
  c.ctx.pollEvents

def beginFrame (clearColor : Color) (c : Canvas) : IO Bool :=
  c.ctx.beginFrame clearColor

def endFrame (c : Canvas) : IO Unit :=
  c.ctx.endFrame

def destroy (c : Canvas) : IO Unit :=
  c.ctx.destroy

def width (c : Canvas) : Float := c.ctx.width
def height (c : Canvas) : Float := c.ctx.height

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

end Canvas

end Afferent

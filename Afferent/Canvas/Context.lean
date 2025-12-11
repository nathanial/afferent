/-
  Afferent Canvas Context
  High-level drawing API similar to HTML5 Canvas.
-/
import Afferent.Core.Types
import Afferent.Core.Path
import Afferent.Core.Transform
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

/-- Run a render loop until the window is closed. -/
def runLoop (ctx : DrawContext) (clearColor : Color) (draw : DrawContext → IO Unit) : IO Unit := do
  while !(← ctx.shouldClose) do
    ctx.pollEvents
    let ok ← ctx.beginFrame clearColor
    if ok then
      draw ctx
      ctx.endFrame

end DrawContext

end Afferent

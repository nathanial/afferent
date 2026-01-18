/-
  Afferent Canvas Context
  High-level drawing API similar to HTML5 Canvas.
-/
import Afferent.Core.Types
import Afferent.Core.Path
import Afferent.Core.Transform
import Afferent.Core.Paint
import Afferent.Canvas.State
import Afferent.Render.Dynamic
import Afferent.Text.Font
import Afferent.FFI
import Afferent.Arbor.Render.Cache
import Afferent.Shader
import Std.Data.HashMap

namespace Afferent

private def runWithEventLoop (window : FFI.Window) (loop : IO Unit) : IO Unit := do
  let task ← IO.asTask (prio := .dedicated) loop
  window.runEventLoop
  match task.get with
  | .ok _ => pure ()
  | .error err => throw err

/-- Drawing context that wraps FFI renderer with high-level API. -/
structure DrawContext where
  window : FFI.Window
  renderer : FFI.Renderer
  /-- Initial/logical canvas width (used as reference for coordinate system) -/
  baseWidth : Float
  /-- Initial/logical canvas height (used as reference for coordinate system) -/
  baseHeight : Float

/-- Persistent GPU buffers for a stroked path (static geometry cache). -/
structure StrokeCache where
  lineBuffer : Option FFI.Buffer
  curveBuffer : Option FFI.Buffer
  lineCount : Nat
  curveCount : Nat
  /-- Transform used when computing path distances (dash alignment). -/
  transform : Transform
deriving Inhabited

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

/-- Run the native event loop (blocks until stopped). -/
def runEventLoop (ctx : DrawContext) : IO Unit :=
  ctx.window.runEventLoop

/-- Get the last key code pressed (only valid if hasKeyPressed is true). -/
def getKeyCode (ctx : DrawContext) : IO UInt16 :=
  ctx.window.getKeyCode

/-- Check if a key is pending (use to distinguish key code 0 from "no key"). -/
def hasKeyPressed (ctx : DrawContext) : IO Bool :=
  ctx.window.hasKeyPressed

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
  -- Use current drawable size for NDC conversion (dynamic resize support)
  let (w, h) ← ctx.getCurrentSize
  let result := Tessellation.tessellateRectNDC rect color w h
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
  -- Use current drawable size for NDC conversion (dynamic resize support)
  let (w, h) ← ctx.getCurrentSize
  let result := Tessellation.tessellateConvexPathNDC path color w h
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
  -- Use current drawable size for NDC conversion (dynamic resize support)
  let (w, h) ← ctx.getCurrentSize
  let result := Tessellation.tessellateRectFillNDC rect style w h
  if result.vertices.size > 0 && result.indices.size > 0 then
    let vertexBuffer ← FFI.Buffer.createVertex ctx.renderer result.vertices
    let indexBuffer ← FFI.Buffer.createIndex ctx.renderer result.indices
    ctx.renderer.drawTriangles vertexBuffer indexBuffer result.indices.size.toUInt32
    FFI.Buffer.destroy indexBuffer
    FFI.Buffer.destroy vertexBuffer

/-- Fill a transformed rectangle with a fill style (fast path - no Path allocation). -/
def fillTransformedRectWithStyle (ctx : DrawContext) (rect : Rect) (transform : Transform) (style : FillStyle) : IO Unit := do
  -- Use current drawable size for NDC conversion (dynamic resize support)
  let (w, h) ← ctx.getCurrentSize
  let result := Tessellation.tessellateTransformedRectNDC rect transform style w h
  if result.vertices.size > 0 && result.indices.size > 0 then
    let vertexBuffer ← FFI.Buffer.createVertex ctx.renderer result.vertices
    let indexBuffer ← FFI.Buffer.createIndex ctx.renderer result.indices
    ctx.renderer.drawTriangles vertexBuffer indexBuffer result.indices.size.toUInt32
    FFI.Buffer.destroy indexBuffer
    FFI.Buffer.destroy vertexBuffer

/-- Fill a convex path with a fill style (solid color or gradient). -/
def fillPathWithStyle (ctx : DrawContext) (path : Path) (style : FillStyle) : IO Unit := do
  -- Use current drawable size for NDC conversion (dynamic resize support)
  let (w, h) ← ctx.getCurrentSize
  let result := Tessellation.tessellateConvexPathFillNDC path style w h
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
def strokePath (ctx : DrawContext) (path : Path) (style : StrokeStyle)
    (transform : Transform := Transform.identity) : IO Unit := do
  -- Use current drawable size for NDC conversion (dynamic resize support)
  let (w, h) ← ctx.getCurrentSize
  let segments := Tessellation.tessellateStrokeSegments path style transform
  if segments.lineCount == 0 && segments.curveCount == 0 then
    return

  let halfWidth := style.lineWidth / 2.0
  let lineCap : UInt32 :=
    match style.lineCap with
    | .butt => 0
    | .round => 1
    | .square => 2
  let lineJoin : UInt32 :=
    match style.lineJoin with
    | .miter => 0
    | .round => 1
    | .bevel => 2

  let dashMax : Nat := 8
  let (dashSegments, dashCount, dashOffset) := Id.run do
    match style.dashPattern with
    | none => return (#[], 0, 0.0)
    | some pat =>
      let mut trimmed : Array Float := Array.mkEmpty (min pat.segments.size dashMax)
      for i in [:min pat.segments.size dashMax] do
        trimmed := trimmed.push pat.segments[i]!
      return (trimmed, trimmed.size, pat.offset)

  if segments.lineCount > 0 then
    let buffer ← FFI.Buffer.createStrokeSegment ctx.renderer segments.lineSegments
    ctx.renderer.drawStrokePath
      buffer
      segments.lineCount.toUInt32
      1
      halfWidth
      w h
      style.miterLimit
      lineCap lineJoin
      transform.a transform.b transform.c transform.d transform.tx transform.ty
      dashSegments
      dashCount.toUInt32
      dashOffset
      style.color.r style.color.g style.color.b style.color.a
    FFI.Buffer.destroy buffer

  if segments.curveCount > 0 then
    let buffer ← FFI.Buffer.createStrokeSegment ctx.renderer segments.curveSegments
    ctx.renderer.drawStrokePath
      buffer
      segments.curveCount.toUInt32
      Tessellation.strokeCurveSubdivisions.toUInt32
      halfWidth
      w h
      style.miterLimit
      lineCap lineJoin
      transform.a transform.b transform.c transform.d transform.tx transform.ty
      dashSegments
      dashCount.toUInt32
      dashOffset
      style.color.r style.color.g style.color.b style.color.a
    FFI.Buffer.destroy buffer

/-- Build persistent stroke buffers for a path (static geometry cache). -/
def createStrokeCache (ctx : DrawContext) (path : Path)
    (transform : Transform := Transform.identity) : IO StrokeCache := do
  -- Stroke tessellation ignores style; use default for geometry.
  let segments := Tessellation.tessellateStrokeSegments path StrokeStyle.default transform
  if segments.lineCount == 0 && segments.curveCount == 0 then
    return { lineBuffer := none, curveBuffer := none, lineCount := 0, curveCount := 0, transform }

  let lineBuffer ←
    if segments.lineCount > 0 then
      some <$> FFI.Buffer.createStrokeSegmentPersistent ctx.renderer segments.lineSegments
    else
      pure none

  let curveBuffer ←
    if segments.curveCount > 0 then
      some <$> FFI.Buffer.createStrokeSegmentPersistent ctx.renderer segments.curveSegments
    else
      pure none

  return {
    lineBuffer
    curveBuffer
    lineCount := segments.lineCount
    curveCount := segments.curveCount
    transform
  }

/-- Draw a cached stroke with the given style. -/
def drawStrokeCache (ctx : DrawContext) (cache : StrokeCache) (style : StrokeStyle) : IO Unit := do
  if cache.lineCount == 0 && cache.curveCount == 0 then
    return
  let (w, h) ← ctx.getCurrentSize
  let halfWidth := style.lineWidth / 2.0
  let lineCap : UInt32 :=
    match style.lineCap with
    | .butt => 0
    | .round => 1
    | .square => 2
  let lineJoin : UInt32 :=
    match style.lineJoin with
    | .miter => 0
    | .round => 1
    | .bevel => 2

  let dashMax : Nat := 8
  let (dashSegments, dashCount, dashOffset) := Id.run do
    match style.dashPattern with
    | none => return (#[], 0, 0.0)
    | some pat =>
      let mut trimmed : Array Float := Array.mkEmpty (min pat.segments.size dashMax)
      for i in [:min pat.segments.size dashMax] do
        trimmed := trimmed.push pat.segments[i]!
      return (trimmed, trimmed.size, pat.offset)

  if let some buffer := cache.lineBuffer then
    ctx.renderer.drawStrokePath
      buffer
      cache.lineCount.toUInt32
      1
      halfWidth
      w h
      style.miterLimit
      lineCap lineJoin
      cache.transform.a cache.transform.b cache.transform.c cache.transform.d cache.transform.tx cache.transform.ty
      dashSegments
      dashCount.toUInt32
      dashOffset
      style.color.r style.color.g style.color.b style.color.a

  if let some buffer := cache.curveBuffer then
    ctx.renderer.drawStrokePath
      buffer
      cache.curveCount.toUInt32
      Tessellation.strokeCurveSubdivisions.toUInt32
      halfWidth
      w h
      style.miterLimit
      lineCap lineJoin
      cache.transform.a cache.transform.b cache.transform.c cache.transform.d cache.transform.tx cache.transform.ty
      dashSegments
      dashCount.toUInt32
      dashOffset
      style.color.r style.color.g style.color.b style.color.a

/-- Destroy cached stroke buffers. -/
def destroyStrokeCache (_ctx : DrawContext) (cache : StrokeCache) : IO Unit := do
  if let some buffer := cache.lineBuffer then
    FFI.Buffer.destroy buffer
  if let some buffer := cache.curveBuffer then
    FFI.Buffer.destroy buffer

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

/-! ## Text Rendering -/

/-- Draw text at a position with a font, color, and transform.
    Uses the current drawable size for NDC conversion (dynamic resize support). -/
def fillTextTransformed (ctx : DrawContext) (text : String) (pos : Point) (font : Font) (color : Color) (transform : Transform) : IO Unit := do
  let (w, h) ← ctx.getCurrentSize
  FFI.Text.render ctx.renderer font.handle text pos.x pos.y color.r color.g color.b color.a transform.toArray w h

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
  let render := do
    while !(← ctx.shouldClose) do
      let ok ← ctx.beginFrame clearColor
      if ok then
        draw ctx
        ctx.endFrame
  runWithEventLoop ctx.window render

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
  let render := do
    let mut stack := StateStack.new
    while !(← ctx.shouldClose) do
      let ok ← ctx.beginFrame clearColor
      if ok then
        stack ← draw ctx stack
        ctx.endFrame
  runWithEventLoop ctx.window render

end DrawContext

/-! ## Canvas Configuration -/

/-- Configuration for creating a canvas application.
    Dimensions are in logical pixels; if `scaleToScreen` is true (default),
    they will be multiplied by the screen scale factor for Retina displays. -/
structure CanvasConfig where
  /-- Logical width in pixels (default: 1920) -/
  width : Float := 1920.0
  /-- Logical height in pixels (default: 1080) -/
  height : Float := 1080.0
  /-- Window title -/
  title : String := "Afferent"
  /-- Background color cleared each frame -/
  clearColor : Color := Color.darkGray
  /-- If true, multiply dimensions by screen scale factor for Retina displays -/
  scaleToScreen : Bool := true
deriving Repr, Inhabited

/-! ## Stateful Canvas - Higher-level API with automatic state management -/

/-- A canvas with built-in state management. -/
structure Canvas where
  ctx : DrawContext
  stateStack : StateStack
  /-- Screen scale factor (e.g., 2.0 for Retina). Used for auto-scaling mode. -/
  screenScale : Float := 1.0
  /-- High-performance mutable FloatBuffer for zero-copy line rendering. -/
  floatBuffer : Option FFI.FloatBuffer := none
  /-- Capacity of line FloatBuffer (in floats). -/
  floatBufferCapacity : Nat := 0
  /-- High-performance mutable FloatBuffer for zero-copy rect rendering. -/
  rectBuffer : Option FFI.FloatBuffer := none
  /-- Capacity of rect FloatBuffer (in floats). -/
  rectBufferCapacity : Nat := 0
  /-- High-performance mutable FloatBuffer for zero-copy circle rendering. -/
  circleBuffer : Option FFI.FloatBuffer := none
  /-- Capacity of circle FloatBuffer (in floats). -/
  circleBufferCapacity : Nat := 0
  /-- High-performance mutable FloatBuffer for zero-copy strokeRect rendering. -/
  strokeRectBuffer : Option FFI.FloatBuffer := none
  /-- Capacity of strokeRect FloatBuffer (in floats). -/
  strokeRectBufferCapacity : Nat := 0
  /-- High-performance mutable FloatBuffer for zero-copy strokeCircle rendering. -/
  strokeCircleBuffer : Option FFI.FloatBuffer := none
  /-- Capacity of strokeCircle FloatBuffer (in floats). -/
  strokeCircleBufferCapacity : Nat := 0
  /-- High-performance mutable FloatBuffer for padded fragment params. -/
  fragmentBuffer : Option FFI.FloatBuffer := none
  /-- Capacity of fragment FloatBuffer (in floats). -/
  fragmentBufferCapacity : Nat := 0
  /-- Cache of GPU meshes keyed by path hash for instanced polygon rendering. -/
  meshCache : Std.HashMap UInt64 FFI.CachedMesh := {}
  /-- High-performance mutable FloatBuffer for mesh instance data. -/
  meshInstanceBuffer : Option FFI.FloatBuffer := none
  /-- Capacity of mesh instance FloatBuffer (in floats). -/
  meshInstanceBufferCapacity : Nat := 0
  /-- Persistent cache for CustomSpec render commands across frames.
      Cache is keyed by widget name (from registerComponentW) + layout hash.
      When data changes, dynWidget rebuilds and generates new widget names,
      causing natural cache invalidation. -/
  renderCache : IO.Ref Arbor.RenderCache
  /-- Cache of compiled fragment pipelines by hash.
      Fragment definitions are looked up from the global registry. -/
  fragmentCache : IO.Ref Shader.FragmentCache

namespace Canvas

/-- Create a new canvas with a window. -/
def create (width height : UInt32) (title : String) : IO Canvas := do
  let ctx ← DrawContext.create width height title
  let renderCache ← IO.mkRef Arbor.RenderCache.empty
  let fragmentCache ← IO.mkRef Shader.FragmentCache.empty
  pure { ctx, stateStack := StateStack.new, renderCache, fragmentCache }

/-- Create a new canvas with a window and explicit screen scale factor. -/
def createWithScale (width height : UInt32) (title : String) (screenScale : Float) : IO Canvas := do
  let ctx ← DrawContext.create width height title
  let renderCache ← IO.mkRef Arbor.RenderCache.empty
  let fragmentCache ← IO.mkRef Shader.FragmentCache.empty
  pure { ctx, stateStack := StateStack.new, screenScale, renderCache, fragmentCache }

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

def setBaseTransform (t : Transform) (c : Canvas) : Canvas :=
  { c with stateStack := c.stateStack.setBaseTransform t }

def resetTransform (c : Canvas) : Canvas :=
  { c with stateStack := c.stateStack.resetTransform }

/-- Reset the entire canvas state to defaults (transform, styles, alpha, clips). -/
def resetState (c : Canvas) : Canvas :=
  { c with stateStack := StateStack.new }

/-- Reset the canvas state and clear any active scissor. -/
def resetStateAndScissor (c : Canvas) : IO Canvas := do
  c.ctx.resetScissor
  pure { c with stateStack := StateStack.new }

/-! ## Render Cache Diagnostics -/

/-- Get the number of cached render command entries.
    Useful for verifying that caching is working. -/
def getRenderCacheSize (c : Canvas) : IO Nat := do
  let cache ← c.renderCache.get
  pure cache.size

/-- Clear the render cache. Call this if you need to force re-rendering
    (e.g., after a theme change that doesn't trigger widget rebuild). -/
def clearRenderCache (c : Canvas) : IO Unit := do
  c.renderCache.modify fun rc => Arbor.RenderCache.clear rc

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

def setLineCap (cap : LineCap) (c : Canvas) : Canvas :=
  { c with stateStack := c.stateStack.setLineCap cap }

def setLineJoin (join : LineJoin) (c : Canvas) : Canvas :=
  { c with stateStack := c.stateStack.setLineJoin join }

def setDashPattern (pattern : Option DashPattern) (c : Canvas) : Canvas :=
  { c with stateStack := c.stateStack.setDashPattern pattern }

def setDashed (dashLen gapLen : Float) (c : Canvas) : Canvas :=
  { c with stateStack := c.stateStack.setDashed dashLen gapLen }

def setDotted (c : Canvas) : Canvas :=
  { c with stateStack := c.stateStack.setDotted }

def setSolid (c : Canvas) : Canvas :=
  { c with stateStack := c.stateStack.setSolid }

/-! ## Drawing operations -/

/-- Fill a path using the current state. -/
def fillPath (path : Path) (c : Canvas) : IO Canvas := do
  c.ctx.fillPathWithState path c.state
  pure c

/-- Fill a rectangle using the current state. -/
def fillRect (rect : Rect) (c : Canvas) : IO Canvas := do
  c.ctx.fillRectWithState rect c.state
  pure c

/-- Fill a rectangle specified by x, y, width, height using current state. -/
def fillRectXYWH (x y width height : Float) (c : Canvas) : IO Canvas :=
  c.fillRect (Rect.mk' x y width height)

/-- Fill a circle using the current state. -/
def fillCircle (center : Point) (radius : Float) (c : Canvas) : IO Canvas :=
  c.fillPath (Path.circle center radius)

/-- Fill an ellipse using the current state. -/
def fillEllipse (center : Point) (radiusX radiusY : Float) (c : Canvas) : IO Canvas :=
  c.fillPath (Path.ellipse center radiusX radiusY)

/-- Fill a rounded rectangle using the current state. -/
def fillRoundedRect (rect : Rect) (cornerRadius : Float) (c : Canvas) : IO Canvas :=
  c.fillPath (Path.roundedRect rect cornerRadius)

/-! ## Stroke operations -/

/-- Get the effective stroke style with transform and global alpha applied. -/
private def effectiveStrokeStyle (c : Canvas) : StrokeStyle :=
  let state := c.state
  { state.strokeStyle with
    color := state.effectiveStrokeColor }

/-- Stroke a path using the current state.
    Strokes bypass the fill batch because they use a different vertex format. -/
def strokePath (path : Path) (c : Canvas) : IO Canvas := do
  let transform := c.state.transform
  let style := c.effectiveStrokeStyle
  -- Stroke extrusion uses a separate vertex format, so it bypasses the fill batch.
  c.ctx.strokePath path style transform
  pure c

/-- Build a persistent stroke cache using the current transform. -/
def createStrokeCache (path : Path) (c : Canvas) : IO StrokeCache :=
  c.ctx.createStrokeCache path c.state.transform

/-- Draw a cached stroke using the current stroke style. -/
def drawStrokeCache (cache : StrokeCache) (c : Canvas) : IO Canvas := do
  let style := c.effectiveStrokeStyle
  c.ctx.drawStrokeCache cache style
  pure c

/-- Destroy cached stroke buffers. -/
def destroyStrokeCache (cache : StrokeCache) (c : Canvas) : IO Canvas := do
  c.ctx.destroyStrokeCache cache
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

/-- Ensure floatBuffer has at least the required capacity (in floats).
    Returns updated Canvas with properly sized buffer. -/
private def ensureFloatBufferCapacity (requiredFloats : Nat) (c : Canvas) : IO Canvas := do
  match c.floatBuffer with
  | some buf =>
    if c.floatBufferCapacity >= requiredFloats then pure c
    else
      FFI.FloatBuffer.destroy buf
      let newBuf ← FFI.FloatBuffer.create requiredFloats.toUSize
      pure { c with floatBuffer := some newBuf, floatBufferCapacity := requiredFloats }
  | none =>
    let newBuf ← FFI.FloatBuffer.create requiredFloats.toUSize
    pure { c with floatBuffer := some newBuf, floatBufferCapacity := requiredFloats }

/-- Draw text at a position with a font using the current fill color and transform.
    Note: Text uses a different shader and cannot be batched with shapes.
    Auto-batch is flushed before drawing text. -/
def fillText (text : String) (pos : Point) (font : Font) (c : Canvas) : IO Canvas := do
  let color := c.state.effectiveFillColor
  let transform := c.state.transform
  c.ctx.fillTextTransformed text pos font color transform
  pure c

/-- Draw text at x, y coordinates with a font using the current fill color and transform. -/
def fillTextXY (text : String) (x y : Float) (font : Font) (c : Canvas) : IO Canvas :=
  c.fillText text ⟨x, y⟩ font

/-- Draw text with an explicit color (still uses current transform). -/
def fillTextColor (text : String) (pos : Point) (font : Font) (color : Color) (c : Canvas) : IO Canvas := do
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

def runEventLoop (c : Canvas) : IO Unit :=
  c.ctx.runEventLoop

/-- Get the last key code pressed (only valid if hasKeyPressed is true). Common codes: Space=49, Escape=53, P=35 -/
def getKeyCode (c : Canvas) : IO UInt16 :=
  c.ctx.getKeyCode

/-- Check if a key is pending (use to distinguish key code 0 from "no key"). -/
def hasKeyPressed (c : Canvas) : IO Bool :=
  c.ctx.hasKeyPressed

/-- Clear the key pressed state (call after handling the key). -/
def clearKey (c : Canvas) : IO Unit :=
  c.ctx.clearKey

def beginFrame (clearColor : Color) (c : Canvas) : IO Bool :=
  c.ctx.beginFrame clearColor

/-- End the current frame. Presents the drawable. -/
def endFrame (c : Canvas) : IO Canvas := do
  c.ctx.endFrame
  pure c

/-- End the current frame (unit version for compatibility).
    Prefer using endFrame when you need the updated Canvas. -/
def endFrame' (c : Canvas) : IO Unit := do
  discard (c.endFrame)

def destroy (c : Canvas) : IO Unit := do
  if let some buf := c.floatBuffer then
    FFI.FloatBuffer.destroy buf
  if let some buf := c.fragmentBuffer then
    FFI.FloatBuffer.destroy buf
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

/-- Helper to compute and apply the effective scissor from the clip stack. -/
private def applyEffectiveScissor (c : Canvas) : IO Unit := do
  match c.state.effectiveClipRect with
  | some r =>
    -- Clamp to non-negative values for UInt32
    let x := (max 0 r.x).toUInt32
    let y := (max 0 r.y).toUInt32
    let w := (max 0 r.width).toUInt32
    let h := (max 0 r.height).toUInt32
    c.ctx.setScissor x y w h
  | none =>
    c.ctx.resetScissor

/-- Push a clip rectangle onto the clip stack. The rect coordinates are in the
    current coordinate system (after any transforms). The clip will be transformed
    by the CURRENT canvas transform, so clipping respects translate/scale/rotate. -/
def clip (rect : Rect) (c : Canvas) : IO Canvas := do
  -- Push clip with current transform onto the stack
  let c := c.modifyState (·.pushClip rect)
  -- Apply effective scissor
  c.applyEffectiveScissor
  pure c

/-- Pop the most recent clip rectangle from the clip stack.
    Restores the previous clip state (or disables clipping if stack is empty). -/
def popClip (c : Canvas) : IO Canvas := do
  let c := c.modifyState (·.popClip)
  c.applyEffectiveScissor
  pure c

/-- Remove all clipping and restore full viewport.
    Clears the entire clip stack. -/
def unclip (c : Canvas) : IO Canvas := do
  let c := c.modifyState (·.clearClipStack)
  c.ctx.resetScissor
  pure c

/-- Run a render loop with a Canvas that maintains state across frames.
    The draw function can return a modified Canvas with updated state. -/
def runLoop (c : Canvas) (clearColor : Color) (draw : Canvas → IO Canvas) : IO Unit := do
  let render := do
    let mut canvas := c
    while !(← canvas.shouldClose) do
      let ok ← canvas.beginFrame clearColor
      if ok then
        canvas ← draw canvas
        canvas ← canvas.endFrame
  runWithEventLoop c.ctx.window render

/-- Run a render loop with time parameter (in seconds since start).
    The draw function receives canvas and elapsed time. -/
def runLoopWithTime (c : Canvas) (clearColor : Color) (draw : Canvas → Float → IO Canvas) : IO Unit := do
  let render := do
    let startTime ← IO.monoMsNow
    let mut canvas := c
    while !(← canvas.shouldClose) do
      let ok ← canvas.beginFrame clearColor
      if ok then
        let now ← IO.monoMsNow
        let elapsed := (now - startTime).toFloat / 1000.0  -- Convert ms to seconds
        canvas ← draw canvas elapsed
        canvas ← canvas.endFrame
  runWithEventLoop c.ctx.window render

/-! ## Dynamic Particle Rendering

High-level API for GPU-instanced particle rendering. These methods manage
FloatBuffer allocation internally, providing zero-copy performance without
exposing buffer management to callers. -/

/-- Fill dynamic instanced shapes with uniform rotation.
    shapeType: 0=rect, 1=triangle, 2=circle. -/
def fillDynamicInstanced (shapeType : UInt32) (particles : Render.Dynamic.ParticleState)
    (halfSize rotation t : Float) (c : Canvas) : IO Canvas := do
  let c ← c.ensureFloatBufferCapacity (particles.count * 8)
  match c.floatBuffer with
  | some buf =>
    Render.Dynamic.drawInstancedUniform c.ctx.renderer shapeType particles buf halfSize rotation t
    pure c
  | none => pure c

/-- Fill dynamic instanced shapes with animated rotation.
    shapeType: 0=rect, 1=triangle, 2=circle. -/
def fillDynamicInstancedAnimated (shapeType : UInt32) (particles : Render.Dynamic.ParticleState)
    (halfSize t spinSpeed : Float) (c : Canvas) : IO Canvas := do
  let c ← c.ensureFloatBufferCapacity (particles.count * 8)
  match c.floatBuffer with
  | some buf =>
    Render.Dynamic.drawInstancedAnimated c.ctx.renderer shapeType particles buf halfSize t spinSpeed
    pure c
  | none => pure c

/-- Draw dynamic sprites (textured quads) from particle state.
    Uses Canvas's managed floatBuffer for zero-copy performance.
    - texture: The texture to draw
    - particles: ParticleState containing position/velocity/hue data
    - halfSize: Half the sprite size in pixels
    - rotation: Rotation angle in radians (default 0)
    - alpha: Opacity (default 1.0) -/
def fillDynamicSprites (texture : FFI.Texture) (particles : Render.Dynamic.ParticleState)
    (halfSize : Float) (rotation : Float := 0.0) (alpha : Float := 1.0) (c : Canvas) : IO Canvas := do
  -- Sprites use 5 floats per instance (x, y, rotation, halfSize, alpha)
  let c ← c.ensureFloatBufferCapacity (particles.count * 5)
  match c.floatBuffer with
  | some buf =>
    Render.Dynamic.writeSpritesToBuffer particles buf halfSize rotation alpha
    Render.Dynamic.drawSpritesFromBuffer c.ctx.renderer texture buf particles.count.toUInt32 halfSize
      particles.screenWidth particles.screenHeight
    pure c
  | none => pure c

/-- Draw orbital particles as rectangles. Each particle orbits around a center point.
    - orbital: OrbitalState containing orbital parameters
    - t: Current time (controls orbital position and HSV animation) -/
def fillOrbitalRects (orbital : Render.Dynamic.OrbitalState) (t : Float) (c : Canvas) : IO Canvas := do
  let c ← c.ensureFloatBufferCapacity (orbital.count * 8)
  match c.floatBuffer with
  | some buf =>
    Render.Dynamic.drawOrbitalRects c.ctx.renderer orbital buf t
    pure c
  | none => pure c

end Canvas

/-! ## CanvasM - StateT-based Canvas Monad for automatic state threading -/

/-- Canvas monad that automatically threads Canvas state through operations.
    Use this to avoid manually passing Canvas through every drawing operation. -/
abbrev CanvasM := StateT Canvas IO

namespace CanvasM

/-- Run a CanvasM action with an initial canvas, returning the result and final canvas. -/
def run (c : Canvas) (action : CanvasM α) : IO (α × Canvas) :=
  StateT.run action c

/-- Run a CanvasM action, returning only the final canvas. -/
def run' (c : Canvas) (action : CanvasM Unit) : IO Canvas := do
  let ((), c') ← StateT.run action c
  pure c'

/-- Get the current canvas. -/
def getCanvas : CanvasM Canvas := get

/-- Replace the current canvas. -/
def setCanvas (c : Canvas) : CanvasM Unit := set c

/-- Modify the canvas with a pure function. -/
def modifyCanvas (f : Canvas → Canvas) : CanvasM Unit := modify f

/-- Lift an IO action that takes and returns Canvas. -/
def liftCanvas (f : Canvas → IO Canvas) : CanvasM Unit := do
  let c ← get
  let c' ← f c
  set c'

/-! ## Transform operations -/

def save : CanvasM Unit := modifyCanvas Canvas.save
def restore : CanvasM Unit := do
  modifyCanvas Canvas.restore
  -- Ensure scissor matches the restored clip stack.
  liftCanvas (fun c => do
    Canvas.applyEffectiveScissor c
    pure c)
def translate (dx dy : Float) : CanvasM Unit := modifyCanvas (Canvas.translate dx dy)
def rotate (angle : Float) : CanvasM Unit := modifyCanvas (Canvas.rotate angle)
def scale (sx sy : Float) : CanvasM Unit := modifyCanvas (Canvas.scale sx sy)
def scaleUniform (s : Float) : CanvasM Unit := modifyCanvas (Canvas.scaleUniform s)
def setBaseTransform (t : Transform) : CanvasM Unit := modifyCanvas (Canvas.setBaseTransform t)
def resetTransform : CanvasM Unit := modifyCanvas Canvas.resetTransform
def resetState : CanvasM Unit := modifyCanvas Canvas.resetState
def resetStateAndScissor : CanvasM Unit := liftCanvas Canvas.resetStateAndScissor

/-- Run an action with the current state saved and restored.
    Equivalent to `save; action; restore` but guarantees restore is called.

    Example:
    ```lean
    saved do
      translate 100 100
      rotate 0.5
      fillRect (Rect.mk' 0 0 50 50)
    -- state is restored here
    ```
-/
def saved (action : CanvasM α) : CanvasM α := do
  save
  let result ← action
  restore
  pure result

/-- Run an action with a transform applied, then restore.
    The transform is applied after saving, and state is restored after the action.

    Example:
    ```lean
    withTransform (translate 100 100 *> rotate 0.5) do
      fillRect (Rect.mk' 0 0 50 50)
    -- state is restored here
    ```
-/
def withTransform (transform : CanvasM Unit) (action : CanvasM α) : CanvasM α := do
  save
  transform
  let result ← action
  restore
  pure result

/-! ## Style operations -/

def setFillColor (color : Color) : CanvasM Unit := modifyCanvas (Canvas.setFillColor color)
def setStrokeColor (color : Color) : CanvasM Unit := modifyCanvas (Canvas.setStrokeColor color)
def setLineWidth (w : Float) : CanvasM Unit := modifyCanvas (Canvas.setLineWidth w)
def setGlobalAlpha (a : Float) : CanvasM Unit := modifyCanvas (Canvas.setGlobalAlpha a)
def setFillStyle (style : FillStyle) : CanvasM Unit := modifyCanvas (Canvas.setFillStyle style)
def setFillLinearGradient (start finish : Point) (stops : Array GradientStop) : CanvasM Unit :=
  modifyCanvas (Canvas.setFillLinearGradient start finish stops)
def setFillRadialGradient (center : Point) (radius : Float) (stops : Array GradientStop) : CanvasM Unit :=
  modifyCanvas (Canvas.setFillRadialGradient center radius stops)

def setLineCap (cap : LineCap) : CanvasM Unit := modifyCanvas (Canvas.setLineCap cap)
def setLineJoin (join : LineJoin) : CanvasM Unit := modifyCanvas (Canvas.setLineJoin join)
def setDashPattern (pattern : Option DashPattern) : CanvasM Unit := modifyCanvas (Canvas.setDashPattern pattern)
def setDashed (dashLen gapLen : Float) : CanvasM Unit := modifyCanvas (Canvas.setDashed dashLen gapLen)
def setDotted : CanvasM Unit := modifyCanvas Canvas.setDotted
def setSolid : CanvasM Unit := modifyCanvas Canvas.setSolid

/-! ## Drawing operations -/

def fillPath (path : Path) : CanvasM Unit := liftCanvas (Canvas.fillPath path)
def fillRect (rect : Rect) : CanvasM Unit := liftCanvas (Canvas.fillRect rect)
def fillRectXYWH (x y width height : Float) : CanvasM Unit := liftCanvas (Canvas.fillRectXYWH x y width height)
def fillCircle (center : Point) (radius : Float) : CanvasM Unit := liftCanvas (Canvas.fillCircle center radius)
def fillEllipse (center : Point) (radiusX radiusY : Float) : CanvasM Unit := liftCanvas (Canvas.fillEllipse center radiusX radiusY)
def fillRoundedRect (rect : Rect) (cornerRadius : Float) : CanvasM Unit := liftCanvas (Canvas.fillRoundedRect rect cornerRadius)

def strokePath (path : Path) : CanvasM Unit := liftCanvas (Canvas.strokePath path)
def strokeRect (rect : Rect) : CanvasM Unit := liftCanvas (Canvas.strokeRect rect)
def strokeRectXYWH (x y width height : Float) : CanvasM Unit := liftCanvas (Canvas.strokeRectXYWH x y width height)
def strokeCircle (center : Point) (radius : Float) : CanvasM Unit := liftCanvas (Canvas.strokeCircle center radius)
def strokeEllipse (center : Point) (radiusX radiusY : Float) : CanvasM Unit := liftCanvas (Canvas.strokeEllipse center radiusX radiusY)
def strokeRoundedRect (rect : Rect) (cornerRadius : Float) : CanvasM Unit := liftCanvas (Canvas.strokeRoundedRect rect cornerRadius)
def drawLine (p1 p2 : Point) : CanvasM Unit := liftCanvas (Canvas.drawLine p1 p2)

/-! ## Dynamic Particle Rendering -/

def fillDynamicInstanced (shapeType : UInt32) (particles : Render.Dynamic.ParticleState)
    (halfSize rotation t : Float) : CanvasM Unit :=
  liftCanvas (Canvas.fillDynamicInstanced shapeType particles halfSize rotation t)

def fillDynamicInstancedAnimated (shapeType : UInt32) (particles : Render.Dynamic.ParticleState)
    (halfSize t spinSpeed : Float) : CanvasM Unit :=
  liftCanvas (Canvas.fillDynamicInstancedAnimated shapeType particles halfSize t spinSpeed)

def fillDynamicSprites (texture : FFI.Texture) (particles : Render.Dynamic.ParticleState)
    (halfSize : Float) (rotation : Float := 0.0) (alpha : Float := 1.0) : CanvasM Unit :=
  liftCanvas (Canvas.fillDynamicSprites texture particles halfSize rotation alpha)

def fillOrbitalRects (orbital : Render.Dynamic.OrbitalState) (t : Float) : CanvasM Unit :=
  liftCanvas (Canvas.fillOrbitalRects orbital t)

/-! ## Text operations -/

def fillText (text : String) (pos : Point) (font : Font) : CanvasM Unit := liftCanvas (Canvas.fillText text pos font)
def fillTextXY (text : String) (x y : Float) (font : Font) : CanvasM Unit := liftCanvas (Canvas.fillTextXY text x y font)
def fillTextColor (text : String) (pos : Point) (font : Font) (color : Color) : CanvasM Unit :=
  liftCanvas (Canvas.fillTextColor text pos font color)
def measureText (text : String) (font : Font) : CanvasM (Float × Float) := do
  let c ← get
  c.measureText text font

/-! ## Clipping -/

def clip (rect : Rect) : CanvasM Unit := liftCanvas (Canvas.clip rect)
def popClip : CanvasM Unit := liftCanvas Canvas.popClip
def unclip : CanvasM Unit := liftCanvas Canvas.unclip

/-! ## Accessors -/

def baseWidth : CanvasM Float := do return (← get).baseWidth
def baseHeight : CanvasM Float := do return (← get).baseHeight
def width : CanvasM Float := do (← get).width
def height : CanvasM Float := do (← get).height
def getCurrentSize : CanvasM (Float × Float) := do (← get).ctx.getCurrentSize

/-- Get the screen scale factor (e.g., 2.0 for Retina displays).
    Use this when loading fonts at physical pixel sizes:
    `Font.load path (logicalSize * (← getScreenScale)).toUInt32` -/
def getScreenScale : CanvasM Float := do return (← get).screenScale

/-! ## Window Input (lifted from Canvas/FFI.Window) -/

def getKeyCode : CanvasM UInt16 := do (← get).getKeyCode
def hasKeyPressed : CanvasM Bool := do (← get).hasKeyPressed
def clearKey : CanvasM Unit := do (← get).clearKey

def getPointerLock : CanvasM Bool := do
  FFI.Window.getPointerLock (← get).ctx.window

def setPointerLock (locked : Bool) : CanvasM Unit := do
  FFI.Window.setPointerLock (← get).ctx.window locked

def isKeyDown (keyCode : UInt16) : CanvasM Bool := do
  FFI.Window.isKeyDown (← get).ctx.window keyCode

def getMouseDelta : CanvasM (Float × Float) := do
  FFI.Window.getMouseDelta (← get).ctx.window

def getClick : CanvasM (Option FFI.ClickEvent) := do
  FFI.Window.getClick (← get).ctx.window

def clearClick : CanvasM Unit := do
  FFI.Window.clearClick (← get).ctx.window

/-! ## Context Accessors -/

def getRenderer : CanvasM FFI.Renderer := do return (← get).ctx.renderer
def getWindow : CanvasM FFI.Window := do return (← get).ctx.window

/-! ## Frame Loop -/

/-- Run a render loop entirely in CanvasM.
    The render function receives elapsed time in seconds and handles all drawing.
    Frame begin/end and polling are handled automatically. -/
def runLoopM (c : Canvas) (clearColor : Color) (render : Float → CanvasM Unit) : IO Unit := do
  let renderLoop := do
    let startTime ← IO.monoMsNow
    let mut canvas := c
    while !(← canvas.shouldClose) do
      let ok ← canvas.beginFrame clearColor
      if ok then
        let now ← IO.monoMsNow
        let elapsed := (now - startTime).toFloat / 1000.0
        canvas ← run' canvas (render elapsed)
        canvas ← canvas.endFrame
  runWithEventLoop c.ctx.window renderLoop

end CanvasM

/-! ## Canvas.run - Simplified Application Entry Point -/

namespace Canvas

/-- Run a canvas application with automatic setup and frame loop.

    This is the recommended way to create a simple canvas application.
    The frame callback receives (elapsed, deltaTime) in seconds and runs in CanvasM.

    Example:
    ```lean
    def main : IO Unit := do
      let font ← Font.load "/System/Library/Fonts/Monaco.ttf" 24
      Canvas.run { title := "My App" } fun elapsed dt => do
        resetTransform
        setFillColor Color.white
        fillTextXY s!"Time: {elapsed:.2f}" 20 30 font
    ```

    For stateful applications, capture IORefs in the closure:
    ```lean
    def main : IO Unit := do
      let counterRef ← IO.mkRef 0
      Canvas.run { title := "Counter" } fun elapsed dt => do
        let count ← counterRef.get
        if ← hasKeyPressed then
          counterRef.modify (· + 1)
          clearKey
        fillTextXY s!"Count: {count}" 20 30 font
    ```
-/
def run (config : CanvasConfig) (frame : Float → Float → CanvasM Unit) : IO Unit := do
  let screenScale ← if config.scaleToScreen then FFI.getScreenScale else pure 1.0
  let physWidth := (config.width * screenScale).toUInt32
  let physHeight := (config.height * screenScale).toUInt32
  let canvas ← Canvas.createWithScale physWidth physHeight config.title screenScale
  let renderLoop := do
    let startTime ← IO.monoMsNow
    let mut lastTime := startTime
    let mut c := canvas
    while !(← c.shouldClose) do
      let ok ← c.beginFrame config.clearColor
      if ok then
        let now ← IO.monoMsNow
        let elapsed := (now - startTime).toFloat / 1000.0
        let dt := (now - lastTime).toFloat / 1000.0
        lastTime := now
        -- Auto-scaling: apply screen scale transform so user works in logical pixels
        c ← CanvasM.run' c do
          CanvasM.resetTransform
          CanvasM.scale screenScale screenScale
          frame elapsed dt
        c ← c.endFrame
  runWithEventLoop canvas.ctx.window renderLoop

end Canvas

end Afferent

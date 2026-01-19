/-
  Arbor Render Builder
  A monadic DSL for building render command buffers.
-/
import Afferent.Arbor.Render.Command

namespace Afferent.Arbor

/-- Monad for building render command buffers.
    Uses StateM internally to accumulate commands. -/
abbrev RenderM := StateM RenderCommands

namespace RenderM

/-- Run a RenderM computation and return the accumulated commands. -/
def build (m : RenderM Unit) : RenderCommands :=
  (StateT.run m #[]).2

/-- Run a RenderM computation with an initial set of commands. -/
def buildWith (initial : RenderCommands) (m : RenderM Unit) : RenderCommands :=
  (StateT.run m initial).2

/-- Emit a single render command. -/
def emit (cmd : RenderCommand) : RenderM Unit :=
  modify (·.push cmd)

/-- Emit multiple render commands. -/
def emitAll (cmds : Array RenderCommand) : RenderM Unit :=
  modify (· ++ cmds)

/-! ## Rectangle Commands -/

/-- Fill a rectangle with a solid color. -/
def fillRect (rect : Rect) (color : Color) (cornerRadius : Float := 0) : RenderM Unit :=
  emit (.fillRect rect color cornerRadius)

/-- Fill a rectangle specified by position and size. -/
def fillRect' (x y w h : Float) (color : Color) (cornerRadius : Float := 0) : RenderM Unit :=
  emit (.fillRect (Rect.mk' x y w h) color cornerRadius)

/-- Fill a rectangle with a gradient or fill style. -/
def fillRectStyle (rect : Rect) (style : Afferent.FillStyle) (cornerRadius : Float := 0) : RenderM Unit :=
  emit (.fillRectStyle rect style cornerRadius)

/-- Stroke a rectangle outline. -/
def strokeRect (rect : Rect) (color : Color) (lineWidth : Float) (cornerRadius : Float := 0) : RenderM Unit :=
  emit (.strokeRect rect color lineWidth cornerRadius)

/-- Stroke a rectangle specified by position and size. -/
def strokeRect' (x y w h : Float) (color : Color) (lineWidth : Float) (cornerRadius : Float := 0) : RenderM Unit :=
  emit (.strokeRect (Rect.mk' x y w h) color lineWidth cornerRadius)

/-! ## Text Commands -/

/-- Fill text at a position. -/
def fillText (text : String) (x y : Float) (font : FontId) (color : Color) : RenderM Unit :=
  emit (.fillText text x y font color)

/-- Fill wrapped text within bounds. -/
def fillTextBlock (text : String) (rect : Rect) (font : FontId) (color : Color)
    (align : TextAlign := .left) (valign : TextVAlign := .top) : RenderM Unit :=
  emit (.fillTextBlock text rect font color align valign)

/-! ## Path Commands -/

/-- Fill a path with a solid color. -/
def fillPath (path : Path) (color : Color) : RenderM Unit :=
  emit (.fillPath path color)

/-- Fill a path with a gradient or fill style. -/
def fillPathStyle (path : Path) (style : Afferent.FillStyle) : RenderM Unit :=
  emit (.fillPathStyle path style)

/-- Stroke a path outline. -/
def strokePath (path : Path) (color : Color) (lineWidth : Float) : RenderM Unit :=
  emit (.strokePath path color lineWidth)

/-! ## Line Commands -/

/-- Stroke multiple line segments in a single command. -/
def strokeLineBatch (data : Array Float) (count : Nat) (lineWidth : Float) : RenderM Unit :=
  emit (.strokeLineBatch data count lineWidth)

/-- Fill multiple circles in a single command.
    data layout: [cx, cy, radius, r, g, b, a] per circle (7 floats). -/
def fillCircleBatch (data : Array Float) (count : Nat) : RenderM Unit :=
  emit (.fillCircleBatch data count)

/-! ## Polygon Commands -/

/-- Fill a convex polygon with a solid color. -/
def fillPolygon (points : Array Point) (color : Color) : RenderM Unit :=
  emit (.fillPolygon points color)

/-- Stroke a polygon outline. -/
def strokePolygon (points : Array Point) (color : Color) (lineWidth : Float := 1.0) : RenderM Unit :=
  emit (.strokePolygon points color lineWidth)

/-- Fill multiple instances of a tessellated polygon.
    High-performance path for rendering many identical shapes with different transforms.
    - pathHash: Hash of the canonical path (for mesh cache lookup)
    - vertices: Pre-tessellated vertex positions [x, y, x, y, ...]
    - indices: Triangle indices
    - instances: Per-instance transform and color data
    - centerX, centerY: Centroid for rotation pivot -/
def fillPolygonInstanced (pathHash : UInt64) (vertices : Array Float) (indices : Array UInt32)
    (instances : Array MeshInstance) (centerX centerY : Float) : RenderM Unit :=
  emit (.fillPolygonInstanced pathHash vertices indices instances centerX centerY)

/-- Stroke multiple arcs with instanced rendering.
    GPU-generated arc geometry for high-performance repeated arc strokes.
    - instances: Per-instance arc parameters (center, angles, radius, strokeWidth, color)
    - segments: Number of subdivisions per arc (higher = smoother, default 16) -/
def strokeArcInstanced (instances : Array ArcInstance) (segments : Nat := 16) : RenderM Unit :=
  emit (.strokeArcInstanced instances segments)

/-! ## Fragment Commands -/

/-- Draw using a custom shader fragment.
    High-performance GPU code for computing primitive positions/colors.
    - fragmentHash: Hash of the fragment definition (for pipeline caching)
    - primitiveType: Type of primitive (0 = circle)
    - params: Flat array of floats passed to the shader
    - instanceCount: Number of primitives to generate -/
def drawFragment (fragmentHash : UInt64) (primitiveType : UInt32)
    (params : Array Float) (instanceCount : UInt32) : RenderM Unit :=
  emit (.drawFragment fragmentHash primitiveType params instanceCount)

/-! ## Batched Tessellation Commands -/

/-- Fill a batch of pre-tessellated triangles.
    High-performance path for rendering many polygons with pre-computed tessellation.
    - vertices: Flat array of [x, y, r, g, b, a, ...] in NDC
    - indices: Triangle indices
    - vertexCount: Number of vertices (vertices.size / 6) -/
def fillTessellatedBatch (vertices : Array Float) (indices : Array UInt32) (vertexCount : Nat) : RenderM Unit :=
  if vertexCount == 0 then pure ()
  else emit (.fillTessellatedBatch vertices indices vertexCount)

/-! ## Circles -/

/-- Fill a circle with a solid color (GPU-batched). -/
def fillCircle (center : Point) (radius : Float) (color : Color) : RenderM Unit :=
  emit (.fillCircle center radius color)

/-- Fill a circle with a solid color (convenience version with x, y coordinates). -/
def fillCircle' (cx cy radius : Float) (color : Color) : RenderM Unit :=
  emit (.fillCircle ⟨cx, cy⟩ radius color)

/-- Stroke a circle outline. -/
def strokeCircle (center : Point) (radius : Float) (color : Color) (lineWidth : Float := 1.0) : RenderM Unit :=
  emit (.strokeCircle center radius color lineWidth)

/-- Stroke a circle outline (convenience version with x, y coordinates). -/
def strokeCircle' (cx cy radius : Float) (color : Color) (lineWidth : Float := 1.0) : RenderM Unit :=
  emit (.strokeCircle ⟨cx, cy⟩ radius color lineWidth)

/-! ## Clipping -/

/-- Push a clipping rectangle. -/
def pushClip (rect : Rect) : RenderM Unit :=
  emit (.pushClip rect)

/-- Pop the clipping rectangle. -/
def popClip : RenderM Unit :=
  emit .popClip

/-- Execute commands within a clip region. -/
def withClip (rect : Rect) (m : RenderM Unit) : RenderM Unit := do
  pushClip rect
  m
  popClip

/-! ## Transforms -/

/-- Push a translation transform. -/
def pushTranslate (dx dy : Float) : RenderM Unit :=
  emit (.pushTranslate dx dy)

/-- Push a rotation transform. -/
def pushRotate (angle : Float) : RenderM Unit :=
  emit (.pushRotate angle)

/-- Push a scaling transform. -/
def pushScale (sx sy : Float) : RenderM Unit :=
  emit (.pushScale sx sy)

/-- Pop the top transform. -/
def popTransform : RenderM Unit :=
  emit .popTransform

/-- Execute commands within a translation. -/
def withTranslate (dx dy : Float) (m : RenderM Unit) : RenderM Unit := do
  pushTranslate dx dy
  m
  popTransform

/-- Execute commands within a rotation. -/
def withRotate (angle : Float) (m : RenderM Unit) : RenderM Unit := do
  pushRotate angle
  m
  popTransform

/-- Execute commands within a scale. -/
def withScale (sx sy : Float) (m : RenderM Unit) : RenderM Unit := do
  pushScale sx sy
  m
  popTransform

/-! ## State Save/Restore -/

/-- Save the current graphics state. -/
def save : RenderM Unit :=
  emit .save

/-- Restore the previously saved graphics state. -/
def restore : RenderM Unit :=
  emit .restore

/-- Execute commands with saved/restored state. -/
def withSave (m : RenderM Unit) : RenderM Unit := do
  save
  m
  restore

end RenderM

end Afferent.Arbor

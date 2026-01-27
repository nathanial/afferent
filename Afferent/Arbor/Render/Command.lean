/-
  Arbor Render Commands
  Abstract rendering commands that can be interpreted by different backends.
-/
import Afferent.Arbor.Core.Types
import Afferent.Core.Path
import Afferent.Core.Paint

namespace Afferent.Arbor

/-- Text horizontal alignment. -/
inductive TextAlign where
  | left
  | center
  | right
deriving Repr, BEq, Inhabited

/-- Text vertical alignment. -/
inductive TextVAlign where
  | top
  | middle
  | bottom
deriving Repr, BEq, Inhabited

/-- Instance data for instanced polygon rendering.
    8 floats per instance: position(2), rotation(1), scale(1), color(4). -/
structure MeshInstance where
  x : Float       -- Position X
  y : Float       -- Position Y
  rotation : Float -- Rotation in radians
  scale : Float   -- Uniform scale
  r : Float       -- Red component [0, 1]
  g : Float       -- Green component [0, 1]
  b : Float       -- Blue component [0, 1]
  a : Float       -- Alpha component [0, 1]
deriving Repr, BEq, Inhabited

/-- Instance data for instanced arc stroke rendering.
    10 floats per instance: center(2), startAngle(1), sweepAngle(1),
    radius(1), strokeWidth(1), color(4). -/
structure ArcInstance where
  centerX : Float      -- Arc center X position
  centerY : Float      -- Arc center Y position
  startAngle : Float   -- Start angle in radians
  sweepAngle : Float   -- Sweep angle in radians (can be negative for CCW)
  radius : Float       -- Arc radius
  strokeWidth : Float  -- Stroke thickness
  r : Float            -- Red component [0, 1]
  g : Float            -- Green component [0, 1]
  b : Float            -- Blue component [0, 1]
  a : Float            -- Alpha component [0, 1]
deriving Repr, BEq, Inhabited

/-- Abstract render command.
    These commands describe what to render without knowing how. -/
inductive RenderCommand where
  /-- Fill a rectangle with a solid color. -/
  | fillRect (rect : Rect) (color : Color) (cornerRadius : Float := 0)

  /-- Fill a rectangle with a gradient or solid fill style. -/
  | fillRectStyle (rect : Rect) (style : Afferent.FillStyle) (cornerRadius : Float := 0)

  /-- Stroke a rectangle outline. -/
  | strokeRect (rect : Rect) (color : Color) (lineWidth : Float) (cornerRadius : Float := 0)

  /-- Fill a circle with a solid color. -/
  | fillCircle (center : Point) (radius : Float) (color : Color)

  /-- Stroke a circle outline. -/
  | strokeCircle (center : Point) (radius : Float) (color : Color) (lineWidth : Float)

  /-- Stroke a line segment. -/
  | strokeLine (p1 p2 : Point) (color : Color) (lineWidth : Float)

  /-- Stroke multiple line segments in a single command.
      data layout: [x1, y1, x2, y2, r, g, b, a, padding] per line. -/
  | strokeLineBatch (data : Array Float) (count : Nat) (lineWidth : Float)

  /-- Fill multiple circles in a single command.
      data layout: [cx, cy, radius, r, g, b, a] per circle (7 floats). -/
  | fillCircleBatch (data : Array Float) (count : Nat)

  /-- Fill text at a position. -/
  | fillText (text : String) (x y : Float) (font : FontId) (color : Color)

  /-- Fill wrapped/multi-line text within bounds. -/
  | fillTextBlock (text : String) (rect : Rect) (font : FontId) (color : Color)
                  (align : TextAlign) (valign : TextVAlign)

  /-- Fill a convex polygon with a solid color. -/
  | fillPolygon (points : Array Point) (color : Color)

  /-- Stroke a convex polygon outline. -/
  | strokePolygon (points : Array Point) (color : Color) (lineWidth : Float)

  /-- Fill a path with a solid color. -/
  | fillPath (path : Path) (color : Color)

  /-- Fill a path with a gradient or solid fill style. -/
  | fillPathStyle (path : Path) (style : Afferent.FillStyle)

  /-- Stroke a path outline. -/
  | strokePath (path : Path) (color : Color) (lineWidth : Float)

  /-- Fill a polygon with instanced rendering.
      Uses cached GPU mesh for high-performance repeated polygon rendering.
      - pathHash: 64-bit hash of path for mesh cache lookup
      - vertices: Flat array of [x, y, ...] tessellated positions
      - indices: Triangle indices
      - instances: Array of MeshInstance (x, y, rotation, scale, color)
      - centerX, centerY: Mesh centroid for rotation pivot -/
  | fillPolygonInstanced (pathHash : UInt64) (vertices : Array Float) (indices : Array UInt32)
      (instances : Array MeshInstance) (centerX centerY : Float)

  /-- Stroke arcs with instanced rendering.
      GPU-generated arc geometry for high-performance repeated arc strokes.
      - instances: Array of ArcInstance (center, angles, radius, strokeWidth, color)
      - segments: Number of subdivisions per arc (higher = smoother, default 16) -/
  | strokeArcInstanced (instances : Array ArcInstance) (segments : Nat := 16)

  /-- Draw using a custom shader fragment.
      High-performance GPU code for computing primitive positions/colors.
      - fragmentHash: Hash of the fragment definition (for pipeline caching)
      - primitiveType: Type of primitive (0 = circle)
      - params: Flat array of floats passed to the shader
      - instanceCount: Number of primitives to generate -/
  | drawFragment (fragmentHash : UInt64) (primitiveType : UInt32)
      (params : Array Float) (instanceCount : UInt32)

  /-- Fill a batch of pre-tessellated triangles.
      High-performance path for rendering many polygons with pre-computed tessellation.
      - vertices: Flat array of [x, y, r, g, b, a, ...] in NDC
      - indices: Triangle indices
      - vertexCount: Number of vertices (vertices.size / 6) -/
  | fillTessellatedBatch (vertices : Array Float) (indices : Array UInt32) (vertexCount : Nat)

  /-- Push a clipping rectangle onto the clip stack. -/
  | pushClip (rect : Rect)

  /-- Pop the top clipping rectangle from the stack. -/
  | popClip

  /-- Push a translation transform. -/
  | pushTranslate (dx dy : Float)

  /-- Push a rotation transform. -/
  | pushRotate (angle : Float)

  /-- Push a scaling transform. -/
  | pushScale (sx sy : Float)

  /-- Pop the top transform from the stack. -/
  | popTransform

  /-- Save the current graphics state. -/
  | save

  /-- Restore the previously saved graphics state. -/
  | restore
deriving Repr

namespace RenderCommand

/-- Create a filled rectangle command. -/
def fill (x y w h : Float) (color : Color) (radius : Float := 0) : RenderCommand :=
  .fillRect (Rect.mk' x y w h) color radius

/-- Create a filled rectangle command with a fill style. -/
def fillStyle (x y w h : Float) (style : Afferent.FillStyle) (radius : Float := 0) : RenderCommand :=
  .fillRectStyle (Rect.mk' x y w h) style radius

/-- Create a stroked rectangle command. -/
def stroke (x y w h : Float) (color : Color) (lineWidth : Float) (radius : Float := 0) : RenderCommand :=
  .strokeRect (Rect.mk' x y w h) color lineWidth radius

/-- Create a text command. -/
def text (s : String) (x y : Float) (font : FontId) (color : Color) : RenderCommand :=
  .fillText s x y font color

/-- Create a filled polygon command. -/
def fillPoly (points : Array Point) (color : Color) : RenderCommand :=
  .fillPolygon points color

/-- Create a stroked polygon command. -/
def strokePoly (points : Array Point) (color : Color) (lineWidth : Float := 1.0) : RenderCommand :=
  .strokePolygon points color lineWidth

/-- Create a filled circle command. -/
def circle (cx cy radius : Float) (color : Color) : RenderCommand :=
  .fillCircle ⟨cx, cy⟩ radius color

/-- Create a stroked circle command. -/
def strokeCircle' (cx cy radius : Float) (color : Color) (lineWidth : Float := 1.0) : RenderCommand :=
  .strokeCircle ⟨cx, cy⟩ radius color lineWidth

end RenderCommand

/-! ## Command Bounds for Overlap Analysis -/

/-- Screen-space bounding box for a render command.
    Used for overlap detection during command coalescing. -/
structure CommandBounds where
  minX : Float
  minY : Float
  maxX : Float
  maxY : Float
deriving Repr, BEq

namespace CommandBounds

/-- Check if two bounding boxes overlap. -/
def overlaps (a b : CommandBounds) : Bool :=
  a.minX < b.maxX && a.maxX > b.minX &&
  a.minY < b.maxY && a.maxY > b.minY

/-- Create bounds from rectangle (x, y, width, height). -/
def fromRect (x y w h : Float) : CommandBounds :=
  { minX := x, minY := y, maxX := x + w, maxY := y + h }

/-- Create bounds from circle (centerX, centerY, radius). -/
def fromCircle (cx cy r : Float) : CommandBounds :=
  { minX := cx - r, minY := cy - r, maxX := cx + r, maxY := cy + r }

end CommandBounds

/-- Command category for batching (which commands can batch together). -/
inductive CommandCategory
  | fillRect
  | strokeRect
  | fillCircle
  | strokeCircle
  | strokeLine
  | fillText
  | fillPolygonInstanced
  | strokeArcInstanced
  | drawFragment
  | fillTessellatedBatch
  | other
deriving Repr, BEq, Hashable

namespace CommandCategory

/-- Priority for sorting commands by category during coalescing.
    Lower values come first in output. Drawing commands are grouped together,
    state commands ("other") come last to stay near their original position. -/
def sortPriority : CommandCategory → Nat
  | .fillRect => 0
  | .fillCircle => 1
  | .strokeRect => 2
  | .strokeCircle => 3
  | .strokeLine => 4
  | .strokeArcInstanced => 5
  | .drawFragment => 6
  | .fillText => 7
  | .fillPolygonInstanced => 8
  | .fillTessellatedBatch => 9
  | .other => 10

end CommandCategory

namespace RenderCommand

/-- Get the batching category for a render command. -/
def category : RenderCommand → CommandCategory
  | .fillRect .. | .fillRectStyle .. => .fillRect
  | .strokeRect .. => .strokeRect
  | .fillCircle .. | .fillCircleBatch .. => .fillCircle
  | .strokeCircle .. => .strokeCircle
  | .strokeLine .. | .strokeLineBatch .. => .strokeLine
  | .fillText .. | .fillTextBlock .. => .fillText
  | .fillPolygonInstanced .. => .fillPolygonInstanced
  | .strokeArcInstanced .. => .strokeArcInstanced
  | .drawFragment .. => .drawFragment
  | .fillTessellatedBatch .. => .fillTessellatedBatch
  | _ => .other

end RenderCommand

/-- Render command with computed screen-space bounds for overlap analysis.
    Used during overlap-aware coalescing to determine which commands can be reordered. -/
structure BoundedCommand where
  /-- The underlying render command. -/
  cmd : RenderCommand
  /-- Screen-space bounding box. None for state-changing commands. -/
  bounds : Option CommandBounds
  /-- Original index in the command stream for stable sorting. -/
  originalIndex : Nat
deriving Repr

instance : Inhabited BoundedCommand where
  default := { cmd := .save, bounds := none, originalIndex := 0 }

/-- A batch of render commands. -/
abbrev RenderCommands := Array RenderCommand

namespace RenderCommands

def empty : RenderCommands := #[]

def single (cmd : RenderCommand) : RenderCommands := #[cmd]

def add (cmds : RenderCommands) (cmd : RenderCommand) : RenderCommands :=
  cmds.push cmd

def merge (cmds1 cmds2 : RenderCommands) : RenderCommands :=
  cmds1 ++ cmds2

/-- Wrap commands in a clip region. -/
def withClip (rect : Rect) (cmds : RenderCommands) : RenderCommands :=
  #[.pushClip rect] ++ cmds ++ #[.popClip]

/-- Wrap commands in a translation. -/
def withTranslate (dx dy : Float) (cmds : RenderCommands) : RenderCommands :=
  #[.pushTranslate dx dy] ++ cmds ++ #[.popTransform]

end RenderCommands

end Afferent.Arbor

/-
  Afferent Tessellation Types
-/
import Afferent.Core.Types

namespace Afferent

namespace Tessellation

/-- Pi constant for arc calculations. -/
private def pi : Float := 3.14159265358979323846

/-- Vertex size for 2D rendering: x, y, r, g, b, a -/
def vertexSize2D : Nat := 6

/-- Float stride for GPU stroke segments (see StrokeSegment packing). -/
def strokeSegmentStride : Nat := 18

/-- Vertex size for 3D rendering: x, y, z, nx, ny, nz, r, g, b, a -/
def vertexSize3D : Nat := 10

/-- Vertex size for textured 3D: x, y, z, nx, ny, nz, u, v, r, g, b, a -/
def vertexSize3DTextured : Nat := 12

end Tessellation

/-- Result of tessellating a path into triangles. -/
structure TessellationResult where
  /-- Flat array of vertex data: x, y, r, g, b, a per vertex. -/
  vertices : Array Float
  /-- Triangle indices (3 per triangle). -/
  indices : Array UInt32
deriving Repr, Inhabited

/-- Stroke segment kind for GPU extrusion. -/
inductive StrokeSegmentKind where
  | line
  | cubic
deriving Repr, BEq, Inhabited

/-- Parametric stroke segment (CPU-side) for GPU extrusion. -/
structure StrokeSegment where
  p0 : Point
  p1 : Point
  c1 : Point
  c2 : Point
  prevDir : Point
  nextDir : Point
  startDist : Float
  length : Float
  hasPrev : Bool
  hasNext : Bool
  kind : StrokeSegmentKind
deriving Repr, Inhabited

/-- GPU stroke segment buffers split by segment type. -/
structure StrokePathSegments where
  lineSegments : Array Float
  curveSegments : Array Float
  lineCount : Nat
  curveCount : Nat
deriving Repr, Inhabited

end Afferent

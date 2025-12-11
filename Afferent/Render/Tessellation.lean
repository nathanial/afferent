/-
  Afferent Tessellation
  Convert paths to triangles for GPU rendering.
-/
import Afferent.Core.Types
import Afferent.Core.Path

namespace Afferent

/-- Result of tessellating a path into triangles. -/
structure TessellationResult where
  /-- Flat array of vertex data: x, y, r, g, b, a per vertex. -/
  vertices : Array Float
  /-- Triangle indices (3 per triangle). -/
  indices : Array UInt32
deriving Repr, Inhabited

namespace Tessellation

/-- Flatten a cubic Bezier curve to line segments using de Casteljau subdivision.
    Returns array of points (excluding start point, which caller already has). -/
partial def flattenCubicBezier (p0 p1 p2 p3 : Point) (tolerance : Float := 0.5) : Array Point :=
  let rec go (p0 p1 p2 p3 : Point) (acc : Array Point) : Array Point :=
    -- Check if curve is flat enough using distance from control points to line
    let d1 := linePointDistance p0 p3 p1
    let d2 := linePointDistance p0 p3 p2
    if max d1 d2 < tolerance then
      acc.push p3
    else
      -- Subdivide at t=0.5 using de Casteljau
      let m01 := Point.midpoint p0 p1
      let m12 := Point.midpoint p1 p2
      let m23 := Point.midpoint p2 p3
      let m012 := Point.midpoint m01 m12
      let m123 := Point.midpoint m12 m23
      let mid := Point.midpoint m012 m123
      let acc' := go p0 m01 m012 mid acc
      go mid m123 m23 p3 acc'
  go p0 p1 p2 p3 #[]
where
  linePointDistance (lineStart lineEnd point : Point) : Float :=
    let dx := lineEnd.x - lineStart.x
    let dy := lineEnd.y - lineStart.y
    let len := Float.sqrt (dx * dx + dy * dy)
    if len < 0.0001 then
      Point.distance lineStart point
    else
      Float.abs ((point.x - lineStart.x) * dy - (point.y - lineStart.y) * dx) / len

/-- Flatten a quadratic Bezier curve by converting to cubic and flattening. -/
def flattenQuadraticBezier (p0 cp p2 : Point) (tolerance : Float := 0.5) : Array Point :=
  -- Convert quadratic to cubic: cubic control points are 2/3 of the way to quadratic control point
  let cp1 := Point.lerp p0 cp (2.0 / 3.0)
  let cp2 := Point.lerp p2 cp (2.0 / 3.0)
  flattenCubicBezier p0 cp1 cp2 p2 tolerance

/-- Convert a path to an array of polygon vertices (flatten all curves). -/
def pathToPolygon (path : Path) (tolerance : Float := 0.5) : Array Point := Id.run do
  let mut points : Array Point := #[]
  let mut current := Point.zero
  let mut subpathStart := Point.zero

  for cmd in path.commands do
    match cmd with
    | .moveTo p =>
      current := p
      subpathStart := p
      points := points.push p
    | .lineTo p =>
      current := p
      points := points.push p
    | .quadraticCurveTo cp p =>
      let flat := flattenQuadraticBezier current cp p tolerance
      for pt in flat do
        points := points.push pt
      current := p
    | .bezierCurveTo cp1 cp2 p =>
      let flat := flattenCubicBezier current cp1 cp2 p tolerance
      for pt in flat do
        points := points.push pt
      current := p
    | .rect r =>
      -- Add rectangle vertices
      points := points.push r.topLeft
      points := points.push r.topRight
      points := points.push r.bottomRight
      points := points.push r.bottomLeft
      current := r.topLeft
      subpathStart := r.topLeft
    | .closePath =>
      current := subpathStart
    | .arc _ _ _ _ _ =>
      -- TODO: Convert arc to bezier segments
      pure ()
    | .arcTo _ _ _ =>
      -- TODO: Implement arcTo
      pure ()

  return points

/-- Simple fan triangulation for convex polygons.
    Triangulates from first vertex to all other vertices. -/
def triangulateConvexFan (numVertices : Nat) : Array UInt32 := Id.run do
  if numVertices < 3 then return #[]
  let mut indices : Array UInt32 := #[]
  for i in [1:numVertices - 1] do
    indices := indices.push 0
    indices := indices.push i.toUInt32
    indices := indices.push (i + 1).toUInt32
  return indices

/-- Tessellate a rectangle into two triangles. -/
def tessellateRect (r : Rect) (color : Color) : TessellationResult :=
  let tl := r.topLeft
  let tr := r.topRight
  let bl := r.bottomLeft
  let br := r.bottomRight

  -- 4 vertices, 6 floats each (x, y, r, g, b, a)
  let vertices := #[
    tl.x, tl.y, color.r, color.g, color.b, color.a,  -- 0: top-left
    tr.x, tr.y, color.r, color.g, color.b, color.a,  -- 1: top-right
    br.x, br.y, color.r, color.g, color.b, color.a,  -- 2: bottom-right
    bl.x, bl.y, color.r, color.g, color.b, color.a   -- 3: bottom-left
  ]

  -- Two triangles: (0,1,2) and (0,2,3)
  let indices : Array UInt32 := #[0, 1, 2, 0, 2, 3]

  { vertices, indices }

/-- Tessellate a convex path with a solid color. -/
def tessellateConvexPath (path : Path) (color : Color) (tolerance : Float := 0.5) : TessellationResult := Id.run do
  let points := pathToPolygon path tolerance

  if points.size < 3 then
    return { vertices := #[], indices := #[] }

  -- Build vertex array
  let mut vertices : Array Float := #[]
  for p in points do
    vertices := vertices.push p.x
    vertices := vertices.push p.y
    vertices := vertices.push color.r
    vertices := vertices.push color.g
    vertices := vertices.push color.b
    vertices := vertices.push color.a

  let indices := triangulateConvexFan points.size
  return { vertices, indices }

/-- Convert pixel coordinates to NDC (Normalized Device Coordinates).
    NDC range is -1 to 1, with (0,0) at center.
    Pixel coordinates have (0,0) at top-left. -/
def pixelToNDC (x y : Float) (width height : Float) : Point :=
  { x := (x / width) * 2.0 - 1.0
    y := 1.0 - (y / height) * 2.0 }  -- Flip Y for top-left origin

/-- Tessellate a rectangle with pixel coordinates, converting to NDC. -/
def tessellateRectNDC (r : Rect) (color : Color) (screenWidth screenHeight : Float) : TessellationResult :=
  let toNDC := fun (p : Point) => pixelToNDC p.x p.y screenWidth screenHeight
  let tl := toNDC r.topLeft
  let tr := toNDC r.topRight
  let bl := toNDC r.bottomLeft
  let br := toNDC r.bottomRight

  let vertices := #[
    tl.x, tl.y, color.r, color.g, color.b, color.a,
    tr.x, tr.y, color.r, color.g, color.b, color.a,
    br.x, br.y, color.r, color.g, color.b, color.a,
    bl.x, bl.y, color.r, color.g, color.b, color.a
  ]

  let indices : Array UInt32 := #[0, 1, 2, 0, 2, 3]
  { vertices, indices }

/-- Tessellate a convex path with pixel coordinates, converting to NDC. -/
def tessellateConvexPathNDC (path : Path) (color : Color)
    (screenWidth screenHeight : Float) (tolerance : Float := 0.5) : TessellationResult := Id.run do
  let points := pathToPolygon path tolerance

  if points.size < 3 then
    return { vertices := #[], indices := #[] }

  let mut vertices : Array Float := #[]
  for p in points do
    let ndc := pixelToNDC p.x p.y screenWidth screenHeight
    vertices := vertices.push ndc.x
    vertices := vertices.push ndc.y
    vertices := vertices.push color.r
    vertices := vertices.push color.g
    vertices := vertices.push color.b
    vertices := vertices.push color.a

  let indices := triangulateConvexFan points.size
  return { vertices, indices }

end Tessellation

end Afferent

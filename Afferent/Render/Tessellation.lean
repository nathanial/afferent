/-
  Afferent Tessellation
  Convert paths to triangles for GPU rendering.
-/
import Afferent.Core.Types
import Afferent.Core.Path
import Afferent.Core.Paint
import Afferent.Core.Transform
import Afferent.Render.Earcut


namespace Afferent

namespace Tessellation

/-- Pi constant for arc calculations. -/
private def pi : Float := 3.14159265358979323846

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

namespace Tessellation

/-- Vertex size for 2D rendering: x, y, r, g, b, a -/
def vertexSize2D : Nat := 6

/-- Float stride for GPU stroke segments (see StrokeSegment packing). -/
def strokeSegmentStride : Nat := 18

/-- Vertex size for 3D rendering: x, y, z, nx, ny, nz, r, g, b, a -/
def vertexSize3D : Nat := 10

/-- Vertex size for textured 3D: x, y, z, nx, ny, nz, u, v, r, g, b, a -/
def vertexSize3DTextured : Nat := 12

/-- Cached rectangle indices for two triangles (0,1,2) and (0,2,3).
    Reused across all rectangle tessellation to avoid repeated allocation. -/
private def rectIndices : Array UInt32 := #[0, 1, 2, 0, 2, 3]

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
  -- Start with capacity for typical curve subdivision depth (8-16 segments)
  go p0 p1 p2 p3 (Array.mkEmpty 16)
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

/-! ## arcTo Geometry -/

/-- Compute the tangent arc for arcTo command.
    Given current point, corner point p1, direction point p2, and radius:
    1. Find tangent points on both line segments
    2. Calculate arc center
    3. Return (tangentPoint1, bezierSegments, tangentPoint2)
    Returns none for degenerate cases (collinear points, zero radius). -/
def computeArcTo (current p1 p2 : Point) (radius : Float)
    : Option (Point × Array (Point × Point × Point) × Point) := Id.run do
  if radius <= 0 then return none

  -- Direction vectors
  let d1x := current.x - p1.x
  let d1y := current.y - p1.y
  let d2x := p2.x - p1.x
  let d2y := p2.y - p1.y

  -- Normalize direction vectors
  let len1 := Float.sqrt (d1x * d1x + d1y * d1y)
  let len2 := Float.sqrt (d2x * d2x + d2y * d2y)
  if len1 < 0.0001 || len2 < 0.0001 then return none

  let u1x := d1x / len1
  let u1y := d1y / len1
  let u2x := d2x / len2
  let u2y := d2y / len2

  -- Cross product to check if lines are collinear
  let cross := u1x * u2y - u1y * u2x
  if cross.abs < 0.0001 then return none  -- Collinear, no arc possible

  -- Compute half-angle between the two directions
  -- The dot product gives cos(angle between directions)
  let dot := u1x * u2x + u1y * u2y
  let clampedDot := if dot < -1.0 then -1.0 else if dot > 1.0 then 1.0 else dot
  let halfAngle := Float.acos clampedDot / 2.0

  -- Distance from corner to tangent point: radius / tan(halfAngle)
  let tanHalf := Float.tan halfAngle
  if tanHalf.abs < 0.0001 then return none
  let dist := radius / tanHalf

  -- Clamp distance if it exceeds the line segment lengths
  let minLen := if len1 < len2 then len1 else len2
  let dist := if dist < minLen then dist else minLen
  let actualRadius := dist * tanHalf

  -- Tangent points
  let t1 := Point.mk' (p1.x + u1x * dist) (p1.y + u1y * dist)
  let t2 := Point.mk' (p1.x + u2x * dist) (p1.y + u2y * dist)

  -- Arc center is at distance radius from both tangent points,
  -- perpendicular to the line segments
  -- Unit bisector direction (pointing toward center)
  let bisectX := u1x + u2x
  let bisectY := u1y + u2y
  let bisectLen := Float.sqrt (bisectX * bisectX + bisectY * bisectY)
  if bisectLen < 0.0001 then return none

  -- The center is at distance radius/sin(halfAngle) from p1 along bisector
  let sinHalf := Float.sin halfAngle
  if sinHalf.abs < 0.0001 then return none
  let centerDist := actualRadius / sinHalf

  -- Determine which side the center is on (based on cross product sign)
  let centerX := p1.x + (bisectX / bisectLen) * centerDist
  let centerY := p1.y + (bisectY / bisectLen) * centerDist
  let center := Point.mk' centerX centerY

  -- Calculate start and end angles for the arc
  let startAngle := Float.atan2 (t1.y - center.y) (t1.x - center.x)
  let endAngle := Float.atan2 (t2.y - center.y) (t2.x - center.x)

  -- Determine if we go clockwise or counterclockwise
  let counterclockwise := cross > 0

  -- Generate bezier segments for the arc
  let beziers := Path.arcToBeziers center actualRadius startAngle endAngle counterclockwise

  return some (t1, beziers, t2)

/-- Convert a path to an array of polygon vertices (flatten all curves).
    Also returns whether the path is closed (ends with closePath or first/last points match). -/
def pathToPolygonWithClosed (path : Path) (tolerance : Float := 0.5) : Array Point × Bool := Id.run do
  -- Estimate capacity: typically 2-4 points per command on average
  let mut points : Array Point := Array.mkEmpty (path.commands.size * 4)
  let mut current := Point.zero
  let mut subpathStart := Point.zero
  let mut isClosed := false

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
      -- Add rectangle vertices (rectangles are implicitly closed)
      points := points.push r.topLeft
      points := points.push r.topRight
      points := points.push r.bottomRight
      points := points.push r.bottomLeft
      current := r.topLeft
      subpathStart := r.topLeft
      isClosed := true
    | .closePath =>
      isClosed := true
      current := subpathStart
    | .arc center radius startAngle endAngle counterclockwise =>
      -- Convert arc to bezier segments
      let beziers := Path.arcToBeziers center radius startAngle endAngle counterclockwise
      for (cp1, cp2, endPt) in beziers do
        let flat := flattenCubicBezier current cp1 cp2 endPt tolerance
        for pt in flat do
          points := points.push pt
        current := endPt
    | .arcTo p1 p2 radius =>
      -- arcTo draws a line to the first tangent point, then an arc to the second tangent point
      match computeArcTo current p1 p2 radius with
      | some (t1, beziers, t2) =>
        -- Line to first tangent point
        points := points.push t1
        -- Flatten the arc beziers
        let mut arcCurrent := t1
        for (cp1, cp2, endPt) in beziers do
          let flat := flattenCubicBezier arcCurrent cp1 cp2 endPt tolerance
          for pt in flat do
            points := points.push pt
          arcCurrent := endPt
        current := t2
      | none =>
        -- Degenerate case: just draw line to p1
        points := points.push p1
        current := p1

  return (points, isClosed)

/-- Convert a path to an array of polygon vertices (flatten all curves). -/
private def trimDuplicateTail (ring : Array Point) : Array Point := Id.run do
  if ring.size < 2 then
    return ring
  let first := ring[0]!
  let last := ring[ring.size - 1]!
  if first == last then
    let mut trimmed : Array Point := Array.mkEmpty (ring.size - 1)
    for i in [:ring.size - 1] do
      trimmed := trimmed.push ring[i]!
    return trimmed
  return ring

/-- Convert a path to an array of polygon vertices (flatten all curves). -/
def pathToPolygon (path : Path) (tolerance : Float := 0.5) : Array Point :=
  let points := (pathToPolygonWithClosed path tolerance).1
  trimDuplicateTail points

/-- Convert a path to an array of rings (each ring is a flattened subpath). -/
def pathToRings (path : Path) (tolerance : Float := 0.5) : Array (Array Point) := Id.run do
  let mut rings : Array (Array Point) := #[]
  let mut ring : Array Point := #[]
  let mut current := Point.zero
  let mut subpathStart := Point.zero
  let mut hasCurrent := false

  for cmd in path.commands do
    match cmd with
    | .moveTo p =>
      if ring.size >= 3 then
        let trimmed := trimDuplicateTail ring
        if trimmed.size >= 3 then
          rings := rings.push trimmed
      ring := #[]
      hasCurrent := true
      current := p
      subpathStart := p
      ring := ring.push p
    | .lineTo p =>
      if !hasCurrent then
        hasCurrent := true
        current := p
        subpathStart := p
        ring := ring.push p
      else
        if ring.isEmpty then
          ring := ring.push current
        ring := ring.push p
        current := p
    | .quadraticCurveTo cp p =>
      if !hasCurrent then
        hasCurrent := true
        current := p
        subpathStart := p
        ring := ring.push p
      else
        if ring.isEmpty then
          ring := ring.push current
        let flat := flattenQuadraticBezier current cp p tolerance
        for pt in flat do
          ring := ring.push pt
        current := p
    | .bezierCurveTo cp1 cp2 p =>
      if !hasCurrent then
        hasCurrent := true
        current := p
        subpathStart := p
        ring := ring.push p
      else
        if ring.isEmpty then
          ring := ring.push current
        let flat := flattenCubicBezier current cp1 cp2 p tolerance
        for pt in flat do
          ring := ring.push pt
        current := p
    | .arc center radius startAngle endAngle counterclockwise =>
      if !hasCurrent then
        let endPt := Point.mk'
          (center.x + radius * Float.cos endAngle)
          (center.y + radius * Float.sin endAngle)
        hasCurrent := true
        current := endPt
        subpathStart := endPt
        ring := ring.push endPt
      else
        if ring.isEmpty then
          ring := ring.push current
        let beziers := Path.arcToBeziers center radius startAngle endAngle counterclockwise
        let mut arcCurrent := current
        for (cp1, cp2, endPt) in beziers do
          let flat := flattenCubicBezier arcCurrent cp1 cp2 endPt tolerance
          for pt in flat do
            ring := ring.push pt
          arcCurrent := endPt
        current := arcCurrent
    | .arcTo p1 p2 radius =>
      if !hasCurrent then
        hasCurrent := true
        current := p1
        subpathStart := p1
        ring := ring.push p1
      else
        match computeArcTo current p1 p2 radius with
        | some (t1, beziers, t2) =>
          if ring.isEmpty then
            ring := ring.push current
          if Point.distance current t1 > 0.0001 then
            ring := ring.push t1
          let mut arcCurrent := t1
          for (cp1, cp2, endPt) in beziers do
            let flat := flattenCubicBezier arcCurrent cp1 cp2 endPt tolerance
            for pt in flat do
              ring := ring.push pt
            arcCurrent := endPt
          current := t2
        | none =>
          if ring.isEmpty then
            ring := ring.push current
          ring := ring.push p1
          current := p1
    | .rect rect =>
      if ring.size >= 3 then
        let trimmed := trimDuplicateTail ring
        if trimmed.size >= 3 then
          rings := rings.push trimmed
      ring := #[]
      let tl := rect.topLeft
      let tr := rect.topRight
      let br := rect.bottomRight
      let bl := rect.bottomLeft
      rings := rings.push (trimDuplicateTail #[tl, tr, br, bl])
      current := tl
      subpathStart := tl
      hasCurrent := true
    | .closePath =>
      if ring.size >= 3 then
        let trimmed := trimDuplicateTail ring
        if trimmed.size >= 3 then
          rings := rings.push trimmed
      ring := #[]
      current := subpathStart
      hasCurrent := true

  if ring.size >= 3 then
    let trimmed := trimDuplicateTail ring
    if trimmed.size >= 3 then
      rings := rings.push trimmed
  ring := #[]
  return rings

private def flattenRings (rings : Array (Array Point)) : Array Point × Array Nat := Id.run do
  let mut points : Array Point := #[]
  let mut holes : Array Nat := #[]
  for i in [:rings.size] do
    let ring := rings[i]!
    if ring.size >= 3 then
      if i > 0 then
        holes := holes.push points.size
      for p in ring do
        points := points.push p
  return (points, holes)

private def pointsToData (points : Array Point) : Array Float := Id.run do
  let mut data : Array Float := Array.mkEmpty (points.size * 2)
  for p in points do
    data := data.push p.x
    data := data.push p.y
  return data

/-- Simple fan triangulation for convex polygons.
    Triangulates from first vertex to all other vertices. -/
def triangulateConvexFan (numVertices : Nat) : Array UInt32 := Id.run do
  if numVertices < 3 then return #[]
  let numTriangles := numVertices - 2
  let mut indices : Array UInt32 := Array.mkEmpty (numTriangles * 3)
  for i in [1:numVertices - 1] do
    indices := indices.push 0
    indices := indices.push i.toUInt32
    indices := indices.push (i + 1).toUInt32
  return indices

/-- Triangulate a polygon using earcut (handles concave shapes). -/
def triangulatePolygon (vertices : Array Point) : Array UInt32 := Id.run do
  if vertices.size < 3 then return #[]
  let mut data : Array Float := Array.mkEmpty (vertices.size * 2)
  for p in vertices do
    data := data.push p.x
    data := data.push p.y
  return Earcut.earcut data #[]

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

  { vertices, indices := rectIndices }

/-- Tessellate a path with a solid color.
    Works for both convex and non-convex simple polygons. -/
def tessellatePath (path : Path) (color : Color) (tolerance : Float := 0.5) : TessellationResult := Id.run do
  let rings := pathToRings path tolerance
  let (points, holes) := flattenRings rings
  if points.size < 3 then
    return { vertices := #[], indices := #[] }

  -- Build vertex array (6 floats per vertex: x, y, r, g, b, a)
  let mut vertices : Array Float := Array.mkEmpty (points.size * 6)
  for p in points do
    vertices := vertices.push p.x
    vertices := vertices.push p.y
    vertices := vertices.push color.r
    vertices := vertices.push color.g
    vertices := vertices.push color.b
    vertices := vertices.push color.a

  let data := pointsToData points
  let indices := Earcut.earcut data holes
  return { vertices, indices }

/-- Tessellate a convex path with a solid color.
    @deprecated Use tessellatePath which handles both convex and non-convex paths. -/
def tessellateConvexPath (path : Path) (color : Color) (tolerance : Float := 0.5) : TessellationResult :=
  tessellatePath path color tolerance

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

  { vertices, indices := rectIndices }

/-- Tessellate a path with pixel coordinates, converting to NDC.
    Works for both convex and non-convex simple polygons. -/
def tessellatePathNDC (path : Path) (color : Color)
    (screenWidth screenHeight : Float) (tolerance : Float := 0.5) : TessellationResult := Id.run do
  let rings := pathToRings path tolerance
  let (points, holes) := flattenRings rings
  if points.size < 3 then
    return { vertices := #[], indices := #[] }

  -- Pre-allocate vertex array (6 floats per vertex: x, y, r, g, b, a)
  let mut vertices : Array Float := Array.mkEmpty (points.size * 6)
  for p in points do
    let ndc := pixelToNDC p.x p.y screenWidth screenHeight
    vertices := vertices.push ndc.x
    vertices := vertices.push ndc.y
    vertices := vertices.push color.r
    vertices := vertices.push color.g
    vertices := vertices.push color.b
    vertices := vertices.push color.a

  let data := pointsToData points
  let indices := Earcut.earcut data holes
  return { vertices, indices }

/-- Tessellate a convex path with pixel coordinates, converting to NDC.
    @deprecated Use tessellatePathNDC which handles both convex and non-convex paths. -/
def tessellateConvexPathNDC (path : Path) (color : Color)
    (screenWidth screenHeight : Float) (tolerance : Float := 0.5) : TessellationResult :=
  tessellatePathNDC path color screenWidth screenHeight tolerance

/-! ## Gradient Sampling -/

/-- Clamp a value to [0, 1] range. -/
private def clamp01 (x : Float) : Float :=
  if x < 0.0 then 0.0 else if x > 1.0 then 1.0 else x

/-- Find the two gradient stops surrounding a given t value and interpolate. -/
def interpolateGradientStops (stops : Array GradientStop) (t : Float) : Color := Id.run do
  if stops.size == 0 then return Color.black
  if stops.size == 1 then return stops[0]!.color

  let t := clamp01 t

  -- Find surrounding stops
  let mut prevStop := stops[0]!
  let mut nextStop := stops[0]!

  for i in [:stops.size] do
    if h : i < stops.size then
      let stop := stops[i]
      if stop.position <= t then
        prevStop := stop
      if stop.position >= t && (i == 0 || stops[i-1]!.position < t) then
        nextStop := stop
        break

  -- Handle edge cases
  if t <= prevStop.position then return prevStop.color
  if t >= nextStop.position then return nextStop.color
  if prevStop.position == nextStop.position then return prevStop.color

  -- Interpolate between stops
  let localT := (t - prevStop.position) / (nextStop.position - prevStop.position)
  Color.lerp prevStop.color nextStop.color localT

/-- Sample a linear gradient at a given point.
    Projects the point onto the gradient line and returns the interpolated color. -/
def sampleLinearGradient (start finish : Point) (stops : Array GradientStop) (p : Point) : Color :=
  -- Vector from start to finish
  let dx := finish.x - start.x
  let dy := finish.y - start.y
  let lenSq := dx * dx + dy * dy

  if lenSq < 0.0001 then
    -- Degenerate gradient (start == finish)
    if stops.size > 0 then stops[0]!.color else Color.black
  else
    -- Project point onto gradient line
    let px := p.x - start.x
    let py := p.y - start.y
    let t := (px * dx + py * dy) / lenSq
    interpolateGradientStops stops t

/-- Sample a radial gradient at a given point.
    Uses distance from center to determine color. -/
def sampleRadialGradient (center : Point) (radius : Float) (stops : Array GradientStop) (p : Point) : Color :=
  if radius < 0.0001 then
    if stops.size > 0 then stops[0]!.color else Color.black
  else
    let dist := Point.distance center p
    let t := dist / radius
    interpolateGradientStops stops t

/-- Sample any gradient type at a given point. -/
def sampleGradient (g : Gradient) (p : Point) : Color :=
  match g with
  | .linear start finish stops => sampleLinearGradient start finish stops p
  | .radial center radius stops => sampleRadialGradient center radius stops p

/-- Sample a fill style at a given point. -/
def sampleFillStyle (style : FillStyle) (p : Point) : Color :=
  match style with
  | .solid c => c
  | .gradient g => sampleGradient g p

/-- Tessellate a path with a fill style (solid or gradient), converting to NDC.
    Handles both convex and non-convex polygons. -/
def tessellateConvexPathFillNDC (path : Path) (style : FillStyle)
    (screenWidth screenHeight : Float) (tolerance : Float := 0.5) : TessellationResult := Id.run do
  let rings := pathToRings path tolerance
  let (points, holes) := flattenRings rings
  if points.size < 3 then
    return { vertices := #[], indices := #[] }

  -- Pre-allocate vertex array (6 floats per vertex: x, y, r, g, b, a)
  let mut vertices : Array Float := Array.mkEmpty (points.size * 6)
  for p in points do
    let color := sampleFillStyle style p
    let ndc := pixelToNDC p.x p.y screenWidth screenHeight
    vertices := vertices.push ndc.x
    vertices := vertices.push ndc.y
    vertices := vertices.push color.r
    vertices := vertices.push color.g
    vertices := vertices.push color.b
    vertices := vertices.push color.a

  let data := pointsToData points
  let indices := Earcut.earcut data holes
  return { vertices, indices }

/-- Compute the centroid of a set of points. -/
private def computeCentroid (points : Array Point) : Point := Id.run do
  if points.size == 0 then return Point.zero
  let mut sumX := 0.0
  let mut sumY := 0.0
  for p in points do
    sumX := sumX + p.x
    sumY := sumY + p.y
  { x := sumX / points.size.toFloat, y := sumY / points.size.toFloat }

/-- Tessellate a convex path with separate original/transformed paths for correct gradient sampling.
    - originalPath: used for gradient color sampling (in original coordinate space)
    - transformedPath: used for vertex positions (after transform applied)
    This is needed because gradients are defined in original space but shapes are transformed.
    For radial gradients, adds a center vertex to ensure proper color interpolation from center to edge. -/
def tessellateConvexPathFillNDCWithOriginal (originalPath transformedPath : Path) (style : FillStyle)
    (screenWidth screenHeight : Float) (tolerance : Float := 0.5) : TessellationResult := Id.run do
  let originalPoints := pathToPolygon originalPath tolerance
  let transformedPoints := pathToPolygon transformedPath tolerance

  -- Both paths should produce same number of points (same topology, different positions)
  let numPoints := min originalPoints.size transformedPoints.size

  if numPoints < 3 then
    return { vertices := #[], indices := #[] }

  -- Check if this is a radial gradient - if so, we need a center vertex for proper interpolation
  let isRadialGradient := match style with
    | .gradient (.radial _ _ _) => true
    | _ => false

  if isRadialGradient then
    -- For radial gradients: add center vertex and create fan triangles from center
    -- This ensures the gradient interpolates properly from center (t=0) to edge (t=1)
    let originalCenter := computeCentroid originalPoints
    let transformedCenter := computeCentroid transformedPoints
    let centerColor := sampleFillStyle style originalCenter
    let centerNDC := pixelToNDC transformedCenter.x transformedCenter.y screenWidth screenHeight

    -- Vertex 0 is center, vertices 1..n are perimeter
    let mut vertices : Array Float := Array.mkEmpty ((numPoints + 1) * 6)

    -- Add center vertex first
    vertices := vertices.push centerNDC.x
    vertices := vertices.push centerNDC.y
    vertices := vertices.push centerColor.r
    vertices := vertices.push centerColor.g
    vertices := vertices.push centerColor.b
    vertices := vertices.push centerColor.a

    -- Add perimeter vertices
    for i in [:numPoints] do
      if h : i < originalPoints.size ∧ i < transformedPoints.size then
        let color := sampleFillStyle style originalPoints[i]
        let ndc := pixelToNDC transformedPoints[i].x transformedPoints[i].y screenWidth screenHeight
        vertices := vertices.push ndc.x
        vertices := vertices.push ndc.y
        vertices := vertices.push color.r
        vertices := vertices.push color.g
        vertices := vertices.push color.b
        vertices := vertices.push color.a

    -- Create fan triangles from center (vertex 0) to perimeter
    let mut indices : Array UInt32 := Array.mkEmpty (numPoints * 3)
    for i in [:numPoints] do
      let curr := (i + 1).toUInt32  -- perimeter vertices start at index 1
      let next := if i + 1 < numPoints then (i + 2).toUInt32 else 1  -- wrap around
      indices := indices.push 0       -- center
      indices := indices.push curr    -- current perimeter vertex
      indices := indices.push next    -- next perimeter vertex

    return { vertices, indices }
  else
    -- For solid colors and linear gradients: use smart triangulation
    -- that handles both convex and non-convex polygons
    let mut vertices : Array Float := Array.mkEmpty (numPoints * 6)
    -- Build a truncated points array that matches the vertices we're creating
    let mut truncatedPoints : Array Point := Array.mkEmpty numPoints
    for i in [:numPoints] do
      if h : i < originalPoints.size ∧ i < transformedPoints.size then
        let color := sampleFillStyle style originalPoints[i]
        let ndc := pixelToNDC transformedPoints[i].x transformedPoints[i].y screenWidth screenHeight
        vertices := vertices.push ndc.x
        vertices := vertices.push ndc.y
        vertices := vertices.push color.r
        vertices := vertices.push color.g
        vertices := vertices.push color.b
        vertices := vertices.push color.a
        truncatedPoints := truncatedPoints.push transformedPoints[i]

    -- Use triangulatePolygon which handles both convex and non-convex shapes
    -- Pass the truncated points array that matches our vertex count
    let indices := triangulatePolygon truncatedPoints
    return { vertices, indices }

/-- Tessellate a path with a fill style using a transform for position calculation.
    Unlike tessellateConvexPathFillNDCWithOriginal, this function flattens the path only once
    and applies the transform to each point, ensuring exact 1-to-1 correspondence between
    original and transformed points. This fixes issues with non-uniform scaling where
    adaptive bezier flattening would produce different numbers of points for each path.
    - originalPath: used for both structure AND gradient color sampling
    - transform: applied to each flattened point for final positions
    For radial gradients, adds a center vertex to ensure proper color interpolation. -/
def tessellatePathWithTransform (originalPath : Path) (transform : Transform) (style : FillStyle)
    (screenWidth screenHeight : Float) (tolerance : Float := 0.5) : TessellationResult := Id.run do
  -- Flatten only the original path once (supports holes)
  let rings := pathToRings originalPath tolerance
  let (originalPoints, holes) := flattenRings rings
  let numPoints := originalPoints.size
  if numPoints < 3 then
    return { vertices := #[], indices := #[] }

  -- Apply transform to get transformed positions - guaranteed 1-to-1 correspondence
  let transformedPoints := originalPoints.map transform.apply

  -- Check if this is a radial gradient - if so, we need a center vertex for proper interpolation
  let isRadialGradient := match style with
    | .gradient (.radial _ _ _) => true
    | _ => false

  if isRadialGradient && holes.isEmpty then
    -- For radial gradients: add center vertex and create fan triangles from center
    let originalCenter := computeCentroid originalPoints
    let transformedCenter := transform.apply originalCenter
    let centerColor := sampleFillStyle style originalCenter
    let centerNDC := pixelToNDC transformedCenter.x transformedCenter.y screenWidth screenHeight

    -- Vertex 0 is center, vertices 1..n are perimeter
    let mut vertices : Array Float := Array.mkEmpty ((numPoints + 1) * 6)

    -- Add center vertex first
    vertices := vertices.push centerNDC.x
    vertices := vertices.push centerNDC.y
    vertices := vertices.push centerColor.r
    vertices := vertices.push centerColor.g
    vertices := vertices.push centerColor.b
    vertices := vertices.push centerColor.a

    -- Add perimeter vertices
    for i in [:numPoints] do
      let color := sampleFillStyle style originalPoints[i]!
      let ndc := pixelToNDC transformedPoints[i]!.x transformedPoints[i]!.y screenWidth screenHeight
      vertices := vertices.push ndc.x
      vertices := vertices.push ndc.y
      vertices := vertices.push color.r
      vertices := vertices.push color.g
      vertices := vertices.push color.b
      vertices := vertices.push color.a

    -- Create fan triangles from center (vertex 0) to perimeter
    let mut indices : Array UInt32 := Array.mkEmpty (numPoints * 3)
    for i in [:numPoints] do
      let curr := (i + 1).toUInt32  -- perimeter vertices start at index 1
      let next := if i + 1 < numPoints then (i + 2).toUInt32 else 1  -- wrap around
      indices := indices.push 0       -- center
      indices := indices.push curr    -- current perimeter vertex
      indices := indices.push next    -- next perimeter vertex

    return { vertices, indices }
  else
    -- For solid colors and linear gradients: use smart triangulation
    let mut vertices : Array Float := Array.mkEmpty (numPoints * 6)
    for i in [:numPoints] do
      let color := sampleFillStyle style originalPoints[i]!
      let ndc := pixelToNDC transformedPoints[i]!.x transformedPoints[i]!.y screenWidth screenHeight
      vertices := vertices.push ndc.x
      vertices := vertices.push ndc.y
      vertices := vertices.push color.r
      vertices := vertices.push color.g
      vertices := vertices.push color.b
      vertices := vertices.push color.a

    let data := pointsToData transformedPoints
    let indices := Earcut.earcut data holes
    return { vertices, indices }

/-- Tessellate a rectangle with a fill style (solid or gradient), converting to NDC. -/
def tessellateRectFillNDC (r : Rect) (style : FillStyle) (screenWidth screenHeight : Float) : TessellationResult :=
  let tl := r.topLeft
  let tr := r.topRight
  let bl := r.bottomLeft
  let br := r.bottomRight

  -- Sample colors at each corner
  let tlColor := sampleFillStyle style tl
  let trColor := sampleFillStyle style tr
  let blColor := sampleFillStyle style bl
  let brColor := sampleFillStyle style br

  let toNDC := fun (p : Point) => pixelToNDC p.x p.y screenWidth screenHeight
  let tlNDC := toNDC tl
  let trNDC := toNDC tr
  let blNDC := toNDC bl
  let brNDC := toNDC br

  let vertices := #[
    tlNDC.x, tlNDC.y, tlColor.r, tlColor.g, tlColor.b, tlColor.a,
    trNDC.x, trNDC.y, trColor.r, trColor.g, trColor.b, trColor.a,
    brNDC.x, brNDC.y, brColor.r, brColor.g, brColor.b, brColor.a,
    blNDC.x, blNDC.y, blColor.r, blColor.g, blColor.b, blColor.a
  ]

  { vertices, indices := rectIndices }

/-- Fast path: Tessellate a rectangle with pre-transformed corners.
    Skips Path creation entirely - just transforms 4 points and outputs triangles.
    This is ~10x faster than going through Path for simple rectangles.
    Note: Gradients are sampled at ORIGINAL positions since gradient coordinates
    are defined in the original coordinate space. -/
def tessellateTransformedRectNDC
    (rect : Rect) (transform : Transform) (style : FillStyle)
    (screenWidth screenHeight : Float) : TessellationResult :=
  -- Sample colors at ORIGINAL positions (before transform)
  -- This is correct because gradient coordinates are in original space
  let tlColor := sampleFillStyle style rect.topLeft
  let trColor := sampleFillStyle style rect.topRight
  let blColor := sampleFillStyle style rect.bottomLeft
  let brColor := sampleFillStyle style rect.bottomRight

  -- Transform the 4 corners for rendering positions
  let tl := transform.apply rect.topLeft
  let tr := transform.apply rect.topRight
  let bl := transform.apply rect.bottomLeft
  let br := transform.apply rect.bottomRight

  -- Convert to NDC
  let toNDC := fun (p : Point) => pixelToNDC p.x p.y screenWidth screenHeight
  let tlNDC := toNDC tl
  let trNDC := toNDC tr
  let blNDC := toNDC bl
  let brNDC := toNDC br

  let vertices := #[
    tlNDC.x, tlNDC.y, tlColor.r, tlColor.g, tlColor.b, tlColor.a,
    trNDC.x, trNDC.y, trColor.r, trColor.g, trColor.b, trColor.a,
    brNDC.x, brNDC.y, brColor.r, brColor.g, brColor.b, brColor.a,
    blNDC.x, blNDC.y, blColor.r, blColor.g, blColor.b, blColor.a
  ]

  { vertices, indices := rectIndices }

/-! ## Stroke Segment Builder (GPU extrusion) -/

/-- Normalize a 2D vector. Returns zero vector if input is zero length. -/
private def normalize (dx dy : Float) : Point :=
  let len := Float.sqrt (dx * dx + dy * dy)
  if len < 0.0001 then ⟨0, 0⟩
  else ⟨dx / len, dy / len⟩

/-- Compute the normal (perpendicular) at a point given the direction. -/
private def computeNormal (dir : Point) : Point :=
  ⟨-dir.y, dir.x⟩

/-- Convert a quadratic Bezier curve to cubic control points. -/
private def quadraticToCubic (p0 cp p1 : Point) : Point × Point :=
  let c1 := Point.lerp p0 cp (2.0 / 3.0)
  let c2 := Point.lerp p1 cp (2.0 / 3.0)
  (c1, c2)

/-- Evaluate a cubic Bezier at parameter t. -/
private def evalCubic (p0 c1 c2 p1 : Point) (t : Float) : Point :=
  let u := 1.0 - t
  let tt := t * t
  let uu := u * u
  let uuu := uu * u
  let ttt := tt * t
  p0 * uuu +
    c1 * (3.0 * uu * t) +
    c2 * (3.0 * u * tt) +
    p1 * ttt

/-- Evaluate the cubic Bezier tangent at parameter t. -/
private def evalCubicTangent (p0 c1 c2 p1 : Point) (t : Float) : Point :=
  let u := 1.0 - t
  let tt := t * t
  let uu := u * u
  let term1 := (c1 - p0) * (3.0 * uu)
  let term2 := (c2 - c1) * (6.0 * u * t)
  let term3 := (p1 - c2) * (3.0 * tt)
  term1 + term2 + term3

/-- Approximate cubic Bezier length by sampling after applying a transform. -/
private def approximateCubicLength (p0 c1 c2 p1 : Point) (transform : Transform)
    (steps : Nat := 12) : Float := Id.run do
  let steps := if steps < 2 then 2 else steps
  let mut length := 0.0
  let mut prev := transform.apply p0
  for i in [1:steps + 1] do
    let t := i.toFloat / steps.toFloat
    let pt := transform.apply (evalCubic p0 c1 c2 p1 t)
    length := length + Point.distance prev pt
    prev := pt
  return length

/-- Internal raw stroke segment before adjacency is computed. -/
private structure RawStrokeSegment where
  p0 : Point
  p1 : Point
  c1 : Point
  c2 : Point
  kind : StrokeSegmentKind
deriving Inhabited

private def mkLineSegment (p0 p1 : Point) : RawStrokeSegment :=
  { p0, p1, c1 := p0, c2 := p1, kind := .line }

private def mkCubicSegment (p0 c1 c2 p1 : Point) : RawStrokeSegment :=
  { p0, p1, c1, c2, kind := .cubic }

private def segmentStartDir (seg : RawStrokeSegment) : Point :=
  match seg.kind with
  | .line =>
      let dx := seg.p1.x - seg.p0.x
      let dy := seg.p1.y - seg.p0.y
      normalize dx dy
  | .cubic =>
      let d := evalCubicTangent seg.p0 seg.c1 seg.c2 seg.p1 0.0
      normalize d.x d.y

private def segmentEndDir (seg : RawStrokeSegment) : Point :=
  match seg.kind with
  | .line =>
      let dx := seg.p1.x - seg.p0.x
      let dy := seg.p1.y - seg.p0.y
      normalize dx dy
  | .cubic =>
      let d := evalCubicTangent seg.p0 seg.c1 seg.c2 seg.p1 1.0
      normalize d.x d.y

private def segmentLength (transform : Transform) (seg : RawStrokeSegment) : Float :=
  match seg.kind with
  | .line => Point.distance (transform.apply seg.p0) (transform.apply seg.p1)
  | .cubic => approximateCubicLength seg.p0 seg.c1 seg.c2 seg.p1 transform

/-- Remove degenerate segments (zero-length). -/
private def filterDegenerateSegments (segments : Array RawStrokeSegment)
    (transform : Transform) : Array RawStrokeSegment := Id.run do
  let mut result : Array RawStrokeSegment := #[]
  for seg in segments do
    if segmentLength transform seg > 0.0001 then
      result := result.push seg
  return result

/-- Finalize raw segments into GPU-ready stroke segments (with adjacency + distances). -/
private def finalizeStrokeSegments (segments : Array RawStrokeSegment) (closed : Bool)
    (transform : Transform) : Array StrokeSegment := Id.run do
  let segments := filterDegenerateSegments segments transform
  if segments.size == 0 then
    return #[]

  let mut result : Array StrokeSegment := Array.mkEmpty segments.size
  let mut dist : Float := 0.0

  for i in [:segments.size] do
    let seg := segments[i]!
    let len := segmentLength transform seg
    let hasPrev := closed || i > 0
    let hasNext := closed || i + 1 < segments.size
    let prevIdx := if hasPrev then (if i == 0 then segments.size - 1 else i - 1) else 0
    let nextIdx := if hasNext then (if i + 1 == segments.size then 0 else i + 1) else 0
    let prevDir := if hasPrev then segmentEndDir (segments[prevIdx]!) else segmentStartDir seg
    let nextDir := if hasNext then segmentStartDir (segments[nextIdx]!) else segmentEndDir seg

    result := result.push {
      p0 := seg.p0
      p1 := seg.p1
      c1 := seg.c1
      c2 := seg.c2
      prevDir := prevDir
      nextDir := nextDir
      startDist := dist
      length := len
      hasPrev := hasPrev
      hasNext := hasNext
      kind := seg.kind
    }

    dist := dist + len

  return result

private def segmentKindFloat (kind : StrokeSegmentKind) : Float :=
  match kind with
  | .line => 0.0
  | .cubic => 1.0

private def pushStrokeSegment (arr : Array Float) (seg : StrokeSegment) : Array Float :=
  arr
    |>.push seg.p0.x |>.push seg.p0.y
    |>.push seg.p1.x |>.push seg.p1.y
    |>.push seg.c1.x |>.push seg.c1.y
    |>.push seg.c2.x |>.push seg.c2.y
    |>.push seg.prevDir.x |>.push seg.prevDir.y
    |>.push seg.nextDir.x |>.push seg.nextDir.y
    |>.push seg.startDist |>.push seg.length
    |>.push (if seg.hasPrev then 1.0 else 0.0)
    |>.push (if seg.hasNext then 1.0 else 0.0)
    |>.push (segmentKindFloat seg.kind)
    |>.push 0.0

/-- Append finalized stroke segments into line/curve buffers. -/
private def appendStrokeSegments (rawSegments : Array RawStrokeSegment) (closed : Bool)
    (transform : Transform)
    (lineSegments : Array Float) (curveSegments : Array Float)
    (lineCount curveCount : Nat) : Array Float × Array Float × Nat × Nat := Id.run do
  if rawSegments.size == 0 then
    return (lineSegments, curveSegments, lineCount, curveCount)
  let finalized := finalizeStrokeSegments rawSegments closed transform
  let mut lineSegs := lineSegments
  let mut curveSegs := curveSegments
  let mut lineCnt := lineCount
  let mut curveCnt := curveCount
  for seg in finalized do
    match seg.kind with
    | .line =>
        lineSegs := pushStrokeSegment lineSegs seg
        lineCnt := lineCnt + 1
    | .cubic =>
        curveSegs := pushStrokeSegment curveSegs seg
        curveCnt := curveCnt + 1
  return (lineSegs, curveSegs, lineCnt, curveCnt)

/-- Recommended subdivision count for cubic segments in the GPU shader. -/
def strokeCurveSubdivisions : Nat := 16

/-- Tessellate a path into GPU stroke segments (no CPU extrusion). -/
def tessellateStrokeSegments (path : Path) (_style : StrokeStyle)
    (transform : Transform := Transform.identity) : StrokePathSegments := Id.run do
  let mut lineSegments : Array Float := #[]
  let mut curveSegments : Array Float := #[]
  let mut lineCount : Nat := 0
  let mut curveCount : Nat := 0

  let mut rawSegments : Array RawStrokeSegment := #[]
  let mut hasCurrent := false
  let mut current := Point.zero
  let mut subpathStart := Point.zero
  let mut closed := false

  for cmd in path.commands do
    match cmd with
    | .moveTo p =>
      if rawSegments.size > 0 then
        let (ls, cs, lc, cc) := appendStrokeSegments rawSegments closed transform
          lineSegments curveSegments lineCount curveCount
        lineSegments := ls
        curveSegments := cs
        lineCount := lc
        curveCount := cc
        rawSegments := #[]
        closed := false
      hasCurrent := true
      current := p
      subpathStart := p
    | .lineTo p =>
      if !hasCurrent then
        hasCurrent := true
        current := p
        subpathStart := p
      else
        rawSegments := rawSegments.push (mkLineSegment current p)
        current := p
    | .quadraticCurveTo cp p =>
      if !hasCurrent then
        hasCurrent := true
        current := p
        subpathStart := p
      else
        let (c1, c2) := quadraticToCubic current cp p
        rawSegments := rawSegments.push (mkCubicSegment current c1 c2 p)
        current := p
    | .bezierCurveTo cp1 cp2 p =>
      if !hasCurrent then
        hasCurrent := true
        current := p
        subpathStart := p
      else
        rawSegments := rawSegments.push (mkCubicSegment current cp1 cp2 p)
        current := p
    | .arc center radius startAngle endAngle counterclockwise =>
      if !hasCurrent then
        -- Treat arc as moveTo end point when no current
        let endPt := Point.mk'
          (center.x + radius * Float.cos endAngle)
          (center.y + radius * Float.sin endAngle)
        hasCurrent := true
        current := endPt
        subpathStart := endPt
      else
        let beziers := Path.arcToBeziers center radius startAngle endAngle counterclockwise
        for (cp1, cp2, endPt) in beziers do
          rawSegments := rawSegments.push (mkCubicSegment current cp1 cp2 endPt)
          current := endPt
    | .arcTo p1 p2 radius =>
      if !hasCurrent then
        hasCurrent := true
        current := p1
        subpathStart := p1
      else
        match computeArcTo current p1 p2 radius with
        | some (t1, beziers, t2) =>
          if Point.distance current t1 > 0.0001 then
            rawSegments := rawSegments.push (mkLineSegment current t1)
          let mut arcCurrent := t1
          for (cp1, cp2, endPt) in beziers do
            rawSegments := rawSegments.push (mkCubicSegment arcCurrent cp1 cp2 endPt)
            arcCurrent := endPt
          current := t2
        | none =>
          rawSegments := rawSegments.push (mkLineSegment current p1)
          current := p1
    | .rect r =>
      if rawSegments.size > 0 then
        let (ls, cs, lc, cc) := appendStrokeSegments rawSegments closed transform
          lineSegments curveSegments lineCount curveCount
        lineSegments := ls
        curveSegments := cs
        lineCount := lc
        curveCount := cc
        rawSegments := #[]
        closed := false
      let p0 := r.topLeft
      let p1 := r.topRight
      let p2 := r.bottomRight
      let p3 := r.bottomLeft
      rawSegments := #[
        mkLineSegment p0 p1,
        mkLineSegment p1 p2,
        mkLineSegment p2 p3,
        mkLineSegment p3 p0
      ]
      closed := true
      current := p0
      subpathStart := p0
      if rawSegments.size > 0 then
        let (ls, cs, lc, cc) := appendStrokeSegments rawSegments closed transform
          lineSegments curveSegments lineCount curveCount
        lineSegments := ls
        curveSegments := cs
        lineCount := lc
        curveCount := cc
        rawSegments := #[]
        closed := false
      hasCurrent := true
    | .closePath =>
      if hasCurrent then
        if Point.distance current subpathStart > 0.0001 then
          rawSegments := rawSegments.push (mkLineSegment current subpathStart)
        closed := true
        current := subpathStart
        if rawSegments.size > 0 then
          let (ls, cs, lc, cc) := appendStrokeSegments rawSegments closed transform
            lineSegments curveSegments lineCount curveCount
          lineSegments := ls
          curveSegments := cs
          lineCount := lc
          curveCount := cc
          rawSegments := #[]
          closed := false
        hasCurrent := true

  if rawSegments.size > 0 then
    let (ls, cs, lc, cc) := appendStrokeSegments rawSegments closed transform
      lineSegments curveSegments lineCount curveCount
    lineSegments := ls
    curveSegments := cs
    lineCount := lc
    curveCount := cc

  return { lineSegments, curveSegments, lineCount, curveCount }

end Tessellation

/-! ## Batch Accumulation -/

/-- Accumulates tessellated geometry for a single draw call.
    Use this to batch many shapes into one draw call for better performance. -/
structure Batch where
  /-- Accumulated vertex data (vertexSize2D floats per vertex: x, y, r, g, b, a). -/
  vertices : Array Float
  /-- Accumulated triangle indices. -/
  indices : Array UInt32
  /-- Current vertex count (vertices.size / vertexSize2D), used for index remapping. -/
  vertexCount : Nat
deriving Inhabited

namespace Batch

/-- Create an empty batch. -/
def empty : Batch := { vertices := #[], indices := #[], vertexCount := 0 }

/-- Create a batch with pre-allocated capacity for estimated shape count.
    Assumes ~30 floats and ~10 indices per shape on average. -/
def withCapacity (shapeCount : Nat) : Batch :=
  { vertices := Array.mkEmpty (shapeCount * 30)
    indices := Array.mkEmpty (shapeCount * 10)
    vertexCount := 0 }

/-- Add a tessellation result to the batch.
    Indices are automatically remapped to account for existing vertices.
    Uses in-place push loop to avoid intermediate array allocation from .map -/
def add (batch : Batch) (result : TessellationResult) : Batch :=
  if result.vertices.size == 0 then batch
  else Id.run do
    let offset := batch.vertexCount.toUInt32
    -- Push indices one by one with offset applied inline (no intermediate array)
    let mut indices := batch.indices
    for idx in result.indices do
      indices := indices.push (idx + offset)
    { vertices := batch.vertices ++ result.vertices
      indices := indices
      vertexCount := batch.vertexCount + result.vertices.size / Tessellation.vertexSize2D }

/-- Combine two batches.
    Uses in-place push loop to avoid intermediate array allocation from .map -/
def append (b1 b2 : Batch) : Batch :=
  if b2.vertices.size == 0 then b1
  else if b1.vertices.size == 0 then b2
  else Id.run do
    let offset := b1.vertexCount.toUInt32
    -- Push indices one by one with offset applied inline (no intermediate array)
    let mut indices := b1.indices
    for idx in b2.indices do
      indices := indices.push (idx + offset)
    { vertices := b1.vertices ++ b2.vertices
      indices := indices
      vertexCount := b1.vertexCount + b2.vertexCount }

/-- FAST PATH: Add a transformed rectangle directly to the batch.
    No intermediate TessellationResult allocation - writes directly to batch arrays.
    This is the hot path for batched rectangle rendering.
    Note: Gradients are sampled at ORIGINAL positions (before transform) since gradient
    coordinates are defined in the original coordinate space. -/
def addTransformedRect (batch : Batch) (rect : Rect) (transform : Transform)
    (style : FillStyle) (screenWidth screenHeight : Float) : Batch :=
  -- Sample colors at ORIGINAL positions (before transform)
  -- This is correct because gradient coordinates are in original space
  let tlColor := Tessellation.sampleFillStyle style rect.topLeft
  let trColor := Tessellation.sampleFillStyle style rect.topRight
  let blColor := Tessellation.sampleFillStyle style rect.bottomLeft
  let brColor := Tessellation.sampleFillStyle style rect.bottomRight

  -- Transform corners for rendering positions
  let tl := transform.apply rect.topLeft
  let tr := transform.apply rect.topRight
  let bl := transform.apply rect.bottomLeft
  let br := transform.apply rect.bottomRight

  -- Convert to NDC
  let tlNDC := Tessellation.pixelToNDC tl.x tl.y screenWidth screenHeight
  let trNDC := Tessellation.pixelToNDC tr.x tr.y screenWidth screenHeight
  let blNDC := Tessellation.pixelToNDC bl.x bl.y screenWidth screenHeight
  let brNDC := Tessellation.pixelToNDC br.x br.y screenWidth screenHeight

  -- Current vertex index for this rect
  let baseIdx := batch.vertexCount.toUInt32

  -- Push vertices directly (no intermediate array)
  let vertices := batch.vertices
    |>.push tlNDC.x |>.push tlNDC.y |>.push tlColor.r |>.push tlColor.g |>.push tlColor.b |>.push tlColor.a
    |>.push trNDC.x |>.push trNDC.y |>.push trColor.r |>.push trColor.g |>.push trColor.b |>.push trColor.a
    |>.push brNDC.x |>.push brNDC.y |>.push brColor.r |>.push brColor.g |>.push brColor.b |>.push brColor.a
    |>.push blNDC.x |>.push blNDC.y |>.push blColor.r |>.push blColor.g |>.push blColor.b |>.push blColor.a

  -- Push indices directly (two triangles: 0,1,2 and 0,2,3 offset by baseIdx)
  let indices := batch.indices
    |>.push baseIdx |>.push (baseIdx + 1) |>.push (baseIdx + 2)
    |>.push baseIdx |>.push (baseIdx + 2) |>.push (baseIdx + 3)

  { vertices, indices, vertexCount := batch.vertexCount + 4 }

/-- FASTEST PATH: Add a rectangle with pre-computed position, rotation, size, and color.
    Computes transform inline - no Canvas state, no Transform struct allocation.
    x, y: center position; angle: rotation in radians; halfSize: half the side length -/
def addRectDirect (batch : Batch) (x y angle halfSize : Float) (color : Color)
    (screenWidth screenHeight : Float) : Batch :=
  -- Inline rotation matrix computation (avoid Transform allocation)
  let cosA := Float.cos angle
  let sinA := Float.sin angle

  -- Compute 4 corners relative to center, then rotate and translate
  -- Corner offsets: (-h,-h), (h,-h), (h,h), (-h,h) where h = halfSize
  let h := halfSize

  -- Top-left corner (relative: -h, -h)
  let tlX := x + (-h) * cosA - (-h) * sinA
  let tlY := y + (-h) * sinA + (-h) * cosA

  -- Top-right corner (relative: h, -h)
  let trX := x + h * cosA - (-h) * sinA
  let trY := y + h * sinA + (-h) * cosA

  -- Bottom-right corner (relative: h, h)
  let brX := x + h * cosA - h * sinA
  let brY := y + h * sinA + h * cosA

  -- Bottom-left corner (relative: -h, h)
  let blX := x + (-h) * cosA - h * sinA
  let blY := y + (-h) * sinA + h * cosA

  -- Convert to NDC inline
  let toNdcX := fun px => (px / screenWidth) * 2.0 - 1.0
  let toNdcY := fun py => 1.0 - (py / screenHeight) * 2.0

  let tlNdcX := toNdcX tlX; let tlNdcY := toNdcY tlY
  let trNdcX := toNdcX trX; let trNdcY := toNdcY trY
  let brNdcX := toNdcX brX; let brNdcY := toNdcY brY
  let blNdcX := toNdcX blX; let blNdcY := toNdcY blY

  let baseIdx := batch.vertexCount.toUInt32

  let vertices := batch.vertices
    |>.push tlNdcX |>.push tlNdcY |>.push color.r |>.push color.g |>.push color.b |>.push color.a
    |>.push trNdcX |>.push trNdcY |>.push color.r |>.push color.g |>.push color.b |>.push color.a
    |>.push brNdcX |>.push brNdcY |>.push color.r |>.push color.g |>.push color.b |>.push color.a
    |>.push blNdcX |>.push blNdcY |>.push color.r |>.push color.g |>.push color.b |>.push color.a

  let indices := batch.indices
    |>.push baseIdx |>.push (baseIdx + 1) |>.push (baseIdx + 2)
    |>.push baseIdx |>.push (baseIdx + 2) |>.push (baseIdx + 3)

  { vertices, indices, vertexCount := batch.vertexCount + 4 }

/-- Check if the batch is empty. -/
def isEmpty (batch : Batch) : Bool := batch.vertices.size == 0

/-- Get the number of indices (for draw call). -/
def indexCount (batch : Batch) : Nat := batch.indices.size

/-- Get the number of vertices. -/
def vertexCount' (batch : Batch) : Nat := batch.vertexCount

end Batch

end Afferent

/-
  Afferent Tessellation
  Convert paths to triangles for GPU rendering.
-/
import Afferent.Core.Types
import Afferent.Core.Path
import Afferent.Core.Paint

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

/-- Convert a path to an array of polygon vertices (flatten all curves).
    Also returns whether the path is closed (ends with closePath or first/last points match). -/
def pathToPolygonWithClosed (path : Path) (tolerance : Float := 0.5) : Array Point × Bool := Id.run do
  let mut points : Array Point := #[]
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
    | .arcTo p1 p2 _radius =>
      -- arcTo draws a line to p1, then an arc tangent to both lines
      -- For now, approximate with line to p2 (full implementation is complex)
      points := points.push p1
      points := points.push p2
      current := p2

  return (points, isClosed)

/-- Convert a path to an array of polygon vertices (flatten all curves). -/
def pathToPolygon (path : Path) (tolerance : Float := 0.5) : Array Point :=
  (pathToPolygonWithClosed path tolerance).1

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

/-- Tessellate a convex path with a fill style (solid or gradient), converting to NDC. -/
def tessellateConvexPathFillNDC (path : Path) (style : FillStyle)
    (screenWidth screenHeight : Float) (tolerance : Float := 0.5) : TessellationResult := Id.run do
  let points := pathToPolygon path tolerance

  if points.size < 3 then
    return { vertices := #[], indices := #[] }

  let mut vertices : Array Float := #[]
  for p in points do
    let color := sampleFillStyle style p
    let ndc := pixelToNDC p.x p.y screenWidth screenHeight
    vertices := vertices.push ndc.x
    vertices := vertices.push ndc.y
    vertices := vertices.push color.r
    vertices := vertices.push color.g
    vertices := vertices.push color.b
    vertices := vertices.push color.a

  let indices := triangulateConvexFan points.size
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

  let indices : Array UInt32 := #[0, 1, 2, 0, 2, 3]
  { vertices, indices }

/-! ## Stroke Tessellation -/

/-- Normalize a 2D vector. Returns zero vector if input is zero length. -/
private def normalize (dx dy : Float) : Point :=
  let len := Float.sqrt (dx * dx + dy * dy)
  if len < 0.0001 then ⟨0, 0⟩
  else ⟨dx / len, dy / len⟩

/-- Get perpendicular vector (rotated 90 degrees counterclockwise). -/
private def perpendicular (dx dy : Float) : Point :=
  ⟨-dy, dx⟩

/-- Compute the normal (perpendicular) at a point given the direction. -/
private def computeNormal (dir : Point) : Point :=
  ⟨-dir.y, dir.x⟩

/-- Expand a polyline into stroke geometry.
    Returns left and right edge points for the stroke. -/
def expandPolylineToStroke (points : Array Point) (halfWidth : Float)
    (lineCap : LineCap) (lineJoin : LineJoin) (miterLimit : Float := 10.0)
    : Array Point × Array Point := Id.run do
  if points.size < 2 then
    return (#[], #[])

  let mut leftPoints : Array Point := #[]
  let mut rightPoints : Array Point := #[]

  -- Process each segment
  for i in [:points.size] do
    if h : i < points.size then
      let p := points[i]

      if i == 0 then
        -- First point: use direction to next point
        if h2 : i + 1 < points.size then
          let next := points[i + 1]
          let dx := next.x - p.x
          let dy := next.y - p.y
          let dir := normalize dx dy
          let normal := computeNormal dir

          -- Apply line cap at start
          let (capLeft, capRight) := match lineCap with
            | .butt =>
              (⟨p.x + normal.x * halfWidth, p.y + normal.y * halfWidth⟩,
               ⟨p.x - normal.x * halfWidth, p.y - normal.y * halfWidth⟩)
            | .square =>
              -- Extend backwards by halfWidth
              let backX := p.x - dir.x * halfWidth
              let backY := p.y - dir.y * halfWidth
              (⟨backX + normal.x * halfWidth, backY + normal.y * halfWidth⟩,
               ⟨backX - normal.x * halfWidth, backY - normal.y * halfWidth⟩)
            | .round =>
              -- For round caps, we'd add arc points; simplified to butt for now
              (⟨p.x + normal.x * halfWidth, p.y + normal.y * halfWidth⟩,
               ⟨p.x - normal.x * halfWidth, p.y - normal.y * halfWidth⟩)

          leftPoints := leftPoints.push capLeft
          rightPoints := rightPoints.push capRight

      else if i == points.size - 1 then
        -- Last point: use direction from previous point
        if h2 : i > 0 then
          let prev := points[i - 1]
          let dx := p.x - prev.x
          let dy := p.y - prev.y
          let dir := normalize dx dy
          let normal := computeNormal dir

          -- Apply line cap at end
          let (capLeft, capRight) := match lineCap with
            | .butt =>
              (⟨p.x + normal.x * halfWidth, p.y + normal.y * halfWidth⟩,
               ⟨p.x - normal.x * halfWidth, p.y - normal.y * halfWidth⟩)
            | .square =>
              -- Extend forwards by halfWidth
              let fwdX := p.x + dir.x * halfWidth
              let fwdY := p.y + dir.y * halfWidth
              (⟨fwdX + normal.x * halfWidth, fwdY + normal.y * halfWidth⟩,
               ⟨fwdX - normal.x * halfWidth, fwdY - normal.y * halfWidth⟩)
            | .round =>
              -- Simplified to butt
              (⟨p.x + normal.x * halfWidth, p.y + normal.y * halfWidth⟩,
               ⟨p.x - normal.x * halfWidth, p.y - normal.y * halfWidth⟩)

          leftPoints := leftPoints.push capLeft
          rightPoints := rightPoints.push capRight

      else
        -- Middle point: compute join between two segments
        if h2 : i > 0 ∧ i + 1 < points.size then
          let prev := points[i - 1]
          let next := points[i + 1]

          -- Direction vectors
          let dx1 := p.x - prev.x
          let dy1 := p.y - prev.y
          let dx2 := next.x - p.x
          let dy2 := next.y - p.y

          let dir1 := normalize dx1 dy1
          let dir2 := normalize dx2 dy2

          let normal1 := computeNormal dir1
          let normal2 := computeNormal dir2

          -- Average normal for the join
          let avgNx := (normal1.x + normal2.x) / 2.0
          let avgNy := (normal1.y + normal2.y) / 2.0
          let avgNormal := normalize avgNx avgNy

          -- Compute miter length (how much to extend at sharp corners)
          let dot := dir1.x * dir2.x + dir1.y * dir2.y
          let miterScale := if dot > -0.999 then 1.0 / Float.sqrt ((1.0 + dot) / 2.0) else miterLimit

          -- Apply line join
          match lineJoin with
          | .miter =>
            let scale := min miterScale miterLimit
            leftPoints := leftPoints.push ⟨p.x + avgNormal.x * halfWidth * scale,
                                           p.y + avgNormal.y * halfWidth * scale⟩
            rightPoints := rightPoints.push ⟨p.x - avgNormal.x * halfWidth * scale,
                                             p.y - avgNormal.y * halfWidth * scale⟩
          | .bevel =>
            -- Add two points for bevel (simplified)
            leftPoints := leftPoints.push ⟨p.x + normal1.x * halfWidth, p.y + normal1.y * halfWidth⟩
            leftPoints := leftPoints.push ⟨p.x + normal2.x * halfWidth, p.y + normal2.y * halfWidth⟩
            rightPoints := rightPoints.push ⟨p.x - normal1.x * halfWidth, p.y - normal1.y * halfWidth⟩
            rightPoints := rightPoints.push ⟨p.x - normal2.x * halfWidth, p.y - normal2.y * halfWidth⟩
          | .round =>
            -- Simplified to miter for now
            let scale := min miterScale miterLimit
            leftPoints := leftPoints.push ⟨p.x + avgNormal.x * halfWidth * scale,
                                           p.y + avgNormal.y * halfWidth * scale⟩
            rightPoints := rightPoints.push ⟨p.x - avgNormal.x * halfWidth * scale,
                                             p.y - avgNormal.y * halfWidth * scale⟩

  return (leftPoints, rightPoints)

/-- Convert stroke edges to triangles.
    Takes left and right edge point arrays and creates a triangle strip. -/
def strokeEdgesToTriangles (leftPoints rightPoints : Array Point) (color : Color)
    : TessellationResult := Id.run do
  if leftPoints.size < 2 || rightPoints.size < 2 then
    return { vertices := #[], indices := #[] }

  let mut vertices : Array Float := #[]
  let mut indices : Array UInt32 := #[]

  -- Build vertices: interleave left and right points
  let numPairs := min leftPoints.size rightPoints.size
  for i in [:numPairs] do
    if h : i < leftPoints.size ∧ i < rightPoints.size then
      let lp := leftPoints[i]
      let rp := rightPoints[i]
      -- Left point
      vertices := vertices.push lp.x
      vertices := vertices.push lp.y
      vertices := vertices.push color.r
      vertices := vertices.push color.g
      vertices := vertices.push color.b
      vertices := vertices.push color.a
      -- Right point
      vertices := vertices.push rp.x
      vertices := vertices.push rp.y
      vertices := vertices.push color.r
      vertices := vertices.push color.g
      vertices := vertices.push color.b
      vertices := vertices.push color.a

  -- Build triangle strip indices
  -- Vertices are: L0, R0, L1, R1, L2, R2, ...
  -- Triangles: (L0, R0, L1), (R0, L1, R1), (L1, R1, L2), (R1, L2, R2), ...
  for i in [:(numPairs - 1)] do
    let baseIdx := (i * 2).toUInt32
    -- First triangle: Li, Ri, Li+1
    indices := indices.push baseIdx        -- Li
    indices := indices.push (baseIdx + 1)  -- Ri
    indices := indices.push (baseIdx + 2)  -- Li+1
    -- Second triangle: Ri, Li+1, Ri+1
    indices := indices.push (baseIdx + 1)  -- Ri
    indices := indices.push (baseIdx + 3)  -- Ri+1
    indices := indices.push (baseIdx + 2)  -- Li+1

  return { vertices, indices }

/-- Tessellate a path as a stroke (outline). -/
def tessellateStroke (path : Path) (style : StrokeStyle) (tolerance : Float := 0.5)
    : TessellationResult := Id.run do
  let (points, isClosed) := pathToPolygonWithClosed path tolerance

  if points.size < 2 then
    return { vertices := #[], indices := #[] }

  -- For closed paths, add the first point at the end to close the loop
  let points := if isClosed && points.size > 0 then
    points.push points[0]!
  else
    points

  let halfWidth := style.lineWidth / 2.0
  let (leftPoints, rightPoints) := expandPolylineToStroke points halfWidth
    style.lineCap style.lineJoin style.miterLimit

  return strokeEdgesToTriangles leftPoints rightPoints style.color

/-- Tessellate a path as a stroke with NDC conversion. -/
def tessellateStrokeNDC (path : Path) (style : StrokeStyle)
    (screenWidth screenHeight : Float) (tolerance : Float := 0.5)
    : TessellationResult := Id.run do
  let (points, isClosed) := pathToPolygonWithClosed path tolerance

  if points.size < 2 then
    return { vertices := #[], indices := #[] }

  -- For closed paths, add the first point at the end to close the loop
  let points := if isClosed && points.size > 0 then
    points.push points[0]!
  else
    points

  let halfWidth := style.lineWidth / 2.0
  let (leftPoints, rightPoints) := expandPolylineToStroke points halfWidth
    style.lineCap style.lineJoin style.miterLimit

  -- Convert to NDC
  let toNDC := fun (p : Point) => pixelToNDC p.x p.y screenWidth screenHeight
  let leftNDC := leftPoints.map toNDC
  let rightNDC := rightPoints.map toNDC

  return strokeEdgesToTriangles leftNDC rightNDC style.color

/-- Create a simple line segment as a stroked path. -/
def tessellateLineNDC (p1 p2 : Point) (style : StrokeStyle)
    (screenWidth screenHeight : Float) : TessellationResult :=
  let path := Path.empty |>.moveTo p1 |>.lineTo p2
  tessellateStrokeNDC path style screenWidth screenHeight

/-- Tessellate a stroked rectangle (just the outline). -/
def tessellateStrokeRectNDC (r : Rect) (style : StrokeStyle)
    (screenWidth screenHeight : Float) : TessellationResult :=
  let path := Path.rectangle r
  tessellateStrokeNDC path style screenWidth screenHeight

end Tessellation

end Afferent

/-
  Afferent Tessellation Fill
-/
import Afferent.Core.Types
import Afferent.Core.Path
import Afferent.Core.Paint
import Afferent.Core.Transform
import Afferent.Render.Earcut
import Afferent.Render.Tessellation.Types
import Afferent.Render.Tessellation.Path
import Afferent.Render.Tessellation.Triangulate
import Afferent.Render.Tessellation.Gradient
import Afferent.Render.Tessellation.Cache

namespace Afferent

namespace Tessellation

/-- Cached rectangle indices for two triangles (0,1,2) and (0,2,3).
    Reused across all rectangle tessellation to avoid repeated allocation. -/
private def rectIndices : Array UInt32 := #[0, 1, 2, 0, 2, 3]

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

/-! ## Linear Gradient Slicing (Convex Polygons) -/

private def isConvexPolygon (points : Array Point) (eps : Float := 1.0e-6) : Bool := Id.run do
  if points.size < 4 then
    return true
  let n := points.size
  let mut sign : Int := 0
  for i in [:n] do
    let a := points[i]!
    let b := points[(i + 1) % n]!
    let c := points[(i + 2) % n]!
    let cross := (b.x - a.x) * (c.y - a.y) - (b.y - a.y) * (c.x - a.x)
    if Float.abs cross > eps then
      let s : Int := if cross > 0.0 then 1 else -1
      if sign == 0 then
        sign := s
      else if sign != s then
        return false
  return true

private def dedupSorted (values : Array Float) (eps : Float := 1.0e-6) : Array Float := Id.run do
  if values.isEmpty then
    return #[]
  let mut result : Array Float := #[values[0]!]
  for i in [1:values.size] do
    let v := values[i]!
    let last := result[result.size - 1]!
    if Float.abs (v - last) > eps then
      result := result.push v
  return result

private def interpolateAtT (p1 p2 : Point) (t1 t2 tBound : Float) (eps : Float := 1.0e-6) : Point :=
  if Float.abs (t2 - t1) < eps then
    p1
  else
    let u0 := (tBound - t1) / (t2 - t1)
    let u := if u0 < 0.0 then 0.0 else if u0 > 1.0 then 1.0 else u0
    { x := p1.x + (p2.x - p1.x) * u, y := p1.y + (p2.y - p1.y) * u }

private def clipPolygonByT (points : Array Point) (start : Point)
    (dx dy lenSq tBound : Float) (keepAbove : Bool) : Array Point := Id.run do
  if points.isEmpty then
    return #[]
  let tAt := fun p =>
    ((p.x - start.x) * dx + (p.y - start.y) * dy) / lenSq
  let n := points.size
  let mut output : Array Point := #[]
  for i in [:n] do
    let curr := points[i]!
    let next := points[(i + 1) % n]!
    let tCurr := tAt curr
    let tNext := tAt next
    let currInside := if keepAbove then tCurr >= tBound else tCurr <= tBound
    let nextInside := if keepAbove then tNext >= tBound else tNext <= tBound
    if currInside && nextInside then
      output := output.push next
    else if currInside && !nextInside then
      output := output.push (interpolateAtT curr next tCurr tNext tBound)
    else if !currInside && nextInside then
      output := output.push (interpolateAtT curr next tCurr tNext tBound)
      output := output.push next
  return output

private def clipPolygonToSlab (points : Array Point) (start : Point)
    (dx dy lenSq t0 t1 : Float) : Array Point :=
  let lower := min t0 t1
  let upper := max t0 t1
  let clipped := clipPolygonByT points start dx dy lenSq lower true
  clipPolygonByT clipped start dx dy lenSq upper false

private def tessellateLinearGradientConvex (points : Array Point) (transform : Point → Point)
    (style : FillStyle) (start finish : Point) (stops : Array GradientStop)
    (screenWidth screenHeight : Float) : TessellationResult := Id.run do
  if points.size < 3 then
    return { vertices := #[], indices := #[] }
  let dx := finish.x - start.x
  let dy := finish.y - start.y
  let lenSq := dx * dx + dy * dy
  if lenSq < 0.0001 then
    -- Degenerate gradient; fall back to solid fill sampling.
    let mut vertices : Array Float := Array.mkEmpty (points.size * 6)
    for p in points do
      let color := sampleFillStyle style p
      let rp := transform p
      let ndc := pixelToNDC rp.x rp.y screenWidth screenHeight
      vertices := vertices.push ndc.x
      vertices := vertices.push ndc.y
      vertices := vertices.push color.r
      vertices := vertices.push color.g
      vertices := vertices.push color.b
      vertices := vertices.push color.a
    let indices := triangulateConvexFan points.size
    return { vertices, indices }

  let tAt := fun p =>
    ((p.x - start.x) * dx + (p.y - start.y) * dy) / lenSq
  let mut tMin := tAt points[0]!
  let mut tMax := tMin
  for p in points do
    let t := tAt p
    if t < tMin then tMin := t
    if t > tMax then tMax := t

  let mut boundsAcc : Array Float := #[tMin, tMax]
  for stop in stops do
    if stop.position > tMin && stop.position < tMax then
      boundsAcc := boundsAcc.push stop.position
  let bounds := dedupSorted (boundsAcc.qsort (· < ·))

  if bounds.size < 2 then
    -- No usable slices; fall back to a single fan.
    let mut vertices : Array Float := Array.mkEmpty (points.size * 6)
    for p in points do
      let color := sampleFillStyle style p
      let rp := transform p
      let ndc := pixelToNDC rp.x rp.y screenWidth screenHeight
      vertices := vertices.push ndc.x
      vertices := vertices.push ndc.y
      vertices := vertices.push color.r
      vertices := vertices.push color.g
      vertices := vertices.push color.b
      vertices := vertices.push color.a
    let indices := triangulateConvexFan points.size
    return { vertices, indices }

  let mut vertices : Array Float := #[]
  let mut indices : Array UInt32 := #[]

  for i in [:bounds.size - 1] do
    let t0 := bounds[i]!
    let t1 := bounds[i + 1]!
    if Float.abs (t1 - t0) < 1.0e-6 then
      continue
    let slice := clipPolygonToSlab points start dx dy lenSq t0 t1
    if slice.size < 3 then
      continue
    let base : UInt32 := (vertices.size / 6).toUInt32
    for p in slice do
      let color := sampleFillStyle style p
      let rp := transform p
      let ndc := pixelToNDC rp.x rp.y screenWidth screenHeight
      vertices := vertices.push ndc.x
      vertices := vertices.push ndc.y
      vertices := vertices.push color.r
      vertices := vertices.push color.g
      vertices := vertices.push color.b
      vertices := vertices.push color.a
    for j in [1:slice.size - 1] do
      indices := indices.push base
      indices := indices.push (base + j.toUInt32)
      indices := indices.push (base + (j + 1).toUInt32)

  return { vertices, indices }

/-- Tessellate a path with a fill style (solid or gradient), converting to NDC.
    Handles both convex and non-convex polygons. -/
def tessellateConvexPathFillNDC (path : Path) (style : FillStyle)
    (screenWidth screenHeight : Float) (tolerance : Float := 0.5) : TessellationResult := Id.run do
  let rings := pathToRings path tolerance
  let (points, holes) := flattenRings rings
  if points.size < 3 then
    return { vertices := #[], indices := #[] }

  -- For convex polygons with multi-stop linear gradients, slice along stops
  -- to preserve intermediate colors (e.g., hue bars).
  match style with
  | .gradient (.linear start finish stops) =>
    if holes.isEmpty && stops.size > 2 && isConvexPolygon points then
      return tessellateLinearGradientConvex points (fun p => p) style start finish stops screenWidth screenHeight
  | _ => pure ()

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

  match style with
  | .gradient (.linear start finish stops) =>
    if holes.isEmpty && stops.size > 2 && isConvexPolygon originalPoints then
      return tessellateLinearGradientConvex originalPoints transform.apply style start finish stops screenWidth screenHeight
  | _ => pure ()

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

/-- Tessellate a polygon for caching (no color, no NDC conversion).
    Takes an array of points in normalized 0-1 coordinates and returns a
    TessellatedPolygon with positions and indices that can be transformed
    and colored later during batched rendering. -/
def tessellatePolygonForCache (points : Array Point) : TessellatedPolygon :=
  if points.size < 3 then
    TessellatedPolygon.empty
  else Id.run do
    -- Build positions array
    let mut positions : Array Float := Array.mkEmpty (points.size * 2)
    let mut minX := points[0]!.x
    let mut minY := points[0]!.y
    let mut maxX := minX
    let mut maxY := minY
    let mut sumX := 0.0
    let mut sumY := 0.0

    for p in points do
      positions := positions.push p.x
      positions := positions.push p.y
      sumX := sumX + p.x
      sumY := sumY + p.y
      if p.x < minX then minX := p.x
      if p.y < minY then minY := p.y
      if p.x > maxX then maxX := p.x
      if p.y > maxY then maxY := p.y

    let centroidX := sumX / points.size.toFloat
    let centroidY := sumY / points.size.toFloat

    -- Use Earcut for tessellation
    let data := pointsToData points
    let indices := Earcut.earcut data #[]

    { positions
      indices
      vertexCount := points.size
      centroidX
      centroidY
      bounds := (minX, minY, maxX, maxY) }

end Tessellation

end Afferent

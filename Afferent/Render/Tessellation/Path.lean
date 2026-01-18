/-
  Afferent Tessellation Path Flattening
-/
import Afferent.Core.Types
import Afferent.Core.Path

namespace Afferent

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

end Tessellation

end Afferent

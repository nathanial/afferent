/-
  Afferent Path
  Path representation following HTML5 Canvas model.
-/
import Afferent.Core.Types

namespace Afferent

/-- Individual path commands following the HTML5 Canvas model. -/
inductive PathCommand where
  | moveTo (p : Point)
  | lineTo (p : Point)
  | quadraticCurveTo (cp : Point) (p : Point)
  | bezierCurveTo (cp1 cp2 : Point) (p : Point)
  | arcTo (p1 p2 : Point) (radius : Float)
  | arc (center : Point) (radius : Float) (startAngle endAngle : Float) (counterclockwise : Bool)
  | rect (r : Rect)
  | closePath
deriving Repr, BEq

/-- Fill rule for determining inside/outside of a path. -/
inductive FillRule where
  | nonZero
  | evenOdd
deriving Repr, BEq, Inhabited

/-- A path is a sequence of commands with tracking of current/start points. -/
structure Path where
  commands : Array PathCommand
  currentPoint : Option Point
  startPoint : Option Point  -- For closePath
  fillRule : FillRule
deriving Repr, Inhabited

namespace Path

def empty : Path :=
  { commands := #[]
    currentPoint := none
    startPoint := none
    fillRule := .nonZero }

def isEmpty (p : Path) : Bool :=
  p.commands.isEmpty

def moveTo (pt : Point) (path : Path) : Path :=
  { path with
    commands := path.commands.push (.moveTo pt)
    currentPoint := some pt
    startPoint := some pt }

def lineTo (pt : Point) (path : Path) : Path :=
  { path with
    commands := path.commands.push (.lineTo pt)
    currentPoint := some pt }

def quadraticCurveTo (cp pt : Point) (path : Path) : Path :=
  { path with
    commands := path.commands.push (.quadraticCurveTo cp pt)
    currentPoint := some pt }

def bezierCurveTo (cp1 cp2 pt : Point) (path : Path) : Path :=
  { path with
    commands := path.commands.push (.bezierCurveTo cp1 cp2 pt)
    currentPoint := some pt }

def arcTo (p1 p2 : Point) (radius : Float) (path : Path) : Path :=
  { path with
    commands := path.commands.push (.arcTo p1 p2 radius)
    currentPoint := some p2 }  -- Approximate; actual endpoint depends on geometry

def arc (center : Point) (radius : Float) (startAngle endAngle : Float)
    (counterclockwise : Bool := false) (path : Path) : Path :=
  let endPt := Point.mk'
    (center.x + radius * Float.cos endAngle)
    (center.y + radius * Float.sin endAngle)
  { path with
    commands := path.commands.push (.arc center radius startAngle endAngle counterclockwise)
    currentPoint := some endPt }

def rect (r : Rect) (path : Path) : Path :=
  { path with
    commands := path.commands.push (.rect r)
    currentPoint := some r.origin
    startPoint := some r.origin }

def closePath (path : Path) : Path :=
  { path with
    commands := path.commands.push .closePath
    currentPoint := path.startPoint }

def withFillRule (rule : FillRule) (path : Path) : Path :=
  { path with fillRule := rule }

/-- Create a rectangular path. -/
def rectangle (r : Rect) : Path :=
  empty
    |>.moveTo r.topLeft
    |>.lineTo r.topRight
    |>.lineTo r.bottomRight
    |>.lineTo r.bottomLeft
    |>.closePath

/-- Create a rectangular path from coordinates. -/
def rectangleXYWH (x y width height : Float) : Path :=
  rectangle (Rect.mk' x y width height)

/-- Approximate a circle using cubic Bezier curves (4 segments). -/
def circle (center : Point) (radius : Float) : Path :=
  -- Magic number for circular arc approximation with cubic beziers
  -- k = 4/3 * tan(π/8) ≈ 0.5522847498
  let k := 0.5522847498 * radius
  let cx := center.x
  let cy := center.y
  let r := radius
  empty
    |>.moveTo ⟨cx + r, cy⟩
    |>.bezierCurveTo ⟨cx + r, cy + k⟩ ⟨cx + k, cy + r⟩ ⟨cx, cy + r⟩
    |>.bezierCurveTo ⟨cx - k, cy + r⟩ ⟨cx - r, cy + k⟩ ⟨cx - r, cy⟩
    |>.bezierCurveTo ⟨cx - r, cy - k⟩ ⟨cx - k, cy - r⟩ ⟨cx, cy - r⟩
    |>.bezierCurveTo ⟨cx + k, cy - r⟩ ⟨cx + r, cy - k⟩ ⟨cx + r, cy⟩
    |>.closePath

/-- Create an ellipse path. -/
def ellipse (center : Point) (radiusX radiusY : Float) : Path :=
  let k := 0.5522847498
  let kx := k * radiusX
  let ky := k * radiusY
  let cx := center.x
  let cy := center.y
  empty
    |>.moveTo ⟨cx + radiusX, cy⟩
    |>.bezierCurveTo ⟨cx + radiusX, cy + ky⟩ ⟨cx + kx, cy + radiusY⟩ ⟨cx, cy + radiusY⟩
    |>.bezierCurveTo ⟨cx - kx, cy + radiusY⟩ ⟨cx - radiusX, cy + ky⟩ ⟨cx - radiusX, cy⟩
    |>.bezierCurveTo ⟨cx - radiusX, cy - ky⟩ ⟨cx - kx, cy - radiusY⟩ ⟨cx, cy - radiusY⟩
    |>.bezierCurveTo ⟨cx + kx, cy - radiusY⟩ ⟨cx + radiusX, cy - ky⟩ ⟨cx + radiusX, cy⟩
    |>.closePath

/-- Create a rounded rectangle path. -/
def roundedRect (r : Rect) (cornerRadius : Float) : Path :=
  let cr := min cornerRadius (min (r.width / 2) (r.height / 2))
  let k := 0.5522847498 * cr
  let x := r.x
  let y := r.y
  let w := r.width
  let h := r.height
  empty
    |>.moveTo ⟨x + cr, y⟩
    |>.lineTo ⟨x + w - cr, y⟩
    |>.bezierCurveTo ⟨x + w - cr + k, y⟩ ⟨x + w, y + cr - k⟩ ⟨x + w, y + cr⟩
    |>.lineTo ⟨x + w, y + h - cr⟩
    |>.bezierCurveTo ⟨x + w, y + h - cr + k⟩ ⟨x + w - cr + k, y + h⟩ ⟨x + w - cr, y + h⟩
    |>.lineTo ⟨x + cr, y + h⟩
    |>.bezierCurveTo ⟨x + cr - k, y + h⟩ ⟨x, y + h - cr + k⟩ ⟨x, y + h - cr⟩
    |>.lineTo ⟨x, y + cr⟩
    |>.bezierCurveTo ⟨x, y + cr - k⟩ ⟨x + cr - k, y⟩ ⟨x + cr, y⟩
    |>.closePath

/-- Convert an arc to cubic Bezier curves.
    Returns an array of (cp1, cp2, endPoint) tuples for each Bezier segment.
    Uses the standard approach of splitting arcs > 90° into multiple segments. -/
def arcToBeziers (center : Point) (radius : Float) (startAngle endAngle : Float)
    (counterclockwise : Bool := false) : Array (Point × Point × Point) := Id.run do
  let pi := 3.14159265358979323846
  let twoPi := 2.0 * pi

  -- Normalize angles and compute sweep
  let mut start := startAngle
  let mut sweep := endAngle - startAngle

  if counterclockwise then
    if sweep > 0 then sweep := sweep - twoPi
  else
    if sweep < 0 then sweep := sweep + twoPi

  -- Split into segments of at most 90 degrees (π/2)
  let maxSweep := pi / 2.0
  let numSegments := (Float.ceil (Float.abs sweep / maxSweep)).toUInt32.toNat
  let numSegments := if numSegments == 0 then 1 else numSegments
  let segmentSweep := sweep / numSegments.toFloat

  let mut result : Array (Point × Point × Point) := #[]

  for _ in [:numSegments] do
    let endAng := start + segmentSweep

    -- Compute control points for this arc segment
    -- Using the formula for bezier approximation of circular arc
    let halfSweep := segmentSweep / 2.0
    let k := 4.0 / 3.0 * Float.tan halfSweep

    let cosStart := Float.cos start
    let sinStart := Float.sin start
    let cosEnd := Float.cos endAng
    let sinEnd := Float.sin endAng

    let p0x := center.x + radius * cosStart
    let p0y := center.y + radius * sinStart
    let p3x := center.x + radius * cosEnd
    let p3y := center.y + radius * sinEnd

    -- Control points perpendicular to radius
    let cp1 := Point.mk' (p0x - k * radius * sinStart) (p0y + k * radius * cosStart)
    let cp2 := Point.mk' (p3x + k * radius * sinEnd) (p3y - k * radius * cosEnd)
    let endPt := Point.mk' p3x p3y

    result := result.push (cp1, cp2, endPt)
    start := endAng

  return result

/-- Create a pie/wedge shape (like a pie chart slice). -/
def pie (center : Point) (radius : Float) (startAngle endAngle : Float) : Path := Id.run do
  let beziers := arcToBeziers center radius startAngle endAngle false

  let startPt := Point.mk'
    (center.x + radius * Float.cos startAngle)
    (center.y + radius * Float.sin startAngle)

  let mut path := empty
    |>.moveTo center
    |>.lineTo startPt

  for (cp1, cp2, endPt) in beziers do
    path := path.bezierCurveTo cp1 cp2 endPt

  return path.closePath

/-- Create an arc path (just the curved part, not closed). -/
def arcPath (center : Point) (radius : Float) (startAngle endAngle : Float)
    (counterclockwise : Bool := false) : Path := Id.run do
  let beziers := arcToBeziers center radius startAngle endAngle counterclockwise

  let startPt := Point.mk'
    (center.x + radius * Float.cos startAngle)
    (center.y + radius * Float.sin startAngle)

  let mut path := empty |>.moveTo startPt

  for (cp1, cp2, endPt) in beziers do
    path := path.bezierCurveTo cp1 cp2 endPt

  return path

/-- Create a semicircle. -/
def semicircle (center : Point) (radius : Float) (startAngle : Float := 0.0) : Path :=
  let pi := 3.14159265358979323846
  arcPath center radius startAngle (startAngle + pi) |>.closePath

/-- Create a quadratic bezier curve path (for demo purposes). -/
def quadraticCurve (start cp endPt : Point) : Path :=
  empty
    |>.moveTo start
    |>.quadraticCurveTo cp endPt

/-- Create a cubic bezier curve path (for demo purposes). -/
def cubicCurve (start cp1 cp2 endPt : Point) : Path :=
  empty
    |>.moveTo start
    |>.bezierCurveTo cp1 cp2 endPt

/-- Create a heart shape. -/
def heart (center : Point) (size : Float) : Path :=
  let s := size
  let cx := center.x
  let cy := center.y
  -- Heart shape using bezier curves
  empty
    |>.moveTo ⟨cx, cy + s * 0.3⟩  -- Bottom point
    |>.bezierCurveTo ⟨cx - s * 0.5, cy - s * 0.2⟩ ⟨cx - s * 0.5, cy - s * 0.5⟩ ⟨cx, cy - s * 0.2⟩
    |>.bezierCurveTo ⟨cx + s * 0.5, cy - s * 0.5⟩ ⟨cx + s * 0.5, cy - s * 0.2⟩ ⟨cx, cy + s * 0.3⟩
    |>.closePath

/-- Create a star shape. -/
def star (center : Point) (outerRadius innerRadius : Float) (points : Nat := 5) : Path := Id.run do
  let pi := 3.14159265358979323846
  let numPoints := if points < 3 then 3 else points
  let angleStep := pi / numPoints.toFloat
  let startAngle := -pi / 2.0  -- Start at top

  let mut path := empty
  let mut first := true

  for i in [:numPoints * 2] do
    let angle := startAngle + i.toFloat * angleStep
    let r := if i % 2 == 0 then outerRadius else innerRadius
    let pt := Point.mk' (center.x + r * Float.cos angle) (center.y + r * Float.sin angle)

    if first then
      path := path.moveTo pt
      first := false
    else
      path := path.lineTo pt

  return path.closePath

/-- Create a regular polygon. -/
def polygon (center : Point) (radius : Float) (sides : Nat) : Path := Id.run do
  let pi := 3.14159265358979323846
  let numSides := if sides < 3 then 3 else sides
  let angleStep := 2.0 * pi / numSides.toFloat
  let startAngle := -pi / 2.0  -- Start at top

  let mut path := empty
  let mut first := true

  for i in [:numSides] do
    let angle := startAngle + i.toFloat * angleStep
    let pt := Point.mk' (center.x + radius * Float.cos angle) (center.y + radius * Float.sin angle)

    if first then
      path := path.moveTo pt
      first := false
    else
      path := path.lineTo pt

  return path.closePath

/-- Create a triangle. -/
def triangle (p1 p2 p3 : Point) : Path :=
  empty
    |>.moveTo p1
    |>.lineTo p2
    |>.lineTo p3
    |>.closePath

/-- Create an equilateral triangle centered at a point. -/
def equilateralTriangle (center : Point) (size : Float) : Path :=
  polygon center size 3

/-- Create a hexagon. -/
def hexagon (center : Point) (radius : Float) : Path :=
  polygon center radius 6

/-- Create an octagon. -/
def octagon (center : Point) (radius : Float) : Path :=
  polygon center radius 8

end Path

end Afferent

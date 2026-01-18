/-
  Afferent Path
  Path representation following HTML5 Canvas model.
-/
import Afferent.Core.Types
import Linalg.Core  -- For Float.pi, Float.twoPi, Float.halfPi
open Linalg

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

/-- Bezier approximation factor for circular arcs: 4/3 * tan(π/8) ≈ 0.5522847498 -/
def bezierCircleK : Float := 0.5522847498

def empty : Path :=
  { commands := #[]
    currentPoint := none
    startPoint := none
    fillRule := .nonZero }

def isEmpty (p : Path) : Bool :=
  p.commands.isEmpty

/-- FNV-1a hash constants for 64-bit hashing. -/
private def fnvOffsetBasis : UInt64 := 14695981039346656037
private def fnvPrime : UInt64 := 1099511628211

/-- Mix a float value into an FNV-1a hash. -/
private def hashMixFloat (h : UInt64) (f : Float) : UInt64 :=
  let bits := f.toUInt64
  let h1 := h ^^^ (bits &&& 0xFF)
  let h2 := (h1 * fnvPrime) ^^^ ((bits >>> 8) &&& 0xFF)
  let h3 := (h2 * fnvPrime) ^^^ ((bits >>> 16) &&& 0xFF)
  let h4 := (h3 * fnvPrime) ^^^ ((bits >>> 24) &&& 0xFF)
  let h5 := (h4 * fnvPrime) ^^^ ((bits >>> 32) &&& 0xFF)
  let h6 := (h5 * fnvPrime) ^^^ ((bits >>> 40) &&& 0xFF)
  let h7 := (h6 * fnvPrime) ^^^ ((bits >>> 48) &&& 0xFF)
  (h7 * fnvPrime) ^^^ ((bits >>> 56) &&& 0xFF)

/-- Mix a byte value into an FNV-1a hash. -/
private def hashMixByte (h : UInt64) (b : UInt8) : UInt64 :=
  (h ^^^ b.toUInt64) * fnvPrime

/-- Mix a Point into an FNV-1a hash. -/
private def hashMixPoint (h : UInt64) (p : Point) : UInt64 :=
  hashMixFloat (hashMixFloat h p.x) p.y

/-- Mix a Rect into an FNV-1a hash. -/
private def hashMixRect (h : UInt64) (r : Rect) : UInt64 :=
  hashMixFloat (hashMixFloat (hashMixFloat (hashMixFloat h r.x) r.y) r.width) r.height

/-- Hash a PathCommand using FNV-1a algorithm.
    Each command type gets a unique tag byte, followed by its parameters. -/
def hashCommand (h : UInt64) : PathCommand → UInt64
  | .moveTo p => hashMixPoint (hashMixByte h 1) p
  | .lineTo p => hashMixPoint (hashMixByte h 2) p
  | .quadraticCurveTo cp p => hashMixPoint (hashMixPoint (hashMixByte h 3) cp) p
  | .bezierCurveTo cp1 cp2 p => hashMixPoint (hashMixPoint (hashMixPoint (hashMixByte h 4) cp1) cp2) p
  | .arcTo p1 p2 radius => hashMixFloat (hashMixPoint (hashMixPoint (hashMixByte h 5) p1) p2) radius
  | .arc center radius startAngle endAngle counterclockwise =>
      let h1 := hashMixByte h 6
      let h2 := hashMixPoint h1 center
      let h3 := hashMixFloat h2 radius
      let h4 := hashMixFloat h3 startAngle
      let h5 := hashMixFloat h4 endAngle
      hashMixByte h5 (if counterclockwise then 1 else 0)
  | .rect r => hashMixRect (hashMixByte h 7) r
  | .closePath => hashMixByte h 8

/-- Compute a 64-bit hash of a Path based on its commands.
    Structurally identical paths will have the same hash.
    Used for O(1) mesh cache lookup. -/
def hash (p : Path) : UInt64 :=
  p.commands.foldl hashCommand fnvOffsetBasis

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
  let k := bezierCircleK * radius
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
  let kx := bezierCircleK * radiusX
  let ky := bezierCircleK * radiusY
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
  let k := bezierCircleK * cr
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
  -- Normalize angles and compute sweep
  let mut start := startAngle
  let mut sweep := endAngle - startAngle

  if counterclockwise then
    if sweep > 0 then sweep := sweep - Float.twoPi
  else
    if sweep < 0 then sweep := sweep + Float.twoPi

  -- Split into segments of at most 90 degrees (π/2)
  let maxSweep := Float.halfPi
  let numSegments := (Float.ceil (Float.abs sweep / maxSweep)).toUInt32.toNat
  let numSegments := if numSegments == 0 then 1 else numSegments
  let segmentSweep := sweep / numSegments.toFloat

  let mut result : Array (Point × Point × Point) := #[]

  for _ in [:numSegments] do
    let endAng := start + segmentSweep

    -- Compute control points for this arc segment
    -- Using the formula for bezier approximation of circular arc
    let quarterSweep := segmentSweep / 4.0
    let k := 4.0 / 3.0 * Float.tan quarterSweep

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
  arcPath center radius startAngle (startAngle + Float.pi) |>.closePath

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
  let numPoints := if points < 3 then 3 else points
  let angleStep := Float.pi / numPoints.toFloat
  let startAngle := -Float.halfPi - angleStep  -- Offset so first outer point is at top

  let mut path := empty
  let mut first := true

  for i in [:numPoints * 2] do
    let angle := startAngle + i.toFloat * angleStep
    let r := if i % 2 == 0 then innerRadius else outerRadius
    let pt := Point.mk' (center.x + r * Float.cos angle) (center.y + r * Float.sin angle)

    if first then
      path := path.moveTo pt
      first := false
    else
      path := path.lineTo pt

  return path.closePath

/-- Create a regular polygon. -/
def polygon (center : Point) (radius : Float) (sides : Nat) : Path := Id.run do
  let numSides := if sides < 3 then 3 else sides
  let angleStep := Float.twoPi / numSides.toFloat
  let startAngle := -Float.halfPi  -- Start at top

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

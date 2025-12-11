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

end Path

end Afferent

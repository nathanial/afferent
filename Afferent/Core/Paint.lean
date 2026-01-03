/-
  Afferent Paint
  Fill and stroke styles for canvas drawing.
-/
import Afferent.Core.Types


namespace Afferent

/-- Line cap style for stroke endpoints. -/
inductive LineCap where
  | butt    -- Flat end at the exact endpoint
  | round   -- Rounded end extending past endpoint
  | square  -- Square end extending past endpoint
deriving Repr, BEq, Inhabited

/-- Line join style for stroke corners. -/
inductive LineJoin where
  | miter  -- Sharp corner (with miter limit)
  | round  -- Rounded corner
  | bevel  -- Beveled corner
deriving Repr, BEq, Inhabited

/-- Dash pattern for stroked lines.
    Segments alternate between dash and gap lengths (e.g., #[5, 3] = 5px dash, 3px gap).
    The pattern repeats along the path. -/
structure DashPattern where
  /-- Alternating dash and gap lengths. Must have even number of elements. -/
  segments : Array Float
  /-- Phase offset to shift the pattern start point. -/
  offset : Float := 0.0
deriving Repr, BEq, Inhabited

namespace DashPattern

/-- Create a simple dash pattern with equal dash and gap. -/
def simple (dashLen gapLen : Float) : DashPattern :=
  { segments := #[dashLen, gapLen], offset := 0.0 }

/-- Create a dotted pattern (very short dashes with gaps). -/
def dotted (gapLen : Float) : DashPattern :=
  { segments := #[1.0, gapLen], offset := 0.0 }

/-- Create a dash-dot pattern. -/
def dashDot (dashLen dotLen gapLen : Float) : DashPattern :=
  { segments := #[dashLen, gapLen, dotLen, gapLen], offset := 0.0 }

/-- Total length of one complete pattern cycle. -/
def cycleLength (p : DashPattern) : Float :=
  p.segments.foldl (· + ·) 0.0

end DashPattern

/-- Stroke style for path outlines. -/
structure StrokeStyle where
  color : Color
  lineWidth : Float
  lineCap : LineCap
  lineJoin : LineJoin
  miterLimit : Float
  /-- Optional dash pattern. None = solid line. -/
  dashPattern : Option DashPattern := none
deriving Repr, BEq

namespace StrokeStyle

def default : StrokeStyle :=
  { color := Color.black
    lineWidth := 1.0
    lineCap := .butt
    lineJoin := .miter
    miterLimit := 10.0 }

def withColor (s : StrokeStyle) (c : Color) : StrokeStyle :=
  { s with color := c }

def withLineWidth (s : StrokeStyle) (w : Float) : StrokeStyle :=
  { s with lineWidth := w }

def withLineCap (s : StrokeStyle) (cap : LineCap) : StrokeStyle :=
  { s with lineCap := cap }

def withLineJoin (s : StrokeStyle) (join : LineJoin) : StrokeStyle :=
  { s with lineJoin := join }

def withDashPattern (s : StrokeStyle) (pattern : Option DashPattern) : StrokeStyle :=
  { s with dashPattern := pattern }

/-- Create a dashed stroke style with specified dash and gap lengths. -/
def dashed (color : Color) (lineWidth : Float) (dashLen gapLen : Float) : StrokeStyle :=
  { color, lineWidth, lineCap := .butt, lineJoin := .miter,
    miterLimit := 10.0, dashPattern := some (DashPattern.simple dashLen gapLen) }

/-- Create a dotted stroke style. Uses round caps for circular dots. -/
def dotted (color : Color) (lineWidth : Float) : StrokeStyle :=
  { color, lineWidth, lineCap := .round, lineJoin := .miter,
    miterLimit := 10.0, dashPattern := some (DashPattern.dotted (lineWidth * 2)) }

instance : Inhabited StrokeStyle := ⟨default⟩

end StrokeStyle

/-- Gradient stop (position 0-1 and color). -/
structure GradientStop where
  position : Float
  color : Color
deriving Repr, BEq, Inhabited

namespace GradientStop

/-- Create gradient stops with auto-distributed positions from colors.
    For n colors, positions are: 0, 1/(n-1), 2/(n-1), ..., 1 -/
def distribute (colors : Array Color) : Array GradientStop :=
  let n := colors.size
  if n <= 1 then
    colors.map fun c => { position := 0.0, color := c }
  else
    let divisor := (n - 1).toFloat
    Id.run do
      let mut result := #[]
      for h : i in [:n] do
        result := result.push { position := i.toFloat / divisor, color := colors[i] }
      return result

end GradientStop

/-- Gradient macro for creating gradient stop arrays with auto-distributed positions.
    For n colors, positions are evenly spaced: 0, 1/(n-1), 2/(n-1), ..., 1

    ```
    gradient![Color.red, Color.blue]               -- positions: 0.0, 1.0
    gradient![Color.red, Color.green, Color.blue]  -- positions: 0.0, 0.5, 1.0
    ```
-/
macro "gradient![" cs:term,+ "]" : term =>
  `(GradientStop.distribute #[$cs,*])

/-- Gradient definition. -/
inductive Gradient where
  | linear (start finish : Point) (stops : Array GradientStop)
  | radial (center : Point) (radius : Float) (stops : Array GradientStop)
deriving Repr, BEq, Inhabited

/-- Fill style for path interiors. -/
inductive FillStyle where
  | solid (color : Color)
  | gradient (g : Gradient)
  -- | pattern (p : Pattern)  -- Future: texture/pattern fills
deriving Repr, BEq

namespace FillStyle

def default : FillStyle := .solid Color.black

def color (c : Color) : FillStyle := .solid c

def linearGradient (start finish : Point) (stops : Array GradientStop) : FillStyle :=
  .gradient (.linear start finish stops)

def radialGradient (center : Point) (radius : Float) (stops : Array GradientStop) : FillStyle :=
  .gradient (.radial center radius stops)

/-- Extract the primary color from a fill style (for simple rendering). -/
def toColor : FillStyle → Color
  | .solid c => c
  | .gradient (.linear _ _ stops) =>
    if h : stops.size > 0 then stops[0].color else Color.black
  | .gradient (.radial _ _ stops) =>
    if h : stops.size > 0 then stops[0].color else Color.black

instance : Inhabited FillStyle := ⟨default⟩

end FillStyle

end Afferent

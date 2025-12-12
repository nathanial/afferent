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

/-- Stroke style for path outlines. -/
structure StrokeStyle where
  color : Color
  lineWidth : Float
  lineCap : LineCap
  lineJoin : LineJoin
  miterLimit : Float
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

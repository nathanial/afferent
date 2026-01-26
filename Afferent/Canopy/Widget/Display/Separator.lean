/-
  Canopy Separator Widget
  Simple horizontal/vertical divider line.
-/
import Afferent.Canopy.Theme
import Afferent.Canopy.Reactive.Component

namespace Afferent.Canopy

open Afferent.Arbor hiding Event

/-- Separator orientation. -/
inductive SeparatorOrientation where
  | horizontal  -- Horizontal line divider
  | vertical    -- Vertical line divider
deriving Repr, BEq, Inhabited

/-- Create a separator divider line.
    - `orientation`: Horizontal or vertical
    - `theme`: Theme for color
    - `thickness`: Line thickness in pixels (default: 1)
    - `margin`: Space around the separator (default: 8)
-/
def separator (orientation : SeparatorOrientation) (theme : Theme)
    (thickness : Float := 1.0) (margin : Float := 8.0) : WidgetBuilder := do
  let color := theme.panel.border.withAlpha 0.5

  let style : BoxStyle := match orientation with
    | .horizontal => {
        backgroundColor := some color
        width := .percent 1.0
        height := .length thickness
        margin := Trellis.EdgeInsets.symmetric 0 margin
      }
    | .vertical => {
        backgroundColor := some color
        width := .length thickness
        height := .percent 1.0
        margin := Trellis.EdgeInsets.symmetric margin 0
      }

  box style

/-- Create a horizontal separator. -/
def hseparator (theme : Theme) (thickness : Float := 1.0)
    (margin : Float := 8.0) : WidgetBuilder :=
  separator .horizontal theme thickness margin

/-- Create a vertical separator. -/
def vseparator (theme : Theme) (thickness : Float := 1.0)
    (margin : Float := 8.0) : WidgetBuilder :=
  separator .vertical theme thickness margin

/-! ## WidgetM Wrappers -/

open Afferent.Canopy.Reactive

/-- Emit a separator. -/
def separator' (orientation : SeparatorOrientation)
    (thickness : Float := 1.0) (margin : Float := 8.0) : WidgetM Unit := do
  let theme ← getThemeW
  emit (pure (separator orientation theme thickness margin))

/-- Emit a horizontal separator. -/
def hseparator' (thickness : Float := 1.0)
    (margin : Float := 8.0) : WidgetM Unit := do
  let theme ← getThemeW
  emit (pure (hseparator theme thickness margin))

/-- Emit a vertical separator. -/
def vseparator' (thickness : Float := 1.0)
    (margin : Float := 8.0) : WidgetM Unit := do
  let theme ← getThemeW
  emit (pure (vseparator theme thickness margin))

end Afferent.Canopy

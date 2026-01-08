/-
  Canopy RadioButton Widget
  Single selection radio button within a group.
-/
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Widget.Button

namespace Afferent.Canopy

open Afferent.Arbor

namespace RadioButton

/-- Custom spec for radio button circle rendering (filled dot when selected). -/
def circleSpec (selected : Bool) (_hovered : Bool) (theme : Theme) (size : Float) : CustomSpec := {
  measure := fun _ _ => (size, size)
  collect := fun layout =>
    let rect := layout.contentRect
    let cmds : Array RenderCommand := #[]
    if selected then
      -- Draw inner filled circle (50% of outer size)
      let innerSize := size * 0.5
      let offsetX := (size - innerSize) / 2
      let offsetY := (size - innerSize) / 2
      let innerRect := Arbor.Rect.mk' (rect.x + offsetX) (rect.y + offsetY) innerSize innerSize
      cmds.push (.fillRect innerRect theme.primary.foreground (innerSize / 2))
    else
      cmds
  draw := none
}

end RadioButton

/-- Build a visual radio button (WidgetBuilder version).
    - `name`: Widget name for hit testing
    - `labelText`: Text to display next to radio button
    - `theme`: Theme for styling
    - `selected`: Whether this radio button is currently selected
    - `state`: Widget interaction state (hover, focus, etc.)
-/
def radioButtonVisual (name : String) (labelText : String) (theme : Theme)
    (selected : Bool) (state : WidgetState := {}) : WidgetBuilder := do
  let colors := theme.input
  let circleSize : Float := 20.0
  let circleBg := if selected then theme.primary.background else colors.background
  let borderColor := if state.focused then colors.borderFocused else colors.border

  let radioCircle : WidgetBuilder := do
    custom (RadioButton.circleSpec selected state.hovered theme circleSize) {
      minWidth := some circleSize
      minHeight := some circleSize
      cornerRadius := circleSize / 2  -- Full circle
      borderColor := some borderColor
      borderWidth := if state.focused then 2 else 1
      backgroundColor := some circleBg
    }

  -- Use custom flex container with alignItems := .center to prevent stretching
  let wid ← freshId
  let props : Trellis.FlexContainer := { Trellis.FlexContainer.row 8 with alignItems := .center }
  let circle ← radioCircle
  let label ← text' labelText theme.font theme.text .left
  pure (.flex wid (some name) props {} #[circle, label])

/-- Create an interactive radio button (UIBuilder version).
    - `name`: Unique name for state tracking
    - `value`: Value this radio button represents
    - `labelText`: Text to display next to radio button
    - `onSelect`: Message to emit with value when selected
    - `theme`: Theme for styling
    - `selected`: Whether this radio button is currently selected
    - `state`: Widget interaction state
-/
def radioButton {Msg : Type} (name : String) (value : String) (labelText : String)
    (onSelect : String → Msg)
    (theme : Theme)
    (selected : Bool := false)
    (state : WidgetState := {})
    : UIBuilder (WidgetMsg Msg) Widget := do
  let wid ← UIBuilder.freshId

  UIBuilder.register wid fun _ event =>
    match event with
    | .mouseEnter _ => { msgs := #[.hover name true] }
    | .mouseLeave _ => { msgs := #[.hover name false] }
    | .mouseClick _ =>
        if !state.disabled && !selected then
          { msgs := #[.app (onSelect value)] }
        else {}
    | .keyPress e =>
        if state.focused && (e.key == .space || e.key == .enter) && !state.disabled && !selected then
          { msgs := #[.app (onSelect value)] }
        else {}
    | _ => {}

  UIBuilder.lift (radioButtonVisual name labelText theme selected state)

end Afferent.Canopy

/-
  Canopy Checkbox Widget
  Toggle checkbox with checked/unchecked states.
-/
import Reactive
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Widget.Button
import Afferent.Canopy.Reactive.Component

namespace Afferent.Canopy

open Afferent.Arbor hiding Event

/-- Extended state for checkbox widgets. -/
structure CheckboxState extends WidgetState where
  checked : Bool := false
deriving Repr, BEq, Inhabited

namespace Checkbox

/-- Build a checkmark path centered in a box. -/
def checkmarkPath (x y size : Float) : Arbor.Path :=
  let s := size * 0.6  -- Scale factor for checkmark within box
  let cx := x + size / 2
  let cy := y + size / 2
  -- Checkmark shape: down-left to bottom, then up-right
  let p1 : Arbor.Point := ⟨cx - s * 0.35, cy⟩                -- Left arm start
  let p2 : Arbor.Point := ⟨cx - s * 0.1, cy + s * 0.35⟩     -- Bottom point
  let p3 : Arbor.Point := ⟨cx + s * 0.4, cy - s * 0.35⟩     -- Right arm end
  Arbor.Path.empty
    |>.moveTo p1
    |>.lineTo p2
    |>.lineTo p3

/-- Custom spec for checkbox box rendering. -/
def boxSpec (checked : Bool) (_hovered : Bool) (theme : Theme) (size : Float) : CustomSpec := {
  measure := fun _ _ => (size, size)
  collect := fun layout =>
    let rect := layout.contentRect
    let cmds : Array RenderCommand := #[]
    if checked then
      let path := checkmarkPath rect.x rect.y size
      cmds.push (.strokePath path theme.primary.foreground 2.5)
    else
      cmds
  draw := none
}

end Checkbox

/-- Create an interactive checkbox with label.
    - `name`: Unique name for state tracking
    - `labelText`: Text to display next to checkbox
    - `onToggle`: Message to emit with new checked state when toggled
    - `theme`: Theme for styling
    - `state`: Current checkbox state (includes checked boolean)
-/
def checkbox {Msg : Type} (name : String) (labelText : String)
    (onToggle : Bool → Msg)
    (theme : Theme)
    (state : CheckboxState := {})
    : UIBuilder (WidgetMsg Msg) Widget := do
  let wid ← UIBuilder.freshId

  let colors := theme.input
  let boxSize : Float := 20.0

  -- Checkbox box background - blue when checked, transparent otherwise
  let boxBg := if state.checked then theme.primary.background else colors.background
  let borderColor := if state.focused then colors.borderFocused else colors.border

  -- Register event handlers
  UIBuilder.register wid fun _ event =>
    match event with
    | .mouseEnter _ => { msgs := #[.hover name true] }
    | .mouseLeave _ => { msgs := #[.hover name false] }
    | .mouseClick _ =>
        if !state.disabled then
          let newChecked := !state.checked
          { msgs := #[.app (onToggle newChecked)] }
        else {}
    | .keyPress e =>
        if state.focused && (e.key == .space || e.key == .enter) && !state.disabled then
          let newChecked := !state.checked
          { msgs := #[.app (onToggle newChecked)] }
        else {}
    | _ => {}

  -- Build visual structure: row with checkbox box + label
  UIBuilder.lift do
    -- Use custom flex container with alignItems := .center to prevent stretching
    let wid ← freshId
    let props : Trellis.FlexContainer := { Trellis.FlexContainer.row 8 with alignItems := .center }
    let checkboxBox ← custom (Checkbox.boxSpec state.checked state.hovered theme boxSize) {
      minWidth := some boxSize
      minHeight := some boxSize
      cornerRadius := 4
      borderColor := some borderColor
      borderWidth := if state.focused then 2 else 1
      backgroundColor := some boxBg
    }
    let label ← text' labelText theme.font theme.text .left
    pure (.flex wid none props {} #[checkboxBox, label])

/-- Create a checkbox without a label (box only). -/
def checkboxOnly {Msg : Type} (name : String)
    (onToggle : Bool → Msg)
    (theme : Theme)
    (state : CheckboxState := {})
    : UIBuilder (WidgetMsg Msg) Widget := do
  let wid ← UIBuilder.freshId

  let colors := theme.input
  let boxSize : Float := 20.0
  let boxBg := if state.checked then theme.primary.background else colors.background
  let borderColor := if state.focused then colors.borderFocused else colors.border

  UIBuilder.register wid fun _ event =>
    match event with
    | .mouseEnter _ => { msgs := #[.hover name true] }
    | .mouseLeave _ => { msgs := #[.hover name false] }
    | .mouseClick _ =>
        if !state.disabled then
          let newChecked := !state.checked
          { msgs := #[.app (onToggle newChecked)] }
        else {}
    | _ => {}

  UIBuilder.lift do
    custom (Checkbox.boxSpec state.checked state.hovered theme boxSize) {
      minWidth := some boxSize
      minHeight := some boxSize
      cornerRadius := 4
      borderColor := some borderColor
      borderWidth := if state.focused then 2 else 1
      backgroundColor := some boxBg
    }

/-! ## Reactive Checkbox Components (FRP-based)

These use WidgetM for declarative composition with automatic state management.
-/

open Reactive Reactive.Host
open Afferent.Canopy.Reactive

/-- Checkbox result - events and dynamics. -/
structure CheckboxResult where
  onToggle : Reactive.Event Spider Bool
  isChecked : Reactive.Dynamic Spider Bool

/-- Build the visual for a checkbox given its state (pure WidgetBuilder). -/
def checkboxVisual (name : String) (labelText : String) (theme : Theme)
    (checked : Bool) (state : WidgetState) : WidgetBuilder := do
  let colors := theme.input
  let boxSize : Float := 20.0
  let boxBg := if checked then theme.primary.background else colors.background
  let borderColor := if state.focused then colors.borderFocused else colors.border

  let checkboxBox : WidgetBuilder := do
    if checked then
      custom (Checkbox.boxSpec checked state.hovered theme boxSize) {
        minWidth := some boxSize
        minHeight := some boxSize
        cornerRadius := 4
        borderColor := some borderColor
        borderWidth := if state.focused then 2 else 1
        backgroundColor := some boxBg
      }
    else
      box {
        minWidth := some boxSize
        minHeight := some boxSize
        cornerRadius := 4
        borderColor := some borderColor
        borderWidth := if state.focused then 2 else 1
        backgroundColor := some boxBg
      }

  let wid ← freshId
  let props : Trellis.FlexContainer := { Trellis.FlexContainer.row 8 with alignItems := .center }
  let checkBox ← checkboxBox
  let label ← text' labelText theme.font theme.text .left
  pure (.flex wid (some name) props {} #[checkBox, label])

/-- Create a reactive checkbox component using WidgetM.
    Emits the checkbox widget and returns toggle state.
    - `label`: Label text displayed next to checkbox
    - `theme`: Theme for styling
    - `initialChecked`: Initial checked state
-/
def Checkbox.reactive (label : String) (theme : Theme) (initialChecked : Bool := false)
    : WidgetM CheckboxResult := do
  let name ← registerComponentW "checkbox"
  let isHovered ← useHover name
  let clicks ← useClick name
  let isChecked ← Reactive.foldDyn (fun _ checked => !checked) initialChecked clicks
  let onToggle := isChecked.updated

  emit do
    let hovered ← isHovered.sample
    let checked ← isChecked.sample
    let state : WidgetState := { hovered, pressed := false, focused := false }
    pure (checkboxVisual name label theme checked state)

  pure { onToggle, isChecked }

end Afferent.Canopy

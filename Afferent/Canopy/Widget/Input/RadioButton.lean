/-
  Canopy RadioButton Widget
  Single selection radio button within a group.
-/
import Reactive
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Widget.Input.Button
import Afferent.Canopy.Reactive.Component

namespace Afferent.Canopy

open Afferent.Arbor hiding Event

namespace RadioButton

/-- Custom spec for radio button circle rendering (filled dot when selected). -/
def circleSpec (selected : Bool) (_hovered : Bool) (theme : Theme) (size : Float) : CustomSpec := {
  measure := fun _ _ => (size, size)
  collect := fun layout =>
    let rect := layout.contentRect
    RenderM.build do
      if selected then
        -- Draw inner filled circle (50% of outer size)
        let innerSize := size * 0.5
        let offsetX := (size - innerSize) / 2
        let offsetY := (size - innerSize) / 2
        let innerRect := Arbor.Rect.mk' (rect.x + offsetX) (rect.y + offsetY) innerSize innerSize
        RenderM.fillRect innerRect theme.primary.foreground (innerSize / 2)
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

/-! ## Reactive RadioGroup Components (FRP-based)

These use WidgetM for declarative composition with automatic selection tracking.
-/

open Reactive Reactive.Host
open Afferent.Canopy.Reactive

/-- A single radio button option. -/
structure RadioOption where
  label : String
  value : String

/-- RadioGroup result - events and dynamics. -/
structure RadioGroupResult where
  onSelect : Reactive.Event Spider String
  selected : Reactive.Dynamic Spider String

/-- Create a reactive radio group component using WidgetM.
    Emits the radio group widget and returns selection state.
    - `options`: Array of radio options (label and value)
    - `initialSelection`: Initial selected value
-/
def radioGroup (options : Array RadioOption) (initialSelection : String)
    : WidgetM RadioGroupResult := do
  let theme ← getThemeW
  let mut optionNames : Array String := #[]
  for _ in options do
    let name ← registerComponentW "radio"
    optionNames := optionNames.push name

  let allClicks ← useAllClicks

  let findClickedOption (data : ClickData) : Option String :=
    (options.zip optionNames).findSome? fun (opt, name) =>
      if hitWidget data name then some opt.value else none

  let selectionChanges ← Event.mapMaybeM findClickedOption allClicks
  let selected ← Reactive.holdDyn initialSelection selectionChanges
  let onSelect := selectionChanges

  let hoverChanges ← StateT.lift (hoverEventForTargets (optionNames.map fun name => (name, name)))
  let hoveredOption ← Reactive.holdDyn none hoverChanges

  let optionsWithNames := options.zip optionNames

  -- Use dynWidget for efficient change-driven rebuilds
  let renderState ← Dynamic.zipWithM (fun s h => (s, h)) selected hoveredOption
  let _ ← dynWidget renderState fun (selectedValue, hoveredName) => do
    let mut builders : Array WidgetBuilder := #[]
    for (opt, name) in optionsWithNames do
      let isHovered := hoveredName == some name
      let isSelected := selectedValue == opt.value
      let state : WidgetState := { hovered := isHovered, pressed := false, focused := false }
      builders := builders.push (radioButtonVisual name opt.label theme isSelected state)
    emit do pure (column (gap := 8) (style := {}) builders)

  pure { onSelect, selected }

end Afferent.Canopy

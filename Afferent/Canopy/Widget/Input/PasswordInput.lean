/-
  Canopy PasswordInput Widget
  Masked text input with show/hide toggle.
-/
import Reactive
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Widget.Display.Label
import Afferent.Canopy.Widget.Input.TextInput
import Afferent.Canopy.Reactive.Component

namespace Afferent.Canopy

open Afferent.Arbor hiding Event
open Reactive Reactive.Host
open Afferent.Canopy.Reactive
open Trellis

namespace PasswordInput

def maskChar : Char := '*'

def maskString (n : Nat) : String :=
  String.ofList (List.replicate n maskChar)

def maskedText (s : String) : String :=
  maskString s.length

/-- Compute cursor pixel position based on display mode. -/
def computeCursorPixelX (font : Afferent.Font) (state : TextInputState)
    (showPassword : Bool) : IO TextInputState := do
  let textBeforeCursor :=
    if showPassword then
      state.value.take state.cursor
    else
      maskString state.cursor
  let (width, _) ← font.measureText textBeforeCursor
  pure { state with cursorPixelX := width }

end PasswordInput

/-- Visual for password input with optional masking. -/
def passwordInputVisual (name toggleName : String) (theme : Theme)
    (state : TextInputState) (showPassword : Bool) (toggleHovered : Bool)
    (placeholder : String := "") : WidgetBuilder := do
  let colors := theme.input
  let bgColor := if state.disabled then colors.backgroundDisabled else colors.background
  let borderColor := if state.focused then colors.borderFocused else colors.border

  let verticalPadding := theme.padding * 0.5 * 2
  let minHeight := theme.font.lineHeight + verticalPadding + 4

  let style : BoxStyle := {
    backgroundColor := some bgColor
    borderColor := some borderColor
    borderWidth := if state.focused then 2 else 1
    cornerRadius := theme.cornerRadius
    padding := Trellis.EdgeInsets.symmetric theme.padding (theme.padding * 0.5)
    minWidth := some 200
    minHeight := some minHeight
  }

  let showPlaceholder := state.value.isEmpty && !state.focused
  let displayText :=
    if showPassword then state.value else PasswordInput.maskedText state.value

  let textWidget ← custom (TextInput.inputSpec displayText placeholder showPlaceholder
      state.cursorPixelX state.focused theme) { flexItem := some (FlexItem.growing 1) }

  let toggleLabel := if showPassword then "Hide" else "Show"
  let toggleBg := if toggleHovered then theme.secondary.backgroundHover else Color.transparent
  let toggleStyle : BoxStyle := {
    backgroundColor := some toggleBg
    cornerRadius := theme.cornerRadius
    padding := EdgeInsets.symmetric (theme.padding * 0.4) (theme.padding * 0.2)
  }
  let toggleWid ← freshId
  let toggleProps : FlexContainer := {
    direction := .row
    alignItems := .center
    justifyContent := .center
  }
  let toggleText ← text' toggleLabel theme.font theme.textMuted .center
  let toggle : Widget := .flex toggleWid (some toggleName) toggleProps toggleStyle #[toggleText]

  let innerWid ← freshId
  let innerProps : FlexContainer := { direction := .row, gap := 8, alignItems := .center }
  let inner : Widget := .flex innerWid none innerProps {} #[textWidget, toggle]

  let outerWid ← freshId
  let outerProps : FlexContainer := {
    direction := .row
    alignItems := .center
  }
  pure (.flex outerWid (some name) outerProps style #[inner])

/-! ## Reactive PasswordInput Components (FRP-based) -/

structure PasswordInputState where
  text : TextInputState := {}
  revealed : Bool := false
deriving Repr, BEq, Inhabited

inductive PasswordInputEvent where
  | key (data : KeyData)
  | toggle

/-- Create a reactive password input component using WidgetM.
    Emits the password input widget and returns text state.
    Uses the default font from WidgetM context (set via createInputs).
    - `placeholder`: Placeholder text when empty
    - `initialValue`: Initial text value
-/
def passwordInput (placeholder : String) (initialValue : String := "") : WidgetM TextInputResult := do
  let theme ← getThemeW
  let font ← getFontW
  let name ← registerComponentW "password-input" (isInput := true)
  let toggleName ← registerComponentW "password-toggle"
  let events ← getEventsW
  let focusedInput := events.registry.focusedInput
  let fireFocusedInput := events.registry.fireFocus

  let clicks ← useClick name
  let toggleClicks ← useClick toggleName
  let keyEvents ← useKeyboard
  let toggleHovered ← useHover toggleName

  let isFocused ← Dynamic.mapM (· == some name) focusedInput

  let focusChanges ← Dynamic.changesM focusedInput
  let focusEvents ← Event.filterM
    (fun (old, new) => old != some name && new == some name) focusChanges
  let onFocus ← Event.voidM focusEvents
  let blurEvents ← Event.filterM
    (fun (old, new) => old == some name && new != some name) focusChanges
  let onBlur ← Event.voidM blurEvents

  let notFocused ← Dynamic.mapM (· != some name) focusedInput
  let focusClicks ← Event.gateM notFocused.current clicks
  let focusAction ← Event.mapM (fun _ => fireFocusedInput (some name)) focusClicks
  performEvent_ focusAction

  let gatedKeys ← Event.gateM isFocused.current keyEvents
  let initialText : TextInputState := {
    value := initialValue
    cursor := initialValue.length
    cursorPixelX := 0.0
  }
  let initialText ← SpiderM.liftIO (PasswordInput.computeCursorPixelX font initialText false)
  let initialState : PasswordInputState := { text := initialText, revealed := false }

  let liftSpider {α : Type} : SpiderM α → WidgetM α := fun m => StateT.lift (liftM m)
  let allEvents ← liftSpider do
    let keyE ← Event.mapM PasswordInputEvent.key gatedKeys
    let toggleE ← Event.mapM (fun _ => PasswordInputEvent.toggle) toggleClicks
    Event.leftmostM [keyE, toggleE]

  let combinedState ← Reactive.foldDynM
    (fun event state => do
      match event with
      | .key keyData =>
        let updated := TextInput.handleKeyPress keyData.event state.text none
        let updated ← SpiderM.liftIO (PasswordInput.computeCursorPixelX font updated state.revealed)
        pure { state with text := updated }
      | .toggle =>
        let revealed := !state.revealed
        let updated ← SpiderM.liftIO (PasswordInput.computeCursorPixelX font state.text revealed)
        pure { text := updated, revealed := revealed }
    )
    initialState
    allEvents

  let textState ← Dynamic.mapM (·.text) combinedState
  let textChanges ← Dynamic.changesM textState
  let valueChanges ← Event.mapMaybeM
    (fun (old, new) => if old.value != new.value then some new.value else none)
    textChanges
  let onChange := valueChanges

  let text ← Dynamic.mapM (·.value) textState

  -- Use dynWidget for efficient change-driven rebuilds
  let renderState1 ← Dynamic.zipWithM (fun s f => (s, f)) combinedState focusedInput
  let renderState2 ← Dynamic.zipWithM (fun (s, f) h => (s, f, h)) renderState1 toggleHovered
  let _ ← dynWidget renderState2 fun (state, focused, toggleH) => do
    let isFoc := focused == some name
    let displayState : TextInputState := { state.text with focused := isFoc }
    emit do pure (passwordInputVisual name toggleName theme displayState state.revealed toggleH placeholder)

  pure { onChange, onFocus, onBlur, text, isFocused }

end Afferent.Canopy

/-
  Canopy TextInput Widget
  Single-line text input with cursor and editing support.
-/
import Reactive
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Widget.Input.Button
import Afferent.Canopy.Reactive.Component

namespace Afferent.Canopy

open Afferent.Arbor hiding Event

/-- Extended state for text input widgets. -/
structure TextInputState extends WidgetState where
  value : String := ""
  cursor : Nat := 0
  /-- Pre-computed cursor X position in pixels (for accurate positioning with proportional fonts). -/
  cursorPixelX : Float := 0
deriving Repr, BEq, Inhabited

namespace TextInputState

/-- Insert a character at cursor position. -/
def insertChar (s : TextInputState) (c : Char) : TextInputState :=
  let before := s.value.take s.cursor
  let after := s.value.drop s.cursor
  { s with
    value := before ++ c.toString ++ after
    cursor := s.cursor + 1 }

/-- Delete character before cursor (backspace). -/
def deleteBackward (s : TextInputState) : TextInputState :=
  if s.cursor > 0 then
    let before := s.value.take (s.cursor - 1)
    let after := s.value.drop s.cursor
    { s with
      value := before ++ after
      cursor := s.cursor - 1 }
  else s

/-- Delete character at cursor (delete key). -/
def deleteForward (s : TextInputState) : TextInputState :=
  if s.cursor < s.value.length then
    let before := s.value.take s.cursor
    let after := s.value.drop (s.cursor + 1)
    { s with value := before ++ after }
  else s

/-- Move cursor left. -/
def moveCursorLeft (s : TextInputState) : TextInputState :=
  if s.cursor > 0 then { s with cursor := s.cursor - 1 }
  else s

/-- Move cursor right. -/
def moveCursorRight (s : TextInputState) : TextInputState :=
  if s.cursor < s.value.length then { s with cursor := s.cursor + 1 }
  else s

/-- Move cursor to start. -/
def moveCursorStart (s : TextInputState) : TextInputState :=
  { s with cursor := 0 }

/-- Move cursor to end. -/
def moveCursorEnd (s : TextInputState) : TextInputState :=
  { s with cursor := s.value.length }

/-- Delete all text from cursor to end. -/
def deleteToEnd (s : TextInputState) : TextInputState :=
  { s with value := s.value.take s.cursor }

/-- Delete all text from start to cursor. -/
def deleteToStart (s : TextInputState) : TextInputState :=
  { s with value := s.value.drop s.cursor, cursor := 0 }

end TextInputState

namespace TextInput

/-- Custom spec for text input rendering with cursor.
    cursorPixelX is the pre-computed cursor position in pixels (measured from text start). -/
def inputSpec (displayText : String) (placeholder : String) (showPlaceholder : Bool)
    (cursorPixelX : Float) (focused : Bool) (theme : Theme) : CustomSpec := {
  measure := fun _ _ =>
    -- Use actual font metrics for sizing
    let lineHeight := theme.font.lineHeight
    let height := lineHeight + 4
    -- Return minimum intrinsic width (container minWidth handles actual sizing)
    (0, height)
  collect := fun layout =>
    let rect := layout.contentRect
    let text := if showPlaceholder then placeholder else displayText
    let textColor := if showPlaceholder then theme.textMuted else theme.text
    -- Vertical centering using actual font metrics
    let lineHeight := theme.font.lineHeight
    let ascender := theme.font.ascender
    let verticalOffset := (rect.height - lineHeight) / 2
    let textY := rect.y + verticalOffset + ascender
    RenderM.build do
      RenderM.fillText text rect.x textY theme.font textColor
      if focused then
        let cursorX := rect.x + cursorPixelX  -- Use pre-computed cursor position
        let cursorY := rect.y + verticalOffset
        let cursorH := lineHeight
        RenderM.fillRect (Arbor.Rect.mk' cursorX cursorY 2 cursorH) theme.focusRing 0
  draw := none
}

/-- Handle key press for text input. -/
def handleKeyPress (e : KeyEvent) (state : TextInputState) (maxLen : Option Nat) : TextInputState :=
  if e.modifiers.cmd then
    match e.key with
    | .left => state.moveCursorStart
    | .right => state.moveCursorEnd
    | .delete => state.deleteToEnd
    | .backspace => state.deleteToStart
    | _ => state
  else if e.modifiers.ctrl then
    match e.key with
    | .delete => state.deleteToEnd
    | .backspace => state.deleteToStart
    | _ => state
  else
    match e.key with
    | .char c =>
        let charToInsert := if e.modifiers.shift then Key.shiftChar c else c
        match maxLen with
        | some max => if state.value.length >= max then state else state.insertChar charToInsert
        | none => state.insertChar charToInsert
    | .space =>
        match maxLen with
        | some max => if state.value.length >= max then state else state.insertChar ' '
        | none => state.insertChar ' '
    | .backspace => state.deleteBackward
    | .delete => state.deleteForward
    | .left => state.moveCursorLeft
    | .right => state.moveCursorRight
    | .home => state.moveCursorStart
    | .«end» => state.moveCursorEnd
    | _ => state

/-- Compute cursor pixel position by measuring text before cursor. -/
def computeCursorPixelX (font : Afferent.Font) (state : TextInputState) : IO TextInputState := do
  let textBeforeCursor := state.value.take state.cursor
  let (width, _) ← font.measureText textBeforeCursor
  pure { state with cursorPixelX := width }

end TextInput

/-- Build the visual representation of a text input (WidgetBuilder version).
    Use this when you need just the visual widget without UIBuilder event handling.
    - `name`: Widget name for hit testing
    - `theme`: Theme for styling
    - `state`: Current text input state
    - `placeholder`: Placeholder text when empty
-/
def textInputVisual (name : String) (theme : Theme)
    (state : TextInputState) (placeholder : String := "") : WidgetBuilder := do
  let colors := theme.input
  let bgColor := if state.disabled then colors.backgroundDisabled else colors.background
  let borderColor := if state.focused then colors.borderFocused else colors.border

  -- Calculate minHeight from actual font metrics + padding
  let verticalPadding := theme.padding * 0.5 * 2  -- top + bottom
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

  -- Use left-aligned flex container (not centered) for text input content
  let wid ← freshId
  let props : Trellis.FlexContainer := {
    direction := .row
    justifyContent := .flexStart  -- Left align horizontally
    alignItems := .center         -- Center vertically
  }
  let child ← custom (TextInput.inputSpec state.value placeholder showPlaceholder
          state.cursorPixelX state.focused theme) {}
  pure (.flex wid (some name) props style #[child])

/-! ## Reactive TextInput Components (FRP-based)

These use WidgetM for declarative composition with automatic focus and keyboard handling.
-/

open Reactive Reactive.Host
open Afferent.Canopy.Reactive

/-- TextInput result - events and dynamics. -/
structure TextInputResult where
  onChange : Reactive.Event Spider String
  onFocus : Reactive.Event Spider Unit
  onBlur : Reactive.Event Spider Unit
  text : Reactive.Dynamic Spider String
  isFocused : Reactive.Dynamic Spider Bool

/-- Create a reactive text input component using WidgetM.
    Emits the text input widget and returns text state.
    Uses the default font from WidgetM context (set via createInputs).
    - `placeholder`: Placeholder text when empty
    - `initialValue`: Initial text value
-/
def textInput (placeholder : String) (initialValue : String := "") : WidgetM TextInputResult := do
  let theme ← getThemeW
  let font ← getFontW
  let name ← registerComponentW "text-input" (isInput := true)
  let events ← getEventsW
  let focusedInput := events.registry.focusedInput
  let fireFocusedInput := events.registry.fireFocus

  let clicks ← useClick name
  let keyEvents ← useKeyboard

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
  let initialState : TextInputState := {
    value := initialValue
    cursor := initialValue.length
    cursorPixelX := 0.0
  }
  let initialState ← SpiderM.liftIO (TextInput.computeCursorPixelX font initialState)
  let textState ← Reactive.foldDynM
    (fun keyData state => SpiderM.liftIO do
      let updated := TextInput.handleKeyPress keyData.event state none
      TextInput.computeCursorPixelX font updated)
    initialState gatedKeys

  let textChanges ← Dynamic.changesM textState
  let valueChanges ← Event.mapMaybeM
    (fun (old, new) => if old.value != new.value then some new.value else none)
    textChanges
  let onChange := valueChanges

  let text ← Dynamic.mapM (·.value) textState

  -- Use dynWidget for efficient change-driven rebuilds
  let renderState ← Dynamic.zipWithM (fun s f => (s, f)) textState focusedInput
  let _ ← dynWidget renderState fun (state, focused) => do
    let isFoc := focused == some name
    emit do pure (textInputVisual name theme { state with focused := isFoc } placeholder)

  pure { onChange, onFocus, onBlur, text, isFocused }

end Afferent.Canopy

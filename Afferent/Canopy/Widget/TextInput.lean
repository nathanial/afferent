/-
  Canopy TextInput Widget
  Single-line text input with cursor and editing support.
-/
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Widget.Button

namespace Afferent.Canopy

open Afferent.Arbor

/-- Extended state for text input widgets. -/
structure TextInputState extends WidgetState where
  value : String := ""
  cursor : Nat := 0
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

end TextInputState

/-- Messages emitted by text input widget. -/
inductive TextInputMsg (Msg : Type) where
  | app (msg : Msg)
  | valueChanged (name : String) (value : String) (cursor : Nat)
  | focus (name : String) (focused : Bool)
  | hover (name : String) (hovered : Bool)
deriving Repr

namespace TextInput

/-- Custom spec for text input rendering with cursor. -/
def inputSpec (displayText : String) (placeholder : String) (showPlaceholder : Bool)
    (cursorPos : Nat) (focused : Bool) (theme : Theme) : CustomSpec := {
  measure := fun availW _ =>
    let textWidth := displayText.length.toFloat * 8  -- Approximate char width
    let minW := if textWidth < (availW - 24) then textWidth else (availW - 24)
    let width := if 200 > minW then 200 else minW
    (width, 20)
  collect := fun layout =>
    let rect := layout.contentRect
    let text := if showPlaceholder then placeholder else displayText
    let textColor := if showPlaceholder then theme.textMuted else theme.text
    let textCmd := RenderCommand.fillText text rect.x (rect.y + 14) theme.font textColor
    if focused then
      let cursorX := rect.x + cursorPos.toFloat * 8  -- Approximate
      let cursorY := rect.y + 2
      let cursorH := rect.height - 4
      let cursorCmd := RenderCommand.fillRect (Arbor.Rect.mk' cursorX cursorY 2 cursorH) theme.focusRing 0
      #[textCmd, cursorCmd]
    else
      #[textCmd]
  draw := none
}

/-- Handle key press for text input. -/
def handleKeyPress (e : KeyEvent) (state : TextInputState) (maxLen : Option Nat) : TextInputState :=
  if e.modifiers.cmd then
    match e.key with
    | .left => state.moveCursorStart
    | .right => state.moveCursorEnd
    | _ => state
  else
    match e.key with
    | .char c =>
        match maxLen with
        | some max => if state.value.length >= max then state else state.insertChar c
        | none => state.insertChar c
    | .backspace => state.deleteBackward
    | .delete => state.deleteForward
    | .left => state.moveCursorLeft
    | .right => state.moveCursorRight
    | .home => state.moveCursorStart
    | .«end» => state.moveCursorEnd
    | _ => state

end TextInput

/-- Create a text input field.
    - `name`: Unique name for state tracking
    - `onValueChange`: Message to emit when value changes
    - `theme`: Theme for styling
    - `state`: Current text input state
    - `placeholder`: Placeholder text when empty
    - `maxLength`: Optional maximum character length
-/
def textInput {Msg : Type} (name : String)
    (onValueChange : String → Msg)
    (theme : Theme)
    (state : TextInputState := {})
    (placeholder : String := "")
    (maxLength : Option Nat := none)
    : UIBuilder (TextInputMsg Msg) Widget := do
  let wid ← UIBuilder.freshId

  let colors := theme.input
  let bgColor := if state.disabled then colors.backgroundDisabled else colors.background
  let borderColor := if state.focused then colors.borderFocused else colors.border

  let style : BoxStyle := {
    backgroundColor := some bgColor
    borderColor := some borderColor
    borderWidth := if state.focused then 2 else 1
    cornerRadius := theme.cornerRadius
    padding := Trellis.EdgeInsets.symmetric theme.padding (theme.padding * 0.5)
    minWidth := some 200
    minHeight := some 40
  }

  let showPlaceholder := state.value.isEmpty && !state.focused

  -- Register event handlers
  UIBuilder.register wid fun _ event =>
    match event with
    | .mouseEnter _ => { msgs := #[.hover name true] }
    | .mouseLeave _ => { msgs := #[.hover name false] }
    | .mouseClick _ =>
        if !state.disabled then
          { msgs := #[.focus name true] }
        else {}
    | .keyPress e =>
        if state.focused && !state.disabled then
          let newState := TextInput.handleKeyPress e state maxLength
          if newState.value != state.value || newState.cursor != state.cursor then
            { msgs := #[.valueChanged name newState.value newState.cursor, .app (onValueChange newState.value)] }
          else
            { msgs := #[.valueChanged name newState.value newState.cursor] }
        else {}
    | _ => {}

  -- Build visual structure
  UIBuilder.lift do
    box style
    -- Note: For a proper implementation, we'd use a custom widget here
    -- that renders both the text and cursor. For now, we use a simple box
    -- and rely on the demo to show the value.

end Afferent.Canopy

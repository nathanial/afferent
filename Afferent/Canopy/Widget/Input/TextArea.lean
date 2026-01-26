/-
  Canopy TextArea Widget
  Multi-line text input with word wrapping and vertical scrolling.
-/
import Reactive
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Text.Font
import Afferent.Canopy.Reactive.Component

namespace Afferent.Canopy

open Afferent.Arbor hiding Event

/-- A wrapped line of text with its character range in the original string. -/
structure WrappedLine where
  /-- The text content of this line. -/
  text : String
  /-- Start index in original string. -/
  startIdx : Nat
  /-- End index in original string (exclusive). -/
  endIdx : Nat
  /-- Measured width of this line in pixels. -/
  width : Float
deriving Repr, BEq, Inhabited

/-- Pre-computed rendering state for TextArea.
    This is computed during event handling where we have Font access. -/
structure TextAreaRenderState where
  /-- Wrapped lines with measured widths. -/
  wrappedLines : Array WrappedLine := #[]
  /-- Pre-computed cursor X position in pixels (relative to line start). -/
  cursorPixelX : Float := 0
  /-- Pre-computed cursor Y position in pixels (relative to content area). -/
  cursorPixelY : Float := 0
  /-- Line height from font metrics. -/
  lineHeight : Float := 20.0
  /-- Content padding. -/
  padding : Float := 8.0
deriving Repr, BEq, Inhabited

/-- Extended state for text area widgets. -/
structure TextAreaState extends WidgetState where
  value : String := ""
  cursor : Nat := 0
  /-- Vertical scroll offset in pixels. -/
  scrollOffsetY : Float := 0
  /-- Target column for up/down navigation (preserves column when moving through shorter lines). -/
  targetColumn : Option Nat := none
  /-- Pre-computed rendering state (updated during event handling). -/
  renderState : TextAreaRenderState := {}
deriving Repr, BEq, Inhabited

namespace TextAreaState

/-- Insert a character at cursor position. -/
def insertChar (s : TextAreaState) (c : Char) : TextAreaState :=
  let before := s.value.take s.cursor
  let after := s.value.drop s.cursor
  { s with
    value := before ++ c.toString ++ after
    cursor := s.cursor + 1
    targetColumn := none }

/-- Delete character before cursor (backspace). -/
def deleteBackward (s : TextAreaState) : TextAreaState :=
  if s.cursor > 0 then
    let before := s.value.take (s.cursor - 1)
    let after := s.value.drop s.cursor
    { s with
      value := before ++ after
      cursor := s.cursor - 1
      targetColumn := none }
  else s

/-- Delete character at cursor (delete key). -/
def deleteForward (s : TextAreaState) : TextAreaState :=
  if s.cursor < s.value.length then
    let before := s.value.take s.cursor
    let after := s.value.drop (s.cursor + 1)
    { s with value := before ++ after, targetColumn := none }
  else s

/-- Move cursor left. -/
def moveCursorLeft (s : TextAreaState) : TextAreaState :=
  if s.cursor > 0 then { s with cursor := s.cursor - 1, targetColumn := none }
  else s

/-- Move cursor right. -/
def moveCursorRight (s : TextAreaState) : TextAreaState :=
  if s.cursor < s.value.length then { s with cursor := s.cursor + 1, targetColumn := none }
  else s

/-- Move cursor to start of text. -/
def moveCursorStart (s : TextAreaState) : TextAreaState :=
  { s with cursor := 0, targetColumn := none }

/-- Move cursor to end of text. -/
def moveCursorEnd (s : TextAreaState) : TextAreaState :=
  { s with cursor := s.value.length, targetColumn := none }

end TextAreaState

namespace TextArea

/-- Find the cursor's position within wrapped lines.
    Returns (lineIndex, columnInLine). -/
def cursorToLineCol (cursor : Nat) (lines : Array WrappedLine) : Nat × Nat :=
  let rec findLine (idx : Nat) : Nat × Nat :=
    if idx >= lines.size then
      -- Cursor is at the very end
      if lines.size > 0 then
        let lastLine := lines[lines.size - 1]!
        (lines.size - 1, cursor - lastLine.startIdx)
      else
        (0, cursor)
    else
      let line := lines[idx]!
      if cursor >= line.startIdx && cursor < line.endIdx then
        (idx, cursor - line.startIdx)
      else if cursor == line.endIdx && idx == lines.size - 1 then
        -- Cursor at very end of last line
        (idx, cursor - line.startIdx)
      else
        findLine (idx + 1)
  findLine 0

/-- Convert (lineIndex, column) back to flat cursor index. -/
def lineColToCursor (lineIdx : Nat) (col : Nat) (lines : Array WrappedLine) : Nat :=
  if lineIdx >= lines.size then
    if lines.size > 0 then
      let lastLine := lines[lines.size - 1]!
      lastLine.endIdx
    else
      0
  else
    let line := lines[lineIdx]!
    let maxCol := line.endIdx - line.startIdx
    line.startIdx + min col maxCol

/-- Move cursor up one line. -/
def moveCursorUp (s : TextAreaState) (lines : Array WrappedLine) : TextAreaState :=
  let (lineIdx, col) := cursorToLineCol s.cursor lines
  if lineIdx == 0 then
    -- Already at top, move to start of line
    { s with cursor := lineColToCursor 0 0 lines, targetColumn := none }
  else
    let targetCol := s.targetColumn.getD col
    let newCursor := lineColToCursor (lineIdx - 1) targetCol lines
    { s with cursor := newCursor, targetColumn := some targetCol }

/-- Move cursor down one line. -/
def moveCursorDown (s : TextAreaState) (lines : Array WrappedLine) : TextAreaState :=
  let (lineIdx, col) := cursorToLineCol s.cursor lines
  if lineIdx >= lines.size - 1 then
    -- Already at bottom, move to end of line
    let lastLine := lines[lines.size - 1]!
    { s with cursor := lastLine.endIdx, targetColumn := none }
  else
    let targetCol := s.targetColumn.getD col
    let newCursor := lineColToCursor (lineIdx + 1) targetCol lines
    { s with cursor := newCursor, targetColumn := some targetCol }

/-- Move cursor to start of current line. -/
def moveCursorLineStart (s : TextAreaState) (lines : Array WrappedLine) : TextAreaState :=
  let (lineIdx, _) := cursorToLineCol s.cursor lines
  { s with cursor := lineColToCursor lineIdx 0 lines, targetColumn := none }

/-- Move cursor to end of current line. -/
def moveCursorLineEnd (s : TextAreaState) (lines : Array WrappedLine) : TextAreaState :=
  let (lineIdx, _) := cursorToLineCol s.cursor lines
  if lineIdx < lines.size then
    let line := lines[lineIdx]!
    -- End of line is before the newline character (if any)
    let endCol := if line.text.isEmpty then 0 else line.text.length
    { s with cursor := lineColToCursor lineIdx endCol lines, targetColumn := none }
  else
    s

/-- Ensure cursor is visible by adjusting scroll offset. -/
def scrollToCursor (s : TextAreaState) (viewportHeight : Float) : TextAreaState :=
  let cursorY := s.renderState.cursorPixelY
  let lineHeight := s.renderState.lineHeight
  let cursorBottom := cursorY + lineHeight

  let newScrollY :=
    if cursorY < s.scrollOffsetY then
      -- Cursor above viewport - scroll up
      cursorY
    else if cursorBottom > s.scrollOffsetY + viewportHeight then
      -- Cursor below viewport - scroll down
      cursorBottom - viewportHeight
    else
      s.scrollOffsetY

  { s with scrollOffsetY := max 0 newScrollY }

/-- Handle key press for text area.
    Returns updated state (without render state - call computeRenderState after). -/
def handleKeyPress (e : KeyEvent) (state : TextAreaState) (maxLen : Option Nat := none) : TextAreaState :=
  let lines := state.renderState.wrappedLines
  if e.modifiers.cmd then
    match e.key with
    | .left => moveCursorLineStart state lines
    | .right => moveCursorLineEnd state lines
    | .up => state.moveCursorStart
    | .down => state.moveCursorEnd
    | _ => state
  else
    match e.key with
    | .char c =>
        match maxLen with
        | some max => if state.value.length >= max then state else state.insertChar c
        | none => state.insertChar c
    | .space =>
        match maxLen with
        | some max => if state.value.length >= max then state else state.insertChar ' '
        | none => state.insertChar ' '
    | .enter =>
        match maxLen with
        | some max => if state.value.length >= max then state else state.insertChar '\n'
        | none => state.insertChar '\n'
    | .backspace => state.deleteBackward
    | .delete => state.deleteForward
    | .left => state.moveCursorLeft
    | .right => state.moveCursorRight
    | .up => moveCursorUp state lines
    | .down => moveCursorDown state lines
    | .home => moveCursorLineStart state lines
    | .«end» => moveCursorLineEnd state lines
    | _ => state

/-- Wrap text into lines that fit within maxWidth using actual font measurements.
    This is an IO function because it needs to measure text. -/
def wrapTextMeasured (font : Afferent.Font) (text : String) (maxWidth : Float)
    : IO (Array WrappedLine) := do
  if text.isEmpty then
    return #[{ text := "", startIdx := 0, endIdx := 0, width := 0 }]

  let mut result : Array WrappedLine := #[]
  let mut currentIdx : Nat := 0
  let chars := text.toList

  while currentIdx < chars.length do
    -- Find end of current line (either newline or need to wrap)
    let mut lineEnd := currentIdx
    let mut lastWordEnd := currentIdx
    let mut lastWordWidth : Float := 0

    while lineEnd < chars.length do
      let c := chars[lineEnd]!

      -- Hard newline - end line here
      if c == '\n' then
        break

      -- Measure the current line segment
      let lineChars := chars.drop currentIdx |>.take (lineEnd - currentIdx + 1)
      let lineText := String.ofList lineChars
      let (lineWidth, _) ← font.measureText lineText

      -- Check if adding this char exceeds width
      if lineWidth > maxWidth && lineEnd > currentIdx then
        -- Need to wrap - prefer wrapping at word boundary
        if lastWordEnd > currentIdx then
          lineEnd := lastWordEnd
        break

      -- Track word boundaries (space marks end of word)
      if c == ' ' then
        lastWordEnd := lineEnd + 1
        lastWordWidth := lineWidth

      lineEnd := lineEnd + 1

    -- Extract the line text and measure it
    let lineChars := chars.drop currentIdx |>.take (lineEnd - currentIdx)
    let lineText := String.ofList lineChars
    let (lineWidth, _) ← font.measureText lineText

    -- Handle newline character
    let nextIdx := if lineEnd < chars.length && chars[lineEnd]! == '\n'
      then lineEnd + 1
      else lineEnd

    result := result.push {
      text := lineText
      startIdx := currentIdx
      endIdx := nextIdx
      width := lineWidth
    }

    currentIdx := nextIdx

  -- Ensure at least one empty line if text ends with newline
  if chars.length > 0 && chars[chars.length - 1]! == '\n' then
    result := result.push {
      text := ""
      startIdx := chars.length
      endIdx := chars.length
      width := 0
    }

  return result

/-- Compute the pre-computed rendering state for a TextArea.
    This must be called during event handling (where we have Font access)
    after any change to the text value or cursor position.
    - `font`: The font used for text rendering
    - `state`: The current text area state
    - `contentWidth`: Available width for text content (widget width - padding * 2)
    - `padding`: Content padding in pixels -/
def computeRenderState (font : Afferent.Font) (state : TextAreaState)
    (contentWidth : Float) (padding : Float := 8.0) : IO TextAreaState := do
  -- Get font metrics
  let lineHeight := font.lineHeight

  -- Wrap text with actual measurements
  let wrappedLines ← wrapTextMeasured font state.value contentWidth

  -- Find cursor position
  let (lineIdx, colInLine) := cursorToLineCol state.cursor wrappedLines

  -- Measure text before cursor on the current line to get exact X position
  let cursorPixelX ← do
    if lineIdx < wrappedLines.size then
      let line := wrappedLines[lineIdx]!
      let textBeforeCursor := line.text.take colInLine
      let (width, _) ← font.measureText textBeforeCursor
      pure width
    else
      pure 0.0

  -- Calculate cursor Y position
  let cursorPixelY := lineIdx.toFloat * lineHeight

  let renderState : TextAreaRenderState := {
    wrappedLines
    cursorPixelX
    cursorPixelY
    lineHeight
    padding
  }

  return { state with renderState }

/-- Custom spec for text area rendering with multi-line text and cursor. -/
def areaSpec (renderState : TextAreaRenderState) (placeholder : String) (showPlaceholder : Bool)
    (scrollOffsetY : Float) (focused : Bool) (theme : Theme)
    (viewportWidth viewportHeight : Float) : CustomSpec := {
  measure := fun _ _ =>
    -- Return fixed viewport dimensions (container handles actual sizing)
    (viewportWidth, viewportHeight)
  collect := fun layout =>
    let rect := layout.contentRect
    let lineHeight := renderState.lineHeight
    let lines := renderState.wrappedLines

    -- Use actual font ascender for baseline positioning
    let ascender := theme.font.ascender

    RenderM.build do
      -- Clip to viewport
      let clipRect := Arbor.Rect.mk' rect.x rect.y rect.width viewportHeight
      RenderM.pushClip clipRect

      if showPlaceholder then
        -- Render placeholder
        let textY := rect.y + ascender
        RenderM.fillText placeholder rect.x textY theme.font theme.textMuted
      else
        -- Render each visible line
        for i in [:lines.size] do
          match lines[i]? with
          | some line =>
            let lineY := rect.y + i.toFloat * lineHeight - scrollOffsetY
            -- Only render if line is visible
            if lineY + lineHeight >= rect.y && lineY < rect.y + viewportHeight then
              let textY := lineY + ascender  -- Baseline position from actual font metrics
              RenderM.fillText line.text rect.x textY theme.font theme.text
          | none => pure ()

      -- Render cursor if focused
      if focused then
        let cursorScreenX := rect.x + renderState.cursorPixelX
        let cursorScreenY := rect.y + renderState.cursorPixelY - scrollOffsetY
        -- Only render cursor if visible
        if cursorScreenY + lineHeight >= rect.y && cursorScreenY < rect.y + viewportHeight then
          let cursorRect := Arbor.Rect.mk' cursorScreenX cursorScreenY 2 lineHeight
          RenderM.fillRect cursorRect theme.focusRing 0

      RenderM.popClip
  draw := none
}

end TextArea

/-- Build the visual representation of a text area.
    - `name`: Widget name for hit testing
    - `theme`: Theme for styling
    - `state`: Current text area state (must have pre-computed renderState)
    - `placeholder`: Placeholder text when empty
    - `width`: Widget width in pixels
    - `height`: Widget height in pixels (viewport height)
-/
def textAreaVisual (name : String) (theme : Theme)
    (state : TextAreaState) (placeholder : String := "")
    (width : Float := 300) (height : Float := 150) : WidgetBuilder := do
  let colors := theme.input
  let bgColor := if state.disabled then colors.backgroundDisabled else colors.background
  let borderColor := if state.focused then colors.borderFocused else colors.border
  let padding := state.renderState.padding

  let style : BoxStyle := {
    backgroundColor := some bgColor
    borderColor := some borderColor
    borderWidth := if state.focused then 2 else 1
    cornerRadius := theme.cornerRadius
    padding := Trellis.EdgeInsets.uniform padding
    minWidth := some width
    minHeight := some height
    maxHeight := some height
  }

  let contentWidth := width - padding * 2
  let contentHeight := height - padding * 2
  let showPlaceholder := state.value.isEmpty && !state.focused

  let wid ← freshId
  let props : Trellis.FlexContainer := {
    direction := .column
    justifyContent := .flexStart
    alignItems := .stretch
  }

  let child ← custom (TextArea.areaSpec state.renderState placeholder showPlaceholder
      state.scrollOffsetY state.focused theme contentWidth contentHeight) {
    width := .length contentWidth
  }

  pure (.flex wid (some name) props style #[child])

/-! ## Reactive TextArea Components (FRP-based)

These use WidgetM for declarative composition with automatic focus and keyboard handling.
-/

open Reactive Reactive.Host
open Afferent.Canopy.Reactive

/-- TextArea result - events and dynamics. -/
structure TextAreaResult where
  onChange : Reactive.Event Spider String
  onFocus : Reactive.Event Spider Unit
  onBlur : Reactive.Event Spider Unit
  text : Reactive.Dynamic Spider String
  isFocused : Reactive.Dynamic Spider Bool

/-- Create a reactive text area component using WidgetM.
    Emits the text area widget and returns text state.
    Uses the default font from WidgetM context (set via createInputs).
    - `placeholder`: Placeholder text when empty
    - `initialState`: Initial text area state
    - `width`: Width of the text area
    - `height`: Height of the text area
-/
def textArea (placeholder : String) (initialState : TextAreaState)
    (width : Float := 280) (height : Float := 120) : WidgetM TextAreaResult := do
  let theme ← getThemeW
  let font ← getFontW
  let name ← registerComponentW "text-area" (isInput := true)
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
  let padding : Float := 8.0
  let contentWidth := width - padding * 2
  let viewportHeight := height - padding * 2
  let textState ← Reactive.foldDynM
    (fun keyData state => do
      let updated := TextArea.handleKeyPress keyData.event state none
      let renderedState ← TextArea.computeRenderState font updated contentWidth padding
      pure (TextArea.scrollToCursor renderedState viewportHeight))
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
    emit do pure (textAreaVisual name theme { state with focused := isFoc } placeholder width height)

  pure { onChange, onFocus, onBlur, text, isFocused }

end Afferent.Canopy

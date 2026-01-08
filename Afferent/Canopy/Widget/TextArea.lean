/-
  Canopy TextArea Widget
  Multi-line text input with word wrapping and vertical scrolling.
-/
import Afferent.Canopy.Core
import Afferent.Canopy.Theme

namespace Afferent.Canopy

open Afferent.Arbor

/-- A wrapped line of text with its character range in the original string. -/
structure WrappedLine where
  /-- The text content of this line. -/
  text : String
  /-- Start index in original string. -/
  startIdx : Nat
  /-- End index in original string (exclusive). -/
  endIdx : Nat
  /-- Width of this line in pixels. -/
  width : Float
deriving Repr, BEq, Inhabited

/-- Extended state for text area widgets. -/
structure TextAreaState extends WidgetState where
  value : String := ""
  cursor : Nat := 0
  /-- Vertical scroll offset in pixels. -/
  scrollOffsetY : Float := 0
  /-- Target column for up/down navigation (preserves column when moving through shorter lines). -/
  targetColumn : Option Nat := none
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

/-- Default dimensions for text area. -/
structure Dimensions where
  charWidth : Float := 8.0  -- Approximate character width
  lineHeight : Float := 20.0
  padding : Float := 8.0
deriving Repr, Inhabited

def defaultDimensions : Dimensions := {}

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

/-- Get pixel position (x, y) for cursor rendering.
    Returns (pixelX, pixelY) relative to content area. -/
def cursorPixelPosition (cursor : Nat) (lines : Array WrappedLine)
    (dims : Dimensions := defaultDimensions) : Float × Float :=
  let (lineIdx, col) := cursorToLineCol cursor lines
  let x := col.toFloat * dims.charWidth
  let y := lineIdx.toFloat * dims.lineHeight
  (x, y)

/-- Wrap text into lines that fit within maxWidth.
    Handles both hard newlines and soft word wrapping. -/
def wrapText (text : String) (maxWidth : Float)
    (dims : Dimensions := defaultDimensions) : Array WrappedLine := Id.run do
  if text.isEmpty then
    return #[{ text := "", startIdx := 0, endIdx := 0, width := 0 }]

  let mut result : Array WrappedLine := #[]
  let mut currentIdx : Nat := 0
  let chars := text.toList

  while currentIdx < chars.length do
    -- Find end of current line (either newline or need to wrap)
    let mut lineEnd := currentIdx
    let mut lastWordEnd := currentIdx
    let mut lineWidth : Float := 0

    while lineEnd < chars.length do
      let c := chars[lineEnd]!

      -- Hard newline - end line here
      if c == '\n' then
        break

      -- Check if adding this char exceeds width
      let charWidth := dims.charWidth
      if lineWidth + charWidth > maxWidth && lineEnd > currentIdx then
        -- Need to wrap - prefer wrapping at word boundary
        if lastWordEnd > currentIdx then
          lineEnd := lastWordEnd
        break

      lineWidth := lineWidth + charWidth

      -- Track word boundaries (space marks end of word)
      if c == ' ' then
        lastWordEnd := lineEnd + 1

      lineEnd := lineEnd + 1

    -- Extract the line text
    let lineChars := chars.drop currentIdx |>.take (lineEnd - currentIdx)
    let lineText := String.ofList lineChars
    let finalLineWidth := lineChars.length.toFloat * dims.charWidth

    -- Handle newline character
    let nextIdx := if lineEnd < chars.length && chars[lineEnd]! == '\n'
      then lineEnd + 1
      else lineEnd

    result := result.push {
      text := lineText
      startIdx := currentIdx
      endIdx := nextIdx
      width := finalLineWidth
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

  result

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
def scrollToCursor (s : TextAreaState) (lines : Array WrappedLine)
    (viewportHeight : Float) (dims : Dimensions := defaultDimensions) : TextAreaState :=
  let (_, cursorY) := cursorPixelPosition s.cursor lines dims
  let cursorBottom := cursorY + dims.lineHeight

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

/-- Handle key press for text area. -/
def handleKeyPress (e : KeyEvent) (state : TextAreaState)
    (lines : Array WrappedLine) (maxLen : Option Nat := none) : TextAreaState :=
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

/-- Custom spec for text area rendering with multi-line text and cursor. -/
def areaSpec (lines : Array WrappedLine) (placeholder : String) (showPlaceholder : Bool)
    (cursor : Nat) (scrollOffsetY : Float) (focused : Bool) (theme : Theme)
    (viewportHeight : Float) (dims : Dimensions := defaultDimensions) : CustomSpec := {
  measure := fun availW _ =>
    let contentHeight := lines.size.toFloat * dims.lineHeight
    (availW, contentHeight)
  collect := fun layout =>
    let rect := layout.contentRect

    -- Clip to viewport
    let clipRect := Arbor.Rect.mk' rect.x rect.y rect.width viewportHeight
    let clipCmd := RenderCommand.pushClip clipRect

    let textCmds := if showPlaceholder then
      -- Render placeholder
      let textY := rect.y + dims.lineHeight * 0.8
      #[RenderCommand.fillText placeholder rect.x textY theme.font theme.textMuted]
    else
      -- Render each visible line
      let indices := Array.range lines.size
      indices.filterMap fun i =>
        match lines[i]? with
        | some line =>
          let lineY := rect.y + i.toFloat * dims.lineHeight - scrollOffsetY
          -- Only render if line is visible
          if lineY + dims.lineHeight >= rect.y && lineY < rect.y + viewportHeight then
            let textY := lineY + dims.lineHeight * 0.8  -- Baseline position
            some (RenderCommand.fillText line.text rect.x textY theme.font theme.text)
          else
            none
        | none => none

    -- Render cursor if focused
    let cursorCmd := if focused then
      let (cursorX, cursorY) := cursorPixelPosition cursor lines dims
      let cursorScreenX := rect.x + cursorX
      let cursorScreenY := rect.y + cursorY - scrollOffsetY
      -- Only render cursor if visible
      if cursorScreenY + dims.lineHeight >= rect.y && cursorScreenY < rect.y + viewportHeight then
        let cursorRect := Arbor.Rect.mk' cursorScreenX cursorScreenY 2 dims.lineHeight
        #[RenderCommand.fillRect cursorRect theme.focusRing 0]
      else
        #[]
    else
      #[]

    #[clipCmd] ++ textCmds ++ cursorCmd ++ #[RenderCommand.popClip]
  draw := none
}

end TextArea

/-- Build the visual representation of a text area.
    - `name`: Widget name for hit testing
    - `theme`: Theme for styling
    - `state`: Current text area state
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
  let dims := TextArea.defaultDimensions

  let style : BoxStyle := {
    backgroundColor := some bgColor
    borderColor := some borderColor
    borderWidth := if state.focused then 2 else 1
    cornerRadius := theme.cornerRadius
    padding := Trellis.EdgeInsets.uniform dims.padding
    minWidth := some width
    minHeight := some height
    maxHeight := some height
  }

  let contentWidth := width - dims.padding * 2
  let contentHeight := height - dims.padding * 2
  let showPlaceholder := state.value.isEmpty && !state.focused
  let lines := TextArea.wrapText state.value contentWidth dims

  let wid ← freshId
  let props : Trellis.FlexContainer := {
    direction := .column
    justifyContent := .flexStart
    alignItems := .stretch
  }

  let child ← custom (TextArea.areaSpec lines placeholder showPlaceholder
      state.cursor state.scrollOffsetY state.focused theme contentHeight dims) {
    width := .length contentWidth
  }

  pure (.flex wid (some name) props style #[child])

end Afferent.Canopy

/-
  Afferent Widget Text Layout
  Text wrapping algorithm using word-by-word measurement.
-/
import Afferent.Widget.Core

namespace Afferent.Widget

/-- Token type for text tokenization. -/
inductive Token where
  | word (text : String)
  | space
  | newline
deriving Repr, BEq

namespace Token

def text : Token → String
  | .word t => t
  | .space => " "
  | .newline => "\n"

def isNewline : Token → Bool
  | .newline => true
  | _ => false

end Token

/-- Tokenize text into words, spaces, and newlines.
    Words are sequences of non-space, non-newline characters.
    Spaces are preserved individually.
    Newlines are explicit line breaks. -/
def tokenize (text : String) : Array Token := Id.run do
  let mut tokens : Array Token := #[]
  let mut currentWord := ""

  for c in text.toList do
    if c == '\n' then
      -- Flush current word
      if !currentWord.isEmpty then
        tokens := tokens.push (.word currentWord)
        currentWord := ""
      tokens := tokens.push .newline
    else if c == ' ' then
      -- Flush current word
      if !currentWord.isEmpty then
        tokens := tokens.push (.word currentWord)
        currentWord := ""
      tokens := tokens.push .space
    else
      currentWord := currentWord.push c

  -- Flush final word
  if !currentWord.isEmpty then
    tokens := tokens.push (.word currentWord)

  tokens

/-- Wrap text to fit within maxWidth using word-by-word measurement.
    Returns a TextLayout with wrapped lines and metrics. -/
def wrapText (font : Font) (text : String) (maxWidth : Float) : IO TextLayout := do
  -- Empty text case
  if text.isEmpty then
    return TextLayout.empty

  let glyphHeight := font.glyphHeight
  let lineAdvance := max font.lineHeight glyphHeight

  -- No wrapping case (maxWidth <= 0 means single line)
  if maxWidth <= 0 then
    let (w, h) ← font.measureText text
    return TextLayout.singleLine text w (max h glyphHeight)

  let tokens := tokenize text

  let mut lines : Array TextLine := #[]
  let mut currentLineText := ""
  let mut currentLineWidth : Float := 0
  let mut maxLineWidth : Float := 0

  for token in tokens do
    match token with
    | .newline =>
      -- Explicit line break - emit current line
      let lineText := currentLineText.trimRight
      let (lineWidth, _) ← font.measureText lineText
      lines := lines.push ⟨lineText, lineWidth⟩
      maxLineWidth := max maxLineWidth lineWidth
      currentLineText := ""
      currentLineWidth := 0

    | .space =>
      -- Space token - try to add to current line
      if currentLineText.isEmpty then
        -- Skip leading spaces on new line
        pure ()
      else
        let (spaceWidth, _) ← font.measureText " "
        let newWidth := currentLineWidth + spaceWidth
        if newWidth <= maxWidth then
          currentLineText := currentLineText ++ " "
          currentLineWidth := newWidth
        else
          -- Space would overflow, emit line and skip the space
          let lineText := currentLineText.trimRight
          let (lineWidth, _) ← font.measureText lineText
          lines := lines.push ⟨lineText, lineWidth⟩
          maxLineWidth := max maxLineWidth lineWidth
          currentLineText := ""
          currentLineWidth := 0

    | .word w =>
      let (wordWidth, _) ← font.measureText w

      if currentLineText.isEmpty then
        -- First word on line - always add it (even if it exceeds maxWidth)
        currentLineText := w
        currentLineWidth := wordWidth
      else
        -- Check if word fits on current line
        let newWidth := currentLineWidth + wordWidth
        if newWidth <= maxWidth then
          currentLineText := currentLineText ++ w
          currentLineWidth := newWidth
        else
          -- Word doesn't fit - emit current line and start new one
          let lineText := currentLineText.trimRight
          let (lineWidth, _) ← font.measureText lineText
          lines := lines.push ⟨lineText, lineWidth⟩
          maxLineWidth := max maxLineWidth lineWidth
          currentLineText := w
          currentLineWidth := wordWidth

  -- Emit final line if non-empty
  if !currentLineText.isEmpty then
    let finalText := currentLineText.trimRight
    let (finalWidth, _) ← font.measureText finalText
    lines := lines.push ⟨finalText, finalWidth⟩
    maxLineWidth := max maxLineWidth finalWidth

  -- Handle case where text was all spaces/empty
  if lines.isEmpty then
    return TextLayout.empty

  return {
    lines := lines
    totalHeight :=
      if lines.size == 0 then 0
      else glyphHeight + lineAdvance * (lines.size - 1).toFloat
    maxWidth := maxLineWidth
  }

/-- Measure text without wrapping (single line). -/
def measureSingleLine (font : Font) (text : String) : IO TextLayout := do
  if text.isEmpty then
    return TextLayout.empty
  let (w, h) ← font.measureText text
  let glyphHeight := font.glyphHeight
  return TextLayout.singleLine text w (max h glyphHeight)

end Afferent.Widget

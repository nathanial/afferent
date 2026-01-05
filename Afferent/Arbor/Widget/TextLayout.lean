/-
  Arbor Widget Text Layout
  Text wrapping algorithm using word-by-word measurement.
  Uses the TextMeasurer typeclass for backend independence.
-/
import Afferent.Arbor.Widget.Core
import Afferent.Arbor.Core.TextMeasurer

namespace Afferent.Arbor

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
    Returns a TextLayout with wrapped lines and metrics.
    Uses the TextMeasurer typeclass for backend independence. -/
def wrapText {M : Type → Type} [Monad M] [TextMeasurer M] (font : FontId) (text : String)
    (maxWidth : Float) : M TextLayout := do
  -- Empty text case
  if text.isEmpty then
    return TextLayout.empty

  let metrics ← TextMeasurer.fontMetrics font
  let glyphHeight := metrics.height
  let lineAdvance := max metrics.lineHeight glyphHeight

  -- No wrapping case (maxWidth <= 0 means single line)
  if maxWidth <= 0 then
    let m ← TextMeasurer.measureText text font
    return TextLayout.singleLine text m.width (max m.height glyphHeight)

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
      let m ← TextMeasurer.measureText lineText font
      lines := lines.push ⟨lineText, m.width⟩
      maxLineWidth := max maxLineWidth m.width
      currentLineText := ""
      currentLineWidth := 0

    | .space =>
      -- Space token - try to add to current line
      if currentLineText.isEmpty then
        -- Skip leading spaces on new line
        pure ()
      else
        let spaceWidth ← TextMeasurer.measureChar ' ' font
        let newWidth := currentLineWidth + spaceWidth
        if newWidth <= maxWidth then
          currentLineText := currentLineText ++ " "
          currentLineWidth := newWidth
        else
          -- Space would overflow, emit line and skip the space
          let lineText := currentLineText.trimRight
          let m ← TextMeasurer.measureText lineText font
          lines := lines.push ⟨lineText, m.width⟩
          maxLineWidth := max maxLineWidth m.width
          currentLineText := ""
          currentLineWidth := 0

    | .word w =>
      let wordMetrics ← TextMeasurer.measureText w font
      let wordWidth := wordMetrics.width

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
          let m ← TextMeasurer.measureText lineText font
          lines := lines.push ⟨lineText, m.width⟩
          maxLineWidth := max maxLineWidth m.width
          currentLineText := w
          currentLineWidth := wordWidth

  -- Emit final line if non-empty
  if !currentLineText.isEmpty then
    let finalText := currentLineText.trimRight
    let m ← TextMeasurer.measureText finalText font
    lines := lines.push ⟨finalText, m.width⟩
    maxLineWidth := max maxLineWidth m.width

  -- Handle case where text was all spaces/empty
  if lines.isEmpty then
    return TextLayout.empty

  return {
    lines := lines
    totalHeight :=
      if lines.size == 0 then 0
      else glyphHeight + lineAdvance * (lines.size - 1).toFloat
    maxWidth := maxLineWidth
    lineHeight := lineAdvance
    ascender := metrics.ascender
  }

/-- Measure text without wrapping (single line). -/
def measureSingleLine {M : Type → Type} [Monad M] [TextMeasurer M] (font : FontId) (text : String) : M TextLayout := do
  if text.isEmpty then
    return TextLayout.empty
  let m ← TextMeasurer.measureText text font
  let metrics ← TextMeasurer.fontMetrics font
  let height := max m.height metrics.height
  return { lines := #[⟨text, m.width⟩], totalHeight := height, maxWidth := m.width, lineHeight := height, ascender := metrics.ascender }

end Afferent.Arbor

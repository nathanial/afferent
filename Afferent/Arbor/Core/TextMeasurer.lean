/-
  Arbor Text Measurer
  Typeclass for measuring text dimensions.
  Different backends (FreeType, terminal, etc.) provide implementations.
-/
import Afferent.Arbor.Core.Types

namespace Afferent.Arbor

/-- Result of measuring text. -/
structure TextMetrics where
  /-- Width of the text in pixels. -/
  width : Float
  /-- Height of the text in pixels. -/
  height : Float
  /-- Distance from baseline to top of tallest glyph. -/
  ascender : Float
  /-- Distance from baseline to bottom of lowest glyph (typically negative). -/
  descender : Float
  /-- Recommended line height. -/
  lineHeight : Float
deriving Repr, BEq, Inhabited

namespace TextMetrics

def zero : TextMetrics := ⟨0, 0, 0, 0, 0⟩

/-- Create simple metrics with just width and height. -/
def simple (width height : Float) : TextMetrics :=
  ⟨width, height, height * 0.8, height * 0.2, height⟩

end TextMetrics

/-- Typeclass for measuring text.
    Backends implement this to provide actual text measurement. -/
class TextMeasurer (M : Type → Type) where
  /-- Measure a single line of text. -/
  measureText : String → FontId → M TextMetrics

  /-- Measure the width of a single character. -/
  measureChar : Char → FontId → M Float

  /-- Get font metrics (ascender, descender, line height) without specific text. -/
  fontMetrics : FontId → M TextMetrics

/-- A simple fixed-width text measurer for testing.
    Assumes each character is a fixed width. -/
structure FixedWidthMeasurer where
  charWidth : Float := 8.0
  charHeight : Float := 16.0
deriving Repr, Inhabited

namespace FixedWidthMeasurer

def measureText (m : FixedWidthMeasurer) (text : String) (_font : FontId) : TextMetrics :=
  let width := text.length.toFloat * m.charWidth
  let height := m.charHeight
  ⟨width, height, height * 0.75, height * 0.25, height⟩

def measureChar (m : FixedWidthMeasurer) (_c : Char) (_font : FontId) : Float :=
  m.charWidth

def fontMetrics (m : FixedWidthMeasurer) (_font : FontId) : TextMetrics :=
  ⟨0, m.charHeight, m.charHeight * 0.75, m.charHeight * 0.25, m.charHeight⟩

end FixedWidthMeasurer

/-- Identity monad instance for FixedWidthMeasurer. -/
instance : TextMeasurer (fun α => FixedWidthMeasurer → α) where
  measureText text font m := m.measureText text font
  measureChar c font m := m.measureChar c font
  fontMetrics font m := m.fontMetrics font

/-- A text measurer that works in IO, wrapping a pure measurer. -/
def pureTextMeasurer (m : FixedWidthMeasurer) : TextMeasurer IO where
  measureText text font := pure (m.measureText text font)
  measureChar c font := pure (m.measureChar c font)
  fontMetrics font := pure (m.fontMetrics font)

end Afferent.Arbor

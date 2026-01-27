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

end Afferent.Arbor

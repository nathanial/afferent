/-
  Afferent Font
  High-level font loading and text rendering API.
-/
import Afferent.Core.Types
import Afferent.FFI.Metal

namespace Afferent

/-- Font metrics (ascender, descender, line height). -/
structure FontMetrics where
  ascender : Float
  descender : Float
  lineHeight : Float
deriving Repr

/-- A loaded font with cached metrics. -/
structure Font where
  handle : FFI.Font
  size : UInt32
  metrics : FontMetrics

namespace Font

/-- Load a font from a file path at a given size (in pixels). -/
def load (path : String) (size : UInt32) : IO Font := do
  let handle ← FFI.Font.load path size
  let (ascender, descender, lineHeight) ← FFI.Font.getMetrics handle
  pure {
    handle
    size
    metrics := { ascender, descender, lineHeight }
  }

/-- Destroy a font and free resources. -/
def destroy (font : Font) : IO Unit :=
  FFI.Font.destroy font.handle

/-- Get the font's metrics. -/
def getMetrics (font : Font) : FontMetrics :=
  font.metrics

/-- Get the font's ascender (distance from baseline to top of highest glyph). -/
def ascender (font : Font) : Float :=
  font.metrics.ascender

/-- Get the font's descender (distance from baseline to bottom of lowest glyph, usually negative). -/
def descender (font : Font) : Float :=
  font.metrics.descender

/-- Get the font's line height (recommended vertical distance between baselines). -/
def lineHeight (font : Font) : Float :=
  font.metrics.lineHeight

/-- Approximate glyph bounding-box height for a single line (ascender - descender). -/
def glyphHeight (font : Font) : Float :=
  font.metrics.ascender - font.metrics.descender

/-- Measure the dimensions of a text string. Returns (width, height). -/
def measureText (font : Font) (text : String) : IO (Float × Float) :=
  FFI.Text.measure font.handle text

end Font

end Afferent

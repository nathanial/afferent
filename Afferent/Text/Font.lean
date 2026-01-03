/-
  Afferent Font
  High-level font loading and text rendering API.
-/
import Afferent.Core.Types
import Afferent.FFI

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

/-! ## System Font Loading -/

/-- Known system font paths on macOS. -/
private def systemFontPaths : List (String × String) :=
  [ -- Monospace fonts
    ("monaco", "/System/Library/Fonts/Monaco.ttf"),
    ("menlo", "/System/Library/Fonts/Menlo.ttc"),
    ("courier", "/System/Library/Fonts/Courier.ttc"),
    ("couriernew", "/Library/Fonts/Courier New.ttf"),
    -- Sans-serif fonts
    ("helvetica", "/System/Library/Fonts/Helvetica.ttc"),
    ("helveticaneue", "/System/Library/Fonts/HelveticaNeue.ttc"),
    ("arial", "/Library/Fonts/Arial.ttf"),
    ("arialunicode", "/Library/Fonts/Arial Unicode.ttf"),
    ("verdana", "/Library/Fonts/Verdana.ttf"),
    ("tahoma", "/Library/Fonts/Tahoma.ttf"),
    ("trebuchet", "/Library/Fonts/Trebuchet MS.ttf"),
    ("lucidagrande", "/System/Library/Fonts/LucidaGrande.ttc"),
    -- Serif fonts
    ("times", "/System/Library/Fonts/Times.ttc"),
    ("timesnewroman", "/Library/Fonts/Times New Roman.ttf"),
    ("georgia", "/Library/Fonts/Georgia.ttf"),
    ("palatino", "/System/Library/Fonts/Palatino.ttc"),
    -- Generic family aliases
    ("monospace", "/System/Library/Fonts/Monaco.ttf"),
    ("sans-serif", "/System/Library/Fonts/Helvetica.ttc"),
    ("serif", "/System/Library/Fonts/Times.ttc"),
    ("system", "/System/Library/Fonts/SFNS.ttf"),
    ("system-ui", "/System/Library/Fonts/SFNS.ttf")
  ]

/-- Normalize a font name for lookup (lowercase, remove spaces and dashes). -/
private def normalizeFontName (name : String) : String :=
  name.toLower.replace " " "" |>.replace "-" ""

/-- Look up a font path by name. Returns none if not found. -/
def findSystemFont (name : String) : Option String :=
  let normalized := normalizeFontName name
  systemFontPaths.find? (fun (n, _) => n == normalized) |>.map (·.2)

/-- Load a system font by name.
    Supports common font names like "Monaco", "Helvetica", "Times New Roman".
    Also supports generic family names: "monospace", "sans-serif", "serif", "system-ui".

    Example:
    ```lean
    let font ← Font.loadSystem "Monaco" 16
    let font ← Font.loadSystem "monospace" 24
    ```
-/
def loadSystem (name : String) (size : UInt32) : IO Font := do
  match findSystemFont name with
  | some path => load path size
  | none => throw (IO.Error.userError s!"Unknown system font: {name}. Try: Monaco, Helvetica, Times, monospace, sans-serif, serif")

/-- Load a system font with logical size, scaled by screen factor.
    This is the recommended way to load fonts when using Canvas.run with auto-scaling.

    Example:
    ```lean
    let screenScale ← FFI.getScreenScale
    let font ← Font.loadSystemScaled "Monaco" 16 screenScale
    -- or inside CanvasM:
    let font ← Font.loadSystemScaled "Monaco" 16 (← getScreenScale)
    ```
-/
def loadSystemScaled (name : String) (logicalSize : Float) (screenScale : Float) : IO Font :=
  loadSystem name (logicalSize * screenScale).toUInt32

/-- Load a font from a path with logical size, scaled by screen factor. -/
def loadScaled (path : String) (logicalSize : Float) (screenScale : Float) : IO Font :=
  load path (logicalSize * screenScale).toUInt32

end Font

end Afferent

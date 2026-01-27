/-
  Afferent Text Measurer
  Implementation of Arbor's TextMeasurer typeclass for FreeType fonts.
-/
import Afferent.Text.Font
import Afferent.Arbor.Core.TextMeasurer

namespace Afferent

/-- Font registry that maps FontIds to loaded Font handles.
    This allows Arbor widgets (which use abstract FontIds) to work
    with Afferent's concrete Font type. -/
structure FontRegistry where
  fonts : Array Font
  defaultFont : Option Font := none

instance : Inhabited FontRegistry where
  default := ⟨#[], none⟩

namespace FontRegistry

def empty : FontRegistry := ⟨#[], none⟩

/-- Register a font and return its FontId with actual metrics. -/
def register (reg : FontRegistry) (font : Font) (name : String) : FontRegistry × Afferent.Arbor.FontId :=
  let id : Afferent.Arbor.FontId := {
    id := reg.fonts.size
    name := name
    size := font.size.toFloat
    lineHeight := font.lineHeight
    ascender := font.ascender
    descender := font.descender
  }
  let newReg := { reg with fonts := reg.fonts.push font }
  (newReg, id)

/-- Set the default font (used when FontId lookup fails). -/
def setDefault (reg : FontRegistry) (font : Font) : FontRegistry :=
  { reg with defaultFont := some font }

/-- Look up a Font by FontId. Returns default font if not found. -/
def get (reg : FontRegistry) (fontId : Afferent.Arbor.FontId) : Option Font :=
  reg.fonts[fontId.id]? <|> reg.defaultFont

end FontRegistry

/-- Reader monad with access to a FontRegistry. -/
abbrev FontReaderT (m : Type → Type) := ReaderT FontRegistry m

/-- TextMeasurer instance for FontReaderT IO.
    This allows Arbor's text measurement functions to work with Afferent's fonts. -/
instance : Afferent.Arbor.TextMeasurer (FontReaderT IO) where
  measureText text fontId := do
    let reg ← read
    match reg.get fontId with
    | some font =>
      let (w, h) ← font.measureText text
      pure ⟨w, h, font.ascender, font.descender, font.lineHeight⟩
    | none =>
      panic! s!"FontId {fontId.id} ('{fontId.name}') not found in FontRegistry"

  measureChar c fontId := do
    let reg ← read
    match reg.get fontId with
    | some font =>
      let (w, _) ← font.measureText (String.singleton c)
      pure w
    | none =>
      panic! s!"FontId {fontId.id} ('{fontId.name}') not found in FontRegistry"

  fontMetrics fontId := do
    let reg ← read
    match reg.get fontId with
    | some font =>
      pure ⟨0, font.glyphHeight, font.ascender, font.descender, font.lineHeight⟩
    | none =>
      panic! s!"FontId {fontId.id} ('{fontId.name}') not found in FontRegistry"

/-- Run a FontReaderT computation with a registry. -/
def runWithFonts {α : Type} (reg : FontRegistry) (m : FontReaderT IO α) : IO α :=
  m.run reg

/-- Convenience: create a registry with a single font as both registered and default. -/
def withFont (font : Font) (name : String := "default") : IO (FontRegistry × Afferent.Arbor.FontId) := do
  let (reg, fontId) := FontRegistry.empty.register font name
  let reg := reg.setDefault font
  pure (reg, fontId)

end Afferent

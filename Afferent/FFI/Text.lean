/-
  Afferent FFI Text
  Font loading and text rendering bindings (FreeType-based, cross-platform).
-/
import Afferent.FFI.Types

namespace Afferent.FFI

-- Font management
@[extern "lean_afferent_font_load"]
opaque Font.load (path : @& String) (size : UInt32) : IO Font

@[extern "lean_afferent_font_destroy"]
opaque Font.destroy (font : @& Font) : IO Unit

@[extern "lean_afferent_font_get_metrics"]
opaque Font.getMetrics (font : @& Font) : IO (Float × Float × Float)

-- Text rendering
@[extern "lean_afferent_text_measure"]
opaque Text.measure (font : @& Font) (text : @& String) : IO (Float × Float)

@[extern "lean_afferent_text_render"]
opaque Text.render
  (renderer : @& Renderer)
  (font : @& Font)
  (text : @& String)
  (x y : Float)
  (r g b a : Float)
  (transform : @& Array Float)
  (canvasWidth canvasHeight : Float) : IO Unit

/-- Render multiple text strings with the same font in a single draw call.
    Each entry has its own position, color, and transform. -/
@[extern "lean_afferent_text_render_batch"]
opaque Text.renderBatch
  (renderer : @& Renderer)
  (font : @& Font)
  (texts : @& Array String)
  (positions : @& Array Float)   -- [x0, y0, x1, y1, ...]
  (colors : @& Array Float)      -- [r0, g0, b0, a0, ...]
  (transforms : @& Array Float)  -- [a0, b0, c0, d0, tx0, ty0, a1, ...]
  (canvasWidth canvasHeight : Float) : IO Unit

end Afferent.FFI

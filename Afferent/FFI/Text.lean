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

end Afferent.FFI

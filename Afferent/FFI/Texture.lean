/-
  Afferent FFI Texture
  Texture loading and sprite rendering bindings.
-/
import Afferent.FFI.Types

namespace Afferent.FFI

-- Load a texture from a file path (supports PNG, JPG, etc via stb_image)
@[extern "lean_afferent_texture_load"]
opaque Texture.load (path : @& String) : IO Texture

-- Load a texture from memory (PNG/JPG data in ByteArray)
@[extern "lean_afferent_texture_load_from_memory"]
opaque Texture.loadFromMemory (data : @& ByteArray) : IO Texture

-- Destroy a texture
@[extern "lean_afferent_texture_destroy"]
opaque Texture.destroy (texture : @& Texture) : IO Unit

-- Get texture dimensions (width, height)
@[extern "lean_afferent_texture_get_size"]
opaque Texture.getSize (texture : @& Texture) : IO (UInt32 × UInt32)

-- Draw sprites from FloatBuffer already in SpriteInstanceData layout.
@[extern "lean_afferent_renderer_draw_sprites_instance_buffer"]
opaque Renderer.drawSpritesInstanceBuffer
  (renderer : @& Renderer)
  (texture : @& Texture)
  (buffer : @& FloatBuffer)
  (count : UInt32)
  (canvasWidth : Float)
  (canvasHeight : Float) : IO Unit

end Afferent.FFI

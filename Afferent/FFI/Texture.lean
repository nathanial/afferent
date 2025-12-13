/-
  Afferent FFI Texture
  Texture loading and sprite rendering bindings.
-/
import Afferent.FFI.Types

namespace Afferent.FFI

-- Load a texture from a file path (supports PNG, JPG, etc via stb_image)
@[extern "lean_afferent_texture_load"]
opaque Texture.load (path : @& String) : IO Texture

-- Destroy a texture
@[extern "lean_afferent_texture_destroy"]
opaque Texture.destroy (texture : @& Texture) : IO Unit

-- Get texture dimensions (width, height)
@[extern "lean_afferent_texture_get_size"]
opaque Texture.getSize (texture : @& Texture) : IO (UInt32 × UInt32)

-- Draw textured sprites (called every frame with position data)
-- data: [pixelX, pixelY, rotation, halfSizePixels, alpha] × count (5 floats per sprite)
@[extern "lean_afferent_renderer_draw_sprites"]
opaque Renderer.drawSprites
  (renderer : @& Renderer)
  (texture : @& Texture)
  (data : @& Array Float)
  (count : UInt32)
  (canvasWidth : Float)
  (canvasHeight : Float) : IO Unit

-- ============================================================================
-- HIGH-PERFORMANCE SPRITE SYSTEM (FloatBuffer-based, C-side physics)
-- For 1M+ sprites at 60fps
-- ============================================================================

-- Initialize sprites in FloatBuffer with random positions/velocities
-- Layout: [x, y, vx, vy, rotation] per sprite (5 floats)
@[extern "lean_afferent_float_buffer_init_sprites"]
opaque FloatBuffer.initSprites
  (buffer : @& FloatBuffer)
  (count : UInt32)
  (screenWidth : Float)
  (screenHeight : Float)
  (seed : UInt32) : IO Unit

-- Update sprite physics (bouncing) - runs entirely in C, no per-sprite FFI overhead
@[extern "lean_afferent_float_buffer_update_sprites"]
opaque FloatBuffer.updateSprites
  (buffer : @& FloatBuffer)
  (count : UInt32)
  (dt : Float)
  (halfSize : Float)
  (screenWidth : Float)
  (screenHeight : Float) : IO Unit

-- Draw sprites from FloatBuffer (zero-copy path)
@[extern "lean_afferent_renderer_draw_sprites_buffer"]
opaque Renderer.drawSpritesBuffer
  (renderer : @& Renderer)
  (texture : @& Texture)
  (buffer : @& FloatBuffer)
  (count : UInt32)
  (halfSize : Float)
  (canvasWidth : Float)
  (canvasHeight : Float) : IO Unit

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

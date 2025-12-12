/-
  Afferent FFI bindings to native Metal rendering code.
  These are low-level bindings - higher level APIs will be built on top.
-/

namespace Afferent.FFI

-- Opaque handle types using NonemptyType pattern
opaque WindowPointed : NonemptyType
def Window : Type := WindowPointed.type
instance : Nonempty Window := WindowPointed.property

opaque RendererPointed : NonemptyType
def Renderer : Type := RendererPointed.type
instance : Nonempty Renderer := RendererPointed.property

opaque BufferPointed : NonemptyType
def Buffer : Type := BufferPointed.type
instance : Nonempty Buffer := BufferPointed.property

opaque FontPointed : NonemptyType
def Font : Type := FontPointed.type
instance : Nonempty Font := FontPointed.property

-- FloatBuffer: High-performance mutable float array for instance data
-- Lives in C memory, avoids Lean's copy-on-write array semantics
opaque FloatBufferPointed : NonemptyType
def FloatBuffer : Type := FloatBufferPointed.type
instance : Nonempty FloatBuffer := FloatBufferPointed.property

-- Module initialization (registers external classes)
@[extern "afferent_initialize"]
opaque init : IO Unit

-- Window management
@[extern "lean_afferent_window_create"]
opaque Window.create (width height : UInt32) (title : @& String) : IO Window

@[extern "lean_afferent_window_destroy"]
opaque Window.destroy (window : @& Window) : IO Unit

@[extern "lean_afferent_window_should_close"]
opaque Window.shouldClose (window : @& Window) : IO Bool

@[extern "lean_afferent_window_poll_events"]
opaque Window.pollEvents (window : @& Window) : IO Unit

@[extern "lean_afferent_window_get_size"]
opaque Window.getSize (window : @& Window) : IO (UInt32 × UInt32)

-- Keyboard input
@[extern "lean_afferent_window_get_key_code"]
opaque Window.getKeyCode (window : @& Window) : IO UInt16

@[extern "lean_afferent_window_clear_key"]
opaque Window.clearKey (window : @& Window) : IO Unit

-- Renderer management
@[extern "lean_afferent_renderer_create"]
opaque Renderer.create (window : @& Window) : IO Renderer

@[extern "lean_afferent_renderer_destroy"]
opaque Renderer.destroy (renderer : @& Renderer) : IO Unit

@[extern "lean_afferent_renderer_begin_frame"]
opaque Renderer.beginFrame (renderer : @& Renderer) (r g b a : Float) : IO Bool

@[extern "lean_afferent_renderer_end_frame"]
opaque Renderer.endFrame (renderer : @& Renderer) : IO Unit

-- Buffer management
-- Vertices: Array of Float, 6 per vertex (pos.x, pos.y, color.r, color.g, color.b, color.a)
@[extern "lean_afferent_buffer_create_vertex"]
opaque Buffer.createVertex (renderer : @& Renderer) (vertices : @& Array Float) : IO Buffer

-- Indices: Array of UInt32
@[extern "lean_afferent_buffer_create_index"]
opaque Buffer.createIndex (renderer : @& Renderer) (indices : @& Array UInt32) : IO Buffer

@[extern "lean_afferent_buffer_destroy"]
opaque Buffer.destroy (buffer : @& Buffer) : IO Unit

-- Drawing
@[extern "lean_afferent_renderer_draw_triangles"]
opaque Renderer.drawTriangles
  (renderer : @& Renderer)
  (vertexBuffer indexBuffer : @& Buffer)
  (indexCount : UInt32) : IO Unit

-- Instanced rectangle drawing (GPU-accelerated transforms)
-- instanceData: Array of 8 floats per instance (pos.x, pos.y, angle, halfSize, r, g, b, a)
@[extern "lean_afferent_renderer_draw_instanced_rects"]
opaque Renderer.drawInstancedRects
  (renderer : @& Renderer)
  (instanceData : @& Array Float)
  (instanceCount : UInt32) : IO Unit

-- Instanced triangle drawing (GPU-accelerated transforms)
-- instanceData: Array of 8 floats per instance (pos.x, pos.y, angle, halfSize, r, g, b, a)
@[extern "lean_afferent_renderer_draw_instanced_triangles"]
opaque Renderer.drawInstancedTriangles
  (renderer : @& Renderer)
  (instanceData : @& Array Float)
  (instanceCount : UInt32) : IO Unit

-- Instanced circle drawing (smooth circles via fragment shader)
-- instanceData: Array of 8 floats per instance (pos.x, pos.y, angle, halfSize, r, g, b, a)
@[extern "lean_afferent_renderer_draw_instanced_circles"]
opaque Renderer.drawInstancedCircles
  (renderer : @& Renderer)
  (instanceData : @& Array Float)
  (instanceCount : UInt32) : IO Unit

-- Scissor rect for clipping
@[extern "lean_afferent_renderer_set_scissor"]
opaque Renderer.setScissor
  (renderer : @& Renderer)
  (x y width height : UInt32) : IO Unit

@[extern "lean_afferent_renderer_reset_scissor"]
opaque Renderer.resetScissor (renderer : @& Renderer) : IO Unit

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

-- FloatBuffer management
@[extern "lean_afferent_float_buffer_create"]
opaque FloatBuffer.create (capacity : USize) : IO FloatBuffer

@[extern "lean_afferent_float_buffer_destroy"]
opaque FloatBuffer.destroy (buf : @& FloatBuffer) : IO Unit

@[extern "lean_afferent_float_buffer_set"]
opaque FloatBuffer.set (buf : @& FloatBuffer) (index : USize) (value : Float) : IO Unit

@[extern "lean_afferent_float_buffer_get"]
opaque FloatBuffer.get (buf : @& FloatBuffer) (index : USize) : IO Float

-- Set 8 consecutive floats at once (8x less FFI overhead for instance data)
@[extern "lean_afferent_float_buffer_set_vec8"]
opaque FloatBuffer.setVec8 (buf : @& FloatBuffer) (index : USize)
  (v0 v1 v2 v3 v4 v5 v6 v7 : Float) : IO Unit

-- Draw instanced shapes directly from FloatBuffer (zero-copy path)
@[extern "lean_afferent_renderer_draw_instanced_rects_buffer"]
opaque Renderer.drawInstancedRectsBuffer
  (renderer : @& Renderer)
  (buffer : @& FloatBuffer)
  (instanceCount : UInt32) : IO Unit

@[extern "lean_afferent_renderer_draw_instanced_triangles_buffer"]
opaque Renderer.drawInstancedTrianglesBuffer
  (renderer : @& Renderer)
  (buffer : @& FloatBuffer)
  (instanceCount : UInt32) : IO Unit

@[extern "lean_afferent_renderer_draw_instanced_circles_buffer"]
opaque Renderer.drawInstancedCirclesBuffer
  (renderer : @& Renderer)
  (buffer : @& FloatBuffer)
  (instanceCount : UInt32) : IO Unit

-- ============================================================================
-- ANIMATED RENDERING - GPU-side animation for maximum performance
-- Static data uploaded once, only time uniform sent per frame
-- Data format: [pixelX, pixelY, hueBase, halfSizePixels, phaseOffset, spinSpeed] × count
-- ============================================================================

-- Upload static instance data (called once at startup)
@[extern "lean_afferent_renderer_upload_animated_rects"]
opaque Renderer.uploadAnimatedRects
  (renderer : @& Renderer)
  (data : @& Array Float)
  (count : UInt32) : IO Unit

@[extern "lean_afferent_renderer_upload_animated_triangles"]
opaque Renderer.uploadAnimatedTriangles
  (renderer : @& Renderer)
  (data : @& Array Float)
  (count : UInt32) : IO Unit

@[extern "lean_afferent_renderer_upload_animated_circles"]
opaque Renderer.uploadAnimatedCircles
  (renderer : @& Renderer)
  (data : @& Array Float)
  (count : UInt32) : IO Unit

-- Draw animated shapes (called every frame - only sends time!)
@[extern "lean_afferent_renderer_draw_animated_rects"]
opaque Renderer.drawAnimatedRects
  (renderer : @& Renderer)
  (time : Float) : IO Unit

@[extern "lean_afferent_renderer_draw_animated_triangles"]
opaque Renderer.drawAnimatedTriangles
  (renderer : @& Renderer)
  (time : Float) : IO Unit

@[extern "lean_afferent_renderer_draw_animated_circles"]
opaque Renderer.drawAnimatedCircles
  (renderer : @& Renderer)
  (time : Float) : IO Unit

-- ============================================================================
-- ORBITAL RENDERING - Particles orbiting around a center point
-- Position computed on GPU from orbital parameters
-- Data format: [phase, baseRadius, orbitSpeed, phaseX3, phase2, hueBase, halfSizePixels, padding] × count
-- ============================================================================

-- Upload static orbital instance data (called once at startup)
@[extern "lean_afferent_renderer_upload_orbital_particles"]
opaque Renderer.uploadOrbitalParticles
  (renderer : @& Renderer)
  (data : @& Array Float)
  (count : UInt32)
  (centerX centerY : Float) : IO Unit

-- Draw orbital particles (called every frame - only sends time!)
@[extern "lean_afferent_renderer_draw_orbital_particles"]
opaque Renderer.drawOrbitalParticles
  (renderer : @& Renderer)
  (time : Float) : IO Unit

-- ============================================================================
-- DYNAMIC CIRCLE RENDERING - CPU positions, GPU color/NDC
-- Positions updated each frame, HSV->RGB and pixel->NDC done on GPU
-- Data format: [pixelX, pixelY, hueBase, radiusPixels] × count (4 floats per circle)
-- ============================================================================

-- Draw dynamic circles (called every frame with position data)
@[extern "lean_afferent_renderer_draw_dynamic_circles"]
opaque Renderer.drawDynamicCircles
  (renderer : @& Renderer)
  (data : @& Array Float)
  (count : UInt32)
  (time : Float)
  (canvasWidth : Float)
  (canvasHeight : Float) : IO Unit

-- Draw dynamic rects (called every frame with position data)
-- data: [pixelX, pixelY, hueBase, halfSizePixels, rotation] × count (5 floats per rect)
@[extern "lean_afferent_renderer_draw_dynamic_rects"]
opaque Renderer.drawDynamicRects
  (renderer : @& Renderer)
  (data : @& Array Float)
  (count : UInt32)
  (time : Float)
  (canvasWidth : Float)
  (canvasHeight : Float) : IO Unit

-- Draw dynamic triangles (called every frame with position data)
-- data: [pixelX, pixelY, hueBase, halfSizePixels, rotation] × count (5 floats per triangle)
@[extern "lean_afferent_renderer_draw_dynamic_triangles"]
opaque Renderer.drawDynamicTriangles
  (renderer : @& Renderer)
  (data : @& Array Float)
  (count : UInt32)
  (time : Float)
  (canvasWidth : Float)
  (canvasHeight : Float) : IO Unit

-- ============================================================================
-- TEXTURE/SPRITE RENDERING - Textured quads with transparency
-- Load textures and render textured sprites with rotation and alpha
-- ============================================================================

opaque TexturePointed : NonemptyType
def Texture : Type := TexturePointed.type
instance : Nonempty Texture := TexturePointed.property

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

end Afferent.FFI

/-
  Afferent FFI Renderer
  GPU rendering operations including frame management, drawing, and instanced rendering.
-/
import Afferent.FFI.Types

namespace Afferent.FFI

-- Renderer management
@[extern "lean_afferent_renderer_create"]
opaque Renderer.create (window : @& Window) : IO Renderer

@[extern "lean_afferent_renderer_destroy"]
opaque Renderer.destroy (renderer : @& Renderer) : IO Unit

@[extern "lean_afferent_renderer_begin_frame"]
opaque Renderer.beginFrame (renderer : @& Renderer) (r g b a : Float) : IO Bool

@[extern "lean_afferent_renderer_end_frame"]
opaque Renderer.endFrame (renderer : @& Renderer) : IO Unit

-- Enable/disable MSAA for subsequent frames.
@[extern "lean_afferent_renderer_set_msaa_enabled"]
opaque Renderer.setMSAAEnabled (renderer : @& Renderer) (enabled : Bool) : IO Unit

-- Override drawable pixel scale (1.0 disables Retina). Pass 0 to restore native scale.
@[extern "lean_afferent_renderer_set_drawable_scale"]
opaque Renderer.setDrawableScale (renderer : @& Renderer) (scale : Float) : IO Unit

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

-- Draw dynamic circles directly from a FloatBuffer containing the expected layout
-- (no Lean Array boxing/unboxing).
@[extern "lean_afferent_renderer_draw_dynamic_circles_buffer"]
opaque Renderer.drawDynamicCirclesBuffer
  (renderer : @& Renderer)
  (buffer : @& FloatBuffer)
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
-- TEXTURED RECTANGLE RENDERING - Map tile rendering with source/dest rects
-- ============================================================================

-- Draw a textured rectangle with source and destination rectangles
-- Used for map tile rendering with cropping and scaling
-- srcX/Y/W/H: source rectangle in texture pixels
-- dstX/Y/W/H: destination rectangle in screen pixels
-- alpha: transparency (0.0-1.0)
@[extern "lean_afferent_renderer_draw_textured_rect"]
opaque Renderer.drawTexturedRect
  (renderer : @& Renderer)
  (texture : @& Texture)
  (srcX srcY srcW srcH : Float)   -- Source rectangle in texture pixels
  (dstX dstY dstW dstH : Float)   -- Destination rectangle in screen pixels
  (canvasWidth canvasHeight : Float) -- Canvas dimensions for NDC conversion
  (alpha : Float) : IO Unit

end Afferent.FFI

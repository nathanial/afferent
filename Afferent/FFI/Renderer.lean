/-
  Afferent FFI Renderer
  GPU rendering operations including frame management, drawing, and instanced rendering.
-/
import Afferent.FFI.Types
import Afferent.FFI.Init

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

-- Stroke vertices: Array of Float, 5 per vertex (pos.x, pos.y, nx, ny, side)
@[extern "lean_afferent_buffer_create_stroke_vertex"]
opaque Buffer.createStrokeVertex (renderer : @& Renderer) (vertices : @& Array Float) : IO Buffer

-- Stroke segments: Array of Float, 18 per segment (packed parametric segment data)
@[extern "lean_afferent_buffer_create_stroke_segment"]
opaque Buffer.createStrokeSegment (renderer : @& Renderer) (segments : @& Array Float) : IO Buffer

-- Persistent stroke segments (not pooled): Array of Float, 18 per segment
@[extern "lean_afferent_buffer_create_stroke_segment_persistent"]
opaque Buffer.createStrokeSegmentPersistent (renderer : @& Renderer) (segments : @& Array Float) : IO Buffer

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

-- Draw extruded strokes (screen-space width)
@[extern "lean_afferent_renderer_draw_stroke"]
opaque Renderer.drawStroke
  (renderer : @& Renderer)
  (vertexBuffer indexBuffer : @& Buffer)
  (indexCount : UInt32)
  (halfWidth : Float)
  (canvasWidth : Float)
  (canvasHeight : Float)
  (r g b a : Float) : IO Unit

-- Draw GPU-extruded strokes from parametric segments
@[extern "lean_afferent_renderer_draw_stroke_path"]
opaque Renderer.drawStrokePath
  (renderer : @& Renderer)
  (segmentBuffer : @& Buffer)
  (segmentCount : UInt32)
  (segmentSubdivisions : UInt32)
  (halfWidth : Float)
  (canvasWidth : Float)
  (canvasHeight : Float)
  (miterLimit : Float)
  (lineCap : UInt32)
  (lineJoin : UInt32)
  (transformA : Float)
  (transformB : Float)
  (transformC : Float)
  (transformD : Float)
  (transformTx : Float)
  (transformTy : Float)
  (dashSegments : @& Array Float)
  (dashCount : UInt32)
  (dashOffset : Float)
  (r g b a : Float) : IO Unit

-- Instanced rectangle drawing (GPU-accelerated transforms)
-- instanceData: Array of 8 floats per instance (pos.x, pos.y, angle, halfSize, r, g, b, a)
-- sizeMode: 0 = world (offset transformed by matrix), 1 = screen (pixel size)
-- colorMode: 0 = RGBA, 1 = HSV(time-based)
@[extern "lean_afferent_renderer_draw_instanced_rects"]
opaque Renderer.drawInstancedRects
  (renderer : @& Renderer)
  (instanceData : @& Array Float)
  (instanceCount : UInt32)
  (transformA transformB transformC transformD transformTx transformTy : Float)
  (viewportWidth viewportHeight : Float)
  (sizeMode : UInt32)
  (time hueSpeed : Float)
  (colorMode : UInt32) : IO Unit

-- Instanced triangle drawing (GPU-accelerated transforms)
-- instanceData: Array of 8 floats per instance (pos.x, pos.y, angle, halfSize, r, g, b, a)
-- sizeMode: 0 = world (offset transformed by matrix), 1 = screen (pixel size)
-- colorMode: 0 = RGBA, 1 = HSV(time-based)
@[extern "lean_afferent_renderer_draw_instanced_triangles"]
opaque Renderer.drawInstancedTriangles
  (renderer : @& Renderer)
  (instanceData : @& Array Float)
  (instanceCount : UInt32)
  (transformA transformB transformC transformD transformTx transformTy : Float)
  (viewportWidth viewportHeight : Float)
  (sizeMode : UInt32)
  (time hueSpeed : Float)
  (colorMode : UInt32) : IO Unit

-- Instanced circle drawing (smooth circles via fragment shader)
-- instanceData: Array of 8 floats per instance (pos.x, pos.y, angle, halfSize, r, g, b, a)
-- sizeMode: 0 = world (offset transformed by matrix), 1 = screen (pixel size)
-- colorMode: 0 = RGBA, 1 = HSV(time-based)
@[extern "lean_afferent_renderer_draw_instanced_circles"]
opaque Renderer.drawInstancedCircles
  (renderer : @& Renderer)
  (instanceData : @& Array Float)
  (instanceCount : UInt32)
  (transformA transformB transformC transformD transformTx transformTy : Float)
  (viewportWidth viewportHeight : Float)
  (sizeMode : UInt32)
  (time hueSpeed : Float)
  (colorMode : UInt32) : IO Unit

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
  (instanceCount : UInt32)
  (transformA transformB transformC transformD transformTx transformTy : Float)
  (viewportWidth viewportHeight : Float)
  (sizeMode : UInt32)
  (time hueSpeed : Float)
  (colorMode : UInt32) : IO Unit

@[extern "lean_afferent_renderer_draw_instanced_triangles_buffer"]
opaque Renderer.drawInstancedTrianglesBuffer
  (renderer : @& Renderer)
  (buffer : @& FloatBuffer)
  (instanceCount : UInt32)
  (transformA transformB transformC transformD transformTx transformTy : Float)
  (viewportWidth viewportHeight : Float)
  (sizeMode : UInt32)
  (time hueSpeed : Float)
  (colorMode : UInt32) : IO Unit

@[extern "lean_afferent_renderer_draw_instanced_circles_buffer"]
opaque Renderer.drawInstancedCirclesBuffer
  (renderer : @& Renderer)
  (buffer : @& FloatBuffer)
  (instanceCount : UInt32)
  (transformA transformB transformC transformD transformTx transformTy : Float)
  (viewportWidth viewportHeight : Float)
  (sizeMode : UInt32)
  (time hueSpeed : Float)
  (colorMode : UInt32) : IO Unit

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

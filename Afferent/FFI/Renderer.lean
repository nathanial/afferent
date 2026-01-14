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

-- Instanced shape drawing (GPU-accelerated transforms)
-- shapeType: 0=rect, 1=triangle, 2=circle
-- instanceData: Array of 8 floats per instance (pos.x, pos.y, angle, halfSize, r, g, b, a)
-- sizeMode: 0 = world (offset transformed by matrix), 1 = screen (pixel size)
-- colorMode: 0 = RGBA, 1 = HSV(time-based)
@[extern "lean_afferent_renderer_draw_instanced_shapes"]
opaque Renderer.drawInstancedShapes
  (renderer : @& Renderer)
  (shapeType : UInt32)
  (instanceData : @& Array Float)
  (instanceCount : UInt32)
  (transformA transformB transformC transformD transformTx transformTy : Float)
  (viewportWidth viewportHeight : Float)
  (sizeMode : UInt32)
  (time hueSpeed : Float)
  (colorMode : UInt32) : IO Unit

-- Convenience wrappers for type safety
def Renderer.drawInstancedRects (r : Renderer) (d : Array Float) (n : UInt32)
    (a b c d' tx ty vw vh : Float) (sm : UInt32) (t hs : Float) (cm : UInt32) : IO Unit :=
  r.drawInstancedShapes 0 d n a b c d' tx ty vw vh sm t hs cm

def Renderer.drawInstancedTriangles (r : Renderer) (d : Array Float) (n : UInt32)
    (a b c d' tx ty vw vh : Float) (sm : UInt32) (t hs : Float) (cm : UInt32) : IO Unit :=
  r.drawInstancedShapes 1 d n a b c d' tx ty vw vh sm t hs cm

def Renderer.drawInstancedCircles (r : Renderer) (d : Array Float) (n : UInt32)
    (a b c d' tx ty vw vh : Float) (sm : UInt32) (t hs : Float) (cm : UInt32) : IO Unit :=
  r.drawInstancedShapes 2 d n a b c d' tx ty vw vh sm t hs cm

-- Scissor rect for clipping
@[extern "lean_afferent_renderer_set_scissor"]
opaque Renderer.setScissor
  (renderer : @& Renderer)
  (x y width height : UInt32) : IO Unit

@[extern "lean_afferent_renderer_reset_scissor"]
opaque Renderer.resetScissor (renderer : @& Renderer) : IO Unit

-- Draw instanced shapes directly from FloatBuffer (zero-copy path)
-- shapeType: 0=rect, 1=triangle, 2=circle
@[extern "lean_afferent_renderer_draw_instanced_shapes_buffer"]
opaque Renderer.drawInstancedShapesBuffer
  (renderer : @& Renderer)
  (shapeType : UInt32)
  (buffer : @& FloatBuffer)
  (instanceCount : UInt32)
  (transformA transformB transformC transformD transformTx transformTy : Float)
  (viewportWidth viewportHeight : Float)
  (sizeMode : UInt32)
  (time hueSpeed : Float)
  (colorMode : UInt32) : IO Unit

-- Convenience wrappers for type safety (FloatBuffer variants)
def Renderer.drawInstancedRectsBuffer (r : Renderer) (buf : FloatBuffer) (n : UInt32)
    (a b c d' tx ty vw vh : Float) (sm : UInt32) (t hs : Float) (cm : UInt32) : IO Unit :=
  r.drawInstancedShapesBuffer 0 buf n a b c d' tx ty vw vh sm t hs cm

def Renderer.drawInstancedTrianglesBuffer (r : Renderer) (buf : FloatBuffer) (n : UInt32)
    (a b c d' tx ty vw vh : Float) (sm : UInt32) (t hs : Float) (cm : UInt32) : IO Unit :=
  r.drawInstancedShapesBuffer 1 buf n a b c d' tx ty vw vh sm t hs cm

def Renderer.drawInstancedCirclesBuffer (r : Renderer) (buf : FloatBuffer) (n : UInt32)
    (a b c d' tx ty vw vh : Float) (sm : UInt32) (t hs : Float) (cm : UInt32) : IO Unit :=
  r.drawInstancedShapesBuffer 2 buf n a b c d' tx ty vw vh sm t hs cm

-- ============================================================================
-- BATCHED RECTANGLE RENDERING - For chart/widget optimization
-- ============================================================================

-- Draw multiple axis-aligned rectangles in a single draw call.
-- Used to batch fillRect commands for charts (heatmaps, scatter plots, etc.)
-- instanceData: Array of 8 floats per rect [x, y, width, height, r, g, b, a]
-- cornerRadius: Uniform corner radius for all rects in the batch (0 for sharp corners)
@[extern "lean_afferent_renderer_draw_rects_batch"]
opaque Renderer.drawRectsBatch
  (renderer : @& Renderer)
  (instanceData : @& Array Float)
  (instanceCount : UInt32)
  (cornerRadius : Float)
  (canvasWidth canvasHeight : Float) : IO Unit

-- Draw multiple circles in a single draw call.
-- instanceData: Array of 6 floats per circle [centerX, centerY, radius, r, g, b, a]
@[extern "lean_afferent_renderer_draw_circles_batch"]
opaque Renderer.drawCirclesBatch
  (renderer : @& Renderer)
  (instanceData : @& Array Float)
  (instanceCount : UInt32)
  (canvasWidth canvasHeight : Float) : IO Unit

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

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

-- Draw triangles with screen-space coordinates (GPU converts to NDC)
-- vertexData: [x, y, r, g, b, a] per vertex (6 floats) in pixel coordinates
-- indices: triangle indices
@[extern "lean_afferent_renderer_draw_triangles_screen_coords"]
opaque Renderer.drawTrianglesScreenCoords
  (renderer : @& Renderer)
  (vertexData : @& Array Float)
  (indices : @& Array UInt32)
  (vertexCount : UInt32)
  (canvasWidth canvasHeight : Float) : IO Unit

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

-- ============================================================================
-- BATCHED SHAPE RENDERING - For chart/widget optimization
-- ============================================================================

-- Draw multiple shapes in a single draw call.
-- kind: 0=rect, 1=circle, 2=strokeRect, 4=strokeCircle
-- instanceData: Array of 9 floats per instance [x, y, w, h, r, g, b, a, cornerRadius]
-- param0: lineWidth for strokeRect/strokeCircle, unused for rects (per-instance cornerRadius), ignored for circles
-- param1: unused for strokeRect/strokeCircle (per-instance cornerRadius), ignored otherwise
@[extern "lean_afferent_renderer_draw_batch"]
opaque Renderer.drawBatch
  (renderer : @& Renderer)
  (kind : UInt32)
  (instanceData : @& Array Float)
  (instanceCount : UInt32)
  (param0 : Float)
  (param1 : Float)
  (canvasWidth canvasHeight : Float) : IO Unit

-- Draw multiple line segments in a single draw call.
-- instanceData: Array of 9 floats per line [x1, y1, x2, y2, r, g, b, a, padding]
-- lineWidth: Width of all lines in the batch
@[extern "lean_afferent_renderer_draw_line_batch"]
opaque Renderer.drawLineBatch
  (renderer : @& Renderer)
  (instanceData : @& Array Float)
  (instanceCount : UInt32)
  (lineWidth : Float)
  (canvasWidth canvasHeight : Float) : IO Unit

-- Draw multiple line segments from a FloatBuffer (high-performance path).
-- buffer: FloatBuffer with 9 floats per line [x1, y1, x2, y2, r, g, b, a, padding]
-- lineWidth: Width of all lines in the batch
@[extern "lean_afferent_renderer_draw_line_batch_buffer"]
opaque Renderer.drawLineBatchBuffer
  (renderer : @& Renderer)
  (buffer : @& FloatBuffer)
  (instanceCount : UInt32)
  (lineWidth : Float)
  (canvasWidth canvasHeight : Float) : IO Unit

-- Draw multiple shapes from a FloatBuffer (high-performance path).
-- kind: 0=rect, 1=circle, 2=strokeRect
-- buffer: FloatBuffer with 9 floats per instance
-- param0: unused for rects (per-instance cornerRadius), ignored for circles, lineWidth for strokeRect
-- param1: unused
@[extern "lean_afferent_renderer_draw_batch_buffer"]
opaque Renderer.drawBatchBuffer
  (renderer : @& Renderer)
  (kind : UInt32)
  (buffer : @& FloatBuffer)
  (instanceCount : UInt32)
  (param0 : Float)
  (param1 : Float)
  (canvasWidth canvasHeight : Float) : IO Unit

-- ============================================================================
-- INSTANCED ARC STROKE RENDERING
-- ============================================================================

-- Draw multiple arc strokes in a single draw call with GPU-generated geometry.
-- instanceData: Array of 10 floats per arc [centerX, centerY, startAngle, sweepAngle,
--                                           radius, strokeWidth, r, g, b, a]
-- segments: Number of subdivisions per arc (higher = smoother)
@[extern "lean_afferent_arc_draw_instanced"]
opaque Renderer.drawArcInstanced
  (renderer : @& Renderer)
  (instanceData : @& Array Float)
  (instanceCount : UInt32)
  (segments : UInt32)
  (canvasWidth canvasHeight : Float) : IO Unit

-- Draw multiple arc strokes from a FloatBuffer (high-performance path).
@[extern "lean_afferent_arc_draw_instanced_buffer"]
opaque Renderer.drawArcInstancedBuffer
  (renderer : @& Renderer)
  (buffer : @& FloatBuffer)
  (instanceCount : UInt32)
  (segments : UInt32)
  (canvasWidth canvasHeight : Float) : IO Unit

end Afferent.FFI

#ifndef AFFERENT_H
#define AFFERENT_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

// Opaque handles
typedef struct AfferentWindow* AfferentWindowRef;
typedef struct AfferentRenderer* AfferentRendererRef;
typedef struct AfferentBuffer* AfferentBufferRef;
typedef struct AfferentFont* AfferentFontRef;
typedef struct AfferentFloatBuffer* AfferentFloatBufferRef;
typedef struct AfferentTexture* AfferentTextureRef;
typedef struct AfferentCachedMesh* AfferentCachedMeshRef;

// Result codes
typedef enum {
    AFFERENT_OK = 0,
    AFFERENT_ERROR_INIT_FAILED = 1,
    AFFERENT_ERROR_WINDOW_FAILED = 2,
    AFFERENT_ERROR_DEVICE_FAILED = 3,
    AFFERENT_ERROR_PIPELINE_FAILED = 4,
    AFFERENT_ERROR_BUFFER_FAILED = 5,
    AFFERENT_ERROR_FONT_FAILED = 6,
    AFFERENT_ERROR_TEXT_FAILED = 7,
} AfferentResult;

// Vertex structure (matches Metal shader input)
typedef struct {
    float position[2];
    float color[4];
} AfferentVertex;

// Stroke vertex structure (position, normal, side)
typedef struct {
    float position[2];
    float normal[2];
    float side;
} AfferentStrokeVertex;

// Stroke segment structure for GPU extrusion (packed floats)
typedef struct __attribute__((packed)) {
    float p0[2];
    float p1[2];
    float c1[2];
    float c2[2];
    float prevDir[2];
    float nextDir[2];
    float startDist;
    float length;
    float hasPrev;
    float hasNext;
    float kind;
    float padding;
} AfferentStrokeSegment;

// 3D Vertex structure (for 3D mesh rendering)
typedef struct {
    float position[3];  // x, y, z
    float normal[3];    // nx, ny, nz
    float color[4];     // r, g, b, a
} AfferentVertex3D;

// 3D Vertex with UV coordinates (for textured 3D mesh rendering)
typedef struct {
    float position[3];  // x, y, z
    float normal[3];    // nx, ny, nz
    float uv[2];        // u, v texture coordinates
    float color[4];     // r, g, b, a
} AfferentVertex3DTextured;  // 12 floats = 48 bytes

// Window management
AfferentResult afferent_window_create(
    uint32_t width,
    uint32_t height,
    const char* title,
    AfferentWindowRef* out_window
);
void afferent_window_destroy(AfferentWindowRef window);
bool afferent_window_should_close(AfferentWindowRef window);
void afferent_window_poll_events(AfferentWindowRef window);
void afferent_window_run_event_loop(AfferentWindowRef window);
void afferent_window_get_size(AfferentWindowRef window, uint32_t* width, uint32_t* height);

// Keyboard input
uint16_t afferent_window_get_key_code(AfferentWindowRef window);
bool afferent_window_has_key_pressed(AfferentWindowRef window);
void afferent_window_clear_key(AfferentWindowRef window);

// Mouse input
void afferent_window_get_mouse_pos(AfferentWindowRef window, float* x, float* y);
uint8_t afferent_window_get_mouse_buttons(AfferentWindowRef window);
uint16_t afferent_window_get_modifiers(AfferentWindowRef window);
void afferent_window_get_scroll_delta(AfferentWindowRef window, float* dx, float* dy);
void afferent_window_clear_scroll(AfferentWindowRef window);
bool afferent_window_mouse_in_window(AfferentWindowRef window);
bool afferent_window_get_click(AfferentWindowRef window, uint8_t* button, float* x, float* y, uint16_t* modifiers);
void afferent_window_clear_click(AfferentWindowRef window);

// Pointer lock (for FPS camera controls)
void afferent_window_set_pointer_lock(AfferentWindowRef window, bool locked);
bool afferent_window_get_pointer_lock(AfferentWindowRef window);
void afferent_window_get_mouse_delta(AfferentWindowRef window, float* dx, float* dy);

// Key state (for continuous movement input)
bool afferent_window_is_key_down(AfferentWindowRef window, uint16_t keyCode);

// Get the main screen's backing scale factor (e.g., 2.0 for Retina, 1.5 for 150% scaling)
float afferent_get_screen_scale(void);

// Renderer management
AfferentResult afferent_renderer_create(
    AfferentWindowRef window,
    AfferentRendererRef* out_renderer
);
void afferent_renderer_destroy(AfferentRendererRef renderer);

// Frame rendering
AfferentResult afferent_renderer_begin_frame(AfferentRendererRef renderer, float r, float g, float b, float a);
AfferentResult afferent_renderer_end_frame(AfferentRendererRef renderer);

// Override drawable pixel scale (1.0 disables Retina). Pass <= 0 to restore native scale.
void afferent_renderer_set_drawable_scale(AfferentRendererRef renderer, float scale);

// Buffer management
AfferentResult afferent_buffer_create_vertex(
    AfferentRendererRef renderer,
    const AfferentVertex* vertices,
    uint32_t vertex_count,
    AfferentBufferRef* out_buffer
);
AfferentResult afferent_buffer_create_stroke_vertex(
    AfferentRendererRef renderer,
    const AfferentStrokeVertex* vertices,
    uint32_t vertex_count,
    AfferentBufferRef* out_buffer
);
AfferentResult afferent_buffer_create_stroke_segment(
    AfferentRendererRef renderer,
    const AfferentStrokeSegment* segments,
    uint32_t segment_count,
    AfferentBufferRef* out_buffer
);
AfferentResult afferent_buffer_create_stroke_segment_persistent(
    AfferentRendererRef renderer,
    const AfferentStrokeSegment* segments,
    uint32_t segment_count,
    AfferentBufferRef* out_buffer
);
AfferentResult afferent_buffer_create_index(
    AfferentRendererRef renderer,
    const uint32_t* indices,
    uint32_t index_count,
    AfferentBufferRef* out_buffer
);
void afferent_buffer_destroy(AfferentBufferRef buffer);

// Drawing
void afferent_renderer_draw_triangles(
    AfferentRendererRef renderer,
    AfferentBufferRef vertex_buffer,
    AfferentBufferRef index_buffer,
    uint32_t index_count
);

// Draw triangles with screen-space coordinates (GPU converts to NDC)
// vertex_data: [x, y, r, g, b, a] per vertex (6 floats) in pixel coordinates
// indices: triangle indices
void afferent_renderer_draw_triangles_screen_coords(
    AfferentRendererRef renderer,
    const float* vertex_data,
    const uint32_t* indices,
    uint32_t vertex_count,
    uint32_t index_count,
    float canvas_width,
    float canvas_height
);
void afferent_renderer_draw_stroke(
    AfferentRendererRef renderer,
    AfferentBufferRef vertex_buffer,
    AfferentBufferRef index_buffer,
    uint32_t index_count,
    float half_width,
    float canvas_width,
    float canvas_height,
    float r,
    float g,
    float b,
    float a
);
void afferent_renderer_draw_stroke_path(
    AfferentRendererRef renderer,
    AfferentBufferRef segment_buffer,
    uint32_t segment_count,
    uint32_t segment_subdivisions,
    float half_width,
    float canvas_width,
    float canvas_height,
    float miter_limit,
    uint32_t line_cap,
    uint32_t line_join,
    float transform_a,
    float transform_b,
    float transform_c,
    float transform_d,
    float transform_tx,
    float transform_ty,
    const float* dash_segments,
    uint32_t dash_count,
    float dash_offset,
    float r,
    float g,
    float b,
    float a
);

// Instanced shape drawing (GPU-accelerated transforms)
// shape_type: 0=rect, 1=triangle, 2=circle
// instance_data: array of 8 floats per instance:
//   pos.x, pos.y, angle, halfSize, r, g, b, a
// transform: column-major affine (a, b, c, d, tx, ty)
// sizeMode: 0 = world (offset transformed by matrix), 1 = screen (pixel size)
void afferent_renderer_draw_instanced_shapes(
    AfferentRendererRef renderer,
    uint32_t shape_type,
    const float* instance_data,
    uint32_t instance_count,
    float transform_a,
    float transform_b,
    float transform_c,
    float transform_d,
    float transform_tx,
    float transform_ty,
    float viewport_width,
    float viewport_height,
    uint32_t size_mode,
    float time,
    float hue_speed,
    uint32_t color_mode
);

// Scissor rect for clipping (in pixel coordinates)
void afferent_renderer_set_scissor(
    AfferentRendererRef renderer,
    uint32_t x,
    uint32_t y,
    uint32_t width,
    uint32_t height
);

// Reset scissor to full viewport
void afferent_renderer_reset_scissor(AfferentRendererRef renderer);

// Text rendering (FreeType)
// Initialize the text rendering subsystem (call once)
AfferentResult afferent_text_init(void);

// Shutdown the text rendering subsystem
void afferent_text_shutdown(void);

// Load a font from a file path at a given size (in pixels)
AfferentResult afferent_font_load(
    const char* path,
    uint32_t size,
    AfferentFontRef* out_font
);

// Destroy a loaded font
void afferent_font_destroy(AfferentFontRef font);

// Get font metrics (ascender, descender, line height)
void afferent_font_get_metrics(
    AfferentFontRef font,
    float* ascender,
    float* descender,
    float* line_height
);

// Measure text dimensions (returns width and height)
void afferent_text_measure(
    AfferentFontRef font,
    const char* text,
    float* width,
    float* height
);

// Render text - generates vertices for textured quads
// Returns vertex data (pos.x, pos.y, uv.x, uv.y, color.r, color.g, color.b, color.a)
// and index data for rendering. Caller must free the returned arrays.
// Transform is a 6-component affine matrix: [a, b, c, d, tx, ty]
// where: x' = a*x + c*y + tx, y' = b*x + d*y + ty
// canvas_width/height are the logical canvas dimensions used for NDC conversion
AfferentResult afferent_text_render(
    AfferentRendererRef renderer,
    AfferentFontRef font,
    const char* text,
    float x,
    float y,
    float r,
    float g,
    float b,
    float a,
    const float* transform,
    float canvas_width,
    float canvas_height
);

// Batch text rendering - render multiple strings with the same font in one draw call
// texts: array of C strings
// positions: [x0, y0, x1, y1, ...] (2 floats per entry)
// colors: [r0, g0, b0, a0, ...] (4 floats per entry)
// transforms: [a0, b0, c0, d0, tx0, ty0, ...] (6 floats per entry)
AfferentResult afferent_text_render_batch(
    AfferentRendererRef renderer,
    AfferentFontRef font,
    const char** texts,
    const float* positions,
    const float* colors,
    const float* transforms,
    uint32_t count,
    float canvas_width,
    float canvas_height
);

// FloatBuffer - mutable float array for high-performance instance data
// Lives in C memory, avoids Lean's copy-on-write array semantics
AfferentResult afferent_float_buffer_create(size_t capacity, AfferentFloatBufferRef* out);
void afferent_float_buffer_destroy(AfferentFloatBufferRef buf);
void afferent_float_buffer_set(AfferentFloatBufferRef buf, size_t index, float value);
float afferent_float_buffer_get(AfferentFloatBufferRef buf, size_t index);
size_t afferent_float_buffer_capacity(AfferentFloatBufferRef buf);
float* afferent_float_buffer_data(AfferentFloatBufferRef buf);
size_t afferent_float_buffer_count(AfferentFloatBufferRef buf);
void afferent_float_buffer_set_count(AfferentFloatBufferRef buf, size_t count);

// Set 8 consecutive floats at once (reduces FFI overhead by 8x for instance data)
void afferent_float_buffer_set_vec8(AfferentFloatBufferRef buf, size_t index,
    float v0, float v1, float v2, float v3, float v4, float v5, float v6, float v7);

// Set 9 consecutive floats at once (reduces FFI overhead for 9-float instance data)
void afferent_float_buffer_set_vec9(AfferentFloatBufferRef buf, size_t index,
    float v0, float v1, float v2, float v3, float v4, float v5, float v6, float v7, float v8);

// Set 5 consecutive floats at once (for sprite data: x, y, rotation, halfSize, alpha)
void afferent_float_buffer_set_vec5(AfferentFloatBufferRef buf, size_t index,
    float v0, float v1, float v2, float v3, float v4);

// Sprite system - high-performance bouncing sprites with C-side physics
// Layout: [x, y, vx, vy, rotation] per sprite (5 floats)
void afferent_float_buffer_init_sprites(AfferentFloatBufferRef buf, uint32_t count,
    float screenWidth, float screenHeight, uint32_t seed);
void afferent_float_buffer_update_sprites(AfferentFloatBufferRef buf, uint32_t count,
    float dt, float halfSize, float screenWidth, float screenHeight);

// ============================================================================
// Texture/Sprite rendering - Create textures and render textured sprites
// ============================================================================

// Create a texture from already-decoded RGBA pixel data
// Image decoding is done at the Lean level using the raster library
AfferentResult afferent_texture_create_from_rgba(
    const uint8_t* rgba_data,
    uint32_t width,
    uint32_t height,
    AfferentTextureRef* out_texture
);

// Destroy a loaded texture
void afferent_texture_destroy(AfferentTextureRef texture);

// Get texture dimensions
void afferent_texture_get_size(
    AfferentTextureRef texture,
    uint32_t* width,
    uint32_t* height
);

// Draw textured sprites (called every frame with position data)
// data: [pixelX, pixelY, rotation, halfSizePixels, alpha] × count (5 floats per sprite)
void afferent_renderer_draw_sprites(
    AfferentRendererRef renderer,
    AfferentTextureRef texture,
    const float* data,
    uint32_t count,
    float canvasWidth,
    float canvasHeight
);

// 3D Mesh rendering with perspective projection and lighting
// vertices: array of AfferentVertex3D (10 floats each: pos[3], normal[3], color[4])
// indices: triangle indices
// mvp_matrix: 4x4 model-view-projection matrix (16 floats, column-major)
// model_matrix: 4x4 model matrix for normal transformation (16 floats)
// light_dir: normalized light direction (3 floats)
// ambient: ambient light factor (0.0-1.0)
void afferent_renderer_draw_mesh_3d(
    AfferentRendererRef renderer,
    const AfferentVertex3D* vertices,
    uint32_t vertex_count,
    const uint32_t* indices,
    uint32_t index_count,
    const float* mvp_matrix,
    const float* model_matrix,
    const float* light_dir,
    float ambient,
    const float* camera_pos,
    const float* fog_color,
    float fog_start,
    float fog_end
);

// Projected-grid ocean rendering with GPU Gerstner waves + fog.
// Uses a fixed 4-wave set provided via `wave_params`:
// - wave_params[0..15]  : 4x waveA = (dirX, dirZ, k, omegaSpeed)
// - wave_params[16..31] : 4x waveB = (amplitude, ak, 0, 0)
void afferent_renderer_draw_ocean_projected_grid_with_fog(
    AfferentRendererRef renderer,
    uint32_t grid_size,
    const float* mvp_matrix,
    const float* model_matrix,
    const float* light_dir,
    float ambient,
    const float* camera_pos,
    const float* fog_color,
    float fog_start,
    float fog_end,
    float time,
    float fovY,
    float aspect,
    float maxDistance,
    float snapSize,
    float overscanNdc,
    float horizonMargin,
    float yaw,
    float pitch,
    const float* wave_params,
    uint32_t wave_param_count
);

// ============================================================================
// Batched shape rendering (for charts)
// ============================================================================

// kind: 0=rect, 1=circle, 2=stroke rect
// instance_data: Array of 9 floats per instance
// param0: unused for rects (per-instance cornerRadius), ignored for circles, lineWidth for stroke rects
// param1: unused for stroke rects (per-instance cornerRadius), ignored otherwise
void afferent_renderer_draw_batch(
    AfferentRendererRef renderer,
    uint32_t kind,
    const float* instance_data,
    uint32_t instance_count,
    float param0,
    float param1,
    float canvas_width,
    float canvas_height
);

// Draw multiple line segments in a single draw call.
// instance_data: array of 9 floats per line [x1, y1, x2, y2, r, g, b, a, padding]
void afferent_renderer_draw_line_batch(
    AfferentRendererRef renderer,
    const float* instance_data,
    uint32_t instance_count,
    float line_width,
    float canvas_width,
    float canvas_height
);

// ============================================================================
// Textured 3D Mesh rendering
// ============================================================================

// Draw a textured 3D mesh with perspective projection, lighting, and fog.
// vertices: array of floats (12 per vertex: pos[3], normal[3], uv[2], color[4])
// Uses a sub-range of the index buffer specified by index_offset and index_count.
void afferent_renderer_draw_mesh_3d_textured(
    AfferentRendererRef renderer,
    const float* vertices,
    uint32_t vertex_count,
    const uint32_t* indices,
    uint32_t index_offset,
    uint32_t index_count,
    const float* mvp_matrix,
    const float* model_matrix,
    const float* light_dir,
    float ambient,
    const float* camera_pos,
    const float* fog_color,
    float fog_start,
    float fog_end,
    AfferentTextureRef texture
);

// ============================================================================
// Cached Mesh for Instanced Polygon Rendering
// Tessellate a polygon once, store in GPU memory, draw many instances
// ============================================================================

// Create a cached mesh from tessellated polygon data
// vertices: flat array of [x, y, x, y, ...] positions
// vertex_count: number of vertices (not floats)
// indices: triangle indices
// index_count: number of indices
// center_x, center_y: mesh centroid (rotation pivot)
AfferentCachedMeshRef afferent_mesh_cache_create(
    AfferentRendererRef renderer,
    const float* vertices,
    uint32_t vertex_count,
    const uint32_t* indices,
    uint32_t index_count,
    float center_x,
    float center_y
);

// Destroy a cached mesh and free GPU resources
void afferent_mesh_cache_destroy(AfferentCachedMeshRef mesh);

// Draw all instances of a cached mesh in a single draw call
// instance_data: 8 floats per instance [x, y, rotation, scale, r, g, b, a]
void afferent_mesh_draw_instanced(
    AfferentRendererRef renderer,
    AfferentCachedMeshRef mesh,
    const float* instance_data,
    uint32_t instance_count,
    float canvas_width,
    float canvas_height
);

// ============================================================================
// Instanced Arc Stroke Rendering
// Draw multiple arc strokes in a single draw call with GPU-generated geometry.
// ============================================================================

// Draw instanced arc strokes
// instance_data: 10 floats per instance [centerX, centerY, startAngle, sweepAngle,
//                                        radius, strokeWidth, r, g, b, a]
// segments: number of subdivisions per arc (higher = smoother, default 16)
void afferent_arc_draw_instanced(
    AfferentRendererRef renderer,
    const float* instance_data,
    uint32_t instance_count,
    uint32_t segments,
    float canvas_width,
    float canvas_height
);

#ifdef __cplusplus
}
#endif

#endif // AFFERENT_H

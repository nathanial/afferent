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
void afferent_window_get_size(AfferentWindowRef window, uint32_t* width, uint32_t* height);

// Keyboard input
uint16_t afferent_window_get_key_code(AfferentWindowRef window);
void afferent_window_clear_key(AfferentWindowRef window);

// Renderer management
AfferentResult afferent_renderer_create(
    AfferentWindowRef window,
    AfferentRendererRef* out_renderer
);
void afferent_renderer_destroy(AfferentRendererRef renderer);

// Frame rendering
AfferentResult afferent_renderer_begin_frame(AfferentRendererRef renderer, float r, float g, float b, float a);
AfferentResult afferent_renderer_end_frame(AfferentRendererRef renderer);

// Buffer management
AfferentResult afferent_buffer_create_vertex(
    AfferentRendererRef renderer,
    const AfferentVertex* vertices,
    uint32_t vertex_count,
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

// Instanced rectangle drawing (GPU-accelerated transforms)
// instance_data: array of 8 floats per instance:
//   pos.x, pos.y (NDC), angle, halfSize (NDC), r, g, b, a
void afferent_renderer_draw_instanced_rects(
    AfferentRendererRef renderer,
    const float* instance_data,
    uint32_t instance_count
);

// Instanced triangle drawing (GPU-accelerated transforms)
// instance_data: same format as rects (8 floats per instance)
void afferent_renderer_draw_instanced_triangles(
    AfferentRendererRef renderer,
    const float* instance_data,
    uint32_t instance_count
);

// Instanced circle drawing (smooth circles via fragment shader)
// instance_data: same format as rects (8 floats per instance)
void afferent_renderer_draw_instanced_circles(
    AfferentRendererRef renderer,
    const float* instance_data,
    uint32_t instance_count
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

// FloatBuffer - mutable float array for high-performance instance data
// Lives in C memory, avoids Lean's copy-on-write array semantics
AfferentResult afferent_float_buffer_create(size_t capacity, AfferentFloatBufferRef* out);
void afferent_float_buffer_destroy(AfferentFloatBufferRef buf);
void afferent_float_buffer_set(AfferentFloatBufferRef buf, size_t index, float value);
float afferent_float_buffer_get(AfferentFloatBufferRef buf, size_t index);
size_t afferent_float_buffer_capacity(AfferentFloatBufferRef buf);
const float* afferent_float_buffer_data(AfferentFloatBufferRef buf);

// Set 8 consecutive floats at once (reduces FFI overhead by 8x for instance data)
void afferent_float_buffer_set_vec8(AfferentFloatBufferRef buf, size_t index,
    float v0, float v1, float v2, float v3, float v4, float v5, float v6, float v7);

// ============================================================================
// Animated rendering - GPU-side animation for maximum performance
// Static data uploaded once, only time uniform sent per frame
// ============================================================================

// Upload static instance data for animated rects (called once at startup)
// data: [pixelX, pixelY, hueBase, halfSizePixels, phaseOffset, spinSpeed] × count
void afferent_renderer_upload_animated_rects(
    AfferentRendererRef renderer,
    const float* data,
    uint32_t count
);

// Upload static instance data for animated triangles
void afferent_renderer_upload_animated_triangles(
    AfferentRendererRef renderer,
    const float* data,
    uint32_t count
);

// Upload static instance data for animated circles
void afferent_renderer_upload_animated_circles(
    AfferentRendererRef renderer,
    const float* data,
    uint32_t count
);

// Draw animated rects (called every frame - only sends time uniform!)
void afferent_renderer_draw_animated_rects(
    AfferentRendererRef renderer,
    float time
);

// Draw animated triangles (called every frame)
void afferent_renderer_draw_animated_triangles(
    AfferentRendererRef renderer,
    float time
);

// Draw animated circles (called every frame)
void afferent_renderer_draw_animated_circles(
    AfferentRendererRef renderer,
    float time
);

// ============================================================================
// Orbital rendering - Particles orbiting around a center point
// Position computed on GPU from orbital parameters
// ============================================================================

// Upload static orbital instance data (called once at startup)
// data: [phase, baseRadius, orbitSpeed, phaseX3, phase2, hueBase, halfSizePixels, padding] × count
void afferent_renderer_upload_orbital_particles(
    AfferentRendererRef renderer,
    const float* data,
    uint32_t count,
    float centerX,
    float centerY
);

// Draw orbital particles (called every frame - only sends time!)
void afferent_renderer_draw_orbital_particles(
    AfferentRendererRef renderer,
    float time
);

// ============================================================================
// Dynamic circle rendering - CPU positions, GPU color/NDC conversion
// Positions updated each frame, HSV->RGB and pixel->NDC done on GPU
// ============================================================================

// Draw dynamic circles (called every frame with position data)
// data: [pixelX, pixelY, hueBase, radiusPixels] × count (4 floats per circle)
void afferent_renderer_draw_dynamic_circles(
    AfferentRendererRef renderer,
    const float* data,
    uint32_t count,
    float time,
    float canvasWidth,
    float canvasHeight
);

// Draw dynamic rects (called every frame with position data)
// data: [pixelX, pixelY, hueBase, halfSizePixels, rotation] × count (5 floats per rect)
void afferent_renderer_draw_dynamic_rects(
    AfferentRendererRef renderer,
    const float* data,
    uint32_t count,
    float time,
    float canvasWidth,
    float canvasHeight
);

// Draw dynamic triangles (called every frame with position data)
// data: [pixelX, pixelY, hueBase, halfSizePixels, rotation] × count (5 floats per triangle)
void afferent_renderer_draw_dynamic_triangles(
    AfferentRendererRef renderer,
    const float* data,
    uint32_t count,
    float time,
    float canvasWidth,
    float canvasHeight
);

// ============================================================================
// Texture/Sprite rendering - Load textures and render textured sprites
// ============================================================================

// Load a texture from a file path (supports PNG, JPG, etc via stb_image)
AfferentResult afferent_texture_load(
    const char* path,
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

#ifdef __cplusplus
}
#endif

#endif // AFFERENT_H

#ifndef AFFERENT_H
#define AFFERENT_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

// Opaque handles
typedef struct AfferentWindow* AfferentWindowRef;
typedef struct AfferentRenderer* AfferentRendererRef;
typedef struct AfferentBuffer* AfferentBufferRef;

// Result codes
typedef enum {
    AFFERENT_OK = 0,
    AFFERENT_ERROR_INIT_FAILED = 1,
    AFFERENT_ERROR_WINDOW_FAILED = 2,
    AFFERENT_ERROR_DEVICE_FAILED = 3,
    AFFERENT_ERROR_PIPELINE_FAILED = 4,
    AFFERENT_ERROR_BUFFER_FAILED = 5,
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

#ifdef __cplusplus
}
#endif

#endif // AFFERENT_H

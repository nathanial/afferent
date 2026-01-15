// render.h - Internal renderer header with structures and declarations
#ifndef AFFERENT_METAL_RENDER_H
#define AFFERENT_METAL_RENDER_H

#import <Metal/Metal.h>
#import <QuartzCore/CAMetalLayer.h>
#include "afferent.h"
#include "types.h"

// External declarations from window.m
extern id<MTLDevice> afferent_window_get_device(AfferentWindowRef window);
extern CAMetalLayer* afferent_window_get_metal_layer(AfferentWindowRef window);

// External declarations from text_render.c for atlas dirty tracking
extern int afferent_font_atlas_dirty(AfferentFontRef font);
extern void afferent_font_atlas_clear_dirty(AfferentFontRef font);

// External declarations from text_render.c
extern uint8_t* afferent_font_get_atlas_data(AfferentFontRef font);
extern uint32_t afferent_font_get_atlas_width(AfferentFontRef font);
extern uint32_t afferent_font_get_atlas_height(AfferentFontRef font);
extern void* afferent_font_get_metal_texture(AfferentFontRef font);
extern void afferent_font_set_metal_texture(AfferentFontRef font, void* texture);
extern int afferent_text_generate_vertices(
    AfferentFontRef font,
    const char* text,
    float x, float y,
    float r, float g, float b, float a,
    float screen_width, float screen_height,
    const float* transform,
    float** out_vertices,
    uint32_t** out_indices,
    uint32_t* out_vertex_count,
    uint32_t* out_index_count
);

// External declarations from texture.c
extern const uint8_t* afferent_texture_get_data(AfferentTextureRef texture);
extern void afferent_texture_get_size(AfferentTextureRef texture, uint32_t* width, uint32_t* height);
extern void* afferent_texture_get_metal_texture(AfferentTextureRef texture);
extern void afferent_texture_set_metal_texture(AfferentTextureRef texture, void* metal_tex);

// Internal renderer structure
struct AfferentRenderer {
    AfferentWindowRef window;
    __strong id<MTLDevice> device;
    __strong id<MTLCommandQueue> commandQueue;
    bool msaaEnabled;                                  // Per-frame MSAA toggle
    float drawableScaleOverride;                       // 0 = native scale, >0 overrides
    // Active pipeline pointers (match current render pass sample count)
    __strong id<MTLRenderPipelineState> pipelineState;
    __strong id<MTLRenderPipelineState> strokePipelineState;    // Screen-space stroke pipeline
    __strong id<MTLRenderPipelineState> strokePathPipelineState; // GPU stroke path pipeline
    __strong id<MTLRenderPipelineState> textPipelineState;      // For text rendering
    __strong id<MTLRenderPipelineState> spritePipelineState;    // Sprite layout (5-float instances)
    __strong id<MTLRenderPipelineState> texturedSpritePipelineState; // Textured layout (10-float instances)
    // MSAA / non-MSAA variants for pipelines used in sprite benchmark
    __strong id<MTLRenderPipelineState> pipelineStateMSAA;
    __strong id<MTLRenderPipelineState> pipelineStateNoMSAA;
    __strong id<MTLRenderPipelineState> strokePipelineStateMSAA;
    __strong id<MTLRenderPipelineState> strokePipelineStateNoMSAA;
    __strong id<MTLRenderPipelineState> strokePathPipelineStateMSAA;
    __strong id<MTLRenderPipelineState> strokePathPipelineStateNoMSAA;
    __strong id<MTLRenderPipelineState> textPipelineStateMSAA;
    __strong id<MTLRenderPipelineState> textPipelineStateNoMSAA;
    __strong id<MTLRenderPipelineState> spritePipelineStateMSAA;
    __strong id<MTLRenderPipelineState> spritePipelineStateNoMSAA;
    __strong id<MTLRenderPipelineState> texturedSpritePipelineStateMSAA;
    __strong id<MTLRenderPipelineState> texturedSpritePipelineStateNoMSAA;
    __strong id<MTLRenderPipelineState> instancedPipelineState; // For instanced rect rendering
    __strong id<MTLRenderPipelineState> trianglePipelineState;  // For instanced triangle rendering
    __strong id<MTLRenderPipelineState> circlePipelineState;    // For instanced circle rendering
    __strong id<MTLRenderPipelineState> batchedRectPipelineState;       // Active batched rect pipeline
    __strong id<MTLRenderPipelineState> batchedRectPipelineStateMSAA;   // Batched rects (4x MSAA)
    __strong id<MTLRenderPipelineState> batchedRectPipelineStateNoMSAA; // Batched rects (no MSAA)
    __strong id<MTLRenderPipelineState> batchedCirclePipelineState;       // Active batched circle pipeline
    __strong id<MTLRenderPipelineState> batchedCirclePipelineStateMSAA;   // Batched circles (4x MSAA)
    __strong id<MTLRenderPipelineState> batchedCirclePipelineStateNoMSAA; // Batched circles (no MSAA)
    __strong id<MTLRenderPipelineState> batchedStrokeRectPipelineState;       // Active batched stroke rect pipeline
    __strong id<MTLRenderPipelineState> batchedStrokeRectPipelineStateMSAA;   // Batched stroke rects (4x MSAA)
    __strong id<MTLRenderPipelineState> batchedStrokeRectPipelineStateNoMSAA; // Batched stroke rects (no MSAA)
    __strong id<MTLSamplerState> textSampler;                   // For text texture sampling
    __strong id<MTLSamplerState> spriteSampler;                 // For sprite texture sampling
    __strong id<MTLCommandBuffer> currentCommandBuffer;
    __strong id<MTLRenderCommandEncoder> currentEncoder;
    __strong id<CAMetalDrawable> currentDrawable;
    __strong id<MTLTexture> msaaTexture;  // 4x MSAA render target
    NSUInteger msaaWidth;        // Track size for recreation
    NSUInteger msaaHeight;
    // 3D rendering support
    __strong id<MTLTexture> depthTexture;           // Depth buffer (non-MSAA)
    __strong id<MTLTexture> msaaDepthTexture;       // Depth buffer (MSAA)
    __strong id<MTLDepthStencilState> depthState;   // Depth test state (enabled)
    __strong id<MTLDepthStencilState> depthStateDisabled; // Depth test disabled for 2D after 3D
    __strong id<MTLDepthStencilState> depthStateOcean;    // Ocean depth state (test on, no writes)
    __strong id<MTLRenderPipelineState> pipeline3D;       // Active 3D rendering pipeline
    __strong id<MTLRenderPipelineState> pipeline3DMSAA;   // 3D pipeline (4x MSAA)
    __strong id<MTLRenderPipelineState> pipeline3DNoMSAA; // 3D pipeline (no MSAA)
    __strong id<MTLRenderPipelineState> pipeline3DOcean;       // Active ocean projected-grid pipeline
    __strong id<MTLRenderPipelineState> pipeline3DOceanMSAA;   // Ocean pipeline (4x MSAA)
    __strong id<MTLRenderPipelineState> pipeline3DOceanNoMSAA; // Ocean pipeline (no MSAA)
    // Textured 3D rendering (for loaded assets with diffuse textures)
    __strong id<MTLRenderPipelineState> pipeline3DTextured;       // Active textured 3D pipeline
    __strong id<MTLRenderPipelineState> pipeline3DTexturedMSAA;   // Textured 3D pipeline (4x MSAA)
    __strong id<MTLRenderPipelineState> pipeline3DTexturedNoMSAA; // Textured 3D pipeline (no MSAA)
    __strong id<MTLSamplerState> texturedMeshSampler;             // Sampler for textured meshes
    __strong id<MTLBuffer> oceanIndexBuffer;
    uint32_t oceanIndexCount;
    uint32_t oceanGridSize;
    NSUInteger depthWidth;                 // Track depth texture size
    NSUInteger depthHeight;
    MTLClearColor clearColor;
    float screenWidth;   // Current screen dimensions for text rendering
    float screenHeight;
};

// Internal buffer structure
struct AfferentBuffer {
    __strong id<MTLBuffer> mtlBuffer;
    uint32_t count;
    bool persistent;
    bool pooled;
};

// ============================================================================
// Buffer Pool - Reuse MTLBuffers across frames to avoid allocation overhead
// ============================================================================

#define BUFFER_POOL_SIZE 64
#define MAX_BUFFER_SIZE (1024 * 1024)  // 1MB max per pooled buffer
#define WRAPPER_POOL_SIZE 256  // Pool for AfferentBuffer wrapper structs

typedef struct {
    id<MTLBuffer> buffer;
    size_t capacity;
    bool in_use;
} PooledBuffer;

typedef struct {
    PooledBuffer vertex_pool[BUFFER_POOL_SIZE];
    PooledBuffer index_pool[BUFFER_POOL_SIZE];
    int vertex_pool_count;
    int index_pool_count;
    // Wrapper struct pool to avoid malloc/free per draw call
    struct AfferentBuffer* wrapper_pool[WRAPPER_POOL_SIZE];
    int wrapper_pool_count;
    int wrapper_pool_used;
    // Text rendering buffer pools (separate from shape buffers)
    PooledBuffer text_vertex_pool[BUFFER_POOL_SIZE];
    PooledBuffer text_index_pool[BUFFER_POOL_SIZE];
    int text_vertex_pool_count;
    int text_index_pool_count;
} BufferPool;

// Global buffer pool
extern BufferPool g_buffer_pool;

// Staging buffer for text vertex conversion (reused across frames)
extern TextVertex* g_text_vertex_staging;
extern size_t g_text_vertex_staging_capacity;

// Buffer pool functions (buffer_pool.m)
struct AfferentBuffer* pool_acquire_wrapper(void);
id<MTLBuffer> pool_acquire_buffer(id<MTLDevice> device, PooledBuffer* pool, int* count, size_t required_size, bool is_vertex);
void pool_reset_frame(void);

// Persistent buffer creation (not pooled)
AfferentResult afferent_buffer_create_stroke_segment_persistent(
    AfferentRendererRef renderer,
    const AfferentStrokeSegment* segments,
    uint32_t segment_count,
    AfferentBufferRef* out_buffer
);

// Pipeline creation (pipeline.m)
AfferentResult create_pipelines(struct AfferentRenderer* renderer);
void ensureMSAATexture(AfferentRendererRef renderer, NSUInteger width, NSUInteger height);
void ensureDepthTexture(AfferentRendererRef renderer, NSUInteger width, NSUInteger height, bool msaa);

// Text rendering helpers (draw_text.m)
id<MTLTexture> ensureFontTexture(AfferentRendererRef renderer, AfferentFontRef font);
void updateFontTexture(AfferentRendererRef renderer, AfferentFontRef font);

// Sprite rendering helpers (draw_sprites.m)
id<MTLTexture> createMetalTexture(id<MTLDevice> device, const uint8_t* data, uint32_t width, uint32_t height);

// 3D rendering helpers (draw_3d.m)
void ensure_ocean_index_buffer(AfferentRendererRef renderer, uint32_t gridSize);

#endif // AFFERENT_METAL_RENDER_H

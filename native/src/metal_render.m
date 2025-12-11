#import <Metal/Metal.h>
#import <QuartzCore/CAMetalLayer.h>
#include "afferent.h"

// External declarations from window.m
extern id<MTLDevice> afferent_window_get_device(AfferentWindowRef window);
extern CAMetalLayer* afferent_window_get_metal_layer(AfferentWindowRef window);

// External declarations from text_render.c for atlas dirty tracking
extern int afferent_font_atlas_dirty(AfferentFontRef font);
extern void afferent_font_atlas_clear_dirty(AfferentFontRef font);

// Shader source embedded in code - basic colored vertices
static NSString *shaderSource = @R"(
#include <metal_stdlib>
using namespace metal;

struct VertexIn {
    float2 position [[attribute(0)]];
    float4 color [[attribute(1)]];
};

struct VertexOut {
    float4 position [[position]];
    float4 color;
};

vertex VertexOut vertex_main(VertexIn in [[stage_in]]) {
    VertexOut out;
    // Position is already in NDC (-1 to 1)
    out.position = float4(in.position, 0.0, 1.0);
    out.color = in.color;
    return out;
}

fragment float4 fragment_main(VertexOut in [[stage_in]]) {
    return in.color;
}
)";

// Text shader source - textured quads with alpha from texture
static NSString *textShaderSource = @R"(
#include <metal_stdlib>
using namespace metal;

struct TextVertexIn {
    float2 position [[attribute(0)]];
    float2 texCoord [[attribute(1)]];
    float4 color [[attribute(2)]];
};

struct TextVertexOut {
    float4 position [[position]];
    float2 texCoord;
    float4 color;
};

vertex TextVertexOut text_vertex_main(TextVertexIn in [[stage_in]]) {
    TextVertexOut out;
    out.position = float4(in.position, 0.0, 1.0);
    out.texCoord = in.texCoord;
    out.color = in.color;
    return out;
}

fragment float4 text_fragment_main(TextVertexOut in [[stage_in]],
                                   texture2d<float> tex [[texture(0)]],
                                   sampler smp [[sampler(0)]]) {
    float alpha = tex.sample(smp, in.texCoord).r;  // Single channel (grayscale) atlas
    return float4(in.color.rgb, in.color.a * alpha);
}
)";

// Instanced rectangle shader - GPU-side transforms for massive parallelism
static NSString *instancedShaderSource = @R"(
#include <metal_stdlib>
using namespace metal;

// Instance data: position(2) + angle(1) + halfSize(1) + color(4) = 8 floats
// Use packed layout to match the flat array from Lean
struct InstanceData {
    packed_float2 pos;       // Center position in NDC (8 bytes)
    float angle;             // Rotation angle in radians (4 bytes)
    float halfSize;          // Half side length in NDC (4 bytes)
    packed_float4 color;     // RGBA (16 bytes)
};  // Total: 32 bytes, no padding

struct InstancedVertexOut {
    float4 position [[position]];
    float4 color;
};

vertex InstancedVertexOut instanced_vertex_main(
    uint vid [[vertex_id]],
    uint iid [[instance_id]],
    constant InstanceData* instances [[buffer(0)]]
) {
    // Unit quad vertices for triangle strip: forms a quad with vertices 0,1,2,3
    // Order: bottom-left, bottom-right, top-left, top-right (Z pattern for strip)
    float2 unitQuad[4] = {
        float2(-1, -1),  // 0: bottom-left
        float2( 1, -1),  // 1: bottom-right
        float2(-1,  1),  // 2: top-left
        float2( 1,  1)   // 3: top-right
    };

    InstanceData inst = instances[iid];
    float2 v = unitQuad[vid];

    // Compute sin/cos on GPU (massively parallel!)
    float sinA = sin(inst.angle);
    float cosA = cos(inst.angle);

    // Rotate: v' = (v.x * cos - v.y * sin, v.x * sin + v.y * cos)
    float2 rotated = float2(
        v.x * cosA - v.y * sinA,
        v.x * sinA + v.y * cosA
    );

    // Scale and translate
    float2 finalPos = inst.pos + rotated * inst.halfSize;

    InstancedVertexOut out;
    out.position = float4(finalPos, 0.0, 1.0);
    out.color = inst.color;
    return out;
}

fragment float4 instanced_fragment_main(InstancedVertexOut in [[stage_in]]) {
    return in.color;
}
)";

// Text vertex structure (different layout than AfferentVertex)
typedef struct {
    float position[2];
    float texCoord[2];
    float color[4];
} TextVertex;

// Instance data structure (matches shader) - 32 bytes packed
typedef struct __attribute__((packed)) {
    float pos[2];       // Center position in NDC (8 bytes)
    float angle;        // Rotation angle in radians (4 bytes)
    float halfSize;     // Half side length in NDC (4 bytes)
    float color[4];     // RGBA (16 bytes)
} InstanceData;  // Total: 32 bytes

// Internal renderer structure
struct AfferentRenderer {
    AfferentWindowRef window;
    id<MTLDevice> device;
    id<MTLCommandQueue> commandQueue;
    id<MTLRenderPipelineState> pipelineState;
    id<MTLRenderPipelineState> textPipelineState;      // For text rendering
    id<MTLRenderPipelineState> instancedPipelineState; // For instanced rect rendering
    id<MTLSamplerState> textSampler;                   // For text texture sampling
    id<MTLCommandBuffer> currentCommandBuffer;
    id<MTLRenderCommandEncoder> currentEncoder;
    id<CAMetalDrawable> currentDrawable;
    id<MTLTexture> msaaTexture;  // 4x MSAA render target
    NSUInteger msaaWidth;        // Track size for recreation
    NSUInteger msaaHeight;
    MTLClearColor clearColor;
    float screenWidth;   // Current screen dimensions for text rendering
    float screenHeight;
};

// Internal buffer structure
struct AfferentBuffer {
    id<MTLBuffer> mtlBuffer;
    uint32_t count;
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
} BufferPool;

static BufferPool g_buffer_pool = {0};

// Get a wrapper struct from the pool (or allocate if pool is empty)
static struct AfferentBuffer* pool_acquire_wrapper(void) {
    if (g_buffer_pool.wrapper_pool_used < g_buffer_pool.wrapper_pool_count) {
        return g_buffer_pool.wrapper_pool[g_buffer_pool.wrapper_pool_used++];
    }
    // Pool exhausted, allocate new and try to add to pool
    struct AfferentBuffer* wrapper = malloc(sizeof(struct AfferentBuffer));
    if (g_buffer_pool.wrapper_pool_count < WRAPPER_POOL_SIZE) {
        g_buffer_pool.wrapper_pool[g_buffer_pool.wrapper_pool_count++] = wrapper;
        g_buffer_pool.wrapper_pool_used++;
    }
    return wrapper;
}

// Find or create a buffer of at least the required size
static id<MTLBuffer> pool_acquire_buffer(id<MTLDevice> device, PooledBuffer* pool, int* count, size_t required_size, bool is_vertex) {
    // First, try to find an existing buffer that's large enough and not in use
    for (int i = 0; i < *count; i++) {
        if (!pool[i].in_use && pool[i].capacity >= required_size) {
            pool[i].in_use = true;
            return pool[i].buffer;
        }
    }

    // No suitable buffer found - create a new one
    // Round up to power of 2 for better reuse
    size_t capacity = 4096;  // Minimum 4KB
    while (capacity < required_size && capacity < MAX_BUFFER_SIZE) {
        capacity *= 2;
    }
    if (capacity < required_size) {
        capacity = required_size;  // For very large buffers
    }

    id<MTLBuffer> newBuffer = [device newBufferWithLength:capacity
                                                  options:MTLResourceStorageModeShared];
    if (!newBuffer) {
        return nil;
    }

    // Add to pool if there's room
    if (*count < BUFFER_POOL_SIZE) {
        pool[*count].buffer = newBuffer;
        pool[*count].capacity = capacity;
        pool[*count].in_use = true;
        (*count)++;
    }
    // If pool is full, just return the buffer (it won't be pooled)

    return newBuffer;
}

// Mark all buffers as available for reuse (call at frame start)
static void pool_reset_frame(void) {
    for (int i = 0; i < g_buffer_pool.vertex_pool_count; i++) {
        g_buffer_pool.vertex_pool[i].in_use = false;
    }
    for (int i = 0; i < g_buffer_pool.index_pool_count; i++) {
        g_buffer_pool.index_pool[i].in_use = false;
    }
    // Reset wrapper pool (structs stay allocated, just reset usage counter)
    g_buffer_pool.wrapper_pool_used = 0;
}

AfferentResult afferent_renderer_create(
    AfferentWindowRef window,
    AfferentRendererRef* out_renderer
) {
    @autoreleasepool {
        struct AfferentRenderer *renderer = malloc(sizeof(struct AfferentRenderer));
        memset(renderer, 0, sizeof(struct AfferentRenderer));

        renderer->window = window;
        renderer->device = afferent_window_get_device(window);
        renderer->clearColor = MTLClearColorMake(0.0, 0.0, 0.0, 1.0);

        if (!renderer->device) {
            NSLog(@"No Metal device available");
            free(renderer);
            return AFFERENT_ERROR_DEVICE_FAILED;
        }

        // Create command queue
        renderer->commandQueue = [renderer->device newCommandQueue];
        if (!renderer->commandQueue) {
            NSLog(@"Failed to create command queue");
            free(renderer);
            return AFFERENT_ERROR_INIT_FAILED;
        }

        // Compile shaders
        NSError *error = nil;
        id<MTLLibrary> library = [renderer->device newLibraryWithSource:shaderSource
                                                                options:nil
                                                                  error:&error];
        if (!library) {
            NSLog(@"Shader compilation failed: %@", error);
            free(renderer);
            return AFFERENT_ERROR_PIPELINE_FAILED;
        }

        id<MTLFunction> vertexFunction = [library newFunctionWithName:@"vertex_main"];
        id<MTLFunction> fragmentFunction = [library newFunctionWithName:@"fragment_main"];

        if (!vertexFunction || !fragmentFunction) {
            NSLog(@"Failed to find shader functions");
            free(renderer);
            return AFFERENT_ERROR_PIPELINE_FAILED;
        }

        // Create vertex descriptor
        MTLVertexDescriptor *vertexDescriptor = [[MTLVertexDescriptor alloc] init];

        // Position: 2 floats at offset 0
        vertexDescriptor.attributes[0].format = MTLVertexFormatFloat2;
        vertexDescriptor.attributes[0].offset = offsetof(AfferentVertex, position);
        vertexDescriptor.attributes[0].bufferIndex = 0;

        // Color: 4 floats at offset 8 (after position)
        vertexDescriptor.attributes[1].format = MTLVertexFormatFloat4;
        vertexDescriptor.attributes[1].offset = offsetof(AfferentVertex, color);
        vertexDescriptor.attributes[1].bufferIndex = 0;

        // Layout
        vertexDescriptor.layouts[0].stride = sizeof(AfferentVertex);
        vertexDescriptor.layouts[0].stepFunction = MTLVertexStepFunctionPerVertex;

        // Create pipeline state
        MTLRenderPipelineDescriptor *pipelineDesc = [[MTLRenderPipelineDescriptor alloc] init];
        pipelineDesc.vertexFunction = vertexFunction;
        pipelineDesc.fragmentFunction = fragmentFunction;
        pipelineDesc.vertexDescriptor = vertexDescriptor;
        pipelineDesc.colorAttachments[0].pixelFormat = MTLPixelFormatBGRA8Unorm;
        pipelineDesc.rasterSampleCount = 4;  // Enable 4x MSAA

        // Enable blending for transparency
        pipelineDesc.colorAttachments[0].blendingEnabled = YES;
        pipelineDesc.colorAttachments[0].sourceRGBBlendFactor = MTLBlendFactorSourceAlpha;
        pipelineDesc.colorAttachments[0].destinationRGBBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
        pipelineDesc.colorAttachments[0].sourceAlphaBlendFactor = MTLBlendFactorOne;
        pipelineDesc.colorAttachments[0].destinationAlphaBlendFactor = MTLBlendFactorOneMinusSourceAlpha;

        renderer->pipelineState = [renderer->device newRenderPipelineStateWithDescriptor:pipelineDesc
                                                                                   error:&error];
        if (!renderer->pipelineState) {
            NSLog(@"Pipeline creation failed: %@", error);
            free(renderer);
            return AFFERENT_ERROR_PIPELINE_FAILED;
        }

        // Create text rendering pipeline
        id<MTLLibrary> textLibrary = [renderer->device newLibraryWithSource:textShaderSource
                                                                    options:nil
                                                                      error:&error];
        if (!textLibrary) {
            NSLog(@"Text shader compilation failed: %@", error);
            free(renderer);
            return AFFERENT_ERROR_PIPELINE_FAILED;
        }

        id<MTLFunction> textVertexFunction = [textLibrary newFunctionWithName:@"text_vertex_main"];
        id<MTLFunction> textFragmentFunction = [textLibrary newFunctionWithName:@"text_fragment_main"];

        if (!textVertexFunction || !textFragmentFunction) {
            NSLog(@"Failed to find text shader functions");
            free(renderer);
            return AFFERENT_ERROR_PIPELINE_FAILED;
        }

        // Create text vertex descriptor
        MTLVertexDescriptor *textVertexDescriptor = [[MTLVertexDescriptor alloc] init];

        // Position: 2 floats at offset 0
        textVertexDescriptor.attributes[0].format = MTLVertexFormatFloat2;
        textVertexDescriptor.attributes[0].offset = offsetof(TextVertex, position);
        textVertexDescriptor.attributes[0].bufferIndex = 0;

        // TexCoord: 2 floats at offset 8
        textVertexDescriptor.attributes[1].format = MTLVertexFormatFloat2;
        textVertexDescriptor.attributes[1].offset = offsetof(TextVertex, texCoord);
        textVertexDescriptor.attributes[1].bufferIndex = 0;

        // Color: 4 floats at offset 16
        textVertexDescriptor.attributes[2].format = MTLVertexFormatFloat4;
        textVertexDescriptor.attributes[2].offset = offsetof(TextVertex, color);
        textVertexDescriptor.attributes[2].bufferIndex = 0;

        // Layout
        textVertexDescriptor.layouts[0].stride = sizeof(TextVertex);
        textVertexDescriptor.layouts[0].stepFunction = MTLVertexStepFunctionPerVertex;

        // Create text pipeline state
        MTLRenderPipelineDescriptor *textPipelineDesc = [[MTLRenderPipelineDescriptor alloc] init];
        textPipelineDesc.vertexFunction = textVertexFunction;
        textPipelineDesc.fragmentFunction = textFragmentFunction;
        textPipelineDesc.vertexDescriptor = textVertexDescriptor;
        textPipelineDesc.colorAttachments[0].pixelFormat = MTLPixelFormatBGRA8Unorm;
        textPipelineDesc.rasterSampleCount = 4;  // Match MSAA

        // Enable blending for text
        textPipelineDesc.colorAttachments[0].blendingEnabled = YES;
        textPipelineDesc.colorAttachments[0].sourceRGBBlendFactor = MTLBlendFactorSourceAlpha;
        textPipelineDesc.colorAttachments[0].destinationRGBBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
        textPipelineDesc.colorAttachments[0].sourceAlphaBlendFactor = MTLBlendFactorOne;
        textPipelineDesc.colorAttachments[0].destinationAlphaBlendFactor = MTLBlendFactorOneMinusSourceAlpha;

        renderer->textPipelineState = [renderer->device newRenderPipelineStateWithDescriptor:textPipelineDesc
                                                                                       error:&error];
        if (!renderer->textPipelineState) {
            NSLog(@"Text pipeline creation failed: %@", error);
            free(renderer);
            return AFFERENT_ERROR_PIPELINE_FAILED;
        }

        // Create text sampler
        MTLSamplerDescriptor *samplerDesc = [[MTLSamplerDescriptor alloc] init];
        samplerDesc.minFilter = MTLSamplerMinMagFilterLinear;
        samplerDesc.magFilter = MTLSamplerMinMagFilterLinear;
        samplerDesc.sAddressMode = MTLSamplerAddressModeClampToEdge;
        samplerDesc.tAddressMode = MTLSamplerAddressModeClampToEdge;
        renderer->textSampler = [renderer->device newSamplerStateWithDescriptor:samplerDesc];

        // Create instanced rendering pipeline (for GPU-accelerated rectangle batches)
        id<MTLLibrary> instancedLibrary = [renderer->device newLibraryWithSource:instancedShaderSource
                                                                         options:nil
                                                                           error:&error];
        if (!instancedLibrary) {
            NSLog(@"Instanced shader compilation failed: %@", error);
            free(renderer);
            return AFFERENT_ERROR_PIPELINE_FAILED;
        }

        id<MTLFunction> instancedVertexFunction = [instancedLibrary newFunctionWithName:@"instanced_vertex_main"];
        id<MTLFunction> instancedFragmentFunction = [instancedLibrary newFunctionWithName:@"instanced_fragment_main"];

        if (!instancedVertexFunction || !instancedFragmentFunction) {
            NSLog(@"Failed to find instanced shader functions");
            free(renderer);
            return AFFERENT_ERROR_PIPELINE_FAILED;
        }

        // Instanced pipeline - no vertex descriptor needed (we use vertex_id and instance_id)
        MTLRenderPipelineDescriptor *instancedPipelineDesc = [[MTLRenderPipelineDescriptor alloc] init];
        instancedPipelineDesc.vertexFunction = instancedVertexFunction;
        instancedPipelineDesc.fragmentFunction = instancedFragmentFunction;
        instancedPipelineDesc.colorAttachments[0].pixelFormat = MTLPixelFormatBGRA8Unorm;
        instancedPipelineDesc.rasterSampleCount = 4;  // Match MSAA

        // Enable blending
        instancedPipelineDesc.colorAttachments[0].blendingEnabled = YES;
        instancedPipelineDesc.colorAttachments[0].sourceRGBBlendFactor = MTLBlendFactorSourceAlpha;
        instancedPipelineDesc.colorAttachments[0].destinationRGBBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
        instancedPipelineDesc.colorAttachments[0].sourceAlphaBlendFactor = MTLBlendFactorOne;
        instancedPipelineDesc.colorAttachments[0].destinationAlphaBlendFactor = MTLBlendFactorOneMinusSourceAlpha;

        renderer->instancedPipelineState = [renderer->device newRenderPipelineStateWithDescriptor:instancedPipelineDesc
                                                                                            error:&error];
        if (!renderer->instancedPipelineState) {
            NSLog(@"Instanced pipeline creation failed: %@", error);
            free(renderer);
            return AFFERENT_ERROR_PIPELINE_FAILED;
        }

        *out_renderer = renderer;
        return AFFERENT_OK;
    }
}

void afferent_renderer_destroy(AfferentRendererRef renderer) {
    if (renderer) {
        free(renderer);
    }
}

// Helper function to create or recreate MSAA texture if needed
static void ensureMSAATexture(AfferentRendererRef renderer, NSUInteger width, NSUInteger height) {
    if (renderer->msaaTexture &&
        renderer->msaaWidth == width &&
        renderer->msaaHeight == height) {
        return;  // Already have correct size
    }

    // Create new MSAA texture
    MTLTextureDescriptor *msaaDesc = [MTLTextureDescriptor texture2DDescriptorWithPixelFormat:MTLPixelFormatBGRA8Unorm
                                                                                        width:width
                                                                                       height:height
                                                                                    mipmapped:NO];
    msaaDesc.textureType = MTLTextureType2DMultisample;
    msaaDesc.sampleCount = 4;
    msaaDesc.usage = MTLTextureUsageRenderTarget;
    msaaDesc.storageMode = MTLStorageModePrivate;  // GPU-only, no CPU access needed

    renderer->msaaTexture = [renderer->device newTextureWithDescriptor:msaaDesc];
    renderer->msaaWidth = width;
    renderer->msaaHeight = height;
}

AfferentResult afferent_renderer_begin_frame(AfferentRendererRef renderer, float r, float g, float b, float a) {
    @autoreleasepool {
        // Reset buffer pool at frame start - all buffers become available for reuse
        pool_reset_frame();

        CAMetalLayer *metalLayer = afferent_window_get_metal_layer(renderer->window);
        if (!metalLayer) {
            return AFFERENT_ERROR_INIT_FAILED;
        }

        renderer->currentDrawable = [metalLayer nextDrawable];
        if (!renderer->currentDrawable) {
            return AFFERENT_ERROR_INIT_FAILED;
        }

        renderer->currentCommandBuffer = [renderer->commandQueue commandBuffer];
        if (!renderer->currentCommandBuffer) {
            return AFFERENT_ERROR_INIT_FAILED;
        }

        // Ensure MSAA texture matches drawable size
        id<MTLTexture> drawableTexture = renderer->currentDrawable.texture;
        ensureMSAATexture(renderer, drawableTexture.width, drawableTexture.height);

        // Store screen dimensions for text rendering
        renderer->screenWidth = drawableTexture.width;
        renderer->screenHeight = drawableTexture.height;

        // Set up render pass with MSAA
        MTLRenderPassDescriptor *passDesc = [MTLRenderPassDescriptor renderPassDescriptor];
        passDesc.colorAttachments[0].texture = renderer->msaaTexture;        // Render to MSAA texture
        passDesc.colorAttachments[0].resolveTexture = drawableTexture;       // Resolve to drawable
        passDesc.colorAttachments[0].loadAction = MTLLoadActionClear;
        passDesc.colorAttachments[0].storeAction = MTLStoreActionMultisampleResolve;  // Resolve on store
        passDesc.colorAttachments[0].clearColor = MTLClearColorMake(r, g, b, a);

        renderer->currentEncoder = [renderer->currentCommandBuffer renderCommandEncoderWithDescriptor:passDesc];
        if (!renderer->currentEncoder) {
            return AFFERENT_ERROR_INIT_FAILED;
        }

        [renderer->currentEncoder setRenderPipelineState:renderer->pipelineState];

        return AFFERENT_OK;
    }
}

AfferentResult afferent_renderer_end_frame(AfferentRendererRef renderer) {
    @autoreleasepool {
        if (renderer->currentEncoder) {
            [renderer->currentEncoder endEncoding];
            renderer->currentEncoder = nil;
        }

        if (renderer->currentCommandBuffer && renderer->currentDrawable) {
            [renderer->currentCommandBuffer presentDrawable:renderer->currentDrawable];
            [renderer->currentCommandBuffer commit];
        }

        renderer->currentCommandBuffer = nil;
        renderer->currentDrawable = nil;

        return AFFERENT_OK;
    }
}

AfferentResult afferent_buffer_create_vertex(
    AfferentRendererRef renderer,
    const AfferentVertex* vertices,
    uint32_t vertex_count,
    AfferentBufferRef* out_buffer
) {
    @autoreleasepool {
        size_t required_size = vertex_count * sizeof(AfferentVertex);

        // Get a buffer from the pool (or create a new one)
        id<MTLBuffer> mtlBuffer = pool_acquire_buffer(
            renderer->device,
            g_buffer_pool.vertex_pool,
            &g_buffer_pool.vertex_pool_count,
            required_size,
            true
        );

        if (!mtlBuffer) {
            return AFFERENT_ERROR_BUFFER_FAILED;
        }

        // Copy vertex data into the pooled buffer
        memcpy(mtlBuffer.contents, vertices, required_size);

        // Get wrapper struct from pool (avoids malloc per draw call)
        struct AfferentBuffer *buffer = pool_acquire_wrapper();
        buffer->count = vertex_count;
        buffer->mtlBuffer = mtlBuffer;
        *out_buffer = buffer;
        return AFFERENT_OK;
    }
}

AfferentResult afferent_buffer_create_index(
    AfferentRendererRef renderer,
    const uint32_t* indices,
    uint32_t index_count,
    AfferentBufferRef* out_buffer
) {
    @autoreleasepool {
        size_t required_size = index_count * sizeof(uint32_t);

        // Get a buffer from the pool (or create a new one)
        id<MTLBuffer> mtlBuffer = pool_acquire_buffer(
            renderer->device,
            g_buffer_pool.index_pool,
            &g_buffer_pool.index_pool_count,
            required_size,
            false
        );

        if (!mtlBuffer) {
            return AFFERENT_ERROR_BUFFER_FAILED;
        }

        // Copy index data into the pooled buffer
        memcpy(mtlBuffer.contents, indices, required_size);

        // Get wrapper struct from pool (avoids malloc per draw call)
        struct AfferentBuffer *buffer = pool_acquire_wrapper();
        buffer->count = index_count;
        buffer->mtlBuffer = mtlBuffer;
        *out_buffer = buffer;
        return AFFERENT_OK;
    }
}

void afferent_buffer_destroy(AfferentBufferRef buffer) {
    // Note: We don't destroy anything here anymore.
    // MTLBuffers stay in the pool for reuse, and wrapper structs
    // are pooled and recycled at frame boundaries.
    // This function is kept for API compatibility but is now a no-op.
    (void)buffer;
}

void afferent_renderer_draw_triangles(
    AfferentRendererRef renderer,
    AfferentBufferRef vertex_buffer,
    AfferentBufferRef index_buffer,
    uint32_t index_count
) {
    if (!renderer->currentEncoder || !vertex_buffer || !index_buffer) {
        return;
    }

    // Ensure we're using the basic pipeline (not text pipeline)
    [renderer->currentEncoder setRenderPipelineState:renderer->pipelineState];

    [renderer->currentEncoder setVertexBuffer:vertex_buffer->mtlBuffer offset:0 atIndex:0];

    [renderer->currentEncoder drawIndexedPrimitives:MTLPrimitiveTypeTriangle
                                         indexCount:index_count
                                          indexType:MTLIndexTypeUInt32
                                        indexBuffer:index_buffer->mtlBuffer
                                  indexBufferOffset:0];
}

// Draw instanced rectangles - GPU computes transforms
// instance_data: array of 9 floats per instance (pos.x, pos.y, sin, cos, halfSize, r, g, b, a)
void afferent_renderer_draw_instanced_rects(
    AfferentRendererRef renderer,
    const float* instance_data,
    uint32_t instance_count
) {
    if (!renderer || !renderer->currentEncoder || !instance_data || instance_count == 0) {
        return;
    }

    @autoreleasepool {
        // Create buffer for instance data
        size_t data_size = instance_count * sizeof(InstanceData);
        id<MTLBuffer> instanceBuffer = pool_acquire_buffer(
            renderer->device,
            g_buffer_pool.vertex_pool,
            &g_buffer_pool.vertex_pool_count,
            data_size,
            true
        );

        if (!instanceBuffer) {
            NSLog(@"Failed to create instance buffer");
            return;
        }

        // Copy instance data
        memcpy(instanceBuffer.contents, instance_data, data_size);

        // Switch to instanced pipeline
        [renderer->currentEncoder setRenderPipelineState:renderer->instancedPipelineState];

        // Bind instance buffer
        [renderer->currentEncoder setVertexBuffer:instanceBuffer offset:0 atIndex:0];

        // Draw: 4 vertices per quad (triangle strip would be 4, but we use 2 triangles = 6 indices)
        // Actually we use drawPrimitives with triangle strip for simplicity
        [renderer->currentEncoder drawPrimitives:MTLPrimitiveTypeTriangleStrip
                                     vertexStart:0
                                     vertexCount:4
                                   instanceCount:instance_count];

        // Switch back to basic pipeline
        [renderer->currentEncoder setRenderPipelineState:renderer->pipelineState];
    }
}

void afferent_renderer_set_scissor(
    AfferentRendererRef renderer,
    uint32_t x,
    uint32_t y,
    uint32_t width,
    uint32_t height
) {
    if (!renderer || !renderer->currentEncoder) {
        return;
    }

    // Clamp scissor to render target bounds
    NSUInteger maxW = (NSUInteger)renderer->screenWidth;
    NSUInteger maxH = (NSUInteger)renderer->screenHeight;

    NSUInteger sx = (NSUInteger)x;
    NSUInteger sy = (NSUInteger)y;
    NSUInteger sw = (NSUInteger)width;
    NSUInteger sh = (NSUInteger)height;

    // Ensure scissor doesn't exceed render target
    if (sx + sw > maxW) sw = maxW - sx;
    if (sy + sh > maxH) sh = maxH - sy;

    MTLScissorRect scissor;
    scissor.x = sx;
    scissor.y = sy;
    scissor.width = sw;
    scissor.height = sh;

    [renderer->currentEncoder setScissorRect:scissor];
}

void afferent_renderer_reset_scissor(AfferentRendererRef renderer) {
    if (!renderer || !renderer->currentEncoder) {
        return;
    }

    // Reset to full drawable size
    MTLScissorRect scissor;
    scissor.x = 0;
    scissor.y = 0;
    scissor.width = (NSUInteger)renderer->screenWidth;
    scissor.height = (NSUInteger)renderer->screenHeight;
    [renderer->currentEncoder setScissorRect:scissor];
}

// External functions from text_render.c
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

// Create or update font atlas texture
static id<MTLTexture> ensureFontTexture(AfferentRendererRef renderer, AfferentFontRef font) {
    void* stored_texture = afferent_font_get_metal_texture(font);
    id<MTLTexture> texture = (__bridge id<MTLTexture>)stored_texture;

    if (!texture) {
        // Create texture from atlas data
        uint8_t* atlas_data = afferent_font_get_atlas_data(font);
        uint32_t atlas_width = afferent_font_get_atlas_width(font);
        uint32_t atlas_height = afferent_font_get_atlas_height(font);

        MTLTextureDescriptor *desc = [MTLTextureDescriptor texture2DDescriptorWithPixelFormat:MTLPixelFormatR8Unorm
                                                                                        width:atlas_width
                                                                                       height:atlas_height
                                                                                    mipmapped:NO];
        desc.usage = MTLTextureUsageShaderRead;

        texture = [renderer->device newTextureWithDescriptor:desc];

        MTLRegion region = MTLRegionMake2D(0, 0, atlas_width, atlas_height);
        [texture replaceRegion:region mipmapLevel:0 withBytes:atlas_data bytesPerRow:atlas_width];

        // Use __bridge_retained to transfer ownership to the C struct
        // This prevents ARC from releasing the texture when the function returns
        afferent_font_set_metal_texture(font, (__bridge_retained void*)texture);
    }

    return texture;
}

// Update the font texture with new glyph data (only if atlas has changed)
static void updateFontTexture(AfferentRendererRef renderer, AfferentFontRef font) {
    // Only upload if new glyphs were added to the atlas
    if (!afferent_font_atlas_dirty(font)) {
        return;
    }

    id<MTLTexture> texture = (__bridge id<MTLTexture>)afferent_font_get_metal_texture(font);
    if (texture) {
        uint8_t* atlas_data = afferent_font_get_atlas_data(font);
        uint32_t atlas_width = afferent_font_get_atlas_width(font);
        uint32_t atlas_height = afferent_font_get_atlas_height(font);

        MTLRegion region = MTLRegionMake2D(0, 0, atlas_width, atlas_height);
        [texture replaceRegion:region mipmapLevel:0 withBytes:atlas_data bytesPerRow:atlas_width];

        // Clear dirty flag after successful upload
        afferent_font_atlas_clear_dirty(font);
    }
}

// Render text using the text pipeline
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
) {
    @autoreleasepool {
        if (!renderer || !renderer->currentEncoder || !font || !text || text[0] == '\0') {
            return AFFERENT_OK;  // Nothing to render
        }

        // Generate vertex data
        float* vertices = NULL;
        uint32_t* indices = NULL;
        uint32_t vertex_count = 0;
        uint32_t index_count = 0;

        // Use the canvas dimensions (not current drawable size) for NDC conversion
        // This ensures text scales consistently with shapes when the window is resized
        int success = afferent_text_generate_vertices(
            font, text, x, y, r, g, b, a,
            canvas_width, canvas_height,
            transform,
            &vertices, &indices, &vertex_count, &index_count
        );

        if (!success || vertex_count == 0) {
            free(vertices);
            free(indices);
            return AFFERENT_OK;
        }

        // Ensure font texture is created and up to date
        id<MTLTexture> fontTexture = ensureFontTexture(renderer, font);
        updateFontTexture(renderer, font);

        // Convert float vertex data to TextVertex format
        TextVertex* textVertices = malloc(vertex_count * sizeof(TextVertex));
        for (uint32_t i = 0; i < vertex_count; i++) {
            size_t base = i * 8;  // 8 floats per vertex
            textVertices[i].position[0] = vertices[base + 0];
            textVertices[i].position[1] = vertices[base + 1];
            textVertices[i].texCoord[0] = vertices[base + 2];
            textVertices[i].texCoord[1] = vertices[base + 3];
            textVertices[i].color[0] = vertices[base + 4];
            textVertices[i].color[1] = vertices[base + 5];
            textVertices[i].color[2] = vertices[base + 6];
            textVertices[i].color[3] = vertices[base + 7];
        }

        // Create Metal buffers
        id<MTLBuffer> vertexBuffer = [renderer->device newBufferWithBytes:textVertices
                                                                   length:vertex_count * sizeof(TextVertex)
                                                                  options:MTLResourceStorageModeShared];
        id<MTLBuffer> indexBuffer = [renderer->device newBufferWithBytes:indices
                                                                  length:index_count * sizeof(uint32_t)
                                                                 options:MTLResourceStorageModeShared];

        free(textVertices);
        free(vertices);
        free(indices);

        if (!vertexBuffer || !indexBuffer) {
            return AFFERENT_ERROR_TEXT_FAILED;
        }

        // Switch to text pipeline
        [renderer->currentEncoder setRenderPipelineState:renderer->textPipelineState];

        // Set texture and sampler
        [renderer->currentEncoder setFragmentTexture:fontTexture atIndex:0];
        [renderer->currentEncoder setFragmentSamplerState:renderer->textSampler atIndex:0];

        // Draw text quads
        [renderer->currentEncoder setVertexBuffer:vertexBuffer offset:0 atIndex:0];
        [renderer->currentEncoder drawIndexedPrimitives:MTLPrimitiveTypeTriangle
                                             indexCount:index_count
                                              indexType:MTLIndexTypeUInt32
                                            indexBuffer:indexBuffer
                                      indexBufferOffset:0];

        // Switch back to basic pipeline for subsequent drawing
        [renderer->currentEncoder setRenderPipelineState:renderer->pipelineState];

        return AFFERENT_OK;
    }
}

// Helper to get renderer screen dimensions (for Lean FFI)
float afferent_renderer_get_screen_width(AfferentRendererRef renderer) {
    return renderer ? renderer->screenWidth : 0;
}

float afferent_renderer_get_screen_height(AfferentRendererRef renderer) {
    return renderer ? renderer->screenHeight : 0;
}

// Release a retained Metal texture (called from text_render.c when font is destroyed)
void afferent_release_metal_texture(void* texture_ptr) {
    if (texture_ptr) {
        // Transfer ownership back to ARC so it can release the texture
        id<MTLTexture> texture = (__bridge_transfer id<MTLTexture>)texture_ptr;
        (void)texture;  // Let ARC release it
    }
}

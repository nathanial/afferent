// render.m - Main renderer module: lifecycle, frame management, buffer creation
#import "render.h"

// Include all sub-modules (compiled separately but included here for single translation unit)
// Note: These are compiled as separate .m files but share headers through render.h
#import "shaders.m"
#import "buffer_pool.m"
#import "pipeline.m"
#import "draw_2d.m"
#import "draw_text.m"
#import "draw_sprites.m"
#import "draw_3d.m"

// ============================================================================
// Renderer Creation and Destruction
// ============================================================================

AfferentResult afferent_renderer_create(
    AfferentWindowRef window,
    AfferentRendererRef* out_renderer
) {
    @autoreleasepool {
        id<MTLDevice> device = afferent_window_get_device(window);
        if (!device) {
            NSLog(@"Failed to get Metal device from window");
            return AFFERENT_ERROR_DEVICE_FAILED;
        }

        struct AfferentRenderer *renderer = calloc(1, sizeof(struct AfferentRenderer));
        if (!renderer) {
            return AFFERENT_ERROR_INIT_FAILED;
        }

        renderer->window = window;
        renderer->device = device;
        renderer->commandQueue = [device newCommandQueue];
        renderer->drawableScaleOverride = 0.0f;

        if (!renderer->commandQueue) {
            NSLog(@"Failed to create command queue");
            free(renderer);
            return AFFERENT_ERROR_INIT_FAILED;
        }

        // Load shaders from external files
        if (!afferent_init_shaders()) {
            NSLog(@"Failed to load shaders");
            free(renderer);
            return AFFERENT_ERROR_INIT_FAILED;
        }

        // Create all pipelines
        AfferentResult pipelineResult = create_pipelines(renderer);
        if (pipelineResult != AFFERENT_OK) {
            free(renderer);
            return pipelineResult;
        }

        // Initialize ocean-related fields
        renderer->oceanIndexBuffer = nil;
        renderer->oceanIndexCount = 0;
        renderer->oceanGridSize = 0;

        // Initialize depth texture pointers
        renderer->msaaColorTexture = nil;
        renderer->depthTexture = nil;
        renderer->depthWidth = 0;
        renderer->depthHeight = 0;
        renderer->msaaWidth = 0;
        renderer->msaaHeight = 0;

        *out_renderer = renderer;
        return AFFERENT_OK;
    }
}

void afferent_renderer_destroy(AfferentRendererRef renderer) {
    if (renderer) {
        @autoreleasepool {
            renderer->currentEncoder = nil;
            renderer->currentCommandBuffer = nil;
            renderer->currentDrawable = nil;

            renderer->msaaColorTexture = nil;
            renderer->depthTexture = nil;
            renderer->depthState = nil;
            renderer->depthStateDisabled = nil;

            renderer->pipelineState = nil;
            renderer->strokePipelineState = nil;
            renderer->textPipelineState = nil;
            renderer->spritePipelineState = nil;
            renderer->instancedPipelineState = nil;
            renderer->pipeline3D = nil;
            renderer->pipeline3DOcean = nil;
            renderer->pipeline3DTextured = nil;

            renderer->textSampler = nil;
            renderer->spriteSampler = nil;
            renderer->texturedMeshSampler = nil;

            renderer->oceanIndexBuffer = nil;

            renderer->commandQueue = nil;
            renderer->device = nil;
            renderer->window = NULL;
        }
        free(renderer);
    }
}

// ============================================================================
// Internal Accessors
// ============================================================================

id<MTLDevice> afferent_renderer_get_device(AfferentRendererRef renderer) {
    return renderer ? renderer->device : nil;
}

id<MTLRenderCommandEncoder> afferent_renderer_get_encoder(AfferentRendererRef renderer) {
    return renderer ? renderer->currentEncoder : nil;
}

// ============================================================================
// Drawable Scale Control
// ============================================================================

// Enable a drawable scale override (typically 1.0 to disable Retina).
// Pass scale <= 0 to restore native backing scale.
void afferent_renderer_set_drawable_scale(AfferentRendererRef renderer, float scale) {
    if (!renderer) return;
    renderer->drawableScaleOverride = scale;
    CAMetalLayer *metalLayer = afferent_window_get_metal_layer(renderer->window);
    if (!metalLayer) return;
    CGSize boundsSize = metalLayer.bounds.size;
    if (scale > 0.0f) {
        metalLayer.drawableSize = CGSizeMake(boundsSize.width * scale, boundsSize.height * scale);
    } else {
        CGFloat nativeScale = metalLayer.contentsScale;
        if (nativeScale <= 0.0) nativeScale = 1.0;
        metalLayer.drawableSize = CGSizeMake(boundsSize.width * nativeScale, boundsSize.height * nativeScale);
    }
}

// ============================================================================
// Frame Management
// ============================================================================

AfferentResult afferent_renderer_begin_frame(AfferentRendererRef renderer, float r, float g, float b, float a) {
    @autoreleasepool {
        // Reset buffer pool at frame start - all buffers become available for reuse
        pool_reset_frame();

        CAMetalLayer *metalLayer = afferent_window_get_metal_layer(renderer->window);
        if (!metalLayer) {
            return AFFERENT_ERROR_INIT_FAILED;
        }

        // Re-apply drawable scale override each frame (handles window resizes)
        if (renderer->drawableScaleOverride > 0.0f) {
            CGSize boundsSize = metalLayer.bounds.size;
            float s = renderer->drawableScaleOverride;
            metalLayer.drawableSize = CGSizeMake(boundsSize.width * s, boundsSize.height * s);
        }

        renderer->currentDrawable = [metalLayer nextDrawable];
        if (!renderer->currentDrawable) {
            return AFFERENT_ERROR_INIT_FAILED;
        }

        renderer->currentCommandBuffer = [renderer->commandQueue commandBuffer];
        if (!renderer->currentCommandBuffer) {
            return AFFERENT_ERROR_INIT_FAILED;
        }

        id<MTLTexture> drawableTexture = renderer->currentDrawable.texture;

        // Store screen dimensions for text rendering
        renderer->screenWidth = drawableTexture.width;
        renderer->screenHeight = drawableTexture.height;

        MTLRenderPassDescriptor *passDesc = [MTLRenderPassDescriptor renderPassDescriptor];
        passDesc.colorAttachments[0].loadAction = MTLLoadActionClear;
        passDesc.colorAttachments[0].clearColor = MTLClearColorMake(r, g, b, a);

        ensureMSAATexture(renderer, drawableTexture.width, drawableTexture.height);
        ensureDepthTexture(renderer, drawableTexture.width, drawableTexture.height);
        if (!renderer->msaaColorTexture || !renderer->depthTexture) {
            return AFFERENT_ERROR_INIT_FAILED;
        }
        passDesc.colorAttachments[0].texture = renderer->msaaColorTexture;
        passDesc.colorAttachments[0].resolveTexture = drawableTexture;
        passDesc.colorAttachments[0].storeAction = MTLStoreActionMultisampleResolve;
        passDesc.depthAttachment.texture = renderer->depthTexture;
        passDesc.depthAttachment.loadAction = MTLLoadActionClear;
        passDesc.depthAttachment.storeAction = MTLStoreActionDontCare;
        passDesc.depthAttachment.clearDepth = 1.0;

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

// ============================================================================
// Buffer Creation
// ============================================================================

static AfferentResult afferent_buffer_create_pooled(
    AfferentRendererRef renderer,
    PooledBuffer* pool,
    int* pool_count,
    const void* data,
    uint32_t element_count,
    size_t element_size,
    AfferentBufferRef* out_buffer
) {
    @autoreleasepool {
        size_t required_size = (size_t)element_count * element_size;

        id<MTLBuffer> mtlBuffer = pool_acquire_buffer(
            renderer->device,
            pool,
            pool_count,
            required_size
        );

        if (!mtlBuffer) {
            return AFFERENT_ERROR_BUFFER_FAILED;
        }

        memcpy(mtlBuffer.contents, data, required_size);

        struct AfferentBuffer *buffer = pool_acquire_wrapper();
        buffer->count = element_count;
        buffer->mtlBuffer = mtlBuffer;
        buffer->persistent = false;
        *out_buffer = buffer;
        return AFFERENT_OK;
    }
}

AfferentResult afferent_buffer_create_vertex(
    AfferentRendererRef renderer,
    const AfferentVertex* vertices,
    uint32_t vertex_count,
    AfferentBufferRef* out_buffer
) {
    return afferent_buffer_create_pooled(
        renderer,
        g_buffer_pool.vertex_pool,
        &g_buffer_pool.vertex_pool_count,
        vertices,
        vertex_count,
        sizeof(AfferentVertex),
        out_buffer
    );
}

AfferentResult afferent_buffer_create_stroke_vertex(
    AfferentRendererRef renderer,
    const AfferentStrokeVertex* vertices,
    uint32_t vertex_count,
    AfferentBufferRef* out_buffer
) {
    return afferent_buffer_create_pooled(
        renderer,
        g_buffer_pool.vertex_pool,
        &g_buffer_pool.vertex_pool_count,
        vertices,
        vertex_count,
        sizeof(AfferentStrokeVertex),
        out_buffer
    );
}

AfferentResult afferent_buffer_create_stroke_segment(
    AfferentRendererRef renderer,
    const AfferentStrokeSegment* segments,
    uint32_t segment_count,
    AfferentBufferRef* out_buffer
) {
    return afferent_buffer_create_pooled(
        renderer,
        g_buffer_pool.vertex_pool,
        &g_buffer_pool.vertex_pool_count,
        segments,
        segment_count,
        sizeof(AfferentStrokeSegment),
        out_buffer
    );
}

AfferentResult afferent_buffer_create_stroke_segment_persistent(
    AfferentRendererRef renderer,
    const AfferentStrokeSegment* segments,
    uint32_t segment_count,
    AfferentBufferRef* out_buffer
) {
    if (!renderer || !segments || segment_count == 0 || !out_buffer) {
        return AFFERENT_ERROR_BUFFER_FAILED;
    }

    @autoreleasepool {
        size_t required_size = segment_count * sizeof(AfferentStrokeSegment);
        id<MTLBuffer> mtlBuffer = [renderer->device newBufferWithBytes:segments
                                                                length:required_size
                                                               options:MTLResourceStorageModeShared];
        if (!mtlBuffer) {
            return AFFERENT_ERROR_BUFFER_FAILED;
        }

        struct AfferentBuffer *buffer = malloc(sizeof(struct AfferentBuffer));
        if (!buffer) {
            return AFFERENT_ERROR_BUFFER_FAILED;
        }
        buffer->count = segment_count;
        buffer->mtlBuffer = mtlBuffer;
        buffer->persistent = true;
        buffer->pooled = false;
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
    return afferent_buffer_create_pooled(
        renderer,
        g_buffer_pool.index_pool,
        &g_buffer_pool.index_pool_count,
        indices,
        index_count,
        sizeof(uint32_t),
        out_buffer
    );
}

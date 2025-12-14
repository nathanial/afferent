// render.m - Main renderer module: lifecycle, frame management, buffer creation
#import "render.h"

// Include all sub-modules (compiled separately but included here for single translation unit)
// Note: These are compiled as separate .m files but share headers through render.h
#import "shaders.m"
#import "buffer_pool.m"
#import "pipeline.m"
#import "draw_2d.m"
#import "draw_text.m"
#import "draw_animated.m"
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
        renderer->depthTexture = nil;
        renderer->msaaDepthTexture = nil;
        renderer->depthWidth = 0;
        renderer->depthHeight = 0;

        // Initialize animated buffer pointers to nil
        renderer->animatedRectBuffer = nil;
        renderer->animatedTriangleBuffer = nil;
        renderer->animatedCircleBuffer = nil;
        renderer->orbitalBuffer = nil;
        renderer->animatedRectCount = 0;
        renderer->animatedTriangleCount = 0;
        renderer->animatedCircleCount = 0;
        renderer->orbitalCount = 0;
        renderer->orbitalCenterX = 0;
        renderer->orbitalCenterY = 0;

        *out_renderer = renderer;
        return AFFERENT_OK;
    }
}

void afferent_renderer_destroy(AfferentRendererRef renderer) {
    if (renderer) {
        free(renderer);
    }
}

// ============================================================================
// MSAA and Drawable Scale Control
// ============================================================================

// Toggle MSAA for subsequent frames. This only switches the active pipelines and
// beginFrame render pass configuration; it doesn't rebuild resources.
void afferent_renderer_set_msaa_enabled(AfferentRendererRef renderer, bool enabled) {
    if (!renderer) return;
    renderer->msaaEnabled = enabled;
    renderer->pipelineState = enabled ? renderer->pipelineStateMSAA : renderer->pipelineStateNoMSAA;
    renderer->textPipelineState = enabled ? renderer->textPipelineStateMSAA : renderer->textPipelineStateNoMSAA;
    renderer->spritePipelineState = enabled ? renderer->spritePipelineStateMSAA : renderer->spritePipelineStateNoMSAA;
    renderer->pipeline3D = enabled ? renderer->pipeline3DMSAA : renderer->pipeline3DNoMSAA;
    renderer->pipeline3DOcean = enabled ? renderer->pipeline3DOceanMSAA : renderer->pipeline3DOceanNoMSAA;
}

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

        if (renderer->msaaEnabled) {
            // Ensure MSAA texture matches drawable size
            ensureMSAATexture(renderer, drawableTexture.width, drawableTexture.height);
            // Ensure MSAA depth texture
            ensureDepthTexture(renderer, drawableTexture.width, drawableTexture.height, true);
            // Render to MSAA texture and resolve to drawable
            passDesc.colorAttachments[0].texture = renderer->msaaTexture;
            passDesc.colorAttachments[0].resolveTexture = drawableTexture;
            passDesc.colorAttachments[0].storeAction = MTLStoreActionMultisampleResolve;
            // Attach depth buffer for 3D rendering
            passDesc.depthAttachment.texture = renderer->msaaDepthTexture;
            passDesc.depthAttachment.loadAction = MTLLoadActionClear;
            passDesc.depthAttachment.storeAction = MTLStoreActionDontCare;
            passDesc.depthAttachment.clearDepth = 1.0;
        } else {
            // Ensure non-MSAA depth texture
            ensureDepthTexture(renderer, drawableTexture.width, drawableTexture.height, false);
            // Render directly to drawable without MSAA
            passDesc.colorAttachments[0].texture = drawableTexture;
            passDesc.colorAttachments[0].resolveTexture = nil;
            passDesc.colorAttachments[0].storeAction = MTLStoreActionStore;
            // Attach depth buffer for 3D rendering
            passDesc.depthAttachment.texture = renderer->depthTexture;
            passDesc.depthAttachment.loadAction = MTLLoadActionClear;
            passDesc.depthAttachment.storeAction = MTLStoreActionDontCare;
            passDesc.depthAttachment.clearDepth = 1.0;
        }

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

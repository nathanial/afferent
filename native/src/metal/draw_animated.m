// draw_animated.m - Animated, orbital, and dynamic shapes
#import "render.h"

// ============================================================================
// ANIMATED RENDERING - GPU-side animation for maximum performance
// Static data uploaded once, only time uniform sent per frame
// ============================================================================

// Upload static instance data for animated rects (called once at startup)
// data: [pixelX, pixelY, hueBase, halfSizePixels, phaseOffset, spinSpeed] × count
void afferent_renderer_upload_animated_rects(
    AfferentRendererRef renderer,
    const float* data,
    uint32_t count
) {
    if (!renderer || !data || count == 0) return;

    @autoreleasepool {
        size_t size = count * sizeof(AnimatedInstanceData);
        renderer->animatedRectBuffer = [renderer->device newBufferWithBytes:data
                                                                     length:size
                                                                    options:MTLResourceStorageModeShared];
        renderer->animatedRectCount = count;
    }
}

// Upload static instance data for animated triangles
void afferent_renderer_upload_animated_triangles(
    AfferentRendererRef renderer,
    const float* data,
    uint32_t count
) {
    if (!renderer || !data || count == 0) return;

    @autoreleasepool {
        size_t size = count * sizeof(AnimatedInstanceData);
        renderer->animatedTriangleBuffer = [renderer->device newBufferWithBytes:data
                                                                         length:size
                                                                        options:MTLResourceStorageModeShared];
        renderer->animatedTriangleCount = count;
    }
}

// Upload static instance data for animated circles
void afferent_renderer_upload_animated_circles(
    AfferentRendererRef renderer,
    const float* data,
    uint32_t count
) {
    if (!renderer || !data || count == 0) return;

    @autoreleasepool {
        size_t size = count * sizeof(AnimatedInstanceData);
        renderer->animatedCircleBuffer = [renderer->device newBufferWithBytes:data
                                                                       length:size
                                                                      options:MTLResourceStorageModeShared];
        renderer->animatedCircleCount = count;
    }
}

// Draw animated rects (called every frame - only sends time uniform!)
void afferent_renderer_draw_animated_rects(
    AfferentRendererRef renderer,
    float time
) {
    if (!renderer || !renderer->currentEncoder || !renderer->animatedRectBuffer || renderer->animatedRectCount == 0) {
        return;
    }

    @autoreleasepool {
        // Prepare uniforms (just 16 bytes!)
        AnimationUniforms uniforms = {
            .time = time,
            .canvasWidth = renderer->screenWidth,
            .canvasHeight = renderer->screenHeight,
            .padding = 0
        };

        [renderer->currentEncoder setRenderPipelineState:renderer->animatedRectPipelineState];
        [renderer->currentEncoder setVertexBuffer:renderer->animatedRectBuffer offset:0 atIndex:0];
        [renderer->currentEncoder setVertexBytes:&uniforms length:sizeof(uniforms) atIndex:1];
        [renderer->currentEncoder drawPrimitives:MTLPrimitiveTypeTriangleStrip
                                     vertexStart:0
                                     vertexCount:4
                                   instanceCount:renderer->animatedRectCount];
        [renderer->currentEncoder setRenderPipelineState:renderer->pipelineState];
    }
}

// Draw animated triangles (called every frame)
void afferent_renderer_draw_animated_triangles(
    AfferentRendererRef renderer,
    float time
) {
    if (!renderer || !renderer->currentEncoder || !renderer->animatedTriangleBuffer || renderer->animatedTriangleCount == 0) {
        return;
    }

    @autoreleasepool {
        AnimationUniforms uniforms = {
            .time = time,
            .canvasWidth = renderer->screenWidth,
            .canvasHeight = renderer->screenHeight,
            .padding = 0
        };

        [renderer->currentEncoder setRenderPipelineState:renderer->animatedTrianglePipelineState];
        [renderer->currentEncoder setVertexBuffer:renderer->animatedTriangleBuffer offset:0 atIndex:0];
        [renderer->currentEncoder setVertexBytes:&uniforms length:sizeof(uniforms) atIndex:1];
        [renderer->currentEncoder drawPrimitives:MTLPrimitiveTypeTriangle
                                     vertexStart:0
                                     vertexCount:3
                                   instanceCount:renderer->animatedTriangleCount];
        [renderer->currentEncoder setRenderPipelineState:renderer->pipelineState];
    }
}

// Draw animated circles (called every frame)
void afferent_renderer_draw_animated_circles(
    AfferentRendererRef renderer,
    float time
) {
    if (!renderer || !renderer->currentEncoder || !renderer->animatedCircleBuffer || renderer->animatedCircleCount == 0) {
        return;
    }

    @autoreleasepool {
        AnimationUniforms uniforms = {
            .time = time,
            .canvasWidth = renderer->screenWidth,
            .canvasHeight = renderer->screenHeight,
            .padding = 0
        };

        [renderer->currentEncoder setRenderPipelineState:renderer->animatedCirclePipelineState];
        [renderer->currentEncoder setVertexBuffer:renderer->animatedCircleBuffer offset:0 atIndex:0];
        [renderer->currentEncoder setVertexBytes:&uniforms length:sizeof(uniforms) atIndex:1];
        [renderer->currentEncoder drawPrimitives:MTLPrimitiveTypeTriangleStrip
                                     vertexStart:0
                                     vertexCount:4
                                   instanceCount:renderer->animatedCircleCount];
        [renderer->currentEncoder setRenderPipelineState:renderer->pipelineState];
    }
}

// ============================================================================
// ORBITAL RENDERING - Particles orbiting around a center point
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
) {
    if (!renderer || !data || count == 0) return;

    @autoreleasepool {
        size_t size = count * sizeof(OrbitalInstanceData);
        renderer->orbitalBuffer = [renderer->device newBufferWithBytes:data
                                                                length:size
                                                               options:MTLResourceStorageModeShared];
        renderer->orbitalCount = count;
        renderer->orbitalCenterX = centerX;
        renderer->orbitalCenterY = centerY;
    }
}

// Draw orbital particles (called every frame - only sends time and uniforms!)
void afferent_renderer_draw_orbital_particles(
    AfferentRendererRef renderer,
    float time
) {
    if (!renderer || !renderer->currentEncoder || !renderer->orbitalBuffer || renderer->orbitalCount == 0) {
        return;
    }

    @autoreleasepool {
        OrbitalUniforms uniforms = {
            .time = time,
            .centerX = renderer->orbitalCenterX,
            .centerY = renderer->orbitalCenterY,
            .canvasWidth = renderer->screenWidth,
            .canvasHeight = renderer->screenHeight,
            .radiusWobble = 30.0f,
            .padding1 = 0,
            .padding2 = 0
        };

        [renderer->currentEncoder setRenderPipelineState:renderer->orbitalPipelineState];
        [renderer->currentEncoder setVertexBuffer:renderer->orbitalBuffer offset:0 atIndex:0];
        [renderer->currentEncoder setVertexBytes:&uniforms length:sizeof(uniforms) atIndex:1];
        [renderer->currentEncoder drawPrimitives:MTLPrimitiveTypeTriangleStrip
                                     vertexStart:0
                                     vertexCount:4
                                   instanceCount:renderer->orbitalCount];
        [renderer->currentEncoder setRenderPipelineState:renderer->pipelineState];
    }
}

// Draw dynamic circles (positions updated each frame, GPU does color + NDC)
// data: [pixelX, pixelY, hueBase, radiusPixels] × count (4 floats per circle)
void afferent_renderer_draw_dynamic_circles(
    AfferentRendererRef renderer,
    const float* data,
    uint32_t count,
    float time,
    float canvasWidth,
    float canvasHeight
) {
    if (!renderer || !renderer->currentEncoder || !data || count == 0) {
        return;
    }

    @autoreleasepool {
        size_t dataSize = count * sizeof(DynamicCircleData);
        id<MTLBuffer> circleBuffer = pool_acquire_buffer(
            renderer->device,
            g_buffer_pool.vertex_pool,
            &g_buffer_pool.vertex_pool_count,
            dataSize,
            true
        );

        if (!circleBuffer) {
            return;
        }

        memcpy(circleBuffer.contents, data, dataSize);

        DynamicCircleUniforms uniforms = {
            .time = time,
            .canvasWidth = canvasWidth,
            .canvasHeight = canvasHeight,
            .hueSpeed = 0.2f
        };

        [renderer->currentEncoder setRenderPipelineState:renderer->dynamicCirclePipelineState];
        [renderer->currentEncoder setVertexBuffer:circleBuffer offset:0 atIndex:0];
        [renderer->currentEncoder setVertexBytes:&uniforms length:sizeof(uniforms) atIndex:1];
        [renderer->currentEncoder drawPrimitives:MTLPrimitiveTypeTriangleStrip
                                     vertexStart:0
                                     vertexCount:4
                                   instanceCount:count];
        [renderer->currentEncoder setRenderPipelineState:renderer->pipelineState];
    }
}

// Draw dynamic rects (positions/rotation updated each frame, GPU does color + NDC)
// data: [pixelX, pixelY, hueBase, halfSizePixels, rotation] × count (5 floats per rect)
void afferent_renderer_draw_dynamic_rects(
    AfferentRendererRef renderer,
    const float* data,
    uint32_t count,
    float time,
    float canvasWidth,
    float canvasHeight
) {
    if (!renderer || !renderer->currentEncoder || !data || count == 0) {
        return;
    }

    @autoreleasepool {
        // Create temporary buffer for this frame's rect data
        size_t dataSize = count * sizeof(DynamicRectData);
        id<MTLBuffer> rectBuffer = [renderer->device newBufferWithBytes:data
                                                                 length:dataSize
                                                                options:MTLResourceStorageModeShared];

        DynamicRectUniforms uniforms = {
            .time = time,
            .canvasWidth = canvasWidth,
            .canvasHeight = canvasHeight,
            .hueSpeed = 0.2f
        };

        [renderer->currentEncoder setRenderPipelineState:renderer->dynamicRectPipelineState];
        [renderer->currentEncoder setVertexBuffer:rectBuffer offset:0 atIndex:0];
        [renderer->currentEncoder setVertexBytes:&uniforms length:sizeof(uniforms) atIndex:1];
        [renderer->currentEncoder drawPrimitives:MTLPrimitiveTypeTriangleStrip
                                     vertexStart:0
                                     vertexCount:4
                                   instanceCount:count];
        [renderer->currentEncoder setRenderPipelineState:renderer->pipelineState];
    }
}

// Draw dynamic triangles (positions/rotation updated each frame, GPU does color + NDC)
// data: [pixelX, pixelY, hueBase, halfSizePixels, rotation] × count (5 floats per triangle)
void afferent_renderer_draw_dynamic_triangles(
    AfferentRendererRef renderer,
    const float* data,
    uint32_t count,
    float time,
    float canvasWidth,
    float canvasHeight
) {
    if (!renderer || !renderer->currentEncoder || !data || count == 0) {
        return;
    }

    @autoreleasepool {
        // Create temporary buffer for this frame's triangle data
        size_t dataSize = count * sizeof(DynamicTriangleData);
        id<MTLBuffer> triangleBuffer = [renderer->device newBufferWithBytes:data
                                                                     length:dataSize
                                                                    options:MTLResourceStorageModeShared];

        DynamicTriangleUniforms uniforms = {
            .time = time,
            .canvasWidth = canvasWidth,
            .canvasHeight = canvasHeight,
            .hueSpeed = 0.2f
        };

        [renderer->currentEncoder setRenderPipelineState:renderer->dynamicTrianglePipelineState];
        [renderer->currentEncoder setVertexBuffer:triangleBuffer offset:0 atIndex:0];
        [renderer->currentEncoder setVertexBytes:&uniforms length:sizeof(uniforms) atIndex:1];
        [renderer->currentEncoder drawPrimitives:MTLPrimitiveTypeTriangle
                                     vertexStart:0
                                     vertexCount:3
                                   instanceCount:count];
        [renderer->currentEncoder setRenderPipelineState:renderer->pipelineState];
    }
}

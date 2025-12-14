// draw_2d.m - Basic 2D rendering (triangles, instanced shapes, scissor)
#import "render.h"

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

// Draw instanced triangles - GPU computes transforms
void afferent_renderer_draw_instanced_triangles(
    AfferentRendererRef renderer,
    const float* instance_data,
    uint32_t instance_count
) {
    if (!renderer || !renderer->currentEncoder || !instance_data || instance_count == 0) {
        return;
    }

    @autoreleasepool {
        size_t data_size = instance_count * sizeof(InstanceData);
        id<MTLBuffer> instanceBuffer = pool_acquire_buffer(
            renderer->device,
            g_buffer_pool.vertex_pool,
            &g_buffer_pool.vertex_pool_count,
            data_size,
            true
        );

        if (!instanceBuffer) {
            NSLog(@"Failed to create triangle instance buffer");
            return;
        }

        memcpy(instanceBuffer.contents, instance_data, data_size);

        [renderer->currentEncoder setRenderPipelineState:renderer->trianglePipelineState];
        [renderer->currentEncoder setVertexBuffer:instanceBuffer offset:0 atIndex:0];

        // Draw: 3 vertices per triangle
        [renderer->currentEncoder drawPrimitives:MTLPrimitiveTypeTriangle
                                     vertexStart:0
                                     vertexCount:3
                                   instanceCount:instance_count];

        [renderer->currentEncoder setRenderPipelineState:renderer->pipelineState];
    }
}

// Draw instanced circles - smooth circles via fragment shader
void afferent_renderer_draw_instanced_circles(
    AfferentRendererRef renderer,
    const float* instance_data,
    uint32_t instance_count
) {
    if (!renderer || !renderer->currentEncoder || !instance_data || instance_count == 0) {
        return;
    }

    @autoreleasepool {
        size_t data_size = instance_count * sizeof(InstanceData);
        id<MTLBuffer> instanceBuffer = pool_acquire_buffer(
            renderer->device,
            g_buffer_pool.vertex_pool,
            &g_buffer_pool.vertex_pool_count,
            data_size,
            true
        );

        if (!instanceBuffer) {
            NSLog(@"Failed to create circle instance buffer");
            return;
        }

        memcpy(instanceBuffer.contents, instance_data, data_size);

        [renderer->currentEncoder setRenderPipelineState:renderer->circlePipelineState];
        [renderer->currentEncoder setVertexBuffer:instanceBuffer offset:0 atIndex:0];

        // Draw: 4 vertices per quad (triangle strip)
        [renderer->currentEncoder drawPrimitives:MTLPrimitiveTypeTriangleStrip
                                     vertexStart:0
                                     vertexCount:4
                                   instanceCount:instance_count];

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

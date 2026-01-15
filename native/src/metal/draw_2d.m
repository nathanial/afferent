// draw_2d.m - Basic 2D rendering (triangles, instanced shapes, scissor)
#import "render.h"

void afferent_buffer_destroy(AfferentBufferRef buffer) {
    if (!buffer) {
        return;
    }
    // Pooled buffers are kept for reuse. Persistent buffers are owned here.
    if (buffer->persistent || !buffer->pooled) {
        buffer->mtlBuffer = nil;
        free(buffer);
    }
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
    // Disable depth testing for 2D rendering (may have been enabled by 3D)
    [renderer->currentEncoder setDepthStencilState:renderer->depthStateDisabled];

    [renderer->currentEncoder setVertexBuffer:vertex_buffer->mtlBuffer offset:0 atIndex:0];

    [renderer->currentEncoder drawIndexedPrimitives:MTLPrimitiveTypeTriangle
                                         indexCount:index_count
                                          indexType:MTLIndexTypeUInt32
                                        indexBuffer:index_buffer->mtlBuffer
                                  indexBufferOffset:0];
}

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
) {
    if (!renderer->currentEncoder || !vertex_buffer || !index_buffer) {
        return;
    }

    StrokeUniforms uniforms;
    uniforms.viewport[0] = canvas_width;
    uniforms.viewport[1] = canvas_height;
    uniforms.halfWidth = half_width;
    uniforms.padding = 0.0f;
    uniforms.color[0] = r;
    uniforms.color[1] = g;
    uniforms.color[2] = b;
    uniforms.color[3] = a;

    [renderer->currentEncoder setRenderPipelineState:renderer->strokePipelineState];
    // Disable depth testing for 2D rendering (may have been enabled by 3D)
    [renderer->currentEncoder setDepthStencilState:renderer->depthStateDisabled];
    [renderer->currentEncoder setVertexBuffer:vertex_buffer->mtlBuffer offset:0 atIndex:0];
    [renderer->currentEncoder setVertexBytes:&uniforms length:sizeof(StrokeUniforms) atIndex:1];
    [renderer->currentEncoder setFragmentBytes:&uniforms length:sizeof(StrokeUniforms) atIndex:1];

    [renderer->currentEncoder drawIndexedPrimitives:MTLPrimitiveTypeTriangle
                                         indexCount:index_count
                                          indexType:MTLIndexTypeUInt32
                                        indexBuffer:index_buffer->mtlBuffer
                                  indexBufferOffset:0];

    [renderer->currentEncoder setRenderPipelineState:renderer->pipelineState];
}

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
) {
    if (!renderer || !renderer->currentEncoder || !segment_buffer || segment_count == 0) {
        return;
    }

    uint32_t subdivisions = segment_subdivisions > 0 ? segment_subdivisions : 1;

    StrokePathVertexUniforms v;
    v.viewport[0] = canvas_width;
    v.viewport[1] = canvas_height;
    v.halfWidth = half_width;
    v.miterLimit = miter_limit;
    v.lineCap = line_cap;
    v.lineJoin = line_join;
    v.segmentSubdivisions = subdivisions;
    v.padding0 = 0;
    v.transform0[0] = transform_a;
    v.transform0[1] = transform_b;
    v.transform0[2] = transform_c;
    v.transform0[3] = transform_d;
    v.transform1[0] = transform_tx;
    v.transform1[1] = transform_ty;
    v.transform1[2] = 0.0f;
    v.transform1[3] = 0.0f;

    StrokePathFragmentUniforms f;
    f.color[0] = r;
    f.color[1] = g;
    f.color[2] = b;
    f.color[3] = a;
    for (uint32_t i = 0; i < 8; ++i) {
        f.dashSegments[i] = (dash_segments && i < dash_count) ? dash_segments[i] : 0.0f;
    }
    f.dashCount = dash_count;
    f.dashOffset = dash_offset;
    f.lineCap = line_cap;
    f.halfWidth = half_width;
    f.padding0 = 0.0f;
    f.padding1 = 0.0f;
    f.padding2 = 0.0f;
    f.padding3 = 0.0f;

    [renderer->currentEncoder setRenderPipelineState:renderer->strokePathPipelineState];
    // Disable depth testing for 2D rendering (may have been enabled by 3D)
    [renderer->currentEncoder setDepthStencilState:renderer->depthStateDisabled];
    [renderer->currentEncoder setVertexBuffer:segment_buffer->mtlBuffer offset:0 atIndex:0];
    [renderer->currentEncoder setVertexBytes:&v length:sizeof(StrokePathVertexUniforms) atIndex:1];
    [renderer->currentEncoder setFragmentBytes:&f length:sizeof(StrokePathFragmentUniforms) atIndex:1];

    uint32_t vertexCount = (subdivisions + 1) * 2;
    [renderer->currentEncoder drawPrimitives:MTLPrimitiveTypeTriangleStrip
                                 vertexStart:0
                                 vertexCount:vertexCount
                               instanceCount:segment_count];

    [renderer->currentEncoder setRenderPipelineState:renderer->pipelineState];
}

// Draw instanced shapes - GPU computes transforms
// shape_type: 0=rect, 1=triangle, 2=circle
// instance_data: array of 8 floats per instance (pos.x, pos.y, angle, halfSize, r, g, b, a)
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
) {
    if (!renderer || !renderer->currentEncoder || !instance_data || instance_count == 0) {
        return;
    }

    // Select pipeline, vertex count, and primitive type based on shape
    id<MTLRenderPipelineState> pipeline;
    uint32_t vertexCount;
    MTLPrimitiveType primType;

    switch (shape_type) {
        case 0: // rect
            pipeline = renderer->instancedPipelineState;
            vertexCount = 4;
            primType = MTLPrimitiveTypeTriangleStrip;
            break;
        case 1: // triangle
            pipeline = renderer->trianglePipelineState;
            vertexCount = 3;
            primType = MTLPrimitiveTypeTriangle;
            break;
        case 2: // circle
            pipeline = renderer->circlePipelineState;
            vertexCount = 4;
            primType = MTLPrimitiveTypeTriangleStrip;
            break;
        default:
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
            return;
        }

        memcpy(instanceBuffer.contents, instance_data, data_size);

        InstancedUniforms u;
        u.transform0[0] = transform_a;
        u.transform0[1] = transform_b;
        u.transform0[2] = transform_c;
        u.transform0[3] = transform_d;
        u.transform1[0] = transform_tx;
        u.transform1[1] = transform_ty;
        u.transform1[2] = 0.0f;
        u.transform1[3] = 0.0f;
        u.viewport[0] = viewport_width;
        u.viewport[1] = viewport_height;
        u.time = time;
        u.hueSpeed = hue_speed;
        u.sizeMode = size_mode;
        u.colorMode = color_mode;
        u.padding0 = 0.0f;
        u.padding1 = 0.0f;

        [renderer->currentEncoder setRenderPipelineState:pipeline];
        // Disable depth testing for 2D rendering (may have been enabled by 3D)
        [renderer->currentEncoder setDepthStencilState:renderer->depthStateDisabled];
        [renderer->currentEncoder setVertexBuffer:instanceBuffer offset:0 atIndex:0];
        [renderer->currentEncoder setVertexBytes:&u length:sizeof(InstancedUniforms) atIndex:1];

        [renderer->currentEncoder drawPrimitives:primType
                                     vertexStart:0
                                     vertexCount:vertexCount
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

    // If origin is outside the drawable, clamp to empty scissor to avoid underflow.
    if (sx >= maxW || sy >= maxH) {
        MTLScissorRect scissor = {0, 0, 0, 0};
        [renderer->currentEncoder setScissorRect:scissor];
        return;
    }

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

// =============================================================================
// BATCHED RECT DRAWING - Optimized for charts (heatmaps, scatter plots, etc.)
// Instance data: [x, y, width, height, r, g, b, a] per rect (8 floats)
// =============================================================================

// Uniforms structure matching the shader
typedef struct {
    float viewport[2];
    float cornerRadius;
    float padding;
} BatchedRectUniforms;

void afferent_renderer_draw_rects_batch(
    AfferentRendererRef renderer,
    const float* instance_data,
    uint32_t instance_count,
    float corner_radius,
    float canvas_width,
    float canvas_height
) {
    if (!renderer || !renderer->currentEncoder || !instance_data || instance_count == 0) {
        return;
    }

    if (!renderer->batchedRectPipelineState) {
        NSLog(@"Batched rect pipeline not available");
        return;
    }

    @autoreleasepool {
        // 8 floats per instance: [x, y, width, height, r, g, b, a]
        size_t data_size = instance_count * 8 * sizeof(float);
        id<MTLBuffer> instanceBuffer = pool_acquire_buffer(
            renderer->device,
            g_buffer_pool.vertex_pool,
            &g_buffer_pool.vertex_pool_count,
            data_size,
            true
        );

        if (!instanceBuffer) {
            instanceBuffer = [renderer->device newBufferWithLength:data_size options:MTLResourceStorageModeShared];
        }

        if (!instanceBuffer) {
            NSLog(@"Failed to create batched rect instance buffer");
            return;
        }

        memcpy([instanceBuffer contents], instance_data, data_size);

        BatchedRectUniforms uniforms;
        uniforms.viewport[0] = canvas_width;
        uniforms.viewport[1] = canvas_height;
        uniforms.cornerRadius = corner_radius;
        uniforms.padding = 0.0f;

        [renderer->currentEncoder setRenderPipelineState:renderer->batchedRectPipelineState];
        [renderer->currentEncoder setDepthStencilState:renderer->depthStateDisabled];
        [renderer->currentEncoder setVertexBuffer:instanceBuffer offset:0 atIndex:0];
        [renderer->currentEncoder setVertexBytes:&uniforms length:sizeof(BatchedRectUniforms) atIndex:1];

        // Triangle strip with 4 vertices per rect instance
        [renderer->currentEncoder drawPrimitives:MTLPrimitiveTypeTriangleStrip
                                     vertexStart:0
                                     vertexCount:4
                                   instanceCount:instance_count];

        // Restore default pipeline
        [renderer->currentEncoder setRenderPipelineState:renderer->pipelineState];
    }
}

// =============================================================================
// BATCHED CIRCLE DRAWING - Optimized for scatter plots, bubble charts
// Instance data: [centerX, centerY, radius, padding, r, g, b, a] per circle (8 floats)
// =============================================================================

typedef struct {
    float viewport[2];
} BatchedCircleUniforms;

void afferent_renderer_draw_circles_batch(
    AfferentRendererRef renderer,
    const float* instance_data,
    uint32_t instance_count,
    float canvas_width,
    float canvas_height
) {
    if (!renderer || !renderer->currentEncoder || !instance_data || instance_count == 0) {
        return;
    }

    if (!renderer->batchedCirclePipelineState) {
        NSLog(@"Batched circle pipeline not available");
        return;
    }

    @autoreleasepool {
        // 8 floats per instance: [centerX, centerY, radius, padding, r, g, b, a]
        size_t data_size = instance_count * 8 * sizeof(float);
        id<MTLBuffer> instanceBuffer = pool_acquire_buffer(
            renderer->device,
            g_buffer_pool.vertex_pool,
            &g_buffer_pool.vertex_pool_count,
            data_size,
            true
        );

        if (!instanceBuffer) {
            instanceBuffer = [renderer->device newBufferWithLength:data_size options:MTLResourceStorageModeShared];
        }

        if (!instanceBuffer) {
            NSLog(@"Failed to create batched circle instance buffer");
            return;
        }

        memcpy([instanceBuffer contents], instance_data, data_size);

        BatchedCircleUniforms uniforms;
        uniforms.viewport[0] = canvas_width;
        uniforms.viewport[1] = canvas_height;

        [renderer->currentEncoder setRenderPipelineState:renderer->batchedCirclePipelineState];
        [renderer->currentEncoder setDepthStencilState:renderer->depthStateDisabled];
        [renderer->currentEncoder setVertexBuffer:instanceBuffer offset:0 atIndex:0];
        [renderer->currentEncoder setVertexBytes:&uniforms length:sizeof(BatchedCircleUniforms) atIndex:1];

        // Triangle strip with 4 vertices per circle instance (quad covering bounding box)
        [renderer->currentEncoder drawPrimitives:MTLPrimitiveTypeTriangleStrip
                                     vertexStart:0
                                     vertexCount:4
                                   instanceCount:instance_count];

        // Restore default pipeline
        [renderer->currentEncoder setRenderPipelineState:renderer->pipelineState];
    }
}

// =============================================================================
// BATCHED STROKED RECT DRAWING - For UI borders, chart axes, grid lines
// Instance data: [x, y, width, height, r, g, b, a] per rect (8 floats)
// =============================================================================

typedef struct {
    float viewport[2];
    float lineWidth;
    float cornerRadius;
} BatchedStrokeRectUniforms;

void afferent_renderer_draw_stroke_rects_batch(
    AfferentRendererRef renderer,
    const float* instance_data,
    uint32_t instance_count,
    float line_width,
    float corner_radius,
    float canvas_width,
    float canvas_height
) {
    if (!renderer || !renderer->currentEncoder || !instance_data || instance_count == 0) {
        return;
    }

    if (!renderer->batchedStrokeRectPipelineState) {
        NSLog(@"Batched stroke rect pipeline not available");
        return;
    }

    @autoreleasepool {
        // 8 floats per instance: [x, y, width, height, r, g, b, a]
        size_t data_size = instance_count * 8 * sizeof(float);
        id<MTLBuffer> instanceBuffer = pool_acquire_buffer(
            renderer->device,
            g_buffer_pool.vertex_pool,
            &g_buffer_pool.vertex_pool_count,
            data_size,
            true
        );

        if (!instanceBuffer) {
            instanceBuffer = [renderer->device newBufferWithLength:data_size options:MTLResourceStorageModeShared];
        }

        if (!instanceBuffer) {
            NSLog(@"Failed to create batched stroke rect instance buffer");
            return;
        }

        memcpy([instanceBuffer contents], instance_data, data_size);

        BatchedStrokeRectUniforms uniforms;
        uniforms.viewport[0] = canvas_width;
        uniforms.viewport[1] = canvas_height;
        uniforms.lineWidth = line_width;
        uniforms.cornerRadius = corner_radius;

        [renderer->currentEncoder setRenderPipelineState:renderer->batchedStrokeRectPipelineState];
        [renderer->currentEncoder setDepthStencilState:renderer->depthStateDisabled];
        [renderer->currentEncoder setVertexBuffer:instanceBuffer offset:0 atIndex:0];
        [renderer->currentEncoder setVertexBytes:&uniforms length:sizeof(BatchedStrokeRectUniforms) atIndex:1];

        // Triangle strip with 4 vertices per rect instance
        [renderer->currentEncoder drawPrimitives:MTLPrimitiveTypeTriangleStrip
                                     vertexStart:0
                                     vertexCount:4
                                   instanceCount:instance_count];

        // Restore default pipeline
        [renderer->currentEncoder setRenderPipelineState:renderer->pipelineState];
    }
}

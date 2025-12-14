// draw_3d.m - 3D mesh and ocean wave rendering
#import "render.h"

// Ensure ocean index buffer is created for the given grid size
void ensure_ocean_index_buffer(AfferentRendererRef renderer, uint32_t gridSize) {
    if (!renderer || gridSize < 2) return;
    if (renderer->oceanIndexBuffer && renderer->oceanGridSize == gridSize) return;

    uint32_t quadsPerRow = gridSize - 1;
    uint32_t quadCount = quadsPerRow * quadsPerRow;
    uint32_t indexCount = quadCount * 6;
    size_t indexSize = (size_t)indexCount * sizeof(uint32_t);

    uint32_t* indices = (uint32_t*)malloc(indexSize);
    if (!indices) {
        NSLog(@"Failed to allocate ocean index buffer");
        return;
    }

    uint32_t w = gridSize;
    uint32_t idx = 0;
    for (uint32_t row = 0; row < gridSize - 1; row++) {
        for (uint32_t col = 0; col < gridSize - 1; col++) {
            uint32_t topLeft = row * w + col;
            uint32_t topRight = topLeft + 1;
            uint32_t bottomLeft = (row + 1) * w + col;
            uint32_t bottomRight = bottomLeft + 1;

            indices[idx++] = topLeft;
            indices[idx++] = bottomLeft;
            indices[idx++] = topRight;

            indices[idx++] = topRight;
            indices[idx++] = bottomLeft;
            indices[idx++] = bottomRight;
        }
    }

    id<MTLBuffer> indexBuffer = [renderer->device newBufferWithBytes:indices
                                                              length:indexSize
                                                             options:MTLResourceStorageModeShared];
    free(indices);
    if (!indexBuffer) {
        NSLog(@"Failed to create ocean index MTLBuffer");
        return;
    }

    renderer->oceanIndexBuffer = indexBuffer;
    renderer->oceanIndexCount = indexCount;
    renderer->oceanGridSize = gridSize;
}

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
) {
    if (!renderer || !renderer->currentEncoder || !mvp_matrix || !model_matrix ||
        !light_dir || !camera_pos || !fog_color || grid_size < 2) {
        return;
    }

    ensure_ocean_index_buffer(renderer, grid_size);
    if (!renderer->oceanIndexBuffer || renderer->oceanIndexCount == 0) return;

    @autoreleasepool {
        OceanProjectedUniforms uniforms;
        memset(&uniforms, 0, sizeof(uniforms));

        memcpy(uniforms.scene.modelViewProj, mvp_matrix, 64);
        memcpy(uniforms.scene.modelMatrix, model_matrix, 64);
        memcpy(uniforms.scene.lightDir, light_dir, 12);
        uniforms.scene.ambient = ambient;
        memcpy(uniforms.scene.cameraPos, camera_pos, 12);
        uniforms.scene.fogStart = fog_start;
        memcpy(uniforms.scene.fogColor, fog_color, 12);
        uniforms.scene.fogEnd = fog_end;

        uniforms.params0[0] = time;
        uniforms.params0[1] = fovY;
        uniforms.params0[2] = aspect;
        uniforms.params0[3] = maxDistance;

        uniforms.params1[0] = snapSize;
        uniforms.params1[1] = overscanNdc;
        uniforms.params1[2] = horizonMargin;
        uniforms.params1[3] = yaw;

        uniforms.params2[0] = pitch;
        uniforms.params2[1] = (float)grid_size;
        // Reserved (was used for local patch). Keep 0 so shader stays in projected-grid-only mode.
        uniforms.params2[2] = 0.0f;
        uniforms.params2[3] = 0.0f;

        if (wave_params && wave_param_count >= 32) {
            for (uint32_t i = 0; i < 4; i++) {
                for (uint32_t j = 0; j < 4; j++) {
                    uniforms.waveA[i][j] = wave_params[i * 4 + j];
                    uniforms.waveB[i][j] = wave_params[16 + i * 4 + j];
                }
            }
        }

        [renderer->currentEncoder setRenderPipelineState:renderer->pipeline3DOcean];
        [renderer->currentEncoder setDepthStencilState:renderer->depthState];
        [renderer->currentEncoder setFragmentBytes:&uniforms.scene length:sizeof(uniforms.scene) atIndex:0];
        [renderer->currentEncoder setVertexBytes:&uniforms length:sizeof(uniforms) atIndex:1];
        [renderer->currentEncoder drawIndexedPrimitives:MTLPrimitiveTypeTriangle
                                             indexCount:renderer->oceanIndexCount
                                              indexType:MTLIndexTypeUInt32
                                            indexBuffer:renderer->oceanIndexBuffer
                                      indexBufferOffset:0];

        [renderer->currentEncoder setRenderPipelineState:renderer->pipelineState];
    }
}

void afferent_renderer_draw_mesh_3d(
    AfferentRendererRef renderer,
    const AfferentVertex3D* vertices,
    uint32_t vertex_count,
    const uint32_t* indices,
    uint32_t index_count,
    const float* mvp_matrix,
    const float* model_matrix,
    const float* light_dir,
    float ambient
) {
    if (!renderer || !renderer->currentEncoder || !vertices || !indices ||
        vertex_count == 0 || index_count == 0) {
        return;
    }

    @autoreleasepool {
        // Acquire temporary vertex buffer (pooled)
        size_t vertex_size = vertex_count * sizeof(AfferentVertex3D);
        id<MTLBuffer> vertexBuffer = pool_acquire_buffer(
            renderer->device,
            g_buffer_pool.vertex_pool,
            &g_buffer_pool.vertex_pool_count,
            vertex_size,
            true
        );
        if (!vertexBuffer) {
            NSLog(@"Failed to create 3D vertex buffer");
            return;
        }
        memcpy(vertexBuffer.contents, vertices, vertex_size);

        // Acquire temporary index buffer (pooled)
        size_t index_size = index_count * sizeof(uint32_t);
        id<MTLBuffer> indexBuffer = pool_acquire_buffer(
            renderer->device,
            g_buffer_pool.index_pool,
            &g_buffer_pool.index_pool_count,
            index_size,
            false
        );
        if (!indexBuffer) {
            NSLog(@"Failed to create 3D index buffer");
            return;
        }
        memcpy(indexBuffer.contents, indices, index_size);

        // Set up uniforms
        Scene3DUniforms uniforms;
        memcpy(uniforms.modelViewProj, mvp_matrix, 64);
        memcpy(uniforms.modelMatrix, model_matrix, 64);
        memcpy(uniforms.lightDir, light_dir, 12);
        uniforms.ambient = ambient;
        // Default: no fog (start=end=0 disables fog in shader)
        uniforms.cameraPos[0] = 0.0f;
        uniforms.cameraPos[1] = 0.0f;
        uniforms.cameraPos[2] = 0.0f;
        uniforms.fogStart = 0.0f;
        uniforms.fogEnd = 0.0f;
        uniforms.fogColor[0] = 0.5f;
        uniforms.fogColor[1] = 0.5f;
        uniforms.fogColor[2] = 0.5f;

        // Configure encoder for 3D rendering
        [renderer->currentEncoder setRenderPipelineState:renderer->pipeline3D];
        [renderer->currentEncoder setDepthStencilState:renderer->depthState];
        [renderer->currentEncoder setVertexBuffer:vertexBuffer offset:0 atIndex:0];
        [renderer->currentEncoder setVertexBytes:&uniforms length:sizeof(uniforms) atIndex:1];
        [renderer->currentEncoder setFragmentBytes:&uniforms length:sizeof(uniforms) atIndex:0];

        // Draw indexed triangles
        [renderer->currentEncoder drawIndexedPrimitives:MTLPrimitiveTypeTriangle
                                             indexCount:index_count
                                              indexType:MTLIndexTypeUInt32
                                            indexBuffer:indexBuffer
                                      indexBufferOffset:0];

        // Restore default pipeline
        [renderer->currentEncoder setRenderPipelineState:renderer->pipelineState];
    }
}

// 3D Mesh Rendering with fog parameters
void afferent_renderer_draw_mesh_3d_with_fog(
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
) {
    if (!renderer || !renderer->currentEncoder || !vertices || !indices ||
        vertex_count == 0 || index_count == 0) {
        return;
    }

    @autoreleasepool {
        // Acquire temporary vertex buffer (pooled)
        size_t vertex_size = vertex_count * sizeof(AfferentVertex3D);
        id<MTLBuffer> vertexBuffer = pool_acquire_buffer(
            renderer->device,
            g_buffer_pool.vertex_pool,
            &g_buffer_pool.vertex_pool_count,
            vertex_size,
            true
        );
        if (!vertexBuffer) {
            NSLog(@"Failed to create 3D vertex buffer (fog)");
            return;
        }
        memcpy(vertexBuffer.contents, vertices, vertex_size);

        // Acquire temporary index buffer (pooled)
        size_t index_size = index_count * sizeof(uint32_t);
        id<MTLBuffer> indexBuffer = pool_acquire_buffer(
            renderer->device,
            g_buffer_pool.index_pool,
            &g_buffer_pool.index_pool_count,
            index_size,
            false
        );
        if (!indexBuffer) {
            NSLog(@"Failed to create 3D index buffer (fog)");
            return;
        }
        memcpy(indexBuffer.contents, indices, index_size);

        // Set up uniforms with fog parameters
        Scene3DUniforms uniforms;
        memcpy(uniforms.modelViewProj, mvp_matrix, 64);
        memcpy(uniforms.modelMatrix, model_matrix, 64);
        memcpy(uniforms.lightDir, light_dir, 12);
        uniforms.ambient = ambient;
        memcpy(uniforms.cameraPos, camera_pos, 12);
        uniforms.fogStart = fog_start;
        memcpy(uniforms.fogColor, fog_color, 12);
        uniforms.fogEnd = fog_end;

        // Configure encoder for 3D rendering
        [renderer->currentEncoder setRenderPipelineState:renderer->pipeline3D];
        [renderer->currentEncoder setDepthStencilState:renderer->depthState];
        [renderer->currentEncoder setVertexBuffer:vertexBuffer offset:0 atIndex:0];
        [renderer->currentEncoder setVertexBytes:&uniforms length:sizeof(uniforms) atIndex:1];
        [renderer->currentEncoder setFragmentBytes:&uniforms length:sizeof(uniforms) atIndex:0];

        // Draw indexed triangles
        [renderer->currentEncoder drawIndexedPrimitives:MTLPrimitiveTypeTriangle
                                             indexCount:index_count
                                              indexType:MTLIndexTypeUInt32
                                            indexBuffer:indexBuffer
                                      indexBufferOffset:0];

        // Restore default pipeline
        [renderer->currentEncoder setRenderPipelineState:renderer->pipelineState];
    }
}

// 3D Textured Mesh Rendering with diffuse texture, lighting, and fog
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
) {
    if (!renderer || !renderer->currentEncoder || !vertices || !indices ||
        vertex_count == 0 || index_count == 0 || !texture) {
        return;
    }

    @autoreleasepool {
        // Get or create Metal texture for this texture handle
        id<MTLTexture> metalTex = (__bridge id<MTLTexture>)afferent_texture_get_metal_texture(texture);

        if (!metalTex) {
            // Create Metal texture from pixel data
            const uint8_t* pixelData = afferent_texture_get_data(texture);
            uint32_t texWidth, texHeight;
            afferent_texture_get_size(texture, &texWidth, &texHeight);

            if (!pixelData || texWidth == 0 || texHeight == 0) {
                NSLog(@"Invalid texture data for 3D textured mesh");
                return;
            }

            metalTex = createMetalTexture(renderer->device, pixelData, texWidth, texHeight);
            if (!metalTex) {
                NSLog(@"Failed to create Metal texture for 3D textured mesh");
                return;
            }

            // Store the Metal texture in the texture handle (retain with __bridge_retained)
            afferent_texture_set_metal_texture(texture, (__bridge_retained void*)metalTex);
        }

        // Acquire temporary vertex buffer (pooled)
        // 12 floats per vertex: position(3) + normal(3) + uv(2) + color(4)
        size_t vertex_size = vertex_count * 12 * sizeof(float);
        id<MTLBuffer> vertexBuffer = pool_acquire_buffer(
            renderer->device,
            g_buffer_pool.vertex_pool,
            &g_buffer_pool.vertex_pool_count,
            vertex_size,
            true
        );
        if (!vertexBuffer) {
            NSLog(@"Failed to create 3D textured vertex buffer");
            return;
        }
        memcpy(vertexBuffer.contents, vertices, vertex_size);

        // Acquire temporary index buffer (pooled)
        size_t total_index_count = index_offset + index_count;
        size_t index_size = total_index_count * sizeof(uint32_t);
        id<MTLBuffer> indexBuffer = pool_acquire_buffer(
            renderer->device,
            g_buffer_pool.index_pool,
            &g_buffer_pool.index_pool_count,
            index_size,
            false
        );
        if (!indexBuffer) {
            NSLog(@"Failed to create 3D textured index buffer");
            return;
        }
        memcpy(indexBuffer.contents, indices, index_size);

        // Set up uniforms with fog and UV parameters
        Scene3DTexturedUniforms uniforms;
        memcpy(uniforms.modelViewProj, mvp_matrix, 64);
        memcpy(uniforms.modelMatrix, model_matrix, 64);
        memcpy(uniforms.lightDir, light_dir, 12);
        uniforms.ambient = ambient;
        memcpy(uniforms.cameraPos, camera_pos, 12);
        uniforms.fogStart = fog_start;
        memcpy(uniforms.fogColor, fog_color, 12);
        uniforms.fogEnd = fog_end;
        // Default UV scale and offset (no tiling)
        uniforms.uvScale[0] = 1.0f;
        uniforms.uvScale[1] = 1.0f;
        uniforms.uvOffset[0] = 0.0f;
        uniforms.uvOffset[1] = 0.0f;

        // Configure encoder for textured 3D rendering
        [renderer->currentEncoder setRenderPipelineState:renderer->pipeline3DTextured];
        [renderer->currentEncoder setDepthStencilState:renderer->depthState];
        [renderer->currentEncoder setVertexBuffer:vertexBuffer offset:0 atIndex:0];
        [renderer->currentEncoder setVertexBytes:&uniforms length:sizeof(uniforms) atIndex:1];
        [renderer->currentEncoder setFragmentBytes:&uniforms length:sizeof(uniforms) atIndex:0];

        // Bind texture and sampler
        [renderer->currentEncoder setFragmentTexture:metalTex atIndex:0];
        [renderer->currentEncoder setFragmentSamplerState:renderer->texturedMeshSampler atIndex:0];

        // Draw indexed triangles with offset
        [renderer->currentEncoder drawIndexedPrimitives:MTLPrimitiveTypeTriangle
                                             indexCount:index_count
                                              indexType:MTLIndexTypeUInt32
                                            indexBuffer:indexBuffer
                                      indexBufferOffset:index_offset * sizeof(uint32_t)];

        // Restore default pipeline
        [renderer->currentEncoder setRenderPipelineState:renderer->pipelineState];
    }
}

// draw_text.m - Text rendering and font texture management
#import "render.h"

// Create or update font atlas texture
id<MTLTexture> ensureFontTexture(AfferentRendererRef renderer, AfferentFontRef font) {
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
void updateFontTexture(AfferentRendererRef renderer, AfferentFontRef font) {
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

        // Ensure staging buffer is large enough (grows as needed, never shrinks)
        if (vertex_count > g_text_vertex_staging_capacity) {
            free(g_text_vertex_staging);
            g_text_vertex_staging_capacity = vertex_count + 64;  // Add some headroom
            g_text_vertex_staging = malloc(g_text_vertex_staging_capacity * sizeof(TextVertex));
        }

        // Convert float vertex data to TextVertex format using staging buffer
        TextVertex* textVertices = g_text_vertex_staging;
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

        // Use pooled Metal buffers instead of creating fresh ones each call
        size_t vertex_buffer_size = vertex_count * sizeof(TextVertex);
        size_t index_buffer_size = index_count * sizeof(uint32_t);

        id<MTLBuffer> vertexBuffer = pool_acquire_buffer(
            renderer->device,
            g_buffer_pool.text_vertex_pool,
            &g_buffer_pool.text_vertex_pool_count,
            vertex_buffer_size
        );
        id<MTLBuffer> indexBuffer = pool_acquire_buffer(
            renderer->device,
            g_buffer_pool.text_index_pool,
            &g_buffer_pool.text_index_pool_count,
            index_buffer_size
        );

        // Copy data into pooled buffers
        if (vertexBuffer) {
            memcpy(vertexBuffer.contents, textVertices, vertex_buffer_size);
        }
        if (indexBuffer) {
            memcpy(indexBuffer.contents, indices, index_buffer_size);
        }

        // Free the vertex/index data generated by afferent_text_generate_vertices
        // (staging buffer is kept for reuse)
        free(vertices);
        free(indices);

        if (!vertexBuffer || !indexBuffer) {
            return AFFERENT_ERROR_TEXT_FAILED;
        }

        // Switch to text pipeline and disable depth testing for 2D text
        [renderer->currentEncoder setRenderPipelineState:renderer->textPipelineState];
        [renderer->currentEncoder setDepthStencilState:renderer->depthStateDisabled];

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

// Render multiple text strings with the same font in a single draw call
AfferentResult afferent_text_render_batch(
    AfferentRendererRef renderer,
    AfferentFontRef font,
    const char** texts,
    const float* positions,
    const float* colors,
    const float* transforms,
    uint32_t count,
    float canvas_width,
    float canvas_height
) {
    @autoreleasepool {
        if (!renderer || !renderer->currentEncoder || !font || !texts || count == 0) {
            return AFFERENT_OK;  // Nothing to render
        }

        // Generate vertex data for all text strings
        float* vertices = NULL;
        uint32_t* indices = NULL;
        uint32_t vertex_count = 0;
        uint32_t index_count = 0;

        int success = afferent_text_generate_vertices_batch(
            font, texts, positions, colors, transforms, count,
            canvas_width, canvas_height,
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

        // Ensure staging buffer is large enough
        if (vertex_count > g_text_vertex_staging_capacity) {
            free(g_text_vertex_staging);
            g_text_vertex_staging_capacity = vertex_count + 64;
            g_text_vertex_staging = malloc(g_text_vertex_staging_capacity * sizeof(TextVertex));
        }

        // Convert float vertex data to TextVertex format
        TextVertex* textVertices = g_text_vertex_staging;
        for (uint32_t i = 0; i < vertex_count; i++) {
            size_t base = i * 8;
            textVertices[i].position[0] = vertices[base + 0];
            textVertices[i].position[1] = vertices[base + 1];
            textVertices[i].texCoord[0] = vertices[base + 2];
            textVertices[i].texCoord[1] = vertices[base + 3];
            textVertices[i].color[0] = vertices[base + 4];
            textVertices[i].color[1] = vertices[base + 5];
            textVertices[i].color[2] = vertices[base + 6];
            textVertices[i].color[3] = vertices[base + 7];
        }

        // Use pooled Metal buffers
        size_t vertex_buffer_size = vertex_count * sizeof(TextVertex);
        size_t index_buffer_size = index_count * sizeof(uint32_t);

        id<MTLBuffer> vertexBuffer = pool_acquire_buffer(
            renderer->device,
            g_buffer_pool.text_vertex_pool,
            &g_buffer_pool.text_vertex_pool_count,
            vertex_buffer_size
        );
        id<MTLBuffer> indexBuffer = pool_acquire_buffer(
            renderer->device,
            g_buffer_pool.text_index_pool,
            &g_buffer_pool.text_index_pool_count,
            index_buffer_size
        );

        // Copy data into pooled buffers
        if (vertexBuffer) {
            memcpy(vertexBuffer.contents, textVertices, vertex_buffer_size);
        }
        if (indexBuffer) {
            memcpy(indexBuffer.contents, indices, index_buffer_size);
        }

        free(vertices);
        free(indices);

        if (!vertexBuffer || !indexBuffer) {
            return AFFERENT_ERROR_TEXT_FAILED;
        }

        // Switch to text pipeline and disable depth testing
        [renderer->currentEncoder setRenderPipelineState:renderer->textPipelineState];
        [renderer->currentEncoder setDepthStencilState:renderer->depthStateDisabled];

        // Set texture and sampler
        [renderer->currentEncoder setFragmentTexture:fontTexture atIndex:0];
        [renderer->currentEncoder setFragmentSamplerState:renderer->textSampler atIndex:0];

        // Draw all text quads in one call
        [renderer->currentEncoder setVertexBuffer:vertexBuffer offset:0 atIndex:0];
        [renderer->currentEncoder drawIndexedPrimitives:MTLPrimitiveTypeTriangle
                                             indexCount:index_count
                                              indexType:MTLIndexTypeUInt32
                                            indexBuffer:indexBuffer
                                      indexBufferOffset:0];

        // Switch back to basic pipeline
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

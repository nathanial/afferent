// draw_sprites.m - Sprite and texture rendering
#import "render.h"

// Create a Metal texture from raw RGBA pixel data
id<MTLTexture> createMetalTexture(id<MTLDevice> device, const uint8_t* data, uint32_t width, uint32_t height) {
    MTLTextureDescriptor *desc = [MTLTextureDescriptor texture2DDescriptorWithPixelFormat:MTLPixelFormatRGBA8Unorm
                                                                                    width:width
                                                                                   height:height
                                                                                mipmapped:YES];
    // Keep this conservative: shader-read is required; render-target helps some drivers/tools with mip generation paths.
    desc.usage = MTLTextureUsageShaderRead | MTLTextureUsageRenderTarget;
    desc.storageMode = MTLStorageModeManaged;

    id<MTLTexture> texture = [device newTextureWithDescriptor:desc];
    if (!texture) return nil;

    MTLRegion region = MTLRegionMake2D(0, 0, width, height);
    [texture replaceRegion:region
               mipmapLevel:0
                 withBytes:data
               bytesPerRow:width * 4];

    // Generate mip chain on CPU once (avoids needing a blit encoder mid-frame).
    // This matters a lot when drawing many minified sprites from a large source texture.
    const uint8_t* prev = data;
    uint8_t* prevOwned = NULL;
    uint32_t prevW = width;
    uint32_t prevH = height;

    uint32_t mipCount = (uint32_t)texture.mipmapLevelCount;
    for (uint32_t level = 1; level < mipCount; level++) {
        uint32_t nextW = prevW > 1 ? (prevW / 2) : 1;
        uint32_t nextH = prevH > 1 ? (prevH / 2) : 1;

        size_t nextSize = (size_t)nextW * (size_t)nextH * 4;
        uint8_t* next = (uint8_t*)malloc(nextSize);
        if (!next) {
            break;
        }

        for (uint32_t y = 0; y < nextH; y++) {
            uint32_t sy0 = (2 * y);
            uint32_t sy1 = (sy0 + 1 < prevH) ? (sy0 + 1) : (prevH - 1);
            for (uint32_t x = 0; x < nextW; x++) {
                uint32_t sx0 = (2 * x);
                uint32_t sx1 = (sx0 + 1 < prevW) ? (sx0 + 1) : (prevW - 1);

                const uint8_t* p00 = prev + ((size_t)sy0 * (size_t)prevW + (size_t)sx0) * 4;
                const uint8_t* p10 = prev + ((size_t)sy0 * (size_t)prevW + (size_t)sx1) * 4;
                const uint8_t* p01 = prev + ((size_t)sy1 * (size_t)prevW + (size_t)sx0) * 4;
                const uint8_t* p11 = prev + ((size_t)sy1 * (size_t)prevW + (size_t)sx1) * 4;

                uint32_t r = (uint32_t)p00[0] + (uint32_t)p10[0] + (uint32_t)p01[0] + (uint32_t)p11[0];
                uint32_t g = (uint32_t)p00[1] + (uint32_t)p10[1] + (uint32_t)p01[1] + (uint32_t)p11[1];
                uint32_t b = (uint32_t)p00[2] + (uint32_t)p10[2] + (uint32_t)p01[2] + (uint32_t)p11[2];
                uint32_t a = (uint32_t)p00[3] + (uint32_t)p10[3] + (uint32_t)p01[3] + (uint32_t)p11[3];

                uint8_t* dst = next + ((size_t)y * (size_t)nextW + (size_t)x) * 4;
                dst[0] = (uint8_t)(r >> 2);
                dst[1] = (uint8_t)(g >> 2);
                dst[2] = (uint8_t)(b >> 2);
                dst[3] = (uint8_t)(a >> 2);
            }
        }

        MTLRegion mipRegion = MTLRegionMake2D(0, 0, nextW, nextH);
        [texture replaceRegion:mipRegion
                   mipmapLevel:level
                     withBytes:next
                   bytesPerRow:nextW * 4];

        if (prevOwned) {
            free(prevOwned);
        }
        prev = next;
        prevOwned = next;
        prevW = nextW;
        prevH = nextH;
    }

    if (prevOwned) {
        free(prevOwned);
    }

    return texture;
}

static id<MTLTexture> afferent_get_sprite_texture(AfferentRendererRef renderer, AfferentTextureRef texture) {
    id<MTLTexture> metalTex = (__bridge id<MTLTexture>)afferent_texture_get_metal_texture(texture);

    if (!metalTex) {
        const uint8_t* pixelData = afferent_texture_get_data(texture);
        uint32_t width, height;
        afferent_texture_get_size(texture, &width, &height);

        if (!pixelData || width == 0 || height == 0) {
            return nil;
        }

        metalTex = createMetalTexture(renderer->device, pixelData, width, height);
        if (!metalTex) {
            return nil;
        }

        // Store for future use (transfer ownership via __bridge_retained)
        afferent_texture_set_metal_texture(texture, (__bridge_retained void*)metalTex);
    }

    return metalTex;
}

static void afferent_draw_textured_instances(
    AfferentRendererRef renderer,
    AfferentTextureRef texture,
    const float* data,
    uint32_t count,
    uint32_t layout,
    float canvasWidth,
    float canvasHeight,
    float u0,
    float v0,
    float u1,
    float v1,
    uint32_t useMatrix,
    float transformA,
    float transformB,
    float transformC,
    float transformD,
    float transformTx,
    float transformTy
) {
    if (!renderer || !renderer->currentEncoder || !texture || !data || count == 0) {
        return;
    }

    @autoreleasepool {
        id<MTLTexture> metalTex = afferent_get_sprite_texture(renderer, texture);
        if (!metalTex) {
            return;
        }

        size_t stride = (layout == 0) ? 5 : 10;
        size_t dataSize = (size_t)count * stride * sizeof(float);
        id<MTLBuffer> spriteBuffer = pool_acquire_buffer(
            renderer->device,
            g_buffer_pool.vertex_pool,
            &g_buffer_pool.vertex_pool_count,
            dataSize,
            true
        );

        if (!spriteBuffer) {
            NSLog(@"Failed to acquire sprite instance buffer");
            return;
        }

        memcpy(spriteBuffer.contents, data, dataSize);

        SpriteUniforms uniforms = {
            .viewport = { canvasWidth, canvasHeight },
            .layout = layout,
            .useMatrix = useMatrix,
            .uvRect = { u0, v0, u1, v1 },
            .transform0 = { transformA, transformB, transformC, transformD },
            .transform1 = { transformTx, transformTy, 0.0f, 0.0f }
        };

        id<MTLRenderPipelineState> pipeline = (layout == 0)
            ? renderer->spritePipelineState
            : renderer->texturedSpritePipelineState;
        if (!pipeline) {
            return;
        }

        [renderer->currentEncoder setRenderPipelineState:pipeline];
        [renderer->currentEncoder setVertexBuffer:spriteBuffer offset:0 atIndex:0];
        [renderer->currentEncoder setVertexBytes:&uniforms length:sizeof(uniforms) atIndex:1];
        [renderer->currentEncoder setFragmentTexture:metalTex atIndex:0];
        [renderer->currentEncoder setFragmentSamplerState:renderer->spriteSampler atIndex:0];
        [renderer->currentEncoder drawPrimitives:MTLPrimitiveTypeTriangleStrip
                                     vertexStart:0
                                     vertexCount:4
                                   instanceCount:count];
        [renderer->currentEncoder setRenderPipelineState:renderer->pipelineState];
    }
}

// Draw textured sprites (positions/rotation updated each frame)
// data: [pixelX, pixelY, rotation, halfSizePixels, alpha] × count (5 floats per sprite)
void afferent_renderer_draw_sprites(
    AfferentRendererRef renderer,
    AfferentTextureRef texture,
    const float* data,
    uint32_t count,
    float canvasWidth,
    float canvasHeight
) {
    afferent_draw_textured_instances(
        renderer,
        texture,
        data,
        count,
        0,
        canvasWidth,
        canvasHeight,
        0.0f,
        0.0f,
        1.0f,
        1.0f,
        0,
        1.0f, 0.0f, 0.0f, 1.0f,
        0.0f, 0.0f
    );
}

// Draw sprites with a transform matrix (world-space or custom projection)
// data: [pixelX, pixelY, rotation, halfSizePixels, alpha] × count (5 floats per sprite)
void afferent_renderer_draw_sprites_matrix(
    AfferentRendererRef renderer,
    AfferentTextureRef texture,
    const float* data,
    uint32_t count,
    float canvasWidth,
    float canvasHeight,
    float transformA,
    float transformB,
    float transformC,
    float transformD,
    float transformTx,
    float transformTy
) {
    afferent_draw_textured_instances(
        renderer,
        texture,
        data,
        count,
        0,
        canvasWidth,
        canvasHeight,
        0.0f,
        0.0f,
        1.0f,
        1.0f,
        1,
        transformA, transformB, transformC, transformD,
        transformTx, transformTy
    );
}

// Draw sprites from FloatBuffer that already contains sprite layout (5 floats)
void afferent_renderer_draw_sprites_instance_buffer(
    AfferentRendererRef renderer,
    AfferentTextureRef texture,
    const float* data,
    uint32_t count,
    float canvasWidth,
    float canvasHeight
) {
    // Same layout as afferent_renderer_draw_sprites, so forward directly
    afferent_draw_textured_instances(
        renderer,
        texture,
        data,
        count,
        0,
        canvasWidth,
        canvasHeight,
        0.0f,
        0.0f,
        1.0f,
        1.0f,
        0,
        1.0f, 0.0f, 0.0f, 1.0f,
        0.0f, 0.0f
    );
}

// Draw sprites from FloatBuffer with a transform matrix (world-space or custom projection)
void afferent_renderer_draw_sprites_instance_buffer_matrix(
    AfferentRendererRef renderer,
    AfferentTextureRef texture,
    const float* data,
    uint32_t count,
    float canvasWidth,
    float canvasHeight,
    float transformA,
    float transformB,
    float transformC,
    float transformD,
    float transformTx,
    float transformTy
) {
    afferent_draw_textured_instances(
        renderer,
        texture,
        data,
        count,
        0,
        canvasWidth,
        canvasHeight,
        0.0f,
        0.0f,
        1.0f,
        1.0f,
        1,
        transformA, transformB, transformC, transformD,
        transformTx, transformTy
    );
}

// Release Metal texture associated with an AfferentTexture (called when texture is destroyed)
void afferent_release_sprite_metal_texture(AfferentTextureRef texture) {
    if (!texture) return;

    void* metalTexPtr = afferent_texture_get_metal_texture(texture);
    if (metalTexPtr) {
        // Release the Metal texture (transfer back ownership with __bridge_transfer)
        id<MTLTexture> metalTex = (__bridge_transfer id<MTLTexture>)metalTexPtr;
        metalTex = nil;  // ARC will release
        afferent_texture_set_metal_texture(texture, NULL);
    }
}

// Draw a textured rectangle with source and destination rectangles
// Used for map tile rendering with cropping and scaling
void afferent_renderer_draw_textured_rect(
    AfferentRendererRef renderer,
    AfferentTextureRef texture,
    float srcX, float srcY, float srcW, float srcH,
    float dstX, float dstY, float dstW, float dstH,
    float canvasWidth, float canvasHeight,
    float alpha
) {
    if (!renderer || !renderer->currentEncoder || !texture) {
        return;
    }

    // Get texture dimensions for UV conversion
    uint32_t texWidth, texHeight;
    afferent_texture_get_size(texture, &texWidth, &texHeight);
    if (texWidth == 0 || texHeight == 0) {
        return;
    }

    float u0 = srcX / (float)texWidth;
    float v0 = srcY / (float)texHeight;
    float u1 = (srcX + srcW) / (float)texWidth;
    float v1 = (srcY + srcH) / (float)texHeight;

    float centerX = dstX + dstW * 0.5f;
    float centerY = dstY + dstH * 0.5f;
    float halfW = dstW * 0.5f;
    float halfH = dstH * 0.5f;

    float instance[10] = {
        centerX,
        centerY,
        0.0f,
        halfW,
        halfH,
        u0, v0, u1, v1,
        alpha
    };

    afferent_draw_textured_instances(
        renderer,
        texture,
        instance,
        1,
        1,
        canvasWidth,
        canvasHeight,
        0.0f,
        0.0f,
        1.0f,
        1.0f,
        0,
        1.0f, 0.0f, 0.0f, 1.0f,
        0.0f, 0.0f
    );
}

// Draw sprites from FloatBuffer using physics layout.
// Buffer layout: [x, y, vx, vy, rotation] per sprite (5 floats).
// Converted on CPU into sprite layout with uniform halfSize and alpha=1.0.
void afferent_renderer_draw_sprites_buffer(
    AfferentRendererRef renderer,
    AfferentTextureRef texture,
    const float* data,
    uint32_t count,
    float halfSize,
    float canvasWidth,
    float canvasHeight
) {
    if (!renderer || !renderer->currentEncoder || !texture || !data || count == 0) {
        return;
    }

    @autoreleasepool {
        id<MTLTexture> metalTex = afferent_get_sprite_texture(renderer, texture);
        if (!metalTex) {
            return;
        }

        // Convert physics layout [x, y, vx, vy, rotation] -> sprite layout (5 floats)
        size_t instanceSize = (size_t)count * 5 * sizeof(float);
        id<MTLBuffer> spriteBuffer = pool_acquire_buffer(
            renderer->device,
            g_buffer_pool.vertex_pool,
            &g_buffer_pool.vertex_pool_count,
            instanceSize,
            true
        );

        if (!spriteBuffer) {
            NSLog(@"Failed to acquire sprite buffer");
            return;
        }

        float* instances = (float*)spriteBuffer.contents;
        for (uint32_t i = 0; i < count; i++) {
            const float* src = data + i * 5;
            size_t base = (size_t)i * 5;
            instances[base + 0] = src[0];
            instances[base + 1] = src[1];
            instances[base + 2] = src[4];
            instances[base + 3] = halfSize;
            instances[base + 4] = 1.0f;
        }

        SpriteUniforms uniforms = {
            .viewport = { canvasWidth, canvasHeight },
            .layout = 0,
            .useMatrix = 0,
            .uvRect = { 0.0f, 0.0f, 1.0f, 1.0f },
            .transform0 = { 1.0f, 0.0f, 0.0f, 1.0f },
            .transform1 = { 0.0f, 0.0f, 0.0f, 0.0f }
        };

        [renderer->currentEncoder setRenderPipelineState:renderer->spritePipelineState];
        [renderer->currentEncoder setVertexBuffer:spriteBuffer offset:0 atIndex:0];
        [renderer->currentEncoder setVertexBytes:&uniforms length:sizeof(uniforms) atIndex:1];
        [renderer->currentEncoder setFragmentTexture:metalTex atIndex:0];
        [renderer->currentEncoder setFragmentSamplerState:renderer->spriteSampler atIndex:0];
        [renderer->currentEncoder drawPrimitives:MTLPrimitiveTypeTriangleStrip
                                     vertexStart:0
                                     vertexCount:4
                                   instanceCount:count];
        [renderer->currentEncoder setRenderPipelineState:renderer->pipelineState];
    }
}

// Draw textured instances with per-instance UV rects and size (10 floats per instance).
void afferent_renderer_draw_textured_instances(
    AfferentRendererRef renderer,
    AfferentTextureRef texture,
    const float* data,
    uint32_t count,
    float canvasWidth,
    float canvasHeight
) {
    afferent_draw_textured_instances(
        renderer,
        texture,
        data,
        count,
        1,
        canvasWidth,
        canvasHeight,
        0.0f,
        0.0f,
        1.0f,
        1.0f,
        0,
        1.0f, 0.0f, 0.0f, 1.0f,
        0.0f, 0.0f
    );
}

// Draw textured instances with a transform matrix (world-space or custom projection).
void afferent_renderer_draw_textured_instances_matrix(
    AfferentRendererRef renderer,
    AfferentTextureRef texture,
    const float* data,
    uint32_t count,
    float canvasWidth,
    float canvasHeight,
    float transformA,
    float transformB,
    float transformC,
    float transformD,
    float transformTx,
    float transformTy
) {
    afferent_draw_textured_instances(
        renderer,
        texture,
        data,
        count,
        1,
        canvasWidth,
        canvasHeight,
        0.0f,
        0.0f,
        1.0f,
        1.0f,
        1,
        transformA, transformB, transformC, transformD,
        transformTx, transformTy
    );
}

// Draw textured instances from FloatBuffer with a transform matrix.
void afferent_renderer_draw_textured_instances_buffer_matrix(
    AfferentRendererRef renderer,
    AfferentTextureRef texture,
    const float* data,
    uint32_t count,
    float canvasWidth,
    float canvasHeight,
    float transformA,
    float transformB,
    float transformC,
    float transformD,
    float transformTx,
    float transformTy
) {
    afferent_draw_textured_instances(
        renderer,
        texture,
        data,
        count,
        1,
        canvasWidth,
        canvasHeight,
        0.0f,
        0.0f,
        1.0f,
        1.0f,
        1,
        transformA, transformB, transformC, transformD,
        transformTx, transformTy
    );
}

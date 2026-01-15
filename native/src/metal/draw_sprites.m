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

static void afferent_draw_sprites_internal(
    AfferentRendererRef renderer,
    AfferentTextureRef texture,
    const float* data,
    uint32_t count,
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

        size_t dataSize = (size_t)count * 5 * sizeof(float);
        id<MTLBuffer> spriteBuffer = pool_acquire_buffer(
            renderer->device,
            g_buffer_pool.vertex_pool,
            &g_buffer_pool.vertex_pool_count,
            dataSize
        );

        if (!spriteBuffer) {
            NSLog(@"Failed to acquire sprite instance buffer");
            return;
        }

        memcpy(spriteBuffer.contents, data, dataSize);

        SpriteUniforms uniforms = {
            .viewport = { canvasWidth, canvasHeight }
        };

        [renderer->currentEncoder setRenderPipelineState:renderer->spritePipelineState];
        // Disable depth testing for 2D rendering (may have been enabled by 3D)
        [renderer->currentEncoder setDepthStencilState:renderer->depthStateDisabled];
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
    afferent_draw_sprites_internal(renderer, texture, data, count, canvasWidth, canvasHeight);
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

/*
 * Afferent Texture Loading
 * Creates GPU textures from decoded RGBA pixel data.
 * Image decoding is handled by the raster library at the Lean level.
 */

#include "../include/afferent.h"
#include <stdlib.h>
#include <string.h>

// Texture structure
struct AfferentTexture {
    uint8_t* data;          // RGBA pixel data (owned copy)
    uint32_t width;
    uint32_t height;
    void* metal_texture;    // id<MTLTexture>, managed by metal_render.m
};

// Create a texture from already-decoded RGBA pixel data
// This is the primary constructor - decoding is done by raster library
AfferentResult afferent_texture_create_from_rgba(
    const uint8_t* rgba_data,
    uint32_t width,
    uint32_t height,
    AfferentTextureRef* out_texture
) {
    if (!rgba_data || width == 0 || height == 0 || !out_texture) {
        return AFFERENT_ERROR_INIT_FAILED;
    }

    // Allocate texture structure
    AfferentTextureRef texture = (AfferentTextureRef)malloc(sizeof(struct AfferentTexture));
    if (!texture) {
        return AFFERENT_ERROR_INIT_FAILED;
    }

    // Copy the pixel data (we need to own it)
    size_t data_size = (size_t)width * height * 4;
    texture->data = (uint8_t*)malloc(data_size);
    if (!texture->data) {
        free(texture);
        return AFFERENT_ERROR_INIT_FAILED;
    }
    memcpy(texture->data, rgba_data, data_size);

    texture->width = width;
    texture->height = height;
    texture->metal_texture = NULL;  // Created lazily by renderer

    *out_texture = texture;
    return AFFERENT_OK;
}

// External declaration from metal_render.m
extern void afferent_release_sprite_metal_texture(AfferentTextureRef texture);

// Destroy a texture and free its resources
void afferent_texture_destroy(AfferentTextureRef texture) {
    if (!texture) return;

    // Release Metal texture first (before we free the struct)
    afferent_release_sprite_metal_texture(texture);

    if (texture->data) {
        free(texture->data);
        texture->data = NULL;
    }

    free(texture);
}

// Get texture dimensions
void afferent_texture_get_size(AfferentTextureRef texture, uint32_t* width, uint32_t* height) {
    if (!texture) {
        if (width) *width = 0;
        if (height) *height = 0;
        return;
    }
    if (width) *width = texture->width;
    if (height) *height = texture->height;
}

// Get texture pixel data (for Metal texture creation)
const uint8_t* afferent_texture_get_data(AfferentTextureRef texture) {
    return texture ? texture->data : NULL;
}

// Get/set Metal texture handle
void* afferent_texture_get_metal_texture(AfferentTextureRef texture) {
    return texture ? texture->metal_texture : NULL;
}

void afferent_texture_set_metal_texture(AfferentTextureRef texture, void* metal_tex) {
    if (texture) {
        texture->metal_texture = metal_tex;
    }
}

/*
 * Afferent Texture Loading
 * Uses stb_image for PNG/image loading
 */

#define STB_IMAGE_IMPLEMENTATION
#include "stb_image.h"
#include "../include/afferent.h"
#include <stdlib.h>
#include <string.h>

// Texture structure
struct AfferentTexture {
    uint8_t* data;          // RGBA pixel data
    uint32_t width;
    uint32_t height;
    void* metal_texture;    // id<MTLTexture>, managed by metal_render.m
};

// Load a texture from a file path
AfferentResult afferent_texture_load(const char* path, AfferentTextureRef* out_texture) {
    if (!path || !out_texture) {
        return AFFERENT_ERROR_INIT_FAILED;
    }

    // Load image with stb_image (force RGBA)
    int width, height, channels;
    uint8_t* data = stbi_load(path, &width, &height, &channels, 4);  // Force 4 channels (RGBA)

    if (!data) {
        return AFFERENT_ERROR_INIT_FAILED;
    }

    // Allocate texture structure
    AfferentTextureRef texture = (AfferentTextureRef)malloc(sizeof(struct AfferentTexture));
    if (!texture) {
        stbi_image_free(data);
        return AFFERENT_ERROR_INIT_FAILED;
    }

    texture->data = data;
    texture->width = (uint32_t)width;
    texture->height = (uint32_t)height;
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
        stbi_image_free(texture->data);
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

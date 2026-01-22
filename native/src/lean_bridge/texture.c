#include "lean_bridge_internal.h"

// ============================================================================
// Texture/Sprite Rendering FFI
// ============================================================================

// Create texture from decoded RGBA pixel data
// Takes ByteArray of RGBA data, width, and height
LEAN_EXPORT lean_obj_res lean_afferent_texture_create_from_rgba(
    lean_obj_arg data_obj,
    uint32_t width,
    uint32_t height,
    lean_obj_arg world
) {
    afferent_ensure_initialized();

    // Get ByteArray data (should be width * height * 4 bytes)
    size_t size = lean_sarray_size(data_obj);
    size_t expected = (size_t)width * height * 4;
    if (size < expected) {
        return lean_io_result_mk_error(lean_mk_io_user_error(
            lean_mk_string("Texture data too small for specified dimensions")));
    }

    const uint8_t* data = lean_sarray_cptr(data_obj);

    AfferentTextureRef texture = NULL;
    AfferentResult result = afferent_texture_create_from_rgba(data, width, height, &texture);

    if (result != AFFERENT_OK) {
        return lean_io_result_mk_error(lean_mk_io_user_error(
            lean_mk_string("Failed to create texture from RGBA data")));
    }

    lean_object* obj = lean_alloc_external(g_texture_class, texture);
    return lean_io_result_mk_ok(obj);
}

// Destroy texture
LEAN_EXPORT lean_obj_res lean_afferent_texture_destroy(
    lean_obj_arg texture_obj,
    lean_obj_arg world
) {
    AfferentTextureRef texture = (AfferentTextureRef)lean_get_external_data(texture_obj);
    afferent_texture_destroy(texture);
    return lean_io_result_mk_ok(lean_box(0));
}

// Get texture size
LEAN_EXPORT lean_obj_res lean_afferent_texture_get_size(
    lean_obj_arg texture_obj,
    lean_obj_arg world
) {
    AfferentTextureRef texture = (AfferentTextureRef)lean_get_external_data(texture_obj);
    uint32_t width = 0, height = 0;
    afferent_texture_get_size(texture, &width, &height);

    // Return UInt32 × UInt32 = Prod UInt32 UInt32
    lean_object* pair = lean_alloc_ctor(0, 2, 0);
    lean_ctor_set(pair, 0, lean_box_uint32(width));
    lean_ctor_set(pair, 1, lean_box_uint32(height));
    return lean_io_result_mk_ok(pair);
}

// Draw sprites from FloatBuffer already in SpriteInstanceData layout
LEAN_EXPORT lean_obj_res lean_afferent_renderer_draw_sprites_instance_buffer(
    lean_obj_arg renderer_obj,
    lean_obj_arg texture_obj,
    lean_obj_arg buffer_obj,
    uint32_t count,
    double canvasWidth,
    double canvasHeight,
    lean_obj_arg world
) {
    AfferentRendererRef renderer = (AfferentRendererRef)lean_get_external_data(renderer_obj);
    AfferentTextureRef texture = (AfferentTextureRef)lean_get_external_data(texture_obj);
    AfferentFloatBufferRef buffer = (AfferentFloatBufferRef)lean_get_external_data(buffer_obj);

    afferent_renderer_draw_sprites(
        renderer, texture,
        afferent_float_buffer_data(buffer),
        count, (float)canvasWidth, (float)canvasHeight
    );
    return lean_io_result_mk_ok(lean_box(0));
}

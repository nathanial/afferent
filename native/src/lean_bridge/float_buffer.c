#include "lean_bridge_internal.h"
#include <string.h>

// ============== FloatBuffer FFI ==============
// High-performance mutable float buffer for instance data
// Avoids Lean's copy-on-write array semantics

LEAN_EXPORT lean_obj_res lean_afferent_float_buffer_create(size_t capacity, lean_obj_arg world) {
    afferent_ensure_initialized();
    AfferentFloatBufferRef buffer = NULL;
    AfferentResult result = afferent_float_buffer_create(capacity, &buffer);

    if (result != AFFERENT_OK) {
        return lean_io_result_mk_error(lean_mk_io_user_error(
            lean_mk_string("Failed to create float buffer")));
    }

    lean_object* obj = lean_alloc_external(g_float_buffer_class, buffer);
    return lean_io_result_mk_ok(obj);
}

LEAN_EXPORT lean_obj_res lean_afferent_float_buffer_destroy(lean_obj_arg buffer_obj, lean_obj_arg world) {
    AfferentFloatBufferRef buffer = (AfferentFloatBufferRef)lean_get_external_data(buffer_obj);
    afferent_float_buffer_destroy(buffer);
    return lean_io_result_mk_ok(lean_box(0));
}

LEAN_EXPORT lean_obj_res lean_afferent_float_buffer_set(
    lean_obj_arg buffer_obj,
    size_t index,
    double value,
    lean_obj_arg world
) {
    AfferentFloatBufferRef buffer = (AfferentFloatBufferRef)lean_get_external_data(buffer_obj);
    afferent_float_buffer_set(buffer, index, (float)value);
    return lean_io_result_mk_ok(lean_box(0));
}

LEAN_EXPORT lean_obj_res lean_afferent_float_buffer_get(
    lean_obj_arg buffer_obj,
    size_t index,
    lean_obj_arg world
) {
    AfferentFloatBufferRef buffer = (AfferentFloatBufferRef)lean_get_external_data(buffer_obj);
    float value = afferent_float_buffer_get(buffer, index);
    return lean_io_result_mk_ok(lean_box_float((double)value));
}

LEAN_EXPORT lean_obj_res lean_afferent_float_buffer_set_count(
    lean_obj_arg buffer_obj,
    size_t count,
    lean_obj_arg world
) {
    AfferentFloatBufferRef buffer = (AfferentFloatBufferRef)lean_get_external_data(buffer_obj);
    afferent_float_buffer_set_count(buffer, count);
    return lean_io_result_mk_ok(lean_box(0));
}

// Set 8 floats at once - 8x less FFI overhead than 8 separate calls
LEAN_EXPORT lean_obj_res lean_afferent_float_buffer_set_vec8(
    lean_obj_arg buffer_obj,
    size_t index,
    double v0, double v1, double v2, double v3,
    double v4, double v5, double v6, double v7,
    lean_obj_arg world
) {
    AfferentFloatBufferRef buffer = (AfferentFloatBufferRef)lean_get_external_data(buffer_obj);
    afferent_float_buffer_set_vec8(buffer, index,
        (float)v0, (float)v1, (float)v2, (float)v3,
        (float)v4, (float)v5, (float)v6, (float)v7);
    return lean_io_result_mk_ok(lean_box(0));
}

LEAN_EXPORT lean_obj_res lean_afferent_float_buffer_set_vec9(
    lean_obj_arg buffer_obj,
    size_t index,
    double v0, double v1, double v2, double v3,
    double v4, double v5, double v6, double v7, double v8,
    lean_obj_arg world
) {
    AfferentFloatBufferRef buffer = (AfferentFloatBufferRef)lean_get_external_data(buffer_obj);
    afferent_float_buffer_set_vec9(buffer, index,
        (float)v0, (float)v1, (float)v2, (float)v3,
        (float)v4, (float)v5, (float)v6, (float)v7, (float)v8);
    return lean_io_result_mk_ok(lean_box(0));
}

LEAN_EXPORT lean_obj_res lean_afferent_float_buffer_set_vec5(
    lean_obj_arg buffer_obj,
    size_t index,
    double v0, double v1, double v2, double v3, double v4,
    lean_obj_arg world
) {
    AfferentFloatBufferRef buffer = (AfferentFloatBufferRef)lean_get_external_data(buffer_obj);
    afferent_float_buffer_set_vec5(buffer, index,
        (float)v0, (float)v1, (float)v2, (float)v3, (float)v4);
    return lean_io_result_mk_ok(lean_box(0));
}

// Bulk-write packed params into a padded layout in FloatBuffer.
// params_arr: Array Float (packed)
// offsets_arr: Array Nat (packed index -> padded index)
LEAN_EXPORT lean_obj_res lean_afferent_float_buffer_write_padded(
    lean_obj_arg buffer_obj,
    lean_obj_arg params_arr,
    uint32_t packed_count,
    uint32_t padded_count,
    lean_obj_arg offsets_arr,
    lean_obj_arg world
) {
    AfferentFloatBufferRef buffer = (AfferentFloatBufferRef)lean_get_external_data(buffer_obj);
    if (!buffer || packed_count == 0 || padded_count == 0) {
        return lean_io_result_mk_ok(lean_box(0));
    }

    size_t param_count = lean_array_size(params_arr);
    if (param_count == 0 || param_count % packed_count != 0) {
        return lean_io_result_mk_ok(lean_box(0));
    }

    size_t offsets_size = lean_array_size(offsets_arr);
    if (offsets_size != packed_count) {
        return lean_io_result_mk_ok(lean_box(0));
    }

    size_t batch_count = param_count / packed_count;
    size_t needed = batch_count * padded_count;
    if (afferent_float_buffer_capacity(buffer) < needed) {
        return lean_io_result_mk_ok(lean_box(0));
    }

    float* out = (float*)afferent_float_buffer_data(buffer);
    memset(out, 0, needed * sizeof(float));

    for (size_t batch = 0; batch < batch_count; batch++) {
        size_t base_in = batch * packed_count;
        size_t base_out = batch * padded_count;
        for (size_t i = 0; i < packed_count; i++) {
            size_t out_off = (size_t)lean_unbox(lean_array_get_core(offsets_arr, i));
            float value = (float)lean_unbox_float(lean_array_get_core(params_arr, base_in + i));
            out[base_out + out_off] = value;
        }
    }

    afferent_float_buffer_set_count(buffer, needed);
    return lean_io_result_mk_ok(lean_box(0));
}

// Bulk-write sprite instance data from Lean particle array into a FloatBuffer.
// particle_data_arr layout: [x, y, vx, vy, hue] per particle (5 floats).
// Writes SpriteInstanceData layout into buffer: [x, y, rotation, halfSize, alpha].
LEAN_EXPORT lean_obj_res lean_afferent_float_buffer_write_sprites_from_particles(
    lean_obj_arg buffer_obj,
    lean_obj_arg particle_data_arr,
    uint32_t count,
    double halfSize,
    double rotation,
    double alpha,
    lean_obj_arg world
) {
    AfferentFloatBufferRef buffer = (AfferentFloatBufferRef)lean_get_external_data(buffer_obj);

    // particle_data_arr is a FloatArray (unboxed doubles in an sarray)
    size_t arr_size = (size_t)lean_unbox(lean_float_array_size(particle_data_arr));
    size_t expected_size = (size_t)count * 5;
    if (count == 0 || arr_size < expected_size) {
        return lean_io_result_mk_ok(lean_box(0));
    }

    if (afferent_float_buffer_capacity(buffer) < expected_size) {
        return lean_io_result_mk_ok(lean_box(0));
    }

    float h = (float)halfSize;
    float r = (float)rotation;
    float a = (float)alpha;

    const double* src = lean_float_array_cptr(particle_data_arr);
    for (uint32_t i = 0; i < count; i++) {
        size_t base = (size_t)i * 5;
        float x = (float)src[base];
        float y = (float)src[base + 1];
        afferent_float_buffer_set_vec5(buffer, base, x, y, r, h, a);
    }

    return lean_io_result_mk_ok(lean_box(0));
}

// Bulk-write instanced shape data from Lean particle array into a FloatBuffer.
// particle_data_arr layout: [x, y, vx, vy, hue] per particle (5 doubles).
// Writes InstanceData layout into buffer: [x, y, rotation, halfSize, hue, 0, 0, 1].
// rotation_mode: 0 = uniform rotation, 1 = animated (time * spinSpeed + hue * 2π).
LEAN_EXPORT lean_obj_res lean_afferent_float_buffer_write_instanced_from_particles(
    lean_obj_arg buffer_obj,
    lean_obj_arg particle_data_arr,
    uint32_t count,
    double halfSize,
    double rotation,
    double time,
    double spinSpeed,
    uint32_t rotation_mode,
    lean_obj_arg world
) {
    AfferentFloatBufferRef buffer = (AfferentFloatBufferRef)lean_get_external_data(buffer_obj);

    size_t arr_size = (size_t)lean_unbox(lean_float_array_size(particle_data_arr));
    size_t expected_size = (size_t)count * 5;
    if (count == 0 || arr_size < expected_size) {
        return lean_io_result_mk_ok(lean_box(0));
    }

    size_t out_needed = (size_t)count * 8;
    if (!buffer || afferent_float_buffer_capacity(buffer) < out_needed) {
        return lean_io_result_mk_ok(lean_box(0));
    }

    const double* src = lean_float_array_cptr(particle_data_arr);
    float* out = (float*)afferent_float_buffer_data(buffer);
    float h = (float)halfSize;
    float rot = (float)rotation;
    float t = (float)time;
    float spin = (float)spinSpeed;
    const float two_pi = 6.283185307f;

    for (uint32_t i = 0; i < count; i++) {
        size_t base = (size_t)i * 5;
        float x = (float)src[base];
        float y = (float)src[base + 1];
        float hue = (float)src[base + 4];
        float angle = rot;
        if (rotation_mode == 1) {
            angle = t * spin + hue * two_pi;
        }

        size_t o = (size_t)i * 8;
        out[o + 0] = x;
        out[o + 1] = y;
        out[o + 2] = angle;
        out[o + 3] = h;
        out[o + 4] = hue;
        out[o + 5] = 0.0f;
        out[o + 6] = 0.0f;
        out[o + 7] = 1.0f;
    }

    return lean_io_result_mk_ok(lean_box(0));
}

// Draw instanced shapes directly from FloatBuffer (zero-copy path)
// shape_type: 0=rect, 1=triangle, 2=circle
LEAN_EXPORT lean_obj_res lean_afferent_renderer_draw_instanced_shapes_buffer(
    lean_obj_arg renderer_obj,
    uint32_t shape_type,
    lean_obj_arg buffer_obj,
    uint32_t instance_count,
    double transform_a,
    double transform_b,
    double transform_c,
    double transform_d,
    double transform_tx,
    double transform_ty,
    double viewport_width,
    double viewport_height,
    uint32_t size_mode,
    double time,
    double hue_speed,
    uint32_t color_mode,
    lean_obj_arg world
) {
    AfferentRendererRef renderer = (AfferentRendererRef)lean_get_external_data(renderer_obj);
    AfferentFloatBufferRef buffer = (AfferentFloatBufferRef)lean_get_external_data(buffer_obj);

    afferent_renderer_draw_instanced_shapes(
        renderer,
        shape_type,
        afferent_float_buffer_data(buffer),
        instance_count,
        (float)transform_a,
        (float)transform_b,
        (float)transform_c,
        (float)transform_d,
        (float)transform_tx,
        (float)transform_ty,
        (float)viewport_width,
        (float)viewport_height,
        size_mode,
        (float)time,
        (float)hue_speed,
        color_mode
    );
    return lean_io_result_mk_ok(lean_box(0));
}

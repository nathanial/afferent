#include "lean_bridge_internal.h"

// ============== Font/Text FFI ==============

// Load a font from file
LEAN_EXPORT lean_obj_res lean_afferent_font_load(
    lean_obj_arg path_obj,
    uint32_t size,
    lean_obj_arg world
) {
    afferent_ensure_initialized();
    const char* path = lean_string_cstr(path_obj);
    AfferentFontRef font = NULL;
    AfferentResult result = afferent_font_load(path, size, &font);

    if (result != AFFERENT_OK) {
        return lean_io_result_mk_error(lean_mk_io_user_error(
            lean_mk_string("Failed to load font")));
    }

    lean_object* obj = lean_alloc_external(g_font_class, font);
    return lean_io_result_mk_ok(obj);
}

// Destroy a font
LEAN_EXPORT lean_obj_res lean_afferent_font_destroy(lean_obj_arg font_obj, lean_obj_arg world) {
    AfferentFontRef font = (AfferentFontRef)lean_get_external_data(font_obj);
    afferent_font_destroy(font);
    return lean_io_result_mk_ok(lean_box(0));
}

// Get font metrics (returns a tuple: ascender, descender, line_height)
// Float × Float × Float = Prod Float (Prod Float Float)
// Prod has constructor tag 0 with 2 object fields (fst, snd)
LEAN_EXPORT lean_obj_res lean_afferent_font_get_metrics(lean_obj_arg font_obj, lean_obj_arg world) {
    AfferentFontRef font = (AfferentFontRef)lean_get_external_data(font_obj);
    float ascender, descender, line_height;
    afferent_font_get_metrics(font, &ascender, &descender, &line_height);

    // Build nested tuple: (ascender, (descender, line_height))
    // Inner tuple: (descender, line_height)
    lean_object* inner = lean_alloc_ctor(0, 2, 0);  // Prod with 2 object fields
    lean_ctor_set(inner, 0, lean_box_float((double)descender));
    lean_ctor_set(inner, 1, lean_box_float((double)line_height));

    // Outer tuple: (ascender, inner)
    lean_object* outer = lean_alloc_ctor(0, 2, 0);  // Prod with 2 object fields
    lean_ctor_set(outer, 0, lean_box_float((double)ascender));
    lean_ctor_set(outer, 1, inner);

    return lean_io_result_mk_ok(outer);
}

// Measure text dimensions (returns a tuple: width, height)
// Float × Float = Prod Float Float with 2 object fields (boxed floats)
LEAN_EXPORT lean_obj_res lean_afferent_text_measure(
    lean_obj_arg font_obj,
    lean_obj_arg text_obj,
    lean_obj_arg world
) {
    AfferentFontRef font = (AfferentFontRef)lean_get_external_data(font_obj);
    const char* text = lean_string_cstr(text_obj);
    float width, height;
    afferent_text_measure(font, text, &width, &height);

    // Return as a Prod with 2 boxed floats
    lean_object* tuple = lean_alloc_ctor(0, 2, 0);
    lean_ctor_set(tuple, 0, lean_box_float((double)width));
    lean_ctor_set(tuple, 1, lean_box_float((double)height));
    return lean_io_result_mk_ok(tuple);
}

// Render text
LEAN_EXPORT lean_obj_res lean_afferent_text_render(
    lean_obj_arg renderer_obj,
    lean_obj_arg font_obj,
    lean_obj_arg text_obj,
    double x,
    double y,
    double r,
    double g,
    double b,
    double a,
    lean_obj_arg transform_arr,
    double canvas_width,
    double canvas_height,
    lean_obj_arg world
) {
    AfferentRendererRef renderer = (AfferentRendererRef)lean_get_external_data(renderer_obj);
    AfferentFontRef font = (AfferentFontRef)lean_get_external_data(font_obj);
    const char* text = lean_string_cstr(text_obj);

    // Extract transform array (6 floats: a, b, c, d, tx, ty)
    float transform[6] = {1.0f, 0.0f, 0.0f, 1.0f, 0.0f, 0.0f};  // Identity default
    size_t arr_size = lean_array_size(transform_arr);
    if (arr_size >= 6) {
        for (size_t i = 0; i < 6; i++) {
            transform[i] = (float)lean_unbox_float(lean_array_get_core(transform_arr, i));
        }
    }

    AfferentResult result = afferent_text_render(
        renderer, font, text,
        (float)x, (float)y,
        (float)r, (float)g, (float)b, (float)a,
        transform,
        (float)canvas_width, (float)canvas_height
    );

    if (result != AFFERENT_OK) {
        return lean_io_result_mk_error(lean_mk_io_user_error(
            lean_mk_string("Failed to render text")));
    }

    return lean_io_result_mk_ok(lean_box(0));
}

// Batch text rendering - multiple strings with same font in one draw call
LEAN_EXPORT lean_obj_res lean_afferent_text_render_batch(
    lean_obj_arg renderer_obj,
    lean_obj_arg font_obj,
    lean_obj_arg texts_arr,
    lean_obj_arg positions_arr,
    lean_obj_arg colors_arr,
    lean_obj_arg transforms_arr,
    double canvas_width,
    double canvas_height,
    lean_obj_arg world
) {
    AfferentRendererRef renderer = (AfferentRendererRef)lean_get_external_data(renderer_obj);
    AfferentFontRef font = (AfferentFontRef)lean_get_external_data(font_obj);

    uint32_t count = (uint32_t)lean_array_size(texts_arr);
    if (count == 0) {
        return lean_io_result_mk_ok(lean_box(0));
    }

    // Extract text strings
    const char** texts = malloc(count * sizeof(char*));
    if (!texts) {
        return lean_io_result_mk_error(lean_mk_io_user_error(
            lean_mk_string("Failed to allocate text array")));
    }
    for (uint32_t i = 0; i < count; i++) {
        texts[i] = lean_string_cstr(lean_array_get_core(texts_arr, i));
    }

    // Extract positions array (2 floats per entry)
    size_t pos_size = lean_array_size(positions_arr);
    float* positions = NULL;
    if (pos_size >= count * 2) {
        positions = malloc(count * 2 * sizeof(float));
        if (positions) {
            for (size_t i = 0; i < count * 2; i++) {
                positions[i] = (float)lean_unbox_float(lean_array_get_core(positions_arr, i));
            }
        }
    }

    // Extract colors array (4 floats per entry)
    size_t color_size = lean_array_size(colors_arr);
    float* colors = NULL;
    if (color_size >= count * 4) {
        colors = malloc(count * 4 * sizeof(float));
        if (colors) {
            for (size_t i = 0; i < count * 4; i++) {
                colors[i] = (float)lean_unbox_float(lean_array_get_core(colors_arr, i));
            }
        }
    }

    // Extract transforms array (6 floats per entry)
    size_t transform_size = lean_array_size(transforms_arr);
    float* transforms = NULL;
    if (transform_size >= count * 6) {
        transforms = malloc(count * 6 * sizeof(float));
        if (transforms) {
            for (size_t i = 0; i < count * 6; i++) {
                transforms[i] = (float)lean_unbox_float(lean_array_get_core(transforms_arr, i));
            }
        }
    }

    AfferentResult result = afferent_text_render_batch(
        renderer, font, texts, positions, colors, transforms, count,
        (float)canvas_width, (float)canvas_height
    );

    free(texts);
    free(positions);
    free(colors);
    free(transforms);

    if (result != AFFERENT_OK) {
        return lean_io_result_mk_error(lean_mk_io_user_error(
            lean_mk_string("Failed to render text batch")));
    }

    return lean_io_result_mk_ok(lean_box(0));
}

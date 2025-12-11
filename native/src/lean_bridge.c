#include <lean/lean.h>
#include <string.h>
#include <stdio.h>
#include "afferent.h"

// External class registrations for opaque handles
static lean_external_class* g_window_class = NULL;
static lean_external_class* g_renderer_class = NULL;
static lean_external_class* g_buffer_class = NULL;
static lean_external_class* g_font_class = NULL;

// Weak reference so we don't double-free if Lean GC happens after explicit destroy
static void window_finalizer(void* ptr) {
    // Note: We let explicit destroy handle cleanup to avoid double-free
    // In production, you'd want reference counting
}

static void renderer_finalizer(void* ptr) {
    // Same as above
}

static void buffer_finalizer(void* ptr) {
    // Same as above
}

static void font_finalizer(void* ptr) {
    // Same as above
}

// Module initialization
LEAN_EXPORT lean_obj_res afferent_initialize(uint8_t builtin, lean_obj_arg world) {
    g_window_class = lean_register_external_class(window_finalizer, NULL);
    g_renderer_class = lean_register_external_class(renderer_finalizer, NULL);
    g_buffer_class = lean_register_external_class(buffer_finalizer, NULL);
    g_font_class = lean_register_external_class(font_finalizer, NULL);

    // Initialize text subsystem
    afferent_text_init();

    return lean_io_result_mk_ok(lean_box(0));
}

// Window creation
LEAN_EXPORT lean_obj_res lean_afferent_window_create(
    uint32_t width,
    uint32_t height,
    lean_obj_arg title,
    lean_obj_arg world
) {
    const char* title_str = lean_string_cstr(title);
    AfferentWindowRef window = NULL;
    AfferentResult result = afferent_window_create(width, height, title_str, &window);

    if (result != AFFERENT_OK) {
        return lean_io_result_mk_error(lean_mk_io_user_error(
            lean_mk_string("Failed to create window")));
    }

    lean_object* obj = lean_alloc_external(g_window_class, window);
    return lean_io_result_mk_ok(obj);
}

// Window destroy
LEAN_EXPORT lean_obj_res lean_afferent_window_destroy(lean_obj_arg window_obj, lean_obj_arg world) {
    AfferentWindowRef window = (AfferentWindowRef)lean_get_external_data(window_obj);
    afferent_window_destroy(window);
    return lean_io_result_mk_ok(lean_box(0));
}

// Window should close
LEAN_EXPORT lean_obj_res lean_afferent_window_should_close(lean_obj_arg window_obj, lean_obj_arg world) {
    AfferentWindowRef window = (AfferentWindowRef)lean_get_external_data(window_obj);
    bool should_close = afferent_window_should_close(window);
    return lean_io_result_mk_ok(lean_box(should_close ? 1 : 0));
}

// Window poll events
LEAN_EXPORT lean_obj_res lean_afferent_window_poll_events(lean_obj_arg window_obj, lean_obj_arg world) {
    AfferentWindowRef window = (AfferentWindowRef)lean_get_external_data(window_obj);
    afferent_window_poll_events(window);
    return lean_io_result_mk_ok(lean_box(0));
}

// Window get size - returns (width, height) as UInt32 × UInt32
LEAN_EXPORT lean_obj_res lean_afferent_window_get_size(lean_obj_arg window_obj, lean_obj_arg world) {
    AfferentWindowRef window = (AfferentWindowRef)lean_get_external_data(window_obj);
    uint32_t width = 0, height = 0;
    afferent_window_get_size(window, &width, &height);

    // Return as Prod UInt32 UInt32 with 2 boxed fields
    lean_object* tuple = lean_alloc_ctor(0, 2, 0);
    lean_ctor_set(tuple, 0, lean_box_uint32(width));
    lean_ctor_set(tuple, 1, lean_box_uint32(height));
    return lean_io_result_mk_ok(tuple);
}

// Get keyboard key code (returns 0 if no key pressed)
LEAN_EXPORT lean_obj_res lean_afferent_window_get_key_code(lean_obj_arg window_obj, lean_obj_arg world) {
    AfferentWindowRef window = (AfferentWindowRef)lean_get_external_data(window_obj);
    uint16_t key_code = afferent_window_get_key_code(window);
    return lean_io_result_mk_ok(lean_box(key_code));
}

// Clear keyboard state
LEAN_EXPORT lean_obj_res lean_afferent_window_clear_key(lean_obj_arg window_obj, lean_obj_arg world) {
    AfferentWindowRef window = (AfferentWindowRef)lean_get_external_data(window_obj);
    afferent_window_clear_key(window);
    return lean_io_result_mk_ok(lean_box(0));
}

// Renderer creation
LEAN_EXPORT lean_obj_res lean_afferent_renderer_create(lean_obj_arg window_obj, lean_obj_arg world) {
    AfferentWindowRef window = (AfferentWindowRef)lean_get_external_data(window_obj);
    AfferentRendererRef renderer = NULL;
    AfferentResult result = afferent_renderer_create(window, &renderer);

    if (result != AFFERENT_OK) {
        return lean_io_result_mk_error(lean_mk_io_user_error(
            lean_mk_string("Failed to create renderer")));
    }

    lean_object* obj = lean_alloc_external(g_renderer_class, renderer);
    return lean_io_result_mk_ok(obj);
}

// Renderer destroy
LEAN_EXPORT lean_obj_res lean_afferent_renderer_destroy(lean_obj_arg renderer_obj, lean_obj_arg world) {
    AfferentRendererRef renderer = (AfferentRendererRef)lean_get_external_data(renderer_obj);
    afferent_renderer_destroy(renderer);
    return lean_io_result_mk_ok(lean_box(0));
}

// Begin frame with clear color
LEAN_EXPORT lean_obj_res lean_afferent_renderer_begin_frame(
    lean_obj_arg renderer_obj,
    double r, double g, double b, double a,
    lean_obj_arg world
) {
    AfferentRendererRef renderer = (AfferentRendererRef)lean_get_external_data(renderer_obj);
    AfferentResult result = afferent_renderer_begin_frame(renderer, (float)r, (float)g, (float)b, (float)a);

    if (result != AFFERENT_OK) {
        return lean_io_result_mk_ok(lean_box(0)); // false
    }
    return lean_io_result_mk_ok(lean_box(1)); // true
}

// End frame
LEAN_EXPORT lean_obj_res lean_afferent_renderer_end_frame(lean_obj_arg renderer_obj, lean_obj_arg world) {
    AfferentRendererRef renderer = (AfferentRendererRef)lean_get_external_data(renderer_obj);
    afferent_renderer_end_frame(renderer);
    return lean_io_result_mk_ok(lean_box(0));
}

// Create vertex buffer from Float array
// Each vertex is 6 floats: position[2], color[4]
LEAN_EXPORT lean_obj_res lean_afferent_buffer_create_vertex(
    lean_obj_arg renderer_obj,
    lean_obj_arg vertices_arr,
    lean_obj_arg world
) {
    AfferentRendererRef renderer = (AfferentRendererRef)lean_get_external_data(renderer_obj);

    size_t arr_size = lean_array_size(vertices_arr);
    size_t vertex_count = arr_size / 6;  // 6 floats per vertex

    if (vertex_count == 0) {
        return lean_io_result_mk_error(lean_mk_io_user_error(
            lean_mk_string("Empty vertex array")));
    }

    AfferentVertex* vertices = malloc(vertex_count * sizeof(AfferentVertex));
    if (!vertices) {
        return lean_io_result_mk_error(lean_mk_io_user_error(
            lean_mk_string("Failed to allocate vertex memory")));
    }

    for (size_t i = 0; i < vertex_count; i++) {
        size_t base = i * 6;
        // Position
        vertices[i].position[0] = (float)lean_unbox_float(lean_array_get_core(vertices_arr, base + 0));
        vertices[i].position[1] = (float)lean_unbox_float(lean_array_get_core(vertices_arr, base + 1));
        // Color
        vertices[i].color[0] = (float)lean_unbox_float(lean_array_get_core(vertices_arr, base + 2));
        vertices[i].color[1] = (float)lean_unbox_float(lean_array_get_core(vertices_arr, base + 3));
        vertices[i].color[2] = (float)lean_unbox_float(lean_array_get_core(vertices_arr, base + 4));
        vertices[i].color[3] = (float)lean_unbox_float(lean_array_get_core(vertices_arr, base + 5));
    }

    AfferentBufferRef buffer = NULL;
    AfferentResult result = afferent_buffer_create_vertex(renderer, vertices, (uint32_t)vertex_count, &buffer);
    free(vertices);

    if (result != AFFERENT_OK) {
        return lean_io_result_mk_error(lean_mk_io_user_error(
            lean_mk_string("Failed to create vertex buffer")));
    }

    lean_object* obj = lean_alloc_external(g_buffer_class, buffer);
    return lean_io_result_mk_ok(obj);
}

// Create index buffer from UInt32 array
LEAN_EXPORT lean_obj_res lean_afferent_buffer_create_index(
    lean_obj_arg renderer_obj,
    lean_obj_arg indices_arr,
    lean_obj_arg world
) {
    AfferentRendererRef renderer = (AfferentRendererRef)lean_get_external_data(renderer_obj);

    size_t count = lean_array_size(indices_arr);
    if (count == 0) {
        return lean_io_result_mk_error(lean_mk_io_user_error(
            lean_mk_string("Empty index array")));
    }

    uint32_t* indices = malloc(count * sizeof(uint32_t));
    if (!indices) {
        return lean_io_result_mk_error(lean_mk_io_user_error(
            lean_mk_string("Failed to allocate index memory")));
    }

    for (size_t i = 0; i < count; i++) {
        indices[i] = lean_unbox_uint32(lean_array_get_core(indices_arr, i));
    }

    AfferentBufferRef buffer = NULL;
    AfferentResult result = afferent_buffer_create_index(renderer, indices, (uint32_t)count, &buffer);
    free(indices);

    if (result != AFFERENT_OK) {
        return lean_io_result_mk_error(lean_mk_io_user_error(
            lean_mk_string("Failed to create index buffer")));
    }

    lean_object* obj = lean_alloc_external(g_buffer_class, buffer);
    return lean_io_result_mk_ok(obj);
}

// Buffer destroy
LEAN_EXPORT lean_obj_res lean_afferent_buffer_destroy(lean_obj_arg buffer_obj, lean_obj_arg world) {
    AfferentBufferRef buffer = (AfferentBufferRef)lean_get_external_data(buffer_obj);
    afferent_buffer_destroy(buffer);
    return lean_io_result_mk_ok(lean_box(0));
}

// Draw triangles
LEAN_EXPORT lean_obj_res lean_afferent_renderer_draw_triangles(
    lean_obj_arg renderer_obj,
    lean_obj_arg vertex_buffer_obj,
    lean_obj_arg index_buffer_obj,
    uint32_t index_count,
    lean_obj_arg world
) {
    AfferentRendererRef renderer = (AfferentRendererRef)lean_get_external_data(renderer_obj);
    AfferentBufferRef vertex_buffer = (AfferentBufferRef)lean_get_external_data(vertex_buffer_obj);
    AfferentBufferRef index_buffer = (AfferentBufferRef)lean_get_external_data(index_buffer_obj);

    afferent_renderer_draw_triangles(renderer, vertex_buffer, index_buffer, index_count);
    return lean_io_result_mk_ok(lean_box(0));
}

// Draw instanced rectangles - GPU-accelerated transforms
// instance_data_arr: Array Float with 8 floats per instance
//   (pos.x, pos.y, angle, halfSize, r, g, b, a)
LEAN_EXPORT lean_obj_res lean_afferent_renderer_draw_instanced_rects(
    lean_obj_arg renderer_obj,
    lean_obj_arg instance_data_arr,
    uint32_t instance_count,
    lean_obj_arg world
) {
    AfferentRendererRef renderer = (AfferentRendererRef)lean_get_external_data(renderer_obj);

    size_t arr_size = lean_array_size(instance_data_arr);
    size_t expected_size = (size_t)instance_count * 8;

    if (arr_size < expected_size || instance_count == 0) {
        return lean_io_result_mk_ok(lean_box(0));  // Silent fail on invalid input
    }

    // Convert Lean array to float array
    float* instance_data = malloc(arr_size * sizeof(float));
    if (!instance_data) {
        return lean_io_result_mk_ok(lean_box(0));
    }

    for (size_t i = 0; i < arr_size; i++) {
        instance_data[i] = (float)lean_unbox_float(lean_array_get_core(instance_data_arr, i));
    }

    afferent_renderer_draw_instanced_rects(renderer, instance_data, instance_count);
    free(instance_data);

    return lean_io_result_mk_ok(lean_box(0));
}

// Set scissor rect for clipping
LEAN_EXPORT lean_obj_res lean_afferent_renderer_set_scissor(
    lean_obj_arg renderer_obj,
    uint32_t x,
    uint32_t y,
    uint32_t width,
    uint32_t height,
    lean_obj_arg world
) {
    AfferentRendererRef renderer = (AfferentRendererRef)lean_get_external_data(renderer_obj);
    afferent_renderer_set_scissor(renderer, x, y, width, height);
    return lean_io_result_mk_ok(lean_box(0));
}

// Reset scissor to full viewport
LEAN_EXPORT lean_obj_res lean_afferent_renderer_reset_scissor(
    lean_obj_arg renderer_obj,
    lean_obj_arg world
) {
    AfferentRendererRef renderer = (AfferentRendererRef)lean_get_external_data(renderer_obj);
    afferent_renderer_reset_scissor(renderer);
    return lean_io_result_mk_ok(lean_box(0));
}

// ============== Font/Text FFI ==============

// Load a font from file
LEAN_EXPORT lean_obj_res lean_afferent_font_load(
    lean_obj_arg path_obj,
    uint32_t size,
    lean_obj_arg world
) {
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

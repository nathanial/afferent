#include <lean/lean.h>
#include <string.h>
#include <stdio.h>
#include "afferent.h"

// External class registrations for opaque handles
static lean_external_class* g_window_class = NULL;
static lean_external_class* g_renderer_class = NULL;
static lean_external_class* g_buffer_class = NULL;
static lean_external_class* g_font_class = NULL;
static lean_external_class* g_float_buffer_class = NULL;
static lean_external_class* g_texture_class = NULL;

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

static void float_buffer_finalizer(void* ptr) {
    // Same as above
}

static void texture_finalizer(void* ptr) {
    // Same as above
}

// Module initialization
LEAN_EXPORT lean_obj_res afferent_initialize(uint8_t builtin, lean_obj_arg world) {
    g_window_class = lean_register_external_class(window_finalizer, NULL);
    g_renderer_class = lean_register_external_class(renderer_finalizer, NULL);
    g_buffer_class = lean_register_external_class(buffer_finalizer, NULL);
    g_font_class = lean_register_external_class(font_finalizer, NULL);
    g_float_buffer_class = lean_register_external_class(float_buffer_finalizer, NULL);
    g_texture_class = lean_register_external_class(texture_finalizer, NULL);

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

// Enable/disable MSAA for subsequent frames
LEAN_EXPORT lean_obj_res lean_afferent_renderer_set_msaa_enabled(
    lean_obj_arg renderer_obj,
    uint8_t enabled,
    lean_obj_arg world
) {
    AfferentRendererRef renderer = (AfferentRendererRef)lean_get_external_data(renderer_obj);
    afferent_renderer_set_msaa_enabled(renderer, enabled != 0);
    return lean_io_result_mk_ok(lean_box(0));
}

// Override drawable scale (1.0 disables Retina). Pass 0 to restore native scale.
LEAN_EXPORT lean_obj_res lean_afferent_renderer_set_drawable_scale(
    lean_obj_arg renderer_obj,
    double scale,
    lean_obj_arg world
) {
    AfferentRendererRef renderer = (AfferentRendererRef)lean_get_external_data(renderer_obj);
    afferent_renderer_set_drawable_scale(renderer, (float)scale);
    return lean_io_result_mk_ok(lean_box(0));
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

// Reusable buffer for instanced rendering (avoids per-frame malloc)
static float* g_instance_buffer = NULL;
static size_t g_instance_buffer_capacity = 0;

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

    // Reuse or grow the static buffer
    if (arr_size > g_instance_buffer_capacity) {
        free(g_instance_buffer);
        g_instance_buffer = malloc(arr_size * sizeof(float));
        g_instance_buffer_capacity = g_instance_buffer ? arr_size : 0;
    }

    if (!g_instance_buffer) {
        return lean_io_result_mk_ok(lean_box(0));
    }

    // Convert Lean array to float array (reusing buffer)
    for (size_t i = 0; i < arr_size; i++) {
        g_instance_buffer[i] = (float)lean_unbox_float(lean_array_get_core(instance_data_arr, i));
    }

    afferent_renderer_draw_instanced_rects(renderer, g_instance_buffer, instance_count);
    // Don't free - reuse next frame

    return lean_io_result_mk_ok(lean_box(0));
}

// Draw instanced triangles - GPU-accelerated transforms
LEAN_EXPORT lean_obj_res lean_afferent_renderer_draw_instanced_triangles(
    lean_obj_arg renderer_obj,
    lean_obj_arg instance_data_arr,
    uint32_t instance_count,
    lean_obj_arg world
) {
    AfferentRendererRef renderer = (AfferentRendererRef)lean_get_external_data(renderer_obj);

    size_t arr_size = lean_array_size(instance_data_arr);
    size_t expected_size = (size_t)instance_count * 8;

    if (arr_size < expected_size || instance_count == 0) {
        return lean_io_result_mk_ok(lean_box(0));
    }

    if (arr_size > g_instance_buffer_capacity) {
        free(g_instance_buffer);
        g_instance_buffer = malloc(arr_size * sizeof(float));
        g_instance_buffer_capacity = g_instance_buffer ? arr_size : 0;
    }

    if (!g_instance_buffer) {
        return lean_io_result_mk_ok(lean_box(0));
    }

    for (size_t i = 0; i < arr_size; i++) {
        g_instance_buffer[i] = (float)lean_unbox_float(lean_array_get_core(instance_data_arr, i));
    }

    afferent_renderer_draw_instanced_triangles(renderer, g_instance_buffer, instance_count);

    return lean_io_result_mk_ok(lean_box(0));
}

// Draw instanced circles - smooth circles via fragment shader
LEAN_EXPORT lean_obj_res lean_afferent_renderer_draw_instanced_circles(
    lean_obj_arg renderer_obj,
    lean_obj_arg instance_data_arr,
    uint32_t instance_count,
    lean_obj_arg world
) {
    AfferentRendererRef renderer = (AfferentRendererRef)lean_get_external_data(renderer_obj);

    size_t arr_size = lean_array_size(instance_data_arr);
    size_t expected_size = (size_t)instance_count * 8;

    if (arr_size < expected_size || instance_count == 0) {
        return lean_io_result_mk_ok(lean_box(0));
    }

    if (arr_size > g_instance_buffer_capacity) {
        free(g_instance_buffer);
        g_instance_buffer = malloc(arr_size * sizeof(float));
        g_instance_buffer_capacity = g_instance_buffer ? arr_size : 0;
    }

    if (!g_instance_buffer) {
        return lean_io_result_mk_ok(lean_box(0));
    }

    for (size_t i = 0; i < arr_size; i++) {
        g_instance_buffer[i] = (float)lean_unbox_float(lean_array_get_core(instance_data_arr, i));
    }

    afferent_renderer_draw_instanced_circles(renderer, g_instance_buffer, instance_count);

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

// ============== FloatBuffer FFI ==============
// High-performance mutable float buffer for instance data
// Avoids Lean's copy-on-write array semantics

LEAN_EXPORT lean_obj_res lean_afferent_float_buffer_create(size_t capacity, lean_obj_arg world) {
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

// ============================================================================
// FUSED PHYSICS + PACKING (FloatArray particle state -> FloatBuffer instances)
// ============================================================================

// Update bouncing physics in-place and write sprite instance data to FloatBuffer.
// particle_data_arr: FloatArray [x, y, vx, vy, hue] per particle (5 doubles).
// sprite_buffer: FloatBuffer [x, y, rotation(=0), halfSize, alpha(=1)] per particle (5 floats).
LEAN_EXPORT lean_obj_res lean_afferent_particles_update_bouncing_and_write_sprites(
    lean_obj_arg particle_data_arr,
    uint32_t count,
    double dt,
    double halfSize,
    double screenWidth,
    double screenHeight,
    lean_obj_arg sprite_buffer_obj,
    lean_obj_arg world
) {
    if (count == 0) {
        return lean_io_result_mk_ok(particle_data_arr);
    }

    // Ensure exclusive so in-place mutation is safe.
    if (!lean_is_exclusive(particle_data_arr)) {
        lean_object* copy = lean_copy_float_array(particle_data_arr);
        lean_dec(particle_data_arr);
        particle_data_arr = copy;
    }

    size_t arr_size = (size_t)lean_unbox(lean_float_array_size(particle_data_arr));
    size_t expected_size = (size_t)count * 5;
    if (arr_size < expected_size) {
        return lean_io_result_mk_ok(particle_data_arr);
    }

    AfferentFloatBufferRef sprite_buffer = (AfferentFloatBufferRef)lean_get_external_data(sprite_buffer_obj);
    if (!sprite_buffer || afferent_float_buffer_capacity(sprite_buffer) < expected_size) {
        return lean_io_result_mk_ok(particle_data_arr);
    }

    double* p = lean_float_array_cptr(particle_data_arr);
    float* out = (float*)afferent_float_buffer_data(sprite_buffer);
    float h = (float)halfSize;
    float a = 1.0f;
    float rot = 0.0f;

    double w = screenWidth;
    double ht = screenHeight;
    double r = halfSize;

    for (uint32_t i = 0; i < count; i++) {
        size_t base = (size_t)i * 5;
        double x = p[base + 0];
        double y = p[base + 1];
        double vx = p[base + 2];
        double vy = p[base + 3];

        x += vx * dt;
        y += vy * dt;

        if (x < r) { x = r; vx = -vx; }
        else if (x > w - r) { x = w - r; vx = -vx; }
        if (y < r) { y = r; vy = -vy; }
        else if (y > ht - r) { y = ht - r; vy = -vy; }

        p[base + 0] = x;
        p[base + 1] = y;
        p[base + 2] = vx;
        p[base + 3] = vy;

        out[base + 0] = (float)x;
        out[base + 1] = (float)y;
        out[base + 2] = rot;
        out[base + 3] = h;
        out[base + 4] = a;
    }

    return lean_io_result_mk_ok(particle_data_arr);
}

// Update bouncing physics in-place and write dynamic circle data to FloatBuffer.
// circle_buffer: FloatBuffer [x, y, hueBase, radius] per particle (4 floats).
LEAN_EXPORT lean_obj_res lean_afferent_particles_update_bouncing_and_write_circles(
    lean_obj_arg particle_data_arr,
    uint32_t count,
    double dt,
    double radius,
    double screenWidth,
    double screenHeight,
    lean_obj_arg circle_buffer_obj,
    lean_obj_arg world
) {
    if (count == 0) {
        return lean_io_result_mk_ok(particle_data_arr);
    }

    if (!lean_is_exclusive(particle_data_arr)) {
        lean_object* copy = lean_copy_float_array(particle_data_arr);
        lean_dec(particle_data_arr);
        particle_data_arr = copy;
    }

    size_t arr_size = (size_t)lean_unbox(lean_float_array_size(particle_data_arr));
    size_t expected_size = (size_t)count * 5;
    if (arr_size < expected_size) {
        return lean_io_result_mk_ok(particle_data_arr);
    }

    AfferentFloatBufferRef circle_buffer = (AfferentFloatBufferRef)lean_get_external_data(circle_buffer_obj);
    size_t out_needed = (size_t)count * 4;
    if (!circle_buffer || afferent_float_buffer_capacity(circle_buffer) < out_needed) {
        return lean_io_result_mk_ok(particle_data_arr);
    }

    double* p = lean_float_array_cptr(particle_data_arr);
    float* out = (float*)afferent_float_buffer_data(circle_buffer);
    float rad = (float)radius;

    double w = screenWidth;
    double ht = screenHeight;
    double r = radius;

    for (uint32_t i = 0; i < count; i++) {
        size_t base = (size_t)i * 5;
        double x = p[base + 0];
        double y = p[base + 1];
        double vx = p[base + 2];
        double vy = p[base + 3];
        double hue = p[base + 4];

        x += vx * dt;
        y += vy * dt;

        if (x < r) { x = r; vx = -vx; }
        else if (x > w - r) { x = w - r; vx = -vx; }
        if (y < r) { y = r; vy = -vy; }
        else if (y > ht - r) { y = ht - r; vy = -vy; }

        p[base + 0] = x;
        p[base + 1] = y;
        p[base + 2] = vx;
        p[base + 3] = vy;

        size_t o = (size_t)i * 4;
        out[o + 0] = (float)x;
        out[o + 1] = (float)y;
        out[o + 2] = (float)hue;
        out[o + 3] = rad;
    }

    return lean_io_result_mk_ok(particle_data_arr);
}

// Draw instanced shapes directly from FloatBuffer (zero-copy path)
LEAN_EXPORT lean_obj_res lean_afferent_renderer_draw_instanced_rects_buffer(
    lean_obj_arg renderer_obj,
    lean_obj_arg buffer_obj,
    uint32_t instance_count,
    lean_obj_arg world
) {
    AfferentRendererRef renderer = (AfferentRendererRef)lean_get_external_data(renderer_obj);
    AfferentFloatBufferRef buffer = (AfferentFloatBufferRef)lean_get_external_data(buffer_obj);

    // Direct pointer pass - no conversion needed!
    afferent_renderer_draw_instanced_rects(
        renderer,
        afferent_float_buffer_data(buffer),
        instance_count
    );
    return lean_io_result_mk_ok(lean_box(0));
}

LEAN_EXPORT lean_obj_res lean_afferent_renderer_draw_instanced_triangles_buffer(
    lean_obj_arg renderer_obj,
    lean_obj_arg buffer_obj,
    uint32_t instance_count,
    lean_obj_arg world
) {
    AfferentRendererRef renderer = (AfferentRendererRef)lean_get_external_data(renderer_obj);
    AfferentFloatBufferRef buffer = (AfferentFloatBufferRef)lean_get_external_data(buffer_obj);

    afferent_renderer_draw_instanced_triangles(
        renderer,
        afferent_float_buffer_data(buffer),
        instance_count
    );
    return lean_io_result_mk_ok(lean_box(0));
}

LEAN_EXPORT lean_obj_res lean_afferent_renderer_draw_instanced_circles_buffer(
    lean_obj_arg renderer_obj,
    lean_obj_arg buffer_obj,
    uint32_t instance_count,
    lean_obj_arg world
) {
    AfferentRendererRef renderer = (AfferentRendererRef)lean_get_external_data(renderer_obj);
    AfferentFloatBufferRef buffer = (AfferentFloatBufferRef)lean_get_external_data(buffer_obj);

    afferent_renderer_draw_instanced_circles(
        renderer,
        afferent_float_buffer_data(buffer),
        instance_count
    );
    return lean_io_result_mk_ok(lean_box(0));
}

// ============================================================================
// ANIMATED RENDERING FFI - GPU-side animation for maximum performance
// Static data uploaded once, only time uniform sent per frame
// ============================================================================

// Upload static instance data for animated rects
// data format: [pixelX, pixelY, hueBase, halfSizePixels, phaseOffset, spinSpeed] × count
LEAN_EXPORT lean_obj_res lean_afferent_renderer_upload_animated_rects(
    lean_obj_arg renderer_obj,
    lean_obj_arg data_arr,
    uint32_t count,
    lean_obj_arg world
) {
    AfferentRendererRef renderer = (AfferentRendererRef)lean_get_external_data(renderer_obj);

    // Convert Lean Float array to C float array (one-time upload)
    size_t arr_size = lean_array_size(data_arr);
    float* data = malloc(arr_size * sizeof(float));
    for (size_t i = 0; i < arr_size; i++) {
        data[i] = (float)lean_unbox_float(lean_array_get_core(data_arr, i));
    }

    afferent_renderer_upload_animated_rects(renderer, data, count);

    free(data);
    return lean_io_result_mk_ok(lean_box(0));
}

// Upload static instance data for animated triangles
LEAN_EXPORT lean_obj_res lean_afferent_renderer_upload_animated_triangles(
    lean_obj_arg renderer_obj,
    lean_obj_arg data_arr,
    uint32_t count,
    lean_obj_arg world
) {
    AfferentRendererRef renderer = (AfferentRendererRef)lean_get_external_data(renderer_obj);

    size_t arr_size = lean_array_size(data_arr);
    float* data = malloc(arr_size * sizeof(float));
    for (size_t i = 0; i < arr_size; i++) {
        data[i] = (float)lean_unbox_float(lean_array_get_core(data_arr, i));
    }

    afferent_renderer_upload_animated_triangles(renderer, data, count);

    free(data);
    return lean_io_result_mk_ok(lean_box(0));
}

// Upload static instance data for animated circles
LEAN_EXPORT lean_obj_res lean_afferent_renderer_upload_animated_circles(
    lean_obj_arg renderer_obj,
    lean_obj_arg data_arr,
    uint32_t count,
    lean_obj_arg world
) {
    AfferentRendererRef renderer = (AfferentRendererRef)lean_get_external_data(renderer_obj);

    size_t arr_size = lean_array_size(data_arr);
    float* data = malloc(arr_size * sizeof(float));
    for (size_t i = 0; i < arr_size; i++) {
        data[i] = (float)lean_unbox_float(lean_array_get_core(data_arr, i));
    }

    afferent_renderer_upload_animated_circles(renderer, data, count);

    free(data);
    return lean_io_result_mk_ok(lean_box(0));
}

// Draw animated rects (called every frame - only sends time!)
LEAN_EXPORT lean_obj_res lean_afferent_renderer_draw_animated_rects(
    lean_obj_arg renderer_obj,
    double time,
    lean_obj_arg world
) {
    AfferentRendererRef renderer = (AfferentRendererRef)lean_get_external_data(renderer_obj);
    afferent_renderer_draw_animated_rects(renderer, (float)time);
    return lean_io_result_mk_ok(lean_box(0));
}

// Draw animated triangles (called every frame)
LEAN_EXPORT lean_obj_res lean_afferent_renderer_draw_animated_triangles(
    lean_obj_arg renderer_obj,
    double time,
    lean_obj_arg world
) {
    AfferentRendererRef renderer = (AfferentRendererRef)lean_get_external_data(renderer_obj);
    afferent_renderer_draw_animated_triangles(renderer, (float)time);
    return lean_io_result_mk_ok(lean_box(0));
}

// Draw animated circles (called every frame)
LEAN_EXPORT lean_obj_res lean_afferent_renderer_draw_animated_circles(
    lean_obj_arg renderer_obj,
    double time,
    lean_obj_arg world
) {
    AfferentRendererRef renderer = (AfferentRendererRef)lean_get_external_data(renderer_obj);
    afferent_renderer_draw_animated_circles(renderer, (float)time);
    return lean_io_result_mk_ok(lean_box(0));
}

// ============================================================================
// ORBITAL RENDERING FFI - Particles orbiting around a center point
// ============================================================================

// Upload static orbital instance data
// data format: [phase, baseRadius, orbitSpeed, phaseX3, phase2, hueBase, halfSizePixels, padding] × count
LEAN_EXPORT lean_obj_res lean_afferent_renderer_upload_orbital_particles(
    lean_obj_arg renderer_obj,
    lean_obj_arg data_arr,
    uint32_t count,
    double centerX,
    double centerY,
    lean_obj_arg world
) {
    AfferentRendererRef renderer = (AfferentRendererRef)lean_get_external_data(renderer_obj);

    // Convert Lean Float array to C float array (one-time upload)
    size_t arr_size = lean_array_size(data_arr);
    float* data = malloc(arr_size * sizeof(float));
    for (size_t i = 0; i < arr_size; i++) {
        data[i] = (float)lean_unbox_float(lean_array_get_core(data_arr, i));
    }

    afferent_renderer_upload_orbital_particles(renderer, data, count, (float)centerX, (float)centerY);

    free(data);
    return lean_io_result_mk_ok(lean_box(0));
}

// Draw orbital particles (called every frame - only sends time!)
LEAN_EXPORT lean_obj_res lean_afferent_renderer_draw_orbital_particles(
    lean_obj_arg renderer_obj,
    double time,
    lean_obj_arg world
) {
    AfferentRendererRef renderer = (AfferentRendererRef)lean_get_external_data(renderer_obj);
    afferent_renderer_draw_orbital_particles(renderer, (float)time);
    return lean_io_result_mk_ok(lean_box(0));
}

// Draw dynamic circles (positions from CPU, color + NDC from GPU)
// data: [pixelX, pixelY, hueBase, radiusPixels] × count (4 floats per circle)
LEAN_EXPORT lean_obj_res lean_afferent_renderer_draw_dynamic_circles(
    lean_obj_arg renderer_obj,
    lean_obj_arg data_arr,
    uint32_t count,
    double time,
    double canvasWidth,
    double canvasHeight,
    lean_obj_arg world
) {
    AfferentRendererRef renderer = (AfferentRendererRef)lean_get_external_data(renderer_obj);

    // Extract float array data - 4 floats per circle
    size_t arr_size = lean_array_size(data_arr);
    float* data = malloc(arr_size * sizeof(float));
    for (size_t i = 0; i < arr_size; i++) {
        lean_object* elem = lean_array_get_core(data_arr, i);
        data[i] = (float)lean_unbox_float(elem);
    }

    afferent_renderer_draw_dynamic_circles(renderer, data, count, (float)time, (float)canvasWidth, (float)canvasHeight);

    free(data);
    return lean_io_result_mk_ok(lean_box(0));
}

// Draw dynamic circles directly from FloatBuffer (no conversion)
LEAN_EXPORT lean_obj_res lean_afferent_renderer_draw_dynamic_circles_buffer(
    lean_obj_arg renderer_obj,
    lean_obj_arg buffer_obj,
    uint32_t count,
    double time,
    double canvasWidth,
    double canvasHeight,
    lean_obj_arg world
) {
    AfferentRendererRef renderer = (AfferentRendererRef)lean_get_external_data(renderer_obj);
    AfferentFloatBufferRef buffer = (AfferentFloatBufferRef)lean_get_external_data(buffer_obj);
    afferent_renderer_draw_dynamic_circles(
        renderer,
        afferent_float_buffer_data(buffer),
        count,
        (float)time,
        (float)canvasWidth,
        (float)canvasHeight
    );
    return lean_io_result_mk_ok(lean_box(0));
}

// Draw dynamic rects (positions/rotation from CPU, color + NDC from GPU)
// data: [pixelX, pixelY, hueBase, halfSizePixels, rotation] × count (5 floats per rect)
LEAN_EXPORT lean_obj_res lean_afferent_renderer_draw_dynamic_rects(
    lean_obj_arg renderer_obj,
    lean_obj_arg data_arr,
    uint32_t count,
    double time,
    double canvasWidth,
    double canvasHeight,
    lean_obj_arg world
) {
    AfferentRendererRef renderer = (AfferentRendererRef)lean_get_external_data(renderer_obj);

    // Extract float array data - 5 floats per rect
    size_t arr_size = lean_array_size(data_arr);
    float* data = malloc(arr_size * sizeof(float));
    for (size_t i = 0; i < arr_size; i++) {
        lean_object* elem = lean_array_get_core(data_arr, i);
        data[i] = (float)lean_unbox_float(elem);
    }

    afferent_renderer_draw_dynamic_rects(renderer, data, count, (float)time, (float)canvasWidth, (float)canvasHeight);

    free(data);
    return lean_io_result_mk_ok(lean_box(0));
}

// Draw dynamic triangles (positions/rotation from CPU, color + NDC from GPU)
// data: [pixelX, pixelY, hueBase, halfSizePixels, rotation] × count (5 floats per triangle)
LEAN_EXPORT lean_obj_res lean_afferent_renderer_draw_dynamic_triangles(
    lean_obj_arg renderer_obj,
    lean_obj_arg data_arr,
    uint32_t count,
    double time,
    double canvasWidth,
    double canvasHeight,
    lean_obj_arg world
) {
    AfferentRendererRef renderer = (AfferentRendererRef)lean_get_external_data(renderer_obj);

    // Extract float array data - 5 floats per triangle
    size_t arr_size = lean_array_size(data_arr);
    float* data = malloc(arr_size * sizeof(float));
    for (size_t i = 0; i < arr_size; i++) {
        lean_object* elem = lean_array_get_core(data_arr, i);
        data[i] = (float)lean_unbox_float(elem);
    }

    afferent_renderer_draw_dynamic_triangles(renderer, data, count, (float)time, (float)canvasWidth, (float)canvasHeight);

    free(data);
    return lean_io_result_mk_ok(lean_box(0));
}

// ============================================================================
// Texture/Sprite Rendering FFI
// ============================================================================

// Load texture from file
LEAN_EXPORT lean_obj_res lean_afferent_texture_load(
    lean_obj_arg path_obj,
    lean_obj_arg world
) {
    const char* path = lean_string_cstr(path_obj);
    AfferentTextureRef texture = NULL;
    AfferentResult result = afferent_texture_load(path, &texture);

    if (result != AFFERENT_OK) {
        return lean_io_result_mk_error(lean_mk_io_user_error(
            lean_mk_string("Failed to load texture")));
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

// Draw sprites with texture
// data: [pixelX, pixelY, rotation, halfSizePixels, alpha] × count (5 floats per sprite)
LEAN_EXPORT lean_obj_res lean_afferent_renderer_draw_sprites(
    lean_obj_arg renderer_obj,
    lean_obj_arg texture_obj,
    lean_obj_arg data_arr,
    uint32_t count,
    double canvasWidth,
    double canvasHeight,
    lean_obj_arg world
) {
    AfferentRendererRef renderer = (AfferentRendererRef)lean_get_external_data(renderer_obj);
    AfferentTextureRef texture = (AfferentTextureRef)lean_get_external_data(texture_obj);

    // Extract float array data - 5 floats per sprite
    size_t arr_size = lean_array_size(data_arr);
    float* data = malloc(arr_size * sizeof(float));
    for (size_t i = 0; i < arr_size; i++) {
        lean_object* elem = lean_array_get_core(data_arr, i);
        data[i] = (float)lean_unbox_float(elem);
    }

    afferent_renderer_draw_sprites(renderer, texture, data, count, (float)canvasWidth, (float)canvasHeight);

    free(data);
    return lean_io_result_mk_ok(lean_box(0));
}

// ============================================================================
// High-performance sprite system (FloatBuffer-based, C-side physics)
// ============================================================================

// Initialize sprites in FloatBuffer with random positions/velocities
LEAN_EXPORT lean_obj_res lean_afferent_float_buffer_init_sprites(
    lean_obj_arg buffer_obj,
    uint32_t count,
    double screenWidth,
    double screenHeight,
    uint32_t seed,
    lean_obj_arg world
) {
    AfferentFloatBufferRef buffer = (AfferentFloatBufferRef)lean_get_external_data(buffer_obj);
    afferent_float_buffer_init_sprites(buffer, count, (float)screenWidth, (float)screenHeight, seed);
    return lean_io_result_mk_ok(lean_box(0));
}

// Update sprite physics (bouncing) - runs entirely in C
LEAN_EXPORT lean_obj_res lean_afferent_float_buffer_update_sprites(
    lean_obj_arg buffer_obj,
    uint32_t count,
    double dt,
    double halfSize,
    double screenWidth,
    double screenHeight,
    lean_obj_arg world
) {
    AfferentFloatBufferRef buffer = (AfferentFloatBufferRef)lean_get_external_data(buffer_obj);
    afferent_float_buffer_update_sprites(buffer, count, (float)dt, (float)halfSize, (float)screenWidth, (float)screenHeight);
    return lean_io_result_mk_ok(lean_box(0));
}

// Draw sprites from FloatBuffer (zero-copy path)
LEAN_EXPORT lean_obj_res lean_afferent_renderer_draw_sprites_buffer(
    lean_obj_arg renderer_obj,
    lean_obj_arg texture_obj,
    lean_obj_arg buffer_obj,
    uint32_t count,
    double halfSize,
    double canvasWidth,
    double canvasHeight,
    lean_obj_arg world
) {
    AfferentRendererRef renderer = (AfferentRendererRef)lean_get_external_data(renderer_obj);
    AfferentTextureRef texture = (AfferentTextureRef)lean_get_external_data(texture_obj);
    AfferentFloatBufferRef buffer = (AfferentFloatBufferRef)lean_get_external_data(buffer_obj);

    afferent_renderer_draw_sprites_buffer(
        renderer, texture,
        afferent_float_buffer_data(buffer),
        count, (float)halfSize, (float)canvasWidth, (float)canvasHeight
    );
    return lean_io_result_mk_ok(lean_box(0));
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

    afferent_renderer_draw_sprites_instance_buffer(
        renderer, texture,
        afferent_float_buffer_data(buffer),
        count, (float)canvasWidth, (float)canvasHeight
    );
    return lean_io_result_mk_ok(lean_box(0));
}

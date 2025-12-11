#include <lean/lean.h>
#include <string.h>
#include "afferent.h"

// External class registrations for opaque handles
static lean_external_class* g_window_class = NULL;
static lean_external_class* g_renderer_class = NULL;
static lean_external_class* g_buffer_class = NULL;

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

// Module initialization
LEAN_EXPORT lean_obj_res afferent_initialize(uint8_t builtin, lean_obj_arg world) {
    g_window_class = lean_register_external_class(window_finalizer, NULL);
    g_renderer_class = lean_register_external_class(renderer_finalizer, NULL);
    g_buffer_class = lean_register_external_class(buffer_finalizer, NULL);
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

#include "lean_bridge_internal.h"

// =============================================================================
// Batched shape drawing
// =============================================================================
LEAN_EXPORT lean_obj_res lean_afferent_renderer_draw_batch(
    lean_obj_arg renderer_obj,
    uint32_t kind,
    lean_obj_arg instance_data_arr,
    uint32_t instance_count,
    double param0,
    double param1,
    double canvas_width,
    double canvas_height,
    lean_obj_arg world
) {
    AfferentRendererRef renderer = (AfferentRendererRef)lean_get_external_data(renderer_obj);

    size_t arr_size = lean_array_size(instance_data_arr);
    size_t expected_size = (size_t)instance_count * 9;

    if (arr_size < expected_size || instance_count == 0) {
        return lean_io_result_mk_ok(lean_box(0));
    }

    float* data = malloc(arr_size * sizeof(float));
    if (!data) {
        return lean_io_result_mk_ok(lean_box(0));
    }

    for (size_t i = 0; i < arr_size; i++) {
        data[i] = (float)lean_unbox_float(lean_array_get_core(instance_data_arr, i));
    }

    afferent_renderer_draw_batch(
        renderer,
        kind,
        data,
        instance_count,
        (float)param0,
        (float)param1,
        (float)canvas_width,
        (float)canvas_height
    );

    free(data);
    return lean_io_result_mk_ok(lean_box(0));
}

// =============================================================================
// Batched line drawing
// =============================================================================
LEAN_EXPORT lean_obj_res lean_afferent_renderer_draw_line_batch(
    lean_obj_arg renderer_obj,
    lean_obj_arg instance_data_arr,
    uint32_t instance_count,
    double line_width,
    double canvas_width,
    double canvas_height,
    lean_obj_arg world
) {
    AfferentRendererRef renderer = (AfferentRendererRef)lean_get_external_data(renderer_obj);

    size_t arr_size = lean_array_size(instance_data_arr);
    size_t expected_size = (size_t)instance_count * 9;  // 9 floats per line: x1, y1, x2, y2, r, g, b, a, padding

    if (arr_size < expected_size || instance_count == 0) {
        return lean_io_result_mk_ok(lean_box(0));
    }

    float* data = malloc(arr_size * sizeof(float));
    if (!data) {
        return lean_io_result_mk_ok(lean_box(0));
    }

    for (size_t i = 0; i < arr_size; i++) {
        data[i] = (float)lean_unbox_float(lean_array_get_core(instance_data_arr, i));
    }

    afferent_renderer_draw_line_batch(
        renderer,
        data,
        instance_count,
        (float)line_width,
        (float)canvas_width,
        (float)canvas_height
    );

    free(data);
    return lean_io_result_mk_ok(lean_box(0));
}

// High-performance line batch drawing from FloatBuffer (avoids copy)
LEAN_EXPORT lean_obj_res lean_afferent_renderer_draw_line_batch_buffer(
    lean_obj_arg renderer_obj,
    lean_obj_arg buffer_obj,
    uint32_t instance_count,
    double line_width,
    double canvas_width,
    double canvas_height,
    lean_obj_arg world
) {
    AfferentRendererRef renderer = (AfferentRendererRef)lean_get_external_data(renderer_obj);
    AfferentFloatBufferRef buffer = (AfferentFloatBufferRef)lean_get_external_data(buffer_obj);

    if (instance_count == 0) {
        return lean_io_result_mk_ok(lean_box(0));
    }

    // Direct access to buffer data - no copy needed
    const float* data = afferent_float_buffer_data(buffer);

    afferent_renderer_draw_line_batch(
        renderer,
        data,
        instance_count,
        (float)line_width,
        (float)canvas_width,
        (float)canvas_height
    );

    return lean_io_result_mk_ok(lean_box(0));
}

// High-performance batch drawing from FloatBuffer (avoids copy)
// kind: 0=rect, 1=circle, 2=strokeRect
LEAN_EXPORT lean_obj_res lean_afferent_renderer_draw_batch_buffer(
    lean_obj_arg renderer_obj,
    uint32_t kind,
    lean_obj_arg buffer_obj,
    uint32_t instance_count,
    double param0,
    double param1,
    double canvas_width,
    double canvas_height,
    lean_obj_arg world
) {
    AfferentRendererRef renderer = (AfferentRendererRef)lean_get_external_data(renderer_obj);
    AfferentFloatBufferRef buffer = (AfferentFloatBufferRef)lean_get_external_data(buffer_obj);

    if (instance_count == 0) {
        return lean_io_result_mk_ok(lean_box(0));
    }

    // Direct access to buffer data - no copy needed
    const float* data = afferent_float_buffer_data(buffer);

    afferent_renderer_draw_batch(
        renderer,
        kind,
        data,
        instance_count,
        (float)param0,
        (float)param1,
        (float)canvas_width,
        (float)canvas_height
    );

    return lean_io_result_mk_ok(lean_box(0));
}

// =============================================================================
// Cached Mesh (Instanced Polygon Rendering)
// =============================================================================

// Create cached mesh from tessellated polygon data
// vertices_arr: Array Float (flat array of [x, y, x, y, ...])
// indices_arr: Array UInt32 (triangle indices)
LEAN_EXPORT lean_obj_res lean_afferent_mesh_cache_create(
    lean_obj_arg renderer_obj,
    lean_obj_arg vertices_arr,
    lean_obj_arg indices_arr,
    double center_x,
    double center_y,
    lean_obj_arg world
) {
    afferent_ensure_initialized();
    AfferentRendererRef renderer = (AfferentRendererRef)lean_get_external_data(renderer_obj);

    size_t vertex_arr_size = lean_array_size(vertices_arr);
    size_t index_arr_size = lean_array_size(indices_arr);

    if (vertex_arr_size < 6 || index_arr_size < 3) {  // Minimum triangle
        return lean_io_result_mk_error(lean_mk_io_user_error(
            lean_mk_string("Mesh too small: need at least 3 vertices and 3 indices")));
    }

    uint32_t vertex_count = (uint32_t)(vertex_arr_size / 2);  // 2 floats per vertex
    uint32_t index_count = (uint32_t)index_arr_size;

    // Copy vertices
    float* vertices = malloc(vertex_arr_size * sizeof(float));
    if (!vertices) {
        return lean_io_result_mk_error(lean_mk_io_user_error(
            lean_mk_string("Failed to allocate vertex memory")));
    }
    for (size_t i = 0; i < vertex_arr_size; i++) {
        vertices[i] = (float)lean_unbox_float(lean_array_get_core(vertices_arr, i));
    }

    // Copy indices
    uint32_t* indices = malloc(index_arr_size * sizeof(uint32_t));
    if (!indices) {
        free(vertices);
        return lean_io_result_mk_error(lean_mk_io_user_error(
            lean_mk_string("Failed to allocate index memory")));
    }
    for (size_t i = 0; i < index_arr_size; i++) {
        indices[i] = lean_unbox_uint32(lean_array_get_core(indices_arr, i));
    }

    AfferentCachedMeshRef mesh = afferent_mesh_cache_create(
        renderer, vertices, vertex_count, indices, index_count,
        (float)center_x, (float)center_y
    );

    free(vertices);
    free(indices);

    if (!mesh) {
        return lean_io_result_mk_error(lean_mk_io_user_error(
            lean_mk_string("Failed to create cached mesh")));
    }

    lean_object* obj = lean_alloc_external(g_cached_mesh_class, mesh);
    return lean_io_result_mk_ok(obj);
}

// Destroy cached mesh
LEAN_EXPORT lean_obj_res lean_afferent_mesh_cache_destroy(
    lean_obj_arg mesh_obj,
    lean_obj_arg world
) {
    AfferentCachedMeshRef mesh = (AfferentCachedMeshRef)lean_get_external_data(mesh_obj);
    afferent_mesh_cache_destroy(mesh);
    return lean_io_result_mk_ok(lean_box(0));
}

// Draw instanced mesh from FloatBuffer
// instance_data: 8 floats per instance [x, y, rotation, scale, r, g, b, a]
LEAN_EXPORT lean_obj_res lean_afferent_mesh_draw_instanced_buffer(
    lean_obj_arg renderer_obj,
    lean_obj_arg mesh_obj,
    lean_obj_arg buffer_obj,
    uint32_t instance_count,
    double canvas_width,
    double canvas_height,
    lean_obj_arg world
) {
    AfferentRendererRef renderer = (AfferentRendererRef)lean_get_external_data(renderer_obj);
    AfferentCachedMeshRef mesh = (AfferentCachedMeshRef)lean_get_external_data(mesh_obj);
    AfferentFloatBufferRef buffer = (AfferentFloatBufferRef)lean_get_external_data(buffer_obj);

    if (instance_count == 0) {
        return lean_io_result_mk_ok(lean_box(0));
    }

    const float* data = afferent_float_buffer_data(buffer);

    afferent_mesh_draw_instanced(
        renderer,
        mesh,
        data,
        instance_count,
        (float)canvas_width,
        (float)canvas_height
    );

    return lean_io_result_mk_ok(lean_box(0));
}

// =============================================================================
// Screen-coords Triangle Drawing (GPU-side NDC conversion)
// =============================================================================

// Draw tessellated triangles with screen-space coordinates
// vertex_data: [x, y, r, g, b, a] per vertex (6 floats)
// indices: triangle indices
LEAN_EXPORT lean_obj_res lean_afferent_renderer_draw_triangles_screen_coords(
    lean_obj_arg renderer_obj,
    lean_obj_arg vertex_data_arr,
    lean_obj_arg indices_arr,
    uint32_t vertex_count,
    double canvas_width,
    double canvas_height,
    lean_obj_arg world
) {
    AfferentRendererRef renderer = (AfferentRendererRef)lean_get_external_data(renderer_obj);

    size_t vertex_arr_size = lean_array_size(vertex_data_arr);
    size_t index_arr_size = lean_array_size(indices_arr);
    size_t expected_vertex_size = (size_t)vertex_count * 6;  // 6 floats per vertex

    if (vertex_arr_size < expected_vertex_size || index_arr_size == 0 || vertex_count == 0) {
        return lean_io_result_mk_ok(lean_box(0));
    }

    // Copy vertex data
    float* vertices = malloc(vertex_arr_size * sizeof(float));
    if (!vertices) {
        return lean_io_result_mk_ok(lean_box(0));
    }

    for (size_t i = 0; i < vertex_arr_size; i++) {
        vertices[i] = (float)lean_unbox_float(lean_array_get_core(vertex_data_arr, i));
    }

    // Copy index data
    uint32_t* indices = malloc(index_arr_size * sizeof(uint32_t));
    if (!indices) {
        free(vertices);
        return lean_io_result_mk_ok(lean_box(0));
    }

    for (size_t i = 0; i < index_arr_size; i++) {
        indices[i] = lean_unbox_uint32(lean_array_get_core(indices_arr, i));
    }

    afferent_renderer_draw_triangles_screen_coords(
        renderer,
        vertices,
        indices,
        vertex_count,
        (uint32_t)index_arr_size,
        (float)canvas_width,
        (float)canvas_height
    );

    free(indices);
    free(vertices);
    return lean_io_result_mk_ok(lean_box(0));
}

// =============================================================================
// Instanced Arc Stroke Rendering
// =============================================================================

// Draw instanced arcs from Lean Array Float
// instance_data: 10 floats per instance [centerX, centerY, startAngle, sweepAngle,
//                                        radius, strokeWidth, r, g, b, a]
LEAN_EXPORT lean_obj_res lean_afferent_arc_draw_instanced(
    lean_obj_arg renderer_obj,
    lean_obj_arg instance_data_arr,
    uint32_t instance_count,
    uint32_t segments,
    double canvas_width,
    double canvas_height,
    lean_obj_arg world
) {
    AfferentRendererRef renderer = (AfferentRendererRef)lean_get_external_data(renderer_obj);

    size_t arr_size = lean_array_size(instance_data_arr);
    size_t expected_size = (size_t)instance_count * 10;  // 10 floats per instance

    if (arr_size < expected_size || instance_count == 0) {
        return lean_io_result_mk_ok(lean_box(0));
    }

    float* data = malloc(arr_size * sizeof(float));
    if (!data) {
        return lean_io_result_mk_ok(lean_box(0));
    }

    for (size_t i = 0; i < arr_size; i++) {
        data[i] = (float)lean_unbox_float(lean_array_get_core(instance_data_arr, i));
    }

    afferent_arc_draw_instanced(
        renderer,
        data,
        instance_count,
        segments,
        (float)canvas_width,
        (float)canvas_height
    );

    free(data);
    return lean_io_result_mk_ok(lean_box(0));
}

// Draw instanced arcs from FloatBuffer (high-performance, no copy)
LEAN_EXPORT lean_obj_res lean_afferent_arc_draw_instanced_buffer(
    lean_obj_arg renderer_obj,
    lean_obj_arg buffer_obj,
    uint32_t instance_count,
    uint32_t segments,
    double canvas_width,
    double canvas_height,
    lean_obj_arg world
) {
    AfferentRendererRef renderer = (AfferentRendererRef)lean_get_external_data(renderer_obj);
    AfferentFloatBufferRef buffer = (AfferentFloatBufferRef)lean_get_external_data(buffer_obj);

    if (instance_count == 0) {
        return lean_io_result_mk_ok(lean_box(0));
    }

    const float* data = afferent_float_buffer_data(buffer);

    afferent_arc_draw_instanced(
        renderer,
        data,
        instance_count,
        segments,
        (float)canvas_width,
        (float)canvas_height
    );

    return lean_io_result_mk_ok(lean_box(0));
}

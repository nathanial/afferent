#include "lean_bridge_internal.h"

// Create vertex buffer from Float array
// Each vertex is 6 floats: position[2], color[4]
LEAN_EXPORT lean_obj_res lean_afferent_buffer_create_vertex(
    lean_obj_arg renderer_obj,
    lean_obj_arg vertices_arr,
    lean_obj_arg world
) {
    afferent_ensure_initialized();
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

// Create stroke vertex buffer from Float array
// Each vertex is 5 floats: position[2], normal[2], side
LEAN_EXPORT lean_obj_res lean_afferent_buffer_create_stroke_vertex(
    lean_obj_arg renderer_obj,
    lean_obj_arg vertices_arr,
    lean_obj_arg world
) {
    afferent_ensure_initialized();
    AfferentRendererRef renderer = (AfferentRendererRef)lean_get_external_data(renderer_obj);

    size_t arr_size = lean_array_size(vertices_arr);
    size_t vertex_count = arr_size / 5;  // 5 floats per vertex

    if (vertex_count == 0) {
        return lean_io_result_mk_error(lean_mk_io_user_error(
            lean_mk_string("Empty stroke vertex array")));
    }

    AfferentStrokeVertex* vertices = malloc(vertex_count * sizeof(AfferentStrokeVertex));
    if (!vertices) {
        return lean_io_result_mk_error(lean_mk_io_user_error(
            lean_mk_string("Failed to allocate stroke vertex memory")));
    }

    for (size_t i = 0; i < vertex_count; i++) {
        size_t base = i * 5;
        vertices[i].position[0] = (float)lean_unbox_float(lean_array_get_core(vertices_arr, base + 0));
        vertices[i].position[1] = (float)lean_unbox_float(lean_array_get_core(vertices_arr, base + 1));
        vertices[i].normal[0] = (float)lean_unbox_float(lean_array_get_core(vertices_arr, base + 2));
        vertices[i].normal[1] = (float)lean_unbox_float(lean_array_get_core(vertices_arr, base + 3));
        vertices[i].side = (float)lean_unbox_float(lean_array_get_core(vertices_arr, base + 4));
    }

    AfferentBufferRef buffer = NULL;
    AfferentResult result = afferent_buffer_create_stroke_vertex(renderer, vertices, (uint32_t)vertex_count, &buffer);
    free(vertices);

    if (result != AFFERENT_OK) {
        return lean_io_result_mk_error(lean_mk_io_user_error(
            lean_mk_string("Failed to create stroke vertex buffer")));
    }

    lean_object* obj = lean_alloc_external(g_buffer_class, buffer);
    return lean_io_result_mk_ok(obj);
}

// Create stroke segment buffer from Float array
// Each segment is 18 floats:
// p0.xy, p1.xy, c1.xy, c2.xy, prevDir.xy, nextDir.xy, startDist, length, hasPrev, hasNext, kind, padding
LEAN_EXPORT lean_obj_res lean_afferent_buffer_create_stroke_segment(
    lean_obj_arg renderer_obj,
    lean_obj_arg segments_arr,
    lean_obj_arg world
) {
    afferent_ensure_initialized();
    AfferentRendererRef renderer = (AfferentRendererRef)lean_get_external_data(renderer_obj);

    size_t arr_size = lean_array_size(segments_arr);
    size_t segment_count = arr_size / 18;

    if (segment_count == 0) {
        return lean_io_result_mk_error(lean_mk_io_user_error(
            lean_mk_string("Empty stroke segment array")));
    }

    AfferentStrokeSegment* segments = malloc(segment_count * sizeof(AfferentStrokeSegment));
    if (!segments) {
        return lean_io_result_mk_error(lean_mk_io_user_error(
            lean_mk_string("Failed to allocate stroke segment memory")));
    }

    for (size_t i = 0; i < segment_count; i++) {
        size_t base = i * 18;
        segments[i].p0[0] = (float)lean_unbox_float(lean_array_get_core(segments_arr, base + 0));
        segments[i].p0[1] = (float)lean_unbox_float(lean_array_get_core(segments_arr, base + 1));
        segments[i].p1[0] = (float)lean_unbox_float(lean_array_get_core(segments_arr, base + 2));
        segments[i].p1[1] = (float)lean_unbox_float(lean_array_get_core(segments_arr, base + 3));
        segments[i].c1[0] = (float)lean_unbox_float(lean_array_get_core(segments_arr, base + 4));
        segments[i].c1[1] = (float)lean_unbox_float(lean_array_get_core(segments_arr, base + 5));
        segments[i].c2[0] = (float)lean_unbox_float(lean_array_get_core(segments_arr, base + 6));
        segments[i].c2[1] = (float)lean_unbox_float(lean_array_get_core(segments_arr, base + 7));
        segments[i].prevDir[0] = (float)lean_unbox_float(lean_array_get_core(segments_arr, base + 8));
        segments[i].prevDir[1] = (float)lean_unbox_float(lean_array_get_core(segments_arr, base + 9));
        segments[i].nextDir[0] = (float)lean_unbox_float(lean_array_get_core(segments_arr, base + 10));
        segments[i].nextDir[1] = (float)lean_unbox_float(lean_array_get_core(segments_arr, base + 11));
        segments[i].startDist = (float)lean_unbox_float(lean_array_get_core(segments_arr, base + 12));
        segments[i].length = (float)lean_unbox_float(lean_array_get_core(segments_arr, base + 13));
        segments[i].hasPrev = (float)lean_unbox_float(lean_array_get_core(segments_arr, base + 14));
        segments[i].hasNext = (float)lean_unbox_float(lean_array_get_core(segments_arr, base + 15));
        segments[i].kind = (float)lean_unbox_float(lean_array_get_core(segments_arr, base + 16));
        segments[i].padding = (float)lean_unbox_float(lean_array_get_core(segments_arr, base + 17));
    }

    AfferentBufferRef buffer = NULL;
    AfferentResult result = afferent_buffer_create_stroke_segment(renderer, segments, (uint32_t)segment_count, &buffer);
    free(segments);

    if (result != AFFERENT_OK) {
        return lean_io_result_mk_error(lean_mk_io_user_error(
            lean_mk_string("Failed to create stroke segment buffer")));
    }

    lean_object* obj = lean_alloc_external(g_buffer_class, buffer);
    return lean_io_result_mk_ok(obj);
}

// Create persistent stroke segment buffer from Float array
LEAN_EXPORT lean_obj_res lean_afferent_buffer_create_stroke_segment_persistent(
    lean_obj_arg renderer_obj,
    lean_obj_arg segments_arr,
    lean_obj_arg world
) {
    afferent_ensure_initialized();
    AfferentRendererRef renderer = (AfferentRendererRef)lean_get_external_data(renderer_obj);

    size_t arr_size = lean_array_size(segments_arr);
    size_t segment_count = arr_size / 18;

    if (segment_count == 0) {
        return lean_io_result_mk_error(lean_mk_io_user_error(
            lean_mk_string("Empty stroke segment array")));
    }

    AfferentStrokeSegment* segments = malloc(segment_count * sizeof(AfferentStrokeSegment));
    if (!segments) {
        return lean_io_result_mk_error(lean_mk_io_user_error(
            lean_mk_string("Failed to allocate stroke segment memory")));
    }

    for (size_t i = 0; i < segment_count; i++) {
        size_t base = i * 18;
        segments[i].p0[0] = (float)lean_unbox_float(lean_array_get_core(segments_arr, base + 0));
        segments[i].p0[1] = (float)lean_unbox_float(lean_array_get_core(segments_arr, base + 1));
        segments[i].p1[0] = (float)lean_unbox_float(lean_array_get_core(segments_arr, base + 2));
        segments[i].p1[1] = (float)lean_unbox_float(lean_array_get_core(segments_arr, base + 3));
        segments[i].c1[0] = (float)lean_unbox_float(lean_array_get_core(segments_arr, base + 4));
        segments[i].c1[1] = (float)lean_unbox_float(lean_array_get_core(segments_arr, base + 5));
        segments[i].c2[0] = (float)lean_unbox_float(lean_array_get_core(segments_arr, base + 6));
        segments[i].c2[1] = (float)lean_unbox_float(lean_array_get_core(segments_arr, base + 7));
        segments[i].prevDir[0] = (float)lean_unbox_float(lean_array_get_core(segments_arr, base + 8));
        segments[i].prevDir[1] = (float)lean_unbox_float(lean_array_get_core(segments_arr, base + 9));
        segments[i].nextDir[0] = (float)lean_unbox_float(lean_array_get_core(segments_arr, base + 10));
        segments[i].nextDir[1] = (float)lean_unbox_float(lean_array_get_core(segments_arr, base + 11));
        segments[i].startDist = (float)lean_unbox_float(lean_array_get_core(segments_arr, base + 12));
        segments[i].length = (float)lean_unbox_float(lean_array_get_core(segments_arr, base + 13));
        segments[i].hasPrev = (float)lean_unbox_float(lean_array_get_core(segments_arr, base + 14));
        segments[i].hasNext = (float)lean_unbox_float(lean_array_get_core(segments_arr, base + 15));
        segments[i].kind = (float)lean_unbox_float(lean_array_get_core(segments_arr, base + 16));
        segments[i].padding = (float)lean_unbox_float(lean_array_get_core(segments_arr, base + 17));
    }

    AfferentBufferRef buffer = NULL;
    AfferentResult result = afferent_buffer_create_stroke_segment_persistent(
        renderer,
        segments,
        (uint32_t)segment_count,
        &buffer
    );
    free(segments);

    if (result != AFFERENT_OK) {
        return lean_io_result_mk_error(lean_mk_io_user_error(
            lean_mk_string("Failed to create persistent stroke segment buffer")));
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
    afferent_ensure_initialized();
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

// Draw extruded strokes (screen-space width)
LEAN_EXPORT lean_obj_res lean_afferent_renderer_draw_stroke(
    lean_obj_arg renderer_obj,
    lean_obj_arg vertex_buffer_obj,
    lean_obj_arg index_buffer_obj,
    uint32_t index_count,
    double half_width,
    double canvas_width,
    double canvas_height,
    double r,
    double g,
    double b,
    double a,
    lean_obj_arg world
) {
    AfferentRendererRef renderer = (AfferentRendererRef)lean_get_external_data(renderer_obj);
    AfferentBufferRef vertex_buffer = (AfferentBufferRef)lean_get_external_data(vertex_buffer_obj);
    AfferentBufferRef index_buffer = (AfferentBufferRef)lean_get_external_data(index_buffer_obj);

    afferent_renderer_draw_stroke(
        renderer,
        vertex_buffer,
        index_buffer,
        index_count,
        (float)half_width,
        (float)canvas_width,
        (float)canvas_height,
        (float)r,
        (float)g,
        (float)b,
        (float)a
    );
    return lean_io_result_mk_ok(lean_box(0));
}

// Draw GPU stroke path from parametric segments
LEAN_EXPORT lean_obj_res lean_afferent_renderer_draw_stroke_path(
    lean_obj_arg renderer_obj,
    lean_obj_arg segment_buffer_obj,
    uint32_t segment_count,
    uint32_t segment_subdivisions,
    double half_width,
    double canvas_width,
    double canvas_height,
    double miter_limit,
    uint32_t line_cap,
    uint32_t line_join,
    double transform_a,
    double transform_b,
    double transform_c,
    double transform_d,
    double transform_tx,
    double transform_ty,
    lean_obj_arg dash_segments_arr,
    uint32_t dash_count,
    double dash_offset,
    double r,
    double g,
    double b,
    double a,
    lean_obj_arg world
) {
    AfferentRendererRef renderer = (AfferentRendererRef)lean_get_external_data(renderer_obj);
    AfferentBufferRef segment_buffer = (AfferentBufferRef)lean_get_external_data(segment_buffer_obj);

    float dashSegments[8] = {0};
    uint32_t count = dash_count > 8 ? 8 : dash_count;
    size_t arr_size = lean_array_size(dash_segments_arr);
    if (count > arr_size) {
        count = (uint32_t)arr_size;
    }

    for (uint32_t i = 0; i < count; i++) {
        dashSegments[i] = (float)lean_unbox_float(lean_array_get_core(dash_segments_arr, i));
    }

    const float* dash_ptr = count > 0 ? dashSegments : NULL;

    afferent_renderer_draw_stroke_path(
        renderer,
        segment_buffer,
        segment_count,
        segment_subdivisions,
        (float)half_width,
        (float)canvas_width,
        (float)canvas_height,
        (float)miter_limit,
        line_cap,
        line_join,
        (float)transform_a,
        (float)transform_b,
        (float)transform_c,
        (float)transform_d,
        (float)transform_tx,
        (float)transform_ty,
        dash_ptr,
        count,
        (float)dash_offset,
        (float)r,
        (float)g,
        (float)b,
        (float)a
    );

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

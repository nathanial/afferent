#include "lean_bridge_internal.h"

// =============================================================================
// 3D Mesh Rendering
// =============================================================================
// Draw 3D mesh with perspective projection, lighting, and fog parameters
// vertices_arr: Array Float (10 floats per vertex: pos[3], normal[3], color[4])
// indices_arr: Array UInt32 (triangle indices)
// mvp_matrix: Array Float (16 floats, column-major)
// model_matrix: Array Float (16 floats, column-major)
// light_dir: Array Float (3 floats, normalized direction)
// camera_pos: Array Float (3 floats)
// fog_color: Array Float (3 floats)
// fog_start/fog_end: fog distances (0 disables fog)
LEAN_EXPORT lean_obj_res lean_afferent_renderer_draw_mesh_3d(
    lean_obj_arg renderer_obj,
    lean_obj_arg vertices_arr,
    lean_obj_arg indices_arr,
    lean_obj_arg mvp_matrix,
    lean_obj_arg model_matrix,
    lean_obj_arg light_dir,
    double ambient,
    lean_obj_arg camera_pos_arr,
    lean_obj_arg fog_color_arr,
    double fog_start,
    double fog_end,
    lean_obj_arg world
) {
    AfferentRendererRef renderer = (AfferentRendererRef)lean_get_external_data(renderer_obj);

    // Convert vertex array (10 floats per vertex)
    size_t vert_floats = lean_array_size(vertices_arr);
    size_t vertex_count = vert_floats / 10;

    if (vertex_count == 0) {
        return lean_io_result_mk_ok(lean_box(0));
    }

    AfferentVertex3D* vertices = malloc(vertex_count * sizeof(AfferentVertex3D));
    if (!vertices) {
        return lean_io_result_mk_error(lean_mk_io_user_error(
            lean_mk_string("Failed to allocate vertex buffer")));
    }

    for (size_t i = 0; i < vertex_count; i++) {
        size_t base = i * 10;
        // Position
        vertices[i].position[0] = (float)lean_unbox_float(lean_array_get_core(vertices_arr, base + 0));
        vertices[i].position[1] = (float)lean_unbox_float(lean_array_get_core(vertices_arr, base + 1));
        vertices[i].position[2] = (float)lean_unbox_float(lean_array_get_core(vertices_arr, base + 2));
        // Normal
        vertices[i].normal[0] = (float)lean_unbox_float(lean_array_get_core(vertices_arr, base + 3));
        vertices[i].normal[1] = (float)lean_unbox_float(lean_array_get_core(vertices_arr, base + 4));
        vertices[i].normal[2] = (float)lean_unbox_float(lean_array_get_core(vertices_arr, base + 5));
        // Color
        vertices[i].color[0] = (float)lean_unbox_float(lean_array_get_core(vertices_arr, base + 6));
        vertices[i].color[1] = (float)lean_unbox_float(lean_array_get_core(vertices_arr, base + 7));
        vertices[i].color[2] = (float)lean_unbox_float(lean_array_get_core(vertices_arr, base + 8));
        vertices[i].color[3] = (float)lean_unbox_float(lean_array_get_core(vertices_arr, base + 9));
    }

    // Convert index array
    size_t index_count = lean_array_size(indices_arr);
    uint32_t* indices = malloc(index_count * sizeof(uint32_t));
    if (!indices) {
        free(vertices);
        return lean_io_result_mk_error(lean_mk_io_user_error(
            lean_mk_string("Failed to allocate index buffer")));
    }

    for (size_t i = 0; i < index_count; i++) {
        indices[i] = lean_unbox_uint32(lean_array_get_core(indices_arr, i));
    }

    // Convert MVP matrix (16 floats)
    float mvp[16];
    for (size_t i = 0; i < 16; i++) {
        mvp[i] = (float)lean_unbox_float(lean_array_get_core(mvp_matrix, i));
    }

    // Convert model matrix (16 floats)
    float model[16];
    for (size_t i = 0; i < 16; i++) {
        model[i] = (float)lean_unbox_float(lean_array_get_core(model_matrix, i));
    }

    // Convert light direction (3 floats)
    float light[3];
    for (size_t i = 0; i < 3; i++) {
        light[i] = (float)lean_unbox_float(lean_array_get_core(light_dir, i));
    }

    // Convert camera position (3 floats)
    float camera_pos[3];
    for (size_t i = 0; i < 3; i++) {
        camera_pos[i] = (float)lean_unbox_float(lean_array_get_core(camera_pos_arr, i));
    }

    // Convert fog color (3 floats)
    float fog_color[3];
    for (size_t i = 0; i < 3; i++) {
        fog_color[i] = (float)lean_unbox_float(lean_array_get_core(fog_color_arr, i));
    }

    // Draw the mesh with fog
    afferent_renderer_draw_mesh_3d(
        renderer, vertices, (uint32_t)vertex_count,
        indices, (uint32_t)index_count,
        mvp, model, light, (float)ambient,
        camera_pos, fog_color, (float)fog_start, (float)fog_end
    );

    free(vertices);
    free(indices);

    return lean_io_result_mk_ok(lean_box(0));
}

// =============================================================================
// Projected-grid ocean rendering (GPU waves + fog)
// =============================================================================
LEAN_EXPORT lean_obj_res lean_afferent_renderer_draw_ocean_projected_grid_with_fog(
    lean_obj_arg renderer_obj,
    uint32_t grid_size,
    lean_obj_arg mvp_matrix,
    lean_obj_arg model_matrix,
    lean_obj_arg light_dir,
    double ambient,
    lean_obj_arg camera_pos_arr,
    lean_obj_arg fog_color_arr,
    double fog_start,
    double fog_end,
    double time,
    double fovY,
    double aspect,
    double maxDistance,
    double snapSize,
    double overscanNdc,
    double horizonMargin,
    double yaw,
    double pitch,
    lean_obj_arg wave_params_arr,
    lean_obj_arg world
) {
    AfferentRendererRef renderer = (AfferentRendererRef)lean_get_external_data(renderer_obj);

    // Convert MVP matrix (16 floats)
    float mvp[16];
    for (size_t i = 0; i < 16; i++) {
        mvp[i] = (float)lean_unbox_float(lean_array_get_core(mvp_matrix, i));
    }

    // Convert model matrix (16 floats)
    float model[16];
    for (size_t i = 0; i < 16; i++) {
        model[i] = (float)lean_unbox_float(lean_array_get_core(model_matrix, i));
    }

    // Convert light direction (3 floats)
    float light[3];
    for (size_t i = 0; i < 3; i++) {
        light[i] = (float)lean_unbox_float(lean_array_get_core(light_dir, i));
    }

    // Convert camera position (3 floats)
    float camera_pos[3];
    for (size_t i = 0; i < 3; i++) {
        camera_pos[i] = (float)lean_unbox_float(lean_array_get_core(camera_pos_arr, i));
    }

    // Convert fog color (3 floats)
    float fog_color[3];
    for (size_t i = 0; i < 3; i++) {
        fog_color[i] = (float)lean_unbox_float(lean_array_get_core(fog_color_arr, i));
    }

    // Convert wave params (expect 32 floats, but accept shorter)
    float wave_params[32] = {0};
    uint32_t wave_count = (uint32_t)lean_array_size(wave_params_arr);
    if (wave_count > 32) wave_count = 32;
    for (uint32_t i = 0; i < wave_count; i++) {
        wave_params[i] = (float)lean_unbox_float(lean_array_get_core(wave_params_arr, i));
    }

    afferent_renderer_draw_ocean_projected_grid_with_fog(
        renderer,
        grid_size,
        mvp,
        model,
        light,
        (float)ambient,
        camera_pos,
        fog_color,
        (float)fog_start,
        (float)fog_end,
        (float)time,
        (float)fovY,
        (float)aspect,
        (float)maxDistance,
        (float)snapSize,
        (float)overscanNdc,
        (float)horizonMargin,
        (float)yaw,
        (float)pitch,
        wave_params,
        wave_count
    );

    return lean_io_result_mk_ok(lean_box(0));
}

// =============================================================================
// Textured 3D Mesh Rendering FFI
// =============================================================================

// Cached CPU-side conversions for 3D mesh draws.
// The Seascape frigate is drawn as many submeshes that all share the same
// vertex/index arrays. Re-converting and malloc/free per submesh is extremely
// expensive and can lead to memory pressure/crashes.
static lean_object* g_cached_mesh_vertices_arr = NULL;
static float* g_cached_mesh_vertices = NULL;
static size_t g_cached_mesh_vertices_floats = 0;

static lean_object* g_cached_mesh_indices_arr = NULL;
static uint32_t* g_cached_mesh_indices = NULL;
static size_t g_cached_mesh_indices_count = 0;

// Draw textured 3D mesh with fog
// vertices_arr: Array Float (12 floats per vertex: pos[3], normal[3], uv[2], color[4])
// indices_arr: Array UInt32
// index_offset, index_count: sub-range of indices to draw
LEAN_EXPORT lean_obj_res lean_afferent_renderer_draw_mesh_3d_textured(
    lean_obj_arg renderer_obj,
    lean_obj_arg vertices_arr,
    lean_obj_arg indices_arr,
    uint32_t index_offset,
    uint32_t index_count,
    lean_obj_arg mvp_matrix,
    lean_obj_arg model_matrix,
    lean_obj_arg light_dir,
    double ambient,
    lean_obj_arg camera_pos_arr,
    lean_obj_arg fog_color_arr,
    double fog_start,
    double fog_end,
    lean_obj_arg texture_obj,
    lean_obj_arg world
) {
    AfferentRendererRef renderer = (AfferentRendererRef)lean_get_external_data(renderer_obj);
    AfferentTextureRef texture = (AfferentTextureRef)lean_get_external_data(texture_obj);

    // Convert vertex array (12 floats per vertex)
    size_t vert_floats = lean_array_size(vertices_arr);
    size_t vertex_count = vert_floats / 12;

    if (vertex_count == 0 || index_count == 0) {
        return lean_io_result_mk_ok(lean_box(0));
    }

    // Convert/cached vertices
    if (g_cached_mesh_vertices_arr != vertices_arr || g_cached_mesh_vertices_floats != vert_floats) {
        if (g_cached_mesh_vertices_arr) {
            lean_dec(g_cached_mesh_vertices_arr);
            g_cached_mesh_vertices_arr = NULL;
        }
        if (g_cached_mesh_vertices) {
            free(g_cached_mesh_vertices);
            g_cached_mesh_vertices = NULL;
        }

        g_cached_mesh_vertices = malloc(vert_floats * sizeof(float));
        if (!g_cached_mesh_vertices) {
            return lean_io_result_mk_error(lean_mk_io_user_error(
                lean_mk_string("Failed to allocate vertex buffer")));
        }

        for (size_t i = 0; i < vert_floats; i++) {
            g_cached_mesh_vertices[i] = (float)lean_unbox_float(lean_array_get_core(vertices_arr, i));
        }

        g_cached_mesh_vertices_floats = vert_floats;
        g_cached_mesh_vertices_arr = vertices_arr;
        lean_inc(g_cached_mesh_vertices_arr);
    }

    // Convert/cached indices
    size_t total_indices = lean_array_size(indices_arr);
    if (g_cached_mesh_indices_arr != indices_arr || g_cached_mesh_indices_count != total_indices) {
        if (g_cached_mesh_indices_arr) {
            lean_dec(g_cached_mesh_indices_arr);
            g_cached_mesh_indices_arr = NULL;
        }
        if (g_cached_mesh_indices) {
            free(g_cached_mesh_indices);
            g_cached_mesh_indices = NULL;
        }

        g_cached_mesh_indices = malloc(total_indices * sizeof(uint32_t));
        if (!g_cached_mesh_indices) {
            return lean_io_result_mk_error(lean_mk_io_user_error(
                lean_mk_string("Failed to allocate index buffer")));
        }

        for (size_t i = 0; i < total_indices; i++) {
            g_cached_mesh_indices[i] = lean_unbox_uint32(lean_array_get_core(indices_arr, i));
        }

        g_cached_mesh_indices_count = total_indices;
        g_cached_mesh_indices_arr = indices_arr;
        lean_inc(g_cached_mesh_indices_arr);
    }

    // Clamp to valid range.
    if (index_offset >= total_indices) {
        return lean_io_result_mk_ok(lean_box(0));
    }
    if ((size_t)index_offset + (size_t)index_count > total_indices) {
        index_count = (uint32_t)(total_indices - (size_t)index_offset);
        if (index_count == 0) {
            return lean_io_result_mk_ok(lean_box(0));
        }
    }

    // Convert matrices and vectors
    float mvp[16], model[16], light[3], camera_pos[3], fog_color[3];

    for (size_t i = 0; i < 16; i++) {
        mvp[i] = (float)lean_unbox_float(lean_array_get_core(mvp_matrix, i));
        model[i] = (float)lean_unbox_float(lean_array_get_core(model_matrix, i));
    }

    for (size_t i = 0; i < 3; i++) {
        light[i] = (float)lean_unbox_float(lean_array_get_core(light_dir, i));
        camera_pos[i] = (float)lean_unbox_float(lean_array_get_core(camera_pos_arr, i));
        fog_color[i] = (float)lean_unbox_float(lean_array_get_core(fog_color_arr, i));
    }

    // Draw the textured mesh
    afferent_renderer_draw_mesh_3d_textured(
        renderer,
        g_cached_mesh_vertices, (uint32_t)vertex_count,
        g_cached_mesh_indices, index_offset, index_count,
        mvp, model, light, (float)ambient,
        camera_pos, fog_color, (float)fog_start, (float)fog_end,
        texture
    );

    return lean_io_result_mk_ok(lean_box(0));
}

// fragment.m - FFI bridge for shader fragment compilation and rendering
#include "lean_bridge_internal.h"
#include "../metal/fragment_compiler.h"
#include "../metal/render.h"

// External class for fragment pipelines
extern lean_external_class* g_fragment_pipeline_class;

// Helper to construct Option.none
static inline lean_obj_res mk_option_none(void) {
    return lean_box(0);
}

// Helper to construct Option.some
static inline lean_obj_res mk_option_some(lean_obj_arg value) {
    lean_object* obj = lean_alloc_ctor(1, 1, 0);
    lean_ctor_set(obj, 0, value);
    return obj;
}

// =============================================================================
// Fragment Pipeline Compilation
// =============================================================================

// Compile a shader fragment into a GPU pipeline
// Returns Option FragmentPipeline (some on success, none on failure)
LEAN_EXPORT lean_obj_res lean_afferent_fragment_compile(
    b_lean_obj_arg renderer_obj,
    b_lean_obj_arg name_str,
    b_lean_obj_arg params_struct_str,
    b_lean_obj_arg function_code_str,
    uint32_t primitive_type,
    uint32_t instance_count,
    uint32_t params_float_count,
    lean_obj_arg world
) {
    (void)world;
    afferent_ensure_initialized();

    AfferentRendererRef renderer = (AfferentRendererRef)lean_get_external_data(renderer_obj);
    if (!renderer) {
        return lean_io_result_mk_ok(mk_option_none());
    }

    const char* name = lean_string_cstr(name_str);
    const char* params_struct = lean_string_cstr(params_struct_str);
    const char* function_code = lean_string_cstr(function_code_str);

    // Get Metal device from renderer
    id<MTLDevice> device = afferent_renderer_get_device(renderer);
    if (!device) {
        return lean_io_result_mk_ok(mk_option_none());
    }

    // Compile the fragment
    AfferentFragmentPipelineRef pipeline = afferent_fragment_compile(
        device,
        name,
        params_struct,
        function_code,
        primitive_type,
        instance_count,
        params_float_count
    );

    if (!pipeline) {
        return lean_io_result_mk_ok(mk_option_none());
    }

    // Wrap in Lean external object
    lean_object* result = lean_alloc_external(g_fragment_pipeline_class, pipeline);
    return lean_io_result_mk_ok(mk_option_some(result));
}

// =============================================================================
// Fragment Pipeline Destruction
// =============================================================================

LEAN_EXPORT lean_obj_res lean_afferent_fragment_destroy(
    b_lean_obj_arg pipeline_obj,
    lean_obj_arg world
) {
    (void)world;

    AfferentFragmentPipelineRef pipeline =
        (AfferentFragmentPipelineRef)lean_get_external_data(pipeline_obj);

    if (pipeline) {
        afferent_fragment_destroy(pipeline);
    }

    return lean_io_result_mk_ok(lean_box(0));
}

// =============================================================================
// Fragment Drawing
// =============================================================================

// Draw using a compiled fragment pipeline with Array Float params
// The array contains batchCount * paramsFloatCount floats (multiple param structs concatenated)
LEAN_EXPORT lean_obj_res lean_afferent_fragment_draw(
    b_lean_obj_arg renderer_obj,
    b_lean_obj_arg pipeline_obj,
    b_lean_obj_arg params_arr,
    double canvas_width,
    double canvas_height,
    lean_obj_arg world
) {
    (void)world;

    AfferentRendererRef renderer = (AfferentRendererRef)lean_get_external_data(renderer_obj);
    AfferentFragmentPipelineRef pipeline =
        (AfferentFragmentPipelineRef)lean_get_external_data(pipeline_obj);

    if (!renderer || !pipeline || !pipeline->pipelineState) {
        return lean_io_result_mk_ok(lean_box(0));
    }

    // Convert params array to float buffer
    size_t param_count = lean_array_size(params_arr);
    if (param_count == 0 || pipeline->paramsFloatCount == 0) {
        return lean_io_result_mk_ok(lean_box(0));
    }

    // Compute batch count from array size
    uint32_t batchCount = (uint32_t)(param_count / pipeline->paramsFloatCount);
    if (batchCount == 0) {
        return lean_io_result_mk_ok(lean_box(0));
    }

    // Allocate temporary buffer for params
    float* params = (float*)malloc(param_count * sizeof(float));
    if (!params) {
        return lean_io_result_mk_ok(lean_box(0));
    }

    for (size_t i = 0; i < param_count; i++) {
        params[i] = (float)lean_unbox_float(lean_array_get_core(params_arr, i));
    }

    // Get encoder from renderer
    id<MTLRenderCommandEncoder> encoder = afferent_renderer_get_encoder(renderer);
    if (!encoder) {
        free(params);
        return lean_io_result_mk_ok(lean_box(0));
    }

    // Get device for buffer creation
    id<MTLDevice> device = afferent_renderer_get_device(renderer);
    if (!device) {
        free(params);
        return lean_io_result_mk_ok(lean_box(0));
    }

    // Create Metal buffer for params
    id<MTLBuffer> paramsBuffer = [device newBufferWithBytes:params
                                                    length:param_count * sizeof(float)
                                                   options:MTLResourceStorageModeShared];
    free(params);

    if (!paramsBuffer) {
        return lean_io_result_mk_ok(lean_box(0));
    }

    // Draw using the fragment pipeline (batched)
    afferent_fragment_draw(
        encoder,
        pipeline,
        paramsBuffer,
        batchCount,
        (float)canvas_width,
        (float)canvas_height
    );

    return lean_io_result_mk_ok(lean_box(0));
}

// Draw using a compiled fragment pipeline with FloatBuffer params
// The buffer contains batchCount * paramsFloatCount floats (multiple param structs concatenated)
LEAN_EXPORT lean_obj_res lean_afferent_fragment_draw_buffer(
    b_lean_obj_arg renderer_obj,
    b_lean_obj_arg pipeline_obj,
    b_lean_obj_arg params_buffer_obj,
    double canvas_width,
    double canvas_height,
    lean_obj_arg world
) {
    (void)world;

    AfferentRendererRef renderer = (AfferentRendererRef)lean_get_external_data(renderer_obj);
    AfferentFragmentPipelineRef pipeline =
        (AfferentFragmentPipelineRef)lean_get_external_data(pipeline_obj);
    AfferentFloatBufferRef params_buffer =
        (AfferentFloatBufferRef)lean_get_external_data(params_buffer_obj);

    if (!renderer || !pipeline || !pipeline->pipelineState || !params_buffer) {
        return lean_io_result_mk_ok(lean_box(0));
    }

    // Get float data from FloatBuffer
    float* data = afferent_float_buffer_data(params_buffer);
    size_t count = afferent_float_buffer_count(params_buffer);

    if (!data || count == 0 || pipeline->paramsFloatCount == 0) {
        return lean_io_result_mk_ok(lean_box(0));
    }

    // Compute batch count from buffer size
    uint32_t batchCount = (uint32_t)(count / pipeline->paramsFloatCount);
    if (batchCount == 0) {
        return lean_io_result_mk_ok(lean_box(0));
    }

    // Get encoder from renderer
    id<MTLRenderCommandEncoder> encoder = afferent_renderer_get_encoder(renderer);
    if (!encoder) {
        return lean_io_result_mk_ok(lean_box(0));
    }

    // Get device for buffer creation
    id<MTLDevice> device = afferent_renderer_get_device(renderer);
    if (!device) {
        return lean_io_result_mk_ok(lean_box(0));
    }

    // Create Metal buffer for params
    id<MTLBuffer> paramsBuffer = [device newBufferWithBytes:data
                                                    length:count * sizeof(float)
                                                   options:MTLResourceStorageModeShared];

    if (!paramsBuffer) {
        return lean_io_result_mk_ok(lean_box(0));
    }

    // Draw using the fragment pipeline (batched)
    afferent_fragment_draw(
        encoder,
        pipeline,
        paramsBuffer,
        batchCount,
        (float)canvas_width,
        (float)canvas_height
    );

    return lean_io_result_mk_ok(lean_box(0));
}

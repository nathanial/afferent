#include "lean_bridge_internal.h"

// Renderer creation
LEAN_EXPORT lean_obj_res lean_afferent_renderer_create(lean_obj_arg window_obj, lean_obj_arg world) {
    afferent_ensure_initialized();
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

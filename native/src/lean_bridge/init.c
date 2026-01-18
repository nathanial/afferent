#include "lean_bridge_internal.h"

// External class registrations for opaque handles
lean_external_class* g_window_class = NULL;
lean_external_class* g_renderer_class = NULL;
lean_external_class* g_buffer_class = NULL;
lean_external_class* g_font_class = NULL;
lean_external_class* g_float_buffer_class = NULL;
lean_external_class* g_texture_class = NULL;
lean_external_class* g_cached_mesh_class = NULL;
lean_external_class* g_fragment_pipeline_class = NULL;
static uint8_t g_afferent_initialized = 0;

// Weak reference so we don't double-free if Lean GC happens after explicit destroy
static void window_finalizer(void* ptr) {
    // Note: We let explicit destroy handle cleanup to avoid double-free
    // In production, you'd want reference counting
}

// External objects in this project do not reference Lean heap objects, so the
// GC "foreach" callback is always a no-op. Some Lean runtimes may still call
// the callback unconditionally, so it must not be NULL.
static void afferent_external_foreach(void* ptr, b_lean_obj_arg f) {
    (void)ptr;
    (void)f;
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

static void cached_mesh_finalizer(void* ptr) {
    // Same as above
}

static void fragment_pipeline_finalizer(void* ptr) {
    // Same as above - let explicit destroy handle cleanup
}

void afferent_ensure_initialized(void) {
    if (g_afferent_initialized) return;

    g_window_class = lean_register_external_class(window_finalizer, afferent_external_foreach);
    g_renderer_class = lean_register_external_class(renderer_finalizer, afferent_external_foreach);
    g_buffer_class = lean_register_external_class(buffer_finalizer, afferent_external_foreach);
    g_font_class = lean_register_external_class(font_finalizer, afferent_external_foreach);
    g_float_buffer_class = lean_register_external_class(float_buffer_finalizer, afferent_external_foreach);
    g_texture_class = lean_register_external_class(texture_finalizer, afferent_external_foreach);
    g_cached_mesh_class = lean_register_external_class(cached_mesh_finalizer, afferent_external_foreach);
    g_fragment_pipeline_class = lean_register_external_class(fragment_pipeline_finalizer, afferent_external_foreach);

    // Initialize text subsystem
    afferent_text_init();

    g_afferent_initialized = 1;
}

// Module initialization
LEAN_EXPORT lean_obj_res afferent_initialize(uint8_t builtin, lean_obj_arg world) {
    (void)builtin;
    (void)world;
    afferent_ensure_initialized();

    return lean_io_result_mk_ok(lean_box(0));
}

// Set shader source from Lean (embedded shaders)
extern void afferent_set_shader_source(const char* name, const char* source);

LEAN_EXPORT lean_obj_res lean_afferent_set_shader_source(b_lean_obj_arg name, b_lean_obj_arg source, lean_obj_arg world) {
    (void)world;
    const char* name_str = lean_string_cstr(name);
    const char* source_str = lean_string_cstr(source);
    afferent_set_shader_source(name_str, source_str);
    return lean_io_result_mk_ok(lean_box(0));
}

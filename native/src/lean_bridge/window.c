#include "lean_bridge_internal.h"

// Window creation
LEAN_EXPORT lean_obj_res lean_afferent_window_create(
    uint32_t width,
    uint32_t height,
    lean_obj_arg title,
    lean_obj_arg world
) {
    afferent_ensure_initialized();
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

// Window run event loop (blocks until stopped)
LEAN_EXPORT lean_obj_res lean_afferent_window_run_event_loop(lean_obj_arg window_obj, lean_obj_arg world) {
    AfferentWindowRef window = (AfferentWindowRef)lean_get_external_data(window_obj);
    afferent_window_run_event_loop(window);
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

// Get keyboard key code (only valid if hasKeyPressed is true)
LEAN_EXPORT lean_obj_res lean_afferent_window_get_key_code(lean_obj_arg window_obj, lean_obj_arg world) {
    AfferentWindowRef window = (AfferentWindowRef)lean_get_external_data(window_obj);
    uint16_t key_code = afferent_window_get_key_code(window);
    return lean_io_result_mk_ok(lean_box(key_code));
}

// Check if a key is pending (distinguishes key code 0 from "no key")
LEAN_EXPORT lean_obj_res lean_afferent_window_has_key_pressed(lean_obj_arg window_obj, lean_obj_arg world) {
    AfferentWindowRef window = (AfferentWindowRef)lean_get_external_data(window_obj);
    bool has_key = afferent_window_has_key_pressed(window);
    return lean_io_result_mk_ok(lean_box(has_key ? 1 : 0));
}

// Clear keyboard state
LEAN_EXPORT lean_obj_res lean_afferent_window_clear_key(lean_obj_arg window_obj, lean_obj_arg world) {
    AfferentWindowRef window = (AfferentWindowRef)lean_get_external_data(window_obj);
    afferent_window_clear_key(window);
    return lean_io_result_mk_ok(lean_box(0));
}

// Get screen scale factor
LEAN_EXPORT lean_obj_res lean_afferent_get_screen_scale(lean_obj_arg world) {
    float scale = afferent_get_screen_scale();
    return lean_io_result_mk_ok(lean_box_float((double)scale));
}

// Mouse position - returns (Float, Float) tuple
LEAN_EXPORT lean_obj_res lean_afferent_window_get_mouse_pos(lean_obj_arg window_obj, lean_obj_arg world) {
    AfferentWindowRef window = (AfferentWindowRef)lean_get_external_data(window_obj);
    float x = 0, y = 0;
    afferent_window_get_mouse_pos(window, &x, &y);
    lean_object* tuple = lean_alloc_ctor(0, 2, 0);
    lean_ctor_set(tuple, 0, lean_box_float((double)x));
    lean_ctor_set(tuple, 1, lean_box_float((double)y));
    return lean_io_result_mk_ok(tuple);
}

// Mouse buttons - returns UInt8 bitmask
LEAN_EXPORT lean_obj_res lean_afferent_window_get_mouse_buttons(lean_obj_arg window_obj, lean_obj_arg world) {
    AfferentWindowRef window = (AfferentWindowRef)lean_get_external_data(window_obj);
    uint8_t buttons = afferent_window_get_mouse_buttons(window);
    return lean_io_result_mk_ok(lean_box(buttons));
}

// Modifier keys - returns UInt16 bitmask
LEAN_EXPORT lean_obj_res lean_afferent_window_get_modifiers(lean_obj_arg window_obj, lean_obj_arg world) {
    AfferentWindowRef window = (AfferentWindowRef)lean_get_external_data(window_obj);
    uint16_t mods = afferent_window_get_modifiers(window);
    return lean_io_result_mk_ok(lean_box(mods));
}

// Scroll delta - returns (Float, Float) tuple
LEAN_EXPORT lean_obj_res lean_afferent_window_get_scroll_delta(lean_obj_arg window_obj, lean_obj_arg world) {
    AfferentWindowRef window = (AfferentWindowRef)lean_get_external_data(window_obj);
    float dx = 0, dy = 0;
    afferent_window_get_scroll_delta(window, &dx, &dy);
    lean_object* tuple = lean_alloc_ctor(0, 2, 0);
    lean_ctor_set(tuple, 0, lean_box_float((double)dx));
    lean_ctor_set(tuple, 1, lean_box_float((double)dy));
    return lean_io_result_mk_ok(tuple);
}

// Clear scroll delta
LEAN_EXPORT lean_obj_res lean_afferent_window_clear_scroll(lean_obj_arg window_obj, lean_obj_arg world) {
    AfferentWindowRef window = (AfferentWindowRef)lean_get_external_data(window_obj);
    afferent_window_clear_scroll(window);
    return lean_io_result_mk_ok(lean_box(0));
}

// Mouse in window - returns Bool
LEAN_EXPORT lean_obj_res lean_afferent_window_mouse_in_window(lean_obj_arg window_obj, lean_obj_arg world) {
    AfferentWindowRef window = (AfferentWindowRef)lean_get_external_data(window_obj);
    bool inWindow = afferent_window_mouse_in_window(window);
    return lean_io_result_mk_ok(lean_box(inWindow ? 1 : 0));
}

// Get click event - returns Option ClickEvent
// ClickEvent structure: { button: UInt8, x: Float, y: Float, modifiers: UInt16 }
LEAN_EXPORT lean_obj_res lean_afferent_window_get_click(lean_obj_arg window_obj, lean_obj_arg world) {
    AfferentWindowRef window = (AfferentWindowRef)lean_get_external_data(window_obj);
    uint8_t button;
    float x, y;
    uint16_t modifiers;

    if (afferent_window_get_click(window, &button, &x, &y, &modifiers)) {
        // Construct ClickEvent as an unboxed-scalar structure.
        // The Lean compiler represents this struct with 0 object fields and 19 bytes of scalar data:
        //   offset 0  : Float (x)
        //   offset 8  : Float (y)
        //   offset 16 : UInt16 (modifiers)
        //   offset 18 : UInt8 (button)
        lean_object* click = lean_alloc_ctor(0, 0, 19);
        lean_ctor_set_float(click, 0, (double)x);
        lean_ctor_set_float(click, 8, (double)y);
        lean_ctor_set_uint16(click, 16, modifiers);
        lean_ctor_set_uint8(click, 18, button);

        // Wrap in Option.some (constructor 1)
        lean_object* some = lean_alloc_ctor(1, 1, 0);
        lean_ctor_set(some, 0, click);
        return lean_io_result_mk_ok(some);
    } else {
        // Return Option.none (constructor 0, no fields)
        return lean_io_result_mk_ok(lean_box(0));
    }
}

// Clear click event
LEAN_EXPORT lean_obj_res lean_afferent_window_clear_click(lean_obj_arg window_obj, lean_obj_arg world) {
    AfferentWindowRef window = (AfferentWindowRef)lean_get_external_data(window_obj);
    afferent_window_clear_click(window);
    return lean_io_result_mk_ok(lean_box(0));
}

// Pointer lock (for FPS camera)
LEAN_EXPORT lean_obj_res lean_afferent_window_set_pointer_lock(lean_obj_arg window_obj, uint8_t locked, lean_obj_arg world) {
    AfferentWindowRef window = (AfferentWindowRef)lean_get_external_data(window_obj);
    afferent_window_set_pointer_lock(window, locked != 0);
    return lean_io_result_mk_ok(lean_box(0));
}

LEAN_EXPORT lean_obj_res lean_afferent_window_get_pointer_lock(lean_obj_arg window_obj, lean_obj_arg world) {
    AfferentWindowRef window = (AfferentWindowRef)lean_get_external_data(window_obj);
    bool locked = afferent_window_get_pointer_lock(window);
    return lean_io_result_mk_ok(lean_box(locked ? 1 : 0));
}

LEAN_EXPORT lean_obj_res lean_afferent_window_get_mouse_delta(lean_obj_arg window_obj, lean_obj_arg world) {
    AfferentWindowRef window = (AfferentWindowRef)lean_get_external_data(window_obj);
    float dx, dy;
    afferent_window_get_mouse_delta(window, &dx, &dy);

    // Return (Float × Float) as Prod Float Float
    lean_object* result = lean_alloc_ctor(0, 2, 0);
    lean_ctor_set(result, 0, lean_box_float((double)dx));
    lean_ctor_set(result, 1, lean_box_float((double)dy));

    return lean_io_result_mk_ok(result);
}

// Key state (for continuous movement)
LEAN_EXPORT lean_obj_res lean_afferent_window_is_key_down(lean_obj_arg window_obj, uint16_t keyCode, lean_obj_arg world) {
    AfferentWindowRef window = (AfferentWindowRef)lean_get_external_data(window_obj);
    bool down = afferent_window_is_key_down(window, keyCode);
    return lean_io_result_mk_ok(lean_box(down ? 1 : 0));
}

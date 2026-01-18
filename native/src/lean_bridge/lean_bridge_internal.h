#ifndef AFFERENT_LEAN_BRIDGE_INTERNAL_H
#define AFFERENT_LEAN_BRIDGE_INTERNAL_H

#include <lean/lean.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "afferent.h"

// =============================================================================
// FFI struct layout note (IMPORTANT)
//
// Lean `structure`s that contain only scalar fields (e.g. `UInt8`, `UInt16`,
// `Float`) may be compiled to an "unboxed-scalar" object representation:
//   - 0 object fields
//   - a trailing byte blob containing the scalars at fixed offsets
//
// In that case you MUST NOT construct the value as a normal constructor with
// boxed fields (e.g. `lean_alloc_ctor(tag, 4, 0)` + `lean_ctor_set(..., box)`).
// Doing so will silently corrupt the value (classic symptom: garbage enums,
// x/y = 0, etc).
//
// How to confirm the layout:
//   1) Build once, then inspect the generated C for the Lean module that defines
//      the struct, e.g. `.lake/build/ir/Afferent/FFI/Metal.c`.
//   2) Search for the struct's `Inhabited` default initializer; it will show
//      `lean_alloc_ctor(tag, 0, <bytes>)` and a sequence of
//      `lean_ctor_set_float/uint16/uint8` calls with the exact offsets.
//   3) Mirror that layout here.
//
// Example:
//   `Afferent.FFI.ClickEvent` is compiled as 19 bytes of scalars, so we build it
//   with `lean_alloc_ctor(0, 0, 19)` and set fields by offset.
// =============================================================================

// External class registrations for opaque handles
extern lean_external_class* g_window_class;
extern lean_external_class* g_renderer_class;
extern lean_external_class* g_buffer_class;
extern lean_external_class* g_font_class;
extern lean_external_class* g_float_buffer_class;
extern lean_external_class* g_texture_class;
extern lean_external_class* g_cached_mesh_class;
extern lean_external_class* g_fragment_pipeline_class;

void afferent_ensure_initialized(void);

#endif

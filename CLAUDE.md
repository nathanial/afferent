# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Afferent is a Lean 4 2D vector graphics library targeting macOS with Metal GPU rendering. The goal is to provide an HTML5 Canvas-style API that looks as good as Skia and performs as well, without dependencies on external graphics libraries.

**Current Status:** Milestone 1 complete - Hello Triangle renders via Metal.

## Build Commands

**IMPORTANT:** Do not use `lake` directly. Use the provided shell scripts which set `LEAN_CC=/usr/bin/clang` for proper macOS framework linking (the bundled lld linker doesn't handle macOS frameworks).

```bash
# Build the project
./build.sh

# Build a specific target
./build.sh afferent
./build.sh hello_triangle

# Build and run
./run.sh                  # Runs afferent (default)
./run.sh hello_triangle   # Runs the example

# Run tests (not yet implemented)
./test.sh
```

## Architecture

```
┌─────────────────────────────────────┐
│   High-Level API (Canvas-like)      │  Pure Lean (TODO)
│   fillRect, stroke, bezierCurveTo   │
├─────────────────────────────────────┤
│   State Management (collimator)     │  Pure Lean (TODO)
│   save/restore, transforms          │
├─────────────────────────────────────┤
│   Path & Tessellation               │  Pure Lean (TODO)
│   Bezier flattening, triangulation  │
├─────────────────────────────────────┤
│   FFI Layer                         │  Lean - DONE
│   @[extern] bindings                │
├─────────────────────────────────────┤
│   Native Code                       │  Obj-C / C - DONE
│   Metal rendering, window mgmt      │
└─────────────────────────────────────┘
```

## Project Structure

```
afferent/
├── build.sh               # Build script (use instead of lake build)
├── run.sh                 # Build and run script
├── test.sh                # Test script
├── lakefile.lean          # Lake build configuration
├── lean-toolchain         # Lean version (v4.25.2)
├── Main.lean              # Main executable (collimator + graphics demo)
│
├── Afferent/
│   ├── Basic.lean         # Basic definitions
│   └── FFI/
│       └── Metal.lean     # FFI declarations (@[extern] bindings)
│
├── Examples/
│   └── HelloTriangle.lean # Minimal Metal triangle example
│
└── native/                # C/Objective-C native code
    ├── include/
    │   └── afferent.h     # C API header
    └── src/
        ├── window.m       # NSWindow + CAMetalLayer
        ├── metal_render.m # Metal device, pipeline, shaders
        └── lean_bridge.c  # Lean FFI entry points
```

## Key Files

### FFI Boundary

- **`Afferent/FFI/Metal.lean`** - Lean FFI declarations using `@[extern]` attribute
- **`native/src/lean_bridge.c`** - C functions that bridge Lean to native code
- **`native/include/afferent.h`** - C API header defining types and functions

### Native Rendering

- **`native/src/window.m`** - macOS window creation with NSWindow + CAMetalLayer
- **`native/src/metal_render.m`** - Metal device setup, shader compilation, rendering

### Build System

- **`lakefile.lean`** - Defines extern_lib for native code, framework linking

## Dependencies

- **collimator** - Profunctor optics library for Lean 4 (used for state management)
- **mathlib** - Transitive dependency from collimator

## FFI Pattern

Opaque handles are exposed to Lean using the NonemptyType pattern:

```lean
opaque WindowPointed : NonemptyType
def Window : Type := WindowPointed.type
instance : Nonempty Window := WindowPointed.property

@[extern "lean_afferent_window_create"]
opaque Window.create (width height : UInt32) (title : @& String) : IO Window
```

Corresponding C code registers external classes and manages memory:

```c
static lean_external_class* g_window_class = NULL;

LEAN_EXPORT lean_obj_res lean_afferent_window_create(...) {
    // Create native object, wrap in lean_alloc_external
}
```

## Planned Milestones

1. ✅ **Hello Triangle** - Prove FFI + Metal works
2. 🔲 **Basic Shapes** - Core types (Point, Color, Rect), rectangle rendering
3. 🔲 **Bezier Curves** - Path commands, curve flattening
4. 🔲 **Canvas State** - save/restore with collimator lenses
5. 🔲 **Stroke Rendering** - Stroked paths with line styles
6. 🔲 **Anti-Aliasing & Polish** - MSAA, gradients, text

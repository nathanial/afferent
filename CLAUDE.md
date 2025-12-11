# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Afferent is a Lean 4 2D vector graphics library targeting macOS with Metal GPU rendering. It provides an HTML5 Canvas-style API for drawing shapes, paths, gradients, and text with GPU acceleration.

**Current Status:** All 6 milestones complete - full 2D graphics library with shapes, curves, transforms, strokes, gradients, and text rendering.

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
```

## Architecture

```
┌─────────────────────────────────────┐
│   Canvas API                        │  Afferent/Canvas/Context.lean
│   fillRect, fillText, stroke, etc.  │
├─────────────────────────────────────┤
│   State Management (collimator)     │  Afferent/Canvas/State.lean
│   save/restore, transforms, styles  │
├─────────────────────────────────────┤
│   Core Types                        │  Afferent/Core/*.lean
│   Point, Color, Rect, Path, Paint   │
├─────────────────────────────────────┤
│   Tessellation                      │  Afferent/Render/Tessellation.lean
│   Bezier flattening, triangulation  │
├─────────────────────────────────────┤
│   Text Rendering                    │  Afferent/Text/Font.lean
│   Font loading, text measurement    │
├─────────────────────────────────────┤
│   FFI Layer                         │  Afferent/FFI/Metal.lean
│   @[extern] bindings                │
├─────────────────────────────────────┤
│   Native Code                       │  native/src/*.m, *.c
│   Metal rendering, FreeType, window │
└─────────────────────────────────────┘
```

## Project Structure

```
afferent/
├── build.sh               # Build script (use instead of lake build)
├── run.sh                 # Build and run script
├── lakefile.lean          # Lake build configuration
├── lean-toolchain         # Lean version (v4.25.2)
├── Main.lean              # Demo application with all features
│
├── Afferent.lean          # Library root (imports all modules)
├── Afferent/
│   ├── Core/
│   │   ├── Types.lean     # Point, Color, Rect
│   │   ├── Path.lean      # PathCommand, Path builder
│   │   ├── Transform.lean # 2D transformation matrix
│   │   └── Paint.lean     # FillStyle, Gradient, StrokeStyle
│   ├── Render/
│   │   └── Tessellation.lean  # Path to triangles conversion
│   ├── Canvas/
│   │   ├── State.lean     # CanvasState with collimator lenses
│   │   └── Context.lean   # DrawContext and Canvas API
│   ├── Text/
│   │   └── Font.lean      # Font loading and text measurement
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
        ├── metal_render.m # Metal device, pipeline, shaders, 4x MSAA
        ├── text_render.c  # FreeType font loading and glyph rasterization
        └── lean_bridge.c  # Lean FFI entry points
```

## Key Modules

### Core Types (`Afferent/Core/`)

- **Types.lean** - `Point`, `Color` (with named colors), `Rect`
- **Path.lean** - `PathCommand` (moveTo, lineTo, bezierTo, arc, close), `Path` builder
- **Transform.lean** - 2D affine transformation matrix with translate/rotate/scale
- **Paint.lean** - `FillStyle` (solid/gradient), `Gradient` (linear/radial), `StrokeStyle`

### Canvas API (`Afferent/Canvas/`)

- **State.lean** - `CanvasState` with collimator lenses for functional state management
- **Context.lean** - `DrawContext` (low-level) and `Canvas` monad (high-level API)

### Rendering

- **Tessellation.lean** - Converts paths to triangles, samples gradients per-vertex
- **Font.lean** - Wraps FreeType for font loading, metrics, and text measurement

### FFI (`Afferent/FFI/Metal.lean`)

Opaque handles using the NonemptyType pattern:

```lean
opaque WindowPointed : NonemptyType
def Window : Type := WindowPointed.type

@[extern "lean_afferent_window_create"]
opaque Window.create (width height : UInt32) (title : @& String) : IO Window
```

### Native Code (`native/src/`)

- **window.m** - macOS window with NSWindow + CAMetalLayer
- **metal_render.m** - Metal pipeline with 4x MSAA, text shader, alpha blending
- **text_render.c** - FreeType integration with glyph caching and texture atlas
- **lean_bridge.c** - FFI entry points bridging Lean to native code

## Dependencies

- **collimator** - Profunctor optics library for Lean 4 (state management)
- **FreeType** - Font rendering library (installed via Homebrew: `brew install freetype`)
- **Metal/Cocoa** - macOS frameworks for GPU rendering and windowing

## FFI Notes

### Returning Float Tuples

When returning `Float × Float × Float` from C to Lean, use nested `Prod` structures with boxed floats:

```c
// Float × Float × Float = Prod Float (Prod Float Float)
lean_object* inner = lean_alloc_ctor(0, 2, 0);
lean_ctor_set(inner, 0, lean_box_float(val2));
lean_ctor_set(inner, 1, lean_box_float(val3));

lean_object* outer = lean_alloc_ctor(0, 2, 0);
lean_ctor_set(outer, 0, lean_box_float(val1));
lean_ctor_set(outer, 1, inner);
```

### External Classes

Native handles use `lean_alloc_external` with registered classes:

```c
static lean_external_class* g_font_class = NULL;
// In init: g_font_class = lean_register_external_class(finalizer, NULL);
// Usage: lean_alloc_external(g_font_class, native_ptr);
```

## Completed Milestones

1. ✅ **Hello Triangle** - Metal FFI working
2. ✅ **Basic Shapes** - Rectangles, circles, polygons
3. ✅ **Bezier Curves** - Arcs, stars, hearts, custom paths
4. ✅ **Canvas State** - Save/restore, transforms with collimator lenses
5. ✅ **Stroke Rendering** - Path outlines with lineWidth/lineCap/lineJoin
6. ✅ **Anti-Aliasing & Polish** - 4x MSAA, linear/radial gradients, FreeType text

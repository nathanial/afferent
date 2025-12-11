# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Afferent is a Lean 4 2D vector graphics library and UI framework targeting macOS with Metal GPU rendering. It provides an HTML5 Canvas-style API for drawing shapes, paths, gradients, and text with GPU acceleration, plus an immediate-mode widget framework for building interactive applications.

**Current Status:** Graphics engine complete (M1-M6). UI framework in progress with core widgets implemented (M7-M8).

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
├── Main.lean              # UI widget demo application
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
│   ├── UI/                # Immediate-mode widget framework
│   │   ├── Input.lean     # MouseButton, InputState, event queries
│   │   ├── Context.lean   # UIContext, Style defaults, widget state
│   │   └── Widgets.lean   # button, label, checkbox, textBox, slider
│   └── FFI/
│       └── Metal.lean     # FFI declarations (@[extern] bindings)
│
├── Examples/
│   ├── HelloTriangle.lean # Minimal Metal triangle example
│   └── VisualDemo.lean    # Graphics demo (shapes, gradients, text)
│
└── native/                # C/Objective-C native code
    ├── include/
    │   └── afferent.h     # C API header
    └── src/
        ├── window.m       # NSWindow + CAMetalLayer + input handling
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

### UI Framework (`Afferent/UI/`)

Immediate-mode widget system (Dear ImGui-style):

- **Input.lean** - `MouseButton` enum, `InputState` struct, `InputState.query` for polling native events
- **Context.lean** - `UIContext` with hot/active widget tracking, text edit buffer, style constants
- **Widgets.lean** - Core widgets:
  - `button` - Click detection with hover/active states
  - `label`, `labelColored` - Text display
  - `checkbox`, `checkboxLabeled` - Toggle with optional label
  - `textBox` - Editable text input with cursor
  - `slider` - Horizontal value slider with drag support

Widget pattern: Each widget takes `UIContext` and returns `IO (Result × UIContext)` where Result is the interaction outcome (clicked, new value, etc.).

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

## Milestones

### Graphics Engine (Complete)

1. ✅ **M1: Hello Triangle** - Metal FFI working
2. ✅ **M2: Basic Shapes** - Rectangles, circles, polygons
3. ✅ **M3: Bezier Curves** - Arcs, stars, hearts, custom paths
4. ✅ **M4: Canvas State** - Save/restore, transforms with collimator lenses
5. ✅ **M5: Stroke Rendering** - Path outlines with lineWidth/lineCap/lineJoin
6. ✅ **M6: Anti-Aliasing & Polish** - 4x MSAA, linear/radial gradients, FreeType text

### UI Framework

7. ✅ **M7: Input System** - Native event capture in window.m, FFI bridge, InputState queries
8. ✅ **M8: Core Widgets** - button, label, checkbox, textBox, slider with immediate-mode pattern
9. ⬜ **M9: Layout System** - Automatic widget positioning, horizontal/vertical containers
10. ⬜ **M10: Advanced Widgets** - Dropdowns, tabs, radio buttons, scroll views
11. ⬜ **M11: Theming** - Configurable styles, dark/light themes
12. ⬜ **M12: Accessibility** - Keyboard navigation, focus management

## Roadmap

Current focus: Building a complete immediate-mode UI framework

**Next up (M9: Layout System):**
- `HStack` / `VStack` containers for automatic positioning
- Spacing and padding configuration
- Size constraints (min/max width/height)

**Future work:**
- More widgets: dropdowns, tabs, radio buttons, progress bars, tooltips
- Scroll views with virtual rendering for large content
- Theming system with customizable color schemes
- Keyboard navigation and tab focus
- Smooth animations for hover/active state transitions

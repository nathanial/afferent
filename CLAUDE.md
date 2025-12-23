# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Afferent is a Lean 4 2D/3D graphics and UI framework targeting macOS with Metal GPU rendering. It provides:
- HTML5 Canvas-style 2D API for shapes, paths, gradients, and text
- 3D rendering with perspective projection, lighting, fog, and procedural effects
- Declarative widget system with Elm-style architecture
- CSS-style layout engine (Flexbox and Grid)
- High-performance rendering via GPU instancing and zero-copy buffers

## Build Commands

**IMPORTANT:** Do not use `lake` directly. Use the provided shell scripts which set `LEAN_CC=/usr/bin/clang` for proper macOS framework linking (the bundled lld linker doesn't handle macOS frameworks).

```bash
# Build the project
./build.sh

# Build a specific target
./build.sh afferent
./build.sh hello_triangle
./build.sh spinning_cubes

# Build and run
./run.sh                  # Runs afferent (default)
./run.sh hello_triangle   # Runs the example

# Run tests
./test.sh
```

## Architecture

```
┌──────────────────────────────────────┐
│   Lean 4 Application                 │  Main.lean, Demos/, Examples/
│   (demos, examples, user code)       │
├──────────────────────────────────────┤
│   Widget System                      │  Afferent/Widget/*.lean
│   declarative UI, events, Elm arch   │
├──────────────────────────────────────┤
│   Layout Engine                      │  Afferent/Layout/*.lean
│   CSS Flexbox, Grid, constraints     │
├──────────────────────────────────────┤
│   Canvas API                         │  Afferent/Canvas/*.lean
│   2D drawing, state, transforms      │
├──────────────────────────────────────┤
│   3D Rendering                       │  Afferent/Render/*.lean
│   Matrix4, FPSCamera, Mesh, Dynamic  │
├──────────────────────────────────────┤
│   Core Types                         │  Afferent/Core/*.lean
│   Point, Color, Rect, Path, Paint    │
├──────────────────────────────────────┤
│   Text Rendering                     │  Afferent/Text/Font.lean
│   FreeType fonts, measurement        │
├──────────────────────────────────────┤
│   FFI Layer                          │  Afferent/FFI/*.lean
│   Window, Renderer, Texture, 3D      │
├──────────────────────────────────────┤
│   Native Code                        │  native/src/
│   Metal pipeline, FreeType           │
└──────────────────────────────────────┘
```

## Project Structure

```
afferent/
├── build.sh               # Build script (use instead of lake build)
├── run.sh                 # Build and run script
├── test.sh                # Test runner
├── lakefile.lean          # Lake build configuration
├── lean-toolchain         # Lean version (v4.25.2)
├── Main.lean              # Main demo entry point
│
├── Afferent.lean          # Library root (imports all modules)
├── Afferent/
│   ├── Core/
│   │   ├── Types.lean     # Point, Size, Rect
│   │   ├── Color.lean     # Color, named colors, HSV conversion
│   │   ├── Path.lean      # PathCommand, Path builder, arcs
│   │   ├── Transform.lean # 2D affine transformation matrix
│   │   └── Paint.lean     # FillStyle, Gradient, StrokeStyle
│   ├── Render/
│   │   ├── Tessellation.lean  # Path to triangles conversion
│   │   ├── Matrix4.lean       # 4x4 matrices for 3D graphics
│   │   ├── Mesh.lean          # Pre-defined meshes (cube)
│   │   ├── FPSCamera.lean     # First-person camera controller
│   │   └── Dynamic.lean       # High-perf dynamic rendering
│   ├── Canvas/
│   │   ├── State.lean     # CanvasState with collimator lenses
│   │   └── Context.lean   # DrawContext and Canvas monad
│   ├── Widget/
│   │   ├── Core.lean      # Widget type, TextLayout, BoxStyle
│   │   ├── UI.lean        # High-level widget rendering
│   │   ├── Measure.lean   # Widget measurement & intrinsic sizing
│   │   ├── Render.lean    # Widget tree rendering
│   │   ├── DSL.lean       # Builder monad for widget construction
│   │   ├── Event.lean     # Event types, modifiers, mouse handling
│   │   ├── HitTest.lean   # Point-in-widget testing
│   │   ├── App.lean       # Elm-style message passing architecture
│   │   ├── Interactive.lean # Interactive widget container
│   │   ├── Scroll.lean    # Scrolling container
│   │   └── TextLayout.lean # Text wrapping, line breaking
│   ├── Layout/
│   │   ├── Types.lean     # Dimension, EdgeInsets, BoxConstraints
│   │   ├── Flex.lean      # Flexbox properties
│   │   ├── Grid.lean      # CSS Grid layout
│   │   ├── Node.lean      # Layout node tree
│   │   ├── Algorithm.lean # Main layout algorithm
│   │   └── Result.lean    # Layout computation results
│   ├── Text/
│   │   └── Font.lean      # Font loading and text measurement
│   └── FFI/
│       ├── Types.lean     # Opaque handles (Window, Renderer, etc)
│       ├── Window.lean    # Window management & input
│       ├── Renderer.lean  # 2D rendering operations
│       ├── Renderer3D.lean # 3D rendering (meshes, fog, ocean)
│       ├── Text.lean      # Text FFI bindings
│       ├── FloatBuffer.lean # High-perf mutable float arrays
│       └── Texture.lean   # Texture loading & sprites
│
├── Demos/                 # 20+ demo applications
│   ├── Runner.lean        # Multi-pane demo runner
│   ├── Seascape.lean      # 3D ocean with Gerstner waves
│   ├── SpinningCubes.lean # 3D cube grid
│   ├── Widgets.lean       # Widget system showcase
│   ├── Layout.lean        # Layout algorithm demo
│   └── ...                # Many more demos
│
├── Examples/
│   ├── HelloTriangle.lean # Minimal Metal triangle
│   └── SpinningCubes.lean # Standalone 3D example
│
├── Afferent/Tests/        # Test suite
│   ├── TessellationTests.lean
│   ├── LayoutTests.lean
│   ├── WidgetTests.lean
│   └── ...
│
└── native/                # C/Objective-C/C++ native code
    ├── include/
    │   └── afferent.h     # C API header
    └── src/
        ├── lean_bridge.c      # Lean FFI entry points
        ├── texture.c          # stb_image texture loading
        ├── metal/
        │   ├── window.m       # NSWindow + CAMetalLayer
        │   ├── render.m       # Core Metal renderer
        │   ├── pipeline.m     # Metal pipeline & shaders
        │   ├── draw_2d.m      # 2D shape rendering
        │   ├── draw_3d.m      # 3D mesh rendering
        │   ├── draw_animated.m # GPU-side animations
        │   ├── draw_sprites.m # Sprite rendering
        │   ├── draw_text.m    # Text rendering
        │   └── shaders/       # Metal Shading Language files
        └── common/
            ├── text_render.c  # FreeType integration
            └── float_buffer.c # FloatBuffer implementation
```

## Key Modules

### Core Types (`Afferent/Core/`)

- **Types.lean** - `Point`, `Size`, `Rect` with arithmetic operations
- **Color.lean** - RGBA colors, named colors, HSV conversion
- **Path.lean** - `PathCommand` (moveTo, lineTo, bezierTo, arc, close), `Path` builder
- **Transform.lean** - 2D affine transformation matrix
- **Paint.lean** - `FillStyle` (solid/gradient), `Gradient` (linear/radial), `StrokeStyle`

### 3D Rendering (`Afferent/Render/`)

- **Matrix4.lean** - 4x4 matrix operations (perspective, look-at, multiply, transform)
- **FPSCamera.lean** - First-person camera with yaw/pitch, WASD movement, mouse look
- **Mesh.lean** - Pre-defined meshes (cube vertices + indices)
- **Dynamic.lean** - High-performance particle systems with CPU positions, GPU colors
- **Tessellation.lean** - Path to triangle conversion, gradient sampling

### Canvas API (`Afferent/Canvas/`)

- **State.lean** - `CanvasState` with collimator lenses for functional state
- **Context.lean** - `DrawContext` (low-level) and `Canvas` monad (high-level)

### Widget System (`Afferent/Widget/`)

- **Core.lean** - Widget type (Text, Box, Row, Column, Grid, Scroll, Interactive)
- **UI.lean** - `renderUI`, `renderBuilder` for widget rendering
- **Measure.lean** - Widget measurement and intrinsic sizing
- **App.lean** - Elm architecture (init, update, view, message passing)
- **Event.lean** - Event types (mouse, keyboard, scroll)
- **HitTest.lean** - Point-in-widget collision detection

### Layout System (`Afferent/Layout/`)

- **Flex.lean** - CSS Flexbox (direction, wrap, justify, align, gap)
- **Grid.lean** - CSS Grid (columns, template areas)
- **Algorithm.lean** - Constraint-based layout computation
- **Types.lean** - Dimension (auto, length, percent), EdgeInsets, BoxConstraints

### FFI (`Afferent/FFI/`)

Opaque handles using the NonemptyType pattern:

```lean
opaque WindowPointed : NonemptyType
def Window : Type := WindowPointed.type

@[extern "lean_afferent_window_create"]
opaque Window.create (width height : UInt32) (title : @& String) : IO Window
```

Key FFI modules:
- **Window.lean** - Window creation, input (keyboard, mouse, scroll, modifiers)
- **Renderer.lean** - Frame management, 2D drawing, instanced rendering
- **Renderer3D.lean** - 3D meshes, fog, procedural ocean
- **FloatBuffer.lean** - C-allocated mutable arrays (avoids Lean copy-on-write)
- **Texture.lean** - Texture loading, sprite rendering

## Dependencies

- **collimator** - Profunctor optics library for Lean 4 (state management)
- **FreeType** - Font rendering (Homebrew: `brew install freetype`)
- **Assimptor** - Assimp 3D model loading wrapper (see `../assimptor`)
- **Metal/Cocoa/QuartzCore** - macOS frameworks for GPU rendering

## FFI Notes

### Returning Float Tuples

When returning `Float × Float × Float` from C to Lean, use nested `Prod` structures:

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

### Struct Layout

When adding Lean `structure`s that cross FFI:
- Structures with only scalar fields use **unboxed-scalar** layout
- Use `lean_alloc_ctor(tag, 0, <bytes>)` and `lean_ctor_set_float/uint16/uint8`
- Check generated C in `.lake/build/ir/` for exact offsets

## Performance Patterns

### FloatBuffer
C-allocated mutable arrays that avoid Lean's copy-on-write:
```lean
let buf ← FloatBuffer.create 10000
buf.setVec5 index x y size rotation alpha
renderer.drawSpritesFromBuffer texture buf count
```

### Dynamic Rendering
CPU provides positions, GPU computes colors and coordinates:
```lean
renderer.drawDynamicCircles floatBuffer count screenWidth screenHeight
```

### Animated Rendering
Upload static data once, GPU animates with time uniform:
```lean
let buf ← renderer.createAnimatedBuffer staticData
renderer.drawAnimatedCircles buf count time screenWidth screenHeight
```

### Instanced Rendering
Draw millions of shapes via GPU instancing:
```lean
renderer.drawInstancedCircles instanceBuffer count screenWidth screenHeight
```

## Testing

Run tests with `./test.sh`. Test files in `Afferent/Tests/`:
- TessellationTests - Path flattening, triangulation
- LayoutTests - Flexbox, Grid, constraints
- WidgetTests - Widget measurement, events
- AssetLoadingTests - 3D model loading
- FFISafetyTests - FFI struct layout

## Development Tips

- Use `./run.sh` to test the app after changes
- Check `Demos/` for usage examples of all features
- The Seascape demo (`Demos/Seascape.lean`) showcases advanced 3D with ocean simulation
- Widget system demos in `Demos/Widgets.lean` and `Demos/Interactive.lean`
- Performance benchmarks in `Demos/*Perf.lean`

# Afferent

A 2D/3D graphics and UI framework for Lean 4, powered by Metal GPU rendering on macOS.

## Features

### 2D Graphics
- **Hardware-accelerated rendering** via Metal with 4x MSAA anti-aliasing
- **Canvas API** with save/restore state management (HTML5 Canvas-style)
- **Basic shapes**: rectangles, circles, ellipses, rounded rectangles, polygons
- **Paths**: lines, quadratic/cubic Bezier curves, arcs
- **Stroke rendering**: configurable line width, caps (butt, round, square), joins (miter, round, bevel)
- **Gradient fills**: linear and radial gradients with multiple color stops
- **Text rendering**: FreeType-based font loading with glyph caching and texture atlas
- **Transforms**: translate, rotate, scale with matrix composition

### 3D Graphics
- **Perspective projection** with configurable FOV, aspect ratio, near/far planes
- **3D mesh rendering** with per-fragment lighting and depth testing
- **Fog effects** with linear distance-based blending
- **Procedural ocean** with Gerstner wave simulation (GPU-computed)
- **FPS camera** controller with mouse look and WASD movement
- **Asset loading** via Assimp (FBX, OBJ, COLLADA, and more)

### Widget System
- **Declarative widgets**: Text, Box, Row, Column, Grid, Scroll, Interactive
- **Elm-style architecture** with message passing for interactive apps
- **Event handling**: mouse events, keyboard input, hit testing, event bubbling
- **Box styling**: background color, border, radius, padding, margin

### Layout System
- **CSS Flexbox**: direction, wrap, justify-content, align-items, align-content, gap
- **CSS Grid**: columns, template areas, row/column sizing
- **Dimension types**: auto, length, percent, min-content, max-content
- **Box model**: padding, margin, border, corner radius

### High-Performance Rendering
- **Instanced rendering**: 50M+ shapes per frame via GPU instancing
- **Dynamic rendering**: CPU positions with GPU color/coordinate conversion
- **Animated rendering**: static GPU upload with per-frame time updates only
- **Sprite system**: texture sprites with physics (Bunnymark-style benchmarks)
- **FloatBuffer**: C-allocated mutable arrays for zero-copy GPU uploads

## Requirements

- macOS with Metal support (10.13+)
- [Lean 4](https://lean-lang.org/) (v4.25.0+)
- [Homebrew](https://brew.sh/) for dependencies
- CMake (for building Assimp)

### Dependencies

```bash
brew install freetype cmake
```

Note: Assimp is included as a git submodule and built from source automatically.

## Building

```bash
# Clone with submodules
git clone --recurse-submodules <repo-url>
cd afferent

# Build (automatically initializes submodules and builds Assimp on first run)
./build.sh

# Run the demo
./run.sh
```

If you've already cloned without `--recurse-submodules`, the build script will automatically initialize and build the Assimp submodule on first run.

**Note**: Use `./build.sh` instead of `lake build` directly. The build script:
- Sets `LEAN_CC=/usr/bin/clang` for proper macOS framework linking
- Initializes git submodules if needed
- Builds Assimp from source on first run

## Usage

### Canvas API (2D)

```lean
import Afferent

def myDrawing : Canvas Unit := do
  -- Set fill color and draw rectangle
  Canvas.setFillColor (Color.rgb 0.2 0.4 0.8)
  Canvas.fillRect 50 50 200 100

  -- Draw with transforms
  Canvas.save
  Canvas.translate 400 300
  Canvas.rotate (Float.pi / 4)
  Canvas.setFillColor Color.red
  Canvas.fillRect (-50) (-50) 100 100
  Canvas.restore

  -- Draw a circle with gradient
  let gradient := Gradient.radial (Point.mk 600 200) 80
    #[GradientStop.mk 0.0 Color.white, GradientStop.mk 1.0 Color.blue]
  Canvas.setFillStyle (FillStyle.gradient gradient)
  Canvas.fillCircle 600 200 80
```

### Widget System

```lean
import Afferent.Widget

def myUI : Widget :=
  Widget.column [
    Widget.text "Hello, Afferent!" |>.withStyle { fontSize := 24 },
    Widget.row [
      Widget.box { background := Color.red, padding := EdgeInsets.all 10 }
        (Widget.text "Button 1"),
      Widget.box { background := Color.blue, padding := EdgeInsets.all 10 }
        (Widget.text "Button 2")
    ]
  ]
```

### 3D Rendering

```lean
import Afferent.Render.Matrix4
import Afferent.Render.FPSCamera

-- Set up perspective projection
let projection := Matrix4.perspective (Float.pi / 4) aspectRatio 0.1 1000.0
let view := camera.viewMatrix
let mvp := projection * view * modelMatrix

-- Render 3D mesh with fog
renderer.drawMesh3D vertices indices mvp modelMatrix lightDir ambientFactor
  cameraPos fogColor fogStart fogEnd
```

### Layout System

```lean
import Afferent.Layout

-- Flexbox layout
let container := LayoutNode.flex
  { direction := .row, justify := .spaceBetween, gap := 10 }
  [child1, child2, child3]

-- Compute layout
let result := Layout.compute container (BoxConstraints.tight 800 600)
```

## Architecture

```
┌──────────────────────────────────────┐
│   Lean 4 Application                 │
│   (Main.lean, Demos/, Examples/)     │
├──────────────────────────────────────┤
│   High-Level APIs                    │
│  ├─ Canvas Monad (HTML5 Canvas-style)│
│  ├─ Widget System (declarative UI)   │
│  ├─ Layout System (CSS Flexbox/Grid) │
│  └─ 3D Rendering (perspective, fog)  │
├──────────────────────────────────────┤
│   Core Types & Rendering             │
│  ├─ Point, Color, Rect, Size, Path   │
│  ├─ Transform (2D), Matrix4 (3D)     │
│  ├─ Tessellation (paths → triangles) │
│  └─ FPSCamera, Mesh, Paint           │
├──────────────────────────────────────┤
│   FFI Layer (@[extern] bindings)     │
│  ├─ Window, Renderer, Buffer         │
│  ├─ FloatBuffer, Texture, Font       │
│  └─ Asset loading (Assimp)           │
├──────────────────────────────────────┤
│   Native Code (C/Obj-C/C++)          │
│  ├─ Metal rendering pipeline         │
│  ├─ FreeType text rendering          │
│  └─ Assimp model loading             │
├──────────────────────────────────────┤
│   Metal GPU (shaders)                │
│  ├─ 2D/3D rendering with MSAA        │
│  ├─ Instanced rendering              │
│  └─ Procedural effects (ocean, etc)  │
└──────────────────────────────────────┘
```

## Project Structure

```
afferent/
├── Afferent/
│   ├── Core/           # Point, Color, Rect, Path, Transform, Paint
│   ├── Render/         # Tessellation, Matrix4, Mesh, FPSCamera, Dynamic
│   ├── Canvas/         # Canvas monad, state management
│   ├── Widget/         # Declarative UI system, events, hit testing
│   ├── Layout/         # CSS Flexbox and Grid layout engine
│   ├── Text/           # Font loading and text measurement
│   └── FFI/            # Lean FFI bindings (Window, Renderer, Texture, Asset)
├── Demos/              # 20+ demo applications
│   ├── Runner.lean     # Multi-pane demo runner
│   ├── Seascape.lean   # 3D ocean with Gerstner waves
│   ├── Widgets.lean    # Widget system showcase
│   ├── Layout.lean     # Layout algorithm demo
│   └── ...             # Shapes, Gradients, Text, Animations, etc.
├── Examples/
│   ├── HelloTriangle.lean   # Minimal Metal example
│   └── SpinningCubes.lean   # 3D cube rendering
├── native/
│   ├── src/metal/      # Metal renderer, shaders, pipeline
│   ├── src/common/     # FreeType, FloatBuffer, Assimp loader
│   └── include/        # C headers
├── build.sh            # Build script (use instead of lake build)
├── run.sh              # Build and run script
└── test.sh             # Run test suite
```

## Demos

Run demos with `./run.sh`:

| Demo | Description |
|------|-------------|
| Seascape | 3D ocean with Gerstner waves and FPS camera |
| SpinningCubes | Grid of rotating 3D cubes |
| Widgets | Widget system showcase |
| Layout | Flexbox and Grid layout demos |
| Shapes | 2D shapes (rect, circle, polygon) |
| Gradients | Linear and radial gradients |
| Strokes | Stroke rendering styles |
| Text | Text rendering at various sizes |
| Transforms | Transform compositions |
| Animations | GPU-side animation showcase |
| CirclesPerf | 1M+ bouncing circles benchmark |

## Testing

```bash
./test.sh
```

Tests cover tessellation, layout algorithms, widget measurement, asset loading, and FFI safety.

## License

MIT License - see [LICENSE](LICENSE) for details.

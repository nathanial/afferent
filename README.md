# Afferent

A 2D vector graphics library for Lean 4, powered by Metal GPU rendering on macOS.

## Features

- **Hardware-accelerated rendering** via Metal with 4x MSAA anti-aliasing
- **Canvas API** with save/restore state management (HTML5 Canvas-style)
- **Basic shapes**: rectangles, circles, ellipses, rounded rectangles, polygons
- **Paths**: lines, quadratic/cubic Bezier curves, arcs
- **Stroke rendering**: configurable line width, line cap styles (butt, round, square), line join styles (miter, round, bevel)
- **Gradient fills**: linear and radial gradients with multiple color stops
- **Text rendering**: FreeType-based font loading with glyph caching and texture atlas
- **Transforms**: translate, rotate, scale with matrix composition
- **Collimator integration**: proof-carrying coordinates via [collimator](https://github.com/lean-machines/collimator)

## Requirements

- macOS with Metal support
- [Lean 4](https://lean-lang.org/) (v4.25.0+)
- [Homebrew](https://brew.sh/) for dependencies

### Dependencies (installed via Homebrew)

```bash
brew install freetype
```

## Building

```bash
# Clone and build
git clone <repo-url>
cd afferent
./build.sh

# Run the demo
./run.sh
```

**Note**: Use `./build.sh` instead of `lake build` directly. The build script sets `LEAN_CC=/usr/bin/clang` which is required for proper macOS framework linking.

## Usage

### Basic Drawing

```lean
import Afferent

def main : IO Unit := do
  -- Create window and renderer
  let window ← Afferent.Window.create 800 600 "My App"
  let renderer ← Afferent.Renderer.create window

  -- Main loop
  while not (← window.shouldClose) do
    window.pollEvents
    if ← renderer.beginFrame 0.1 0.1 0.1 1.0 then
      -- Draw a red rectangle
      let vertices := Afferent.Shapes.rectangle (-0.5) (-0.5) 1.0 1.0
        |>.map (Afferent.Vertex.withColor 1.0 0.0 0.0 1.0)
      let indices := #[0, 1, 2, 0, 2, 3]
      let vb ← Afferent.Buffer.createVertex renderer vertices
      let ib ← Afferent.Buffer.createIndex renderer indices
      renderer.drawTriangles vb ib indices.size.toUInt32
      vb.destroy
      ib.destroy
      renderer.endFrame

  renderer.destroy
  window.destroy
```

### Canvas API (Recommended)

The Canvas monad provides a higher-level drawing API with state management:

```lean
import Afferent
import Afferent.Canvas

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

  -- Draw a circle
  Canvas.setFillColor (Color.hsva 0.396 1.0 0.8 0.8)  -- green with alpha
  Canvas.fillCircle 600 200 80

def main : IO Unit := do
  Afferent.runApp 800 600 "Canvas Demo" fun renderer => do
    Canvas.render renderer 800 600 myDrawing
```

### Gradients

```lean
-- Linear gradient
let gradient := Gradient.linear
  (Point.mk 0 0) (Point.mk 200 0)
  #[GradientStop.mk 0.0 Color.red, GradientStop.mk 1.0 Color.blue]
Canvas.setFillStyle (FillStyle.gradient gradient)
Canvas.fillRect 50 50 200 100

-- Radial gradient
let radial := Gradient.radial
  (Point.mk 100 100) 80
  #[GradientStop.mk 0.0 Color.white, GradientStop.mk 1.0 Color.black]
Canvas.setFillStyle (FillStyle.gradient radial)
Canvas.fillCircle 100 100 80
```

### Path Drawing

```lean
-- Custom path with curves
Canvas.beginPath
Canvas.moveTo 100 100
Canvas.lineTo 200 100
Canvas.quadraticCurveTo 250 150 200 200
Canvas.bezierCurveTo 150 250 100 200 100 150
Canvas.closePath
Canvas.fill
```

### Stroke Rendering

```lean
Canvas.setStrokeColor Color.white
Canvas.setLineWidth 4.0
Canvas.setLineCap LineCap.round
Canvas.setLineJoin LineJoin.round
Canvas.beginPath
Canvas.moveTo 100 100
Canvas.lineTo 200 150
Canvas.lineTo 150 250
Canvas.stroke
```

### Text Rendering

```lean
-- Load a font
let font ← Afferent.Font.load "/System/Library/Fonts/Helvetica.ttc" 24

-- Render text
Canvas.setFillColor Color.white
Canvas.fillText font "Hello, Afferent!" 100 100

-- Get text metrics
let (width, height) ← font.measureText "Hello"
```

## Architecture

```
Lean 4 Application
        |
   Canvas Monad (state management, transforms)
        |
   Tessellation (paths -> triangles)
        |
   FFI Bridge (lean_bridge.c)
        |
   Native Layer (metal_render.m, text_render.c)
        |
   Metal GPU
```

## Project Structure

```
afferent/
├── Afferent/
│   ├── Core/           # Point, Color, Vertex, Paint types
│   ├── FFI/            # Lean FFI bindings to native code
│   ├── Canvas/         # Canvas monad, state, context
│   ├── Render/         # Tessellation, shapes
│   └── Text/           # Font wrapper
├── native/
│   ├── include/        # C headers (afferent.h)
│   └── src/            # Metal renderer, FreeType text, FFI bridge
├── Main.lean           # Demo application
├── lakefile.lean       # Lake build configuration
├── build.sh            # Build script
└── run.sh              # Run script
```

## Milestones

- [x] **M1: Hello Triangle** - Metal FFI, basic rendering pipeline
- [x] **M2: Basic Shapes** - Rectangles, circles, polygons, tessellation
- [x] **M3: Bezier Curves** - Quadratic/cubic curves, arcs, complex paths
- [x] **M4: Canvas State** - Save/restore, transforms, Canvas monad
- [x] **M5: Stroke Rendering** - Path outlines, line width/cap/join
- [x] **M6: Polish** - MSAA, gradients, text rendering

## License

MIT

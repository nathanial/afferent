# Afferent Roadmap

This document tracks improvement opportunities, feature proposals, and code cleanup tasks for the Afferent graphics framework.

---

## Feature Proposals

### [Priority: High] Pattern and Image Fills

**Description:** Add support for pattern/texture fills in addition to solid colors and gradients. The FillStyle enum already has a commented placeholder for `pattern (p : Pattern)`.

**Rationale:** Pattern fills are essential for many graphics applications (tiled backgrounds, hatching, textures). The infrastructure exists but the feature is not implemented.

**Affected Files:**
- `Afferent/Core/Paint.lean` (FillStyle enum)
- `Afferent/Render/Tessellation.lean` (sampleFillStyle, vertex UV generation)
- `native/src/metal/` (shader support for texture sampling in 2D pipeline)

**Estimated Effort:** Medium

**Dependencies:** Requires UV coordinate generation in tessellation and texture binding in the 2D rendering pipeline.

---

### [Priority: High] ~~Round Line Caps and Joins~~ ✅ COMPLETED

**Status:** Completed - added arc geometry generation for round caps and joins.

**Resolution:** Round line caps and joins now properly generate arc geometry:
- Added `generateArcPoints` helper for arc tessellation
- Round joins generate arcs on the outside of turns
- Round start/end caps generate semicircular arcs
- Uses 8 segments per arc for smooth curves

**Affected Files:**
- `Afferent/Render/Tessellation.lean` (expandPolylineToStroke function)

---

### [Priority: High] PBR Material Support for 3D

**Description:** Extend the 3D asset loading pipeline to support full PBR (Physically Based Rendering) materials including normal maps, metallic, and roughness textures.

**Rationale:** Modern 3D content uses PBR workflows. The current system only loads diffuse textures.

**Affected Files:**
- `assimptor` package (SubMesh structure, asset loading) - now a separate dependency
- `native/src/metal/` (shader updates for PBR in Afferent)

**Estimated Effort:** Large

**Dependencies:** Shader modifications, additional texture slots. Requires coordination with assimptor package.

---

### [Priority: Medium] ~~Dashed and Dotted Lines~~ ✅ COMPLETED

**Status:** Completed - added `DashPattern` structure and stroke segmentation.

**Resolution:** Dashed and dotted lines are now fully supported:
```lean
-- Simple dashed line
setDashed 10 5  -- 10px dash, 5px gap
drawLine p1 p2

-- Dotted line (uses round caps for circular dots)
setDotted
drawLine p1 p2

-- Custom pattern: dash-dot
setDashPattern (some ⟨#[15, 5, 3, 5], 0⟩)

-- Back to solid
setSolid
```

Features:
- `DashPattern` structure with segments array and phase offset
- Arc length utilities for path segmentation
- Segmentation integrated with `tessellateStroke` and `tessellateStrokeNDC`
- CanvasM accessors: `setDashed`, `setDotted`, `setSolid`, `setDashPattern`
- Also added: `setLineCap`, `setLineJoin` accessors

**Affected Files:**
- `Afferent/Core/Paint.lean`
- `Afferent/Render/Tessellation.lean`
- `Afferent/Canvas/State.lean`
- `Afferent/Canvas/Context.lean`

---

### [Priority: Medium] Shadow and Glow Effects

**Description:** Add shadow/glow capabilities to the Canvas API, similar to HTML5 Canvas shadowBlur, shadowColor, shadowOffsetX/Y.

**Rationale:** Shadows and glows are essential for modern UI design, depth perception, and visual effects.

**Affected Files:**
- `Afferent/Canvas/State.lean` (CanvasState structure)
- `Afferent/Canvas/Context.lean` (shadow rendering)
- `native/src/metal/` (blur shader or multi-pass rendering)

**Estimated Effort:** Large

**Dependencies:** May require additional render passes or blur shader.

---

### [Priority: Medium] Image/Texture Drawing in Canvas API

**Description:** Add drawImage/drawTexture functions to the Canvas API for drawing textures with transformations.

**Rationale:** While Renderer.drawSprites exists, there is no high-level Canvas API for texture drawing with transforms, clipping, and compositing.

**Affected Files:**
- `Afferent/Canvas/Context.lean` (new drawImage functions)
- `Afferent/FFI/Texture.lean` (may need additional FFI functions)

**Estimated Effort:** Medium

**Dependencies:** None.

---

### [Priority: Medium] Multi-Window Support

**Description:** Enable creating and managing multiple windows from a single application.

**Rationale:** Some applications require multiple windows (toolbars, palettes, preview windows).

**Affected Files:**
- `Afferent/FFI/Window.lean`
- `Afferent/Canvas/Context.lean`
- `native/src/metal/window.m`

**Estimated Effort:** Large

**Dependencies:** Significant FFI and native code changes.

---

### [Priority: Low] Keyboard Event API Enhancement

**Description:** Add higher-level keyboard event handling with key names (not just key codes), text input support, and key repeat detection.

**Rationale:** Current API returns raw key codes which require manual mapping. Text input for text fields is not directly supported.

**Affected Files:**
- `Afferent/FFI/Window.lean` (new FFI functions)
- `native/src/metal/window.m` (text input delegates)

**Estimated Effort:** Medium

**Dependencies:** None.

---

### [Priority: Low] Cursor Customization

**Description:** Add ability to change the mouse cursor (pointer, text, crosshair, custom image).

**Rationale:** Different cursor styles provide important UI feedback.

**Affected Files:**
- `Afferent/FFI/Window.lean` (new FFI function)
- `native/src/metal/window.m` (NSCursor handling)

**Estimated Effort:** Small

**Dependencies:** None.

---

### [Priority: Low] Window Fullscreen and Resize API

**Description:** Add programmatic fullscreen toggle and window resize/position control.

**Rationale:** Applications often need fullscreen mode and window management.

**Affected Files:**
- `Afferent/FFI/Window.lean`
- `native/src/metal/window.m`

**Estimated Effort:** Small

**Dependencies:** None.

---

## Code Improvements

### [Priority: Medium] Matrix4 Performance (Superseded)

**Current State:** Matrix operations are now provided by the `linalg` library via `Mat4`. The old `Afferent/Render/Matrix4.lean` may be deprecated or removed in favor of `Linalg.Mat4`.

**Note:** Evaluate if `Linalg.Mat4` performance is sufficient. If SIMD optimization is still needed, it should be added to the linalg library rather than Afferent.

**Estimated Effort:** Potentially N/A if linalg Mat4 is sufficient

---

### [Priority: Medium] Font Registry Thread Safety

**Current State:** FontRegistry uses a simple Array which may not be thread-safe for concurrent access.

**Proposed Change:** Consider using thread-safe data structures or document single-threaded usage requirement.

**Benefits:** Safer concurrent font registration and lookup.

**Affected Files:**
- `Afferent/Text/Measurer.lean`

**Estimated Effort:** Small

---

### [Priority: Medium] Batch Capacity Growth Strategy

**Current State:** Batch pre-allocates with capacity hints but growth strategy is implicit via Lean Array behavior.

**Proposed Change:** Add explicit capacity doubling or configurable growth for very large batches.

**Benefits:** Better memory allocation patterns for scenes with many shapes.

**Affected Files:**
- `Afferent/Render/Tessellation.lean` (Batch namespace)

**Estimated Effort:** Small

---

## Code Cleanup

### [Priority: High] Remove Unused Imports

**Issue:** Some files may import modules that are not used.

**Location:** Project-wide audit needed

**Action Required:** Run import analysis and remove unused imports.

**Estimated Effort:** Small

---

### [Priority: Low] Normalize Doc Comments

**Issue:** Some functions have detailed doc comments while others have minimal or no documentation.

**Location:** Throughout codebase, especially FFI modules.

**Action Required:** Add doc comments to all public functions, standardize format.

**Estimated Effort:** Medium

---

### [Priority: Low] Clean Up Deprecated Patterns

**Issue:** Some code uses older Lean patterns that could be modernized.

**Location:** Project-wide

**Action Required:** Audit for deprecated patterns as Lean evolves.

**Estimated Effort:** Ongoing

---

## Architecture Considerations

### Renderer Abstraction for Non-Metal Backends

Currently the framework is tightly coupled to Metal on macOS. Consider a renderer abstraction layer to potentially support:
- Vulkan (for cross-platform)
- WebGPU (for browser targets)
- OpenGL fallback

This would be a significant undertaking but would expand the framework's reach.

### State Machine for Widget Events

The widget event system could benefit from a formal state machine for focus, hover, and pressed states to ensure consistent behavior across all interactive widgets.

### Memory Budget System for Tile Cache

**Note:** The map tile cache has been extracted to the `worldmap` package. This improvement should be tracked in the worldmap roadmap instead.

---

## API Ergonomics

### [Priority: Medium] ~~Auto-Scaling Mode~~ ✅ COMPLETED

**Status:** Completed - `Canvas.run` now auto-scales when `scaleToScreen = true` (default).

**Resolution:** Canvas automatically applies screen scale transform at frame start:
```lean
-- Before: manual scaling everywhere
fillTextXY text (20 * screenScale) (30 * screenScale) fontMedium

-- After: just use logical pixels
Canvas.run { title := "My App" } fun elapsed dt => do
  fillTextXY text 20 30 fontMedium  -- Auto-scaled!
```

Features:
- `Canvas.createWithScale` stores screen scale factor
- `Canvas.run` applies scale transform at frame start when `scaleToScreen = true`
- `getScreenScale : CanvasM Float` accessor for font loading
- `Font.loadSystemScaled` and `Font.loadScaled` for scaled font loading

**Affected Files:** `Afferent/Canvas/Context.lean`

---

### [Priority: Medium] Resource Scoping (RAII-style)

**Current:**
```lean
let fontSmall ← Font.load path size
...
fontSmall.destroy  -- Easy to forget
```

**Proposed:**
```lean
Font.with path size fun font => do
  ...  -- font auto-destroyed on exit

-- Or bracket pattern:
Canvas.run settings fun ctx => do
  ...  -- all resources auto-cleaned
```

**Affected Files:** `Afferent/Text/Font.lean`, `Afferent/Canvas/Context.lean`

---

### [Priority: Medium] ~~Simplified Main Loop~~ ✅ COMPLETED

**Status:** Completed - added `Canvas.run` and `CanvasConfig`.

**Resolution:** New simplified application entry point:
```lean
-- Before: 50+ lines of boilerplate
let screenScale ← FFI.getScreenScale
let canvas ← Canvas.create physWidth physHeight "Title"
let mut c := canvas
while !(← c.shouldClose) do
  c.pollEvents
  let ok ← c.beginFrame Color.darkGray
  if ok then
    let elapsed := ...
    c ← run' c do ...
    c ← c.endFrame

-- After: 3 lines
Canvas.run { title := "My App" } fun elapsed dt => do
  resetTransform
  fillTextXY s!"Time: {elapsed}" 20 30 font
```

`CanvasConfig` provides: width, height, title, clearColor, scaleToScreen (auto Retina scaling).

---

### [Priority: Medium] ~~System Font Loading~~ ✅ COMPLETED

**Status:** Completed - `Font.loadSystem` loads fonts by name.

**Resolution:** System fonts can now be loaded by name:
```lean
-- Before: hardcoded paths
Font.load "/System/Library/Fonts/Monaco.ttf" size

-- After: simple names
let font ← Font.loadSystem "Monaco" 16
let font ← Font.loadSystem "monospace" 24  -- Generic family
let font ← Font.loadSystemScaled "Helvetica" 16 screenScale
```

Features:
- `Font.loadSystem` - load by font name (Monaco, Helvetica, Times, etc.)
- `Font.loadSystemScaled` - load with screen scale factor
- `Font.loadScaled` - load from path with screen scale
- `Font.findSystemFont` - look up path by name
- Generic families: monospace, sans-serif, serif, system-ui

Supports 20+ common macOS fonts with normalized name lookup.

**Affected Files:** `Afferent/Text/Font.lean`

---

### [Priority: Low] Simplified Widget Click Handling

**Current (Interactive.lean):**
```lean
let (widget, layouts, ids, offsetX, offsetY) ←
  Demos.prepareCounterForHitTest fontRegistry fontMediumId ...
let hitId := Demos.hitTestCounter widget layouts offsetX offsetY ce.x ce.y
counterState := { counterState with widgetIds := some ids }
counterState := Demos.processClick counterState hitId
```

**Issue:** Too much ceremony for basic click handling.

**Proposed:** Widget system handles hit testing internally:
```lean
widget.onClick ce.x ce.y fun id =>
  match id with
  | .increment => state.increment
  | .decrement => state.decrement
```

**Affected Files:** `Afferent/Widget/*.lean`

---

## Summary: Next Up

1. **Pattern and image fills** - Texture fills for shapes
2. **Image/Texture drawing in Canvas API** - drawImage with transforms
3. **Shadow and glow effects** - Drop shadows and blur effects

## Recently Completed

- ✅ Dashed and dotted lines - DashPattern structure, setDashed/setDotted API
- ✅ Auto-scaling mode - Canvas.run auto-scales, getScreenScale accessor
- ✅ System font loading - Font.loadSystem with font name lookup
- ✅ Round line caps/joins - Arc geometry for round stroke elements
- ✅ Simplified main loop - Canvas.run and CanvasConfig

---

*Last updated: 2026-01-03*

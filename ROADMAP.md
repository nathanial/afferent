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

### [Priority: High] Round Line Caps and Joins

**Description:** Implement proper round line caps and line joins for stroke rendering. Currently `LineCap.round` and `LineJoin.round` fall back to butt caps and miter joins respectively.

**Rationale:** Round caps and joins are commonly needed for smooth graphics and are part of the standard Canvas API.

**Affected Files:**
- `Afferent/Render/Tessellation.lean` (expandPolylineToStroke function, lines 529, 555-558, 603-609)

**Estimated Effort:** Medium

**Dependencies:** None. Requires generating arc geometry for round elements.

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

### [Priority: Medium] Dashed and Dotted Lines

**Description:** Add support for dashed and dotted line patterns in StrokeStyle.

**Rationale:** Dashed lines are a common requirement for charts, borders, selection indicators, and technical drawings.

**Affected Files:**
- `Afferent/Core/Paint.lean` (StrokeStyle structure)
- `Afferent/Render/Tessellation.lean` (stroke tessellation)

**Estimated Effort:** Medium

**Dependencies:** None.

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

### [Priority: Medium] ~~Animation Easing Library~~ ✅ COMPLETED

**Status:** Completed via linalg library integration (v0.0.1+).

**Description:** Standard easing functions are now available through the `Linalg.Easing` module, which provides ease-in, ease-out, ease-in-out, and other common easing functions.

**Usage:**
```lean
import Linalg.Easing
-- Use Easing.easeInOutCubic, Easing.easeOutQuad, etc.
```

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

### [Priority: High] ~~Non-Convex Polygon Tessellation~~ ✅ COMPLETED

**Status:** Completed - implemented ear-clipping triangulation algorithm.

**Resolution:** Added `triangulateEarClipping`, `triangulatePolygon`, `isConvexPolygon`, `crossProduct2D`, and `pointInTriangle` functions. The `tessellatePath` function now automatically uses ear-clipping for concave polygons and fast fan triangulation for convex ones.

**Demo:** PathFeatures demo (mode 11 in Runner) showcases arrows, L-shapes, stars, and chevrons.

---

### [Priority: High] ~~Proper arcTo Implementation~~ ✅ COMPLETED

**Status:** Completed - implemented HTML5 Canvas-style arcTo geometry.

**Resolution:** Added `computeArcTo` function that calculates tangent points and arc center, then generates bezier curve approximations. The `pathToPolygonWithClosed` function now properly handles arcTo commands with correct rounded corner geometry.

**Demo:** PathFeatures demo shows rounded rectangles, rounded triangles, and pill shapes using arcTo.

---

### [Priority: High] ~~Arc Transform Handling~~ ✅ COMPLETED

**Status:** Completed - arcs are now converted to beziers before transformation.

**Resolution:** Updated `transformPath` in `CanvasState` to convert arc and arcTo commands to bezier curves before applying transforms. This correctly handles non-uniform scaling (circles become ellipses) and rotation.

**Demo:** PathFeatures demo shows circles under 2:1 and 1:2 scaling (rendering as ellipses), rotated pie slices, and combined scale+rotation on arcs.

---

### [Priority: Medium] Matrix4 Performance (Superseded)

**Current State:** Matrix operations are now provided by the `linalg` library via `Mat4`. The old `Afferent/Render/Matrix4.lean` may be deprecated or removed in favor of `Linalg.Mat4`.

**Note:** Evaluate if `Linalg.Mat4` performance is sufficient. If SIMD optimization is still needed, it should be added to the linalg library rather than Afferent.

**Estimated Effort:** Potentially N/A if linalg Mat4 is sufficient

---

### [Priority: Medium] ~~FPSCamera Clamp Function Visibility~~ ✅ COMPLETED

**Status:** Completed - replaced private `clamp` with `Float.clamp` from linalg.

**Resolution:** The private `clamp` helper was removed and replaced with `Float.clamp` from `Linalg.Core`.

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

### [Priority: Low] ~~Reduce Magic Numbers in Path.lean~~ ✅ COMPLETED

**Status:** Completed - defined `bezierCircleK` constant.

**Resolution:** Added `Path.bezierCircleK : Float := 0.5522847498` and updated circle, ellipse, and roundedRect functions to use it.

---

### [Priority: Low] ~~Pi Constant Consolidation~~ ✅ COMPLETED

**Status:** Completed via linalg library integration.

**Resolution:** All files now import `Float.pi`, `Float.twoPi`, and `Float.halfPi` from the `Linalg.Core` module instead of defining them locally. Path.lean, FPSCamera.lean, and other files have been updated to use the linalg constants.

---

## Code Cleanup

### [Priority: High] Remove Unused Imports

**Issue:** Some files may import modules that are not used.

**Location:** Project-wide audit needed

**Action Required:** Run import analysis and remove unused imports.

**Estimated Effort:** Small

---

### [Priority: Medium] ~~Document Vertex Layout Constants~~ ✅ COMPLETED

**Status:** Completed - added vertex size constants to Tessellation.lean.

**Resolution:** Added `Tessellation.vertexSize2D`, `vertexSize3D`, and `vertexSize3DTextured` constants. The Batch module now uses `vertexSize2D` for vertex count calculations.

---

### [Priority: Medium] ~~Add More Test Coverage~~ ✅ COMPLETED

**Status:** Completed - test count increased from 43 to 98 tests.

**New test suites added:**
- `CanvasStateTests.lean`: 20 tests for transform composition, inverse, state stack
- `FontTests.lean`: 7 tests for font loading and text measurement
- Gradient edge cases: 6 tests for empty/single stops, out-of-bounds sampling
- Tessellation tests: 22 tests for ear-clipping, convexity detection, arcTo geometry

**Remaining opportunities:** Widget rendering tests, more layout edge cases.

---

### [Priority: Low] Normalize Doc Comments

**Issue:** Some functions have detailed doc comments while others have minimal or no documentation.

**Location:** Throughout codebase, especially FFI modules.

**Action Required:** Add doc comments to all public functions, standardize format.

**Estimated Effort:** Medium

---

### [Priority: Low] ~~Demo Code Cleanup~~ ✅ COMPLETED

**Status:** Completed - all demos now use linalg constants.

**Resolution:** Updated `Demos/SpinningCubes.lean` and `examples/SpinningCubes.lean` to use `Float.pi` instead of hardcoded values.

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

## Quick Wins

These items can be addressed quickly with minimal risk:

1. ~~Define pi constant in one place~~ ✅ (done via linalg)
2. ~~Add named constants for vertex layout sizes~~ ✅ (vertexSize2D/3D/3DTextured)
3. ~~Add shouldBeNear tolerance parameter to test helpers~~ ✅ (already in Crucible)
4. ~~Document all FFI function parameters~~ ✅ (Window.lean documented)
5. ~~Add more gradient sampling tests~~ ✅ (6 edge case tests added)
6. ~~Define named constant for bezier circle approximation~~ ✅ (bezierCircleK)

---

## Recent Developments (December 2025)

### Linalg Integration ✅

The project now uses the `linalg` library for math operations, providing:
- `Float.pi`, `Float.twoPi`, `Float.halfPi` constants
- `Vec3`, `Mat4` types with standard operations
- Easing functions via `Linalg.Easing`

### Module Extraction ✅

Several components have been extracted to separate packages for better reusability:
- **assimptor**: 3D asset loading (Assimp wrapper)
- **worldmap**: Map tile rendering and caching

### Window Improvements ✅

- Resizable windows with proper handling
- Improved keyboard input handling

### Embedded Shaders ✅

Metal shaders are now embedded directly in the binary, eliminating runtime shader file loading.

### Path Features ✅ (New)

Implemented three high-priority code improvements:
- **Ear-clipping triangulation**: Non-convex polygon support (arrows, L-shapes, stars)
- **Proper arcTo**: HTML5 Canvas-style tangent arc geometry for rounded corners
- **Arc transforms**: Arcs convert to beziers for correct non-uniform scaling

New PathFeatures demo (mode 11 in Runner) showcases all three features.

---

*Last updated: 2026-01-03* (Canvas threading eliminated)

---

## API Ergonomics (January 2026)

Based on review of afferent-demos usage patterns, these improvements would make the API easier to use.

### [Priority: High] ~~Eliminate Canvas Threading Pattern~~ ✅ COMPLETED

**Status:** Completed - added CanvasM wrappers for window input and frame loop.

**Resolution:** Added to CanvasM namespace:
- Window input: `getKeyCode`, `hasKeyPressed`, `clearKey`, `getPointerLock`, `setPointerLock`, `isKeyDown`, `getMouseDelta`, `getClick`, `clearClick`
- Context access: `getRenderer`, `getWindow`, `getCurrentSize`
- Frame loop: `runLoopM` for running render loops entirely in CanvasM

**New pattern:**
```lean
c ← run' c do
  resetTransform
  setFillColor Color.white
  fillTextXY ...
  let renderer ← getRenderer
  someFFICall renderer ...
```

All operations stay inside a single CanvasM block. Demo code refactored to use this pattern.

---

### [Priority: High] Named Key Constants

**Current:**
```lean
if keyCode == 49 then  -- Space bar
if keyCode == 53 then  -- Escape
if keyCode == 124 then  -- Right arrow
```

**Issue:** Magic numbers, platform-specific, hard to maintain.

**Proposed:**
```lean
if keyCode == Key.space then
if keyCode == Key.escape then
if keyCode == Key.right then
```

**Affected Files:** `Afferent/FFI/Window.lean` (add Key namespace with constants)

---

### [Priority: High] ~~Flatten Context Access~~ ✅ COMPLETED (via CanvasM)

**Status:** Completed - CanvasM now has direct access to window/renderer operations.

**Resolution:** Inside CanvasM, you can now use:
```lean
c ← run' c do
  let click ← getClick       -- was: FFI.Window.getClick c.ctx.window
  clearClick                  -- was: FFI.Window.clearClick c.ctx.window
  let down ← isKeyDown 13     -- was: FFI.Window.isKeyDown c.ctx.window 13
  let renderer ← getRenderer  -- was: c.ctx.renderer
```

**Note:** Direct Canvas methods (outside CanvasM) not added, but CanvasM is the recommended pattern.

---

### [Priority: Medium] Scoped Transform Helper

**Current:**
```lean
save
translate 150 150
...stuff...
restore
```

**Issue:** Easy to forget `restore`, verbose.

**Proposed:**
```lean
withTransform (translate 150 150) do
  ...stuff...
-- or
scoped do
  translate 150 150
  ...stuff...
```

**Affected Files:** `Afferent/Canvas/Context.lean`

---

### [Priority: Medium] Auto-Scaling Mode

**Current:**
```lean
fillTextXY text (20 * screenScale) (30 * screenScale) fontMedium
let fontSmall ← Font.load path (16 * screenScale).toUInt32
```

**Issue:** Manual scale multiplication everywhere, easy to miss.

**Proposed:** Canvas has optional logical coordinate mode:
```lean
canvas.setLogicalSize 1920 1080  -- All coords in logical pixels
fillTextXY text 20 30 fontMedium  -- Auto-scaled to physical
```

**Affected Files:** `Afferent/Canvas/Context.lean`, `Afferent/FFI/Renderer.lean`

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

### [Priority: Medium] Simplified Main Loop

**Current:** Every app needs 50+ lines of boilerplate:
- Create canvas, load fonts
- `while !(← c.shouldClose)` loop
- `pollEvents`, `beginFrame`, `endFrame`
- Manual frame timing (`now - lastTime`)
- Manual cleanup

**Proposed:**
```lean
Canvas.run { width := 1920, height := 1080, title := "My App" } fun ctx t dt => do
  -- Just rendering code here
  -- t = elapsed time, dt = delta time
  -- Cleanup automatic
```

**Affected Files:** `Afferent/Canvas/Context.lean` (new `Canvas.run` function)

---

### [Priority: Medium] System Font Loading

**Current:**
```lean
Font.load "/System/Library/Fonts/Monaco.ttf" size
```

**Issue:** Hardcoded macOS paths, brittle.

**Proposed:**
```lean
Font.loadSystem "Monaco" size
Font.loadSystem "monospace" size  -- Generic family names
```

**Affected Files:** `Afferent/Text/Font.lean`, `native/src/common/text_render.c`

---

### [Priority: Low] Color Convenience Methods

**Current:**
```lean
Color.hsva 0.667 0.25 0.20 1.0
Color.rgba color.r color.g color.b alpha
```

**Proposed:**
```lean
Color.hsv 0.667 0.25 0.20  -- alpha defaults to 1
color.withAlpha 0.5
color.lighter 0.2
color.darker 0.2
```

**Affected Files:** `Afferent/Core/Color.lean`

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

## Summary: Quick Wins

1. **Key constants** - Small change, big readability improvement ⬅️ **NEXT**
2. ~~Flatten context access~~ ✅ (via CanvasM wrappers)
3. **withTransform helper** - Simple wrapper around save/restore
4. **Color defaults** - `Color.hsv` with alpha=1 default

## Summary: Bigger Wins (More Effort)

1. **Canvas.run main loop** - Eliminates boilerplate, manages resources
2. **Auto-scaling mode** - No more `* screenScale` everywhere
3. **System font loading** - Platform-independent font names

## Completed in January 2026

- ✅ **Eliminate Canvas Threading Pattern** - CanvasM wrappers for window input, getRenderer, runLoopM
- ✅ **Flatten Context Access** - All window/renderer ops available in CanvasM

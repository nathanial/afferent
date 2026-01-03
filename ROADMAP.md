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

1. **Canvas.run main loop** - Eliminates boilerplate, manages resources
2. **Auto-scaling mode** - No more `* screenScale` everywhere
3. **System font loading** - Platform-independent font names

---

*Last updated: 2026-01-03*

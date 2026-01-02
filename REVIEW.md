# Afferent Review (graphics/afferent)

Date: 2026-01-02

## Goals fit
- The project already delivers a macOS/Metal 2D+3D renderer with Canvas-style APIs, gradients, text, batching/instancing, and basic 3D mesh + fog/ocean.
- It is not yet a full Canvas spec or full 3D engine (missing compound-path fill rules, round caps/joins, dashed strokes, image/pattern fills, etc.).

## Findings (ordered by severity)
1) High — Scissor clamp can underflow when `x`/`y` exceed drawable size, producing huge scissor rects. `native/src/metal/draw_2d.m:162`
2) High — Renderer/window destruction only `free()`s C structs; Obj-C resources are not released, leaking Metal/NSWindow objects. `native/src/metal/render.m:88`, `native/src/metal/window.m:479`
3) Medium — Closed-path strokes still go through cap logic, causing seams/overlaps; closed paths should use joins only. `Afferent/Render/Tessellation.lean:998`
4) Medium — `pathToPolygonWithClosed` flattens all subpaths into one polygon and ignores fill rules; compound paths/holes and even-odd fill are incorrect. `Afferent/Render/Tessellation.lean:156`, `Afferent/Core/Path.lean:23`
5) Medium — ClickEvent FFI layout relies on hard-coded scalar offsets; brittle across Lean upgrades/field changes. `native/src/lean_bridge.c:243`
6) Low — Vertex buffer creation truncates arrays not divisible by 6; instancing comments don’t match actual layout; instance buffer capacity bookkeeping can cause reallocation churn. `native/src/lean_bridge.c:391`, `native/src/metal/draw_2d.m:35`, `Afferent/Canvas/Context.lean:456`

## Suggested improvements
- Implement proper round caps/joins and dashed strokes in tessellation.
- Add compound-path triangulation with fill rules (non-zero / even-odd).
- Add an inverse-transpose normal matrix for 3D normals with non-uniform scaling.
- Consider persistent GPU buffers for 3D meshes and FloatBuffer-first APIs for high-volume 2D paths.

## Addressing (priority order)
- ✅ Scissor underflow clamp fix started.
- ✅ Renderer/window resource cleanup started (strong refs + explicit nil).
- ✅ Closed-path stroke handling started (joins-only for closed paths).

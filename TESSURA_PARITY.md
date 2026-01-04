# Tessura Parity Analysis

This document compares Afferent's rendering architecture to Tessura (a high-performance TypeScript/WebGL 2D graphics framework) and outlines improvements to bring Afferent to parity in quality and performance.

---

## Architecture Comparison

| Aspect | Afferent (Metal/Lean) | Tessura (WebGL/TypeScript) |
|--------|----------------------|---------------------------|
| **Stroke width** | GPU screen-space extrusion | GPU screen-space (constant pixels) |
| **Batching** | Simple merge by call order | Style-keyed batching + z-index sort |
| **Tessellation** | Earcut port (holes supported) | earcut library (proven, handles holes) |
| **Dynamic buffers** | FloatBuffer (C-allocated) | DynamicBuffer with auto-grow + type upgrade |
| **Shader count** | 12 specialized shaders | 4 general shaders |
| **Immediate mode** | Batch accumulation | Canvas 2D API with deferred flush |
| **Fill vertices** | 6 floats (pos + color) | 2 floats (pos), color as uniform |
| **Z-ordering** | Draw order dependent | Explicit z-index with batch sorting |

---

## High-Priority Improvements

### 1. Style-Based Batch Sorting

**Problem:** Afferent merges geometry in draw order but doesn't group by style. This causes unnecessary state changes (color uniform updates, shader switches) between draw calls.

**Tessura's Approach:**
```typescript
interface BatchKey {
  programType: "fill" | "stroke"
  colorHash: number              // Fast 32-bit color comparison
  strokeWidth: number            // 0 for fills
  blendMode: BlendMode
  zIndex: number
}

// Features with identical keys → merged into single GPU buffer
// Batches sorted by: z-index → fills before strokes → color
```

**Solution for Afferent:**
```lean
structure BatchKey where
  programType : RenderProgram  -- fill, stroke, instanced, text
  colorHash : UInt32           -- hash of RGBA bytes
  strokeWidth : Float          -- 0.0 for fills
  zIndex : Int                 -- layer ordering
  deriving BEq, Hashable

structure BatchGroup where
  key : BatchKey
  vertices : Array Float
  indices : Array UInt32
```

**Implementation:**
1. Accumulate draw commands with their batch keys
2. At frame end, sort batches by key
3. Merge geometry within each batch group
4. Issue one draw call per unique batch key

**Benefits:**
- Fewer draw calls (10-100x reduction for complex scenes)
- Fewer uniform updates
- Correct z-ordering without manual management

**Affected Files:**
- `Afferent/Render/Batch.lean` (new module)
- `Afferent/Canvas/Context.lean` (integrate batch system)

---

### 2. Separate Fill Vertex Format

**Problem:** Afferent uses 6 floats per vertex (position + RGBA color) even for solid-color fills. This is 3x the bandwidth needed.

**Tessura's Approach:**
- Fill shader: 2 floats/vertex (position only), color as uniform
- Gradient shader: 6 floats/vertex (position + color)
- Stroke shader: 5 floats/vertex (position + normal + miter)

**Solution for Afferent:**

```lean
-- Solid fill: position only, color as uniform
structure SolidFillVertex where
  x : Float
  y : Float

-- Gradient fill: position + interpolated color
structure GradientVertex where
  x : Float
  y : Float
  r : Float
  g : Float
  b : Float
  a : Float

-- Stroke: position + extrusion data
structure StrokeVertex where
  x : Float
  y : Float
  nx : Float      -- normal x
  ny : Float      -- normal y
  miter : Float   -- miter scale (positive = left, negative = right)
```

**Benefits:**
- 66% bandwidth reduction for solid fills
- Better GPU cache utilization
- Clearer separation of concerns

**Affected Files:**
- `Afferent/Render/Tessellation.lean` (multiple result types)
- `native/src/metal/fill_solid.metal` (new shader)
- `Afferent/FFI/Renderer.lean` (new draw functions)

---

### 3. Z-Index Layer System

**Problem:** Afferent renders in draw order. Complex scenes with overlapping elements require careful ordering by the application.

**Tessura's Approach:**
```typescript
// Each feature has explicit z-index
interface DrawCommand {
  geometry: Geometry
  style: Style
  zIndex: number
}

// Batches sorted by z-index before rendering
// Within same z-index: fills rendered before strokes
batches.sort((a, b) => {
  if (a.zIndex !== b.zIndex) return a.zIndex - b.zIndex
  if (a.isFill !== b.isFill) return a.isFill ? -1 : 1
  return 0
})
```

**Solution for Afferent:**

```lean
-- Add z-index to canvas state
structure CanvasState where
  -- existing fields...
  zIndex : Int := 0

-- CanvasM accessor
def setZIndex (z : Int) : CanvasM Unit := ...

-- Batch system respects z-index
def flushBatches (batches : Array BatchGroup) : IO Unit := do
  let sorted := batches.qsort (·.key < ·.key)
  for batch in sorted do
    drawBatchGroup batch
```

**Benefits:**
- Correct rendering without manual ordering
- Easier scene composition
- Required for proper UI layering

---

## Medium-Priority Improvements

### 4. Dynamic Buffer Growth Strategy

**Tessura's DynamicBuffer:**
- Grows 2x when capacity exceeded
- Automatically upgrades UInt16 → UInt32 indices when > 65535 vertices
- Uses appropriate GPU hints (DYNAMIC_DRAW)

**Solution for Afferent:**
```lean
structure DynamicBatch where
  vertices : FloatBuffer
  indices : IndexBuffer  -- Supports both UInt16 and UInt32
  vertexCapacity : Nat
  indexCapacity : Nat
  useUInt32Indices : Bool

def DynamicBatch.ensureCapacity (b : DynamicBatch) (verts indices : Nat) : IO DynamicBatch := do
  -- Grow 2x if needed
  -- Upgrade index type if vertex count > 65535
```

### 5. Shader Consolidation

**Current:** 12 specialized shaders
**Target:** 6-8 shaders with better reuse

Consolidation opportunities:
- `instanced.metal` handles rectangles; could extend for circles/triangles via vertex buffer
- Keep specialized shaders only where GPU-specific optimization is needed

### 6. Frustum Culling for Large Scenes

**Tessura's Approach:**
- Tile-based spatial partitioning
- Camera bounds query for visible tiles
- Skip rendering of off-screen geometry

**Solution for Afferent:**
- Add `isVisible(rect : Rect, camera : Camera) : Bool`
- Spatial index for large static geometry
- Tile-based culling for map/world rendering

---

## What Afferent Does Well

These aspects are already strong and should be preserved:

1. **FloatBuffer** - Zero-copy particle rendering with C-allocated buffers
2. **3D Pipeline** - Full 3D rendering with mesh loading (Tessura is 2D only)
3. **Widget System** - Declarative UI with layout (no equivalent in Tessura)
4. **Collimator Optics** - Elegant functional state management
5. **Instanced Rendering** - Multiple specialized instanced shaders
6. **GPU-Side Animation** - HSV→RGB conversion, time-based animation on GPU

---

## Implementation Priority

| Priority | Item | Impact | Effort |
|----------|------|--------|--------|
| 1 | Style-keyed batching | Performance | Medium |
| 2 | Separate fill vertex format | Bandwidth | Small |
| 3 | Z-index sorting | Correctness | Small |
| 4 | Dynamic buffer growth | Memory efficiency | Small |
| 5 | Shader consolidation | Maintainability | Medium |
| 6 | Frustum culling | Large scene perf | Large |

---

## Performance Targets

Based on Tessura's capabilities:

| Metric | Current Afferent | Target |
|--------|------------------|--------|
| Solid rectangles/frame | ~10,000 | 100,000+ |
| Draw calls/frame | O(shapes) | O(unique styles) |
| Stroke quality | Screen-space | Screen-space |
| Polygon holes | Supported | Supported |
| Memory growth | Per-shape alloc | Pooled buffers |

---

## Reference Implementation

Tessura source: `references/tessura/`

Key files to study:
- `src/immediate/FillRenderer.ts` - Batch accumulation pattern
- `src/immediate/StrokeRenderer.ts` - Screen-space stroke rendering
- `src/geometry/extrude.ts` - Line extrusion with miter/caps
- `src/geometry/tessellate.ts` - earcut integration
- `src/batch/BatchRenderer.ts` - Style-keyed batching
- `src/shaders/stroke.ts` - Screen-space stroke shader

---

*Last updated: 2026-01-04*

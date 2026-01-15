# Batching Performance Roadmap

This document outlines remaining optimizations for GPU batching in Afferent's rendering pipeline.

## Current State

### Implemented

1. **fillRect Batching** (`draw_2d.m`)
   - Consecutive fillRect commands with same corner radius batch into single GPU draw call
   - Uses `drawRectsBatch` with instanced rendering

2. **Command Coalescing** (`Backend.lean`)
   - Reorders commands within scopes to maximize batching
   - Groups by type: fillRects → strokeRects → fillPaths → strokePaths → texts
   - Respects scope boundaries (save/restore, clips, transforms)

3. **Render Command Caching** (`Collect.lean`)
   - Caches CustomSpec.collect output across frames
   - Keyed by widget name + layout hash + generation
   - Avoids redundant command generation for static widgets

4. **Buffer Pooling** (`buffer_pool.m`)
   - Reuses Metal buffers across frames
   - Separate pools for vertex, index, and text buffers

5. **GPU Instancing** (`draw_2d.m`)
   - `drawInstancedShapes` for rects, triangles, circles
   - `drawCirclesBatch` for scatter plots
   - `drawRectsBatch` for heatmaps/grids

6. **strokeRect Batching** (`Backend.lean`)
   - Consecutive strokeRect commands with same lineWidth and cornerRadius batch into single GPU draw call
   - Uses `executeStrokeRectBatch` with `StrokeRectBatchEntry` structures

7. **fillCircle/strokeCircle Commands** (`Command.lean`, `Backend.lean`)
   - Dedicated `RenderCommand.fillCircle` and `RenderCommand.strokeCircle` variants
   - Batched via `executeFillCircleBatch` with `CircleBatchEntry` structures
   - Avoids CPU tessellation for circles (GPU-native rendering)

8. **Text Batching** (`Backend.lean`, `draw_text.m`, `text_render.c`)
   - Consecutive fillText commands with same font batch into single GPU draw call
   - Uses `executeTextBatch` with `TextBatchEntry` structures
   - Per-entry transforms supported (rotation/scale via affine matrix)
   - Reuses existing glyph atlas infrastructure

---

## Remaining Optimizations

### Phase 1: Extend Command-Level Batching

#### 1.1 fillPolygon Batching

**Current**: Each fillPolygon converts to path and tessellates separately.

**Proposed**: Batch convex polygons with same vertex count.

**Complexity**: Medium-high (vertex counts vary).

**Alternative**: For common cases (triangles, quads), add dedicated commands:
```lean
| fillTriangle (p1 p2 p3 : Point) (color : Color)
| fillQuad (p1 p2 p3 p4 : Point) (color : Color)
```

---

### Phase 2: Path Optimization

#### 2.1 Path Tessellation Caching

**Current**: Same path tessellated every frame if drawn multiple times.

**Proposed**: Cache tessellated vertex/index buffers keyed by path hash.

**Implementation**:
```lean
structure TessellationCache where
  cache : HashMap UInt64 (AfferentBuffer × AfferentBuffer × Nat)  -- vertex, index, indexCount
  maxSize : Nat := 1000

def getCachedTessellation (cache : IO.Ref TessellationCache) (path : Path)
    : IO (Option (AfferentBuffer × AfferentBuffer × Nat))

def cacheTessellation (cache : IO.Ref TessellationCache) (path : Path)
    (vertex index : AfferentBuffer) (indexCount : Nat) : IO Unit
```

**Impact**: Repeated icons, chart shapes, UI decorations.

---

#### 2.2 Path Deduplication

**Current**: Identical paths in same frame tessellated multiple times.

**Proposed**: During coalescing, identify duplicate paths and reuse tessellation.

**Implementation**: Add path hash to fillPath/strokePath, group by hash in bins.

---

### Phase 3: Text Optimization

#### 3.1 Text Batching by Font ✓ DONE

~~**Current**: Each fillText is separate draw call.~~

~~**Proposed**: Batch text commands with same font into single draw call.~~

Implemented in `Backend.lean:executeTextBatch` with:
- `TextBatchEntry` structure with per-entry transform
- `FFI.Text.renderBatch` for batched rendering
- `afferent_text_generate_vertices_batch` generates all vertices in one buffer
- Single `drawIndexedPrimitives` call per font batch

---

### Phase 4: Advanced GPU Optimization

#### 4.1 Indirect Draw Commands

**Current**: Each batch is separate `drawPrimitives` call with CPU→GPU sync.

**Proposed**: Use Metal indirect drawing to dispatch multiple batches in one call.

**Implementation**:
```objc
// Build indirect buffer on CPU
MTLDrawPrimitivesIndirectArguments args[batchCount];
for (int i = 0; i < batchCount; i++) {
    args[i].vertexCount = 4;
    args[i].instanceCount = batches[i].count;
    args[i].vertexStart = 0;
    args[i].baseInstance = batches[i].baseInstance;
}

// Single GPU dispatch
[encoder drawPrimitives:MTLPrimitiveTypeTriangleStrip
          indirectBuffer:indirectBuffer
    indirectBufferOffset:0];
```

**Impact**: Eliminates per-batch CPU overhead for complex scenes.

---

#### 4.2 Persistent Mapped Buffers

**Current**: Buffer contents copied each frame via `memcpy`.

**Proposed**: Use triple-buffered persistent mapping for streaming data.

**Implementation**: Already partially implemented with buffer pooling. Enhancement would add:
- Ring buffer for instance data
- Fence synchronization to prevent overwriting in-flight data

---

#### 4.3 Compute Shader Preprocessing

**Current**: Command coalescing done on CPU in Lean.

**Proposed**: GPU compute pass to sort/bin commands.

**Complexity**: Very high. Only beneficial for 100K+ commands per frame.

---

## Priority Matrix

| Optimization | Impact | Effort | Priority | Status |
|--------------|--------|--------|----------|--------|
| strokeRect batching | Medium | Low | High | Done |
| fillCircle command | High | Low | High | Done |
| Text batching | High | High | Medium | Done |
| Path tessellation cache | Medium | Medium | Medium | |
| Indirect draw | Medium | Medium | Low | |
| Compute preprocessing | Low | Very High | Low | |

## Recommended Implementation Order

1. ~~**fillCircle/strokeCircle commands** - Low effort, high impact for charts~~ Done
2. ~~**strokeRect batching** - Low effort, completes rect batching story~~ Done
3. ~~**Text batching** - High effort, but significant for text-heavy UIs~~ Done
4. **Path tessellation caching** - Medium effort, helps repeated UI elements

## Metrics to Track

- Draw calls per frame (target: <100 for typical UI)
- Batch efficiency (commands batched / total commands)
- GPU time per frame
- CPU time in coalescing/batching

Use `renderArborWidgetWithStats` to get cache hit/miss ratios.
Add `executeCommandsBatchedWithStats` return value logging for batch metrics.

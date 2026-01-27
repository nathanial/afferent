# Render Command Caching Proposal

## The Problem

Every frame, `collectCommands` walks the widget tree and calls `spec.collect` for every `CustomSpec` widget:

```
collectWidget (line 203-205):
  | .custom _ _ style spec =>
    collectBoxStyle borderRect style
    CollectM.emitAll (spec.collect computed)  -- Called EVERY FRAME
```

For a PieChart with 100 slices at 60fps:
- 0.1ms × 60 = **6ms/second** just for command generation
- With 10 charts: **60ms/second** of pure Lean overhead

The `dynWidget` optimization only prevents widget tree rebuilding, not render command collection.

## Root Cause Analysis

The expensive operations in `collect` are:

1. **Path allocation**: Each shape creates a new `Afferent.Path` object
2. **Array appends**: Pure functional arrays copy on each push
3. **Color calculations**: Per-element color lookups
4. **String formatting**: Label generation for each data point
5. **Trigonometry**: `Float.sin`/`Float.cos` for circular charts

These run **every frame** even when nothing changes.

## Cache Invalidation Triggers

Render commands only need rebuilding when:

1. **Data changes** - New values to display
2. **Layout changes** - Widget resized/repositioned
3. **Theme changes** - Colors need recalculation

At 60fps with static data, 59 out of 60 frames do **redundant work**.

---

## Proposed Solutions

### Option A: CachedCustomSpec (Minimal Invasion)

Add a new widget variant that wraps `CustomSpec` with caching:

```lean
structure RenderCache where
  commands : Array RenderCommand
  layoutRect : Trellis.LayoutRect
  dataVersion : UInt64

structure CachedCustomSpec where
  spec : CustomSpec
  cache : IO.Ref (Option RenderCache)
  dataVersion : IO.Ref UInt64  -- Increment on data change
```

In `collectWidget`:

```lean
| .cachedCustom _ _ style cachedSpec =>
  collectBoxStyle borderRect style
  let cached ← cachedSpec.cache.get
  let currentVersion ← cachedSpec.dataVersion.get
  let rect := computed.contentRect

  let commands ← match cached with
  | some c =>
    if c.dataVersion == currentVersion && c.layoutRect == rect then
      pure c.commands  -- Cache hit!
    else
      let cmds := cachedSpec.spec.collect computed
      cachedSpec.cache.set (some ⟨cmds, rect, currentVersion⟩)
      pure cmds
  | none =>
    let cmds := cachedSpec.spec.collect computed
    cachedSpec.cache.set (some ⟨cmds, rect, currentVersion⟩)
    pure cmds

  CollectM.emitAll commands
```

**Pros**: Minimal changes, backward compatible
**Cons**: Requires explicit opt-in per widget, IO.Ref in pure code

---

### Option B: Pre-Computed RenderCommands in Widget Tree

Store commands directly in the widget, computed during widget building:

```lean
inductive Widget where
  | ...
  | custom (id : WidgetId) (name : Option String) (style : BoxStyle)
           (spec : CustomSpec)
  | prerendered (id : WidgetId) (name : Option String) (style : BoxStyle)
                (commands : ComputedLayout → Array RenderCommand)
                (cachedCommands : Option (LayoutRect × Array RenderCommand))
```

The chart widget pre-computes during `dynWidget` rebuild:

```lean
def pieChart (data : Dyn PieChart.Data) ... := do
  -- Pre-compute geometry when data changes
  let geometry ← Dynamic.mapM computePieGeometry data

  let _ ← dynWidget geometry fun geom => do
    emit do
      -- Return a prerendered widget with geometry baked in
      pure (prerenderedPieChart geom theme)
```

Where `prerenderedPieChart` returns a closure that only does layout-dependent work:

```lean
def prerenderedPieChart (geom : PieGeometry) (theme : Theme) : WidgetBuilder :=
  prerendered "pie" { ... } fun layout =>
    -- Only layout-dependent transforms, geometry already computed
    geom.slices.flatMap fun slice =>
      #[.fillPath (slice.path.translate layout.x layout.y) slice.color]
```

**Pros**: Zero per-frame cost for static data, clean separation
**Cons**: Requires refactoring all chart specs, two-phase computation

---

### Option C: Automatic Memoization via Layout Hash (Recommended)

Add transparent caching keyed by layout hash, no API changes:

```lean
structure CustomSpec where
  measure : Float → Float → (Float × Float)
  collect : Trellis.ComputedLayout → RenderCommands
  draw : Option (Trellis.ComputedLayout → Afferent.CanvasM Unit) := none
  hitTest : Option (Trellis.ComputedLayout → Point → Bool) := none
  -- NEW: Optional memoization
  memoize : Bool := false
```

Global frame cache in `CollectState`:

```lean
structure CollectState where
  commands : Array RenderCommand := #[]
  deferredAbsolute : Array (Widget × Trellis.LayoutResult) := #[]
  -- NEW: Memoization cache (widget ID → (layout hash, commands))
  memoCache : Std.HashMap WidgetId (UInt64 × Array RenderCommand) := {}
```

In `collectWidget`:

```lean
| .custom id _ style spec =>
  collectBoxStyle borderRect style
  let cmds ← if spec.memoize then
    let layoutHash := hashLayoutRect computed.contentRect
    let state ← get
    match state.memoCache.find? id with
    | some (cachedHash, cachedCmds) =>
      if cachedHash == layoutHash then
        pure cachedCmds  -- Cache hit
      else
        let cmds := spec.collect computed
        modify fun s => { s with memoCache := s.memoCache.insert id (layoutHash, cmds) }
        pure cmds
    | none =>
      let cmds := spec.collect computed
      modify fun s => { s with memoCache := s.memoCache.insert id (layoutHash, cmds) }
      pure cmds
  else
    pure (spec.collect computed)
  CollectM.emitAll cmds
```

**Pros**: Opt-in per spec, minimal API change, works with existing code
**Cons**: Cache only valid within frame, doesn't persist across frames

---

### Option D: Persistent Frame Cache (Most Effective)

Maintain cache across frames at the `Backend.renderArborWidget` level:

```lean
structure WidgetRenderCache where
  cache : IO.Ref (Std.HashMap WidgetId CachedRender)

structure CachedRender where
  commands : Array RenderCommand
  layoutHash : UInt64
  frameId : Nat  -- Frame when last validated
```

```lean
def renderArborWidgetCached (frameCache : WidgetRenderCache) (frameId : Nat)
    (reg : FontRegistry) (widget : Widget) (w h : Float) : CanvasM Unit := do
  let measureResult ← runWithFonts reg (measureWidget widget w h)
  let layouts := Trellis.layout measureResult.node w h

  -- Collect with cache
  let commands ← collectCommandsCached frameCache frameId measuredWidget layouts

  executeCommands reg commands
```

The cache persists across frames, so:
- Frame 1: Cache miss, compute all commands, store in cache
- Frame 2-60: Cache hits for all static widgets
- Frame 61 (data change): `dynWidget` rebuilds tree with new widget IDs, cache miss for changed widgets

**Widget ID stability is key**: Same widget ID + same layout = cache hit.

**Pros**: Maximum performance gain, zero overhead for static widgets
**Cons**: Requires cache management, memory for cached commands

---

## Recommended Implementation Path

### Phase 1: Quick Win (Option C)

Add `memoize : Bool` to `CustomSpec` and per-frame memoization:

1. Modify `CustomSpec` to add `memoize` field (default `false`)
2. Add `memoCache` to `CollectState`
3. Update `collectWidget` to check cache for memoized specs
4. Update chart specs to set `memoize := true`

**Expected improvement**: ~50% reduction (cache hits within same frame for repeated widgets)

### Phase 2: Persistent Cache (Option D)

Add frame-persistent cache:

1. Create `WidgetRenderCache` structure
2. Pass cache through `renderArborWidget`
3. Use widget ID + layout hash as cache key
4. Invalidate on widget tree rebuild (new IDs from `dynWidget`)

**Expected improvement**: ~95% reduction for static charts

### Phase 3: Two-Phase Rendering (Option B)

For maximum performance, separate geometry computation from rendering:

1. Define geometry types for each chart (e.g., `PieGeometry`)
2. Compute geometry in `Dynamic.mapM` (runs on data change only)
3. Render phase only does layout transforms

**Expected improvement**: Near-zero overhead for static data

---

## Performance Projections

| Scenario | Current | Phase 1 | Phase 2 | Phase 3 |
|----------|---------|---------|---------|---------|
| 10 static charts @ 60fps | 60ms/s | 30ms/s | 1ms/s | 0.1ms/s |
| 1 animating chart + 9 static | 60ms/s | 30ms/s | 7ms/s | 1ms/s |
| All charts animating | 60ms/s | 60ms/s | 60ms/s | 60ms/s |

## Next Steps

1. Benchmark current performance with real app
2. Implement Phase 1 (low risk, immediate benefit)
3. Measure improvement
4. Decide if Phase 2/3 needed based on profiling

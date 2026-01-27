/-
  Arbor Cached Measurement
  Frame-to-frame caching for widget measurement results.
  This module provides cached versions of measureWidget and intrinsicSize.
-/
import Afferent.Arbor.Widget.Measure
import Afferent.Arbor.Widget.MeasureCache

namespace Afferent.Arbor

/-! ## Cached Measurement

These functions use the MeasureCache to avoid redundant measurement across frames.
The cache is keyed by (widgetId, availWidth, availHeight). When the widget tree
structure is stable (same widgets in same order), widget IDs are identical
between frames, enabling cache hits.

Key insight: `measureWidget` already reuses TextLayout if present in the widget.
By caching the entire MeasureResult (which contains the updated widget with
computed TextLayout), subsequent frames can skip all text measurement. -/

/-- Measure a widget tree with caching.
    Returns cached result if available, otherwise computes and caches.
    Uses OpaqueBox to store the widget to avoid circular dependencies. -/
def measureWidgetCached {M : Type → Type} [Monad M] [TextMeasurer M] [MonadLiftT IO M]
    (cache : IO.Ref MeasureCache) (w : Widget) (availWidth availHeight : Float)
    : M MeasureResult := do
  let key := { widgetId := w.id, availWidth, availHeight : MeasureCacheKey }
  let cacheState ← liftM (cache.get : IO MeasureCache)
  match cacheState.find? key with
  | some cached =>
    -- Cache hit: touch for LRU and return
    liftM (cache.modify fun c => c.touch key : IO Unit)
    -- Reconstruct MeasureResult from cached data
    let widget := unsafe OpaqueBox.get cached.widgetBox
    pure { node := cached.node, widget }
  | none =>
    -- Cache miss: measure and store
    let result ← measureWidget w availWidth availHeight
    let cached : CachedMeasurement := {
      node := result.node
      widgetBox := OpaqueBox.mk result.widget
      generation := 0
    }
    liftM (cache.modify fun c => c.insert key cached : IO Unit)
    pure result

/-- Compute intrinsic size with caching.
    Returns (width, height, updatedWidget) where the widget contains cached TextLayouts.
    This solves the double-traversal problem in centered layout mode. -/
def intrinsicSizeCached {M : Type → Type} [Monad M] [TextMeasurer M] [MonadLiftT IO M]
    (cache : IO.Ref IntrinsicCache) (w : Widget) : M (Float × Float × Widget) := do
  let key := { widgetId := w.id : IntrinsicCacheKey }
  let cacheState ← liftM (cache.get : IO IntrinsicCache)
  match cacheState.find? key with
  | some cached =>
    liftM (cache.modify fun c => c.touch key : IO Unit)
    let widget := unsafe OpaqueBox.get cached.widgetBox
    pure (cached.width, cached.height, widget)
  | none =>
    let (intrW, intrH, updatedWidget) ← intrinsicSizeWithWidget w
    let cached : CachedIntrinsicSize := {
      width := intrW
      height := intrH
      widgetBox := OpaqueBox.mk updatedWidget
      generation := 0
    }
    liftM (cache.modify fun c => c.insert key cached : IO Unit)
    pure (intrW, intrH, updatedWidget)

end Afferent.Arbor

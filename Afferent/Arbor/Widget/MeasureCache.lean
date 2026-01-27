/-
  Arbor Widget Measure Cache
  Frame-to-frame caching for widget measurement results.
  Avoids repeated text measurement when widget trees don't change.

  This module can safely import Widget/Measure because it's consumed by
  UIRunner.lean, not by Canvas/Context.lean (which would create a cycle).
-/
import Afferent.Arbor.Widget.Measure
import Afferent.Text.Measurer
import Std.Data.HashMap

namespace Afferent.Arbor

/-! ## Measure Cache

Widget measurement is expensive due to text layout computation (FreeType calls).
This cache stores MeasureResult across frames to avoid redundant measurement.

Widget IDs are auto-generated sequentially (0, 1, 2...) by `freshId` each frame.
Same widget tree structure -> same IDs, so caching by (widgetId, constraints) works
when the tree structure is stable.

Key insight: `measureWidget` already reuses TextLayout if present in the widget.
By caching the entire MeasureResult (which contains the updated widget with
computed TextLayout), we get text layout reuse automatically. -/

/-- Key for looking up cached measurements.
    Combines widget ID with available space constraints. -/
structure MeasureCacheKey where
  widgetId : Nat
  availWidth : Float
  availHeight : Float
deriving BEq

/-- Hash a Float by converting to UInt64 bits with quantization for stability. -/
private def hashFloat (f : Float) : UInt64 :=
  -- Quantize to 0.1 precision to reduce cache misses from tiny variations
  (f * 10).toUInt64

instance : Hashable MeasureCacheKey where
  hash k :=
    let h1 := hash k.widgetId
    let h2 := hashFloat k.availWidth
    let h3 := hashFloat k.availHeight
    mixHash (mixHash h1 h2) h3

/-- Default maximum number of cached entries. -/
def defaultMeasureCacheCapacity : Nat := 512

/-- LRU cache entry for measure results. -/
structure MeasureCacheEntry where
  value : MeasureResult
  prev : Option MeasureCacheKey := none
  next : Option MeasureCacheKey := none

/-- Persistent cache for measurement results across frames.
    Uses LRU eviction strategy. -/
structure MeasureCache where
  cache : Std.HashMap MeasureCacheKey MeasureCacheEntry := {}
  head : Option MeasureCacheKey := none
  tail : Option MeasureCacheKey := none
  capacity : Nat := defaultMeasureCacheCapacity
  hits : Nat := 0
  misses : Nat := 0

namespace MeasureCache

/-- Create an empty measure cache. -/
def empty : MeasureCache :=
  { cache := {}, head := none, tail := none, capacity := defaultMeasureCacheCapacity }

/-- Look up a cached measurement. -/
def find? (mc : MeasureCache) (key : MeasureCacheKey) : Option MeasureResult :=
  (mc.cache[key]?).map (·.value)

private def updateEntry (mc : MeasureCache) (key : MeasureCacheKey) (entry : MeasureCacheEntry) : MeasureCache :=
  { mc with cache := mc.cache.insert key entry }

private def detach (mc : MeasureCache) (entry : MeasureCacheEntry) : MeasureCache :=
  let prev := entry.prev
  let next := entry.next
  let mc :=
    match prev with
    | some p =>
      match mc.cache[p]? with
        | some pEntry => updateEntry mc p { pEntry with next := next }
        | none => mc
    | none =>
        { mc with head := next }
  let mc :=
    match next with
    | some n =>
      match mc.cache[n]? with
        | some nEntry => updateEntry mc n { nEntry with prev := prev }
        | none => mc
    | none =>
        { mc with tail := prev }
  mc

private def appendToTail (mc : MeasureCache) (key : MeasureCacheKey) (entry : MeasureCacheEntry) : MeasureCache :=
  match mc.tail with
  | none =>
      let entry' := { entry with prev := none, next := none }
      { mc with cache := mc.cache.insert key entry', head := some key, tail := some key }
  | some tailKey =>
      match mc.cache[tailKey]? with
      | some tailEntry =>
          let mc := updateEntry mc tailKey { tailEntry with next := some key }
          let entry' := { entry with prev := some tailKey, next := none }
          { mc with cache := mc.cache.insert key entry', tail := some key }
      | none =>
          let entry' := { entry with prev := none, next := none }
          { mc with cache := mc.cache.insert key entry', head := some key, tail := some key }

private def removeEntry (mc : MeasureCache) (key : MeasureCacheKey) (entry : MeasureCacheEntry) : MeasureCache :=
  let prev := entry.prev
  let next := entry.next
  let mc :=
    match prev with
    | some p =>
      match mc.cache[p]? with
        | some pEntry => updateEntry mc p { pEntry with next := next }
        | none => mc
    | none =>
        { mc with head := next }
  let mc :=
    match next with
    | some n =>
      match mc.cache[n]? with
        | some nEntry => updateEntry mc n { nEntry with prev := prev }
        | none => mc
    | none =>
        { mc with tail := prev }
  { mc with cache := mc.cache.erase key }

private def evictIfNeeded (mc : MeasureCache) : MeasureCache :=
  if mc.cache.size <= mc.capacity then mc
  else
    match mc.head with
    | none => mc
    | some headKey =>
        match mc.cache[headKey]? with
        | none => { mc with head := none, tail := none }
        | some headEntry => removeEntry mc headKey headEntry

/-- Mark an entry as most-recently used. -/
def touch (mc : MeasureCache) (key : MeasureCacheKey) : MeasureCache :=
  match mc.cache[key]? with
  | none => mc
  | some entry =>
      if mc.tail == some key then
        mc
      else
        let mc := detach mc entry
        appendToTail mc key entry

/-- Insert or update a cached measurement. -/
def insert (mc : MeasureCache) (key : MeasureCacheKey) (result : MeasureResult) : MeasureCache :=
  let mc :=
    match mc.cache[key]? with
    | some entry =>
        let entry' := { entry with value := result }
        touch (updateEntry mc key entry') key
    | none =>
        let entry : MeasureCacheEntry := { value := result }
        appendToTail mc key entry
  evictIfNeeded mc

/-- Record a cache hit. -/
def recordHit (mc : MeasureCache) : MeasureCache :=
  { mc with hits := mc.hits + 1 }

/-- Record a cache miss. -/
def recordMiss (mc : MeasureCache) : MeasureCache :=
  { mc with misses := mc.misses + 1 }

/-- Number of entries in the cache. -/
def size (mc : MeasureCache) : Nat := mc.cache.size

/-- Clear all cached entries. -/
def clear (mc : MeasureCache) : MeasureCache :=
  { mc with cache := {}, head := none, tail := none, hits := 0, misses := 0 }

/-- Get hit rate as a percentage (0-100). -/
def hitRate (mc : MeasureCache) : Float :=
  let total := mc.hits + mc.misses
  if total == 0 then 0 else (mc.hits.toFloat / total.toFloat) * 100

end MeasureCache

/-! ## Intrinsic Size Cache

For centered layout, we also need to cache intrinsic size computation.
This avoids the double traversal problem. -/

/-- Key for caching intrinsic size by widget ID. -/
structure IntrinsicCacheKey where
  widgetId : Nat
deriving BEq, Hashable

/-- Result of intrinsicSizeWithWidget: dimensions plus updated widget with TextLayouts. -/
structure IntrinsicResult where
  width : Float
  height : Float
  widget : Widget
deriving Inhabited

/-- LRU cache entry for intrinsic size. -/
structure IntrinsicCacheEntry where
  value : IntrinsicResult
  prev : Option IntrinsicCacheKey := none
  next : Option IntrinsicCacheKey := none

/-- Cache for intrinsic size computation. -/
structure IntrinsicCache where
  cache : Std.HashMap IntrinsicCacheKey IntrinsicCacheEntry := {}
  head : Option IntrinsicCacheKey := none
  tail : Option IntrinsicCacheKey := none
  capacity : Nat := defaultMeasureCacheCapacity
  hits : Nat := 0
  misses : Nat := 0

namespace IntrinsicCache

def empty : IntrinsicCache :=
  { cache := {}, head := none, tail := none, capacity := defaultMeasureCacheCapacity }

def find? (ic : IntrinsicCache) (key : IntrinsicCacheKey) : Option IntrinsicResult :=
  (ic.cache[key]?).map (·.value)

private def updateEntry (ic : IntrinsicCache) (key : IntrinsicCacheKey) (entry : IntrinsicCacheEntry) : IntrinsicCache :=
  { ic with cache := ic.cache.insert key entry }

private def appendToTail (ic : IntrinsicCache) (key : IntrinsicCacheKey) (entry : IntrinsicCacheEntry) : IntrinsicCache :=
  match ic.tail with
  | none =>
      let entry' := { entry with prev := none, next := none }
      { ic with cache := ic.cache.insert key entry', head := some key, tail := some key }
  | some tailKey =>
      match ic.cache[tailKey]? with
      | some tailEntry =>
          let ic := updateEntry ic tailKey { tailEntry with next := some key }
          let entry' := { entry with prev := some tailKey, next := none }
          { ic with cache := ic.cache.insert key entry', tail := some key }
      | none =>
          let entry' := { entry with prev := none, next := none }
          { ic with cache := ic.cache.insert key entry', head := some key, tail := some key }

private def removeEntry (ic : IntrinsicCache) (key : IntrinsicCacheKey) (entry : IntrinsicCacheEntry) : IntrinsicCache :=
  let prev := entry.prev
  let next := entry.next
  let ic :=
    match prev with
    | some p =>
      match ic.cache[p]? with
        | some pEntry => updateEntry ic p { pEntry with next := next }
        | none => ic
    | none =>
        { ic with head := next }
  let ic :=
    match next with
    | some n =>
      match ic.cache[n]? with
        | some nEntry => updateEntry ic n { nEntry with prev := prev }
        | none => ic
    | none =>
        { ic with tail := prev }
  { ic with cache := ic.cache.erase key }

private def detach (ic : IntrinsicCache) (entry : IntrinsicCacheEntry) : IntrinsicCache :=
  let prev := entry.prev
  let next := entry.next
  let ic :=
    match prev with
    | some p =>
      match ic.cache[p]? with
        | some pEntry => updateEntry ic p { pEntry with next := next }
        | none => ic
    | none =>
        { ic with head := next }
  let ic :=
    match next with
    | some n =>
      match ic.cache[n]? with
        | some nEntry => updateEntry ic n { nEntry with prev := prev }
        | none => ic
    | none =>
        { ic with tail := prev }
  ic

private def evictIfNeeded (ic : IntrinsicCache) : IntrinsicCache :=
  if ic.cache.size <= ic.capacity then ic
  else
    match ic.head with
    | none => ic
    | some headKey =>
        match ic.cache[headKey]? with
        | none => { ic with head := none, tail := none }
        | some headEntry => removeEntry ic headKey headEntry

def touch (ic : IntrinsicCache) (key : IntrinsicCacheKey) : IntrinsicCache :=
  match ic.cache[key]? with
  | none => ic
  | some entry =>
      if ic.tail == some key then
        ic
      else
        let ic := detach ic entry
        appendToTail ic key entry

def insert (ic : IntrinsicCache) (key : IntrinsicCacheKey) (result : IntrinsicResult) : IntrinsicCache :=
  let ic :=
    match ic.cache[key]? with
    | some entry =>
        let entry' := { entry with value := result }
        touch (updateEntry ic key entry') key
    | none =>
        let entry : IntrinsicCacheEntry := { value := result }
        appendToTail ic key entry
  evictIfNeeded ic

def recordHit (ic : IntrinsicCache) : IntrinsicCache :=
  { ic with hits := ic.hits + 1 }

def recordMiss (ic : IntrinsicCache) : IntrinsicCache :=
  { ic with misses := ic.misses + 1 }

def size (ic : IntrinsicCache) : Nat := ic.cache.size

def clear (ic : IntrinsicCache) : IntrinsicCache :=
  { ic with cache := {}, head := none, tail := none, hits := 0, misses := 0 }

def hitRate (ic : IntrinsicCache) : Float :=
  let total := ic.hits + ic.misses
  if total == 0 then 0 else (ic.hits.toFloat / total.toFloat) * 100

end IntrinsicCache

/-! ## Cached Measurement Functions -/

/-- Cached version of measureWidget specialized for FontReaderT IO.
    Checks the cache first; on miss, measures and stores result. -/
def measureWidgetCached (cache : IO.Ref MeasureCache) (w : Widget) (availWidth availHeight : Float)
    : Afferent.FontReaderT IO MeasureResult := do
  let key : MeasureCacheKey := { widgetId := w.id, availWidth, availHeight }
  let cacheState ← cache.get
  match cacheState.find? key with
  | some result =>
    cache.modify fun c => (c.touch key).recordHit
    pure result
  | none =>
    let result ← measureWidget w availWidth availHeight
    cache.modify fun c => (c.insert key result).recordMiss
    pure result

/-- Cached version of intrinsicSizeWithWidget specialized for FontReaderT IO.
    Checks the cache first; on miss, computes and stores result. -/
def intrinsicSizeCached (cache : IO.Ref IntrinsicCache) (w : Widget)
    : Afferent.FontReaderT IO IntrinsicResult := do
  let key : IntrinsicCacheKey := { widgetId := w.id }
  let cacheState ← cache.get
  match cacheState.find? key with
  | some result =>
    cache.modify fun c => (c.touch key).recordHit
    pure result
  | none =>
    let (width, height, updatedWidget) ← intrinsicSizeWithWidget w
    let result : IntrinsicResult := { width, height, widget := updatedWidget }
    cache.modify fun c => (c.insert key result).recordMiss
    pure result

end Afferent.Arbor

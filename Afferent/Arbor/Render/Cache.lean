/-
  Arbor Render Command Cache
  Types for caching CustomSpec render commands across frames.
  Separated from Collect.lean to avoid circular dependencies.
-/
import Afferent.Arbor.Render.Command
import Std.Data.HashMap
import Trellis

namespace Afferent.Arbor

/-! ## Render Command Caching

Automatic caching of CustomSpec.collect output at the framework level.
All widgets get caching without changes to individual widget implementations.

Cache is keyed by widget name (from registerComponentW) + layout hash.
When data changes, dynWidget rebuilds and generates a new widget name,
causing a cache miss for the new name. -/

/-- Hash a layout rect for cache key comparison.
    Combines position and size into a single hash value. -/
def hashLayoutRect (r : Trellis.LayoutRect) : UInt64 :=
  let h1 := r.x.toUInt64
  let h2 := r.y.toUInt64
  let h3 := r.width.toUInt64
  let h4 := r.height.toUInt64
  h1 ^^^ (h2 * 31) ^^^ (h3 * 961) ^^^ (h4 * 29791)

/-- Key type for render command caching. -/
abbrev CacheKey := UInt64

private def keyTagMask : UInt64 := 0x8000000000000000
private def keyTagClearMask : UInt64 := 0x7fffffffffffffff

private def mix64 (x : UInt64) : UInt64 :=
  let z1 := x + (0x9e3779b97f4a7c15 : UInt64)
  let z2 := (z1 ^^^ (z1 >>> 30)) * (0xbf58476d1ce4e5b9 : UInt64)
  let z3 := (z2 ^^^ (z2 >>> 27)) * (0x94d049bb133111eb : UInt64)
  z3 ^^^ (z3 >>> 31)

private def hashCombine (a b : UInt64) : UInt64 :=
  let salt : UInt64 := 0x9e3779b97f4a7c15
  mix64 (a ^^^ (b + salt) ^^^ (a <<< 6) ^^^ (a >>> 2))

/-- Root path key for unnamed widgets. -/
def rootPathKey : CacheKey := 0

/-- Build a child path key by mixing parent key and index. -/
def childPathKey (parent : CacheKey) (index : Nat) : CacheKey :=
  let idx := UInt64.ofNat (index + 1)
  (hashCombine parent idx) &&& keyTagClearMask

/-- Cache key for a named widget. -/
def nameCacheKey (name : String) : CacheKey :=
  let base := mix64 (hash name)
  (base &&& keyTagClearMask) ||| keyTagMask

/-- Cached render commands for a CustomSpec widget. -/
structure CachedRenderCommands where
  commands : Array RenderCommand
  layoutHash : UInt64
  /-- Generation counter from dynWidget. When generation changes, cache is stale.
      This allows animated widgets to update in place rather than creating new entries. -/
  generation : Nat := 0

/-- Default maximum number of cached entries. -/
def defaultRenderCacheCapacity : Nat := 1024

/-- LRU cache entry for render commands. -/
structure RenderCacheEntry where
  value : CachedRenderCommands
  prev : Option CacheKey := none
  next : Option CacheKey := none

/-- Persistent cache for render commands across frames.
    Keyed by hashed widget names or path keys. -/
structure RenderCache where
  cache : Std.HashMap CacheKey RenderCacheEntry := {}
  head : Option CacheKey := none
  tail : Option CacheKey := none
  capacity : Nat := defaultRenderCacheCapacity

namespace RenderCache

/-- Create an empty render cache. -/
def empty : RenderCache := { cache := {}, head := none, tail := none, capacity := defaultRenderCacheCapacity }

/-- Look up cached commands for a widget name. -/
def find? (rc : RenderCache) (key : CacheKey) : Option CachedRenderCommands :=
  (rc.cache[key]?).map (·.value)

private def updateEntry (rc : RenderCache) (key : CacheKey) (entry : RenderCacheEntry) : RenderCache :=
  { rc with cache := rc.cache.insert key entry }

private def detach (rc : RenderCache) (entry : RenderCacheEntry) : RenderCache :=
  let prev := entry.prev
  let next := entry.next
  let rc :=
    match prev with
    | some p =>
      match rc.cache[p]? with
        | some pEntry => updateEntry rc p { pEntry with next := next }
        | none => rc
    | none =>
        { rc with head := next }
  let rc :=
    match next with
    | some n =>
      match rc.cache[n]? with
        | some nEntry => updateEntry rc n { nEntry with prev := prev }
        | none => rc
    | none =>
        { rc with tail := prev }
  rc

private def appendToTail (rc : RenderCache) (key : CacheKey) (entry : RenderCacheEntry) : RenderCache :=
  match rc.tail with
  | none =>
      let entry' := { entry with prev := none, next := none }
      { rc with cache := rc.cache.insert key entry', head := some key, tail := some key }
  | some tailKey =>
      match rc.cache[tailKey]? with
      | some tailEntry =>
          let rc := updateEntry rc tailKey { tailEntry with next := some key }
          let entry' := { entry with prev := some tailKey, next := none }
          { rc with cache := rc.cache.insert key entry', tail := some key }
      | none =>
          let entry' := { entry with prev := none, next := none }
          { rc with cache := rc.cache.insert key entry', head := some key, tail := some key }

private def removeEntry (rc : RenderCache) (key : CacheKey) (entry : RenderCacheEntry) : RenderCache :=
  let prev := entry.prev
  let next := entry.next
  let rc :=
    match prev with
    | some p =>
      match rc.cache[p]? with
        | some pEntry => updateEntry rc p { pEntry with next := next }
        | none => rc
    | none =>
        { rc with head := next }
  let rc :=
    match next with
    | some n =>
      match rc.cache[n]? with
        | some nEntry => updateEntry rc n { nEntry with prev := prev }
        | none => rc
    | none =>
        { rc with tail := prev }
  { rc with cache := rc.cache.erase key }

private def evictIfNeeded (rc : RenderCache) : RenderCache :=
  if rc.cache.size <= rc.capacity then rc
  else
    match rc.head with
    | none => rc
    | some headKey =>
        match rc.cache[headKey]? with
        | none => { rc with head := none, tail := none }
        | some headEntry => removeEntry rc headKey headEntry

/-- Mark an entry as most-recently used. -/
def touch (rc : RenderCache) (key : CacheKey) : RenderCache :=
  match rc.cache[key]? with
  | none => rc
  | some entry =>
      if rc.tail == some key then
        rc
      else
        let rc := detach rc entry
        appendToTail rc key entry

/-- Insert or update cached commands for a widget name. -/
def insert (rc : RenderCache) (key : CacheKey) (cached : CachedRenderCommands) : RenderCache :=
  let rc :=
    match rc.cache[key]? with
    | some entry =>
        let entry' := { entry with value := cached }
        touch (updateEntry rc key entry') key
    | none =>
        let entry : RenderCacheEntry := { value := cached }
        appendToTail rc key entry
  evictIfNeeded rc

/-- Number of entries in the cache. -/
def size (rc : RenderCache) : Nat := rc.cache.size

/-- Clear all cached entries. -/
def clear (rc : RenderCache) : RenderCache := { rc with cache := {}, head := none, tail := none }

end RenderCache

end Afferent.Arbor

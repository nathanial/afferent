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
  prev : Option String := none
  next : Option String := none

/-- Persistent cache for render commands across frames.
    Keyed by widget name (from registerComponentW). -/
structure RenderCache where
  cache : Std.HashMap String RenderCacheEntry := {}
  head : Option String := none
  tail : Option String := none
  capacity : Nat := defaultRenderCacheCapacity

namespace RenderCache

/-- Create an empty render cache. -/
def empty : RenderCache := { cache := {}, head := none, tail := none, capacity := defaultRenderCacheCapacity }

/-- Look up cached commands for a widget name. -/
def find? (rc : RenderCache) (name : String) : Option CachedRenderCommands :=
  (rc.cache[name]?).map (·.value)

private def updateEntry (rc : RenderCache) (name : String) (entry : RenderCacheEntry) : RenderCache :=
  { rc with cache := rc.cache.insert name entry }

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

private def appendToTail (rc : RenderCache) (name : String) (entry : RenderCacheEntry) : RenderCache :=
  match rc.tail with
  | none =>
      let entry' := { entry with prev := none, next := none }
      { rc with cache := rc.cache.insert name entry', head := some name, tail := some name }
  | some tailName =>
      match rc.cache[tailName]? with
      | some tailEntry =>
          let rc := updateEntry rc tailName { tailEntry with next := some name }
          let entry' := { entry with prev := some tailName, next := none }
          { rc with cache := rc.cache.insert name entry', tail := some name }
      | none =>
          let entry' := { entry with prev := none, next := none }
          { rc with cache := rc.cache.insert name entry', head := some name, tail := some name }

private def removeEntry (rc : RenderCache) (name : String) (entry : RenderCacheEntry) : RenderCache :=
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
  { rc with cache := rc.cache.erase name }

private def evictIfNeeded (rc : RenderCache) : RenderCache :=
  if rc.cache.size <= rc.capacity then rc
  else
    match rc.head with
    | none => rc
    | some headName =>
        match rc.cache[headName]? with
        | none => { rc with head := none, tail := none }
        | some headEntry => removeEntry rc headName headEntry

/-- Mark an entry as most-recently used. -/
def touch (rc : RenderCache) (name : String) : RenderCache :=
  match rc.cache[name]? with
  | none => rc
  | some entry =>
      if rc.tail == some name then
        rc
      else
        let rc := detach rc entry
        appendToTail rc name entry

/-- Insert or update cached commands for a widget name. -/
def insert (rc : RenderCache) (name : String) (cached : CachedRenderCommands) : RenderCache :=
  let rc :=
    match rc.cache[name]? with
    | some entry =>
        let entry' := { entry with value := cached }
        touch (updateEntry rc name entry') name
    | none =>
        let entry : RenderCacheEntry := { value := cached }
        appendToTail rc name entry
  evictIfNeeded rc

/-- Number of entries in the cache. -/
def size (rc : RenderCache) : Nat := rc.cache.size

/-- Clear all cached entries. -/
def clear (rc : RenderCache) : RenderCache := { rc with cache := {}, head := none, tail := none }

end RenderCache

end Afferent.Arbor

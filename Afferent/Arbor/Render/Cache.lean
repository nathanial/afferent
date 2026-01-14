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

/-- Persistent cache for render commands across frames.
    Keyed by widget name (from registerComponentW). -/
structure RenderCache where
  cache : Std.HashMap String CachedRenderCommands := {}

namespace RenderCache

/-- Create an empty render cache. -/
def empty : RenderCache := { cache := {} }

/-- Look up cached commands for a widget name. -/
def find? (rc : RenderCache) (name : String) : Option CachedRenderCommands :=
  rc.cache[name]?

/-- Insert or update cached commands for a widget name. -/
def insert (rc : RenderCache) (name : String) (cached : CachedRenderCommands) : RenderCache :=
  { rc with cache := rc.cache.insert name cached }

/-- Number of entries in the cache. -/
def size (rc : RenderCache) : Nat := rc.cache.size

/-- Clear all cached entries. -/
def clear (rc : RenderCache) : RenderCache := { rc with cache := {} }

end RenderCache

end Afferent.Arbor

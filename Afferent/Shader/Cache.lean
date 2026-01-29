/-
  Afferent Shader Cache
  Pipeline caching for compiled shader fragments.
-/
import Afferent.Shader.Fragment
import Afferent.Shader.Registry
import Afferent.FFI.Fragment
import Afferent.FFI.Renderer
import Std.Data.HashMap

open Shader

namespace Afferent.Shader

open Std
open Afferent.FFI

/-- Cache for compiled fragment pipelines.
    Maps fragment hashes to their compiled GPU pipelines. -/
structure FragmentCache where
  pipelines : HashMap UInt64 FragmentPipeline := {}
deriving Inhabited

namespace FragmentCache

/-- Create an empty cache. -/
def empty : FragmentCache := {}

/-- Get a cached pipeline by fragment hash. -/
def get? (cache : FragmentCache) (hash : UInt64) : Option FragmentPipeline :=
  cache.pipelines.get? hash

/-- Insert a pipeline into the cache. -/
def insert (cache : FragmentCache) (hash : UInt64) (pipeline : FragmentPipeline) : FragmentCache :=
  { pipelines := cache.pipelines.insert hash pipeline }

/-- Check if a pipeline is cached. -/
def contains (cache : FragmentCache) (hash : UInt64) : Bool :=
  cache.pipelines.contains hash

/-- Get the number of cached pipelines. -/
def size (cache : FragmentCache) : Nat :=
  cache.pipelines.size

end FragmentCache

/-! ## Pipeline Compilation -/

/-- Compile a fragment and cache the result.
    Returns the compiled pipeline (or existing cached one), or None if compilation fails. -/
def compileAndCacheFragment (cache : FragmentCache) (renderer : Renderer)
    (fragment : ShaderFragment) : IO (Option FragmentPipeline × FragmentCache) := do
  -- Check if already cached
  if let some pipeline := cache.get? fragment.hash then
    return (some pipeline, cache)

  -- Compile the fragment
  let result ← Fragment.compile renderer fragment.name fragment.paramsStructCode
      fragment.functionCode fragment.primitive.toUInt32
      fragment.instanceCount.toUInt32 fragment.paramsFloatCount.toUInt32

  match result with
  | some pipeline =>
    let newCache := cache.insert fragment.hash pipeline
    return (some pipeline, newCache)
  | none =>
    return (none, cache)

/-- Get or compile a fragment pipeline using a registry for lookup.
    Looks up the fragment definition from the provided registry, then compiles if needed. -/
def getOrCompileWithRegistry (cache : FragmentCache) (renderer : Renderer)
    (registry : FragmentRegistry) (hash : UInt64)
    : IO (Option FragmentPipeline × FragmentCache) := do
  -- Check if already cached
  if let some pipeline := cache.get? hash then
    return (some pipeline, cache)

  -- Look up fragment definition from registry
  match registry.get? hash with
  | some fragment =>
    compileAndCacheFragment cache renderer fragment
  | none =>
    -- Fragment not registered - this is an error but we handle gracefully
    return (none, cache)

/-- Get or compile a fragment pipeline using the global registry.
    This is the primary entry point for fragment compilation. -/
def getOrCompileGlobal (cache : FragmentCache) (renderer : Renderer) (hash : UInt64)
    : IO (Option FragmentPipeline × FragmentCache) := do
  -- Check if already cached
  if let some pipeline := cache.get? hash then
    return (some pipeline, cache)

  -- Look up fragment definition from global registry
  match ← lookupFragment hash with
  | some fragment =>
    compileAndCacheFragment cache renderer fragment
  | none =>
    -- Fragment not registered - this is an error but we handle gracefully
    return (none, cache)

end Afferent.Shader

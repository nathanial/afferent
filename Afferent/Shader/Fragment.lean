/-
  Afferent Shader Fragment
  Defines shader fragments that allow widget authors to write custom GPU code.
-/
import Std.Data.HashMap

namespace Afferent.Shader

open Std

/-- Primitive types that fragments can generate. -/
inductive FragmentPrimitive where
  | circle    -- Returns CircleResult(center, radius, color)
  | rect      -- Future
  | arc       -- Future
deriving Repr, BEq, Hashable, Inhabited

namespace FragmentPrimitive

/-- Convert primitive type to UInt32 for FFI. -/
def toUInt32 : FragmentPrimitive → UInt32
  | .circle => 0
  | .rect => 1
  | .arc => 2

end FragmentPrimitive

/-- A shader fragment definition.
    Contains the Metal shader code that computes primitive properties from parameters + index. -/
structure ShaderFragment where
  /-- Unique identifier for this fragment. -/
  name : String
  /-- Output primitive type. -/
  primitive : FragmentPrimitive
  /-- Number of floats in the parameter struct (per instance). -/
  paramsFloatCount : Nat
  /-- Metal struct definition for parameters (e.g., "struct HelixParams { float2 center; ... };"). -/
  paramsStructCode : String
  /-- Fragment function body (computes primitive from index and params). -/
  functionCode : String
  /-- Number of primitives generated per draw call. -/
  instanceCount : Nat
deriving Repr, BEq, Inhabited

namespace ShaderFragment

/-- Compute a hash for this fragment (for caching compiled pipelines). -/
def hash (f : ShaderFragment) : UInt64 :=
  let h1 := Hashable.hash f.name
  let h2 := Hashable.hash f.paramsStructCode
  let h3 := Hashable.hash f.functionCode
  let h4 := Hashable.hash f.primitive
  let h5 := Hashable.hash f.paramsFloatCount
  -- Combine hashes using FNV-1a style mixing
  let mix (a b : UInt64) : UInt64 := (a ^^^ b) * 0x100000001b3
  mix (mix (mix (mix h1 h2) h3) h4) h5

end ShaderFragment

/-- Define a circle-generating fragment.
    The function body should compute and return a CircleResult.

    Example:
    ```
    def helixFragment : ShaderFragment := fragmentCircle "helix" 16 8
      "struct HelixParams { float2 center; float size; float time; float4 color; };"
      "uint pair = idx / 2; ..."
    ```
-/
def fragmentCircle (name : String) (instanceCount : Nat) (paramsFloatCount : Nat)
    (paramsStruct : String) (functionBody : String) : ShaderFragment :=
  { name
    primitive := .circle
    paramsFloatCount
    paramsStructCode := paramsStruct
    functionCode := functionBody
    instanceCount }

/-! ## Global Fragment Registry -/

/-- Global registry of all defined shader fragments.
    Fragments auto-register when created via `fragmentCircleRegistered`. -/
initialize globalFragmentRegistry : IO.Ref (HashMap UInt64 ShaderFragment) ← IO.mkRef {}

/-- Register a fragment in the global registry. -/
def registerFragment (f : ShaderFragment) : IO Unit :=
  globalFragmentRegistry.modify (·.insert f.hash f)

/-- Look up a fragment by hash from the global registry. -/
def lookupFragment (hash : UInt64) : IO (Option ShaderFragment) := do
  let reg ← globalFragmentRegistry.get
  pure (reg.get? hash)

/-- Define a circle-generating fragment and register it globally.
    Use this for fragments that will be used with `drawFragment` commands. -/
def fragmentCircleRegistered (name : String) (instanceCount : Nat) (paramsFloatCount : Nat)
    (paramsStruct : String) (functionBody : String) : IO ShaderFragment := do
  let f := fragmentCircle name instanceCount paramsFloatCount paramsStruct functionBody
  registerFragment f
  pure f

end Afferent.Shader

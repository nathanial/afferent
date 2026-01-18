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
  /-- Number of floats in the parameter struct (per instance, padded for Metal layout). -/
  paramsFloatCount : Nat
  /-- Number of floats in the packed parameter struct (no padding). -/
  paramsPackedFloatCount : Nat
  /-- Mapping from packed float index to padded float index. -/
  paramsPackOffsets : Array Nat
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
  let h6 := Hashable.hash f.paramsPackedFloatCount
  let h7 := Hashable.hash f.paramsPackOffsets
  -- Combine hashes using FNV-1a style mixing
  let mix (a b : UInt64) : UInt64 := (a ^^^ b) * 0x100000001b3
  mix (mix (mix (mix (mix (mix h1 h2) h3) h4) h5) h6) h7

private def foldNat {α} (n : Nat) (init : α) (f : Nat → α → α) : α :=
  Nat.rec (motive := fun _ => α) init (fun i acc => f i acc) n

/-- Expand a packed params array to match Metal struct padding. -/
def padParams (f : ShaderFragment) (params : Array Float) : Array Float :=
  if f.paramsPackedFloatCount == f.paramsFloatCount then
    params
  else if f.paramsPackedFloatCount == 0 then
    params
  else if f.paramsPackOffsets.size != f.paramsPackedFloatCount then
    params
  else if params.size % f.paramsPackedFloatCount != 0 then
    params
  else
    let packed := f.paramsPackedFloatCount
    let padded := f.paramsFloatCount
    let batchCount := params.size / packed
    let out := foldNat (batchCount * padded) #[] (fun _ acc => acc.push 0.0)
    foldNat batchCount out fun batch acc =>
      let baseIn := batch * packed
      let baseOut := batch * padded
      foldNat packed acc fun i acc =>
        let outIdx := baseOut + f.paramsPackOffsets[i]!
        acc.set! outIdx (params[baseIn + i]!)

end ShaderFragment

private def identityOffsets (n : Nat) : Array Nat :=
  Nat.rec (motive := fun _ => Array Nat) #[] (fun i acc => acc.push i) n

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
    paramsPackedFloatCount := paramsFloatCount
    paramsPackOffsets := identityOffsets paramsFloatCount
    paramsStructCode := paramsStruct
    functionCode := functionBody
    instanceCount }

/-- Define a circle-generating fragment with explicit packing layout. -/
def fragmentCirclePacked (name : String) (instanceCount : Nat) (paramsFloatCount : Nat)
    (paramsPackedFloatCount : Nat) (paramsPackOffsets : Array Nat)
    (paramsStruct : String) (functionBody : String) : ShaderFragment :=
  { name
    primitive := .circle
    paramsFloatCount
    paramsPackedFloatCount
    paramsPackOffsets
    paramsStructCode := paramsStruct
    functionCode := functionBody
    instanceCount }

/-- Define a rectangle-generating fragment.
    The function body should compute and return a RectResult. -/
def fragmentRect (name : String) (instanceCount : Nat) (paramsFloatCount : Nat)
    (paramsStruct : String) (functionBody : String) : ShaderFragment :=
  { name
    primitive := .rect
    paramsFloatCount
    paramsPackedFloatCount := paramsFloatCount
    paramsPackOffsets := identityOffsets paramsFloatCount
    paramsStructCode := paramsStruct
    functionCode := functionBody
    instanceCount }

/-- Define a rectangle-generating fragment with explicit packing layout. -/
def fragmentRectPacked (name : String) (instanceCount : Nat) (paramsFloatCount : Nat)
    (paramsPackedFloatCount : Nat) (paramsPackOffsets : Array Nat)
    (paramsStruct : String) (functionBody : String) : ShaderFragment :=
  { name
    primitive := .rect
    paramsFloatCount
    paramsPackedFloatCount
    paramsPackOffsets
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

/-- Define a circle fragment with packing layout and register it globally. -/
def fragmentCircleRegisteredPacked (name : String) (instanceCount : Nat) (paramsFloatCount : Nat)
    (paramsPackedFloatCount : Nat) (paramsPackOffsets : Array Nat)
    (paramsStruct : String) (functionBody : String) : IO ShaderFragment := do
  let f := fragmentCirclePacked name instanceCount paramsFloatCount paramsPackedFloatCount paramsPackOffsets
    paramsStruct functionBody
  registerFragment f
  pure f

end Afferent.Shader

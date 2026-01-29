/-
  Afferent Shader Fragment
  Defines shader fragments that allow widget authors to write custom GPU code.

  This module re-exports types from the standalone Shader library and adds
  Afferent-specific functionality like the global fragment registry.
-/
import Shader.Fragment
import Std.Data.HashMap

namespace Afferent.Shader

open Std

/-! ## Global Fragment Registry -/

/-- Global registry of all defined shader fragments.
    Fragments auto-register when created via `fragmentCircleRegistered`. -/
initialize globalFragmentRegistry : IO.Ref (HashMap UInt64 _root_.Shader.ShaderFragment) ← IO.mkRef {}

/-- Register a fragment in the global registry. -/
def registerFragment (f : _root_.Shader.ShaderFragment) : IO Unit :=
  globalFragmentRegistry.modify (·.insert f.hash f)

/-- Look up a fragment by hash from the global registry. -/
def lookupFragment (hash : UInt64) : IO (Option _root_.Shader.ShaderFragment) := do
  let reg ← globalFragmentRegistry.get
  pure (reg.get? hash)

/-- Define a circle-generating fragment and register it globally.
    Use this for fragments that will be used with `drawFragment` commands. -/
def fragmentCircleRegistered (name : String) (instanceCount : Nat) (paramsFloatCount : Nat)
    (paramsStruct : String) (functionBody : String) : IO _root_.Shader.ShaderFragment := do
  let f := _root_.Shader.fragmentCircle name instanceCount paramsFloatCount paramsStruct functionBody
  registerFragment f
  pure f

/-- Define a circle fragment with packing layout and register it globally. -/
def fragmentCircleRegisteredPacked (name : String) (instanceCount : Nat) (paramsFloatCount : Nat)
    (paramsPackedFloatCount : Nat) (paramsPackOffsets : Array Nat)
    (paramsStruct : String) (functionBody : String) : IO _root_.Shader.ShaderFragment := do
  let f := _root_.Shader.fragmentCirclePacked name instanceCount paramsFloatCount paramsPackedFloatCount paramsPackOffsets
    paramsStruct functionBody
  registerFragment f
  pure f

end Afferent.Shader

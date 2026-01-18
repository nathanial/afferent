/-
  Afferent Shader Registry
  Registry for shader fragments. Fragments are registered explicitly
  and the registry is passed to the backend for pipeline compilation.
-/
import Afferent.Shader.Fragment
import Std.Data.HashMap

namespace Afferent.Shader

open Std

/-- Registry mapping fragment hashes to their definitions.
    Used for looking up fragment definitions at draw time
    to compile pipelines on first use. -/
structure FragmentRegistry where
  fragments : HashMap UInt64 ShaderFragment := {}
deriving Inhabited

namespace FragmentRegistry

/-- Create an empty registry. -/
def empty : FragmentRegistry := {}

/-- Register a fragment in the registry. -/
def register (reg : FragmentRegistry) (fragment : ShaderFragment) : FragmentRegistry :=
  { fragments := reg.fragments.insert fragment.hash fragment }

/-- Look up a fragment by its hash. -/
def get? (reg : FragmentRegistry) (hash : UInt64) : Option ShaderFragment :=
  reg.fragments.get? hash

/-- Check if a fragment is registered. -/
def contains (reg : FragmentRegistry) (hash : UInt64) : Bool :=
  reg.fragments.contains hash

/-- Get the number of registered fragments. -/
def size (reg : FragmentRegistry) : Nat :=
  reg.fragments.size

/-- Register multiple fragments at once. -/
def registerAll (reg : FragmentRegistry) (fragments : Array ShaderFragment) : FragmentRegistry :=
  fragments.foldl (init := reg) fun r f => r.register f

end FragmentRegistry

/-! ## Fragment Constructors -/

/-- Define a circle-generating fragment.

    Example:
    ```
    def helixFragment : ShaderFragment := Shader.makeCircleFragment "helix" 16 8
      "struct HelixParams { float2 center; float size; float time; float4 color; };"
      """
      uint pair = idx / 2;
      bool strand2 = (idx % 2) == 1;
      float y = (float(pair) / 8.0 - 0.5) * p.size * 0.7;
      float phase = p.time + float(pair) * M_PI_4;
      float sinP = sin(phase); float cosP = cos(phase);
      if (strand2) { sinP = -sinP; cosP = -cosP; }
      float depth = (cosP + 1.0) * 0.5;
      return CircleResult(p.center + float2(p.size * 0.3 * sinP, y),
                          p.size * 0.05 * (0.6 + 0.4 * depth),
                          p.color * float4(1, 1, 1, 0.4 + 0.6 * depth));
      """
    ```
-/
def makeCircleFragment (name : String) (instanceCount : Nat) (paramsFloatCount : Nat)
    (paramsStruct : String) (functionBody : String) : ShaderFragment :=
  fragmentCircle name instanceCount paramsFloatCount paramsStruct functionBody

end Afferent.Shader

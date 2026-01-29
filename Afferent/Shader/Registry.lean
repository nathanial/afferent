/-
  Afferent Shader Registry
  Registry for shader fragments. Fragments are registered explicitly
  and the registry is passed to the backend for pipeline compilation.

  This module re-exports from the standalone Shader library.
-/
import Shader.Registry
import Afferent.Shader.Fragment
import Std.Data.HashMap

namespace Afferent.Shader

open Std

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
    (paramsStruct : String) (functionBody : String) : _root_.Shader.ShaderFragment :=
  _root_.Shader.fragmentCircle name instanceCount paramsFloatCount paramsStruct functionBody

end Afferent.Shader

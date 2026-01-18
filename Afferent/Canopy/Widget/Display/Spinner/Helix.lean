/-
  Helix Spinner - DNA-like double helix rotating
-/
import Afferent.Canopy.Core
import Afferent.Canopy.Widget.Display.Spinner.Core
import Afferent.Shader

namespace Afferent.Canopy.Spinner

open Afferent.Arbor hiding Event
open Afferent.Shader
open Linalg

/-- Helix shader fragment definition.
    Computes 16 circles (8 pairs) for the DNA-like double helix.
    Parameters: center(2), size(1), time(1), color(4) = 8 floats.
    Note: time is raw seconds (not radians) to preserve precision for hue animation. -/
def helixFragment : ShaderFragment := fragmentCircle "helix" 16 8
  "struct HelixParams { float2 center; float size; float time; float4 color; };"
  "const float PI = 3.14159265359;\n\
   const float TWO_PI = PI * 2.0;\n\
   const float PI_4 = PI * 0.25;\n\
   uint pair = idx / 2u;\n\
   bool strand2 = (idx % 2u) == 1u;\n\
   float y = (float(pair) / 8.0 - 0.5) * p.size * 0.7;\n\
   float phase = p.time * TWO_PI + float(pair) * PI_4;\n\
   float sinP = sin(phase);\n\
   float cosP = cos(phase);\n\
   if (strand2) { sinP = -sinP; cosP = -cosP; }\n\
   float depth = (cosP + 1.0) * 0.5;\n\
   // Animate hue based on time and circle index (inline HSV to RGB)\n\
   float hue = fract(p.time * 0.3 + float(idx) * 0.04);\n\
   float4 K = float4(1.0, 2.0/3.0, 1.0/3.0, 3.0);\n\
   float3 hp = abs(fract(float3(hue, hue, hue) + K.xyz) * 6.0 - K.www);\n\
   float3 rgb = mix(K.xxx, clamp(hp - K.xxx, 0.0, 1.0), 0.8);\n\
   CircleResult result;\n\
   result.center = p.center + float2(p.size * 0.3 * sinP, y);\n\
   result.radius = p.size * 0.05 * (0.6 + 0.4 * depth);\n\
   result.color = float4(rgb, p.color.a * (0.4 + 0.6 * depth));\n\
   return result;"

/-- Register the helix fragment in the global registry at module load time. -/
initialize helixFragmentRegistration : Unit ← do
  registerFragment helixFragment

/-- Helix: DNA-like double helix using GPU shader fragment.
    Passes only 8 floats to GPU; the shader computes all 16 circle positions, sizes, and colors. -/
def helixSpec (t : Float) (color : Color) (dims : Dimensions) : CustomSpec := {
  measure := fun _ _ => (dims.size, dims.size)
  collect := fun layout =>
    let rect := layout.contentRect
    let cx := rect.x + dims.size / 2
    let cy := rect.y + dims.size / 2

    -- Only 8 floats: center(2), size(1), time(1), color(4)
    let params : Array Float := #[
      cx, cy,                             -- center
      dims.size,                          -- size
      t,                                  -- time (raw seconds, shader converts to radians)
      color.r, color.g, color.b, color.a  -- color
    ]

    RenderM.build do
      RenderM.drawFragment helixFragment.hash helixFragment.primitive.toUInt32
        params helixFragment.instanceCount.toUInt32
  draw := none
}

end Afferent.Canopy.Spinner

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
    Parameters: center(2), size(1), time(1), color(4) = 8 floats. -/
def helixFragment : ShaderFragment := fragmentCircle "helix" 16 8
  "struct HelixParams { float2 center; float size; float time; float4 color; };"
  "const float PI = 3.14159265359;\n\
   const float PI_4 = PI * 0.25;\n\
   uint pair = idx / 2u;\n\
   bool strand2 = (idx % 2u) == 1u;\n\
   float y = (float(pair) / 8.0 - 0.5) * p.size * 0.7;\n\
   float phase = p.time + float(pair) * PI_4;\n\
   float sinP = sin(phase);\n\
   float cosP = cos(phase);\n\
   if (strand2) { sinP = -sinP; cosP = -cosP; }\n\
   float depth = (cosP + 1.0) * 0.5;\n\
   CircleResult result;\n\
   result.center = p.center + float2(p.size * 0.3 * sinP, y);\n\
   result.radius = p.size * 0.05 * (0.6 + 0.4 * depth);\n\
   result.color = p.color * float4(1.0, 1.0, 1.0, 0.4 + 0.6 * depth);\n\
   return result;"

/-- Register the helix fragment in the global registry at module load time. -/
initialize helixFragmentRegistration : Unit ← do
  registerFragment helixFragment

/-- Precomputed y-offsets for helix dots (8 values, only depends on index). -/
private def helixYOffsets : Array Float := Id.run do
  let mut offsets : Array Float := Array.mkEmpty 8
  for i in [:8] do
    offsets := offsets.push (i.toFloat / 8.0 - 0.5)
  return offsets

/-- Precomputed phase bases for helix dots (8 values, only depends on index). -/
private def helixPhaseBases : Array Float := Id.run do
  let mut bases : Array Float := Array.mkEmpty 8
  for i in [:8] do
    bases := bases.push (i.toFloat * Float.pi / 4.0)
  return bases

/-- Helix: DNA-like double helix rotating.
    Uses fillCircleBatch for efficient rendering (single emit for all 16 circles).
    Uses trig identities: sin(x + π) = -sin(x), cos(x + π) = -cos(x). -/
def helixSpec (t : Float) (color : Color) (dims : Dimensions) : CustomSpec := {
  measure := fun _ _ => (dims.size, dims.size)
  collect := fun layout =>
    let rect := layout.contentRect
    let cx := rect.x + dims.size / 2
    let cy := rect.y + dims.size / 2
    let amplitude := dims.size * 0.3
    let dotRadius := dims.size * 0.05
    let yScale := dims.size * 0.7
    let basePhase := t * Float.twoPi

    -- Build all 16 circles (8 pairs) in a single batch
    -- Format: [cx, cy, radius, r, g, b, a] per circle (7 floats)
    RenderM.build do
      let mut data : Array Float := Array.mkEmpty (16 * 7)
      for i in [:8] do
        let yOffset := helixYOffsets[i]! * yScale
        let phase := basePhase + helixPhaseBases[i]!
        let sinP := Float.sin phase
        let cosP := Float.cos phase
        -- Strand 1
        let x1 := cx + amplitude * sinP
        let depth1 := (cosP + 1.0) / 2.0
        let radius1 := dotRadius * (0.6 + 0.4 * depth1)
        let alpha1 := 0.4 + 0.6 * depth1
        data := data.push x1 |>.push (cy + yOffset) |>.push radius1
                     |>.push color.r |>.push color.g |>.push color.b |>.push (color.a * alpha1)
        -- Strand 2 (180° offset): sin(p + π) = -sinP, cos(p + π) = -cosP
        let x2 := cx - amplitude * sinP  -- -sin(phase)
        let depth2 := (-cosP + 1.0) / 2.0  -- -cos(phase)
        let radius2 := dotRadius * (0.6 + 0.4 * depth2)
        let alpha2 := 0.4 + 0.6 * depth2
        data := data.push x2 |>.push (cy + yOffset) |>.push radius2
                     |>.push color.r |>.push color.g |>.push color.b |>.push (color.a * alpha2)
      RenderM.fillCircleBatch data 16
  draw := none
}

/-- Helix (Fragment version): DNA-like double helix using GPU shader fragment.
    Passes only 8 floats to GPU instead of building 112 floats on CPU.
    The fragment shader computes all 16 circle positions, sizes, and colors.

    NOTE: This requires fragment pipeline caching to be integrated with the canvas.
    Currently a demonstration of the API pattern. -/
def helixFragmentSpec (t : Float) (color : Color) (dims : Dimensions) : CustomSpec := {
  measure := fun _ _ => (dims.size, dims.size)
  collect := fun layout =>
    let rect := layout.contentRect
    let cx := rect.x + dims.size / 2
    let cy := rect.y + dims.size / 2

    -- Only 8 floats: center(2), size(1), time(1), color(4)
    -- vs 112 floats for fillCircleBatch (16 circles × 7 floats)
    let params : Array Float := #[
      cx, cy,                             -- center
      dims.size,                          -- size
      t * Float.twoPi,                    -- time (radians)
      color.r, color.g, color.b, color.a  -- color
    ]

    RenderM.build do
      RenderM.drawFragment helixFragment.hash helixFragment.primitive.toUInt32
        params helixFragment.instanceCount.toUInt32
  draw := none
}

end Afferent.Canopy.Spinner

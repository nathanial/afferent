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
  (include_str "shaders/helix_params.metal")
  (include_str "shaders/helix_body.metal")

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

/-
  Helix Spinner - DNA-like double helix rotating
  Uses the Shader DSL to generate GPU shader code.
-/
import Afferent.Canopy.Core
import Afferent.Canopy.Widget.Display.Spinner.Core
import Afferent.Shader.DSL

namespace Afferent.Canopy.Spinner

open Afferent.Arbor hiding Event
open Afferent.Shader
open Afferent.Shader.DSL hiding center size time color  -- Hide to avoid conflict with Arbor
open Linalg

-- Local aliases for DSL parameter accessors (avoid Arbor name conflicts)
private def pCenter : ShaderExpr .float2 := param "center" .float2
private def pSize : ShaderExpr .float := param "size" .float
private def pTime : ShaderExpr .float := param "time" .float
private def pColor : ShaderExpr .float4 := param "color" .float4

/-- Helix shader definition using the DSL.
    Computes 16 circles (8 pairs) for the DNA-like double helix.
    Parameters: center(2), size(1), time(1), color(4) = 8 floats.
    Note: time is raw seconds (not radians) to preserve precision for hue animation. -/
def helixShader : CircleShader := {
  name := "helix"
  instanceCount := 16
  params := [
    ⟨"center", .float2⟩,
    ⟨"size", .float⟩,
    ⟨"time", .float⟩,
    ⟨"color", .float4⟩
  ]
  body :=
    -- Which pair of circles (0-7)
    let pair := idiv idx 2
    -- Is this the second strand in the pair?
    let strand2 := eqU (imod idx 2) 1
    -- Vertical position: spread pairs along y-axis
    let y := (toFloat pair / 8.0 - 0.5) * pSize * 0.7
    -- Phase angle for rotation
    let phase := pTime * twoPi + toFloat pair * quarterPi
    -- Raw sin/cos values
    let rawSin := sin phase
    let rawCos := cos phase
    -- Flip for second strand to create double helix
    let sinP := cond strand2 (-rawSin) rawSin
    let cosP := cond strand2 (-rawCos) rawCos
    -- Depth for z-ordering and fade effect (0 = far, 1 = near)
    let depth := (cosP + 1.0) * 0.5
    -- Animate hue based on time and circle index
    let hue := fract (pTime * 0.3 + toFloat idx * 0.04)
    -- Convert HSV to RGB with saturation 0.8, value 1.0
    let rgb := hsvToRgb hue 0.8 1.0
    {
      -- Position: offset from center, x varies with sine, y is fixed per pair
      center := pCenter + vec2 (pSize * 0.3 * sinP) y
      -- Radius: varies with depth to create 3D illusion
      radius := pSize * 0.05 * (0.6 + 0.4 * depth)
      -- Color: RGB from HSV animation, alpha varies with depth
      color := vec4from31 rgb (pColor.a * (0.4 + 0.6 * depth))
    }
}

/-- Compiled helix fragment. -/
def helixFragment : ShaderFragment := helixShader.compile

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

/-
  CircleDots Spinner - Classic dots arranged in circle, fading sequentially
  Uses the Shader DSL to generate GPU shader code.
-/
import Afferent.Canopy.Core
import Afferent.Canopy.Widget.Display.Spinner.Core
import Afferent.Shader.DSL

namespace Afferent.Canopy.Spinner

open Afferent.Arbor hiding Event
open Afferent.Shader
open _root_.Shader hiding center size time color
open Linalg

-- Local aliases for DSL parameter accessors (avoid Arbor name conflicts)
private def pCenter : ShaderExpr .float2 := param "center" .float2
private def pSize : ShaderExpr .float := param "size" .float
private def pTime : ShaderExpr .float := param "time" .float
private def pColor : ShaderExpr .float4 := param "color" .float4

/-- CircleDots shader definition using the DSL.
    Computes 8 circles arranged in a ring, fading sequentially.
    Parameters: center(2), size(1), time(1), color(4) = 8 floats. -/
def circleDotsShader : CircleShader := {
  name := "circleDots"
  instanceCount := 8
  params := [
    ⟨"center", .float2⟩,
    ⟨"size", .float⟩,
    ⟨"time", .float⟩,
    ⟨"color", .float4⟩
  ]
  body :=
    -- Angle for this dot: evenly spaced around circle, starting at top (-π/2)
    let angle := (toFloat idx / 8.0) * twoPi - halfPi
    -- Position on the ring (radius = 0.35 * size)
    let ringRadius := pSize * 0.35
    let dx := pCenter.x + ringRadius * cos angle
    let dy := pCenter.y + ringRadius * sin angle
    -- Alpha fades based on phase (time + position offset)
    let phase := pTime + toFloat idx / 8.0
    let alpha := 0.3 + 0.7 * (1.0 - fract phase)
    {
      center := vec2 dx dy
      radius := pSize * 0.06
      color := vec4 pColor.x pColor.y pColor.z (pColor.w * alpha)
    }
}

/-- Compiled circleDots fragment. -/
def circleDotsFragment : ShaderFragment := circleDotsShader.compile

/-- Register the circleDots fragment in the global registry at module load time. -/
initialize circleDotsFragmentRegistration : Unit ← do
  registerFragment circleDotsFragment

/-- CircleDots: 8 dots arranged in a circle using GPU shader fragment.
    Passes only 8 floats to GPU; the shader computes all 8 circle positions and alphas. -/
def circleDotsSpec (t : Float) (color : Color) (dims : Dimensions) : CustomSpec := {
  measure := fun _ _ => (dims.size, dims.size)
  collect := fun layout =>
    let rect := layout.contentRect
    let cx := rect.x + dims.size / 2
    let cy := rect.y + dims.size / 2

    -- Only 8 floats: center(2), size(1), time(1), color(4)
    let params : Array Float := #[
      cx, cy,                             -- center
      dims.size,                          -- size
      t,                                  -- time (raw seconds)
      color.r, color.g, color.b, color.a  -- color
    ]

    RenderM.build do
      RenderM.drawFragment circleDotsFragment.hash circleDotsFragment.primitive.toUInt32
        params circleDotsFragment.instanceCount.toUInt32
  draw := none
}

end Afferent.Canopy.Spinner

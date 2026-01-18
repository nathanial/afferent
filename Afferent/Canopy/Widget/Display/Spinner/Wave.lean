/-
  Wave Spinner - Dots following sine wave pattern
  Uses the Shader DSL to generate GPU shader code.
-/
import Afferent.Canopy.Core
import Afferent.Canopy.Widget.Display.Spinner.Core
import Afferent.Shader.DSL

namespace Afferent.Canopy.Spinner

open Afferent.Arbor hiding Event
open Afferent.Shader
open Afferent.Shader.DSL hiding center size time color
open Linalg

-- Local aliases for DSL parameter accessors (avoid Arbor name conflicts)
private def pCenter : ShaderExpr .float2 := param "center" .float2
private def pSize : ShaderExpr .float := param "size" .float
private def pTime : ShaderExpr .float := param "time" .float
private def pColor : ShaderExpr .float4 := param "color" .float4

/-- Wave shader definition using the DSL.
    Computes 7 dots following a sine wave pattern.
    Parameters: center(2), size(1), time(1), color(4) = 8 floats. -/
def waveShader : CircleShader := {
  name := "wave"
  instanceCount := 7
  params := [
    ⟨"center", .float2⟩,
    ⟨"size", .float⟩,
    ⟨"time", .float⟩,
    ⟨"color", .float4⟩
  ]
  body :=
    let spacing := pSize * 0.12
    let amplitude := pSize * 0.15
    -- X offset: center the 7 dots horizontally ((7-1)/2 = 3)
    let xOffset := (toFloat idx - 3.0) * spacing
    -- Phase offset creates the wave motion
    let phase := pTime * twoPi * 2.0 - toFloat idx * pi / 3.0
    let yOffset := amplitude * sin phase
    {
      center := vec2 (pCenter.x + xOffset) (pCenter.y + yOffset)
      radius := pSize * 0.055
      color := pColor
    }
}

/-- Compiled wave fragment. -/
def waveFragment : ShaderFragment := waveShader.compile

/-- Register the wave fragment in the global registry at module load time. -/
initialize waveFragmentRegistration : Unit ← do
  registerFragment waveFragment

/-- Wave: Dots following sine wave pattern using GPU shader fragment.
    Passes only 8 floats to GPU; the shader computes all 7 circle positions. -/
def waveSpec (t : Float) (color : Color) (dims : Dimensions) : CustomSpec := {
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
      RenderM.drawFragment waveFragment.hash waveFragment.primitive.toUInt32
        params waveFragment.instanceCount.toUInt32
  draw := none
}

end Afferent.Canopy.Spinner

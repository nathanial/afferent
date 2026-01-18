/-
  Bars Spinner - Vertical bars pulsing in sequence (equalizer style)
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

/-- Bars shader definition using the DSL.
    Computes 5 vertical bars pulsing in sequence (equalizer style).
    Parameters: center(2), size(1), time(1), color(4) = 8 floats. -/
def barsShader : RectShader := {
  name := "bars"
  instanceCount := 5
  params := [
    ⟨"center", .float2⟩,
    ⟨"size", .float⟩,
    ⟨"time", .float⟩,
    ⟨"color", .float4⟩
  ]
  body :=
    let barWidth := pSize * 0.1
    let spacing := pSize * 0.15
    let maxHeight := pSize * 0.6
    let baseY := pCenter.y + pSize * 0.3  -- 0.8 - 0.5 = 0.3 offset from center
    -- Phase offset: π/5 between each bar
    let phase := pTime * twoPi + toFloat idx * pi / 5.0
    -- Height factor oscillates between 0.3 and 1.0
    let heightFactor := 0.3 + 0.7 * (sin phase + 1.0) * 0.5
    let barHeight := maxHeight * heightFactor
    -- X position: bars centered around center.x, with (numBars-1)/2 = 2 offset
    let xOffset := (toFloat idx - 2.0) * spacing
    let barX := pCenter.x + xOffset - barWidth * 0.5
    let barY := baseY - barHeight
    {
      position := vec2 barX barY
      size := vec2 barWidth barHeight
      cornerRadius := 2.0
      color := pColor
    }
}

/-- Compiled bars fragment. -/
def barsFragment : ShaderFragment := barsShader.compile

/-- Register the bars fragment in the global registry at module load time. -/
initialize barsFragmentRegistration : Unit ← do
  registerFragment barsFragment

/-- Bars: Vertical bars pulsing in sequence using GPU shader fragment.
    Passes 8 floats to GPU; the shader computes all 5 bar positions and sizes. -/
def barsSpec (t : Float) (color : Color) (dims : Dimensions) : CustomSpec := {
  measure := fun _ _ => (dims.size, dims.size)
  collect := fun layout =>
    let rect := layout.contentRect
    let cx := rect.x + dims.size / 2
    let cy := rect.y + dims.size / 2

    -- 8 floats: center(2), size(1), time(1), color(4)
    let params : Array Float := #[
      cx, cy,                             -- center
      dims.size,                          -- size
      t,                                  -- time (raw seconds)
      color.r, color.g, color.b, color.a  -- color
    ]

    RenderM.build do
      RenderM.drawFragment barsFragment.hash barsFragment.primitive.toUInt32
        params barsFragment.instanceCount.toUInt32
  draw := none
}

end Afferent.Canopy.Spinner

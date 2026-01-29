/-
  BouncingDots Spinner - Three dots bouncing with phase offset
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

/-- BouncingDots shader definition using the DSL.
    Computes 3 dots bouncing with phase offset (120° apart).
    Parameters: center(2), size(1), time(1), color(4) = 8 floats. -/
def bouncingDotsShader : CircleShader := {
  name := "bouncingDots"
  instanceCount := 3
  params := [
    ⟨"center", .float2⟩,
    ⟨"size", .float⟩,
    ⟨"time", .float⟩,
    ⟨"color", .float4⟩
  ]
  body :=
    let spacing := pSize * 0.25
    let bounceHeight := pSize * 0.2
    -- Phase offset: 2π/3 between each dot (120°)
    let phase := pTime * twoPi + toFloat idx * twoPi / 3.0
    -- Bounce uses absolute sine for smooth up-only motion
    let yOffset := absF (sin phase) * bounceHeight
    -- X position: dots centered at -spacing, 0, +spacing
    let xOffset := (toFloat idx - 1.0) * spacing
    {
      center := pCenter + vec2 xOffset (-yOffset)
      radius := pSize * 0.1
      color := pColor
    }
}

/-- Compiled bouncingDots fragment. -/
def bouncingDotsFragment : ShaderFragment := bouncingDotsShader.compile

/-- Register the bouncingDots fragment in the global registry at module load time. -/
initialize bouncingDotsFragmentRegistration : Unit ← do
  registerFragment bouncingDotsFragment

/-- BouncingDots: Three dots bouncing with phase offset using GPU shader fragment.
    Passes only 8 floats to GPU; the shader computes all 3 circle positions. -/
def bouncingDotsSpec (t : Float) (color : Color) (dims : Dimensions) : CustomSpec := {
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
      RenderM.drawFragment bouncingDotsFragment.hash bouncingDotsFragment.primitive.toUInt32
        params bouncingDotsFragment.instanceCount.toUInt32
  draw := none
}

end Afferent.Canopy.Spinner

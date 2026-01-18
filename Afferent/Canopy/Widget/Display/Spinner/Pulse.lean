/-
  Pulse Spinner - Expanding concentric rings that fade as they grow
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
private def pStrokeWidth : ShaderExpr .float := param "strokeWidth" .float
private def pColor : ShaderExpr .float4 := param "color" .float4

/-- Pulse shader definition using the DSL.
    Computes 3 expanding rings that fade as they grow.
    Parameters: center(2), size(1), time(1), strokeWidth(1), color(4) = 9 floats. -/
def pulseShader : CircleShader := {
  name := "pulse"
  instanceCount := 3
  params := [
    ⟨"center", .float2⟩,
    ⟨"size", .float⟩,
    ⟨"time", .float⟩,
    ⟨"strokeWidth", .float⟩,
    ⟨"color", .float4⟩
  ]
  body :=
    let maxRadius := pSize * 0.45
    -- Phase for this ring: time + idx / 3
    let phase := pTime + toFloat idx / 3.0
    let progress := fract phase
    -- Ring radius grows with progress
    let radius := maxRadius * progress
    -- Alpha fades as ring expands (1.0 at start, 0 at end)
    let alpha := 1.0 - progress
    -- Use zero alpha for nearly-invisible rings (progress near 1.0)
    let effectiveAlpha := cond (lt alpha 0.05) 0.0 alpha
    {
      center := pCenter
      radius := radius
      strokeWidth := pStrokeWidth
      color := vec4 pColor.x pColor.y pColor.z (pColor.w * effectiveAlpha)
    }
}

/-- Compiled pulse fragment. -/
def pulseFragment : ShaderFragment := pulseShader.compile

/-- Register the pulse fragment in the global registry at module load time. -/
initialize pulseFragmentRegistration : Unit ← do
  registerFragment pulseFragment

/-- Pulse: Expanding concentric rings using GPU shader fragment.
    Passes 9 floats to GPU; the shader computes all 3 ring positions and alphas. -/
def pulseSpec (t : Float) (color : Color) (dims : Dimensions) : CustomSpec := {
  measure := fun _ _ => (dims.size, dims.size)
  collect := fun layout =>
    let rect := layout.contentRect
    let cx := rect.x + dims.size / 2
    let cy := rect.y + dims.size / 2

    -- 9 floats: center(2), size(1), time(1), strokeWidth(1), color(4)
    let params : Array Float := #[
      cx, cy,                             -- center
      dims.size,                          -- size
      t,                                  -- time (raw seconds)
      dims.strokeWidth,                   -- strokeWidth
      color.r, color.g, color.b, color.a  -- color
    ]

    RenderM.build do
      RenderM.drawFragment pulseFragment.hash pulseFragment.primitive.toUInt32
        params pulseFragment.instanceCount.toUInt32
  draw := none
}

end Afferent.Canopy.Spinner

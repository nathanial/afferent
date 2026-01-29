/-
  Ripple Spinner - Concentric circles expanding outward from center
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
private def pStrokeWidth : ShaderExpr .float := param "strokeWidth" .float
private def pColor : ShaderExpr .float4 := param "color" .float4

/-- Ripple shader definition using the DSL.
    Computes 5 circles: center dot (idx 0) + 4 expanding rings (idx 1-4).
    Parameters: center(2), size(1), time(1), strokeWidth(1), color(4) = 9 floats. -/
def rippleShader : CircleShader := {
  name := "ripple"
  instanceCount := 5
  params := [
    ⟨"center", .float2⟩,
    ⟨"size", .float⟩,
    ⟨"time", .float⟩,
    ⟨"strokeWidth", .float⟩,
    ⟨"color", .float4⟩
  ]
  body :=
    let maxRadius := pSize * 0.45
    let isCenterDot := eqU idx 0
    -- Ripple index (0-3 for ripples, undefined for center dot)
    let rippleIdx := toFloat idx - 1.0
    -- Phase for this ripple: time * 2 + rippleIdx / 4
    let phase := pTime * 2.0 + rippleIdx / 4.0
    let progress := fract phase
    -- Ripple radius grows with progress
    let rippleRadius := maxRadius * progress
    -- Alpha fades as ripple expands (0.8 at start, 0 at end)
    let rippleAlpha := (1.0 - progress) * 0.8
    -- Use zero alpha for nearly-invisible ripples (progress near 1.0)
    let effectiveAlpha := cond (lt rippleAlpha 0.05) 0.0 rippleAlpha
    {
      center := pCenter
      radius := cond isCenterDot (pSize * 0.04) rippleRadius
      strokeWidth := cond isCenterDot 0.0 (pStrokeWidth * 0.7)
      color := cond isCenterDot pColor
                    (vec4 pColor.x pColor.y pColor.z (pColor.w * effectiveAlpha))
    }
}

/-- Compiled ripple fragment. -/
def rippleFragment : ShaderFragment := rippleShader.compile

/-- Register the ripple fragment in the global registry at module load time. -/
initialize rippleFragmentRegistration : Unit ← do
  registerFragment rippleFragment

/-- Ripple: Concentric circles expanding outward using GPU shader fragment.
    Passes 9 floats to GPU; the shader computes center dot + 4 expanding rings. -/
def rippleSpec (t : Float) (color : Color) (dims : Dimensions) : CustomSpec := {
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
      RenderM.drawFragment rippleFragment.hash rippleFragment.primitive.toUInt32
        params rippleFragment.instanceCount.toUInt32
  draw := none
}

end Afferent.Canopy.Spinner

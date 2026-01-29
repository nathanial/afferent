/-
  Pendulum Spinner - Swinging pendulum with motion trail
  Uses the Shader DSL to generate GPU shader code for circles.
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
private def pPivot : ShaderExpr .float2 := param "pivot" .float2
private def pSize : ShaderExpr .float := param "size" .float
private def pTime : ShaderExpr .float := param "time" .float
private def pStrokeWidth : ShaderExpr .float := param "strokeWidth" .float
private def pColor : ShaderExpr .float4 := param "color" .float4

/-- Pendulum shader definition using the DSL.
    Computes 7 circles: pivot (idx 0), trail (idx 1-5), bob (idx 6).
    Parameters: pivot(2), size(1), time(1), strokeWidth(1), color(4) = 9 floats. -/
def pendulumShader : CircleShader := {
  name := "pendulum"
  instanceCount := 7
  params := [
    ⟨"pivot", .float2⟩,
    ⟨"size", .float⟩,
    ⟨"time", .float⟩,
    ⟨"strokeWidth", .float⟩,
    ⟨"color", .float4⟩
  ]
  body :=
    let length := pSize * 0.6
    let maxAngle := pi * 0.35
    let bobRadius := pSize * 0.08

    -- Identify which element: pivot (0), trail (1-5), bob (6)
    let isPivot := eqU idx 0
    let isBob := eqU idx 6

    -- Trail index for offset calculation (idx 1-5 -> 0-4)
    let trailIdx := toFloat idx - 1.0

    -- Time offset: trail circles look back in time
    let timeOffset := cond isPivot 0.0 (cond isBob 0.0 (trailIdx * 0.04))
    let effectiveTime := pTime - timeOffset

    -- Pendulum angle and offset from pivot
    let angle := maxAngle * sin (effectiveTime * twoPi)
    let offsetX := length * sin angle
    let offsetY := length * cos angle

    -- Position: pivot uses zero offset, others use computed offset
    let offset := cond isPivot (vec2 0.0 0.0) (vec2 offsetX offsetY)

    -- Radius: pivot uses strokeWidth, others use bobRadius
    let radius := cond isPivot (pStrokeWidth * 0.8) bobRadius

    -- Alpha: pivot 0.6, trail fading (0.15 down to 0.03), bob full
    let trailAlpha := 0.15 * (1.0 - trailIdx / 5.0)
    let alpha := cond isPivot 0.6 (cond isBob pColor.w trailAlpha)

    {
      center := pPivot + offset
      radius := radius
      color := vec4 pColor.x pColor.y pColor.z alpha
    }
}

/-- Compiled pendulum fragment. -/
def pendulumFragment : ShaderFragment := pendulumShader.compile

/-- Register the pendulum fragment in the global registry at module load time. -/
initialize pendulumFragmentRegistration : Unit ← do
  registerFragment pendulumFragment

/-- Pendulum: Swinging pendulum with motion trail using GPU shader fragment.
    Passes 9 floats to GPU for circles; rod drawn separately. -/
def pendulumSpec (t : Float) (color : Color) (dims : Dimensions) : CustomSpec := {
  measure := fun _ _ => (dims.size, dims.size)
  collect := fun layout =>
    let rect := layout.contentRect
    let cx := rect.x + dims.size / 2
    let pivotY := rect.y + dims.size * 0.15
    let length := dims.size * 0.6
    let maxAngle := Float.pi * 0.35

    -- Compute bob position for the rod (also computed in shader for circles)
    let angle := maxAngle * Float.sin (t * Float.twoPi)
    let bobX := cx + length * Float.sin angle
    let bobY := pivotY + length * Float.cos angle

    -- 9 floats: pivot(2), size(1), time(1), strokeWidth(1), color(4)
    let params : Array Float := #[
      cx, pivotY,                         -- pivot point
      dims.size,                          -- size
      t,                                  -- time (raw seconds)
      dims.strokeWidth,                   -- strokeWidth
      color.r, color.g, color.b, color.a  -- color
    ]

    RenderM.build do
      -- Draw all 7 circles via shader (pivot, 5 trail, bob)
      RenderM.drawFragment pendulumFragment.hash pendulumFragment.primitive.toUInt32
        params pendulumFragment.instanceCount.toUInt32

      -- Rod (line from pivot to bob - not part of circle shader)
      RenderM.strokeLineBatch #[cx, pivotY, bobX, bobY, color.r, color.g, color.b, color.a * 0.7, 0.0] 1 (dims.strokeWidth * 0.7)
  draw := none
}

end Afferent.Canopy.Spinner

/-
  Orbit Spinner - Dots orbiting center at different speeds and radii
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

/-- Orbit shader definition using the DSL.
    Computes 4 orbiting dots at different radii and speeds.
    Orbit params encoded as constants: (radiusFactor, speedMult, sizeFactor, alpha)
    - Orbit 0: (0.35, 1.0, 0.08, 1.0)
    - Orbit 1: (0.28, 1.7, 0.06, 0.8)
    - Orbit 2: (0.20, 2.5, 0.05, 0.6)
    - Orbit 3: (0.12, 4.0, 0.04, 0.4)
    Parameters: center(2), size(1), time(1), color(4) = 8 floats.
    Note: time is raw seconds (not radians) to preserve precision. -/
def orbitShader : CircleShader := {
  name := "orbit"
  instanceCount := 4
  params := [
    ⟨"center", .float2⟩,
    ⟨"size", .float⟩,
    ⟨"time", .float⟩,
    ⟨"color", .float4⟩
  ]
  body :=
    -- Select orbit parameters based on instance index
    -- radiusFactor: 0.35, 0.28, 0.20, 0.12
    let radiusFactor := cond (eqU idx 0) 0.35
                       (cond (eqU idx 1) 0.28
                       (cond (eqU idx 2) 0.20 0.12))
    -- speedMult: 1.0, 1.7, 2.5, 4.0
    let speedMult := cond (eqU idx 0) 1.0
                    (cond (eqU idx 1) 1.7
                    (cond (eqU idx 2) 2.5 4.0))
    -- sizeFactor: 0.08, 0.06, 0.05, 0.04
    let sizeFactor := cond (eqU idx 0) 0.08
                     (cond (eqU idx 1) 0.06
                     (cond (eqU idx 2) 0.05 0.04))
    -- alpha: 1.0, 0.8, 0.6, 0.4
    let alpha := cond (eqU idx 0) 1.0
                (cond (eqU idx 1) 0.8
                (cond (eqU idx 2) 0.6 0.4))

    -- Compute position on orbit
    let angle := pTime * twoPi * speedMult
    let orbitRadius := pSize * radiusFactor
    let dx := orbitRadius * cos angle
    let dy := orbitRadius * sin angle

    {
      center := pCenter + vec2 dx dy
      radius := pSize * sizeFactor
      color := vec4 pColor.x pColor.y pColor.z (pColor.w * alpha)
    }
}

/-- Compiled orbit fragment. -/
def orbitFragment : ShaderFragment := orbitShader.compile

/-- Register the orbit fragment in the global registry at module load time. -/
initialize orbitFragmentRegistration : Unit ← do
  registerFragment orbitFragment

/-- Orbit: Dots orbiting center at different speeds and radii using GPU shader fragment.
    Passes only 8 floats to GPU; the shader computes all 4 circle positions, sizes, and alphas. -/
def orbitSpec (t : Float) (color : Color) (dims : Dimensions) : CustomSpec := {
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
      RenderM.drawFragment orbitFragment.hash orbitFragment.primitive.toUInt32
        params orbitFragment.instanceCount.toUInt32
  draw := none
}

end Afferent.Canopy.Spinner

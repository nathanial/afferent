/-
  Ring Spinner - Rotating arc segment (macOS/iOS style)
-/
import Afferent.Canopy.Core
import Afferent.Canopy.Widget.Display.Spinner.Core

namespace Afferent.Canopy.Spinner

open Afferent.Arbor hiding Event
open Linalg

/-- Ring: Rotating arc segment (macOS/iOS style).
    Uses instanced arc rendering for GPU batching.
    Note: `t` is raw elapsed time in seconds, not wrapped progress. -/
def ringSpec (t : Float) (color : Color) (dims : Dimensions) : CustomSpec := {
  measure := fun _ _ => (dims.size, dims.size)
  collect := fun layout =>
    let rect := layout.contentRect
    let cx := rect.x + dims.size / 2
    let cy := rect.y + dims.size / 2
    let radius := (dims.size - dims.strokeWidth) / 2 - 2
    -- Use raw time directly - cos/sin handle any angle value smoothly
    let startAngle := t * Float.pi  -- Half rotation per second
    let sweepAngle := Float.pi * 1.5  -- 270° arc

    RenderM.build do
      RenderM.strokeArcInstanced #[{
        centerX := cx
        centerY := cy
        startAngle := startAngle
        sweepAngle := sweepAngle
        radius := radius
        strokeWidth := dims.strokeWidth
        r := color.r
        g := color.g
        b := color.b
        a := color.a
      }]
  draw := none
}

end Afferent.Canopy.Spinner

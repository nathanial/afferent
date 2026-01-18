/-
  Pendulum Spinner - Swinging pendulum with motion trail
-/
import Afferent.Canopy.Core
import Afferent.Canopy.Widget.Display.Spinner.Core

namespace Afferent.Canopy.Spinner

open Afferent.Arbor hiding Event
open Linalg

/-- Pendulum: Swinging pendulum with motion trail. -/
def pendulumSpec (t : Float) (color : Color) (dims : Dimensions) : CustomSpec := {
  measure := fun _ _ => (dims.size, dims.size)
  collect := fun layout =>
    let rect := layout.contentRect
    let cx := rect.x + dims.size / 2
    let pivotY := rect.y + dims.size * 0.15
    let length := dims.size * 0.6
    let maxAngle := Float.pi * 0.35
    let bobRadius := dims.size * 0.08

    -- Damped oscillation (sin for smooth back-and-forth)
    let angle := maxAngle * Float.sin (t * Float.twoPi)
    let bobX := cx + length * Float.sin angle
    let bobY := pivotY + length * Float.cos angle

    RenderM.build do
      -- Pivot point
      RenderM.fillCircle (Arbor.Point.mk' cx pivotY) (dims.strokeWidth * 0.8) (color.withAlpha 0.6)

      -- Motion trail (ghost positions)
      for i in [:5] do
        let trailT := t - i.toFloat * 0.04
        let trailAngle := maxAngle * Float.sin (trailT * Float.twoPi)
        let trailX := cx + length * Float.sin trailAngle
        let trailY := pivotY + length * Float.cos trailAngle
        let alpha := 0.15 * (1.0 - i.toFloat / 5.0)
        RenderM.fillCircle (Arbor.Point.mk' trailX trailY) bobRadius (color.withAlpha alpha)

      -- Rod (uses strokeLineBatch for batching)
      RenderM.strokeLineBatch #[cx, pivotY, bobX, bobY, color.r, color.g, color.b, color.a * 0.7, 0.0] 1 (dims.strokeWidth * 0.7)

      -- Bob
      RenderM.fillCircle (Arbor.Point.mk' bobX bobY) bobRadius color
  draw := none
}

end Afferent.Canopy.Spinner

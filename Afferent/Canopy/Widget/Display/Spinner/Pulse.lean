/-
  Pulse Spinner - Expanding concentric rings that fade as they grow
-/
import Afferent.Canopy.Core
import Afferent.Canopy.Widget.Display.Spinner.Core

namespace Afferent.Canopy.Spinner

open Afferent.Arbor hiding Event

/-- Pulse: Expanding concentric rings that fade as they grow. -/
def pulseSpec (t : Float) (color : Color) (dims : Dimensions) : CustomSpec := {
  measure := fun _ _ => (dims.size, dims.size)
  collect := fun layout =>
    let rect := layout.contentRect
    let cx := rect.x + dims.size / 2
    let cy := rect.y + dims.size / 2
    let maxRadius := dims.size * 0.45
    let numRings : Nat := 3
    let center := Arbor.Point.mk' cx cy

    RenderM.build do
      for i in [:numRings] do
        let phase := (t + i.toFloat / numRings.toFloat)
        let progress := phase - phase.floor
        let radius := maxRadius * progress
        let alpha := 1.0 - progress
        if alpha > 0.05 then
          RenderM.strokeCircle center radius (color.withAlpha alpha) dims.strokeWidth
  draw := none
}

end Afferent.Canopy.Spinner

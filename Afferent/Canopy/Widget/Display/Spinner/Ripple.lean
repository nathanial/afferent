/-
  Ripple Spinner - Concentric circles expanding outward from center
-/
import Afferent.Canopy.Core
import Afferent.Canopy.Widget.Display.Spinner.Core

namespace Afferent.Canopy.Spinner

open Afferent.Arbor hiding Event

/-- Ripple: Concentric circles expanding outward from center. -/
def rippleSpec (t : Float) (color : Color) (dims : Dimensions) : CustomSpec := {
  measure := fun _ _ => (dims.size, dims.size)
  collect := fun layout =>
    let rect := layout.contentRect
    let cx := rect.x + dims.size / 2
    let cy := rect.y + dims.size / 2
    let maxRadius := dims.size * 0.45
    let numRipples : Nat := 4
    let center := Arbor.Point.mk' cx cy

    RenderM.build do
      -- Center dot
      RenderM.fillCircle center (dims.size * 0.04) color

      -- Expanding ripples
      for i in [:numRipples] do
        let phase := (t * 2.0 + i.toFloat / numRipples.toFloat)
        let progress := phase - phase.floor
        let radius := maxRadius * progress
        let alpha := (1.0 - progress) * 0.8
        if alpha > 0.05 then
          RenderM.strokeCircle center radius (color.withAlpha alpha) (dims.strokeWidth * 0.7)
  draw := none
}

end Afferent.Canopy.Spinner

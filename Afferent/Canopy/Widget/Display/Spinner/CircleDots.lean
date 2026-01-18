/-
  CircleDots Spinner - Classic dots arranged in circle, fading sequentially
-/
import Afferent.Canopy.Core
import Afferent.Canopy.Widget.Display.Spinner.Core

namespace Afferent.Canopy.Spinner

open Afferent.Arbor hiding Event
open Linalg

/-- CircleDots: 8 dots arranged in a circle, fading sequentially. -/
def circleDotsSpec (t : Float) (color : Color) (dims : Dimensions) : CustomSpec := {
  measure := fun _ _ => (dims.size, dims.size)
  collect := fun layout =>
    let rect := layout.contentRect
    let cx := rect.x + dims.size / 2
    let cy := rect.y + dims.size / 2
    let radius := dims.size * 0.35
    let dotRadius := dims.size * 0.06
    let numDots : Nat := 8

    RenderM.build do
      for i in [:numDots] do
        let angle := (i.toFloat / numDots.toFloat) * Float.twoPi - Float.halfPi
        let dx := cx + radius * Float.cos angle
        let dy := cy + radius * Float.sin angle
        -- Fade based on position relative to animation time
        let phase := (t + i.toFloat / numDots.toFloat)
        let alpha := 0.3 + 0.7 * (1.0 - (phase - phase.floor))
        let dotColor := color.withAlpha alpha
        RenderM.fillCircle (Arbor.Point.mk' dx dy) dotRadius dotColor
  draw := none
}

end Afferent.Canopy.Spinner

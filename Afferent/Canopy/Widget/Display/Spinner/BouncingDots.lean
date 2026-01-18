/-
  BouncingDots Spinner - Three dots bouncing with phase offset
-/
import Afferent.Canopy.Core
import Afferent.Canopy.Widget.Display.Spinner.Core

namespace Afferent.Canopy.Spinner

open Afferent.Arbor hiding Event
open Linalg

/-- BouncingDots: Three dots bouncing with phase offset. -/
def bouncingDotsSpec (t : Float) (color : Color) (dims : Dimensions) : CustomSpec := {
  measure := fun _ _ => (dims.size, dims.size)
  collect := fun layout =>
    let rect := layout.contentRect
    let cx := rect.x + dims.size / 2
    let cy := rect.y + dims.size / 2
    let dotRadius := dims.size * 0.1
    let spacing := dims.size * 0.25
    let bounceHeight := dims.size * 0.2

    RenderM.build do
      for i in [:3] do
        let phase := t * Float.twoPi + i.toFloat * Float.twoPi / 3.0
        let yOffset := Float.abs (Float.sin phase) * bounceHeight
        let dx := cx + (i.toFloat - 1.0) * spacing
        let dy := cy - yOffset
        RenderM.fillCircle (Arbor.Point.mk' dx dy) dotRadius color
  draw := none
}

end Afferent.Canopy.Spinner

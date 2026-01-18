/-
  Wave Spinner - Dots following sine wave pattern
-/
import Afferent.Canopy.Core
import Afferent.Canopy.Widget.Display.Spinner.Core

namespace Afferent.Canopy.Spinner

open Afferent.Arbor hiding Event
open Linalg

/-- Wave: Dots following sine wave pattern. -/
def waveSpec (t : Float) (color : Color) (dims : Dimensions) : CustomSpec := {
  measure := fun _ _ => (dims.size, dims.size)
  collect := fun layout =>
    let rect := layout.contentRect
    let cx := rect.x + dims.size / 2
    let cy := rect.y + dims.size / 2
    let numDots : Nat := 7
    let spacing := dims.size * 0.12
    let amplitude := dims.size * 0.15
    let dotRadius := dims.size * 0.055

    RenderM.build do
      for i in [:numDots] do
        let xOffset := (i.toFloat - (numDots.toFloat - 1.0) / 2.0) * spacing
        let phase := t * Float.twoPi * 2.0 - i.toFloat * Float.pi / 3.0
        let yOffset := amplitude * Float.sin phase
        RenderM.fillCircle (Arbor.Point.mk' (cx + xOffset) (cy + yOffset)) dotRadius color
  draw := none
}

end Afferent.Canopy.Spinner

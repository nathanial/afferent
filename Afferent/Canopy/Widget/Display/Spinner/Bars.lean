/-
  Bars Spinner - Vertical bars pulsing in sequence (equalizer style)
-/
import Afferent.Canopy.Core
import Afferent.Canopy.Widget.Display.Spinner.Core

namespace Afferent.Canopy.Spinner

open Afferent.Arbor hiding Event
open Linalg

/-- Bars: Vertical bars pulsing in sequence (equalizer style). -/
def barsSpec (t : Float) (color : Color) (dims : Dimensions) : CustomSpec := {
  measure := fun _ _ => (dims.size, dims.size)
  collect := fun layout =>
    let rect := layout.contentRect
    let cx := rect.x + dims.size / 2
    let baseY := rect.y + dims.size * 0.8
    let numBars : Nat := 5
    let barWidth := dims.size * 0.1
    let spacing := dims.size * 0.15
    let maxHeight := dims.size * 0.6

    RenderM.build do
      for i in [:numBars] do
        let phase := t * Float.twoPi + i.toFloat * Float.pi / numBars.toFloat
        let heightFactor := 0.3 + 0.7 * (Float.sin phase + 1.0) / 2.0
        let barHeight := maxHeight * heightFactor
        let dx := cx + (i.toFloat - (numBars.toFloat - 1.0) / 2.0) * spacing
        let barRect := Arbor.Rect.mk' (dx - barWidth / 2) (baseY - barHeight) barWidth barHeight
        RenderM.fillRect barRect color 2.0
  draw := none
}

end Afferent.Canopy.Spinner

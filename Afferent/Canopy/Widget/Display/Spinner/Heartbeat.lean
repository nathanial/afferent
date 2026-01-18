/-
  Heartbeat Spinner - Pulsing shape with ECG-like rhythm
-/
import Afferent.Canopy.Core
import Afferent.Canopy.Widget.Display.Spinner.Core

namespace Afferent.Canopy.Spinner

open Afferent.Arbor hiding Event
open Linalg

/-- Heartbeat: Pulsing shape with ECG-like rhythm. -/
def heartbeatSpec (t : Float) (color : Color) (dims : Dimensions) : CustomSpec := {
  measure := fun _ _ => (dims.size, dims.size)
  collect := fun layout =>
    let rect := layout.contentRect
    let cx := rect.x + dims.size / 2
    let cy := rect.y + dims.size / 2
    let baseSize := dims.size * 0.25

    -- ECG-like timing: quick pulse, pause, repeat
    -- Map t (0-1) to a pulse pattern
    let cyclePos := t
    let scale := if cyclePos < 0.15 then
        1.0 + 0.3 * Float.sin (cyclePos / 0.15 * Float.pi)  -- First beat
      else if cyclePos < 0.3 then
        1.0 - 0.1 * Float.sin ((cyclePos - 0.15) / 0.15 * Float.pi)  -- Slight dip
      else if cyclePos < 0.45 then
        1.0 + 0.2 * Float.sin ((cyclePos - 0.3) / 0.15 * Float.pi)  -- Second beat
      else
        1.0  -- Rest

    let heartPath := Arbor.Path.heart (Arbor.Point.mk' cx cy) (baseSize * scale)

    RenderM.build do
      RenderM.fillPath heartPath color
  draw := none
}

end Afferent.Canopy.Spinner

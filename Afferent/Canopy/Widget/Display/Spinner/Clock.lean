/-
  Clock Spinner - Clock hands rotating at different speeds
-/
import Afferent.Canopy.Core
import Afferent.Canopy.Widget.Display.Spinner.Core

namespace Afferent.Canopy.Spinner

open Afferent.Arbor hiding Event
open Linalg

/-- Clock: Clock hands rotating at different speeds. -/
def clockSpec (t : Float) (color : Color) (dims : Dimensions) : CustomSpec := {
  measure := fun _ _ => (dims.size, dims.size)
  collect := fun layout =>
    let rect := layout.contentRect
    let cx := rect.x + dims.size / 2
    let cy := rect.y + dims.size / 2
    let radius := dims.size * 0.4
    let center := Arbor.Point.mk' cx cy

    -- Second hand (fast), minute hand (slower), hour hand (slowest)
    let secondAngle := t * Float.twoPi - Float.halfPi
    let minuteAngle := t * Float.twoPi / 12.0 - Float.halfPi
    let hourAngle := t * Float.twoPi / 60.0 - Float.halfPi

    -- Build all 3 hands as a single line batch (9 floats per line: x1, y1, x2, y2, r, g, b, a, padding)
    let hourEnd := Arbor.Point.mk' (cx + radius * 0.4 * Float.cos hourAngle)
                             (cy + radius * 0.4 * Float.sin hourAngle)
    let minuteEnd := Arbor.Point.mk' (cx + radius * 0.65 * Float.cos minuteAngle)
                               (cy + radius * 0.65 * Float.sin minuteAngle)
    let secondEnd := Arbor.Point.mk' (cx + radius * 0.85 * Float.cos secondAngle)
                               (cy + radius * 0.85 * Float.sin secondAngle)

    let hourColor := color.withAlpha 0.8
    let minuteColor := color.withAlpha 0.9

    RenderM.build do
      -- Clock face circle (batched via GPU shader)
      RenderM.strokeCircle center radius (color.withAlpha 0.3) (dims.strokeWidth * 0.5)

      -- Hour hand (thickest) - strokeLineBatch is batchable
      RenderM.strokeLineBatch #[cx, cy, hourEnd.x, hourEnd.y, hourColor.r, hourColor.g, hourColor.b, hourColor.a, 0.0] 1 (dims.strokeWidth * 1.5)

      -- Minute hand - strokeLineBatch is batchable
      RenderM.strokeLineBatch #[cx, cy, minuteEnd.x, minuteEnd.y, minuteColor.r, minuteColor.g, minuteColor.b, minuteColor.a, 0.0] 1 dims.strokeWidth

      -- Second hand (thinnest) - strokeLineBatch is batchable
      RenderM.strokeLineBatch #[cx, cy, secondEnd.x, secondEnd.y, color.r, color.g, color.b, color.a, 0.0] 1 (dims.strokeWidth * 0.6)

      -- Center dot (batchable fillCircle instead of fillPath)
      RenderM.fillCircle center (dims.strokeWidth * 0.8) color
  draw := none
}

end Afferent.Canopy.Spinner

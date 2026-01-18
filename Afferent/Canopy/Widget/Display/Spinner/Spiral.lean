/-
  Spiral Spinner - Drawing spiral that grows and resets
-/
import Afferent.Canopy.Core
import Afferent.Canopy.Widget.Display.Spinner.Core

namespace Afferent.Canopy.Spinner

open Afferent.Arbor hiding Event
open Linalg

/-! ## Precomputed Spiral Geometry -/

private def spiralPointCount : Nat := 50
private def spiralPointDivisor : Float := spiralPointCount.toFloat
private def spiralTotalAngle : Float := 2.5 * Float.twoPi

private def spiralUnitPoints : Array Arbor.Point := Id.run do
  let mut points : Array Arbor.Point := Array.mkEmpty spiralPointCount
  for i in [:spiralPointCount] do
    let progress := i.toFloat / spiralPointDivisor
    let angle := progress * spiralTotalAngle
    let radius := progress
    let x := radius * Float.cos angle
    let y := radius * Float.sin angle
    points := points.push (Arbor.Point.mk' x y)
  return points

private def spiralSegmentAlphas : Array Float := Id.run do
  let mut alphas : Array Float := Array.mkEmpty spiralPointCount
  for i in [:spiralPointCount] do
    let progress := i.toFloat / spiralPointDivisor
    alphas := alphas.push (0.3 + 0.7 * progress)
  return alphas

/-- Spiral: Drawing spiral that grows and resets. -/
def spiralSpec (t : Float) (color : Color) (dims : Dimensions) : CustomSpec := {
  measure := fun _ _ => (dims.size, dims.size)
  collect := fun layout =>
    let rect := layout.contentRect
    let cx := rect.x + dims.size / 2
    let cy := rect.y + dims.size / 2
    let maxRadius := dims.size * 0.4

    RenderM.build do
      -- Draw spiral up to current progress
      let targetSegments := (t * spiralPointDivisor).toUInt32.toNat
      let numSegments := min spiralPointCount targetSegments
      let lineCount := if numSegments > 1 then numSegments - 1 else 0
      if lineCount > 0 then
        let mut data : Array Float := Array.mkEmpty (lineCount * 9)
        for i in [1:numSegments] do
          let prev := spiralUnitPoints[i - 1]!
          let next := spiralUnitPoints[i]!
          let alpha := spiralSegmentAlphas[i]!
          let c := color.withAlpha alpha
          let x1 := cx + maxRadius * prev.x
          let y1 := cy + maxRadius * prev.y
          let x2 := cx + maxRadius * next.x
          let y2 := cy + maxRadius * next.y
          data := data.push x1 |>.push y1 |>.push x2 |>.push y2
                   |>.push c.r |>.push c.g |>.push c.b |>.push c.a
                   |>.push 0.0
        RenderM.strokeLineBatch data lineCount dims.strokeWidth
  draw := none
}

end Afferent.Canopy.Spinner

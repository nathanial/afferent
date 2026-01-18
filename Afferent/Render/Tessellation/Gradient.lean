/-
  Afferent Tessellation Gradient Sampling
-/
import Afferent.Core.Types
import Afferent.Core.Paint

namespace Afferent

namespace Tessellation

/-- Clamp a value to [0, 1] range. -/
private def clamp01 (x : Float) : Float :=
  if x < 0.0 then 0.0 else if x > 1.0 then 1.0 else x

/-- Find the two gradient stops surrounding a given t value and interpolate. -/
def interpolateGradientStops (stops : Array GradientStop) (t : Float) : Color := Id.run do
  if stops.size == 0 then return Color.black
  if stops.size == 1 then return stops[0]!.color

  let t := clamp01 t

  -- Find surrounding stops
  let mut prevStop := stops[0]!
  let mut nextStop := stops[0]!

  for i in [:stops.size] do
    if h : i < stops.size then
      let stop := stops[i]
      if stop.position <= t then
        prevStop := stop
      if stop.position >= t && (i == 0 || stops[i-1]!.position < t) then
        nextStop := stop
        break

  -- Handle edge cases
  if t <= prevStop.position then return prevStop.color
  if t >= nextStop.position then return nextStop.color
  if prevStop.position == nextStop.position then return prevStop.color

  -- Interpolate between stops
  let localT := (t - prevStop.position) / (nextStop.position - prevStop.position)
  Color.lerp prevStop.color nextStop.color localT

/-- Sample a linear gradient at a given point.
    Projects the point onto the gradient line and returns the interpolated color. -/
def sampleLinearGradient (start finish : Point) (stops : Array GradientStop) (p : Point) : Color :=
  -- Vector from start to finish
  let dx := finish.x - start.x
  let dy := finish.y - start.y
  let lenSq := dx * dx + dy * dy

  if lenSq < 0.0001 then
    -- Degenerate gradient (start == finish)
    if stops.size > 0 then stops[0]!.color else Color.black
  else
    -- Project point onto gradient line
    let px := p.x - start.x
    let py := p.y - start.y
    let t := (px * dx + py * dy) / lenSq
    interpolateGradientStops stops t

/-- Sample a radial gradient at a given point.
    Uses distance from center to determine color. -/
def sampleRadialGradient (center : Point) (radius : Float) (stops : Array GradientStop) (p : Point) : Color :=
  if radius < 0.0001 then
    if stops.size > 0 then stops[0]!.color else Color.black
  else
    let dist := Point.distance center p
    let t := dist / radius
    interpolateGradientStops stops t

/-- Sample any gradient type at a given point. -/
def sampleGradient (g : Gradient) (p : Point) : Color :=
  match g with
  | .linear start finish stops => sampleLinearGradient start finish stops p
  | .radial center radius stops => sampleRadialGradient center radius stops p

/-- Sample a fill style at a given point. -/
def sampleFillStyle (style : FillStyle) (p : Point) : Color :=
  match style with
  | .solid c => c
  | .gradient g => sampleGradient g p

end Tessellation

end Afferent

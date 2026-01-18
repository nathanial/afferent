/-
  Canopy Spinner Widget
  Loading indicators with standard and creative animated variants.
-/
import Reactive
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Reactive.Component

namespace Afferent.Canopy

open Afferent.Arbor hiding Event
open Linalg

/-- Spinner variant for different visual styles. -/
inductive SpinnerVariant where
  -- Standard spinners
  | circleDots      -- Classic dots arranged in circle, fading sequentially
  | ring            -- Rotating arc segment (macOS/iOS style)
  | bouncingDots    -- Three dots bouncing horizontally
  | bars            -- Vertical bars pulsing in sequence
  | dualRing        -- Two concentric rotating rings (opposite directions)
  -- Creative spinners
  | orbit           -- Dots orbiting a center point at different speeds
  | pulse           -- Expanding/contracting concentric rings
  | helix           -- DNA-like double helix rotating
  | wave            -- Dots following sine wave pattern
  | spiral          -- Drawing spiral that resets
  | clock           -- Clock hands rotating at different speeds
  | pendulum        -- Swinging pendulum with trail
  | ripple          -- Concentric circles expanding outward
  | heartbeat       -- Pulsing heart-like shape with ECG timing
  | gears           -- Two interlocking gears rotating
deriving Repr, BEq, Inhabited

namespace Spinner

/-- Dimensions for spinner rendering. -/
structure Dimensions where
  size : Float := 40.0        -- Overall size (width = height)
  strokeWidth : Float := 3.0  -- Line thickness for stroked elements
deriving Repr, Inhabited

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

/-- Configuration for spinner widget. -/
structure Config where
  variant : SpinnerVariant := .ring
  color : Option Color := none  -- Uses theme.primary.background if none
  speed : Float := 1.0          -- Animation speed multiplier (1.0 = normal)
  dims : Dimensions := {}
deriving Repr, Inhabited

/-- Default spinner dimensions. -/
def defaultDimensions : Dimensions := {}

/-- Get the color for rendering, defaulting to theme primary. -/
def getColor (config : Config) (theme : Theme) : Color :=
  config.color.getD theme.primary.background

/-! ## Standard Spinner Specs -/

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
        let dotPath := Arbor.Path.circle (Arbor.Point.mk' dx dy) dotRadius
        RenderM.fillPath dotPath dotColor
  draw := none
}

/-- Ring: Rotating arc segment (macOS/iOS style). -/
def ringSpec (t : Float) (color : Color) (dims : Dimensions) : CustomSpec := {
  measure := fun _ _ => (dims.size, dims.size)
  collect := fun layout =>
    let rect := layout.contentRect
    let cx := rect.x + dims.size / 2
    let cy := rect.y + dims.size / 2
    let radius := (dims.size - dims.strokeWidth) / 2 - 2
    let startAngle := t * Float.twoPi
    let sweepAngle := Float.pi * 1.5  -- 270° arc

    let arcPath := Arbor.Path.arcPath (Arbor.Point.mk' cx cy) radius startAngle (startAngle + sweepAngle)
    RenderM.build do
      RenderM.strokePath arcPath color dims.strokeWidth
  draw := none
}

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
        let dotPath := Arbor.Path.circle (Arbor.Point.mk' dx dy) dotRadius
        RenderM.fillPath dotPath color
  draw := none
}

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

/-- DualRing: Two concentric rings rotating in opposite directions. -/
def dualRingSpec (t : Float) (color : Color) (dims : Dimensions) : CustomSpec := {
  measure := fun _ _ => (dims.size, dims.size)
  collect := fun layout =>
    let rect := layout.contentRect
    let cx := rect.x + dims.size / 2
    let cy := rect.y + dims.size / 2
    let outerRadius := (dims.size - dims.strokeWidth) / 2 - 2
    let innerRadius := outerRadius * 0.6
    let outerAngle := t * Float.twoPi
    let innerAngle := -t * Float.twoPi * 1.5  -- Opposite direction, faster

    let outerArc := Arbor.Path.arcPath (Arbor.Point.mk' cx cy) outerRadius outerAngle (outerAngle + Float.pi)
    let innerArc := Arbor.Path.arcPath (Arbor.Point.mk' cx cy) innerRadius innerAngle (innerAngle + Float.pi * 0.75)

    RenderM.build do
      RenderM.strokePath outerArc color dims.strokeWidth
      RenderM.strokePath innerArc (color.withAlpha 0.6) (dims.strokeWidth * 0.7)
  draw := none
}

/-! ## Creative Spinner Specs -/

/-- Orbit: Dots orbiting center at different speeds and radii. -/
def orbitSpec (t : Float) (color : Color) (dims : Dimensions) : CustomSpec := {
  measure := fun _ _ => (dims.size, dims.size)
  collect := fun layout =>
    let rect := layout.contentRect
    let cx := rect.x + dims.size / 2
    let cy := rect.y + dims.size / 2

    -- Define orbits: (radius factor, speed multiplier, size factor, alpha)
    let orbits : Array (Float × Float × Float × Float) := #[
      (0.35, 1.0, 0.08, 1.0),
      (0.28, 1.7, 0.06, 0.8),
      (0.20, 2.5, 0.05, 0.6),
      (0.12, 4.0, 0.04, 0.4)
    ]

    RenderM.build do
      for (radiusFactor, speedMult, sizeFactor, alpha) in orbits do
        let angle := t * Float.twoPi * speedMult
        let radius := dims.size * radiusFactor
        let dx := cx + radius * Float.cos angle
        let dy := cy + radius * Float.sin angle
        let dotRadius := dims.size * sizeFactor
        let dotPath := Arbor.Path.circle (Arbor.Point.mk' dx dy) dotRadius
        RenderM.fillPath dotPath (color.withAlpha alpha)
  draw := none
}

/-- Pulse: Expanding concentric rings that fade as they grow. -/
def pulseSpec (t : Float) (color : Color) (dims : Dimensions) : CustomSpec := {
  measure := fun _ _ => (dims.size, dims.size)
  collect := fun layout =>
    let rect := layout.contentRect
    let cx := rect.x + dims.size / 2
    let cy := rect.y + dims.size / 2
    let maxRadius := dims.size * 0.45
    let numRings : Nat := 3

    RenderM.build do
      for i in [:numRings] do
        let phase := (t + i.toFloat / numRings.toFloat)
        let progress := phase - phase.floor
        let radius := maxRadius * progress
        let alpha := 1.0 - progress
        if alpha > 0.05 then
          let ringPath := Arbor.Path.circle (Arbor.Point.mk' cx cy) radius
          RenderM.strokePath ringPath (color.withAlpha alpha) dims.strokeWidth
  draw := none
}

/-- Helix: DNA-like double helix rotating. -/
def helixSpec (t : Float) (color : Color) (dims : Dimensions) : CustomSpec := {
  measure := fun _ _ => (dims.size, dims.size)
  collect := fun layout =>
    let rect := layout.contentRect
    let cx := rect.x + dims.size / 2
    let cy := rect.y + dims.size / 2
    let amplitude := dims.size * 0.3
    let numDots : Nat := 8
    let dotRadius := dims.size * 0.05

    RenderM.build do
      for i in [:numDots] do
        let yOffset := (i.toFloat / numDots.toFloat - 0.5) * dims.size * 0.7
        let phase := t * Float.twoPi + i.toFloat * Float.pi / 4.0
        -- Strand 1
        let x1 := cx + amplitude * Float.sin phase
        let depth1 := (Float.cos phase + 1.0) / 2.0
        let dotPath1 := Arbor.Path.circle (Arbor.Point.mk' x1 (cy + yOffset)) (dotRadius * (0.6 + 0.4 * depth1))
        RenderM.fillPath dotPath1 (color.withAlpha (0.4 + 0.6 * depth1))
        -- Strand 2 (180° offset)
        let x2 := cx + amplitude * Float.sin (phase + Float.pi)
        let depth2 := (Float.cos (phase + Float.pi) + 1.0) / 2.0
        let dotPath2 := Arbor.Path.circle (Arbor.Point.mk' x2 (cy + yOffset)) (dotRadius * (0.6 + 0.4 * depth2))
        RenderM.fillPath dotPath2 (color.withAlpha (0.4 + 0.6 * depth2))
  draw := none
}

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
        let dotPath := Arbor.Path.circle (Arbor.Point.mk' (cx + xOffset) (cy + yOffset)) dotRadius
        RenderM.fillPath dotPath color
  draw := none
}

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

/-- Clock: Clock hands rotating at different speeds. -/
def clockSpec (t : Float) (color : Color) (dims : Dimensions) : CustomSpec := {
  measure := fun _ _ => (dims.size, dims.size)
  collect := fun layout =>
    let rect := layout.contentRect
    let cx := rect.x + dims.size / 2
    let cy := rect.y + dims.size / 2
    let radius := dims.size * 0.4

    -- Second hand (fast), minute hand (slower), hour hand (slowest)
    let secondAngle := t * Float.twoPi - Float.halfPi
    let minuteAngle := t * Float.twoPi / 12.0 - Float.halfPi
    let hourAngle := t * Float.twoPi / 60.0 - Float.halfPi

    RenderM.build do
      -- Clock face circle
      let facePath := Arbor.Path.circle (Arbor.Point.mk' cx cy) radius
      RenderM.strokePath facePath (color.withAlpha 0.3) (dims.strokeWidth * 0.5)

      -- Hour hand (shortest, thickest)
      let hourEnd := Arbor.Point.mk' (cx + radius * 0.4 * Float.cos hourAngle)
                               (cy + radius * 0.4 * Float.sin hourAngle)
      let hourPath := Arbor.Path.empty |>.moveTo (Arbor.Point.mk' cx cy) |>.lineTo hourEnd
      RenderM.strokePath hourPath (color.withAlpha 0.8) (dims.strokeWidth * 1.5)

      -- Minute hand (medium)
      let minuteEnd := Arbor.Point.mk' (cx + radius * 0.65 * Float.cos minuteAngle)
                                 (cy + radius * 0.65 * Float.sin minuteAngle)
      let minutePath := Arbor.Path.empty |>.moveTo (Arbor.Point.mk' cx cy) |>.lineTo minuteEnd
      RenderM.strokePath minutePath (color.withAlpha 0.9) dims.strokeWidth

      -- Second hand (longest, thinnest)
      let secondEnd := Arbor.Point.mk' (cx + radius * 0.85 * Float.cos secondAngle)
                                 (cy + radius * 0.85 * Float.sin secondAngle)
      let secondPath := Arbor.Path.empty |>.moveTo (Arbor.Point.mk' cx cy) |>.lineTo secondEnd
      RenderM.strokePath secondPath color (dims.strokeWidth * 0.6)

      -- Center dot
      let centerDot := Arbor.Path.circle (Arbor.Point.mk' cx cy) (dims.strokeWidth * 0.8)
      RenderM.fillPath centerDot color
  draw := none
}

/-- Pendulum: Swinging pendulum with motion trail. -/
def pendulumSpec (t : Float) (color : Color) (dims : Dimensions) : CustomSpec := {
  measure := fun _ _ => (dims.size, dims.size)
  collect := fun layout =>
    let rect := layout.contentRect
    let cx := rect.x + dims.size / 2
    let pivotY := rect.y + dims.size * 0.15
    let length := dims.size * 0.6
    let maxAngle := Float.pi * 0.35
    let bobRadius := dims.size * 0.08

    -- Damped oscillation (sin for smooth back-and-forth)
    let angle := maxAngle * Float.sin (t * Float.twoPi)
    let bobX := cx + length * Float.sin angle
    let bobY := pivotY + length * Float.cos angle

    RenderM.build do
      -- Pivot point
      let pivotDot := Arbor.Path.circle (Arbor.Point.mk' cx pivotY) (dims.strokeWidth * 0.8)
      RenderM.fillPath pivotDot (color.withAlpha 0.6)

      -- Motion trail (ghost positions)
      for i in [:5] do
        let trailT := t - i.toFloat * 0.04
        let trailAngle := maxAngle * Float.sin (trailT * Float.twoPi)
        let trailX := cx + length * Float.sin trailAngle
        let trailY := pivotY + length * Float.cos trailAngle
        let alpha := 0.15 * (1.0 - i.toFloat / 5.0)
        let trailBob := Arbor.Path.circle (Arbor.Point.mk' trailX trailY) bobRadius
        RenderM.fillPath trailBob (color.withAlpha alpha)

      -- Rod
      let rodPath := Arbor.Path.empty |>.moveTo (Arbor.Point.mk' cx pivotY) |>.lineTo (Arbor.Point.mk' bobX bobY)
      RenderM.strokePath rodPath (color.withAlpha 0.7) (dims.strokeWidth * 0.7)

      -- Bob
      let bobPath := Arbor.Path.circle (Arbor.Point.mk' bobX bobY) bobRadius
      RenderM.fillPath bobPath color
  draw := none
}

/-- Ripple: Concentric circles expanding outward from center. -/
def rippleSpec (t : Float) (color : Color) (dims : Dimensions) : CustomSpec := {
  measure := fun _ _ => (dims.size, dims.size)
  collect := fun layout =>
    let rect := layout.contentRect
    let cx := rect.x + dims.size / 2
    let cy := rect.y + dims.size / 2
    let maxRadius := dims.size * 0.45
    let numRipples : Nat := 4

    RenderM.build do
      -- Center dot
      let centerDot := Arbor.Path.circle (Arbor.Point.mk' cx cy) (dims.size * 0.04)
      RenderM.fillPath centerDot color

      -- Expanding ripples
      for i in [:numRipples] do
        let phase := (t * 2.0 + i.toFloat / numRipples.toFloat)
        let progress := phase - phase.floor
        let radius := maxRadius * progress
        let alpha := (1.0 - progress) * 0.8
        if alpha > 0.05 then
          let ripplePath := Arbor.Path.circle (Arbor.Point.mk' cx cy) radius
          RenderM.strokePath ripplePath (color.withAlpha alpha) (dims.strokeWidth * 0.7)
  draw := none
}

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

/-- Gears: Two interlocking gears rotating in opposite directions. -/
def gearsSpec (t : Float) (color : Color) (dims : Dimensions) : CustomSpec := {
  measure := fun _ _ => (dims.size, dims.size)
  collect := fun layout =>
    let rect := layout.contentRect
    let cx := rect.x + dims.size / 2
    let cy := rect.y + dims.size / 2

    -- Two gears offset horizontally
    let gear1Center := Arbor.Point.mk' (cx - dims.size * 0.18) cy
    let gear2Center := Arbor.Point.mk' (cx + dims.size * 0.18) cy
    let gear1Radius := dims.size * 0.22
    let gear2Radius := dims.size * 0.18
    let gear1Teeth : Nat := 8
    let gear2Teeth : Nat := 6

    -- Gears rotate opposite directions, synced by teeth ratio
    let gear1Angle := t * Float.twoPi
    let gear2Angle := -t * Float.twoPi * (gear1Teeth.toFloat / gear2Teeth.toFloat)

    RenderM.build do
      -- Draw gear 1 (larger)
      let gear1Path := gearPath gear1Center gear1Radius gear1Teeth gear1Angle
      RenderM.fillPath gear1Path color
      let gear1Hub := Arbor.Path.circle gear1Center (gear1Radius * 0.25)
      RenderM.fillPath gear1Hub (color.withAlpha 0.3)

      -- Draw gear 2 (smaller)
      let gear2Path := gearPath gear2Center gear2Radius gear2Teeth gear2Angle
      RenderM.fillPath gear2Path (color.withAlpha 0.85)
      let gear2Hub := Arbor.Path.circle gear2Center (gear2Radius * 0.25)
      RenderM.fillPath gear2Hub (color.withAlpha 0.25)
  draw := none
}
where
  /-- Create a gear-shaped path. -/
  gearPath (center : Arbor.Point) (radius : Float) (teeth : Nat) (rotation : Float) : Arbor.Path := Id.run do
    let innerRadius := radius * 0.7
    let toothDepth := radius * 0.3
    let numPoints := teeth * 4  -- 4 points per tooth
    let angleStep := Float.twoPi / numPoints.toFloat

    let mut path := Arbor.Path.empty
    let mut first := true

    for i in [:numPoints] do
      let angle := rotation + i.toFloat * angleStep
      -- Alternate between inner and outer radius based on position in tooth cycle
      let posInTooth := i % 4
      let r := match posInTooth with
        | 0 => innerRadius           -- Valley
        | 1 => radius + toothDepth   -- Outer
        | 2 => radius + toothDepth   -- Outer
        | _ => innerRadius           -- Valley

      let pt := Arbor.Point.mk' (center.x + r * Float.cos angle) (center.y + r * Float.sin angle)
      if first then
        path := path.moveTo pt
        first := false
      else
        path := path.lineTo pt

    return path.closePath

/-- Get the appropriate spec for a spinner variant. -/
def variantSpec (variant : SpinnerVariant) (t : Float) (color : Color)
    (dims : Dimensions) : CustomSpec :=
  match variant with
  | .circleDots => circleDotsSpec t color dims
  | .ring => ringSpec t color dims
  | .bouncingDots => bouncingDotsSpec t color dims
  | .bars => barsSpec t color dims
  | .dualRing => dualRingSpec t color dims
  | .orbit => orbitSpec t color dims
  | .pulse => pulseSpec t color dims
  | .helix => helixSpec t color dims
  | .wave => waveSpec t color dims
  | .spiral => spiralSpec t color dims
  | .clock => clockSpec t color dims
  | .pendulum => pendulumSpec t color dims
  | .ripple => rippleSpec t color dims
  | .heartbeat => heartbeatSpec t color dims
  | .gears => gearsSpec t color dims

end Spinner

/-- Build a spinner (WidgetBuilder version).
    - `name`: Widget name for identification
    - `t`: Animation progress (0.0 to 1.0)
    - `config`: Spinner configuration
    - `theme`: Theme for styling
-/
def spinnerVisual (name : String) (t : Float) (config : Spinner.Config)
    (theme : Theme) : WidgetBuilder := do
  let color := Spinner.getColor config theme
  let baseSpec := Spinner.variantSpec config.variant t color config.dims
  let spec := { baseSpec with skipCache := true }

  let wid ← freshId
  let props : Trellis.FlexContainer := { Trellis.FlexContainer.column 0 with alignItems := .center }

  let spinnerWidget ← custom spec {
    minWidth := some config.dims.size
    minHeight := some config.dims.size
  }

  pure (.flex wid (some name) props {} #[spinnerWidget])

/-! ## Reactive Spinner Component (FRP-based) -/

open Reactive Reactive.Host
open Afferent.Canopy.Reactive

/-- Float modulo for animation cycling. -/
private def floatMod (x y : Float) : Float :=
  x - y * (x / y).floor

/-- Create a spinner component using WidgetM.
    Emits an animated spinner that cycles continuously.
    - `theme`: Theme for styling
    - `config`: Spinner configuration (variant, color, speed, dimensions)
-/
def spinner (theme : Theme) (config : Spinner.Config := {}) : WidgetM Unit := do
  let name ← registerComponentW "spinner" (isInteractive := false)

  -- Use shared elapsed time (all widgets share ONE Dynamic, no per-widget foldDyn)
  let elapsedTime ← useElapsedTime

  -- dynWidget auto-detects that this builder creates no subscriptions
  -- and uses the fast path (skips scope management) automatically.
  let cycleDuration : Float := 2.0 / config.speed
  let _ ← dynWidget elapsedTime fun t => do
    let progress := floatMod t cycleDuration / cycleDuration
    emit do pure (spinnerVisual name progress config theme)

/-- Convenience function: Create a default ring spinner. -/
def spinnerRing (theme : Theme) (color : Option Color := none)
    (size : Float := 40.0) : WidgetM Unit :=
  spinner theme { variant := .ring, color, dims := { size } }

/-- Convenience function: Create a circle dots spinner. -/
def spinnerCircleDots (theme : Theme) (color : Option Color := none)
    (size : Float := 40.0) : WidgetM Unit :=
  spinner theme { variant := .circleDots, color, dims := { size } }

/-- Convenience function: Create a bouncing dots spinner. -/
def spinnerBouncingDots (theme : Theme) (color : Option Color := none)
    (size : Float := 40.0) : WidgetM Unit :=
  spinner theme { variant := .bouncingDots, color, dims := { size } }

end Afferent.Canopy

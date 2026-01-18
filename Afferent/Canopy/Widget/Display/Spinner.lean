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
        RenderM.fillCircle (Arbor.Point.mk' dx dy) dotRadius dotColor
  draw := none
}

/-- Ring: Rotating arc segment (macOS/iOS style).
    Uses instanced arc rendering for GPU batching.
    Note: `t` is raw elapsed time in seconds, not wrapped progress. -/
def ringSpec (t : Float) (color : Color) (dims : Dimensions) : CustomSpec := {
  measure := fun _ _ => (dims.size, dims.size)
  collect := fun layout =>
    let rect := layout.contentRect
    let cx := rect.x + dims.size / 2
    let cy := rect.y + dims.size / 2
    let radius := (dims.size - dims.strokeWidth) / 2 - 2
    -- Use raw time directly - cos/sin handle any angle value smoothly
    let startAngle := t * Float.pi  -- Half rotation per second
    let sweepAngle := Float.pi * 1.5  -- 270° arc

    RenderM.build do
      RenderM.strokeArcInstanced #[{
        centerX := cx
        centerY := cy
        startAngle := startAngle
        sweepAngle := sweepAngle
        radius := radius
        strokeWidth := dims.strokeWidth
        r := color.r
        g := color.g
        b := color.b
        a := color.a
      }]
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
        RenderM.fillCircle (Arbor.Point.mk' dx dy) dotRadius color
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

/-- DualRing: Two concentric rings rotating in opposite directions.
    Uses instanced arc rendering for GPU batching - multiple spinners batch into single draw calls.
    Note: `t` is raw elapsed time in seconds, not wrapped progress. -/
def dualRingSpec (t : Float) (color : Color) (dims : Dimensions) : CustomSpec := {
  measure := fun _ _ => (dims.size, dims.size)
  collect := fun layout =>
    let rect := layout.contentRect
    let cx := rect.x + dims.size / 2
    let cy := rect.y + dims.size / 2
    let outerRadius := (dims.size - dims.strokeWidth) / 2 - 2
    let innerRadius := outerRadius * 0.6
    -- Use raw time directly - cos/sin handle any angle value smoothly
    let outerAngle := t * Float.pi  -- Half rotation per second
    let innerAngle := -t * Float.pi * 1.5  -- Opposite direction, faster
    let innerColor := color.withAlpha 0.6
    let innerStrokeWidth := dims.strokeWidth * 0.7

    RenderM.build do
      -- Outer arc (180°)
      RenderM.strokeArcInstanced #[{
        centerX := cx
        centerY := cy
        startAngle := outerAngle
        sweepAngle := Float.pi
        radius := outerRadius
        strokeWidth := dims.strokeWidth
        r := color.r
        g := color.g
        b := color.b
        a := color.a
      }]
      -- Inner arc (135°)
      RenderM.strokeArcInstanced #[{
        centerX := cx
        centerY := cy
        startAngle := innerAngle
        sweepAngle := Float.pi * 0.75
        radius := innerRadius
        strokeWidth := innerStrokeWidth
        r := innerColor.r
        g := innerColor.g
        b := innerColor.b
        a := innerColor.a
      }]
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
        RenderM.fillCircle (Arbor.Point.mk' dx dy) dotRadius (color.withAlpha alpha)
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
    let center := Arbor.Point.mk' cx cy

    RenderM.build do
      for i in [:numRings] do
        let phase := (t + i.toFloat / numRings.toFloat)
        let progress := phase - phase.floor
        let radius := maxRadius * progress
        let alpha := 1.0 - progress
        if alpha > 0.05 then
          RenderM.strokeCircle center radius (color.withAlpha alpha) dims.strokeWidth
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
        RenderM.fillCircle (Arbor.Point.mk' x1 (cy + yOffset)) (dotRadius * (0.6 + 0.4 * depth1)) (color.withAlpha (0.4 + 0.6 * depth1))
        -- Strand 2 (180° offset)
        let x2 := cx + amplitude * Float.sin (phase + Float.pi)
        let depth2 := (Float.cos (phase + Float.pi) + 1.0) / 2.0
        RenderM.fillCircle (Arbor.Point.mk' x2 (cy + yOffset)) (dotRadius * (0.6 + 0.4 * depth2)) (color.withAlpha (0.4 + 0.6 * depth2))
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
        RenderM.fillCircle (Arbor.Point.mk' (cx + xOffset) (cy + yOffset)) dotRadius color
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
      RenderM.fillCircle (Arbor.Point.mk' cx pivotY) (dims.strokeWidth * 0.8) (color.withAlpha 0.6)

      -- Motion trail (ghost positions)
      for i in [:5] do
        let trailT := t - i.toFloat * 0.04
        let trailAngle := maxAngle * Float.sin (trailT * Float.twoPi)
        let trailX := cx + length * Float.sin trailAngle
        let trailY := pivotY + length * Float.cos trailAngle
        let alpha := 0.15 * (1.0 - i.toFloat / 5.0)
        RenderM.fillCircle (Arbor.Point.mk' trailX trailY) bobRadius (color.withAlpha alpha)

      -- Rod (uses strokeLineBatch for batching)
      RenderM.strokeLineBatch #[cx, pivotY, bobX, bobY, color.r, color.g, color.b, color.a * 0.7, 0.0] 1 (dims.strokeWidth * 0.7)

      -- Bob
      RenderM.fillCircle (Arbor.Point.mk' bobX bobY) bobRadius color
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

/-! ## Precomputed Gear Tessellation for Instanced Rendering -/

/-- Tessellate a canonical gear (centered at origin, outer radius = 1.0) as triangles.
    Returns (vertices, indices, centerX, centerY) for instanced rendering. -/
private def tessellateCanonicalGear (teeth : Nat) : Array Float × Array UInt32 × Float × Float := Id.run do
  let innerRadius : Float := 0.7   -- Inner radius as fraction of outer
  let toothDepth : Float := 0.3    -- Tooth extends outward by this fraction
  let outerRadius : Float := 1.0 + toothDepth  -- Total outer extent
  let numPoints := teeth * 4  -- 4 points per tooth
  let angleStep := Float.twoPi / numPoints.toFloat

  -- Generate perimeter points (at rotation=0)
  let mut perimeterPoints : Array (Float × Float) := Array.mkEmpty numPoints
  for i in [:numPoints] do
    let angle := i.toFloat * angleStep
    let posInTooth := i % 4
    let r := match posInTooth with
      | 0 => innerRadius           -- Valley
      | 1 => outerRadius           -- Outer
      | 2 => outerRadius           -- Outer
      | _ => innerRadius           -- Valley
    perimeterPoints := perimeterPoints.push (r * Float.cos angle, r * Float.sin angle)

  -- Vertices: center (0,0) + perimeter points
  -- Layout: [cx, cy, x0, y0, x1, y1, ...]
  let mut vertices : Array Float := Array.mkEmpty ((numPoints + 1) * 2)
  vertices := vertices.push 0.0 |>.push 0.0  -- Center vertex
  for (x, y) in perimeterPoints do
    vertices := vertices.push x |>.push y

  -- Indices: triangle fan from center
  -- Each triangle: center (0), point i, point (i+1) % numPoints
  let mut indices : Array UInt32 := Array.mkEmpty (numPoints * 3)
  for i in [:numPoints] do
    indices := indices.push 0  -- Center
    indices := indices.push (i + 1).toUInt32
    indices := indices.push ((i % numPoints) + 1 + 1).toUInt32
  -- Fix last triangle to wrap around
  indices := indices.set! (indices.size - 1) 1

  return (vertices, indices, 0.0, 0.0)

/-- Pre-computed 8-tooth gear tessellation. -/
private def gear8Tessellation : Array Float × Array UInt32 × Float × Float :=
  tessellateCanonicalGear 8

/-- Pre-computed 6-tooth gear tessellation. -/
private def gear6Tessellation : Array Float × Array UInt32 × Float × Float :=
  tessellateCanonicalGear 6

/-- Hash for 8-tooth gear (FNV-1a of "gear8"). -/
private def gear8Hash : UInt64 := 0x67656172385F5F5F  -- "gear8___"

/-- Hash for 6-tooth gear (FNV-1a of "gear6"). -/
private def gear6Hash : UInt64 := 0x67656172365F5F5F  -- "gear6___"

/-- Gears: Two interlocking gears rotating in opposite directions.
    Uses instanced rendering for high performance with many gear copies. -/
def gearsSpec (t : Float) (color : Color) (dims : Dimensions) : CustomSpec := {
  measure := fun _ _ => (dims.size, dims.size)
  collect := fun layout =>
    let rect := layout.contentRect
    let cx := rect.x + dims.size / 2
    let cy := rect.y + dims.size / 2

    -- Two gears offset horizontally
    let gear1X := cx - dims.size * 0.18
    let gear1Y := cy
    let gear2X := cx + dims.size * 0.18
    let gear2Y := cy
    let gear1Scale := dims.size * 0.22  -- Scale to desired size
    let gear2Scale := dims.size * 0.18
    let gear1Teeth : Nat := 8
    let gear2Teeth : Nat := 6

    -- Gears rotate opposite directions, synced by teeth ratio
    let gear1Angle := t * Float.twoPi
    let gear2Angle := -t * Float.twoPi * (gear1Teeth.toFloat / gear2Teeth.toFloat)

    -- Get pre-computed tessellations
    let (gear8Verts, gear8Indices, gear8CX, gear8CY) := gear8Tessellation
    let (gear6Verts, gear6Indices, gear6CX, gear6CY) := gear6Tessellation

    RenderM.build do
      -- Draw gear 1 (8-tooth) using instanced rendering
      let gear1Instance : MeshInstance := {
        x := gear1X, y := gear1Y
        rotation := gear1Angle
        scale := gear1Scale
        r := color.r, g := color.g, b := color.b, a := color.a
      }
      RenderM.fillPolygonInstanced gear8Hash gear8Verts gear8Indices #[gear1Instance] gear8CX gear8CY

      -- Draw gear 2 (6-tooth) using instanced rendering
      let gear2Color := color.withAlpha 0.85
      let gear2Instance : MeshInstance := {
        x := gear2X, y := gear2Y
        rotation := gear2Angle
        scale := gear2Scale
        r := gear2Color.r, g := gear2Color.g, b := gear2Color.b, a := gear2Color.a
      }
      RenderM.fillPolygonInstanced gear6Hash gear6Verts gear6Indices #[gear2Instance] gear6CX gear6CY

      -- Center holes (using GPU-batched circles)
      RenderM.fillCircle (Arbor.Point.mk' gear1X gear1Y) (gear1Scale * 0.25) (color.withAlpha 0.3)
      RenderM.fillCircle (Arbor.Point.mk' gear2X gear2Y) (gear2Scale * 0.25) (color.withAlpha 0.25)
  draw := none
}

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

  -- Ring variants use raw time for seamless animation (cos/sin handle any angle).
  -- Other variants use wrapped progress [0,1) for their animation cycles.
  let cycleDuration : Float := 2.0 / config.speed
  let useRawTime := config.variant == .ring || config.variant == .dualRing
  let _ ← dynWidget elapsedTime fun t => do
    let animTime := if useRawTime then t * config.speed else floatMod t cycleDuration / cycleDuration
    emit do pure (spinnerVisual name animTime config theme)

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

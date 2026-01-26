/-
  Canopy GaugeChart Widget
  Speedometer-style gauge chart for showing progress toward a goal.
-/
import Reactive
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Reactive.Component
import Afferent.Canopy.Widget.Charts.Core

namespace Afferent.Canopy

open Afferent.Arbor hiding Event

namespace GaugeChart

/-- Pi constant for angle calculations. -/
private def pi : Float := 3.14159265358979323846

/-- Dimensions and styling for gauge chart rendering. -/
structure Dimensions extends ChartSize where
  width := 250.0
  height := 180.0
  radius : Float := 80.0
  arcThickness : Float := 20.0
  needleLength : Float := 65.0
  needleWidth : Float := 4.0
  startAngle : Float := 0.75  -- Start at 135° (0.75 * π)
  endAngle : Float := 0.25    -- End at 45° (0.25 * π, wrapping around)
  showTicks : Bool := true
  tickCount : Nat := 5
  tickLength : Float := 8.0
  showValue : Bool := true
  showMinMax : Bool := true
  showLabel : Bool := true
deriving Repr, Inhabited

/-- Default gauge chart dimensions. -/
def defaultDimensions : Dimensions := {}

/-- A colored segment of the gauge arc. -/
structure Segment where
  /-- Start value (as fraction 0-1). -/
  startFrac : Float
  /-- End value (as fraction 0-1). -/
  endFrac : Float
  /-- Color for this segment. -/
  color : Color
deriving Repr, Inhabited, BEq

/-- Default segments (green-yellow-red traffic light style). -/
def defaultSegments : Array Segment := #[
  { startFrac := 0.0, endFrac := 0.5, color := Color.rgba 0.20 0.69 0.35 1.0 },   -- Green
  { startFrac := 0.5, endFrac := 0.75, color := Color.rgba 0.95 0.75 0.10 1.0 },  -- Yellow
  { startFrac := 0.75, endFrac := 1.0, color := Color.rgba 0.90 0.25 0.20 1.0 }   -- Red
]

/-- Gauge chart colors. -/
structure ChartColors where
  needle : Color := Color.rgba 0.85 0.85 0.85 1.0
  needleCenter : Color := Color.rgba 0.60 0.60 0.60 1.0
  tickMarks : Color := Color.rgba 0.70 0.70 0.70 1.0
  background : Color := Color.rgba 0.20 0.20 0.20 0.5
deriving Repr, Inhabited, BEq

/-- Default chart colors. -/
def defaultColors : ChartColors := {}

/-- Gauge chart data. -/
structure Data where
  /-- Current value. -/
  value : Float
  /-- Minimum value. -/
  minValue : Float := 0.0
  /-- Maximum value. -/
  maxValue : Float := 100.0
  /-- Optional label (e.g., "Speed", "CPU Usage"). -/
  label : Option String := none
  /-- Optional unit (e.g., "mph", "%"). -/
  unit : Option String := none
  /-- Colored segments (uses default if empty). -/
  segments : Array Segment := #[]
deriving Repr, Inhabited, BEq

/-- Format a value for display. -/
private def formatValue (v : Float) (unit : Option String) : String :=
  let numStr := if v == v.floor then
    s!"{v.floor.toInt32}"
  else
    let whole := v.floor.toInt32
    let frac := ((v - v.floor).abs * 10).floor.toUInt32
    s!"{whole}.{frac}"
  match unit with
  | some u => s!"{numStr}{u}"
  | none => numStr

/-- Calculate angle for a value fraction (0-1). -/
private def fractionToAngle (frac : Float) (dims : Dimensions) : Float :=
  -- Gauge goes from startAngle*π to endAngle*π (going through bottom)
  -- Total arc is (2 - startAngle + endAngle) * π for a typical gauge
  let startRad := dims.startAngle * pi
  -- Arc goes clockwise from start to end (through bottom)
  let totalArc := (2.0 - dims.startAngle + dims.endAngle) * pi
  startRad + frac * totalArc

/-- Custom spec for gauge chart rendering. -/
def gaugeChartSpec (data : Data) (theme : Theme)
    (colors : ChartColors := defaultColors)
    (dims : Dimensions := defaultDimensions) : CustomSpec := {
  measure := fun _ _ => (50, 50)  -- Minimum size for circular gauge
  collect := fun layout => RenderM.build do
    let rect := layout.contentRect
    let actualWidth := rect.width
    let actualHeight := rect.height

    -- Use minimum of width/height for radius calculations (circular gauge)
    let minDim := min actualWidth actualHeight
    let radius := minDim * 0.35  -- Scale radius to fit
    let arcThickness := minDim * 0.08
    let needleLength := radius * 0.8
    let needleWidth := minDim * 0.02
    let tickLength := minDim * 0.035

    -- Calculate center point
    let centerX := rect.x + actualWidth / 2
    let centerY := rect.y + actualHeight * 0.55  -- Slightly below center for visual balance

    -- Normalize value to 0-1 fraction
    let valueRange := data.maxValue - data.minValue
    let valueRange := if valueRange <= 0.0 then 1.0 else valueRange
    let valueFrac := (data.value - data.minValue) / valueRange
    let valueFrac := max 0.0 (min 1.0 valueFrac)

    -- Get segments (use defaults if empty)
    let segments := if data.segments.isEmpty then defaultSegments else data.segments

    -- Draw background arc
    let bgArcPath := Arbor.Path.arcPath
      (Arbor.Point.mk' centerX centerY)
      radius
      (fractionToAngle 0.0 dims)
      (fractionToAngle 1.0 dims)
    RenderM.strokePath bgArcPath colors.background arcThickness

    -- Draw colored segments
    for seg in segments do
      let startAngle := fractionToAngle seg.startFrac dims
      let endAngle := fractionToAngle seg.endFrac dims
      let segPath := Arbor.Path.arcPath
        (Arbor.Point.mk' centerX centerY)
        radius
        startAngle
        endAngle
      RenderM.strokePath segPath seg.color arcThickness

    -- Draw tick marks
    if dims.showTicks && dims.tickCount > 0 then
      for i in [0:dims.tickCount + 1] do
        let frac := i.toFloat / dims.tickCount.toFloat
        let angle := fractionToAngle frac dims
        let innerR := radius - arcThickness / 2 - 2
        let outerR := innerR - tickLength
        let cosA := Float.cos angle
        let sinA := Float.sin angle
        let x1 := centerX + innerR * cosA
        let y1 := centerY + innerR * sinA
        let x2 := centerX + outerR * cosA
        let y2 := centerY + outerR * sinA
        let tickPath := Arbor.Path.empty
          |>.moveTo (Arbor.Point.mk' x1 y1)
          |>.lineTo (Arbor.Point.mk' x2 y2)
        RenderM.strokePath tickPath colors.tickMarks 1.5

    -- Draw needle
    let needleAngle := fractionToAngle valueFrac dims
    let needleTipX := centerX + needleLength * Float.cos needleAngle
    let needleTipY := centerY + needleLength * Float.sin needleAngle
    -- Draw needle as a tapered line (triangle would be better but line works)
    let needlePath := Arbor.Path.empty
      |>.moveTo (Arbor.Point.mk' centerX centerY)
      |>.lineTo (Arbor.Point.mk' needleTipX needleTipY)
    RenderM.strokePath needlePath colors.needle needleWidth

    -- GPU-batched center circle
    let centerCircleRadius := minDim * 0.035
    RenderM.fillCircle' centerX centerY centerCircleRadius colors.needleCenter

    -- Draw current value
    if dims.showValue then
      let valueStr := formatValue data.value data.unit
      let valueY := centerY + minDim * 0.12
      RenderM.fillText valueStr centerX valueY theme.font theme.text

    -- Draw label
    if dims.showLabel then
      match data.label with
      | some label =>
        let labelY := centerY + minDim * 0.2
        RenderM.fillText label centerX labelY theme.smallFont theme.textMuted
      | none => pure ()

    -- Draw min/max labels
    if dims.showMinMax then
      let minAngle := fractionToAngle 0.0 dims
      let maxAngle := fractionToAngle 1.0 dims
      let labelRadius := radius + minDim * 0.07
      let minX := centerX + labelRadius * Float.cos minAngle
      let minY := centerY + labelRadius * Float.sin minAngle
      let maxX := centerX + labelRadius * Float.cos maxAngle
      let maxY := centerY + labelRadius * Float.sin maxAngle
      let minStr := formatValue data.minValue none
      let maxStr := formatValue data.maxValue none
      RenderM.fillText minStr minX minY theme.smallFont theme.textMuted
      RenderM.fillText maxStr maxX maxY theme.smallFont theme.textMuted

  draw := none
}

end GaugeChart

/-- Build a gauge chart visual (WidgetBuilder version).
    - `name`: Widget name for identification
    - `data`: Gauge chart data
    - `theme`: Theme for styling
    - `colors`: Chart colors
    - `dims`: Chart dimensions
-/
def gaugeChartVisual (name : String) (data : GaugeChart.Data)
    (theme : Theme) (colors : GaugeChart.ChartColors := GaugeChart.defaultColors)
    (dims : GaugeChart.Dimensions := GaugeChart.defaultDimensions)
    : WidgetBuilder := do
  let wid ← freshId
  let chart ← custom (GaugeChart.gaugeChartSpec data theme colors dims) {
    width := .percent 1.0
    height := .percent 1.0
    flexItem := some (Trellis.FlexItem.growing 1)
  }
  let style : BoxStyle := {
    width := .percent 1.0
    height := .percent 1.0
    minWidth := some dims.width
    minHeight := some dims.height
    flexItem := some (Trellis.FlexItem.growing 1)
  }
  let props : Trellis.FlexContainer := { Trellis.FlexContainer.column 0 with alignItems := .stretch }
  pure (.flex wid (some name) props style #[chart])

/-! ## Reactive GaugeChart Components (FRP-based)

These use WidgetM for declarative composition.
-/

open Reactive Reactive.Host
open Afferent.Canopy.Reactive

/-- GaugeChart result - provides access to chart state. -/
structure GaugeChartResult where
  /-- The data being displayed. -/
  data : Dyn GaugeChart.Data

/-- Create a gauge chart component using WidgetM with dynamic data.
    The chart automatically rebuilds when the data Dynamic changes.
    - `data`: Dynamic gauge chart data
    - `theme`: Theme for styling
    - `colors`: Chart colors
    - `dims`: Chart dimensions
-/
def gaugeChart (data : Dyn GaugeChart.Data)
    (colors : GaugeChart.ChartColors := GaugeChart.defaultColors)
    (dims : GaugeChart.Dimensions := GaugeChart.defaultDimensions)
    : WidgetM GaugeChartResult := do
  let theme ← getThemeW
  let _ ← dynWidget data fun currentData => do
    let name ← registerComponentW "gauge-chart" (isInteractive := false)
    emit do pure (gaugeChartVisual name currentData theme colors dims)

  pure { data }

/-- Create a simple gauge chart with dynamic value.
    - `value`: Dynamic current value
    - `minValue`: Minimum value
    - `maxValue`: Maximum value
    - `label`: Optional label
    - `unit`: Optional unit
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def gaugeChartSimple (value : Dyn Float)
    (minValue : Float := 0.0) (maxValue : Float := 100.0)
    (label : Option String := none) (unit : Option String := none)
    (dims : GaugeChart.Dimensions := GaugeChart.defaultDimensions)
    : WidgetM GaugeChartResult := do
  let dataDyn ← Dynamic.mapM (fun v =>
    ({ value := v, minValue, maxValue, label, unit } : GaugeChart.Data)
  ) value
  gaugeChart dataDyn GaugeChart.defaultColors dims

end Afferent.Canopy

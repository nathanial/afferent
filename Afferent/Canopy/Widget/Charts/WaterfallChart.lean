/-
  Canopy WaterfallChart Widget
  Waterfall chart for showing cumulative effect of sequential positive/negative values.
-/
import Reactive
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Reactive.Component

namespace Afferent.Canopy

open Afferent.Arbor hiding Event

namespace WaterfallChart

/-- Dimensions and styling for waterfall chart rendering. -/
structure Dimensions where
  width : Float := 400.0
  height : Float := 280.0
  marginTop : Float := 20.0
  marginBottom : Float := 40.0
  marginLeft : Float := 50.0
  marginRight : Float := 20.0
  barGap : Float := 8.0
  connectorWidth : Float := 1.0
  showGridLines : Bool := true
  gridLineCount : Nat := 5
  showConnectors : Bool := true
deriving Repr, Inhabited

/-- Default waterfall chart dimensions. -/
def defaultDimensions : Dimensions := {}

/-- Colors for different bar types. -/
structure ChartColors where
  increase : Color := Color.rgba 0.20 0.69 0.35 1.0   -- Green for positive
  decrease : Color := Color.rgba 0.90 0.25 0.20 1.0   -- Red for negative
  total : Color := Color.rgba 0.29 0.53 0.91 1.0      -- Blue for totals
  connector : Color := Color.rgba 0.50 0.50 0.50 0.6  -- Gray for connectors
deriving Repr, Inhabited

/-- Default chart colors. -/
def defaultColors : ChartColors := {}

/-- Type of waterfall bar. -/
inductive BarType where
  | initial   -- Starting value
  | increase  -- Positive change
  | decrease  -- Negative change
  | total     -- Subtotal or final total
deriving Repr, Inhabited, BEq

/-- A single bar in the waterfall chart. -/
structure Bar where
  /-- Label for this bar. -/
  label : String
  /-- Value (positive for increase, negative for decrease, absolute for initial/total). -/
  value : Float
  /-- Type of bar. -/
  barType : BarType := .increase
deriving Repr, Inhabited

/-- Waterfall chart data. -/
structure Data where
  /-- Array of bars in order. -/
  bars : Array Bar
deriving Repr, Inhabited

/-- Format a value for axis labels. -/
private def formatValue (v : Float) : String :=
  if v >= 1000000 then
    s!"{(v / 1000000).floor.toUInt32}M"
  else if v >= 1000 then
    s!"{(v / 1000).floor.toUInt32}K"
  else if v == v.floor then
    s!"{v.floor.toInt32}"
  else
    let whole := v.floor.toInt32
    let frac := ((v - v.floor).abs * 10).floor.toUInt32
    s!"{whole}.{frac}"

/-- Calculate running totals and bar positions. -/
private def calculatePositions (data : Data) : Array (Float × Float × BarType) := Id.run do
  -- Returns (startY, endY, barType) for each bar
  let mut result : Array (Float × Float × BarType) := #[]
  let mut runningTotal : Float := 0.0

  for bar in data.bars do
    match bar.barType with
    | .initial =>
      -- Initial value starts from 0
      result := result.push (0.0, bar.value, .initial)
      runningTotal := bar.value
    | .increase =>
      -- Positive change: starts at running total, ends higher
      let startVal := runningTotal
      let endVal := runningTotal + bar.value
      result := result.push (startVal, endVal, .increase)
      runningTotal := endVal
    | .decrease =>
      -- Negative change: starts at running total, ends lower
      let startVal := runningTotal
      let endVal := runningTotal + bar.value  -- value is negative
      result := result.push (startVal, endVal, .decrease)
      runningTotal := endVal
    | .total =>
      -- Total bar: shows absolute value from 0
      result := result.push (0.0, bar.value, .total)
      runningTotal := bar.value

  result

/-- Find the min and max values for scaling. -/
private def findValueRange (positions : Array (Float × Float × BarType)) : (Float × Float) := Id.run do
  let mut minVal : Float := 0.0
  let mut maxVal : Float := 0.0

  for (startVal, endVal, _) in positions do
    if startVal < minVal then minVal := startVal
    if endVal < minVal then minVal := endVal
    if startVal > maxVal then maxVal := startVal
    if endVal > maxVal then maxVal := endVal

  -- Add padding
  let range := maxVal - minVal
  let padding := range * 0.1
  (minVal - padding, maxVal + padding)

/-- Custom spec for waterfall chart rendering. -/
def waterfallChartSpec (data : Data) (theme : Theme)
    (colors : ChartColors := defaultColors)
    (dims : Dimensions := defaultDimensions) : CustomSpec := {
  measure := fun _ _ => (dims.marginLeft + dims.marginRight + 50, dims.marginTop + dims.marginBottom + 30)
  collect := fun layout => RenderM.build do
    let rect := layout.contentRect
    let actualWidth := rect.width
    let actualHeight := rect.height

    let numBars := data.bars.size
    if numBars == 0 then return

    -- Calculate positions
    let positions := calculatePositions data
    let (minVal, maxVal) := findValueRange positions
    let valueRange := maxVal - minVal
    let valueRange := if valueRange <= 0.0 then 1.0 else valueRange

    -- Calculate chart area
    let chartX := rect.x + dims.marginLeft
    let chartY := rect.y + dims.marginTop
    let chartWidth := actualWidth - dims.marginLeft - dims.marginRight
    let chartHeight := actualHeight - dims.marginTop - dims.marginBottom

    -- Calculate bar width
    let totalGapWidth := dims.barGap * (numBars + 1).toFloat
    let barWidth := (chartWidth - totalGapWidth) / numBars.toFloat

    -- Helper to convert value to Y coordinate (inverted - higher values at top)
    let valueToY := fun (v : Float) =>
      chartY + chartHeight - ((v - minVal) / valueRange) * chartHeight

    -- Draw background
    let bgRect := Arbor.Rect.mk' rect.x rect.y actualWidth actualHeight
    RenderM.fillRect bgRect (theme.panel.background.withAlpha 0.3) 6.0

    -- Draw grid lines
    if dims.showGridLines && dims.gridLineCount > 0 then
      for i in [0:dims.gridLineCount + 1] do
        let ratio := i.toFloat / dims.gridLineCount.toFloat
        let lineY := chartY + chartHeight - (ratio * chartHeight)
        let lineRect := Arbor.Rect.mk' chartX lineY chartWidth 1.0
        RenderM.fillRect lineRect (Color.gray 0.3) 0.0

    -- Draw zero line if it's within the chart area
    if minVal < 0.0 && maxVal > 0.0 then
      let zeroY := valueToY 0.0
      let zeroRect := Arbor.Rect.mk' chartX zeroY chartWidth 1.5
      RenderM.fillRect zeroRect (Color.gray 0.5) 0.0

    -- Draw connectors and bars
    let mut prevEndY : Option Float := none

    for i in [0:numBars] do
      let (startVal, endVal, barType) := positions[i]!
      let barX := chartX + dims.barGap + i.toFloat * (barWidth + dims.barGap)

      let startY := valueToY startVal
      let endY := valueToY endVal

      -- Draw connector from previous bar (if enabled and not first bar)
      if dims.showConnectors then
        match prevEndY with
        | some prevY =>
          -- Only draw connector if this isn't a total bar
          if barType != .total then
            let connectorRect := Arbor.Rect.mk' (barX - dims.barGap) prevY dims.barGap dims.connectorWidth
            RenderM.fillRect connectorRect colors.connector 0.0
        | none => pure ()

      -- Determine bar color
      let barColor := match barType with
        | .initial => colors.total
        | .increase => colors.increase
        | .decrease => colors.decrease
        | .total => colors.total

      -- Draw bar (from startY to endY)
      let barTop := min startY endY
      let barBottom := max startY endY
      let barHeight := max 2.0 (barBottom - barTop)
      let barRect := Arbor.Rect.mk' barX barTop barWidth barHeight
      RenderM.fillRect barRect barColor 2.0

      -- Update previous end Y for connector
      prevEndY := some endY

    -- Draw Y-axis labels
    if dims.gridLineCount > 0 then
      for i in [0:dims.gridLineCount + 1] do
        let ratio := i.toFloat / dims.gridLineCount.toFloat
        let value := minVal + ratio * valueRange
        let labelY := chartY + chartHeight - (ratio * chartHeight) + 4
        let labelText := formatValue value
        RenderM.fillText labelText (rect.x + 4) labelY theme.smallFont theme.textMuted

    -- Draw X-axis labels
    for i in [0:numBars] do
      let bar := data.bars[i]!
      let barX := chartX + dims.barGap + i.toFloat * (barWidth + dims.barGap)
      let labelX := barX + barWidth / 2
      let labelY := chartY + chartHeight + 16
      RenderM.fillText bar.label labelX labelY theme.smallFont theme.text

    -- Draw axes
    let axisColor := Color.gray 0.5
    let yAxisRect := Arbor.Rect.mk' chartX chartY 1.0 chartHeight
    RenderM.fillRect yAxisRect axisColor 0.0
    let xAxisRect := Arbor.Rect.mk' chartX (chartY + chartHeight) chartWidth 1.0
    RenderM.fillRect xAxisRect axisColor 0.0

  draw := none
}

end WaterfallChart

/-- Build a waterfall chart visual (WidgetBuilder version).
    - `name`: Widget name for identification
    - `data`: Waterfall chart data with bars
    - `theme`: Theme for styling
    - `colors`: Chart colors
    - `dims`: Chart dimensions
-/
def waterfallChartVisual (name : String) (data : WaterfallChart.Data)
    (theme : Theme) (colors : WaterfallChart.ChartColors := WaterfallChart.defaultColors)
    (dims : WaterfallChart.Dimensions := WaterfallChart.defaultDimensions)
    : WidgetBuilder := do
  let wid ← freshId
  let chart ← custom (WaterfallChart.waterfallChartSpec data theme colors dims) {
    width := .percent 1.0
    height := .percent 1.0
    flexItem := some (Trellis.FlexItem.growing 1)
  }
  let style : BoxStyle := { width := .percent 1.0, height := .percent 1.0, flexItem := some (Trellis.FlexItem.growing 1) }
  let props : Trellis.FlexContainer := { Trellis.FlexContainer.column 0 with alignItems := .stretch }
  pure (.flex wid (some name) props style #[chart])

/-! ## Reactive WaterfallChart Components (FRP-based)

These use WidgetM for declarative composition.
-/

open Reactive Reactive.Host
open Afferent.Canopy.Reactive

/-- WaterfallChart result - provides access to chart state. -/
structure WaterfallChartResult where
  /-- The data being displayed. -/
  data : Reactive.Dynamic Spider WaterfallChart.Data

/-- Create a waterfall chart component using WidgetM.
    - `data`: Waterfall chart data with bars
    - `theme`: Theme for styling
    - `colors`: Chart colors
    - `dims`: Chart dimensions
-/
def waterfallChart (data : WaterfallChart.Data)
    (theme : Theme) (colors : WaterfallChart.ChartColors := WaterfallChart.defaultColors)
    (dims : WaterfallChart.Dimensions := WaterfallChart.defaultDimensions)
    : WidgetM WaterfallChartResult := do
  let name ← registerComponentW "waterfall-chart" (isInteractive := false)

  let dataDyn ← Dynamic.pureM data

  emit do
    pure (waterfallChartVisual name data theme colors dims)

  pure { data := dataDyn }

/-- Create a waterfall chart from simple arrays.
    - `labels`: Labels for each bar
    - `values`: Values for each bar (positive for increase, negative for decrease)
    - `barTypes`: Type of each bar
    - `theme`: Theme for styling
    - `colors`: Chart colors
    - `dims`: Chart dimensions
-/
def waterfallChartFromArrays (labels : Array String) (values : Array Float)
    (barTypes : Array WaterfallChart.BarType)
    (theme : Theme) (colors : WaterfallChart.ChartColors := WaterfallChart.defaultColors)
    (dims : WaterfallChart.Dimensions := WaterfallChart.defaultDimensions)
    : WidgetM WaterfallChartResult := do
  let numBars := min labels.size (min values.size barTypes.size)
  let bars := Id.run do
    let mut result : Array WaterfallChart.Bar := #[]
    for i in [0:numBars] do
      result := result.push {
        label := labels[i]!
        value := values[i]!
        barType := barTypes[i]!
      }
    result
  let data : WaterfallChart.Data := { bars }
  waterfallChart data theme colors dims

/-- Create a waterfall chart that updates based on an external event stream.
    - `initialData`: Initial chart data
    - `dataUpdates`: Event stream of data updates
    - `theme`: Theme for styling
    - `colors`: Chart colors
    - `dims`: Chart dimensions
-/
def waterfallChartWithEvents (initialData : WaterfallChart.Data)
    (dataUpdates : Reactive.Event Spider WaterfallChart.Data)
    (theme : Theme) (colors : WaterfallChart.ChartColors := WaterfallChart.defaultColors)
    (dims : WaterfallChart.Dimensions := WaterfallChart.defaultDimensions)
    : WidgetM WaterfallChartResult := do
  let name ← registerComponentW "waterfall-chart" (isInteractive := false)

  let dataDyn ← Reactive.holdDyn initialData dataUpdates

  emit do
    let d ← dataDyn.sample
    pure (waterfallChartVisual name d theme colors dims)

  pure { data := dataDyn }

end Afferent.Canopy

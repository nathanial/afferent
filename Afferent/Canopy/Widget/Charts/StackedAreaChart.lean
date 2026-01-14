/-
  Canopy StackedAreaChart Widget
  Stacked area chart for showing composition over time with filled areas.
-/
import Reactive
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Reactive.Host.Spider
import Afferent.Canopy.Reactive.Component

namespace Afferent.Canopy

open Afferent.Arbor hiding Event
open Reactive.Host.Spider

namespace StackedAreaChart

/-- Dimensions and spacing for stacked area chart rendering. -/
structure Dimensions where
  width : Float := 400.0
  height : Float := 280.0
  marginTop : Float := 20.0
  marginBottom : Float := 40.0
  marginLeft : Float := 50.0
  marginRight : Float := 100.0  -- Extra space for legend
  lineWidth : Float := 1.5
  fillOpacity : Float := 0.7
  showLines : Bool := true
  showGridLines : Bool := true
  gridLineCount : Nat := 5
  showLegend : Bool := true
  legendItemHeight : Float := 16.0
deriving Repr, Inhabited

/-- Default stacked area chart dimensions. -/
def defaultDimensions : Dimensions := {}

/-- A single data series for stacking. -/
structure Series where
  /-- Name of the series (shown in legend). -/
  name : String
  /-- Values for each data point. -/
  values : Array Float
  /-- Color for this series. -/
  color : Option Color := none
deriving Repr, Inhabited

/-- Stacked area chart data. -/
structure Data where
  /-- X-axis labels. -/
  labels : Array String
  /-- Data series to stack. -/
  series : Array Series
deriving Repr, Inhabited

/-- Default series colors. -/
def defaultColors : Array Color := #[
  Color.rgba 0.29 0.53 0.91 1.0,   -- Blue
  Color.rgba 0.95 0.61 0.07 1.0,   -- Orange
  Color.rgba 0.20 0.69 0.35 1.0,   -- Green
  Color.rgba 0.84 0.24 0.29 1.0,   -- Red
  Color.rgba 0.58 0.40 0.74 1.0,   -- Purple
  Color.rgba 0.55 0.34 0.29 1.0,   -- Brown
  Color.rgba 0.89 0.47 0.76 1.0,   -- Pink
  Color.rgba 0.50 0.50 0.50 1.0    -- Gray
]

/-- Get color for a series index. -/
def getSeriesColor (series : Series) (idx : Nat) : Color :=
  series.color.getD (defaultColors[idx % defaultColors.size]!)

/-- Format a float value for axis labels. -/
private def formatValue (v : Float) : String :=
  if v >= 1000000 then
    s!"{(v / 1000000).floor.toUInt32}M"
  else if v >= 1000 then
    s!"{(v / 1000).floor.toUInt32}K"
  else if v == v.floor then
    s!"{v.floor.toUInt32}"
  else
    let whole := v.floor.toInt32
    let frac := ((v - v.floor) * 10).floor.toUInt32
    s!"{whole}.{frac}"

/-- Calculate cumulative sums for stacking at each data point. -/
private def calculateStackedValues (data : Data) : Array (Array Float) := Id.run do
  let numSeries := data.series.size
  if numSeries == 0 then return #[]

  -- Find max number of points across all series
  let maxPoints := data.series.foldl (fun acc s => max acc s.values.size) 0
  if maxPoints == 0 then return #[]

  -- Build cumulative sums: result[seriesIdx][pointIdx] = cumulative value at that point
  let mut result : Array (Array Float) := #[]

  for seriesIdx in [0:numSeries] do
    let series := data.series[seriesIdx]!
    let mut cumulativeValues : Array Float := #[]

    for pointIdx in [0:maxPoints] do
      let thisValue := if pointIdx < series.values.size then series.values[pointIdx]! else 0.0
      let prevCumulative := if seriesIdx > 0 then
        let prevSeries := result[seriesIdx - 1]!
        if pointIdx < prevSeries.size then prevSeries[pointIdx]! else 0.0
      else 0.0
      cumulativeValues := cumulativeValues.push (prevCumulative + thisValue)

    result := result.push cumulativeValues

  result

/-- Find maximum stacked value (top of the stack). -/
private def findMaxStackedValue (stackedValues : Array (Array Float)) : Float :=
  if stackedValues.isEmpty then 1.0
  else
    let topSeries := stackedValues[stackedValues.size - 1]!
    let maxVal := topSeries.foldl (fun acc v => max acc v) 0.0
    if maxVal <= 0.0 then 1.0 else maxVal

/-- Calculate nice max value for axis scaling. -/
private def niceMax (maxVal : Float) : Float :=
  if maxVal <= 0.0 then 1.0
  else if maxVal <= 10 then 10.0
  else if maxVal <= 50 then 50.0
  else if maxVal <= 100 then 100.0
  else if maxVal <= 500 then 500.0
  else if maxVal <= 1000 then 1000.0
  else (maxVal / 100).ceil * 100

/-- Custom spec for stacked area chart rendering. -/
def stackedAreaChartSpec (data : Data) (theme : Theme)
    (dims : Dimensions := defaultDimensions) : CustomSpec := {
  measure := fun _ _ => (dims.marginLeft + dims.marginRight + 50, dims.marginTop + dims.marginBottom + 30)
  collect := fun layout =>
    let rect := layout.contentRect
    let actualWidth := rect.width
    let actualHeight := rect.height

    let numSeries := data.series.size
    if numSeries == 0 then #[] else

    -- Calculate stacked values
    let stackedValues := calculateStackedValues data
    if stackedValues.isEmpty then #[] else

    let maxPoints := stackedValues[0]!.size
    if maxPoints == 0 then #[] else

    -- Calculate chart area
    let legendSpace := if dims.showLegend then dims.marginRight else 20.0
    let chartX := rect.x + dims.marginLeft
    let chartY := rect.y + dims.marginTop
    let chartWidth := actualWidth - dims.marginLeft - legendSpace
    let chartHeight := actualHeight - dims.marginTop - dims.marginBottom

    -- Find max value and calculate nice max
    let maxVal := findMaxStackedValue stackedValues
    let niceMaxVal := niceMax maxVal

    -- Calculate step between data points
    let stepX := if maxPoints > 1 then chartWidth / (maxPoints - 1).toFloat else 0.0

    -- Helper to convert value to Y coordinate
    let valueToY := fun (v : Float) => chartY + chartHeight - (v / niceMaxVal) * chartHeight

    RenderM.build do
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

      -- Draw stacked areas (from bottom to top)
      for seriesIdx in [0:numSeries] do
        let series := data.series[seriesIdx]!
        let color := getSeriesColor series seriesIdx
        let topValues := stackedValues[seriesIdx]!

        -- Get bottom values (previous series cumulative, or baseline)
        let bottomValues := if seriesIdx > 0 then stackedValues[seriesIdx - 1]! else
          Array.replicate maxPoints 0.0

        -- Build area path: trace top edge forward, then bottom edge backward
        let mut areaPath := Arbor.Path.empty

        -- Start at first point's bottom
        let startX := chartX
        let startBottomY := valueToY bottomValues[0]!
        areaPath := areaPath.moveTo (Arbor.Point.mk' startX startBottomY)

        -- Trace up to first point's top
        let startTopY := valueToY topValues[0]!
        areaPath := areaPath.lineTo (Arbor.Point.mk' startX startTopY)

        -- Trace top edge forward
        for i in [1:maxPoints] do
          let x := chartX + i.toFloat * stepX
          let y := valueToY topValues[i]!
          areaPath := areaPath.lineTo (Arbor.Point.mk' x y)

        -- Trace bottom edge backward
        for i in [0:maxPoints] do
          let revIdx := maxPoints - 1 - i
          let x := chartX + revIdx.toFloat * stepX
          let y := valueToY bottomValues[revIdx]!
          areaPath := areaPath.lineTo (Arbor.Point.mk' x y)

        -- Close path
        areaPath := areaPath.lineTo (Arbor.Point.mk' startX startBottomY)

        RenderM.fillPath areaPath (color.withAlpha dims.fillOpacity)

      -- Draw lines on top of areas
      if dims.showLines then
        for seriesIdx in [0:numSeries] do
          let series := data.series[seriesIdx]!
          let color := getSeriesColor series seriesIdx
          let topValues := stackedValues[seriesIdx]!

          let mut linePath := Arbor.Path.empty
          for i in [0:maxPoints] do
            let x := chartX + i.toFloat * stepX
            let y := valueToY topValues[i]!
            let pt := Arbor.Point.mk' x y
            if i == 0 then
              linePath := linePath.moveTo pt
            else
              linePath := linePath.lineTo pt

          RenderM.strokePath linePath color dims.lineWidth

      -- Draw Y-axis labels
      if dims.gridLineCount > 0 then
        for i in [0:dims.gridLineCount + 1] do
          let ratio := i.toFloat / dims.gridLineCount.toFloat
          let value := ratio * niceMaxVal
          let labelY := chartY + chartHeight - (ratio * chartHeight) + 4
          let labelText := formatValue value
          RenderM.fillText labelText (rect.x + 4) labelY theme.smallFont theme.textMuted

      -- Draw X-axis labels
      if data.labels.size > 0 then
        for i in [0:min data.labels.size maxPoints] do
          let label := data.labels[i]!
          let labelX := chartX + i.toFloat * stepX
          let labelY := chartY + chartHeight + 16
          RenderM.fillText label labelX labelY theme.smallFont theme.text

      -- Draw axes
      let axisColor := Color.gray 0.5
      let yAxisRect := Arbor.Rect.mk' chartX chartY 1.0 chartHeight
      RenderM.fillRect yAxisRect axisColor 0.0
      let xAxisRect := Arbor.Rect.mk' chartX (chartY + chartHeight) chartWidth 1.0
      RenderM.fillRect xAxisRect axisColor 0.0

      -- Draw legend
      if dims.showLegend && numSeries > 0 then
        let legendX := chartX + chartWidth + 16
        let legendY := chartY
        for i in [0:numSeries] do
          let series := data.series[i]!
          let color := getSeriesColor series i
          let itemY := legendY + i.toFloat * (dims.legendItemHeight + 4)
          -- Color box
          let colorRect := Arbor.Rect.mk' legendX itemY 12.0 12.0
          RenderM.fillRect colorRect color 2.0
          -- Label
          RenderM.fillText series.name (legendX + 16) (itemY + 10) theme.smallFont theme.text

  draw := none
}

end StackedAreaChart

/-- Build a stacked area chart visual (WidgetBuilder version).
    - `name`: Widget name for identification
    - `data`: Stacked area chart data with labels and series
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def stackedAreaChartVisual (name : String) (data : StackedAreaChart.Data)
    (theme : Theme) (dims : StackedAreaChart.Dimensions := StackedAreaChart.defaultDimensions)
    : WidgetBuilder := do
  let wid ← freshId
  let chart ← custom (StackedAreaChart.stackedAreaChartSpec data theme dims) {
    width := .percent 1.0
    height := .percent 1.0
    flexItem := some (Trellis.FlexItem.growing 1)
  }
  let style : BoxStyle := { width := .percent 1.0, height := .percent 1.0, flexItem := some (Trellis.FlexItem.growing 1) }
  let props : Trellis.FlexContainer := { Trellis.FlexContainer.column 0 with alignItems := .stretch }
  pure (.flex wid (some name) props style #[chart])

/-! ## Reactive StackedAreaChart Components (FRP-based)

These use WidgetM for declarative composition.
-/

open Reactive Reactive.Host
open Afferent.Canopy.Reactive

/-- StackedAreaChart result - provides access to chart state. -/
structure StackedAreaChartResult where
  /-- The data being displayed. -/
  data : Dyn StackedAreaChart.Data

/-- Create a stacked area chart component using WidgetM with dynamic data.
    The chart automatically rebuilds when the data Dynamic changes.
    - `data`: Dynamic stacked area chart data with labels and series
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def stackedAreaChart (data : Dyn StackedAreaChart.Data)
    (theme : Theme) (dims : StackedAreaChart.Dimensions := StackedAreaChart.defaultDimensions)
    : WidgetM StackedAreaChartResult := do
  -- Use dynWidget to rebuild the chart when data changes
  let _ ← dynWidget data fun currentData => do
    let name ← registerComponentW "stacked-area-chart" (isInteractive := false)
    emit do
      pure (stackedAreaChartVisual name currentData theme dims)

  pure { data }

/-- Create a stacked area chart with static data.
    Convenience function that wraps static data in a constant Dynamic.
    - `data`: Static stacked area chart data with labels and series
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def stackedAreaChart' (data : StackedAreaChart.Data)
    (theme : Theme) (dims : StackedAreaChart.Dimensions := StackedAreaChart.defaultDimensions)
    : WidgetM StackedAreaChartResult := do
  let dataDyn ← Dynamic.pureM data
  stackedAreaChart dataDyn theme dims

/-- Create a stacked area chart from simple arrays (static data).
    - `labels`: X-axis labels
    - `seriesNames`: Names for each series (for legend)
    - `seriesData`: Array of value arrays, one per series
    - `colors`: Optional colors for each series
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def stackedAreaChartFromArrays (labels : Array String)
    (seriesNames : Array String) (seriesData : Array (Array Float))
    (colors : Array Color := #[])
    (theme : Theme) (dims : StackedAreaChart.Dimensions := StackedAreaChart.defaultDimensions)
    : WidgetM StackedAreaChartResult := do
  let series := Id.run do
    let mut result : Array StackedAreaChart.Series := #[]
    for i in [0:seriesNames.size] do
      let name := seriesNames[i]!
      let values := if i < seriesData.size then seriesData[i]! else #[]
      let color := if i < colors.size then some colors[i]! else none
      result := result.push { name, values, color }
    result
  let data : StackedAreaChart.Data := { labels, series }
  stackedAreaChart' data theme dims

end Afferent.Canopy

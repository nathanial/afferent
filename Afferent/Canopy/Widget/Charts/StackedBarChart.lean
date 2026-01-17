/-
  Canopy StackedBarChart Widget
  Stacked vertical bar chart for comparing composition across categories.
-/
import Reactive
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Reactive.Component
import Afferent.Canopy.Widget.Charts.Core

namespace Afferent.Canopy

open Afferent.Arbor hiding Event

namespace StackedBarChart

/-- Dimensions and spacing for stacked bar chart rendering. -/
structure Dimensions extends AxisChartDimensions where
  height := 280.0
  marginRight := 100.0  -- Extra space for legend
  barGap : Float := 12.0
  cornerRadius : Float := 0.0  -- No rounding for stacked bars (cleaner joins)
  showLegend : Bool := true
  legendItemHeight : Float := 16.0
deriving Repr, Inhabited

/-- Default stacked bar chart dimensions. -/
def defaultDimensions : Dimensions := {}

/-- A single data series for stacking. -/
structure Series where
  /-- Name of the series (shown in legend). -/
  name : String
  /-- Values for each category. -/
  values : Array Float
  /-- Color for this series. -/
  color : Option Color := none
deriving Repr, Inhabited, BEq

/-- Stacked bar chart data. -/
structure Data where
  /-- Category labels (x-axis). -/
  categories : Array String
  /-- Data series to stack. -/
  series : Array Series
deriving Repr, Inhabited, BEq

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

/-- Calculate stacked totals for each category. -/
private def calculateStackedTotals (data : Data) : Array Float :=
  let numCategories := data.categories.size
  Id.run do
    let mut totals : Array Float := Array.replicate numCategories 0.0
    for series in data.series do
      for i in [0:min numCategories series.values.size] do
        totals := totals.set! i (totals[i]! + series.values[i]!)
    totals

/-- Custom spec for stacked bar chart rendering. -/
def stackedBarChartSpec (data : Data) (theme : Theme)
    (dims : Dimensions := defaultDimensions) : CustomSpec := {
  measure := fun _ _ => (dims.marginLeft + dims.marginRight + 50, dims.marginTop + dims.marginBottom + 30)
  collect := fun layout =>
    let rect := layout.contentRect
    let actualWidth := rect.width
    let actualHeight := rect.height

    let numCategories := data.categories.size
    if numCategories == 0 || data.series.isEmpty then #[] else

    -- Calculate chart area
    let legendSpace := if dims.showLegend then dims.marginRight else 20.0
    let chartX := rect.x + dims.marginLeft
    let chartY := rect.y + dims.marginTop
    let chartWidth := actualWidth - dims.marginLeft - legendSpace
    let chartHeight := actualHeight - dims.marginTop - dims.marginBottom

    -- Find max stacked total for scaling
    let stackedTotals := calculateStackedTotals data
    let maxTotal := stackedTotals.foldl (fun acc v => max acc v) 0.0
    let maxTotal := if maxTotal <= 0.0 then 1.0 else maxTotal

    -- Round up to nice number for axis
    let niceMax := if maxTotal <= 10 then 10.0
                   else if maxTotal <= 50 then 50.0
                   else if maxTotal <= 100 then 100.0
                   else if maxTotal <= 500 then 500.0
                   else if maxTotal <= 1000 then 1000.0
                   else (maxTotal / 100).ceil * 100

    -- Calculate bar width
    let totalGapWidth := if numCategories > 1 then dims.barGap * (numCategories - 1).toFloat else 0.0
    let barWidth := (chartWidth - totalGapWidth) / numCategories.toFloat

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

      -- Draw stacked bars
      for catIdx in [0:numCategories] do
        let barX := chartX + catIdx.toFloat * (barWidth + dims.barGap)
        let mut currentY := chartY + chartHeight  -- Start from bottom

        -- Draw each series segment from bottom to top
        for seriesIdx in [0:data.series.size] do
          let series := data.series[seriesIdx]!
          let value := if catIdx < series.values.size then series.values[catIdx]! else 0.0
          if value > 0.0 then
            let segmentHeight := (value / niceMax) * chartHeight
            currentY := currentY - segmentHeight
            let segmentRect := Arbor.Rect.mk' barX currentY barWidth segmentHeight
            let color := getSeriesColor series seriesIdx
            RenderM.fillRect segmentRect color dims.cornerRadius

      -- Draw Y-axis labels
      if dims.gridLineCount > 0 then
        for i in [0:dims.gridLineCount + 1] do
          let ratio := i.toFloat / dims.gridLineCount.toFloat
          let value := ratio * niceMax
          let labelY := chartY + chartHeight - (ratio * chartHeight) + 4
          let labelText := formatValue value
          RenderM.fillText labelText (rect.x + 4) labelY theme.smallFont theme.textMuted

      -- Draw X-axis labels
      for i in [0:numCategories] do
        let label := data.categories[i]!
        let labelX := chartX + i.toFloat * (barWidth + dims.barGap) + barWidth / 2
        let labelY := chartY + chartHeight + 16
        RenderM.fillText label labelX labelY theme.smallFont theme.text

      -- Draw axes
      let axisColor := Color.gray 0.5
      let yAxisRect := Arbor.Rect.mk' chartX chartY 1.0 chartHeight
      RenderM.fillRect yAxisRect axisColor 0.0
      let xAxisRect := Arbor.Rect.mk' chartX (chartY + chartHeight) chartWidth 1.0
      RenderM.fillRect xAxisRect axisColor 0.0

      -- Draw legend
      if dims.showLegend && data.series.size > 0 then
        let legendX := chartX + chartWidth + 16
        let legendY := chartY
        for i in [0:data.series.size] do
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

end StackedBarChart

/-- Build a stacked bar chart visual (WidgetBuilder version).
    - `name`: Widget name for identification
    - `data`: Stacked bar chart data with categories and series
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def stackedBarChartVisual (name : String) (data : StackedBarChart.Data)
    (theme : Theme) (dims : StackedBarChart.Dimensions := StackedBarChart.defaultDimensions)
    : WidgetBuilder := do
  let wid ← freshId
  let chart ← custom (StackedBarChart.stackedBarChartSpec data theme dims) {
    width := .percent 1.0
    height := .percent 1.0
    flexItem := some (Trellis.FlexItem.growing 1)
  }
  let style : BoxStyle := { width := .percent 1.0, height := .percent 1.0, minWidth := some dims.width, minHeight := some dims.height, flexItem := some (Trellis.FlexItem.growing 1) }
  let props : Trellis.FlexContainer := { Trellis.FlexContainer.column 0 with alignItems := .stretch }
  pure (.flex wid (some name) props style #[chart])

/-! ## Reactive StackedBarChart Components (FRP-based)

These use WidgetM for declarative composition.
-/

open Reactive Reactive.Host
open Afferent.Canopy.Reactive

/-- StackedBarChart result - provides access to chart state. -/
structure StackedBarChartResult where
  /-- The data being displayed. -/
  data : Dyn StackedBarChart.Data

/-- Create a stacked bar chart component using WidgetM with dynamic data.
    The chart automatically rebuilds when the data Dynamic changes.
    - `data`: Dynamic stacked bar chart data with categories and series
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def stackedBarChart (data : Dyn StackedBarChart.Data)
    (theme : Theme) (dims : StackedBarChart.Dimensions := StackedBarChart.defaultDimensions)
    : WidgetM StackedBarChartResult := do
  let _ ← dynWidget data fun currentData => do
    let name ← registerComponentW "stacked-bar-chart" (isInteractive := false)
    emit do pure (stackedBarChartVisual name currentData theme dims)

  pure { data }

/-- Create a stacked bar chart from dynamic arrays.
    - `categories`: Category labels for x-axis
    - `seriesNames`: Names for each series (for legend)
    - `seriesData`: Dynamic array of value arrays, one per series
    - `colors`: Optional colors for each series
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def stackedBarChartFromArrays (categories : Array String)
    (seriesNames : Array String) (seriesData : Dyn (Array (Array Float)))
    (colors : Array Color := #[])
    (theme : Theme) (dims : StackedBarChart.Dimensions := StackedBarChart.defaultDimensions)
    : WidgetM StackedBarChartResult := do
  let dataDyn ← Dynamic.mapM (fun currentSeriesData => Id.run do
    let mut result : Array StackedBarChart.Series := #[]
    for i in [0:seriesNames.size] do
      let name := seriesNames[i]!
      let values := if i < currentSeriesData.size then currentSeriesData[i]! else #[]
      let color := if i < colors.size then some colors[i]! else none
      result := result.push { name, values, color }
    ({ categories, series := result } : StackedBarChart.Data)
  ) seriesData
  stackedBarChart dataDyn theme dims

end Afferent.Canopy

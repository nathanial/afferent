/-
  Canopy StackedBarChart Widget
  Stacked vertical bar chart for comparing composition across categories.
-/
import Reactive
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Reactive.Component

namespace Afferent.Canopy

open Afferent.Arbor hiding Event

namespace StackedBarChart

/-- Dimensions and spacing for stacked bar chart rendering. -/
structure Dimensions where
  width : Float := 400.0
  height : Float := 280.0
  marginTop : Float := 20.0
  marginBottom : Float := 40.0
  marginLeft : Float := 50.0
  marginRight : Float := 100.0  -- Extra space for legend
  barGap : Float := 12.0
  cornerRadius : Float := 0.0  -- No rounding for stacked bars (cleaner joins)
  showGridLines : Bool := true
  gridLineCount : Nat := 5
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
deriving Repr, Inhabited

/-- Stacked bar chart data. -/
structure Data where
  /-- Category labels (x-axis). -/
  categories : Array String
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
  measure := fun _ _ => (dims.width, dims.height)
  collect := fun layout =>
    let rect := layout.contentRect
    let cmds : Array RenderCommand := #[]

    let numCategories := data.categories.size
    if numCategories == 0 || data.series.isEmpty then cmds else

    -- Calculate chart area
    let legendSpace := if dims.showLegend then dims.marginRight else 20.0
    let chartX := rect.x + dims.marginLeft
    let chartY := rect.y + dims.marginTop
    let chartWidth := dims.width - dims.marginLeft - legendSpace
    let chartHeight := dims.height - dims.marginTop - dims.marginBottom

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

    -- Draw background
    let bgRect := Arbor.Rect.mk' rect.x rect.y dims.width dims.height
    let cmds := cmds.push (.fillRect bgRect (theme.panel.background.withAlpha 0.3) 6.0)

    -- Draw grid lines
    let cmds := if dims.showGridLines && dims.gridLineCount > 0 then
      Id.run do
        let mut cmds := cmds
        for i in [0:dims.gridLineCount + 1] do
          let ratio := i.toFloat / dims.gridLineCount.toFloat
          let lineY := chartY + chartHeight - (ratio * chartHeight)
          let lineRect := Arbor.Rect.mk' chartX lineY chartWidth 1.0
          cmds := cmds.push (.fillRect lineRect (Color.gray 0.3) 0.0)
        cmds
    else cmds

    -- Draw stacked bars
    let cmds := Id.run do
      let mut cmds := cmds
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
            cmds := cmds.push (.fillRect segmentRect color dims.cornerRadius)
      cmds

    -- Draw Y-axis labels
    let cmds := if dims.gridLineCount > 0 then
      Id.run do
        let mut cmds := cmds
        for i in [0:dims.gridLineCount + 1] do
          let ratio := i.toFloat / dims.gridLineCount.toFloat
          let value := ratio * niceMax
          let labelY := chartY + chartHeight - (ratio * chartHeight) + 4
          let labelText := formatValue value
          cmds := cmds.push (.fillText labelText (rect.x + 4) labelY theme.smallFont theme.textMuted)
        cmds
    else cmds

    -- Draw X-axis labels
    let cmds := Id.run do
      let mut cmds := cmds
      for i in [0:numCategories] do
        let label := data.categories[i]!
        let labelX := chartX + i.toFloat * (barWidth + dims.barGap) + barWidth / 2
        let labelY := chartY + chartHeight + 16
        cmds := cmds.push (.fillText label labelX labelY theme.smallFont theme.text)
      cmds

    -- Draw axes
    let axisColor := Color.gray 0.5
    let yAxisRect := Arbor.Rect.mk' chartX chartY 1.0 chartHeight
    let cmds := cmds.push (.fillRect yAxisRect axisColor 0.0)
    let xAxisRect := Arbor.Rect.mk' chartX (chartY + chartHeight) chartWidth 1.0
    let cmds := cmds.push (.fillRect xAxisRect axisColor 0.0)

    -- Draw legend
    let cmds := if dims.showLegend && data.series.size > 0 then
      Id.run do
        let mut cmds := cmds
        let legendX := chartX + chartWidth + 16
        let legendY := chartY
        for i in [0:data.series.size] do
          let series := data.series[i]!
          let color := getSeriesColor series i
          let itemY := legendY + i.toFloat * (dims.legendItemHeight + 4)
          -- Color box
          let colorRect := Arbor.Rect.mk' legendX itemY 12.0 12.0
          cmds := cmds.push (.fillRect colorRect color 2.0)
          -- Label
          cmds := cmds.push (.fillText series.name (legendX + 16) (itemY + 10) theme.smallFont theme.text)
        cmds
    else cmds

    cmds

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
    minWidth := some dims.width
    minHeight := some dims.height
  }
  let props : Trellis.FlexContainer := { Trellis.FlexContainer.column 0 with alignItems := .flexStart }
  pure (.flex wid (some name) props {} #[chart])

/-! ## Reactive StackedBarChart Components (FRP-based)

These use WidgetM for declarative composition.
-/

open Reactive Reactive.Host
open Afferent.Canopy.Reactive

/-- StackedBarChart result - provides access to chart state. -/
structure StackedBarChartResult where
  /-- The data being displayed. -/
  data : Reactive.Dynamic Spider StackedBarChart.Data

/-- Create a stacked bar chart component using WidgetM.
    - `data`: Stacked bar chart data with categories and series
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def stackedBarChart (data : StackedBarChart.Data)
    (theme : Theme) (dims : StackedBarChart.Dimensions := StackedBarChart.defaultDimensions)
    : WidgetM StackedBarChartResult := do
  let name ← registerComponentW "stacked-bar-chart" (isInteractive := false)

  let dataDyn ← Dynamic.pureM data

  emit do
    pure (stackedBarChartVisual name data theme dims)

  pure { data := dataDyn }

/-- Create a stacked bar chart from simple arrays.
    - `categories`: Category labels for x-axis
    - `seriesNames`: Names for each series (for legend)
    - `seriesData`: Array of value arrays, one per series
    - `colors`: Optional colors for each series
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def stackedBarChartFromArrays (categories : Array String)
    (seriesNames : Array String) (seriesData : Array (Array Float))
    (colors : Array Color := #[])
    (theme : Theme) (dims : StackedBarChart.Dimensions := StackedBarChart.defaultDimensions)
    : WidgetM StackedBarChartResult := do
  let series := Id.run do
    let mut result : Array StackedBarChart.Series := #[]
    for i in [0:seriesNames.size] do
      let name := seriesNames[i]!
      let values := if i < seriesData.size then seriesData[i]! else #[]
      let color := if i < colors.size then some colors[i]! else none
      result := result.push { name, values, color }
    result
  let data : StackedBarChart.Data := { categories, series }
  stackedBarChart data theme dims

/-- Create a stacked bar chart that updates based on an external event stream.
    - `initialData`: Initial chart data
    - `dataUpdates`: Event stream of data updates
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def stackedBarChartWithEvents (initialData : StackedBarChart.Data)
    (dataUpdates : Reactive.Event Spider StackedBarChart.Data)
    (theme : Theme) (dims : StackedBarChart.Dimensions := StackedBarChart.defaultDimensions)
    : WidgetM StackedBarChartResult := do
  let name ← registerComponentW "stacked-bar-chart" (isInteractive := false)

  let dataDyn ← Reactive.holdDyn initialData dataUpdates

  emit do
    let d ← dataDyn.sample
    pure (stackedBarChartVisual name d theme dims)

  pure { data := dataDyn }

end Afferent.Canopy

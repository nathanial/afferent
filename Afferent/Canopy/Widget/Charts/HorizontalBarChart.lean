/-
  Canopy HorizontalBarChart Widget
  Horizontal bar chart for comparing categorical data with bars extending left-to-right.
-/
import Reactive
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Reactive.Component
import Afferent.Canopy.Widget.Charts.Core

namespace Afferent.Canopy

open Afferent.Arbor hiding Event

/-- Horizontal bar chart color variant. -/
inductive HorizontalBarChartVariant where
  | primary
  | secondary
  | success
  | warning
  | error
deriving Repr, BEq, Inhabited

namespace HorizontalBarChart

/-- Dimensions and spacing for horizontal bar chart rendering. -/
structure Dimensions extends AxisChartDimensions where
  marginBottom := 30.0
  marginLeft := 80.0
  barGap : Float := 8.0
  cornerRadius : Float := 4.0
deriving Repr, Inhabited

/-- Default horizontal bar chart dimensions. -/
def defaultDimensions : Dimensions := {}

/-- Configuration for bar chart data. -/
structure DataPoint where
  value : Float
  label : Option String := none
  color : Option Color := none
deriving Repr, Inhabited

/-- Get the fill color for a variant. -/
def variantColor (variant : HorizontalBarChartVariant) (theme : Theme) : Color :=
  match variant with
  | .primary => theme.primary.background
  | .secondary => theme.secondary.background
  | .success => Color.rgba 0.2 0.8 0.3 1.0
  | .warning => Color.rgba 1.0 0.7 0.0 1.0
  | .error => Color.rgba 0.9 0.2 0.2 1.0

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

/-- Calculate nice max value for axis scaling. -/
private def niceMax (maxVal : Float) : Float :=
  if maxVal <= 0.0 then 1.0
  else if maxVal <= 10 then 10.0
  else if maxVal <= 50 then 50.0
  else if maxVal <= 100 then 100.0
  else if maxVal <= 500 then 500.0
  else if maxVal <= 1000 then 1000.0
  else (maxVal / 100).ceil * 100

/-- Custom spec for horizontal bar chart rendering. -/
def horizontalBarChartSpec (data : Array Float) (labels : Array String)
    (variant : HorizontalBarChartVariant) (theme : Theme)
    (dims : Dimensions := defaultDimensions) : CustomSpec := {
  measure := fun _ _ => (dims.marginLeft + dims.marginRight + 50, dims.marginTop + dims.marginBottom + 30)
  collect := fun layout =>
    let rect := layout.contentRect
    let actualWidth := rect.width
    let actualHeight := rect.height

    -- Calculate chart area (inside margins)
    let chartX := rect.x + dims.marginLeft
    let chartY := rect.y + dims.marginTop
    let chartWidth := actualWidth - dims.marginLeft - dims.marginRight
    let chartHeight := actualHeight - dims.marginTop - dims.marginBottom

    -- Find max value for scaling
    let maxVal := data.foldl (fun acc v => if v > acc then v else acc) 0.0
    let niceMaxVal := niceMax maxVal

    -- Calculate bar height based on data count
    let barCount := data.size
    let totalGapHeight := if barCount > 1 then dims.barGap * (barCount - 1).toFloat else 0.0
    let barHeight := if barCount > 0 then (chartHeight - totalGapHeight) / barCount.toFloat else 0.0

    -- Fill color for bars
    let fillColor := variantColor variant theme

    RenderM.build do
      -- Draw background
      let bgRect := Arbor.Rect.mk' rect.x rect.y actualWidth actualHeight
      RenderM.fillRect bgRect (theme.panel.background.withAlpha 0.3) 6.0

      -- Draw vertical grid lines
      if dims.showGridLines && dims.gridLineCount > 0 then
        for i in [0:dims.gridLineCount + 1] do
          let ratio := i.toFloat / dims.gridLineCount.toFloat
          let lineX := chartX + (ratio * chartWidth)
          let lineRect := Arbor.Rect.mk' lineX chartY 1.0 chartHeight
          RenderM.fillRect lineRect (Color.gray 0.3) 0.0

      -- Draw bars (horizontal)
      for i in [0:barCount] do
        let value := data[i]!
        let barWidth := (value / niceMaxVal) * chartWidth
        let barY := chartY + i.toFloat * (barHeight + dims.barGap)
        let barRect := Arbor.Rect.mk' chartX barY barWidth barHeight
        RenderM.fillRect barRect fillColor dims.cornerRadius

      -- Draw X-axis labels (values at bottom)
      if dims.gridLineCount > 0 then
        for i in [0:dims.gridLineCount + 1] do
          let ratio := i.toFloat / dims.gridLineCount.toFloat
          let value := ratio * niceMaxVal
          let labelX := chartX + (ratio * chartWidth)
          let labelY := chartY + chartHeight + 16
          let labelText := formatValue value
          RenderM.fillText labelText labelX labelY theme.smallFont theme.textMuted

      -- Draw Y-axis labels (category names on left)
      if labels.size > 0 then
        for i in [0:min labels.size barCount] do
          let label := labels[i]!
          let labelX := rect.x + 4
          let labelY := chartY + i.toFloat * (barHeight + dims.barGap) + barHeight / 2 + 4
          RenderM.fillText label labelX labelY theme.smallFont theme.text

      -- Draw axes
      let axisColor := Color.gray 0.5
      -- Y-axis (left edge of chart area)
      let yAxisRect := Arbor.Rect.mk' chartX chartY 1.0 chartHeight
      RenderM.fillRect yAxisRect axisColor 0.0
      -- X-axis (bottom)
      let xAxisRect := Arbor.Rect.mk' chartX (chartY + chartHeight) chartWidth 1.0
      RenderM.fillRect xAxisRect axisColor 0.0

  draw := none
}

/-- Custom spec for horizontal bar chart with individually colored bars. -/
def multiColorHorizontalBarChartSpec (data : Array DataPoint)
    (theme : Theme) (dims : Dimensions := defaultDimensions) : CustomSpec := {
  measure := fun _ _ => (dims.marginLeft + dims.marginRight + 50, dims.marginTop + dims.marginBottom + 30)
  collect := fun layout =>
    let rect := layout.contentRect
    let actualWidth := rect.width
    let actualHeight := rect.height

    let chartX := rect.x + dims.marginLeft
    let chartY := rect.y + dims.marginTop
    let chartWidth := actualWidth - dims.marginLeft - dims.marginRight
    let chartHeight := actualHeight - dims.marginTop - dims.marginBottom

    -- Find max value
    let maxVal := data.foldl (fun acc dp => if dp.value > acc then dp.value else acc) 0.0
    let niceMaxVal := niceMax maxVal

    let barCount := data.size
    let totalGapHeight := if barCount > 1 then dims.barGap * (barCount - 1).toFloat else 0.0
    let barHeight := if barCount > 0 then (chartHeight - totalGapHeight) / barCount.toFloat else 0.0

    -- Default colors
    let defaultColors := #[
      theme.primary.background,
      theme.secondary.background,
      Color.rgba 0.2 0.8 0.3 1.0,
      Color.rgba 1.0 0.7 0.0 1.0,
      Color.rgba 0.9 0.2 0.2 1.0,
      Color.rgba 0.5 0.3 0.9 1.0,
      Color.rgba 0.0 0.7 0.7 1.0
    ]

    RenderM.build do
      -- Background
      let bgRect := Arbor.Rect.mk' rect.x rect.y actualWidth actualHeight
      RenderM.fillRect bgRect (theme.panel.background.withAlpha 0.3) 6.0

      -- Grid lines
      if dims.showGridLines && dims.gridLineCount > 0 then
        for i in [0:dims.gridLineCount + 1] do
          let ratio := i.toFloat / dims.gridLineCount.toFloat
          let lineX := chartX + (ratio * chartWidth)
          let lineRect := Arbor.Rect.mk' lineX chartY 1.0 chartHeight
          RenderM.fillRect lineRect (Color.gray 0.3) 0.0

      -- Draw bars with individual colors
      for i in [0:barCount] do
        let dp := data[i]!
        let barWidth := (dp.value / niceMaxVal) * chartWidth
        let barY := chartY + i.toFloat * (barHeight + dims.barGap)
        let barRect := Arbor.Rect.mk' chartX barY barWidth barHeight
        let color := dp.color.getD (defaultColors[i % defaultColors.size]!)
        RenderM.fillRect barRect color dims.cornerRadius

      -- X-axis labels
      if dims.gridLineCount > 0 then
        for i in [0:dims.gridLineCount + 1] do
          let ratio := i.toFloat / dims.gridLineCount.toFloat
          let value := ratio * niceMaxVal
          let labelX := chartX + (ratio * chartWidth)
          let labelY := chartY + chartHeight + 16
          let labelText := formatValue value
          RenderM.fillText labelText labelX labelY theme.smallFont theme.textMuted

      -- Y-axis labels (from DataPoint labels)
      for i in [0:barCount] do
        let dp := data[i]!
        match dp.label with
        | some label =>
          let labelX := rect.x + 4
          let labelY := chartY + i.toFloat * (barHeight + dims.barGap) + barHeight / 2 + 4
          RenderM.fillText label labelX labelY theme.smallFont theme.text
        | none => pure ()

      -- Axes
      let axisColor := Color.gray 0.5
      let yAxisRect := Arbor.Rect.mk' chartX chartY 1.0 chartHeight
      RenderM.fillRect yAxisRect axisColor 0.0
      let xAxisRect := Arbor.Rect.mk' chartX (chartY + chartHeight) chartWidth 1.0
      RenderM.fillRect xAxisRect axisColor 0.0

  draw := none
}

end HorizontalBarChart

/-- Build a horizontal bar chart visual (WidgetBuilder version).
    - `name`: Widget name for identification
    - `data`: Array of values to display
    - `labels`: Optional labels for each bar
    - `variant`: Color variant for bars
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def horizontalBarChartVisual (name : String) (data : Array Float)
    (labels : Array String := #[])
    (variant : HorizontalBarChartVariant := .primary) (theme : Theme)
    (dims : HorizontalBarChart.Dimensions := HorizontalBarChart.defaultDimensions) : WidgetBuilder := do
  let wid ← freshId
  let chart ← custom (HorizontalBarChart.horizontalBarChartSpec data labels variant theme dims) {
    width := .percent 1.0
    height := .percent 1.0
    flexItem := some (Trellis.FlexItem.growing 1)
  }
  let style : BoxStyle := { width := .percent 1.0, height := .percent 1.0, minWidth := some dims.width, minHeight := some dims.height, flexItem := some (Trellis.FlexItem.growing 1) }
  let props : Trellis.FlexContainer := { Trellis.FlexContainer.column 0 with alignItems := .stretch }
  pure (.flex wid (some name) props style #[chart])

/-- Build a multi-color horizontal bar chart visual (WidgetBuilder version).
    Each data point can have its own color.
    - `name`: Widget name for identification
    - `data`: Array of data points with values, labels, and optional colors
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def multiColorHorizontalBarChartVisual (name : String) (data : Array HorizontalBarChart.DataPoint)
    (theme : Theme) (dims : HorizontalBarChart.Dimensions := HorizontalBarChart.defaultDimensions) : WidgetBuilder := do
  let wid ← freshId
  let chart ← custom (HorizontalBarChart.multiColorHorizontalBarChartSpec data theme dims) {
    width := .percent 1.0
    height := .percent 1.0
    flexItem := some (Trellis.FlexItem.growing 1)
  }
  let style : BoxStyle := { width := .percent 1.0, height := .percent 1.0, minWidth := some dims.width, minHeight := some dims.height, flexItem := some (Trellis.FlexItem.growing 1) }
  let props : Trellis.FlexContainer := { Trellis.FlexContainer.column 0 with alignItems := .stretch }
  pure (.flex wid (some name) props style #[chart])

/-! ## Reactive HorizontalBarChart Components (FRP-based)

These use WidgetM for declarative composition.
-/

open Reactive Reactive.Host
open Afferent.Canopy.Reactive

/-- HorizontalBarChart result - provides access to chart state. -/
structure HorizontalBarChartResult where
  /-- The data being displayed. -/
  data : Reactive.Dynamic Spider (Array Float)

/-- Create a horizontal bar chart component using WidgetM.
    Displays a static horizontal bar chart with the given data.
    - `data`: Array of values to display
    - `labels`: Optional labels for each bar
    - `theme`: Theme for styling
    - `variant`: Color variant for bars
    - `dims`: Chart dimensions
-/
def horizontalBarChart (data : Array Float) (labels : Array String := #[])
    (theme : Theme) (variant : HorizontalBarChartVariant := .primary)
    (dims : HorizontalBarChart.Dimensions := HorizontalBarChart.defaultDimensions)
    : WidgetM HorizontalBarChartResult := do
  let name ← registerComponentW "horizontal-bar-chart" (isInteractive := false)

  let dataDyn ← Dynamic.pureM data

  emit do
    pure (horizontalBarChartVisual name data labels variant theme dims)

  pure { data := dataDyn }

/-- MultiColorHorizontalBarChart result. -/
structure MultiColorHorizontalBarChartResult where
  data : Reactive.Dynamic Spider (Array HorizontalBarChart.DataPoint)

/-- Create a multi-color horizontal bar chart where each bar can have its own color.
    - `data`: Array of data points with values, labels, and optional colors
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def multiColorHorizontalBarChart (data : Array HorizontalBarChart.DataPoint)
    (theme : Theme) (dims : HorizontalBarChart.Dimensions := HorizontalBarChart.defaultDimensions)
    : WidgetM MultiColorHorizontalBarChartResult := do
  let name ← registerComponentW "horizontal-bar-chart" (isInteractive := false)

  let dataDyn ← Dynamic.pureM data

  emit do
    pure (multiColorHorizontalBarChartVisual name data theme dims)

  pure { data := dataDyn }

end Afferent.Canopy

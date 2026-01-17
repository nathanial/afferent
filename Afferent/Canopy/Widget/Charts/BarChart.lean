/-
  Canopy BarChart Widget
  Vertical bar chart for comparing categorical data.
-/
import Reactive
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Reactive.Component
import Afferent.Canopy.Widget.Charts.Core
import Afferent.Canopy.Widget.Charts.ChartUtils

namespace Afferent.Canopy

open Afferent.Arbor hiding Event
open ChartUtils

/-- Bar chart color variant. -/
inductive BarChartVariant where
  | primary
  | secondary
  | success
  | warning
  | error
deriving Repr, BEq, Inhabited

namespace BarChart

/-- Dimensions and spacing for bar chart rendering. -/
structure Dimensions extends AxisChartDimensions where
  barGap : Float := 8.0
  cornerRadius : Float := 4.0
deriving Repr, Inhabited

/-- Default bar chart dimensions. -/
def defaultDimensions : Dimensions := {}

/-- Configuration for bar chart data. -/
structure DataPoint where
  value : Float
  label : Option String := none
  color : Option Color := none
deriving Repr, Inhabited

/-- Get the fill color for a variant. -/
def variantColor (variant : BarChartVariant) (theme : Theme) : Color :=
  match variant with
  | .primary => theme.primary.background
  | .secondary => theme.secondary.background
  | .success => Color.rgba 0.2 0.8 0.3 1.0
  | .warning => Color.rgba 1.0 0.7 0.0 1.0
  | .error => Color.rgba 0.9 0.2 0.2 1.0


/-- Custom spec for bar chart rendering. -/
def barChartSpec (data : Array Float) (labels : Array String)
    (variant : BarChartVariant) (theme : Theme)
    (dims : Dimensions := defaultDimensions) : CustomSpec := {
  measure := fun _ _ => (dims.marginLeft + dims.marginRight + 50, dims.marginTop + dims.marginBottom + 30)
  collect := fun layout =>
    let rect := layout.contentRect

    -- Use actual allocated size from layout
    let actualWidth := rect.width
    let actualHeight := rect.height

    -- Calculate chart area (inside margins)
    let chartX := rect.x + dims.marginLeft
    let chartY := rect.y + dims.marginTop
    let chartWidth := actualWidth - dims.marginLeft - dims.marginRight
    let chartHeight := actualHeight - dims.marginTop - dims.marginBottom

    -- Find max value for scaling (ensure at least 1 to avoid division by zero)
    let maxVal := data.foldl (fun acc v => if v > acc then v else acc) 0.0
    let maxVal := if maxVal <= 0.0 then 1.0 else maxVal

    -- Round up max to nice number for axis
    let niceMax := if maxVal <= 10 then 10.0
                   else if maxVal <= 50 then 50.0
                   else if maxVal <= 100 then 100.0
                   else if maxVal <= 500 then 500.0
                   else if maxVal <= 1000 then 1000.0
                   else (maxVal / 100).ceil * 100

    -- Calculate bar width based on data count
    let barCount := data.size
    let totalGapWidth := if barCount > 1 then dims.barGap * (barCount - 1).toFloat else 0.0
    let barWidth := if barCount > 0 then (chartWidth - totalGapWidth) / barCount.toFloat else 0.0

    let fillColor := variantColor variant theme
    let axisColor := Color.gray 0.5

    RenderM.build do
      -- Draw background
      RenderM.fillRect' rect.x rect.y actualWidth actualHeight (theme.panel.background.withAlpha 0.3) 6.0

      -- Draw grid lines
      if dims.showGridLines && dims.gridLineCount > 0 then
        for i in [0:dims.gridLineCount + 1] do
          let ratio := i.toFloat / dims.gridLineCount.toFloat
          let lineY := chartY + chartHeight - (ratio * chartHeight)
          RenderM.fillRect' chartX lineY chartWidth 1.0 (Color.gray 0.3) 0.0

      -- Draw bars
      for i in [0:barCount] do
        let value := data[i]!
        let barHeight := (value / niceMax) * chartHeight
        let barX := chartX + i.toFloat * (barWidth + dims.barGap)
        let barY := chartY + chartHeight - barHeight
        RenderM.fillRect' barX barY barWidth barHeight fillColor dims.cornerRadius

      -- Draw Y-axis labels
      if dims.gridLineCount > 0 then
        for i in [0:dims.gridLineCount + 1] do
          let ratio := i.toFloat / dims.gridLineCount.toFloat
          let value := ratio * niceMax
          let labelY := chartY + chartHeight - (ratio * chartHeight) - 6
          let labelText := ChartUtils.formatValue value
          RenderM.fillText labelText (rect.x + 4) labelY theme.smallFont theme.textMuted

      -- Draw X-axis labels
      if labels.size > 0 then
        for i in [0:min labels.size barCount] do
          let label := labels[i]!
          let labelX := chartX + i.toFloat * (barWidth + dims.barGap) + barWidth / 2
          let labelY := chartY + chartHeight + 16
          RenderM.fillText label labelX labelY theme.smallFont theme.text

      -- Draw axes
      RenderM.fillRect' chartX chartY 1.0 chartHeight axisColor 0.0
      RenderM.fillRect' chartX (chartY + chartHeight) chartWidth 1.0 axisColor 0.0

  draw := none
}

/-- Custom spec for bar chart with individually colored bars. -/
def multiColorBarChartSpec (data : Array DataPoint)
    (theme : Theme) (dims : Dimensions := defaultDimensions) : CustomSpec := {
  measure := fun _ _ => (dims.marginLeft + dims.marginRight + 50, dims.marginTop + dims.marginBottom + 30)
  collect := fun layout =>
    let rect := layout.contentRect

    -- Use actual allocated size from layout
    let actualWidth := rect.width
    let actualHeight := rect.height

    -- Calculate chart area
    let chartX := rect.x + dims.marginLeft
    let chartY := rect.y + dims.marginTop
    let chartWidth := actualWidth - dims.marginLeft - dims.marginRight
    let chartHeight := actualHeight - dims.marginTop - dims.marginBottom

    -- Find max value
    let maxVal := data.foldl (fun acc dp => if dp.value > acc then dp.value else acc) 0.0
    let maxVal := if maxVal <= 0.0 then 1.0 else maxVal
    let niceMax := if maxVal <= 10 then 10.0
                   else if maxVal <= 50 then 50.0
                   else if maxVal <= 100 then 100.0
                   else if maxVal <= 500 then 500.0
                   else if maxVal <= 1000 then 1000.0
                   else (maxVal / 100).ceil * 100

    let barCount := data.size
    let totalGapWidth := if barCount > 1 then dims.barGap * (barCount - 1).toFloat else 0.0
    let barWidth := if barCount > 0 then (chartWidth - totalGapWidth) / barCount.toFloat else 0.0

    -- Default colors for bars without custom color
    let defaultColors := ChartUtils.defaultColors theme

    let axisColor := Color.gray 0.5

    RenderM.build do
      -- Background
      RenderM.fillRect' rect.x rect.y actualWidth actualHeight (theme.panel.background.withAlpha 0.3) 6.0

      -- Grid lines
      if dims.showGridLines && dims.gridLineCount > 0 then
        for i in [0:dims.gridLineCount + 1] do
          let ratio := i.toFloat / dims.gridLineCount.toFloat
          let lineY := chartY + chartHeight - (ratio * chartHeight)
          RenderM.fillRect' chartX lineY chartWidth 1.0 (Color.gray 0.3) 0.0

      -- Draw bars with individual colors
      for i in [0:barCount] do
        let dp := data[i]!
        let barHeight := (dp.value / niceMax) * chartHeight
        let barX := chartX + i.toFloat * (barWidth + dims.barGap)
        let barY := chartY + chartHeight - barHeight
        let color := dp.color.getD (defaultColors[i % defaultColors.size]!)
        RenderM.fillRect' barX barY barWidth barHeight color dims.cornerRadius

      -- Y-axis labels
      if dims.gridLineCount > 0 then
        for i in [0:dims.gridLineCount + 1] do
          let ratio := i.toFloat / dims.gridLineCount.toFloat
          let value := ratio * niceMax
          let labelY := chartY + chartHeight - (ratio * chartHeight) - 6
          let labelText := ChartUtils.formatValue value
          RenderM.fillText labelText (rect.x + 4) labelY theme.smallFont theme.textMuted

      -- X-axis labels
      for i in [0:barCount] do
        let dp := data[i]!
        if let some label := dp.label then
          let labelX := chartX + i.toFloat * (barWidth + dims.barGap) + barWidth / 2
          let labelY := chartY + chartHeight + 16
          RenderM.fillText label labelX labelY theme.smallFont theme.text

      -- Axes
      RenderM.fillRect' chartX chartY 1.0 chartHeight axisColor 0.0
      RenderM.fillRect' chartX (chartY + chartHeight) chartWidth 1.0 axisColor 0.0

  draw := none
}

end BarChart

/-- Build a bar chart visual (WidgetBuilder version).
    - `name`: Widget name for identification
    - `data`: Array of values to display
    - `labels`: Optional labels for each bar
    - `variant`: Color variant for bars
    - `theme`: Theme for styling
    - `dims`: Chart dimensions (margins only - actual size from layout)
-/
def barChartVisual (name : String) (data : Array Float)
    (labels : Array String := #[])
    (variant : BarChartVariant := .primary) (theme : Theme)
    (dims : BarChart.Dimensions := BarChart.defaultDimensions) : WidgetBuilder := do
  let wid ← freshId
  let chart ← custom (BarChart.barChartSpec data labels variant theme dims) {
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

/-- Build a multi-color bar chart visual (WidgetBuilder version).
    Each data point can have its own color.
    - `name`: Widget name for identification
    - `data`: Array of data points with values, labels, and optional colors
    - `theme`: Theme for styling
    - `dims`: Chart dimensions (margins only - actual size from layout)
-/
def multiColorBarChartVisual (name : String) (data : Array BarChart.DataPoint)
    (theme : Theme) (dims : BarChart.Dimensions := BarChart.defaultDimensions) : WidgetBuilder := do
  let wid ← freshId
  let chart ← custom (BarChart.multiColorBarChartSpec data theme dims) {
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

/-! ## Reactive BarChart Components (FRP-based)

These use WidgetM for declarative composition.
-/

open Reactive Reactive.Host
open Afferent.Canopy.Reactive

/-- BarChart result - provides access to chart state. -/
structure BarChartResult where
  /-- The data being displayed. -/
  data : Dyn (Array Float)

/-- Create a bar chart component using WidgetM with dynamic data.
    The chart automatically rebuilds when the data Dynamic changes.
    - `data`: Dynamic array of values to display
    - `labels`: Optional labels for each bar
    - `theme`: Theme for styling
    - `variant`: Color variant for bars
    - `dims`: Chart dimensions
-/
def barChart (data : Dyn (Array Float)) (labels : Array String := #[])
    (theme : Theme) (variant : BarChartVariant := .primary)
    (dims : BarChart.Dimensions := BarChart.defaultDimensions)
    : WidgetM BarChartResult := do
  let _ ← dynWidget data fun currentData => do
    let name ← registerComponentW "bar-chart" (isInteractive := false)
    emit do pure (barChartVisual name currentData labels variant theme dims)

  pure { data }

/-- MultiColorBarChart result. -/
structure MultiColorBarChartResult where
  data : Dyn (Array BarChart.DataPoint)

/-- Create a multi-color bar chart where each bar can have its own color.
    The chart automatically rebuilds when the data Dynamic changes.
    - `data`: Dynamic array of data points with values, labels, and optional colors
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def multiColorBarChart (data : Dyn (Array BarChart.DataPoint))
    (theme : Theme) (dims : BarChart.Dimensions := BarChart.defaultDimensions)
    : WidgetM MultiColorBarChartResult := do
  let _ ← dynWidget data fun currentData => do
    let name ← registerComponentW "bar-chart" (isInteractive := false)
    emit do pure (multiColorBarChartVisual name currentData theme dims)

  pure { data }

end Afferent.Canopy

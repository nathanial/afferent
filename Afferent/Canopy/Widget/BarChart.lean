/-
  Canopy BarChart Widget
  Vertical bar chart for comparing categorical data.
-/
import Reactive
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Reactive.Component

namespace Afferent.Canopy

open Afferent.Arbor hiding Event

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
structure Dimensions where
  width : Float := 400.0
  height : Float := 250.0
  marginTop : Float := 20.0
  marginBottom : Float := 40.0
  marginLeft : Float := 50.0
  marginRight : Float := 20.0
  barGap : Float := 8.0
  cornerRadius : Float := 4.0
  showGridLines : Bool := true
  gridLineCount : Nat := 5
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

/-- Format a float value for axis labels. -/
private def formatValue (v : Float) : String :=
  if v >= 1000000 then
    s!"{(v / 1000000).floor.toUInt32}M"
  else if v >= 1000 then
    s!"{(v / 1000).floor.toUInt32}K"
  else if v == v.floor then
    s!"{v.floor.toUInt32}"
  else
    -- Show one decimal place
    let whole := v.floor.toInt32
    let frac := ((v - v.floor) * 10).floor.toUInt32
    s!"{whole}.{frac}"

/-- Custom spec for bar chart rendering. -/
def barChartSpec (data : Array Float) (labels : Array String)
    (variant : BarChartVariant) (theme : Theme)
    (dims : Dimensions := defaultDimensions) : CustomSpec := {
  measure := fun _ _ => (dims.width, dims.height)
  collect := fun layout =>
    let rect := layout.contentRect
    let cmds : Array RenderCommand := #[]

    -- Calculate chart area (inside margins)
    let chartX := rect.x + dims.marginLeft
    let chartY := rect.y + dims.marginTop
    let chartWidth := dims.width - dims.marginLeft - dims.marginRight
    let chartHeight := dims.height - dims.marginTop - dims.marginBottom

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

    -- Draw bars
    let fillColor := variantColor variant theme
    let cmds := Id.run do
      let mut cmds := cmds
      for i in [0:barCount] do
        let value := data[i]!
        let barHeight := (value / niceMax) * chartHeight
        let barX := chartX + i.toFloat * (barWidth + dims.barGap)
        let barY := chartY + chartHeight - barHeight
        let barRect := Arbor.Rect.mk' barX barY barWidth barHeight
        -- Use custom color if provided (for multi-series), otherwise variant color
        cmds := cmds.push (.fillRect barRect fillColor dims.cornerRadius)
      cmds

    -- Draw Y-axis labels
    let cmds := if dims.gridLineCount > 0 then
      Id.run do
        let mut cmds := cmds
        for i in [0:dims.gridLineCount + 1] do
          let ratio := i.toFloat / dims.gridLineCount.toFloat
          let value := ratio * niceMax
          let labelY := chartY + chartHeight - (ratio * chartHeight) - 6
          let labelText := formatValue value
          cmds := cmds.push (.fillText labelText (rect.x + 4) labelY theme.smallFont theme.textMuted)
        cmds
    else cmds

    -- Draw X-axis labels
    let cmds := if labels.size > 0 then
      Id.run do
        let mut cmds := cmds
        for i in [0:min labels.size barCount] do
          let label := labels[i]!
          let labelX := chartX + i.toFloat * (barWidth + dims.barGap) + barWidth / 2
          let labelY := chartY + chartHeight + 16
          cmds := cmds.push (.fillText label labelX labelY theme.smallFont theme.text)
        cmds
    else cmds

    -- Draw axes
    let axisColor := Color.gray 0.5
    -- Y-axis
    let yAxisRect := Arbor.Rect.mk' chartX chartY 1.0 chartHeight
    let cmds := cmds.push (.fillRect yAxisRect axisColor 0.0)
    -- X-axis
    let xAxisRect := Arbor.Rect.mk' chartX (chartY + chartHeight) chartWidth 1.0
    cmds.push (.fillRect xAxisRect axisColor 0.0)

  draw := none
}

/-- Custom spec for bar chart with individually colored bars. -/
def multiColorBarChartSpec (data : Array DataPoint)
    (theme : Theme) (dims : Dimensions := defaultDimensions) : CustomSpec := {
  measure := fun _ _ => (dims.width, dims.height)
  collect := fun layout =>
    let rect := layout.contentRect

    -- Calculate chart area
    let chartX := rect.x + dims.marginLeft
    let chartY := rect.y + dims.marginTop
    let chartWidth := dims.width - dims.marginLeft - dims.marginRight
    let chartHeight := dims.height - dims.marginTop - dims.marginBottom

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

    let cmds : Array RenderCommand := #[]

    -- Background
    let bgRect := Arbor.Rect.mk' rect.x rect.y dims.width dims.height
    let cmds := cmds.push (.fillRect bgRect (theme.panel.background.withAlpha 0.3) 6.0)

    -- Grid lines
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

    -- Default colors for bars without custom color
    let defaultColors := #[
      theme.primary.background,
      theme.secondary.background,
      Color.rgba 0.2 0.8 0.3 1.0,
      Color.rgba 1.0 0.7 0.0 1.0,
      Color.rgba 0.9 0.2 0.2 1.0,
      Color.rgba 0.5 0.3 0.9 1.0,
      Color.rgba 0.0 0.7 0.7 1.0
    ]

    -- Draw bars with individual colors
    let cmds := Id.run do
      let mut cmds := cmds
      for i in [0:barCount] do
        let dp := data[i]!
        let barHeight := (dp.value / niceMax) * chartHeight
        let barX := chartX + i.toFloat * (barWidth + dims.barGap)
        let barY := chartY + chartHeight - barHeight
        let barRect := Arbor.Rect.mk' barX barY barWidth barHeight
        let color := dp.color.getD (defaultColors[i % defaultColors.size]!)
        cmds := cmds.push (.fillRect barRect color dims.cornerRadius)
      cmds

    -- Y-axis labels
    let cmds := if dims.gridLineCount > 0 then
      Id.run do
        let mut cmds := cmds
        for i in [0:dims.gridLineCount + 1] do
          let ratio := i.toFloat / dims.gridLineCount.toFloat
          let value := ratio * niceMax
          let labelY := chartY + chartHeight - (ratio * chartHeight) - 6
          let labelText := formatValue value
          cmds := cmds.push (.fillText labelText (rect.x + 4) labelY theme.smallFont theme.textMuted)
        cmds
    else cmds

    -- X-axis labels
    let cmds := Id.run do
      let mut cmds := cmds
      for i in [0:barCount] do
        let dp := data[i]!
        match dp.label with
        | some label =>
          let labelX := chartX + i.toFloat * (barWidth + dims.barGap) + barWidth / 2
          let labelY := chartY + chartHeight + 16
          cmds := cmds.push (.fillText label labelX labelY theme.smallFont theme.text)
        | none => pure ()
      cmds

    -- Axes
    let axisColor := Color.gray 0.5
    let yAxisRect := Arbor.Rect.mk' chartX chartY 1.0 chartHeight
    let cmds := cmds.push (.fillRect yAxisRect axisColor 0.0)
    let xAxisRect := Arbor.Rect.mk' chartX (chartY + chartHeight) chartWidth 1.0
    cmds.push (.fillRect xAxisRect axisColor 0.0)

  draw := none
}

end BarChart

/-- Build a bar chart visual (WidgetBuilder version).
    - `name`: Widget name for identification
    - `data`: Array of values to display
    - `labels`: Optional labels for each bar
    - `variant`: Color variant for bars
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def barChartVisual (name : String) (data : Array Float)
    (labels : Array String := #[])
    (variant : BarChartVariant := .primary) (theme : Theme)
    (dims : BarChart.Dimensions := BarChart.defaultDimensions) : WidgetBuilder := do
  let wid ← freshId
  let chart ← custom (BarChart.barChartSpec data labels variant theme dims) {
    minWidth := some dims.width
    minHeight := some dims.height
  }
  let props : Trellis.FlexContainer := { Trellis.FlexContainer.column 0 with alignItems := .flexStart }
  pure (.flex wid (some name) props {} #[chart])

/-- Build a multi-color bar chart visual (WidgetBuilder version).
    Each data point can have its own color.
    - `name`: Widget name for identification
    - `data`: Array of data points with values, labels, and optional colors
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def multiColorBarChartVisual (name : String) (data : Array BarChart.DataPoint)
    (theme : Theme) (dims : BarChart.Dimensions := BarChart.defaultDimensions) : WidgetBuilder := do
  let wid ← freshId
  let chart ← custom (BarChart.multiColorBarChartSpec data theme dims) {
    minWidth := some dims.width
    minHeight := some dims.height
  }
  let props : Trellis.FlexContainer := { Trellis.FlexContainer.column 0 with alignItems := .flexStart }
  pure (.flex wid (some name) props {} #[chart])

/-! ## Reactive BarChart Components (FRP-based)

These use WidgetM for declarative composition.
-/

open Reactive Reactive.Host
open Afferent.Canopy.Reactive

/-- BarChart result - provides access to chart state. -/
structure BarChartResult where
  /-- The data being displayed. -/
  data : Reactive.Dynamic Spider (Array Float)

/-- Create a bar chart component using WidgetM.
    Displays a static bar chart with the given data.
    - `data`: Array of values to display
    - `labels`: Optional labels for each bar
    - `theme`: Theme for styling
    - `variant`: Color variant for bars
    - `dims`: Chart dimensions
-/
def barChart (data : Array Float) (labels : Array String := #[])
    (theme : Theme) (variant : BarChartVariant := .primary)
    (dims : BarChart.Dimensions := BarChart.defaultDimensions)
    : WidgetM BarChartResult := do
  let name ← registerComponentW "bar-chart" (isInteractive := false)

  let dataDyn ← Dynamic.pureM data

  emit do
    pure (barChartVisual name data labels variant theme dims)

  pure { data := dataDyn }

/-- Create a bar chart that updates based on an external event stream.
    Useful for showing dynamic data that changes over time.
    - `initialData`: Initial data values
    - `dataUpdates`: Event stream of data updates
    - `labels`: Labels for each bar
    - `theme`: Theme for styling
    - `variant`: Color variant
    - `dims`: Chart dimensions
-/
def barChartWithEvents (initialData : Array Float)
    (dataUpdates : Reactive.Event Spider (Array Float))
    (labels : Array String := #[])
    (theme : Theme) (variant : BarChartVariant := .primary)
    (dims : BarChart.Dimensions := BarChart.defaultDimensions)
    : WidgetM BarChartResult := do
  let name ← registerComponentW "bar-chart" (isInteractive := false)

  let dataDyn ← Reactive.holdDyn initialData dataUpdates

  emit do
    let d ← dataDyn.sample
    pure (barChartVisual name d labels variant theme dims)

  pure { data := dataDyn }

/-- MultiColorBarChart result. -/
structure MultiColorBarChartResult where
  data : Reactive.Dynamic Spider (Array BarChart.DataPoint)

/-- Create a multi-color bar chart where each bar can have its own color.
    - `data`: Array of data points with values, labels, and optional colors
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def multiColorBarChart (data : Array BarChart.DataPoint)
    (theme : Theme) (dims : BarChart.Dimensions := BarChart.defaultDimensions)
    : WidgetM MultiColorBarChartResult := do
  let name ← registerComponentW "bar-chart" (isInteractive := false)

  let dataDyn ← Dynamic.pureM data

  emit do
    pure (multiColorBarChartVisual name data theme dims)

  pure { data := dataDyn }

end Afferent.Canopy

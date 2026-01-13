/-
  Canopy LineChart Widget
  Line chart for showing trends over time or continuous data.
-/
import Reactive
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Reactive.Component

namespace Afferent.Canopy

open Afferent.Arbor hiding Event

/-- Line chart color variant. -/
inductive LineChartVariant where
  | primary
  | secondary
  | success
  | warning
  | error
deriving Repr, BEq, Inhabited

namespace LineChart

/-- Dimensions and spacing for line chart rendering. -/
structure Dimensions where
  width : Float := 400.0
  height : Float := 250.0
  marginTop : Float := 20.0
  marginBottom : Float := 40.0
  marginLeft : Float := 50.0
  marginRight : Float := 20.0
  lineWidth : Float := 2.0
  markerRadius : Float := 4.0
  showMarkers : Bool := true
  showGridLines : Bool := true
  gridLineCount : Nat := 5
deriving Repr, Inhabited

/-- Default line chart dimensions. -/
def defaultDimensions : Dimensions := {}

/-- A data series for multi-line charts. -/
structure Series where
  values : Array Float
  color : Option Color := none
  label : Option String := none
deriving Repr, Inhabited

/-- Get the fill color for a variant. -/
def variantColor (variant : LineChartVariant) (theme : Theme) : Color :=
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

/-- Custom spec for single-series line chart rendering. -/
def lineChartSpec (data : Array Float) (labels : Array String)
    (variant : LineChartVariant) (theme : Theme)
    (dims : Dimensions := defaultDimensions) : CustomSpec := {
  measure := fun _ _ => (dims.marginLeft + dims.marginRight + 50, dims.marginTop + dims.marginBottom + 30)
  collect := fun layout =>
    let rect := layout.contentRect
    let cmds : Array RenderCommand := #[]

    -- Use actual allocated size for responsive layout
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

    let pointCount := data.size
    let stepX := if pointCount > 1 then chartWidth / (pointCount - 1).toFloat else 0.0

    -- Draw background
    let bgRect := Arbor.Rect.mk' rect.x rect.y actualWidth actualHeight
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

    -- Build line path
    let lineColor := variantColor variant theme
    let cmds := if pointCount > 0 then
      Id.run do
        let mut path := Arbor.Path.empty
        for i in [0:pointCount] do
          let value := data[i]!
          let x := chartX + i.toFloat * stepX
          let y := chartY + chartHeight - (value / niceMaxVal) * chartHeight
          let pt := Arbor.Point.mk' x y
          if i == 0 then
            path := path.moveTo pt
          else
            path := path.lineTo pt
        cmds.push (.strokePath path lineColor dims.lineWidth)
    else cmds

    -- Draw markers
    let cmds := if dims.showMarkers && pointCount > 0 then
      Id.run do
        let mut cmds := cmds
        for i in [0:pointCount] do
          let value := data[i]!
          let x := chartX + i.toFloat * stepX
          let y := chartY + chartHeight - (value / niceMaxVal) * chartHeight
          let markerPath := Arbor.Path.circle (Arbor.Point.mk' x y) dims.markerRadius
          cmds := cmds.push (.fillPath markerPath lineColor)
        cmds
    else cmds

    -- Draw Y-axis labels
    let cmds := if dims.gridLineCount > 0 then
      Id.run do
        let mut cmds := cmds
        for i in [0:dims.gridLineCount + 1] do
          let ratio := i.toFloat / dims.gridLineCount.toFloat
          let value := ratio * niceMaxVal
          let labelY := chartY + chartHeight - (ratio * chartHeight) - 6
          let labelText := formatValue value
          cmds := cmds.push (.fillText labelText (rect.x + 4) labelY theme.smallFont theme.textMuted)
        cmds
    else cmds

    -- Draw X-axis labels
    let cmds := if labels.size > 0 && pointCount > 0 then
      Id.run do
        let mut cmds := cmds
        for i in [0:min labels.size pointCount] do
          let label := labels[i]!
          let labelX := chartX + i.toFloat * stepX
          let labelY := chartY + chartHeight + 16
          cmds := cmds.push (.fillText label labelX labelY theme.smallFont theme.text)
        cmds
    else cmds

    -- Draw axes
    let axisColor := Color.gray 0.5
    let yAxisRect := Arbor.Rect.mk' chartX chartY 1.0 chartHeight
    let cmds := cmds.push (.fillRect yAxisRect axisColor 0.0)
    let xAxisRect := Arbor.Rect.mk' chartX (chartY + chartHeight) chartWidth 1.0
    cmds.push (.fillRect xAxisRect axisColor 0.0)

  draw := none
}

/-- Default colors for multi-series charts. -/
def defaultSeriesColors (theme : Theme) : Array Color := #[
  theme.primary.background,
  theme.secondary.background,
  Color.rgba 0.2 0.8 0.3 1.0,
  Color.rgba 1.0 0.7 0.0 1.0,
  Color.rgba 0.9 0.2 0.2 1.0,
  Color.rgba 0.5 0.3 0.9 1.0,
  Color.rgba 0.0 0.7 0.7 1.0
]

/-- Custom spec for multi-series line chart rendering. -/
def multiSeriesSpec (series : Array Series) (labels : Array String)
    (theme : Theme) (dims : Dimensions := defaultDimensions) : CustomSpec := {
  measure := fun _ _ => (dims.marginLeft + dims.marginRight + 50, dims.marginTop + dims.marginBottom + 30)
  collect := fun layout =>
    let rect := layout.contentRect
    let cmds : Array RenderCommand := #[]

    -- Use actual allocated size for responsive layout
    let actualWidth := rect.width
    let actualHeight := rect.height

    let chartX := rect.x + dims.marginLeft
    let chartY := rect.y + dims.marginTop
    let chartWidth := actualWidth - dims.marginLeft - dims.marginRight
    let chartHeight := actualHeight - dims.marginTop - dims.marginBottom

    -- Find global max value across all series
    let maxVal := series.foldl (fun acc s =>
      s.values.foldl (fun acc2 v => if v > acc2 then v else acc2) acc) 0.0
    let niceMaxVal := niceMax maxVal

    -- Find max point count
    let maxPoints := series.foldl (fun acc s => max acc s.values.size) 0
    let stepX := if maxPoints > 1 then chartWidth / (maxPoints - 1).toFloat else 0.0

    -- Draw background
    let bgRect := Arbor.Rect.mk' rect.x rect.y actualWidth actualHeight
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

    let defaultColors := defaultSeriesColors theme

    -- Draw each series
    let cmds := Id.run do
      let mut cmds := cmds
      for si in [0:series.size] do
        let s := series[si]!
        let color := s.color.getD (defaultColors[si % defaultColors.size]!)
        let pointCount := s.values.size

        -- Draw line
        if pointCount > 0 then
          let mut path := Arbor.Path.empty
          for i in [0:pointCount] do
            let value := s.values[i]!
            let x := chartX + i.toFloat * stepX
            let y := chartY + chartHeight - (value / niceMaxVal) * chartHeight
            let pt := Arbor.Point.mk' x y
            if i == 0 then
              path := path.moveTo pt
            else
              path := path.lineTo pt
          cmds := cmds.push (.strokePath path color dims.lineWidth)

          -- Draw markers
          if dims.showMarkers then
            for i in [0:pointCount] do
              let value := s.values[i]!
              let x := chartX + i.toFloat * stepX
              let y := chartY + chartHeight - (value / niceMaxVal) * chartHeight
              let markerPath := Arbor.Path.circle (Arbor.Point.mk' x y) dims.markerRadius
              cmds := cmds.push (.fillPath markerPath color)
      cmds

    -- Draw Y-axis labels
    let cmds := if dims.gridLineCount > 0 then
      Id.run do
        let mut cmds := cmds
        for i in [0:dims.gridLineCount + 1] do
          let ratio := i.toFloat / dims.gridLineCount.toFloat
          let value := ratio * niceMaxVal
          let labelY := chartY + chartHeight - (ratio * chartHeight) - 6
          let labelText := formatValue value
          cmds := cmds.push (.fillText labelText (rect.x + 4) labelY theme.smallFont theme.textMuted)
        cmds
    else cmds

    -- Draw X-axis labels
    let cmds := if labels.size > 0 && maxPoints > 0 then
      Id.run do
        let mut cmds := cmds
        for i in [0:min labels.size maxPoints] do
          let label := labels[i]!
          let labelX := chartX + i.toFloat * stepX
          let labelY := chartY + chartHeight + 16
          cmds := cmds.push (.fillText label labelX labelY theme.smallFont theme.text)
        cmds
    else cmds

    -- Draw axes
    let axisColor := Color.gray 0.5
    let yAxisRect := Arbor.Rect.mk' chartX chartY 1.0 chartHeight
    let cmds := cmds.push (.fillRect yAxisRect axisColor 0.0)
    let xAxisRect := Arbor.Rect.mk' chartX (chartY + chartHeight) chartWidth 1.0
    cmds.push (.fillRect xAxisRect axisColor 0.0)

  draw := none
}

end LineChart

/-- Build a line chart visual (WidgetBuilder version).
    - `name`: Widget name for identification
    - `data`: Array of values to display
    - `labels`: Optional labels for each data point
    - `variant`: Color variant for the line
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def lineChartVisual (name : String) (data : Array Float)
    (labels : Array String := #[])
    (variant : LineChartVariant := .primary) (theme : Theme)
    (dims : LineChart.Dimensions := LineChart.defaultDimensions) : WidgetBuilder := do
  let wid ← freshId
  let chart ← custom (LineChart.lineChartSpec data labels variant theme dims) {
    width := .percent 1.0
    height := .percent 1.0
    flexItem := some (Trellis.FlexItem.growing 1)
  }
  let style : BoxStyle := { width := .percent 1.0, height := .percent 1.0, flexItem := some (Trellis.FlexItem.growing 1) }
  let props : Trellis.FlexContainer := { Trellis.FlexContainer.column 0 with alignItems := .stretch }
  pure (.flex wid (some name) props style #[chart])

/-- Build a multi-series line chart visual (WidgetBuilder version).
    - `name`: Widget name for identification
    - `series`: Array of data series
    - `labels`: Labels for the X-axis
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def multiSeriesLineChartVisual (name : String) (series : Array LineChart.Series)
    (labels : Array String := #[]) (theme : Theme)
    (dims : LineChart.Dimensions := LineChart.defaultDimensions) : WidgetBuilder := do
  let wid ← freshId
  let chart ← custom (LineChart.multiSeriesSpec series labels theme dims) {
    width := .percent 1.0
    height := .percent 1.0
    flexItem := some (Trellis.FlexItem.growing 1)
  }
  let style : BoxStyle := { width := .percent 1.0, height := .percent 1.0, flexItem := some (Trellis.FlexItem.growing 1) }
  let props : Trellis.FlexContainer := { Trellis.FlexContainer.column 0 with alignItems := .stretch }
  pure (.flex wid (some name) props style #[chart])

/-! ## Reactive LineChart Components (FRP-based)

These use WidgetM for declarative composition.
-/

open Reactive Reactive.Host
open Afferent.Canopy.Reactive

/-- LineChart result - provides access to chart state. -/
structure LineChartResult where
  /-- The data being displayed. -/
  data : Reactive.Dynamic Spider (Array Float)

/-- Create a line chart component using WidgetM.
    Displays a static line chart with the given data.
    - `data`: Array of values to display
    - `labels`: Optional labels for each data point
    - `theme`: Theme for styling
    - `variant`: Color variant for the line
    - `dims`: Chart dimensions
-/
def lineChart (data : Array Float) (labels : Array String := #[])
    (theme : Theme) (variant : LineChartVariant := .primary)
    (dims : LineChart.Dimensions := LineChart.defaultDimensions)
    : WidgetM LineChartResult := do
  let name ← registerComponentW "line-chart" (isInteractive := false)

  let dataDyn ← Dynamic.pureM data

  emit do
    pure (lineChartVisual name data labels variant theme dims)

  pure { data := dataDyn }

/-- Create a line chart that updates based on an external event stream.
    Useful for showing real-time data.
    - `initialData`: Initial data values
    - `dataUpdates`: Event stream of data updates
    - `labels`: Labels for each data point
    - `theme`: Theme for styling
    - `variant`: Color variant
    - `dims`: Chart dimensions
-/
def lineChartWithEvents (initialData : Array Float)
    (dataUpdates : Reactive.Event Spider (Array Float))
    (labels : Array String := #[])
    (theme : Theme) (variant : LineChartVariant := .primary)
    (dims : LineChart.Dimensions := LineChart.defaultDimensions)
    : WidgetM LineChartResult := do
  let name ← registerComponentW "line-chart" (isInteractive := false)

  let dataDyn ← Reactive.holdDyn initialData dataUpdates

  emit do
    let d ← dataDyn.sample
    pure (lineChartVisual name d labels variant theme dims)

  pure { data := dataDyn }

/-- MultiSeriesLineChart result. -/
structure MultiSeriesLineChartResult where
  series : Reactive.Dynamic Spider (Array LineChart.Series)

/-- Create a multi-series line chart for comparing multiple data sets.
    - `series`: Array of data series with values and optional colors/labels
    - `labels`: Labels for the X-axis
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def multiSeriesLineChart (series : Array LineChart.Series)
    (labels : Array String := #[]) (theme : Theme)
    (dims : LineChart.Dimensions := LineChart.defaultDimensions)
    : WidgetM MultiSeriesLineChartResult := do
  let name ← registerComponentW "line-chart" (isInteractive := false)

  let seriesDyn ← Dynamic.pureM series

  emit do
    pure (multiSeriesLineChartVisual name series labels theme dims)

  pure { series := seriesDyn }

end Afferent.Canopy

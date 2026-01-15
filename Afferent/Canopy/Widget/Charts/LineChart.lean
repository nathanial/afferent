/-
  Canopy LineChart Widget
  Line chart for showing trends over time or continuous data.
-/
import Reactive
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Reactive.Component
import Afferent.Canopy.Widget.Charts.ChartUtils

namespace Afferent.Canopy

open Afferent.Arbor hiding Event
open ChartUtils

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


/-- Custom spec for single-series line chart rendering. -/
def lineChartSpec (data : Array Float) (labels : Array String)
    (variant : LineChartVariant) (theme : Theme)
    (dims : Dimensions := defaultDimensions) : CustomSpec := {
  measure := fun _ _ => (dims.marginLeft + dims.marginRight + 50, dims.marginTop + dims.marginBottom + 30)
  collect := fun layout =>
    let rect := layout.contentRect

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
    let niceMaxVal := ChartUtils.niceMax maxVal

    let pointCount := data.size
    let stepX := if pointCount > 1 then chartWidth / (pointCount - 1).toFloat else 0.0

    let lineColor := variantColor variant theme
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

      -- Build and stroke line path
      if pointCount > 0 then
        let path := Id.run do
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
          path
        RenderM.strokePath path lineColor dims.lineWidth

      -- Draw markers
      if dims.showMarkers && pointCount > 0 then
        for i in [0:pointCount] do
          let value := data[i]!
          let x := chartX + i.toFloat * stepX
          let y := chartY + chartHeight - (value / niceMaxVal) * chartHeight
          -- GPU-batched marker
          RenderM.fillCircle' x y dims.markerRadius lineColor

      -- Draw Y-axis labels
      if dims.gridLineCount > 0 then
        for i in [0:dims.gridLineCount + 1] do
          let ratio := i.toFloat / dims.gridLineCount.toFloat
          let value := ratio * niceMaxVal
          let labelY := chartY + chartHeight - (ratio * chartHeight) - 6
          let labelText := ChartUtils.formatValue value
          RenderM.fillText labelText (rect.x + 4) labelY theme.smallFont theme.textMuted

      -- Draw X-axis labels
      if labels.size > 0 && pointCount > 0 then
        for i in [0:min labels.size pointCount] do
          let label := labels[i]!
          let labelX := chartX + i.toFloat * stepX
          let labelY := chartY + chartHeight + 16
          RenderM.fillText label labelX labelY theme.smallFont theme.text

      -- Draw axes
      RenderM.fillRect' chartX chartY 1.0 chartHeight axisColor 0.0
      RenderM.fillRect' chartX (chartY + chartHeight) chartWidth 1.0 axisColor 0.0

  draw := none
}

/-- Default colors for multi-series charts. -/
def defaultSeriesColors (theme : Theme) : Array Color := ChartUtils.defaultColors theme

/-- Custom spec for multi-series line chart rendering. -/
def multiSeriesSpec (series : Array Series) (labels : Array String)
    (theme : Theme) (dims : Dimensions := defaultDimensions) : CustomSpec := {
  measure := fun _ _ => (dims.marginLeft + dims.marginRight + 50, dims.marginTop + dims.marginBottom + 30)
  collect := fun layout =>
    let rect := layout.contentRect

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
    let niceMaxVal := ChartUtils.niceMax maxVal

    -- Find max point count
    let maxPoints := series.foldl (fun acc s => max acc s.values.size) 0
    let stepX := if maxPoints > 1 then chartWidth / (maxPoints - 1).toFloat else 0.0

    let defaultColors := defaultSeriesColors theme
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

      -- Draw each series
      for si in [0:series.size] do
        let s := series[si]!
        let color := s.color.getD (defaultColors[si % defaultColors.size]!)
        let pointCount := s.values.size

        -- Draw line
        if pointCount > 0 then
          let path := Id.run do
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
            path
          RenderM.strokePath path color dims.lineWidth

          -- Draw markers
          if dims.showMarkers then
            for i in [0:pointCount] do
              let value := s.values[i]!
              let x := chartX + i.toFloat * stepX
              let y := chartY + chartHeight - (value / niceMaxVal) * chartHeight
              -- GPU-batched marker
              RenderM.fillCircle' x y dims.markerRadius color

      -- Draw Y-axis labels
      if dims.gridLineCount > 0 then
        for i in [0:dims.gridLineCount + 1] do
          let ratio := i.toFloat / dims.gridLineCount.toFloat
          let value := ratio * niceMaxVal
          let labelY := chartY + chartHeight - (ratio * chartHeight) - 6
          let labelText := ChartUtils.formatValue value
          RenderM.fillText labelText (rect.x + 4) labelY theme.smallFont theme.textMuted

      -- Draw X-axis labels
      if labels.size > 0 && maxPoints > 0 then
        for i in [0:min labels.size maxPoints] do
          let label := labels[i]!
          let labelX := chartX + i.toFloat * stepX
          let labelY := chartY + chartHeight + 16
          RenderM.fillText label labelX labelY theme.smallFont theme.text

      -- Draw axes
      RenderM.fillRect' chartX chartY 1.0 chartHeight axisColor 0.0
      RenderM.fillRect' chartX (chartY + chartHeight) chartWidth 1.0 axisColor 0.0

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

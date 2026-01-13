/-
  Canopy AreaChart Widget
  Area chart for showing trends with filled areas under the line.
-/
import Reactive
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Reactive.Component

namespace Afferent.Canopy

open Afferent.Arbor hiding Event

/-- Area chart color variant. -/
inductive AreaChartVariant where
  | primary
  | secondary
  | success
  | warning
  | error
deriving Repr, BEq, Inhabited

namespace AreaChart

/-- Dimensions and spacing for area chart rendering. -/
structure Dimensions where
  width : Float := 400.0
  height : Float := 250.0
  marginTop : Float := 20.0
  marginBottom : Float := 40.0
  marginLeft : Float := 50.0
  marginRight : Float := 20.0
  lineWidth : Float := 2.0
  fillOpacity : Float := 0.3
  showLine : Bool := true
  showMarkers : Bool := false
  markerRadius : Float := 3.0
  showGridLines : Bool := true
  gridLineCount : Nat := 5
deriving Repr, Inhabited

/-- Default area chart dimensions. -/
def defaultDimensions : Dimensions := {}

/-- A data series for multi-series area charts. -/
structure Series where
  values : Array Float
  color : Option Color := none
  label : Option String := none
deriving Repr, Inhabited

/-- Get the fill color for a variant. -/
def variantColor (variant : AreaChartVariant) (theme : Theme) : Color :=
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

/-- Custom spec for single-series area chart rendering. -/
def areaChartSpec (data : Array Float) (labels : Array String)
    (variant : AreaChartVariant) (theme : Theme)
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

    -- Build filled area path
    let areaColor := variantColor variant theme
    let cmds := if pointCount > 0 then
      Id.run do
        let mut areaPath := Arbor.Path.empty
        -- Start at bottom-left
        let startX := chartX
        let baseY := chartY + chartHeight
        areaPath := areaPath.moveTo (Arbor.Point.mk' startX baseY)
        -- Line up to first data point
        let firstY := chartY + chartHeight - (data[0]! / niceMaxVal) * chartHeight
        areaPath := areaPath.lineTo (Arbor.Point.mk' startX firstY)
        -- Draw line through all data points
        for i in [1:pointCount] do
          let value := data[i]!
          let x := chartX + i.toFloat * stepX
          let y := chartY + chartHeight - (value / niceMaxVal) * chartHeight
          areaPath := areaPath.lineTo (Arbor.Point.mk' x y)
        -- Line down to bottom-right
        let lastX := chartX + (pointCount - 1).toFloat * stepX
        areaPath := areaPath.lineTo (Arbor.Point.mk' lastX baseY)
        -- Close path back to start
        areaPath := areaPath.lineTo (Arbor.Point.mk' startX baseY)
        cmds.push (.fillPath areaPath (areaColor.withAlpha dims.fillOpacity))
    else cmds

    -- Draw line on top of fill
    let cmds := if dims.showLine && pointCount > 0 then
      Id.run do
        let mut linePath := Arbor.Path.empty
        for i in [0:pointCount] do
          let value := data[i]!
          let x := chartX + i.toFloat * stepX
          let y := chartY + chartHeight - (value / niceMaxVal) * chartHeight
          let pt := Arbor.Point.mk' x y
          if i == 0 then
            linePath := linePath.moveTo pt
          else
            linePath := linePath.lineTo pt
        cmds.push (.strokePath linePath areaColor dims.lineWidth)
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
          cmds := cmds.push (.fillPath markerPath areaColor)
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

/-- Custom spec for multi-series area chart rendering. -/
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
    let baseY := chartY + chartHeight

    -- Draw each series (areas first, then lines on top)
    let cmds := Id.run do
      let mut cmds := cmds
      -- First pass: draw all filled areas
      for si in [0:series.size] do
        let s := series[si]!
        let color := s.color.getD (defaultColors[si % defaultColors.size]!)
        let pointCount := s.values.size

        if pointCount > 0 then
          -- Build area path
          let mut areaPath := Arbor.Path.empty
          let startX := chartX
          areaPath := areaPath.moveTo (Arbor.Point.mk' startX baseY)
          let firstY := chartY + chartHeight - (s.values[0]! / niceMaxVal) * chartHeight
          areaPath := areaPath.lineTo (Arbor.Point.mk' startX firstY)
          for i in [1:pointCount] do
            let value := s.values[i]!
            let x := chartX + i.toFloat * stepX
            let y := chartY + chartHeight - (value / niceMaxVal) * chartHeight
            areaPath := areaPath.lineTo (Arbor.Point.mk' x y)
          let lastX := chartX + (pointCount - 1).toFloat * stepX
          areaPath := areaPath.lineTo (Arbor.Point.mk' lastX baseY)
          areaPath := areaPath.lineTo (Arbor.Point.mk' startX baseY)
          cmds := cmds.push (.fillPath areaPath (color.withAlpha dims.fillOpacity))

      -- Second pass: draw all lines on top
      if dims.showLine then
        for si in [0:series.size] do
          let s := series[si]!
          let color := s.color.getD (defaultColors[si % defaultColors.size]!)
          let pointCount := s.values.size

          if pointCount > 0 then
            let mut linePath := Arbor.Path.empty
            for i in [0:pointCount] do
              let value := s.values[i]!
              let x := chartX + i.toFloat * stepX
              let y := chartY + chartHeight - (value / niceMaxVal) * chartHeight
              let pt := Arbor.Point.mk' x y
              if i == 0 then
                linePath := linePath.moveTo pt
              else
                linePath := linePath.lineTo pt
            cmds := cmds.push (.strokePath linePath color dims.lineWidth)

      -- Third pass: draw markers
      if dims.showMarkers then
        for si in [0:series.size] do
          let s := series[si]!
          let color := s.color.getD (defaultColors[si % defaultColors.size]!)
          let pointCount := s.values.size

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

end AreaChart

/-- Build an area chart visual (WidgetBuilder version).
    - `name`: Widget name for identification
    - `data`: Array of values to display
    - `labels`: Optional labels for each data point
    - `variant`: Color variant for the area
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def areaChartVisual (name : String) (data : Array Float)
    (labels : Array String := #[])
    (variant : AreaChartVariant := .primary) (theme : Theme)
    (dims : AreaChart.Dimensions := AreaChart.defaultDimensions) : WidgetBuilder := do
  let wid ← freshId
  let chart ← custom (AreaChart.areaChartSpec data labels variant theme dims) {
    width := .percent 1.0
    height := .percent 1.0
    flexItem := some (Trellis.FlexItem.growing 1)
  }
  let style : BoxStyle := { width := .percent 1.0, height := .percent 1.0, flexItem := some (Trellis.FlexItem.growing 1) }
  let props : Trellis.FlexContainer := { Trellis.FlexContainer.column 0 with alignItems := .stretch }
  pure (.flex wid (some name) props style #[chart])

/-- Build a multi-series area chart visual (WidgetBuilder version).
    - `name`: Widget name for identification
    - `series`: Array of data series
    - `labels`: Labels for the X-axis
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def multiSeriesAreaChartVisual (name : String) (series : Array AreaChart.Series)
    (labels : Array String := #[]) (theme : Theme)
    (dims : AreaChart.Dimensions := AreaChart.defaultDimensions) : WidgetBuilder := do
  let wid ← freshId
  let chart ← custom (AreaChart.multiSeriesSpec series labels theme dims) {
    width := .percent 1.0
    height := .percent 1.0
    flexItem := some (Trellis.FlexItem.growing 1)
  }
  let style : BoxStyle := { width := .percent 1.0, height := .percent 1.0, flexItem := some (Trellis.FlexItem.growing 1) }
  let props : Trellis.FlexContainer := { Trellis.FlexContainer.column 0 with alignItems := .stretch }
  pure (.flex wid (some name) props style #[chart])

/-! ## Reactive AreaChart Components (FRP-based)

These use WidgetM for declarative composition.
-/

open Reactive Reactive.Host
open Afferent.Canopy.Reactive

/-- AreaChart result - provides access to chart state. -/
structure AreaChartResult where
  /-- The data being displayed. -/
  data : Reactive.Dynamic Spider (Array Float)

/-- Create an area chart component using WidgetM.
    Displays a static area chart with the given data.
    - `data`: Array of values to display
    - `labels`: Optional labels for each data point
    - `theme`: Theme for styling
    - `variant`: Color variant for the area
    - `dims`: Chart dimensions
-/
def areaChart (data : Array Float) (labels : Array String := #[])
    (theme : Theme) (variant : AreaChartVariant := .primary)
    (dims : AreaChart.Dimensions := AreaChart.defaultDimensions)
    : WidgetM AreaChartResult := do
  let name ← registerComponentW "area-chart" (isInteractive := false)

  let dataDyn ← Dynamic.pureM data

  emit do
    pure (areaChartVisual name data labels variant theme dims)

  pure { data := dataDyn }

/-- Create an area chart that updates based on an external event stream.
    Useful for showing real-time data.
    - `initialData`: Initial data values
    - `dataUpdates`: Event stream of data updates
    - `labels`: Labels for each data point
    - `theme`: Theme for styling
    - `variant`: Color variant
    - `dims`: Chart dimensions
-/
def areaChartWithEvents (initialData : Array Float)
    (dataUpdates : Reactive.Event Spider (Array Float))
    (labels : Array String := #[])
    (theme : Theme) (variant : AreaChartVariant := .primary)
    (dims : AreaChart.Dimensions := AreaChart.defaultDimensions)
    : WidgetM AreaChartResult := do
  let name ← registerComponentW "area-chart" (isInteractive := false)

  let dataDyn ← Reactive.holdDyn initialData dataUpdates

  emit do
    let d ← dataDyn.sample
    pure (areaChartVisual name d labels variant theme dims)

  pure { data := dataDyn }

/-- MultiSeriesAreaChart result. -/
structure MultiSeriesAreaChartResult where
  series : Reactive.Dynamic Spider (Array AreaChart.Series)

/-- Create a multi-series area chart for comparing multiple data sets.
    - `series`: Array of data series with values and optional colors/labels
    - `labels`: Labels for the X-axis
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def multiSeriesAreaChart (series : Array AreaChart.Series)
    (labels : Array String := #[]) (theme : Theme)
    (dims : AreaChart.Dimensions := AreaChart.defaultDimensions)
    : WidgetM MultiSeriesAreaChartResult := do
  let name ← registerComponentW "area-chart" (isInteractive := false)

  let seriesDyn ← Dynamic.pureM series

  emit do
    pure (multiSeriesAreaChartVisual name series labels theme dims)

  pure { series := seriesDyn }

end Afferent.Canopy

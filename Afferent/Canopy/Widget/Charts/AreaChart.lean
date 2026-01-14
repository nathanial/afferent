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
    let actualWidth := rect.width
    let actualHeight := rect.height
    let chartX := rect.x + dims.marginLeft
    let chartY := rect.y + dims.marginTop
    let chartWidth := actualWidth - dims.marginLeft - dims.marginRight
    let chartHeight := actualHeight - dims.marginTop - dims.marginBottom
    let maxVal := data.foldl (fun acc v => if v > acc then v else acc) 0.0
    let niceMaxVal := niceMax maxVal
    let pointCount := data.size
    let stepX := if pointCount > 1 then chartWidth / (pointCount - 1).toFloat else 0.0
    let areaColor := variantColor variant theme
    let axisColor := Color.gray 0.5

    RenderM.build do
      -- Background
      RenderM.fillRect (Arbor.Rect.mk' rect.x rect.y actualWidth actualHeight)
        (theme.panel.background.withAlpha 0.3) 6.0

      -- Grid lines
      if dims.showGridLines && dims.gridLineCount > 0 then
        for i in [0:dims.gridLineCount + 1] do
          let ratio := i.toFloat / dims.gridLineCount.toFloat
          let lineY := chartY + chartHeight - (ratio * chartHeight)
          RenderM.fillRect (Arbor.Rect.mk' chartX lineY chartWidth 1.0) (Color.gray 0.3)

      -- Filled area path
      if pointCount > 0 then
        let baseY := chartY + chartHeight
        let areaPath := Id.run do
          let mut path := Arbor.Path.empty
          path := path.moveTo (Arbor.Point.mk' chartX baseY)
          let firstY := chartY + chartHeight - (data[0]! / niceMaxVal) * chartHeight
          path := path.lineTo (Arbor.Point.mk' chartX firstY)
          for i in [1:pointCount] do
            let x := chartX + i.toFloat * stepX
            let y := chartY + chartHeight - (data[i]! / niceMaxVal) * chartHeight
            path := path.lineTo (Arbor.Point.mk' x y)
          let lastX := chartX + (pointCount - 1).toFloat * stepX
          path := path.lineTo (Arbor.Point.mk' lastX baseY)
          path.lineTo (Arbor.Point.mk' chartX baseY)
        RenderM.fillPath areaPath (areaColor.withAlpha dims.fillOpacity)

      -- Line on top of fill
      if dims.showLine && pointCount > 0 then
        let linePath := Id.run do
          let mut path := Arbor.Path.empty
          for i in [0:pointCount] do
            let x := chartX + i.toFloat * stepX
            let y := chartY + chartHeight - (data[i]! / niceMaxVal) * chartHeight
            let pt := Arbor.Point.mk' x y
            path := if i == 0 then path.moveTo pt else path.lineTo pt
          path
        RenderM.strokePath linePath areaColor dims.lineWidth

      -- Markers
      if dims.showMarkers && pointCount > 0 then
        for i in [0:pointCount] do
          let x := chartX + i.toFloat * stepX
          let y := chartY + chartHeight - (data[i]! / niceMaxVal) * chartHeight
          RenderM.fillPath (Arbor.Path.circle (Arbor.Point.mk' x y) dims.markerRadius) areaColor

      -- Y-axis labels
      if dims.gridLineCount > 0 then
        for i in [0:dims.gridLineCount + 1] do
          let ratio := i.toFloat / dims.gridLineCount.toFloat
          let value := ratio * niceMaxVal
          let labelY := chartY + chartHeight - (ratio * chartHeight) - 6
          RenderM.fillText (formatValue value) (rect.x + 4) labelY theme.smallFont theme.textMuted

      -- X-axis labels
      if labels.size > 0 && pointCount > 0 then
        for i in [0:min labels.size pointCount] do
          let labelX := chartX + i.toFloat * stepX
          let labelY := chartY + chartHeight + 16
          RenderM.fillText labels[i]! labelX labelY theme.smallFont theme.text

      -- Axes
      RenderM.fillRect (Arbor.Rect.mk' chartX chartY 1.0 chartHeight) axisColor
      RenderM.fillRect (Arbor.Rect.mk' chartX (chartY + chartHeight) chartWidth 1.0) axisColor

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
    let actualWidth := rect.width
    let actualHeight := rect.height
    let chartX := rect.x + dims.marginLeft
    let chartY := rect.y + dims.marginTop
    let chartWidth := actualWidth - dims.marginLeft - dims.marginRight
    let chartHeight := actualHeight - dims.marginTop - dims.marginBottom
    let maxVal := series.foldl (fun acc s =>
      s.values.foldl (fun acc2 v => if v > acc2 then v else acc2) acc) 0.0
    let niceMaxVal := niceMax maxVal
    let maxPoints := series.foldl (fun acc s => max acc s.values.size) 0
    let stepX := if maxPoints > 1 then chartWidth / (maxPoints - 1).toFloat else 0.0
    let defaultColors := defaultSeriesColors theme
    let baseY := chartY + chartHeight
    let axisColor := Color.gray 0.5

    RenderM.build do
      -- Background
      RenderM.fillRect (Arbor.Rect.mk' rect.x rect.y actualWidth actualHeight)
        (theme.panel.background.withAlpha 0.3) 6.0

      -- Grid lines
      if dims.showGridLines && dims.gridLineCount > 0 then
        for i in [0:dims.gridLineCount + 1] do
          let ratio := i.toFloat / dims.gridLineCount.toFloat
          let lineY := chartY + chartHeight - (ratio * chartHeight)
          RenderM.fillRect (Arbor.Rect.mk' chartX lineY chartWidth 1.0) (Color.gray 0.3)

      -- First pass: draw all filled areas
      for h : si in [0:series.size] do
        let s := series[si]
        let color := s.color.getD (defaultColors[si % defaultColors.size]!)
        let pointCount := s.values.size
        if pointCount > 0 then
          let areaPath := Id.run do
            let mut path := Arbor.Path.empty
            path := path.moveTo (Arbor.Point.mk' chartX baseY)
            let firstY := chartY + chartHeight - (s.values[0]! / niceMaxVal) * chartHeight
            path := path.lineTo (Arbor.Point.mk' chartX firstY)
            for i in [1:pointCount] do
              let x := chartX + i.toFloat * stepX
              let y := chartY + chartHeight - (s.values[i]! / niceMaxVal) * chartHeight
              path := path.lineTo (Arbor.Point.mk' x y)
            let lastX := chartX + (pointCount - 1).toFloat * stepX
            path := path.lineTo (Arbor.Point.mk' lastX baseY)
            path.lineTo (Arbor.Point.mk' chartX baseY)
          RenderM.fillPath areaPath (color.withAlpha dims.fillOpacity)

      -- Second pass: draw all lines on top
      if dims.showLine then
        for h : si in [0:series.size] do
          let s := series[si]
          let color := s.color.getD (defaultColors[si % defaultColors.size]!)
          let pointCount := s.values.size
          if pointCount > 0 then
            let linePath := Id.run do
              let mut path := Arbor.Path.empty
              for i in [0:pointCount] do
                let x := chartX + i.toFloat * stepX
                let y := chartY + chartHeight - (s.values[i]! / niceMaxVal) * chartHeight
                let pt := Arbor.Point.mk' x y
                path := if i == 0 then path.moveTo pt else path.lineTo pt
              path
            RenderM.strokePath linePath color dims.lineWidth

      -- Third pass: draw markers
      if dims.showMarkers then
        for h : si in [0:series.size] do
          let s := series[si]
          let color := s.color.getD (defaultColors[si % defaultColors.size]!)
          for i in [0:s.values.size] do
            let x := chartX + i.toFloat * stepX
            let y := chartY + chartHeight - (s.values[i]! / niceMaxVal) * chartHeight
            RenderM.fillPath (Arbor.Path.circle (Arbor.Point.mk' x y) dims.markerRadius) color

      -- Y-axis labels
      if dims.gridLineCount > 0 then
        for i in [0:dims.gridLineCount + 1] do
          let ratio := i.toFloat / dims.gridLineCount.toFloat
          let value := ratio * niceMaxVal
          let labelY := chartY + chartHeight - (ratio * chartHeight) - 6
          RenderM.fillText (formatValue value) (rect.x + 4) labelY theme.smallFont theme.textMuted

      -- X-axis labels
      if labels.size > 0 && maxPoints > 0 then
        for i in [0:min labels.size maxPoints] do
          let labelX := chartX + i.toFloat * stepX
          let labelY := chartY + chartHeight + 16
          RenderM.fillText labels[i]! labelX labelY theme.smallFont theme.text

      -- Axes
      RenderM.fillRect (Arbor.Rect.mk' chartX chartY 1.0 chartHeight) axisColor
      RenderM.fillRect (Arbor.Rect.mk' chartX (chartY + chartHeight) chartWidth 1.0) axisColor

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

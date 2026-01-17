/-
  Canopy RadarChart Widget
  Spider/radar chart for displaying multivariate data on radial axes.
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

namespace RadarChart

/-- Pi constant for angle calculations. -/
private def pi : Float := 3.14159265358979323846

/-- Dimensions and styling for radar chart rendering. -/
structure Dimensions extends ChartSize, ChartMargins where
  width := 350.0
  height := 350.0
  marginTop := 40.0
  marginBottom := 40.0
  marginLeft := 40.0
  marginRight := 100.0  -- Extra space for legend
  radius : Float := 120.0
  gridLevels : Nat := 5
  showGridPolygons : Bool := true
  showGridLines : Bool := true
  showAxisLabels : Bool := true
  fillOpacity : Float := 0.3
  lineWidth : Float := 2.0
  showMarkers : Bool := true
  markerRadius : Float := 4.0
  showLegend : Bool := true
  legendItemHeight : Float := 16.0
deriving Repr, Inhabited

/-- Default radar chart dimensions. -/
def defaultDimensions : Dimensions := {}

/-- A single data series for radar chart. -/
structure Series where
  /-- Name of the series (shown in legend). -/
  name : String
  /-- Values for each axis (should match number of axes). -/
  values : Array Float
  /-- Color for this series. -/
  color : Option Color := none
deriving Repr, Inhabited, BEq

/-- Radar chart data. -/
structure Data where
  /-- Labels for each axis. -/
  axisLabels : Array String
  /-- Data series to display. -/
  series : Array Series
  /-- Maximum value for scaling (auto-computed if none). -/
  maxValue : Option Float := none
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

/-- Calculate angle for an axis (starting from top, going clockwise). -/
private def axisAngle (axisIdx : Nat) (numAxes : Nat) : Float :=
  let twoPi := 2.0 * pi
  -- Start from top (-π/2) and go clockwise
  (0.0 - pi / 2.0) + (axisIdx.toFloat / numAxes.toFloat) * twoPi

/-- Calculate point position on radar chart. -/
private def pointPosition (centerX centerY : Float) (angle : Float) (distance : Float) : (Float × Float) :=
  let x := centerX + distance * Float.cos angle
  let y := centerY + distance * Float.sin angle
  (x, y)

/-- Find maximum value across all series. -/
private def findMaxValue (data : Data) : Float :=
  match data.maxValue with
  | some v => v
  | none =>
    let maxVal := Id.run do
      let mut maxV : Float := 0.0
      for series in data.series do
        for v in series.values do
          if v > maxV then maxV := v
      maxV
    if maxVal <= 0.0 then 1.0 else maxVal


/-- Custom spec for radar chart rendering. -/
def radarChartSpec (data : Data) (theme : Theme)
    (dims : Dimensions := defaultDimensions) : CustomSpec := {
  measure := fun _ _ => (50, 50)
  collect := fun layout =>
    let rect := layout.contentRect
    let actualWidth := rect.width
    let actualHeight := rect.height

    let numAxes := data.axisLabels.size
    if numAxes < 3 then #[] else  -- Need at least 3 axes for a polygon

    -- Calculate center point and radius from available space
    let legendSpace := if dims.showLegend then dims.marginRight else 20.0
    let chartWidth := actualWidth - dims.marginLeft - legendSpace
    let chartHeight := actualHeight - dims.marginTop - dims.marginBottom
    let centerX := rect.x + dims.marginLeft + chartWidth / 2
    let centerY := rect.y + dims.marginTop + chartHeight / 2
    -- Radius is based on available chart area, leaving room for labels
    let availableRadius := (min chartWidth chartHeight) / 2
    let radius := max 20.0 (availableRadius * 0.75)  -- 75% of available, min 20px

    -- Find max value for scaling
    let maxVal := findMaxValue data
    let niceMaxVal := ChartUtils.niceMax maxVal

    RenderM.build do
      -- Draw background
      RenderM.fillRect' rect.x rect.y actualWidth actualHeight (theme.panel.background.withAlpha 0.3) 6.0

      -- Draw grid polygons (concentric)
      if dims.showGridPolygons && dims.gridLevels > 0 then
        for level in [1:dims.gridLevels + 1] do
          let levelRadius := radius * (level.toFloat / dims.gridLevels.toFloat)
          let mut gridPath := Arbor.Path.empty
          for axisIdx in [0:numAxes] do
            let angle := axisAngle axisIdx numAxes
            let (x, y) := pointPosition centerX centerY angle levelRadius
            if axisIdx == 0 then
              gridPath := gridPath.moveTo (Arbor.Point.mk' x y)
            else
              gridPath := gridPath.lineTo (Arbor.Point.mk' x y)
          -- Close the polygon
          let angle := axisAngle 0 numAxes
          let (x, y) := pointPosition centerX centerY angle levelRadius
          gridPath := gridPath.lineTo (Arbor.Point.mk' x y)
          RenderM.strokePath gridPath (Color.gray 0.3) 1.0

      -- Draw axis lines from center to outer edge
      if dims.showGridLines then
        for axisIdx in [0:numAxes] do
          let angle := axisAngle axisIdx numAxes
          let (outerX, outerY) := pointPosition centerX centerY angle radius
          let mut axisPath := Arbor.Path.empty
          axisPath := axisPath.moveTo (Arbor.Point.mk' centerX centerY)
          axisPath := axisPath.lineTo (Arbor.Point.mk' outerX outerY)
          RenderM.strokePath axisPath (Color.gray 0.4) 1.0

      -- Draw axis labels
      if dims.showAxisLabels then
        let labelDistance := radius + 15.0
        for axisIdx in [0:numAxes] do
          let label := data.axisLabels[axisIdx]!
          let angle := axisAngle axisIdx numAxes
          let (x, y) := pointPosition centerX centerY angle labelDistance
          RenderM.fillText label x (y + 4) theme.smallFont theme.text

      -- Draw data series (filled polygons and lines)
      -- First pass: draw all filled areas
      for seriesIdx in [0:data.series.size] do
        let series := data.series[seriesIdx]!
        let color := getSeriesColor series seriesIdx

        if series.values.size >= numAxes then
          let mut dataPath := Arbor.Path.empty
          for axisIdx in [0:numAxes] do
            let value := series.values[axisIdx]!
            let normalizedValue := value / niceMaxVal
            let distance := radius * normalizedValue
            let angle := axisAngle axisIdx numAxes
            let (x, y) := pointPosition centerX centerY angle distance
            if axisIdx == 0 then
              dataPath := dataPath.moveTo (Arbor.Point.mk' x y)
            else
              dataPath := dataPath.lineTo (Arbor.Point.mk' x y)
          -- Close the polygon
          let value := series.values[0]!
          let normalizedValue := value / niceMaxVal
          let distance := radius * normalizedValue
          let angle := axisAngle 0 numAxes
          let (x, y) := pointPosition centerX centerY angle distance
          dataPath := dataPath.lineTo (Arbor.Point.mk' x y)

          RenderM.fillPath dataPath (color.withAlpha dims.fillOpacity)

      -- Second pass: draw all lines on top
      for seriesIdx in [0:data.series.size] do
        let series := data.series[seriesIdx]!
        let color := getSeriesColor series seriesIdx

        if series.values.size >= numAxes then
          let mut linePath := Arbor.Path.empty
          for axisIdx in [0:numAxes] do
            let value := series.values[axisIdx]!
            let normalizedValue := value / niceMaxVal
            let distance := radius * normalizedValue
            let angle := axisAngle axisIdx numAxes
            let (x, y) := pointPosition centerX centerY angle distance
            if axisIdx == 0 then
              linePath := linePath.moveTo (Arbor.Point.mk' x y)
            else
              linePath := linePath.lineTo (Arbor.Point.mk' x y)
          -- Close the polygon
          let value := series.values[0]!
          let normalizedValue := value / niceMaxVal
          let distance := radius * normalizedValue
          let angle := axisAngle 0 numAxes
          let (x, y) := pointPosition centerX centerY angle distance
          linePath := linePath.lineTo (Arbor.Point.mk' x y)

          RenderM.strokePath linePath color dims.lineWidth

      -- Third pass: draw markers
      if dims.showMarkers then
        for seriesIdx in [0:data.series.size] do
          let series := data.series[seriesIdx]!
          let color := getSeriesColor series seriesIdx

          if series.values.size >= numAxes then
            for axisIdx in [0:numAxes] do
              let value := series.values[axisIdx]!
              let normalizedValue := value / niceMaxVal
              let distance := radius * normalizedValue
              let angle := axisAngle axisIdx numAxes
              let (x, y) := pointPosition centerX centerY angle distance
              -- GPU-batched marker
              RenderM.fillCircle' x y dims.markerRadius color

      -- Draw legend using shared utility
      if dims.showLegend && data.series.size > 0 then
        let legendX := rect.x + dims.marginLeft + chartWidth + 16
        let legendY := rect.y + dims.marginTop
        let legendItems : Array ChartUtils.LegendItem := Id.run do
          let mut items : Array ChartUtils.LegendItem := #[]
          for idx in [0:data.series.size] do
            let series := data.series[idx]!
            let color := getSeriesColor series idx
            items := items.push { label := series.name, color, suffix := none }
          items
        let legendConfig : ChartUtils.LegendConfig := {
          swatchSize := 12.0
          itemHeight := dims.legendItemHeight + 4
          spacing := 4.0
        }
        let _ ← ChartUtils.drawLegend legendItems legendX legendY theme legendConfig

  draw := none
}

end RadarChart

/-- Build a radar chart visual (WidgetBuilder version).
    - `name`: Widget name for identification
    - `data`: Radar chart data with axis labels and series
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def radarChartVisual (name : String) (data : RadarChart.Data)
    (theme : Theme) (dims : RadarChart.Dimensions := RadarChart.defaultDimensions)
    : WidgetBuilder := do
  let wid ← freshId
  let chart ← custom (RadarChart.radarChartSpec data theme dims) {
    width := .percent 1.0
    height := .percent 1.0
    flexItem := some (Trellis.FlexItem.growing 1)
  }
  let style : BoxStyle := { width := .percent 1.0, height := .percent 1.0, minWidth := some dims.width, minHeight := some dims.height, flexItem := some (Trellis.FlexItem.growing 1) }
  let props : Trellis.FlexContainer := { Trellis.FlexContainer.column 0 with alignItems := .stretch }
  pure (.flex wid (some name) props style #[chart])

/-! ## Reactive RadarChart Components (FRP-based)

These use WidgetM for declarative composition.
-/

open Reactive Reactive.Host
open Afferent.Canopy.Reactive

/-- RadarChart result - provides access to chart state. -/
structure RadarChartResult where
  /-- The data being displayed. -/
  data : Dyn RadarChart.Data

/-- Create a radar chart component using WidgetM with dynamic data.
    The chart automatically rebuilds when the data Dynamic changes.
    - `data`: Dynamic radar chart data with axis labels and series
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def radarChart (data : Dyn RadarChart.Data)
    (theme : Theme) (dims : RadarChart.Dimensions := RadarChart.defaultDimensions)
    : WidgetM RadarChartResult := do
  let _ ← dynWidget data fun currentData => do
    let name ← registerComponentW "radar-chart" (isInteractive := false)
    emit do pure (radarChartVisual name currentData theme dims)

  pure { data }

/-- Create a radar chart from dynamic arrays.
    - `axisLabels`: Labels for each axis
    - `seriesNames`: Names for each series (for legend)
    - `seriesData`: Dynamic array of value arrays, one per series
    - `colors`: Optional colors for each series
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def radarChartFromArrays (axisLabels : Array String)
    (seriesNames : Array String) (seriesData : Dyn (Array (Array Float)))
    (colors : Array Color := #[])
    (theme : Theme) (dims : RadarChart.Dimensions := RadarChart.defaultDimensions)
    : WidgetM RadarChartResult := do
  let dataDyn ← Dynamic.mapM (fun currentSeriesData => Id.run do
    let mut result : Array RadarChart.Series := #[]
    for i in [0:seriesNames.size] do
      let name := seriesNames[i]!
      let values := if i < currentSeriesData.size then currentSeriesData[i]! else #[]
      let color := if i < colors.size then some colors[i]! else none
      result := result.push { name, values, color }
    ({ axisLabels, series := result } : RadarChart.Data)
  ) seriesData
  radarChart dataDyn theme dims

/-- Create a single-series radar chart with dynamic values.
    - `axisLabels`: Labels for each axis
    - `values`: Dynamic values for each axis
    - `seriesName`: Name for the series
    - `color`: Optional color for the series
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def radarChartSingle (axisLabels : Array String) (values : Dyn (Array Float))
    (seriesName : String := "Data") (color : Option Color := none)
    (theme : Theme) (dims : RadarChart.Dimensions := RadarChart.defaultDimensions)
    : WidgetM RadarChartResult := do
  let dataDyn ← Dynamic.mapM (fun currentValues =>
    ({ axisLabels, series := #[{ name := seriesName, values := currentValues, color }] } : RadarChart.Data)
  ) values
  radarChart dataDyn theme dims

end Afferent.Canopy

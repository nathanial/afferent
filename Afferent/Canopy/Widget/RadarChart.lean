/-
  Canopy RadarChart Widget
  Spider/radar chart for displaying multivariate data on radial axes.
-/
import Reactive
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Reactive.Component

namespace Afferent.Canopy

open Afferent.Arbor hiding Event

namespace RadarChart

/-- Pi constant for angle calculations. -/
private def pi : Float := 3.14159265358979323846

/-- Dimensions and styling for radar chart rendering. -/
structure Dimensions where
  width : Float := 350.0
  height : Float := 350.0
  marginTop : Float := 40.0
  marginBottom : Float := 40.0
  marginLeft : Float := 40.0
  marginRight : Float := 100.0  -- Extra space for legend
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
deriving Repr, Inhabited

/-- Radar chart data. -/
structure Data where
  /-- Labels for each axis. -/
  axisLabels : Array String
  /-- Data series to display. -/
  series : Array Series
  /-- Maximum value for scaling (auto-computed if none). -/
  maxValue : Option Float := none
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

/-- Calculate nice max value for axis scaling. -/
private def niceMax (maxVal : Float) : Float :=
  if maxVal <= 0.0 then 1.0
  else if maxVal <= 10 then 10.0
  else if maxVal <= 50 then 50.0
  else if maxVal <= 100 then 100.0
  else if maxVal <= 500 then 500.0
  else if maxVal <= 1000 then 1000.0
  else (maxVal / 100).ceil * 100

/-- Custom spec for radar chart rendering. -/
def radarChartSpec (data : Data) (theme : Theme)
    (dims : Dimensions := defaultDimensions) : CustomSpec := {
  measure := fun _ _ => (50, 50)
  collect := fun layout =>
    let rect := layout.contentRect
    let actualWidth := rect.width
    let actualHeight := rect.height
    let cmds : Array RenderCommand := #[]

    let numAxes := data.axisLabels.size
    if numAxes < 3 then cmds else  -- Need at least 3 axes for a polygon

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
    let niceMaxVal := niceMax maxVal

    -- Draw background
    let bgRect := Arbor.Rect.mk' rect.x rect.y actualWidth actualHeight
    let cmds := cmds.push (.fillRect bgRect (theme.panel.background.withAlpha 0.3) 6.0)

    -- Draw grid polygons (concentric)
    let cmds := if dims.showGridPolygons && dims.gridLevels > 0 then
      Id.run do
        let mut cmds := cmds
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
          cmds := cmds.push (.strokePath gridPath (Color.gray 0.3) 1.0)
        cmds
    else cmds

    -- Draw axis lines from center to outer edge
    let cmds := if dims.showGridLines then
      Id.run do
        let mut cmds := cmds
        for axisIdx in [0:numAxes] do
          let angle := axisAngle axisIdx numAxes
          let (outerX, outerY) := pointPosition centerX centerY angle radius
          let mut axisPath := Arbor.Path.empty
          axisPath := axisPath.moveTo (Arbor.Point.mk' centerX centerY)
          axisPath := axisPath.lineTo (Arbor.Point.mk' outerX outerY)
          cmds := cmds.push (.strokePath axisPath (Color.gray 0.4) 1.0)
        cmds
    else cmds

    -- Draw axis labels
    let cmds := if dims.showAxisLabels then
      Id.run do
        let mut cmds := cmds
        let labelDistance := radius + 15.0
        for axisIdx in [0:numAxes] do
          let label := data.axisLabels[axisIdx]!
          let angle := axisAngle axisIdx numAxes
          let (x, y) := pointPosition centerX centerY angle labelDistance
          cmds := cmds.push (.fillText label x (y + 4) theme.smallFont theme.text)
        cmds
    else cmds

    -- Draw data series (filled polygons and lines)
    let cmds := Id.run do
      let mut cmds := cmds

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

          cmds := cmds.push (.fillPath dataPath (color.withAlpha dims.fillOpacity))

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

          cmds := cmds.push (.strokePath linePath color dims.lineWidth)

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
              let markerPath := Arbor.Path.circle (Arbor.Point.mk' x y) dims.markerRadius
              cmds := cmds.push (.fillPath markerPath color)

      cmds

    -- Draw legend
    let cmds := if dims.showLegend && data.series.size > 0 then
      Id.run do
        let mut cmds := cmds
        let legendX := rect.x + dims.marginLeft + chartWidth + 16
        let legendY := rect.y + dims.marginTop
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
  let style : BoxStyle := { width := .percent 1.0, height := .percent 1.0, flexItem := some (Trellis.FlexItem.growing 1) }
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
  data : Reactive.Dynamic Spider RadarChart.Data

/-- Create a radar chart component using WidgetM.
    - `data`: Radar chart data with axis labels and series
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def radarChart (data : RadarChart.Data)
    (theme : Theme) (dims : RadarChart.Dimensions := RadarChart.defaultDimensions)
    : WidgetM RadarChartResult := do
  let name ← registerComponentW "radar-chart" (isInteractive := false)

  let dataDyn ← Dynamic.pureM data

  emit do
    pure (radarChartVisual name data theme dims)

  pure { data := dataDyn }

/-- Create a radar chart from simple arrays.
    - `axisLabels`: Labels for each axis
    - `seriesNames`: Names for each series (for legend)
    - `seriesData`: Array of value arrays, one per series
    - `colors`: Optional colors for each series
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def radarChartFromArrays (axisLabels : Array String)
    (seriesNames : Array String) (seriesData : Array (Array Float))
    (colors : Array Color := #[])
    (theme : Theme) (dims : RadarChart.Dimensions := RadarChart.defaultDimensions)
    : WidgetM RadarChartResult := do
  let series := Id.run do
    let mut result : Array RadarChart.Series := #[]
    for i in [0:seriesNames.size] do
      let name := seriesNames[i]!
      let values := if i < seriesData.size then seriesData[i]! else #[]
      let color := if i < colors.size then some colors[i]! else none
      result := result.push { name, values, color }
    result
  let data : RadarChart.Data := { axisLabels, series }
  radarChart data theme dims

/-- Create a single-series radar chart.
    - `axisLabels`: Labels for each axis
    - `values`: Values for each axis
    - `seriesName`: Name for the series
    - `color`: Optional color for the series
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def radarChartSingle (axisLabels : Array String) (values : Array Float)
    (seriesName : String := "Data") (color : Option Color := none)
    (theme : Theme) (dims : RadarChart.Dimensions := RadarChart.defaultDimensions)
    : WidgetM RadarChartResult := do
  let series : Array RadarChart.Series := #[{ name := seriesName, values, color }]
  let data : RadarChart.Data := { axisLabels, series }
  radarChart data theme dims

/-- Create a radar chart that updates based on an external event stream.
    - `initialData`: Initial chart data
    - `dataUpdates`: Event stream of data updates
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def radarChartWithEvents (initialData : RadarChart.Data)
    (dataUpdates : Reactive.Event Spider RadarChart.Data)
    (theme : Theme) (dims : RadarChart.Dimensions := RadarChart.defaultDimensions)
    : WidgetM RadarChartResult := do
  let name ← registerComponentW "radar-chart" (isInteractive := false)

  let dataDyn ← Reactive.holdDyn initialData dataUpdates

  emit do
    let d ← dataDyn.sample
    pure (radarChartVisual name d theme dims)

  pure { data := dataDyn }

end Afferent.Canopy

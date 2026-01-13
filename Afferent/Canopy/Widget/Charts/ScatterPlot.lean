/-
  Canopy ScatterPlot Widget
  Scatter plot for showing correlations between X/Y data points.
-/
import Reactive
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Reactive.Component

namespace Afferent.Canopy

open Afferent.Arbor hiding Event

namespace ScatterPlot

/-- Dimensions and styling for scatter plot rendering. -/
structure Dimensions where
  width : Float := 400.0
  height : Float := 300.0
  marginTop : Float := 20.0
  marginBottom : Float := 40.0
  marginLeft : Float := 50.0
  marginRight : Float := 20.0
  pointRadius : Float := 5.0
  showGridLines : Bool := true
  gridLineCount : Nat := 5
  showAxisLabels : Bool := true
deriving Repr, Inhabited

/-- Default scatter plot dimensions. -/
def defaultDimensions : Dimensions := {}

/-- A single data point with X and Y coordinates. -/
structure DataPoint where
  x : Float
  y : Float
deriving Repr, Inhabited, BEq

/-- A data series for multi-series scatter plots. -/
structure Series where
  points : Array DataPoint
  color : Option Color := none
  label : Option String := none
  pointRadius : Option Float := none
deriving Repr, Inhabited

/-- Default colors for scatter plot series. -/
def defaultColors (theme : Theme) : Array Color := #[
  theme.primary.background,
  theme.secondary.background,
  Color.rgba 0.2 0.8 0.3 1.0,
  Color.rgba 1.0 0.7 0.0 1.0,
  Color.rgba 0.9 0.2 0.2 1.0,
  Color.rgba 0.5 0.3 0.9 1.0,
  Color.rgba 0.0 0.7 0.7 1.0
]

/-- Format a float value for axis labels. -/
private def formatValue (v : Float) : String :=
  if v >= 1000000 then
    s!"{(v / 1000000).floor.toUInt32}M"
  else if v >= 1000 then
    s!"{(v / 1000).floor.toUInt32}K"
  else if v == v.floor then
    s!"{v.floor.toInt32}"
  else
    let whole := v.floor.toInt32
    let frac := ((v - v.floor).abs * 10).floor.toUInt32
    s!"{whole}.{frac}"

/-- Calculate nice axis bounds (min, max) for scaling. -/
private def niceAxisBounds (minVal maxVal : Float) : Float × Float :=
  let range := maxVal - minVal
  if range <= 0.0 then (minVal - 1.0, maxVal + 1.0)
  else
    -- Add 10% padding
    let padding := range * 0.1
    let niceMin := if minVal >= 0.0 then 0.0 else minVal - padding
    let niceMax := maxVal + padding
    (niceMin, niceMax)

/-- Custom spec for single-series scatter plot rendering. -/
def scatterPlotSpec (points : Array DataPoint) (theme : Theme)
    (dims : Dimensions := defaultDimensions) : CustomSpec := {
  measure := fun _ _ => (dims.marginLeft + dims.marginRight + 50, dims.marginTop + dims.marginBottom + 30)
  collect := fun layout =>
    let rect := layout.contentRect

    -- Use actual container size for responsive layout
    let actualWidth := rect.width
    let actualHeight := rect.height

    -- Calculate chart area
    let chartX := rect.x + dims.marginLeft
    let chartY := rect.y + dims.marginTop
    let chartWidth := actualWidth - dims.marginLeft - dims.marginRight
    let chartHeight := actualHeight - dims.marginTop - dims.marginBottom

    -- Find data bounds
    let (minX, maxX, minY, maxY) := Id.run do
      if points.isEmpty then
        (0.0, 1.0, 0.0, 1.0)
      else
        let first := points[0]!
        let mut minX := first.x
        let mut maxX := first.x
        let mut minY := first.y
        let mut maxY := first.y
        for p in points do
          if p.x < minX then minX := p.x
          if p.x > maxX then maxX := p.x
          if p.y < minY then minY := p.y
          if p.y > maxY then maxY := p.y
        (minX, maxX, minY, maxY)

    let (niceMinX, niceMaxX) := niceAxisBounds minX maxX
    let (niceMinY, niceMaxY) := niceAxisBounds minY maxY
    let rangeX := niceMaxX - niceMinX
    let rangeY := niceMaxY - niceMinY

    let pointColor := theme.primary.background
    let axisColor := Color.gray 0.5

    RenderM.build do
      -- Draw background
      RenderM.fillRect' rect.x rect.y actualWidth actualHeight (theme.panel.background.withAlpha 0.3) 6.0

      -- Draw grid lines
      if dims.showGridLines && dims.gridLineCount > 0 then
        -- Horizontal grid lines
        for i in [0:dims.gridLineCount + 1] do
          let ratio := i.toFloat / dims.gridLineCount.toFloat
          let lineY := chartY + chartHeight - (ratio * chartHeight)
          RenderM.fillRect' chartX lineY chartWidth 1.0 (Color.gray 0.3) 0.0
        -- Vertical grid lines
        for i in [0:dims.gridLineCount + 1] do
          let ratio := i.toFloat / dims.gridLineCount.toFloat
          let lineX := chartX + (ratio * chartWidth)
          RenderM.fillRect' lineX chartY 1.0 chartHeight (Color.gray 0.3) 0.0

      -- Draw data points
      for p in points do
        let px := chartX + ((p.x - niceMinX) / rangeX) * chartWidth
        let py := chartY + chartHeight - ((p.y - niceMinY) / rangeY) * chartHeight
        let pointPath := Arbor.Path.circle (Arbor.Point.mk' px py) dims.pointRadius
        RenderM.fillPath pointPath pointColor

      -- Draw Y-axis labels
      if dims.showAxisLabels && dims.gridLineCount > 0 then
        for i in [0:dims.gridLineCount + 1] do
          let ratio := i.toFloat / dims.gridLineCount.toFloat
          let value := niceMinY + ratio * rangeY
          let labelY := chartY + chartHeight - (ratio * chartHeight) - 6
          let labelText := formatValue value
          RenderM.fillText labelText (rect.x + 4) labelY theme.smallFont theme.textMuted

      -- Draw X-axis labels
      if dims.showAxisLabels && dims.gridLineCount > 0 then
        for i in [0:dims.gridLineCount + 1] do
          let ratio := i.toFloat / dims.gridLineCount.toFloat
          let value := niceMinX + ratio * rangeX
          let labelX := chartX + (ratio * chartWidth)
          let labelY := chartY + chartHeight + 16
          let labelText := formatValue value
          RenderM.fillText labelText labelX labelY theme.smallFont theme.textMuted

      -- Draw axes
      RenderM.fillRect' chartX chartY 1.0 chartHeight axisColor 0.0
      RenderM.fillRect' chartX (chartY + chartHeight) chartWidth 1.0 axisColor 0.0

  draw := none
}

/-- Custom spec for multi-series scatter plot rendering. -/
def multiSeriesSpec (series : Array Series) (theme : Theme)
    (dims : Dimensions := defaultDimensions) : CustomSpec := {
  measure := fun _ _ => (dims.marginLeft + dims.marginRight + 50, dims.marginTop + dims.marginBottom + 30)
  collect := fun layout =>
    let rect := layout.contentRect

    -- Use actual container size for responsive layout
    let actualWidth := rect.width
    let actualHeight := rect.height

    let chartX := rect.x + dims.marginLeft
    let chartY := rect.y + dims.marginTop
    let chartWidth := actualWidth - dims.marginLeft - dims.marginRight
    let chartHeight := actualHeight - dims.marginTop - dims.marginBottom

    -- Find global data bounds across all series
    let (minX, maxX, minY, maxY) := Id.run do
      -- Find first point to initialize bounds
      let mut initialized := false
      let mut minX : Float := 0.0
      let mut maxX : Float := 1.0
      let mut minY : Float := 0.0
      let mut maxY : Float := 1.0
      for s in series do
        for p in s.points do
          if !initialized then
            minX := p.x; maxX := p.x
            minY := p.y; maxY := p.y
            initialized := true
          else
            if p.x < minX then minX := p.x
            if p.x > maxX then maxX := p.x
            if p.y < minY then minY := p.y
            if p.y > maxY then maxY := p.y
      (minX, maxX, minY, maxY)

    let (niceMinX, niceMaxX) := niceAxisBounds minX maxX
    let (niceMinY, niceMaxY) := niceAxisBounds minY maxY
    let rangeX := niceMaxX - niceMinX
    let rangeY := niceMaxY - niceMinY

    let colors := defaultColors theme
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
        for i in [0:dims.gridLineCount + 1] do
          let ratio := i.toFloat / dims.gridLineCount.toFloat
          let lineX := chartX + (ratio * chartWidth)
          RenderM.fillRect' lineX chartY 1.0 chartHeight (Color.gray 0.3) 0.0

      -- Draw data points for each series
      for si in [0:series.size] do
        let s := series[si]!
        let color := s.color.getD (colors[si % colors.size]!)
        let radius := s.pointRadius.getD dims.pointRadius

        for p in s.points do
          let px := chartX + ((p.x - niceMinX) / rangeX) * chartWidth
          let py := chartY + chartHeight - ((p.y - niceMinY) / rangeY) * chartHeight
          let pointPath := Arbor.Path.circle (Arbor.Point.mk' px py) radius
          RenderM.fillPath pointPath color

      -- Draw Y-axis labels
      if dims.showAxisLabels && dims.gridLineCount > 0 then
        for i in [0:dims.gridLineCount + 1] do
          let ratio := i.toFloat / dims.gridLineCount.toFloat
          let value := niceMinY + ratio * rangeY
          let labelY := chartY + chartHeight - (ratio * chartHeight) - 6
          let labelText := formatValue value
          RenderM.fillText labelText (rect.x + 4) labelY theme.smallFont theme.textMuted

      -- Draw X-axis labels
      if dims.showAxisLabels && dims.gridLineCount > 0 then
        for i in [0:dims.gridLineCount + 1] do
          let ratio := i.toFloat / dims.gridLineCount.toFloat
          let value := niceMinX + ratio * rangeX
          let labelX := chartX + (ratio * chartWidth)
          let labelY := chartY + chartHeight + 16
          let labelText := formatValue value
          RenderM.fillText labelText labelX labelY theme.smallFont theme.textMuted

      -- Draw axes
      RenderM.fillRect' chartX chartY 1.0 chartHeight axisColor 0.0
      RenderM.fillRect' chartX (chartY + chartHeight) chartWidth 1.0 axisColor 0.0

  draw := none
}

end ScatterPlot

/-- Build a scatter plot visual (WidgetBuilder version).
    - `name`: Widget name for identification
    - `points`: Array of X/Y data points
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def scatterPlotVisual (name : String) (points : Array ScatterPlot.DataPoint)
    (theme : Theme) (dims : ScatterPlot.Dimensions := ScatterPlot.defaultDimensions)
    : WidgetBuilder := do
  let wid ← freshId
  let chart ← custom (ScatterPlot.scatterPlotSpec points theme dims) {
    width := .percent 1.0
    height := .percent 1.0
    flexItem := some (Trellis.FlexItem.growing 1)
  }
  let style : BoxStyle := { width := .percent 1.0, height := .percent 1.0, flexItem := some (Trellis.FlexItem.growing 1) }
  let props : Trellis.FlexContainer := { Trellis.FlexContainer.column 0 with alignItems := .stretch }
  pure (.flex wid (some name) props style #[chart])

/-- Build a multi-series scatter plot visual (WidgetBuilder version).
    - `name`: Widget name for identification
    - `series`: Array of data series
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def multiSeriesScatterPlotVisual (name : String) (series : Array ScatterPlot.Series)
    (theme : Theme) (dims : ScatterPlot.Dimensions := ScatterPlot.defaultDimensions)
    : WidgetBuilder := do
  let wid ← freshId
  let chart ← custom (ScatterPlot.multiSeriesSpec series theme dims) {
    width := .percent 1.0
    height := .percent 1.0
    flexItem := some (Trellis.FlexItem.growing 1)
  }
  let style : BoxStyle := { width := .percent 1.0, height := .percent 1.0, flexItem := some (Trellis.FlexItem.growing 1) }
  let props : Trellis.FlexContainer := { Trellis.FlexContainer.column 0 with alignItems := .stretch }
  pure (.flex wid (some name) props style #[chart])

/-! ## Reactive ScatterPlot Components (FRP-based)

These use WidgetM for declarative composition.
-/

open Reactive Reactive.Host
open Afferent.Canopy.Reactive

/-- ScatterPlot result - provides access to chart state. -/
structure ScatterPlotResult where
  /-- The points being displayed. -/
  points : Reactive.Dynamic Spider (Array ScatterPlot.DataPoint)

/-- Create a scatter plot component using WidgetM.
    Displays a static scatter plot with the given data points.
    - `points`: Array of X/Y data points
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def scatterPlot (points : Array ScatterPlot.DataPoint)
    (theme : Theme) (dims : ScatterPlot.Dimensions := ScatterPlot.defaultDimensions)
    : WidgetM ScatterPlotResult := do
  let name ← registerComponentW "scatter-plot" (isInteractive := false)

  let pointsDyn ← Dynamic.pureM points

  emit do
    pure (scatterPlotVisual name points theme dims)

  pure { points := pointsDyn }

/-- MultiSeriesScatterPlot result. -/
structure MultiSeriesScatterPlotResult where
  series : Reactive.Dynamic Spider (Array ScatterPlot.Series)

/-- Create a multi-series scatter plot for comparing multiple data sets.
    - `series`: Array of data series with points and optional colors/labels
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def multiSeriesScatterPlot (series : Array ScatterPlot.Series)
    (theme : Theme) (dims : ScatterPlot.Dimensions := ScatterPlot.defaultDimensions)
    : WidgetM MultiSeriesScatterPlotResult := do
  let name ← registerComponentW "scatter-plot" (isInteractive := false)

  let seriesDyn ← Dynamic.pureM series

  emit do
    pure (multiSeriesScatterPlotVisual name series theme dims)

  pure { series := seriesDyn }

/-- Create a scatter plot that updates based on an external event stream.
    - `initialPoints`: Initial data points
    - `pointUpdates`: Event stream of point updates
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def scatterPlotWithEvents (initialPoints : Array ScatterPlot.DataPoint)
    (pointUpdates : Reactive.Event Spider (Array ScatterPlot.DataPoint))
    (theme : Theme) (dims : ScatterPlot.Dimensions := ScatterPlot.defaultDimensions)
    : WidgetM ScatterPlotResult := do
  let name ← registerComponentW "scatter-plot" (isInteractive := false)

  let pointsDyn ← Reactive.holdDyn initialPoints pointUpdates

  emit do
    let p ← pointsDyn.sample
    pure (scatterPlotVisual name p theme dims)

  pure { points := pointsDyn }

/-- Helper to create data points from X/Y pairs. -/
def ScatterPlot.DataPoint.fromPairs (pairs : Array (Float × Float)) : Array ScatterPlot.DataPoint :=
  pairs.map fun (x, y) => { x, y }

end Afferent.Canopy

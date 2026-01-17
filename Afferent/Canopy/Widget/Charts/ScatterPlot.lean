/-
  Canopy ScatterPlot Widget
  Scatter plot for showing correlations between X/Y data points.
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

namespace ScatterPlot

/-- Dimensions and styling for scatter plot rendering. -/
structure Dimensions extends AxisChartDimensions where
  height := 300.0
  pointRadius : Float := 5.0
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

    let (niceMinX, niceMaxX) := ChartUtils.niceAxisBounds minX maxX
    let (niceMinY, niceMaxY) := ChartUtils.niceAxisBounds minY maxY
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

      -- Draw data points (GPU-batched)
      for p in points do
        let px := chartX + ((p.x - niceMinX) / rangeX) * chartWidth
        let py := chartY + chartHeight - ((p.y - niceMinY) / rangeY) * chartHeight
        RenderM.fillCircle' px py dims.pointRadius pointColor

      -- Draw Y-axis labels
      if dims.showAxisLabels && dims.gridLineCount > 0 then
        for i in [0:dims.gridLineCount + 1] do
          let ratio := i.toFloat / dims.gridLineCount.toFloat
          let value := niceMinY + ratio * rangeY
          let labelY := chartY + chartHeight - (ratio * chartHeight) - 6
          let labelText := ChartUtils.formatValue value
          RenderM.fillText labelText (rect.x + 4) labelY theme.smallFont theme.textMuted

      -- Draw X-axis labels
      if dims.showAxisLabels && dims.gridLineCount > 0 then
        for i in [0:dims.gridLineCount + 1] do
          let ratio := i.toFloat / dims.gridLineCount.toFloat
          let value := niceMinX + ratio * rangeX
          let labelX := chartX + (ratio * chartWidth)
          let labelY := chartY + chartHeight + 16
          let labelText := ChartUtils.formatValue value
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

    let (niceMinX, niceMaxX) := ChartUtils.niceAxisBounds minX maxX
    let (niceMinY, niceMaxY) := ChartUtils.niceAxisBounds minY maxY
    let rangeX := niceMaxX - niceMinX
    let rangeY := niceMaxY - niceMinY

    let colors := ChartUtils.defaultColors theme
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

      -- Draw data points for each series (GPU-batched)
      for si in [0:series.size] do
        let s := series[si]!
        let color := s.color.getD (colors[si % colors.size]!)
        let radius := s.pointRadius.getD dims.pointRadius

        for p in s.points do
          let px := chartX + ((p.x - niceMinX) / rangeX) * chartWidth
          let py := chartY + chartHeight - ((p.y - niceMinY) / rangeY) * chartHeight
          RenderM.fillCircle' px py radius color

      -- Draw Y-axis labels
      if dims.showAxisLabels && dims.gridLineCount > 0 then
        for i in [0:dims.gridLineCount + 1] do
          let ratio := i.toFloat / dims.gridLineCount.toFloat
          let value := niceMinY + ratio * rangeY
          let labelY := chartY + chartHeight - (ratio * chartHeight) - 6
          let labelText := ChartUtils.formatValue value
          RenderM.fillText labelText (rect.x + 4) labelY theme.smallFont theme.textMuted

      -- Draw X-axis labels
      if dims.showAxisLabels && dims.gridLineCount > 0 then
        for i in [0:dims.gridLineCount + 1] do
          let ratio := i.toFloat / dims.gridLineCount.toFloat
          let value := niceMinX + ratio * rangeX
          let labelX := chartX + (ratio * chartWidth)
          let labelY := chartY + chartHeight + 16
          let labelText := ChartUtils.formatValue value
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
  let style : BoxStyle := { width := .percent 1.0, height := .percent 1.0, minWidth := some dims.width, minHeight := some dims.height, flexItem := some (Trellis.FlexItem.growing 1) }
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
  let style : BoxStyle := { width := .percent 1.0, height := .percent 1.0, minWidth := some dims.width, minHeight := some dims.height, flexItem := some (Trellis.FlexItem.growing 1) }
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

/-- Helper to create data points from X/Y pairs. -/
def ScatterPlot.DataPoint.fromPairs (pairs : Array (Float × Float)) : Array ScatterPlot.DataPoint :=
  pairs.map fun (x, y) => { x, y }

end Afferent.Canopy

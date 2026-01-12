/-
  Canopy BubbleChart Widget
  Bubble chart - scatter plot with variable point sizes representing a third dimension.
-/
import Reactive
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Reactive.Component

namespace Afferent.Canopy

open Afferent.Arbor hiding Event

namespace BubbleChart

/-- Dimensions and styling for bubble chart rendering. -/
structure Dimensions where
  width : Float := 400.0
  height : Float := 300.0
  marginTop : Float := 20.0
  marginBottom : Float := 40.0
  marginLeft : Float := 50.0
  marginRight : Float := 20.0
  minBubbleRadius : Float := 4.0
  maxBubbleRadius : Float := 30.0
  bubbleOpacity : Float := 0.7
  showGridLines : Bool := true
  gridLineCount : Nat := 5
  showAxisLabels : Bool := true
  showBubbleLabels : Bool := false
deriving Repr, Inhabited

/-- Default bubble chart dimensions. -/
def defaultDimensions : Dimensions := {}

/-- A single bubble data point with X, Y, and size value. -/
structure DataPoint where
  x : Float
  y : Float
  size : Float
  label : Option String := none
  color : Option Color := none
deriving Repr, Inhabited, BEq

/-- A data series for multi-series bubble charts. -/
structure Series where
  points : Array DataPoint
  color : Option Color := none
  label : Option String := none
deriving Repr, Inhabited

/-- Default colors for bubble chart series. -/
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

/-- Map a size value to a bubble radius based on min/max size values. -/
private def sizeToRadius (size minSize maxSize : Float) (dims : Dimensions) : Float :=
  let sizeRange := maxSize - minSize
  if sizeRange <= 0.0 then
    (dims.minBubbleRadius + dims.maxBubbleRadius) / 2
  else
    let normalized := (size - minSize) / sizeRange
    -- Use area scaling (sqrt) for perceptually accurate size representation
    let sqrtNorm := Float.sqrt normalized
    dims.minBubbleRadius + sqrtNorm * (dims.maxBubbleRadius - dims.minBubbleRadius)

/-- Custom spec for single-series bubble chart rendering. -/
def bubbleChartSpec (points : Array DataPoint) (theme : Theme)
    (dims : Dimensions := defaultDimensions) : CustomSpec := {
  measure := fun _ _ => (dims.width, dims.height)
  collect := fun layout =>
    let rect := layout.contentRect
    let cmds : Array RenderCommand := #[]

    -- Calculate chart area
    let chartX := rect.x + dims.marginLeft
    let chartY := rect.y + dims.marginTop
    let chartWidth := dims.width - dims.marginLeft - dims.marginRight
    let chartHeight := dims.height - dims.marginTop - dims.marginBottom

    -- Find data bounds
    let (minX, maxX, minY, maxY, minSize, maxSize) := Id.run do
      if points.isEmpty then
        (0.0, 1.0, 0.0, 1.0, 0.0, 1.0)
      else
        let first := points[0]!
        let mut minX := first.x
        let mut maxX := first.x
        let mut minY := first.y
        let mut maxY := first.y
        let mut minSize := first.size
        let mut maxSize := first.size
        for p in points do
          if p.x < minX then minX := p.x
          if p.x > maxX then maxX := p.x
          if p.y < minY then minY := p.y
          if p.y > maxY then maxY := p.y
          if p.size < minSize then minSize := p.size
          if p.size > maxSize then maxSize := p.size
        (minX, maxX, minY, maxY, minSize, maxSize)

    let (niceMinX, niceMaxX) := niceAxisBounds minX maxX
    let (niceMinY, niceMaxY) := niceAxisBounds minY maxY
    let rangeX := niceMaxX - niceMinX
    let rangeY := niceMaxY - niceMinY

    -- Draw background
    let bgRect := Arbor.Rect.mk' rect.x rect.y dims.width dims.height
    let cmds := cmds.push (.fillRect bgRect (theme.panel.background.withAlpha 0.3) 6.0)

    -- Draw grid lines
    let cmds := if dims.showGridLines && dims.gridLineCount > 0 then
      Id.run do
        let mut cmds := cmds
        -- Horizontal grid lines
        for i in [0:dims.gridLineCount + 1] do
          let ratio := i.toFloat / dims.gridLineCount.toFloat
          let lineY := chartY + chartHeight - (ratio * chartHeight)
          let lineRect := Arbor.Rect.mk' chartX lineY chartWidth 1.0
          cmds := cmds.push (.fillRect lineRect (Color.gray 0.3) 0.0)
        -- Vertical grid lines
        for i in [0:dims.gridLineCount + 1] do
          let ratio := i.toFloat / dims.gridLineCount.toFloat
          let lineX := chartX + (ratio * chartWidth)
          let lineRect := Arbor.Rect.mk' lineX chartY 1.0 chartHeight
          cmds := cmds.push (.fillRect lineRect (Color.gray 0.3) 0.0)
        cmds
    else cmds

    -- Draw bubbles
    let colors := defaultColors theme
    let cmds := Id.run do
      let mut cmds := cmds
      for i in [0:points.size] do
        let p := points[i]!
        let px := chartX + ((p.x - niceMinX) / rangeX) * chartWidth
        let py := chartY + chartHeight - ((p.y - niceMinY) / rangeY) * chartHeight
        let radius := sizeToRadius p.size minSize maxSize dims
        let color := match p.color with
          | some c => c.withAlpha dims.bubbleOpacity
          | none => (colors[i % colors.size]!).withAlpha dims.bubbleOpacity
        let bubblePath := Arbor.Path.circle (Arbor.Point.mk' px py) radius
        cmds := cmds.push (.fillPath bubblePath color)
        -- Optionally add a stroke for visibility
        cmds := cmds.push (.strokePath bubblePath (color.withAlpha 1.0) 1.5)
      cmds

    -- Draw bubble labels if enabled
    let cmds := if dims.showBubbleLabels then
      Id.run do
        let mut cmds := cmds
        for p in points do
          match p.label with
          | some label =>
            let px := chartX + ((p.x - niceMinX) / rangeX) * chartWidth
            let py := chartY + chartHeight - ((p.y - niceMinY) / rangeY) * chartHeight
            cmds := cmds.push (.fillText label px (py - 4) theme.smallFont theme.text)
          | none => pure ()
        cmds
    else cmds

    -- Draw Y-axis labels
    let cmds := if dims.showAxisLabels && dims.gridLineCount > 0 then
      Id.run do
        let mut cmds := cmds
        for i in [0:dims.gridLineCount + 1] do
          let ratio := i.toFloat / dims.gridLineCount.toFloat
          let value := niceMinY + ratio * rangeY
          let labelY := chartY + chartHeight - (ratio * chartHeight) - 6
          let labelText := formatValue value
          cmds := cmds.push (.fillText labelText (rect.x + 4) labelY theme.smallFont theme.textMuted)
        cmds
    else cmds

    -- Draw X-axis labels
    let cmds := if dims.showAxisLabels && dims.gridLineCount > 0 then
      Id.run do
        let mut cmds := cmds
        for i in [0:dims.gridLineCount + 1] do
          let ratio := i.toFloat / dims.gridLineCount.toFloat
          let value := niceMinX + ratio * rangeX
          let labelX := chartX + (ratio * chartWidth)
          let labelY := chartY + chartHeight + 16
          let labelText := formatValue value
          cmds := cmds.push (.fillText labelText labelX labelY theme.smallFont theme.textMuted)
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

/-- Custom spec for multi-series bubble chart rendering. -/
def multiSeriesSpec (series : Array Series) (theme : Theme)
    (dims : Dimensions := defaultDimensions) : CustomSpec := {
  measure := fun _ _ => (dims.width, dims.height)
  collect := fun layout =>
    let rect := layout.contentRect
    let cmds : Array RenderCommand := #[]

    let chartX := rect.x + dims.marginLeft
    let chartY := rect.y + dims.marginTop
    let chartWidth := dims.width - dims.marginLeft - dims.marginRight
    let chartHeight := dims.height - dims.marginTop - dims.marginBottom

    -- Find global data bounds across all series
    let (minX, maxX, minY, maxY, minSize, maxSize) := Id.run do
      let mut initialized := false
      let mut minX : Float := 0.0
      let mut maxX : Float := 1.0
      let mut minY : Float := 0.0
      let mut maxY : Float := 1.0
      let mut minSize : Float := 0.0
      let mut maxSize : Float := 1.0
      for s in series do
        for p in s.points do
          if !initialized then
            minX := p.x; maxX := p.x
            minY := p.y; maxY := p.y
            minSize := p.size; maxSize := p.size
            initialized := true
          else
            if p.x < minX then minX := p.x
            if p.x > maxX then maxX := p.x
            if p.y < minY then minY := p.y
            if p.y > maxY then maxY := p.y
            if p.size < minSize then minSize := p.size
            if p.size > maxSize then maxSize := p.size
      (minX, maxX, minY, maxY, minSize, maxSize)

    let (niceMinX, niceMaxX) := niceAxisBounds minX maxX
    let (niceMinY, niceMaxY) := niceAxisBounds minY maxY
    let rangeX := niceMaxX - niceMinX
    let rangeY := niceMaxY - niceMinY

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
        for i in [0:dims.gridLineCount + 1] do
          let ratio := i.toFloat / dims.gridLineCount.toFloat
          let lineX := chartX + (ratio * chartWidth)
          let lineRect := Arbor.Rect.mk' lineX chartY 1.0 chartHeight
          cmds := cmds.push (.fillRect lineRect (Color.gray 0.3) 0.0)
        cmds
    else cmds

    let colors := defaultColors theme

    -- Draw bubbles for each series
    let cmds := Id.run do
      let mut cmds := cmds
      for si in [0:series.size] do
        let s := series[si]!
        let seriesColor := s.color.getD (colors[si % colors.size]!)

        for p in s.points do
          let px := chartX + ((p.x - niceMinX) / rangeX) * chartWidth
          let py := chartY + chartHeight - ((p.y - niceMinY) / rangeY) * chartHeight
          let radius := sizeToRadius p.size minSize maxSize dims
          let color := (p.color.getD seriesColor).withAlpha dims.bubbleOpacity
          let bubblePath := Arbor.Path.circle (Arbor.Point.mk' px py) radius
          cmds := cmds.push (.fillPath bubblePath color)
          cmds := cmds.push (.strokePath bubblePath (color.withAlpha 1.0) 1.5)
      cmds

    -- Draw Y-axis labels
    let cmds := if dims.showAxisLabels && dims.gridLineCount > 0 then
      Id.run do
        let mut cmds := cmds
        for i in [0:dims.gridLineCount + 1] do
          let ratio := i.toFloat / dims.gridLineCount.toFloat
          let value := niceMinY + ratio * rangeY
          let labelY := chartY + chartHeight - (ratio * chartHeight) - 6
          let labelText := formatValue value
          cmds := cmds.push (.fillText labelText (rect.x + 4) labelY theme.smallFont theme.textMuted)
        cmds
    else cmds

    -- Draw X-axis labels
    let cmds := if dims.showAxisLabels && dims.gridLineCount > 0 then
      Id.run do
        let mut cmds := cmds
        for i in [0:dims.gridLineCount + 1] do
          let ratio := i.toFloat / dims.gridLineCount.toFloat
          let value := niceMinX + ratio * rangeX
          let labelX := chartX + (ratio * chartWidth)
          let labelY := chartY + chartHeight + 16
          let labelText := formatValue value
          cmds := cmds.push (.fillText labelText labelX labelY theme.smallFont theme.textMuted)
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

/-- Custom spec for bubble chart with legend. -/
def bubbleChartWithLegendSpec (series : Array Series) (theme : Theme)
    (dims : Dimensions := defaultDimensions) : CustomSpec := {
  measure := fun _ _ => (dims.width + 120, dims.height)
  collect := fun layout =>
    let rect := layout.contentRect
    let cmds : Array RenderCommand := #[]

    -- Adjust chart area for legend
    let chartX := rect.x + dims.marginLeft
    let chartY := rect.y + dims.marginTop
    let chartWidth := dims.width - dims.marginLeft - dims.marginRight - 20
    let chartHeight := dims.height - dims.marginTop - dims.marginBottom

    -- Find global data bounds
    let (minX, maxX, minY, maxY, minSize, maxSize) := Id.run do
      let mut initialized := false
      let mut minX : Float := 0.0
      let mut maxX : Float := 1.0
      let mut minY : Float := 0.0
      let mut maxY : Float := 1.0
      let mut minSize : Float := 0.0
      let mut maxSize : Float := 1.0
      for s in series do
        for p in s.points do
          if !initialized then
            minX := p.x; maxX := p.x
            minY := p.y; maxY := p.y
            minSize := p.size; maxSize := p.size
            initialized := true
          else
            if p.x < minX then minX := p.x
            if p.x > maxX then maxX := p.x
            if p.y < minY then minY := p.y
            if p.y > maxY then maxY := p.y
            if p.size < minSize then minSize := p.size
            if p.size > maxSize then maxSize := p.size
      (minX, maxX, minY, maxY, minSize, maxSize)

    let (niceMinX, niceMaxX) := niceAxisBounds minX maxX
    let (niceMinY, niceMaxY) := niceAxisBounds minY maxY
    let rangeX := niceMaxX - niceMinX
    let rangeY := niceMaxY - niceMinY

    -- Draw background
    let bgRect := Arbor.Rect.mk' rect.x rect.y (dims.width + 120) dims.height
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
        for i in [0:dims.gridLineCount + 1] do
          let ratio := i.toFloat / dims.gridLineCount.toFloat
          let lineX := chartX + (ratio * chartWidth)
          let lineRect := Arbor.Rect.mk' lineX chartY 1.0 chartHeight
          cmds := cmds.push (.fillRect lineRect (Color.gray 0.3) 0.0)
        cmds
    else cmds

    let colors := defaultColors theme

    -- Draw bubbles
    let cmds := Id.run do
      let mut cmds := cmds
      for si in [0:series.size] do
        let s := series[si]!
        let seriesColor := s.color.getD (colors[si % colors.size]!)
        for p in s.points do
          let px := chartX + ((p.x - niceMinX) / rangeX) * chartWidth
          let py := chartY + chartHeight - ((p.y - niceMinY) / rangeY) * chartHeight
          let radius := sizeToRadius p.size minSize maxSize dims
          let color := (p.color.getD seriesColor).withAlpha dims.bubbleOpacity
          let bubblePath := Arbor.Path.circle (Arbor.Point.mk' px py) radius
          cmds := cmds.push (.fillPath bubblePath color)
          cmds := cmds.push (.strokePath bubblePath (color.withAlpha 1.0) 1.5)
      cmds

    -- Draw axes
    let axisColor := Color.gray 0.5
    let yAxisRect := Arbor.Rect.mk' chartX chartY 1.0 chartHeight
    let cmds := cmds.push (.fillRect yAxisRect axisColor 0.0)
    let xAxisRect := Arbor.Rect.mk' chartX (chartY + chartHeight) chartWidth 1.0
    let cmds := cmds.push (.fillRect xAxisRect axisColor 0.0)

    -- Draw axis labels
    let cmds := if dims.showAxisLabels && dims.gridLineCount > 0 then
      Id.run do
        let mut cmds := cmds
        -- Y-axis labels
        for i in [0:dims.gridLineCount + 1] do
          let ratio := i.toFloat / dims.gridLineCount.toFloat
          let value := niceMinY + ratio * rangeY
          let labelY := chartY + chartHeight - (ratio * chartHeight) - 6
          cmds := cmds.push (.fillText (formatValue value) (rect.x + 4) labelY theme.smallFont theme.textMuted)
        -- X-axis labels
        for i in [0:dims.gridLineCount + 1] do
          let ratio := i.toFloat / dims.gridLineCount.toFloat
          let value := niceMinX + ratio * rangeX
          let labelX := chartX + (ratio * chartWidth)
          cmds := cmds.push (.fillText (formatValue value) labelX (chartY + chartHeight + 16) theme.smallFont theme.textMuted)
        cmds
    else cmds

    -- Draw legend
    let legendX := chartX + chartWidth + 30
    let legendStartY := chartY + 10
    let legendItemHeight : Float := 24.0

    let cmds := Id.run do
      let mut cmds := cmds
      for si in [0:series.size] do
        let s := series[si]!
        let color := s.color.getD (colors[si % colors.size]!)
        let itemY := legendStartY + si.toFloat * legendItemHeight

        -- Color circle
        let circlePath := Arbor.Path.circle (Arbor.Point.mk' (legendX + 8) (itemY + 8)) 6.0
        cmds := cmds.push (.fillPath circlePath color)

        -- Label text
        let label := s.label.getD s!"Series {si + 1}"
        cmds := cmds.push (.fillText label (legendX + 20) (itemY + 12) theme.smallFont theme.text)
      cmds

    cmds

  draw := none
}

end BubbleChart

/-- Build a bubble chart visual (WidgetBuilder version).
    - `name`: Widget name for identification
    - `points`: Array of bubble data points (x, y, size)
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def bubbleChartVisual (name : String) (points : Array BubbleChart.DataPoint)
    (theme : Theme) (dims : BubbleChart.Dimensions := BubbleChart.defaultDimensions)
    : WidgetBuilder := do
  let wid ← freshId
  let chart ← custom (BubbleChart.bubbleChartSpec points theme dims) {
    minWidth := some dims.width
    minHeight := some dims.height
  }
  let props : Trellis.FlexContainer := { Trellis.FlexContainer.column 0 with alignItems := .flexStart }
  pure (.flex wid (some name) props {} #[chart])

/-- Build a multi-series bubble chart visual (WidgetBuilder version).
    - `name`: Widget name for identification
    - `series`: Array of data series
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def multiSeriesBubbleChartVisual (name : String) (series : Array BubbleChart.Series)
    (theme : Theme) (dims : BubbleChart.Dimensions := BubbleChart.defaultDimensions)
    : WidgetBuilder := do
  let wid ← freshId
  let chart ← custom (BubbleChart.multiSeriesSpec series theme dims) {
    minWidth := some dims.width
    minHeight := some dims.height
  }
  let props : Trellis.FlexContainer := { Trellis.FlexContainer.column 0 with alignItems := .flexStart }
  pure (.flex wid (some name) props {} #[chart])

/-- Build a bubble chart with legend visual (WidgetBuilder version).
    - `name`: Widget name for identification
    - `series`: Array of data series
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def bubbleChartWithLegendVisual (name : String) (series : Array BubbleChart.Series)
    (theme : Theme) (dims : BubbleChart.Dimensions := BubbleChart.defaultDimensions)
    : WidgetBuilder := do
  let wid ← freshId
  let chart ← custom (BubbleChart.bubbleChartWithLegendSpec series theme dims) {
    minWidth := some (dims.width + 120)
    minHeight := some dims.height
  }
  let props : Trellis.FlexContainer := { Trellis.FlexContainer.column 0 with alignItems := .flexStart }
  pure (.flex wid (some name) props {} #[chart])

/-! ## Reactive BubbleChart Components (FRP-based)

These use WidgetM for declarative composition.
-/

open Reactive Reactive.Host
open Afferent.Canopy.Reactive

/-- BubbleChart result - provides access to chart state. -/
structure BubbleChartResult where
  /-- The points being displayed. -/
  points : Reactive.Dynamic Spider (Array BubbleChart.DataPoint)

/-- Create a bubble chart component using WidgetM.
    Displays a static bubble chart with the given data points.
    - `points`: Array of bubble data points (x, y, size)
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def bubbleChart (points : Array BubbleChart.DataPoint)
    (theme : Theme) (dims : BubbleChart.Dimensions := BubbleChart.defaultDimensions)
    : WidgetM BubbleChartResult := do
  let name ← registerComponentW "bubble-chart" (isInteractive := false)

  let pointsDyn ← Dynamic.pureM points

  emit do
    pure (bubbleChartVisual name points theme dims)

  pure { points := pointsDyn }

/-- MultiSeriesBubbleChart result. -/
structure MultiSeriesBubbleChartResult where
  series : Reactive.Dynamic Spider (Array BubbleChart.Series)

/-- Create a multi-series bubble chart for comparing multiple data sets.
    - `series`: Array of data series with points and optional colors/labels
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def multiSeriesBubbleChart (series : Array BubbleChart.Series)
    (theme : Theme) (dims : BubbleChart.Dimensions := BubbleChart.defaultDimensions)
    : WidgetM MultiSeriesBubbleChartResult := do
  let name ← registerComponentW "bubble-chart" (isInteractive := false)

  let seriesDyn ← Dynamic.pureM series

  emit do
    pure (multiSeriesBubbleChartVisual name series theme dims)

  pure { series := seriesDyn }

/-- Create a bubble chart with legend for comparing multiple data sets.
    - `series`: Array of data series with points and optional colors/labels
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def bubbleChartWithLegend (series : Array BubbleChart.Series)
    (theme : Theme) (dims : BubbleChart.Dimensions := BubbleChart.defaultDimensions)
    : WidgetM MultiSeriesBubbleChartResult := do
  let name ← registerComponentW "bubble-chart" (isInteractive := false)

  let seriesDyn ← Dynamic.pureM series

  emit do
    pure (bubbleChartWithLegendVisual name series theme dims)

  pure { series := seriesDyn }

/-- Create a bubble chart that updates based on an external event stream.
    - `initialPoints`: Initial data points
    - `pointUpdates`: Event stream of point updates
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def bubbleChartWithEvents (initialPoints : Array BubbleChart.DataPoint)
    (pointUpdates : Reactive.Event Spider (Array BubbleChart.DataPoint))
    (theme : Theme) (dims : BubbleChart.Dimensions := BubbleChart.defaultDimensions)
    : WidgetM BubbleChartResult := do
  let name ← registerComponentW "bubble-chart" (isInteractive := false)

  let pointsDyn ← Reactive.holdDyn initialPoints pointUpdates

  emit do
    let p ← pointsDyn.sample
    pure (bubbleChartVisual name p theme dims)

  pure { points := pointsDyn }

/-- Helper to create bubble data points from (x, y, size) tuples. -/
def BubbleChart.DataPoint.fromTuples (tuples : Array (Float × Float × Float)) : Array BubbleChart.DataPoint :=
  tuples.map fun (x, y, size) => { x, y, size }

/-- Helper to create labeled bubble data points. -/
def BubbleChart.DataPoint.fromLabeledTuples (tuples : Array (Float × Float × Float × String)) : Array BubbleChart.DataPoint :=
  tuples.map fun (x, y, size, label) => { x, y, size, label := some label }

end Afferent.Canopy

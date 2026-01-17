/-
  Canopy BoxPlot Widget
  Box and whisker plot for showing data distribution via five-number summary.
-/
import Reactive
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Reactive.Component
import Afferent.Canopy.Widget.Charts.Core

namespace Afferent.Canopy

open Afferent.Arbor hiding Event

namespace BoxPlot

/-- Dimensions and styling for box plot rendering. -/
structure Dimensions extends AxisChartDimensions where
  boxWidth : Float := 40.0
  whiskerWidth : Float := 20.0
  showOutliers : Bool := true
  outlierRadius : Float := 4.0
  /-- IQR multiplier for outlier detection (typically 1.5). -/
  outlierThreshold : Float := 1.5
deriving Repr, Inhabited

/-- Default box plot dimensions. -/
def defaultDimensions : Dimensions := {}

/-- Five-number summary for a box plot. -/
structure Summary where
  min : Float
  q1 : Float
  median : Float
  q3 : Float
  max : Float
  /-- Data points beyond whiskers. -/
  outliers : Array Float := #[]
  /-- Optional label for this box. -/
  label : Option String := none
deriving Repr, Inhabited, BEq

/-- A complete box plot data set (can have multiple boxes). -/
structure DataSet where
  summaries : Array Summary
  /-- Optional title for the entire plot. -/
  title : Option String := none
deriving Repr, Inhabited

/-- Default colors for box plots. -/
def defaultColors (theme : Theme) : Array Color := #[
  theme.primary.background,
  theme.secondary.background,
  Color.rgba 0.2 0.8 0.3 1.0,
  Color.rgba 1.0 0.7 0.0 1.0,
  Color.rgba 0.9 0.2 0.2 1.0,
  Color.rgba 0.5 0.3 0.9 1.0,
  Color.rgba 0.0 0.7 0.7 1.0
]

/-- Sort an array of floats. -/
private def sortFloats (arr : Array Float) : Array Float :=
  arr.qsort (· < ·)

/-- Compute the percentile value from sorted data. -/
private def percentile (sorted : Array Float) (p : Float) : Float :=
  if sorted.isEmpty then 0.0
  else if sorted.size == 1 then sorted[0]!
  else
    let n := sorted.size.toFloat
    let rank := p * (n - 1)
    let lower := rank.floor.toUInt64.toNat
    let upper := min (lower + 1) (sorted.size - 1)
    let frac := rank - rank.floor
    let lowerVal := sorted[lower]!
    let upperVal := sorted[upper]!
    lowerVal + frac * (upperVal - lowerVal)

/-- Compute a five-number summary from raw data. -/
def computeSummary (data : Array Float) (label : Option String := none)
    (outlierThreshold : Float := 1.5) : Summary :=
  if data.isEmpty then
    { min := 0, q1 := 0, median := 0, q3 := 0, max := 0, label }
  else
    let sorted := sortFloats data
    let q1 := percentile sorted 0.25
    let median := percentile sorted 0.5
    let q3 := percentile sorted 0.75
    let iqr := q3 - q1
    let lowerFence := q1 - outlierThreshold * iqr
    let upperFence := q3 + outlierThreshold * iqr

    -- Find whisker ends (furthest non-outlier points)
    let whiskerMin := sorted.foldl (fun acc v =>
      if v >= lowerFence && v < acc then v else acc) q1
    let whiskerMax := sorted.foldl (fun acc v =>
      if v <= upperFence && v > acc then v else acc) q3

    -- Collect outliers
    let outliers := sorted.filter (fun v => v < lowerFence || v > upperFence)

    { min := whiskerMin, q1, median, q3, max := whiskerMax, outliers, label }

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

/-- Custom spec for box plot rendering. -/
def boxPlotSpec (summaries : Array Summary) (theme : Theme)
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

    if summaries.isEmpty then #[] else

    -- Find global min/max for Y axis scaling
    let (globalMin, globalMax) := Id.run do
      let first := summaries[0]!
      let mut minVal := first.min
      let mut maxVal := first.max
      -- Include outliers in range
      for o in first.outliers do
        if o < minVal then minVal := o
        if o > maxVal then maxVal := o
      for s in summaries do
        if s.min < minVal then minVal := s.min
        if s.max > maxVal then maxVal := s.max
        for o in s.outliers do
          if o < minVal then minVal := o
          if o > maxVal then maxVal := o
      (minVal, maxVal)

    -- Add padding to range
    let range := globalMax - globalMin
    let padding := if range > 0 then range * 0.1 else 1.0
    let yMin := globalMin - padding
    let yMax := globalMax + padding
    let yRange := yMax - yMin

    -- Calculate box spacing
    let boxCount := summaries.size
    let totalBoxWidth := dims.boxWidth * boxCount.toFloat
    let totalGapWidth := chartWidth - totalBoxWidth
    let gapWidth := if boxCount > 1 then totalGapWidth / (boxCount + 1).toFloat else totalGapWidth / 2

    let colors := defaultColors theme
    let axisColor := Color.gray 0.5

    RenderM.build do
      -- Draw background
      RenderM.fillRect' rect.x rect.y actualWidth actualHeight (theme.panel.background.withAlpha 0.3) 6.0

      -- Draw horizontal grid lines
      if dims.showGridLines && dims.gridLineCount > 0 then
        for i in [0:dims.gridLineCount + 1] do
          let ratio := i.toFloat / dims.gridLineCount.toFloat
          let lineY := chartY + chartHeight - (ratio * chartHeight)
          RenderM.fillRect' chartX lineY chartWidth 1.0 (Color.gray 0.3) 0.0

      -- Draw each box plot
      for i in [0:boxCount] do
        let s := summaries[i]!
        let color := colors[i % colors.size]!
        let boxCenterX := chartX + gapWidth + i.toFloat * (dims.boxWidth + gapWidth) + dims.boxWidth / 2

        -- Convert values to Y coordinates (inverted: higher values = lower Y)
        let yToPixel := fun (v : Float) => chartY + chartHeight - ((v - yMin) / yRange) * chartHeight

        let minY := yToPixel s.min
        let q1Y := yToPixel s.q1
        let medianY := yToPixel s.median
        let q3Y := yToPixel s.q3
        let maxY := yToPixel s.max

        -- Draw whisker (vertical line from min to max)
        let whiskerX := boxCenterX
        RenderM.fillRect' (whiskerX - 0.5) maxY 1.0 (minY - maxY) color 0.0

        -- Draw whisker caps
        let capHalfWidth := dims.whiskerWidth / 2
        -- Min cap (bottom)
        RenderM.fillRect' (whiskerX - capHalfWidth) (minY - 0.5) dims.whiskerWidth 1.0 color 0.0
        -- Max cap (top)
        RenderM.fillRect' (whiskerX - capHalfWidth) (maxY - 0.5) dims.whiskerWidth 1.0 color 0.0

        -- Draw box (Q1 to Q3)
        let boxLeft := boxCenterX - dims.boxWidth / 2
        let boxHeight := q1Y - q3Y  -- Q1 is lower on screen (higher Y) than Q3
        let boxRect := Arbor.Rect.mk' boxLeft q3Y dims.boxWidth boxHeight
        RenderM.fillRect boxRect (color.withAlpha 0.6) 2.0
        -- Box outline
        RenderM.strokeRect boxRect color 1.5

        -- Draw median line
        RenderM.fillRect' boxLeft (medianY - 1) dims.boxWidth 2.0 (Color.white.withAlpha 0.9) 0.0

        -- Draw outliers
        if dims.showOutliers then
          for outlier in s.outliers do
            let outlierY := yToPixel outlier
            -- GPU-batched outlier marker
            RenderM.fillCircle' boxCenterX outlierY dims.outlierRadius color

      -- Draw Y-axis labels
      if dims.gridLineCount > 0 then
        for i in [0:dims.gridLineCount + 1] do
          let ratio := i.toFloat / dims.gridLineCount.toFloat
          let value := yMin + ratio * yRange
          let labelY := chartY + chartHeight - (ratio * chartHeight) + 4
          RenderM.fillText (formatValue value) (rect.x + 4) labelY theme.smallFont theme.textMuted

      -- Draw X-axis labels (box labels)
      for i in [0:boxCount] do
        let s := summaries[i]!
        match s.label with
        | some label =>
          let boxCenterX := chartX + gapWidth + i.toFloat * (dims.boxWidth + gapWidth) + dims.boxWidth / 2
          let labelY := chartY + chartHeight + 16
          RenderM.fillText label boxCenterX labelY theme.smallFont theme.textMuted
        | none => pure ()

      -- Draw axes
      RenderM.fillRect' chartX chartY 1.0 chartHeight axisColor 0.0
      RenderM.fillRect' chartX (chartY + chartHeight) chartWidth 1.0 axisColor 0.0

  draw := none
}

/-- Custom spec for horizontal box plot rendering. -/
def horizontalBoxPlotSpec (summaries : Array Summary) (theme : Theme)
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

    if summaries.isEmpty then #[] else

    -- Find global min/max for X axis scaling
    let (globalMin, globalMax) := Id.run do
      let first := summaries[0]!
      let mut minVal := first.min
      let mut maxVal := first.max
      for o in first.outliers do
        if o < minVal then minVal := o
        if o > maxVal then maxVal := o
      for s in summaries do
        if s.min < minVal then minVal := s.min
        if s.max > maxVal then maxVal := s.max
        for o in s.outliers do
          if o < minVal then minVal := o
          if o > maxVal then maxVal := o
      (minVal, maxVal)

    let range := globalMax - globalMin
    let padding := if range > 0 then range * 0.1 else 1.0
    let xMin := globalMin - padding
    let xMax := globalMax + padding
    let xRange := xMax - xMin

    let boxCount := summaries.size
    let totalBoxHeight := dims.boxWidth * boxCount.toFloat
    let totalGapHeight := chartHeight - totalBoxHeight
    let gapHeight := if boxCount > 1 then totalGapHeight / (boxCount + 1).toFloat else totalGapHeight / 2

    let colors := defaultColors theme
    let axisColor := Color.gray 0.5

    RenderM.build do
      -- Draw background
      RenderM.fillRect' rect.x rect.y actualWidth actualHeight (theme.panel.background.withAlpha 0.3) 6.0

      -- Draw vertical grid lines
      if dims.showGridLines && dims.gridLineCount > 0 then
        for i in [0:dims.gridLineCount + 1] do
          let ratio := i.toFloat / dims.gridLineCount.toFloat
          let lineX := chartX + (ratio * chartWidth)
          RenderM.fillRect' lineX chartY 1.0 chartHeight (Color.gray 0.3) 0.0

      -- Draw each horizontal box plot
      for i in [0:boxCount] do
        let s := summaries[i]!
        let color := colors[i % colors.size]!
        let boxCenterY := chartY + gapHeight + i.toFloat * (dims.boxWidth + gapHeight) + dims.boxWidth / 2

        let xToPixel := fun (v : Float) => chartX + ((v - xMin) / xRange) * chartWidth

        let minX := xToPixel s.min
        let q1X := xToPixel s.q1
        let medianX := xToPixel s.median
        let q3X := xToPixel s.q3
        let maxX := xToPixel s.max

        -- Draw whisker (horizontal line from min to max)
        RenderM.fillRect' minX (boxCenterY - 0.5) (maxX - minX) 1.0 color 0.0

        -- Draw whisker caps
        let capHalfHeight := dims.whiskerWidth / 2
        RenderM.fillRect' (minX - 0.5) (boxCenterY - capHalfHeight) 1.0 dims.whiskerWidth color 0.0
        RenderM.fillRect' (maxX - 0.5) (boxCenterY - capHalfHeight) 1.0 dims.whiskerWidth color 0.0

        -- Draw box (Q1 to Q3)
        let boxTop := boxCenterY - dims.boxWidth / 2
        let boxWidth := q3X - q1X
        let boxRect := Arbor.Rect.mk' q1X boxTop boxWidth dims.boxWidth
        RenderM.fillRect boxRect (color.withAlpha 0.6) 2.0
        RenderM.strokeRect boxRect color 1.5

        -- Draw median line
        RenderM.fillRect' (medianX - 1) boxTop 2.0 dims.boxWidth (Color.white.withAlpha 0.9) 0.0

        -- Draw outliers
        if dims.showOutliers then
          for outlier in s.outliers do
            let outlierX := xToPixel outlier
            -- GPU-batched outlier marker
            RenderM.fillCircle' outlierX boxCenterY dims.outlierRadius color

      -- Draw X-axis labels (values)
      if dims.gridLineCount > 0 then
        for i in [0:dims.gridLineCount + 1] do
          let ratio := i.toFloat / dims.gridLineCount.toFloat
          let value := xMin + ratio * xRange
          let labelX := chartX + (ratio * chartWidth)
          let labelY := chartY + chartHeight + 16
          RenderM.fillText (formatValue value) labelX labelY theme.smallFont theme.textMuted

      -- Draw Y-axis labels (box labels)
      for i in [0:boxCount] do
        let s := summaries[i]!
        match s.label with
        | some label =>
          let boxCenterY := chartY + gapHeight + i.toFloat * (dims.boxWidth + gapHeight) + dims.boxWidth / 2
          RenderM.fillText label (rect.x + 4) (boxCenterY + 4) theme.smallFont theme.text
        | none => pure ()

      -- Draw axes
      RenderM.fillRect' chartX chartY 1.0 chartHeight axisColor 0.0
      RenderM.fillRect' chartX (chartY + chartHeight) chartWidth 1.0 axisColor 0.0

  draw := none
}

end BoxPlot

/-- Build a box plot visual (WidgetBuilder version).
    - `name`: Widget name for identification
    - `summaries`: Array of five-number summaries
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def boxPlotVisual (name : String) (summaries : Array BoxPlot.Summary)
    (theme : Theme) (dims : BoxPlot.Dimensions := BoxPlot.defaultDimensions)
    : WidgetBuilder := do
  let wid ← freshId
  let chart ← custom (BoxPlot.boxPlotSpec summaries theme dims) {
    width := .percent 1.0
    height := .percent 1.0
    flexItem := some (Trellis.FlexItem.growing 1)
  }
  let style : BoxStyle := { width := .percent 1.0, height := .percent 1.0, minWidth := some dims.width, minHeight := some dims.height, flexItem := some (Trellis.FlexItem.growing 1) }
  let props : Trellis.FlexContainer := { Trellis.FlexContainer.column 0 with alignItems := .stretch }
  pure (.flex wid (some name) props style #[chart])

/-- Build a horizontal box plot visual (WidgetBuilder version).
    - `name`: Widget name for identification
    - `summaries`: Array of five-number summaries
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def horizontalBoxPlotVisual (name : String) (summaries : Array BoxPlot.Summary)
    (theme : Theme) (dims : BoxPlot.Dimensions := BoxPlot.defaultDimensions)
    : WidgetBuilder := do
  let wid ← freshId
  let chart ← custom (BoxPlot.horizontalBoxPlotSpec summaries theme dims) {
    width := .percent 1.0
    height := .percent 1.0
    flexItem := some (Trellis.FlexItem.growing 1)
  }
  let style : BoxStyle := { width := .percent 1.0, height := .percent 1.0, minWidth := some dims.width, minHeight := some dims.height, flexItem := some (Trellis.FlexItem.growing 1) }
  let props : Trellis.FlexContainer := { Trellis.FlexContainer.column 0 with alignItems := .stretch }
  pure (.flex wid (some name) props style #[chart])

/-! ## Reactive BoxPlot Components (FRP-based)

These use WidgetM for declarative composition.
-/

open Reactive Reactive.Host
open Afferent.Canopy.Reactive

/-- BoxPlot result - provides access to chart state. -/
structure BoxPlotResult where
  /-- The summaries being displayed. -/
  summaries : Dyn (Array BoxPlot.Summary)

/-- Create a box plot component from pre-computed summaries using WidgetM with dynamic data.
    The chart automatically rebuilds when the summaries Dynamic changes.
    - `summaries`: Dynamic array of five-number summaries
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def boxPlot (summaries : Dyn (Array BoxPlot.Summary))
    (theme : Theme) (dims : BoxPlot.Dimensions := BoxPlot.defaultDimensions)
    : WidgetM BoxPlotResult := do
  let _ ← dynWidget summaries fun currentSummaries => do
    let name ← registerComponentW "box-plot" (isInteractive := false)
    emit do pure (boxPlotVisual name currentSummaries theme dims)

  pure { summaries }

/-- Create a box plot from dynamic raw data arrays.
    Each array represents a different group/category.
    - `dataArrays`: Dynamic array of raw data arrays
    - `labels`: Labels for each box
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def boxPlotFromData (dataArrays : Dyn (Array (Array Float))) (labels : Array String := #[])
    (theme : Theme) (dims : BoxPlot.Dimensions := BoxPlot.defaultDimensions)
    : WidgetM BoxPlotResult := do
  let summariesDyn ← Dynamic.mapM (fun currentDataArrays => Id.run do
    let mut result : Array BoxPlot.Summary := #[]
    for i in [0:currentDataArrays.size] do
      let data := currentDataArrays[i]!
      let label := if i < labels.size then some labels[i]! else none
      result := result.push (BoxPlot.computeSummary data label dims.outlierThreshold)
    result
  ) dataArrays
  boxPlot summariesDyn theme dims

/-- Create a horizontal box plot component with dynamic data.
    The chart automatically rebuilds when the summaries Dynamic changes.
    - `summaries`: Dynamic array of five-number summaries
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def horizontalBoxPlot (summaries : Dyn (Array BoxPlot.Summary))
    (theme : Theme) (dims : BoxPlot.Dimensions := BoxPlot.defaultDimensions)
    : WidgetM BoxPlotResult := do
  let _ ← dynWidget summaries fun currentSummaries => do
    let name ← registerComponentW "box-plot" (isInteractive := false)
    emit do pure (horizontalBoxPlotVisual name currentSummaries theme dims)

  pure { summaries }

/-- Create a horizontal box plot from dynamic raw data arrays.
    - `dataArrays`: Dynamic array of raw data arrays
    - `labels`: Labels for each box
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def horizontalBoxPlotFromData (dataArrays : Dyn (Array (Array Float))) (labels : Array String := #[])
    (theme : Theme) (dims : BoxPlot.Dimensions := BoxPlot.defaultDimensions)
    : WidgetM BoxPlotResult := do
  let summariesDyn ← Dynamic.mapM (fun currentDataArrays => Id.run do
    let mut result : Array BoxPlot.Summary := #[]
    for i in [0:currentDataArrays.size] do
      let data := currentDataArrays[i]!
      let label := if i < labels.size then some labels[i]! else none
      result := result.push (BoxPlot.computeSummary data label dims.outlierThreshold)
    result
  ) dataArrays
  horizontalBoxPlot summariesDyn theme dims

end Afferent.Canopy

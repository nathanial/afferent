/-
  Canopy DonutChart Widget
  Donut/ring chart for showing proportional data with a hollow center.
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

namespace DonutChart

/-- Dimensions and styling for donut chart rendering. -/
structure Dimensions extends ChartSize where
  width := 300.0
  height := 300.0
  outerRadius : Float := 100.0
  innerRadius : Float := 60.0
  showLabels : Bool := true
  showPercentages : Bool := true
  labelOffset : Float := 20.0
  strokeWidth : Float := 1.0
  strokeColor : Option Color := some (Color.gray 0.2)
  centerLabel : Option String := none
  centerValue : Option String := none
deriving Repr, Inhabited

/-- Default donut chart dimensions. -/
def defaultDimensions : Dimensions := {}

/-- A slice of the donut chart. -/
structure Slice where
  value : Float
  label : Option String := none
  color : Option Color := none
deriving Repr, Inhabited, BEq

/-- Create an annular (ring) segment path.
    This creates a closed path for a donut slice by:
    1. Drawing the outer arc
    2. Line to inner arc end
    3. Drawing the inner arc (reversed)
    4. Closing back to start -/
private def annularSegment (center : Arbor.Point) (outerR innerR : Float)
    (startAngle endAngle : Float) : Afferent.Path := Id.run do
  let pi := 3.141592653589793
  let twoPi := 2.0 * pi

  -- Outer arc points
  let outerStartX := center.x + outerR * Float.cos startAngle
  let outerStartY := center.y + outerR * Float.sin startAngle
  let outerEndX := center.x + outerR * Float.cos endAngle
  let outerEndY := center.y + outerR * Float.sin endAngle

  -- Inner arc points (reversed direction)
  let innerStartX := center.x + innerR * Float.cos endAngle
  let innerStartY := center.y + innerR * Float.sin endAngle
  let innerEndX := center.x + innerR * Float.cos startAngle
  let innerEndY := center.y + innerR * Float.sin startAngle

  -- Build path with bezier approximation for arcs
  let outerBeziers := Afferent.Path.arcToBeziers center outerR startAngle endAngle false
  let innerBeziers := Afferent.Path.arcToBeziers center innerR endAngle startAngle true

  let mut path := Afferent.Path.empty
  path := path.moveTo (Arbor.Point.mk' outerStartX outerStartY)

  -- Draw outer arc
  for (cp1, cp2, endPt) in outerBeziers do
    path := path.bezierCurveTo cp1 cp2 endPt

  -- Line to inner arc start
  path := path.lineTo (Arbor.Point.mk' innerStartX innerStartY)

  -- Draw inner arc (reversed)
  for (cp1, cp2, endPt) in innerBeziers do
    path := path.bezierCurveTo cp1 cp2 endPt

  -- Close path
  path := path.closePath
  return path

/-- Custom spec for donut chart rendering. -/
def donutChartSpec (slices : Array Slice) (theme : Theme)
    (dims : Dimensions := defaultDimensions) : CustomSpec := {
  measure := fun _ _ => (50, 50)
  collect := fun layout =>
    let rect := layout.contentRect
    let actualWidth := rect.width
    let actualHeight := rect.height

    -- Use smaller of width/height for diameter-based sizing
    let actualDiameter := min actualWidth actualHeight

    -- Calculate center of chart
    let centerX := rect.x + actualWidth / 2
    let centerY := rect.y + actualHeight / 2
    let center := Arbor.Point.mk' centerX centerY

    -- Scale radii based on actual size vs configured size
    let scale := actualDiameter / dims.width
    let outerRadius := dims.outerRadius * scale
    let innerRadius := dims.innerRadius * scale
    let labelOffset := dims.labelOffset * scale

    -- Calculate total value
    let total := slices.foldl (fun acc s => acc + s.value) 0.0
    let total := if total <= 0.0 then 1.0 else total

    let colors := ChartUtils.defaultColors theme
    let pi := 3.141592653589793
    let twoPi := 2.0 * pi

    RenderM.build do
      -- Draw each slice
      let mut startAngle := -pi / 2  -- Start at top (12 o'clock)
      for i in [0:slices.size] do
        let slice := slices[i]!
        let proportion := slice.value / total
        let sweepAngle := proportion * twoPi
        let endAngle := startAngle + sweepAngle

        -- Get color for this slice
        let color := slice.color.getD (colors[i % colors.size]!)

        -- Create annular segment path
        let segmentPath := annularSegment center outerRadius innerRadius startAngle endAngle

        -- Fill the segment
        RenderM.fillPath segmentPath color

        -- Optionally stroke the segment
        if let some strokeColor := dims.strokeColor then
          if dims.strokeWidth > 0.0 then
            RenderM.strokePath segmentPath strokeColor dims.strokeWidth

        startAngle := endAngle

      -- Draw center label/value if specified
      match dims.centerLabel, dims.centerValue with
      | some label, some value =>
        RenderM.fillText label centerX (centerY - 8) theme.font theme.text
        RenderM.fillText value centerX (centerY + 12) theme.smallFont theme.textMuted
      | some label, none =>
        RenderM.fillText label centerX (centerY + 4) theme.font theme.text
      | none, some value =>
        RenderM.fillText value centerX (centerY + 4) theme.font theme.text
      | none, none => pure ()

      -- Draw labels outside the ring
      if dims.showLabels || dims.showPercentages then
        let mut labelAngle := -pi / 2
        for i in [0:slices.size] do
          let slice := slices[i]!
          let proportion := slice.value / total
          let sweepAngle := proportion * twoPi
          let midAngle := labelAngle + sweepAngle / 2

          -- Calculate label position (outside the outer ring)
          let labelRadius := outerRadius + labelOffset
          let labelX := centerX + labelRadius * Float.cos midAngle
          let labelY := centerY + labelRadius * Float.sin midAngle

          -- Build label text
          let labelParts : Array String := Id.run do
            let mut parts : Array String := #[]
            if dims.showLabels then
              if let some label := slice.label then
                parts := parts.push label
            if dims.showPercentages then
              parts := parts.push (ChartUtils.formatPercent proportion)
            parts

          if labelParts.size > 0 then
            let labelText := String.intercalate " " labelParts.toList
            RenderM.fillText labelText labelX (labelY + 4) theme.smallFont theme.text

          labelAngle := labelAngle + sweepAngle

  draw := none
}

/-- Custom spec for donut chart with legend. -/
def donutChartWithLegendSpec (slices : Array Slice) (theme : Theme)
    (dims : Dimensions := defaultDimensions) : CustomSpec := {
  measure := fun _ _ => (170, 50)
  collect := fun layout =>
    let rect := layout.contentRect
    let actualWidth := rect.width
    let actualHeight := rect.height

    -- Reserve space for legend (120px on right)
    let chartAreaWidth := actualWidth - 120
    let chartDiameter := min chartAreaWidth actualHeight

    -- Scale based on actual size
    let scale := chartDiameter / dims.width
    let outerRadius := dims.outerRadius * scale
    let innerRadius := dims.innerRadius * scale

    -- Chart on left, legend on right
    let chartCenterX := rect.x + chartDiameter / 2 + 20
    let chartCenterY := rect.y + actualHeight / 2
    let center := Arbor.Point.mk' chartCenterX chartCenterY

    let total := slices.foldl (fun acc s => acc + s.value) 0.0
    let total := if total <= 0.0 then 1.0 else total

    let colors := ChartUtils.defaultColors theme
    let pi := 3.141592653589793
    let twoPi := 2.0 * pi

    -- Legend positioning
    let legendX := rect.x + chartDiameter + 50
    let legendStartY := rect.y + 20

    RenderM.build do
      -- Draw each slice
      let mut startAngle := -pi / 2
      for i in [0:slices.size] do
        let slice := slices[i]!
        let proportion := slice.value / total
        let sweepAngle := proportion * twoPi
        let endAngle := startAngle + sweepAngle

        let color := slice.color.getD (colors[i % colors.size]!)
        let segmentPath := annularSegment center outerRadius innerRadius startAngle endAngle

        RenderM.fillPath segmentPath color

        if let some strokeColor := dims.strokeColor then
          if dims.strokeWidth > 0.0 then
            RenderM.strokePath segmentPath strokeColor dims.strokeWidth

        startAngle := endAngle

      -- Draw center label/value
      match dims.centerLabel, dims.centerValue with
      | some label, some value =>
        RenderM.fillText label chartCenterX (chartCenterY - 8) theme.font theme.text
        RenderM.fillText value chartCenterX (chartCenterY + 12) theme.smallFont theme.textMuted
      | some label, none =>
        RenderM.fillText label chartCenterX (chartCenterY + 4) theme.font theme.text
      | none, some value =>
        RenderM.fillText value chartCenterX (chartCenterY + 4) theme.font theme.text
      | none, none => pure ()

      -- Build legend items and draw using shared utility
      let legendItems : Array ChartUtils.LegendItem := Id.run do
        let mut items : Array ChartUtils.LegendItem := #[]
        for idx in [0:slices.size] do
          let slice := slices[idx]!
          let proportion := slice.value / total
          let color := slice.color.getD (colors[idx % colors.size]!)
          let label := slice.label.getD s!"Item {idx + 1}"
          items := items.push { label, color, suffix := some (ChartUtils.formatPercent proportion) }
        items
      let _ ← ChartUtils.drawLegend legendItems legendX legendStartY theme

  draw := none
}

end DonutChart

/-- Build a donut chart visual (WidgetBuilder version).
    - `name`: Widget name for identification
    - `slices`: Array of donut slices with values, labels, and optional colors
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def donutChartVisual (name : String) (slices : Array DonutChart.Slice)
    (theme : Theme) (dims : DonutChart.Dimensions := DonutChart.defaultDimensions)
    : WidgetBuilder := do
  let wid ← freshId
  let chart ← custom (DonutChart.donutChartSpec slices theme dims) {
    width := .percent 1.0
    height := .percent 1.0
    flexItem := some (Trellis.FlexItem.growing 1)
  }
  let style : BoxStyle := { width := .percent 1.0, height := .percent 1.0, minWidth := some dims.width, minHeight := some dims.height, flexItem := some (Trellis.FlexItem.growing 1) }
  let props : Trellis.FlexContainer := { Trellis.FlexContainer.column 0 with alignItems := .stretch }
  pure (.flex wid (some name) props style #[chart])

/-- Build a donut chart with legend visual (WidgetBuilder version).
    - `name`: Widget name for identification
    - `slices`: Array of donut slices
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def donutChartWithLegendVisual (name : String) (slices : Array DonutChart.Slice)
    (theme : Theme) (dims : DonutChart.Dimensions := DonutChart.defaultDimensions)
    : WidgetBuilder := do
  let wid ← freshId
  let chart ← custom (DonutChart.donutChartWithLegendSpec slices theme dims) {
    width := .percent 1.0
    height := .percent 1.0
    flexItem := some (Trellis.FlexItem.growing 1)
  }
  let style : BoxStyle := { width := .percent 1.0, height := .percent 1.0, minWidth := some dims.width, minHeight := some dims.height, flexItem := some (Trellis.FlexItem.growing 1) }
  let props : Trellis.FlexContainer := { Trellis.FlexContainer.column 0 with alignItems := .stretch }
  pure (.flex wid (some name) props style #[chart])

/-! ## Reactive DonutChart Components (FRP-based)

These use WidgetM for declarative composition.
-/

open Reactive Reactive.Host
open Afferent.Canopy.Reactive

/-- DonutChart result - provides access to chart state. -/
structure DonutChartResult where
  /-- The slices being displayed. -/
  slices : Dyn (Array DonutChart.Slice)

/-- Create a donut chart component using WidgetM with dynamic data.
    The chart automatically rebuilds when the slices Dynamic changes.
    - `slices`: Dynamic array of donut slices with values, labels, and optional colors
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def donutChart (slices : Dyn (Array DonutChart.Slice))
    (dims : DonutChart.Dimensions := DonutChart.defaultDimensions)
    : WidgetM DonutChartResult := do
  let theme ← getThemeW
  let _ ← dynWidget slices fun currentSlices => do
    let name ← registerComponentW "donut-chart" (isInteractive := false)
    emit do pure (donutChartVisual name currentSlices theme dims)

  pure { slices }

/-- Create a donut chart with legend component using WidgetM with dynamic data.
    The chart automatically rebuilds when the slices Dynamic changes.
    - `slices`: Dynamic array of donut slices
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def donutChartWithLegend (slices : Dyn (Array DonutChart.Slice))
    (dims : DonutChart.Dimensions := DonutChart.defaultDimensions)
    : WidgetM DonutChartResult := do
  let theme ← getThemeW
  let _ ← dynWidget slices fun currentSlices => do
    let name ← registerComponentW "donut-chart" (isInteractive := false)
    emit do pure (donutChartWithLegendVisual name currentSlices theme dims)

  pure { slices }

/-- Helper to create slices from simple value/label pairs. -/
def DonutChart.Slice.fromPairs (pairs : Array (Float × String)) : Array DonutChart.Slice :=
  pairs.map fun (value, label) => { value, label := some label }

end Afferent.Canopy

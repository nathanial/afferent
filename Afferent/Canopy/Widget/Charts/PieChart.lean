/-
  Canopy PieChart Widget
  Pie chart for showing proportional data as circular segments.
-/
import Reactive
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Reactive.Component
import Afferent.Canopy.Widget.Charts.ChartUtils

namespace Afferent.Canopy

open Afferent.Arbor hiding Event
open ChartUtils

namespace PieChart

/-- Dimensions and styling for pie chart rendering. -/
structure Dimensions where
  width : Float := 300.0
  height : Float := 300.0
  radius : Float := 100.0
  showLabels : Bool := true
  showValues : Bool := false
  showPercentages : Bool := true
  labelOffset : Float := 20.0
  strokeWidth : Float := 1.0
  strokeColor : Option Color := some (Color.gray 0.2)
deriving Repr, Inhabited

/-- Default pie chart dimensions. -/
def defaultDimensions : Dimensions := {}

/-- A slice of the pie chart. -/
structure Slice where
  value : Float
  label : Option String := none
  color : Option Color := none
deriving Repr, Inhabited

/-- Custom spec for pie chart rendering. -/
def pieChartSpec (slices : Array Slice) (theme : Theme)
    (dims : Dimensions := defaultDimensions) : CustomSpec := {
  measure := fun _ _ => (50, 50)  -- Minimum size
  collect := fun layout =>
    let rect := layout.contentRect

    -- Use actual allocated size for responsive layout
    let actualWidth := rect.width
    let actualHeight := rect.height

    -- Calculate radius from actual size (leave room for labels if shown)
    let labelSpace := if dims.showLabels || dims.showPercentages || dims.showValues then dims.labelOffset else 0
    let availableSize := min actualWidth actualHeight - labelSpace * 2
    let radius := max 10 (availableSize / 2 * 0.9)

    -- Calculate center of chart
    let centerX := rect.x + actualWidth / 2
    let centerY := rect.y + actualHeight / 2
    let center := Arbor.Point.mk' centerX centerY

    -- Calculate total value
    let total := slices.foldl (fun acc s => acc + s.value) 0.0
    let total := if total <= 0.0 then 1.0 else total

    let colors := ChartUtils.defaultColors theme
    let pi := 3.141592653589793
    let twoPi := 2.0 * pi

    RenderM.build do
      -- GPU-batched background circle
      RenderM.fillCircle center radius (theme.panel.background.withAlpha 0.3)

      -- Draw each slice
      let mut startAngle := -pi / 2  -- Start at top (12 o'clock)
      for i in [0:slices.size] do
        let slice := slices[i]!
        let proportion := slice.value / total
        let sweepAngle := proportion * twoPi
        let endAngle := startAngle + sweepAngle

        -- Get color for this slice
        let color := slice.color.getD (colors[i % colors.size]!)

        -- Create pie slice path
        let slicePath := Arbor.Path.pie center radius startAngle endAngle

        -- Fill the slice
        RenderM.fillPath slicePath color

        -- Optionally stroke the slice
        if let some strokeColor := dims.strokeColor then
          if dims.strokeWidth > 0.0 then
            RenderM.strokePath slicePath strokeColor dims.strokeWidth

        startAngle := endAngle

      -- Draw labels
      if dims.showLabels || dims.showValues || dims.showPercentages then
        let mut labelAngle := -pi / 2
        for i in [0:slices.size] do
          let slice := slices[i]!
          let proportion := slice.value / total
          let sweepAngle := proportion * twoPi
          let midAngle := labelAngle + sweepAngle / 2

          -- Calculate label position (outside the pie)
          let labelRadius := radius + dims.labelOffset
          let labelX := centerX + labelRadius * Float.cos midAngle
          let labelY := centerY + labelRadius * Float.sin midAngle

          -- Build label text
          let labelParts : Array String := Id.run do
            let mut parts : Array String := #[]
            if dims.showLabels then
              if let some label := slice.label then
                parts := parts.push label
            if dims.showValues then
              parts := parts.push (ChartUtils.formatValue slice.value)
            if dims.showPercentages then
              parts := parts.push (ChartUtils.formatPercent proportion)
            parts

          if labelParts.size > 0 then
            let labelText := String.intercalate " " labelParts.toList
            RenderM.fillText labelText labelX (labelY + 4) theme.smallFont theme.text

          labelAngle := labelAngle + sweepAngle

  draw := none
}

/-- Custom spec for pie chart with legend instead of inline labels. -/
def pieChartWithLegendSpec (slices : Array Slice) (theme : Theme)
    (dims : Dimensions := defaultDimensions) : CustomSpec := {
  measure := fun _ _ => (170, 50)  -- Minimum size with legend
  collect := fun layout =>
    let rect := layout.contentRect

    -- Use actual allocated size for responsive layout
    let actualWidth := rect.width
    let actualHeight := rect.height

    -- Reserve space for legend (120px on right)
    let legendWidth : Float := 120
    let chartAreaWidth := actualWidth - legendWidth - 20
    let chartSize := min chartAreaWidth actualHeight
    let radius := max 10 (chartSize / 2 * 0.9)

    -- Chart is on the left, legend on the right
    let chartCenterX := rect.x + radius + 20
    let chartCenterY := rect.y + actualHeight / 2
    let center := Arbor.Point.mk' chartCenterX chartCenterY

    let total := slices.foldl (fun acc s => acc + s.value) 0.0
    let total := if total <= 0.0 then 1.0 else total

    let colors := ChartUtils.defaultColors theme
    let pi := 3.141592653589793
    let twoPi := 2.0 * pi

    -- Legend positioning
    let legendX := rect.x + radius * 2 + 50
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
        let slicePath := Arbor.Path.pie center radius startAngle endAngle

        RenderM.fillPath slicePath color

        if let some strokeColor := dims.strokeColor then
          if dims.strokeWidth > 0.0 then
            RenderM.strokePath slicePath strokeColor dims.strokeWidth

        startAngle := endAngle

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

end PieChart

/-- Build a pie chart visual (WidgetBuilder version).
    - `name`: Widget name for identification
    - `slices`: Array of pie slices with values, labels, and optional colors
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def pieChartVisual (name : String) (slices : Array PieChart.Slice)
    (theme : Theme) (dims : PieChart.Dimensions := PieChart.defaultDimensions)
    : WidgetBuilder := do
  let wid ← freshId
  let chart ← custom (PieChart.pieChartSpec slices theme dims) {
    width := .percent 1.0
    height := .percent 1.0
    flexItem := some (Trellis.FlexItem.growing 1)
  }
  let style : BoxStyle := { width := .percent 1.0, height := .percent 1.0, flexItem := some (Trellis.FlexItem.growing 1) }
  let props : Trellis.FlexContainer := { Trellis.FlexContainer.column 0 with alignItems := .stretch }
  pure (.flex wid (some name) props style #[chart])

/-- Build a pie chart with legend visual (WidgetBuilder version).
    - `name`: Widget name for identification
    - `slices`: Array of pie slices
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def pieChartWithLegendVisual (name : String) (slices : Array PieChart.Slice)
    (theme : Theme) (dims : PieChart.Dimensions := PieChart.defaultDimensions)
    : WidgetBuilder := do
  let wid ← freshId
  let chart ← custom (PieChart.pieChartWithLegendSpec slices theme dims) {
    width := .percent 1.0
    height := .percent 1.0
    flexItem := some (Trellis.FlexItem.growing 1)
  }
  let style : BoxStyle := { width := .percent 1.0, height := .percent 1.0, flexItem := some (Trellis.FlexItem.growing 1) }
  let props : Trellis.FlexContainer := { Trellis.FlexContainer.column 0 with alignItems := .stretch }
  pure (.flex wid (some name) props style #[chart])

/-! ## Reactive PieChart Components (FRP-based)

These use WidgetM for declarative composition.
-/

open Reactive Reactive.Host
open Afferent.Canopy.Reactive

/-- PieChart result - provides access to chart state. -/
structure PieChartResult where
  /-- The slices being displayed. -/
  slices : Reactive.Dynamic Spider (Array PieChart.Slice)

/-- Create a pie chart component using WidgetM.
    Displays a static pie chart with the given slices.
    - `slices`: Array of pie slices with values, labels, and optional colors
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def pieChart (slices : Array PieChart.Slice)
    (theme : Theme) (dims : PieChart.Dimensions := PieChart.defaultDimensions)
    : WidgetM PieChartResult := do
  let name ← registerComponentW "pie-chart" (isInteractive := false)

  let slicesDyn ← Dynamic.pureM slices

  emit do
    pure (pieChartVisual name slices theme dims)

  pure { slices := slicesDyn }

/-- Create a pie chart with legend component using WidgetM.
    - `slices`: Array of pie slices
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def pieChartWithLegend (slices : Array PieChart.Slice)
    (theme : Theme) (dims : PieChart.Dimensions := PieChart.defaultDimensions)
    : WidgetM PieChartResult := do
  let name ← registerComponentW "pie-chart" (isInteractive := false)

  let slicesDyn ← Dynamic.pureM slices

  emit do
    pure (pieChartWithLegendVisual name slices theme dims)

  pure { slices := slicesDyn }

/-- Helper to create slices from simple value/label pairs. -/
def PieChart.Slice.fromPairs (pairs : Array (Float × String)) : Array PieChart.Slice :=
  pairs.map fun (value, label) => { value, label := some label }

/-- Helper to create slices from values with auto-generated labels. -/
def PieChart.Slice.fromValues (values : Array Float) : Array PieChart.Slice :=
  values.mapIdx fun i value => { value, label := some s!"Item {i + 1}" }

end Afferent.Canopy

/-
  Canopy DonutChart Widget
  Donut/ring chart for showing proportional data with a hollow center.
-/
import Reactive
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Reactive.Component

namespace Afferent.Canopy

open Afferent.Arbor hiding Event

namespace DonutChart

/-- Dimensions and styling for donut chart rendering. -/
structure Dimensions where
  width : Float := 300.0
  height : Float := 300.0
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
deriving Repr, Inhabited

/-- Default colors for donut slices. -/
def defaultColors (theme : Theme) : Array Color := #[
  theme.primary.background,
  theme.secondary.background,
  Color.rgba 0.2 0.8 0.3 1.0,
  Color.rgba 1.0 0.7 0.0 1.0,
  Color.rgba 0.9 0.2 0.2 1.0,
  Color.rgba 0.5 0.3 0.9 1.0,
  Color.rgba 0.0 0.7 0.7 1.0,
  Color.rgba 0.9 0.5 0.7 1.0,
  Color.rgba 0.6 0.4 0.2 1.0,
  Color.rgba 0.3 0.6 0.9 1.0
]

/-- Format a percentage value. -/
private def formatPercent (v : Float) : String :=
  let pct := (v * 100).floor.toUInt32
  s!"{pct}%"

/-- Create an annular (ring) segment path.
    This creates a closed path for a donut slice by:
    1. Drawing the outer arc
    2. Line to inner arc end
    3. Drawing the inner arc (reversed)
    4. Closing back to start -/
private def annularSegment (center : Arbor.Point) (outerR innerR : Float)
    (startAngle endAngle : Float) : Arbor.Path := Id.run do
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
  let outerBeziers := Arbor.Path.arcToBeziers center outerR startAngle endAngle false
  let innerBeziers := Arbor.Path.arcToBeziers center innerR endAngle startAngle true

  let mut path := Arbor.Path.empty
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
  measure := fun _ _ => (dims.width, dims.height)
  collect := fun layout =>
    let rect := layout.contentRect
    let cmds : Array RenderCommand := #[]

    -- Calculate center of chart
    let centerX := rect.x + dims.width / 2
    let centerY := rect.y + dims.height / 2
    let center := Arbor.Point.mk' centerX centerY

    -- Calculate total value
    let total := slices.foldl (fun acc s => acc + s.value) 0.0
    let total := if total <= 0.0 then 1.0 else total

    let colors := defaultColors theme
    let pi := 3.141592653589793
    let twoPi := 2.0 * pi

    -- Draw each slice
    let cmds := Id.run do
      let mut cmds := cmds
      let mut startAngle := -pi / 2  -- Start at top (12 o'clock)

      for i in [0:slices.size] do
        let slice := slices[i]!
        let proportion := slice.value / total
        let sweepAngle := proportion * twoPi
        let endAngle := startAngle + sweepAngle

        -- Get color for this slice
        let color := slice.color.getD (colors[i % colors.size]!)

        -- Create annular segment path
        let segmentPath := annularSegment center dims.outerRadius dims.innerRadius startAngle endAngle

        -- Fill the segment
        cmds := cmds.push (.fillPath segmentPath color)

        -- Optionally stroke the segment
        if let some strokeColor := dims.strokeColor then
          if dims.strokeWidth > 0.0 then
            cmds := cmds.push (.strokePath segmentPath strokeColor dims.strokeWidth)

        startAngle := endAngle

      cmds

    -- Draw center label/value if specified
    let cmds := match dims.centerLabel, dims.centerValue with
      | some label, some value =>
        let cmds := cmds.push (.fillText label centerX (centerY - 8) theme.font theme.text)
        cmds.push (.fillText value centerX (centerY + 12) theme.smallFont theme.textMuted)
      | some label, none =>
        cmds.push (.fillText label centerX (centerY + 4) theme.font theme.text)
      | none, some value =>
        cmds.push (.fillText value centerX (centerY + 4) theme.font theme.text)
      | none, none => cmds

    -- Draw labels outside the ring
    let cmds := if dims.showLabels || dims.showPercentages then
      Id.run do
        let mut cmds := cmds
        let mut startAngle := -pi / 2

        for i in [0:slices.size] do
          let slice := slices[i]!
          let proportion := slice.value / total
          let sweepAngle := proportion * twoPi
          let midAngle := startAngle + sweepAngle / 2

          -- Calculate label position (outside the outer ring)
          let labelRadius := dims.outerRadius + dims.labelOffset
          let labelX := centerX + labelRadius * Float.cos midAngle
          let labelY := centerY + labelRadius * Float.sin midAngle

          -- Build label text
          let labelParts : Array String := Id.run do
            let mut parts : Array String := #[]
            if dims.showLabels then
              if let some label := slice.label then
                parts := parts.push label
            if dims.showPercentages then
              parts := parts.push (formatPercent proportion)
            parts

          if labelParts.size > 0 then
            let labelText := String.intercalate " " labelParts.toList
            cmds := cmds.push (.fillText labelText labelX (labelY + 4) theme.smallFont theme.text)

          startAngle := startAngle + sweepAngle

        cmds
    else cmds

    cmds

  draw := none
}

/-- Custom spec for donut chart with legend. -/
def donutChartWithLegendSpec (slices : Array Slice) (theme : Theme)
    (dims : Dimensions := defaultDimensions) : CustomSpec := {
  measure := fun _ _ => (dims.width + 120, dims.height)
  collect := fun layout =>
    let rect := layout.contentRect
    let cmds : Array RenderCommand := #[]

    -- Chart on left, legend on right
    let chartCenterX := rect.x + dims.outerRadius + 20
    let chartCenterY := rect.y + dims.height / 2
    let center := Arbor.Point.mk' chartCenterX chartCenterY

    let total := slices.foldl (fun acc s => acc + s.value) 0.0
    let total := if total <= 0.0 then 1.0 else total

    let colors := defaultColors theme
    let pi := 3.141592653589793
    let twoPi := 2.0 * pi

    -- Draw each slice
    let cmds := Id.run do
      let mut cmds := cmds
      let mut startAngle := -pi / 2

      for i in [0:slices.size] do
        let slice := slices[i]!
        let proportion := slice.value / total
        let sweepAngle := proportion * twoPi
        let endAngle := startAngle + sweepAngle

        let color := slice.color.getD (colors[i % colors.size]!)
        let segmentPath := annularSegment center dims.outerRadius dims.innerRadius startAngle endAngle

        cmds := cmds.push (.fillPath segmentPath color)

        if let some strokeColor := dims.strokeColor then
          if dims.strokeWidth > 0.0 then
            cmds := cmds.push (.strokePath segmentPath strokeColor dims.strokeWidth)

        startAngle := endAngle

      cmds

    -- Draw center label/value
    let cmds := match dims.centerLabel, dims.centerValue with
      | some label, some value =>
        let cmds := cmds.push (.fillText label chartCenterX (chartCenterY - 8) theme.font theme.text)
        cmds.push (.fillText value chartCenterX (chartCenterY + 12) theme.smallFont theme.textMuted)
      | some label, none =>
        cmds.push (.fillText label chartCenterX (chartCenterY + 4) theme.font theme.text)
      | none, some value =>
        cmds.push (.fillText value chartCenterX (chartCenterY + 4) theme.font theme.text)
      | none, none => cmds

    -- Draw legend
    let legendX := rect.x + dims.outerRadius * 2 + 50
    let legendStartY := rect.y + 20
    let legendItemHeight : Float := 24.0
    let swatchSize : Float := 14.0

    let cmds := Id.run do
      let mut cmds := cmds

      for i in [0:slices.size] do
        let slice := slices[i]!
        let proportion := slice.value / total
        let color := slice.color.getD (colors[i % colors.size]!)
        let itemY := legendStartY + i.toFloat * legendItemHeight

        -- Color swatch
        let swatchRect := Arbor.Rect.mk' legendX itemY swatchSize swatchSize
        cmds := cmds.push (.fillRect swatchRect color 2.0)

        -- Label text
        let labelX := legendX + swatchSize + 8
        let labelY := itemY + swatchSize / 2 + 4

        let labelText := match slice.label with
          | some label => s!"{label} ({formatPercent proportion})"
          | none => formatPercent proportion

        cmds := cmds.push (.fillText labelText labelX labelY theme.smallFont theme.text)

      cmds

    cmds

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
    minWidth := some dims.width
    minHeight := some dims.height
  }
  let props : Trellis.FlexContainer := { Trellis.FlexContainer.column 0 with alignItems := .flexStart }
  pure (.flex wid (some name) props {} #[chart])

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
    minWidth := some (dims.width + 120)
    minHeight := some dims.height
  }
  let props : Trellis.FlexContainer := { Trellis.FlexContainer.column 0 with alignItems := .flexStart }
  pure (.flex wid (some name) props {} #[chart])

/-! ## Reactive DonutChart Components (FRP-based)

These use WidgetM for declarative composition.
-/

open Reactive Reactive.Host
open Afferent.Canopy.Reactive

/-- DonutChart result - provides access to chart state. -/
structure DonutChartResult where
  /-- The slices being displayed. -/
  slices : Reactive.Dynamic Spider (Array DonutChart.Slice)

/-- Create a donut chart component using WidgetM.
    Displays a static donut chart with the given slices.
    - `slices`: Array of donut slices with values, labels, and optional colors
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def donutChart (slices : Array DonutChart.Slice)
    (theme : Theme) (dims : DonutChart.Dimensions := DonutChart.defaultDimensions)
    : WidgetM DonutChartResult := do
  let name ← registerComponentW "donut-chart" (isInteractive := false)

  let slicesDyn ← Dynamic.pureM slices

  emit do
    pure (donutChartVisual name slices theme dims)

  pure { slices := slicesDyn }

/-- Create a donut chart with legend component using WidgetM.
    - `slices`: Array of donut slices
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def donutChartWithLegend (slices : Array DonutChart.Slice)
    (theme : Theme) (dims : DonutChart.Dimensions := DonutChart.defaultDimensions)
    : WidgetM DonutChartResult := do
  let name ← registerComponentW "donut-chart" (isInteractive := false)

  let slicesDyn ← Dynamic.pureM slices

  emit do
    pure (donutChartWithLegendVisual name slices theme dims)

  pure { slices := slicesDyn }

/-- Create a donut chart that updates based on an external event stream.
    - `initialSlices`: Initial slice data
    - `sliceUpdates`: Event stream of slice updates
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def donutChartWithEvents (initialSlices : Array DonutChart.Slice)
    (sliceUpdates : Reactive.Event Spider (Array DonutChart.Slice))
    (theme : Theme) (dims : DonutChart.Dimensions := DonutChart.defaultDimensions)
    : WidgetM DonutChartResult := do
  let name ← registerComponentW "donut-chart" (isInteractive := false)

  let slicesDyn ← Reactive.holdDyn initialSlices sliceUpdates

  emit do
    let s ← slicesDyn.sample
    pure (donutChartVisual name s theme dims)

  pure { slices := slicesDyn }

/-- Helper to create slices from simple value/label pairs. -/
def DonutChart.Slice.fromPairs (pairs : Array (Float × String)) : Array DonutChart.Slice :=
  pairs.map fun (value, label) => { value, label := some label }

end Afferent.Canopy

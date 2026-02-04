/-
  Canopy MathPlot Widget
  Line/scatter plot with optional function sampling and configurable axes.
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

namespace MathPlot

/-- Dimensions and styling for math plot rendering. -/
structure Dimensions extends AxisChartDimensions where
  height := 300.0
  lineWidth : Float := 2.0
  pointRadius : Float := 4.0
  showMarkers : Bool := true
  showAxisLabels : Bool := true
  showAxes : Bool := true
deriving Repr, Inhabited

/-- Default math plot dimensions. -/
def defaultDimensions : Dimensions := {}

/-- A single data point with X and Y coordinates. -/
structure Point where
  x : Float
  y : Float
deriving Repr, Inhabited, BEq

/-- Series rendering kind. -/
inductive SeriesKind where
  | line
  | scatter
deriving Repr, Inhabited, BEq

/-- Series style overrides. -/
structure SeriesStyle where
  kind : SeriesKind := .line
  lineWidth : Option Float := none
  pointRadius : Option Float := none
  showMarkers : Option Bool := none
deriving Repr, Inhabited

/-- Data series for math plots. -/
structure Series where
  points : Array Point
  color : Option Color := none
  label : Option String := none
  style : SeriesStyle := {}
deriving Repr, Inhabited

/-- Axis range override. -/
structure AxisRange where
  min : Option Float := none
  max : Option Float := none
deriving Repr, Inhabited

/-- Math plot configuration for axes and labels. -/
structure Config where
  xRange : AxisRange := {}
  yRange : AxisRange := {}
  xLabel : Option String := none
  yLabel : Option String := none
deriving Repr, Inhabited

/-- Function sampling specification. -/
structure FunctionSpec where
  f : Float → Float
  xMin : Float
  xMax : Float
  samples : Nat := 200

/-- Sample a function into plot points. -/
def sampleFunction (spec : FunctionSpec) : Array Point := Id.run do
  let count := max 2 spec.samples
  let span := spec.xMax - spec.xMin
  let step := if count > 1 then span / (count - 1).toFloat else 0.0
  let mut pts : Array Point := #[]
  for i in [0:count] do
    let x := spec.xMin + step * i.toFloat
    let y := spec.f x
    pts := pts.push { x, y }
  pts

/-- Build a line series from a function specification. -/
def seriesFromFunction (spec : FunctionSpec) (color : Option Color := none)
    (label : Option String := none) (style : SeriesStyle := {}) : Series :=
  { points := sampleFunction spec, color := color, label := label, style := style }

/-- Collect data bounds across all series. -/
def dataBounds (series : Array Series) : Float × Float × Float × Float := Id.run do
  if series.isEmpty then
    (0.0, 1.0, 0.0, 1.0)
  else
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

/-- Custom spec for math plot rendering. -/
def mathPlotSpec (series : Array Series) (theme : Theme)
    (dims : Dimensions := defaultDimensions) (config : Config := {}) : CustomSpec := {
  measure := fun _ _ => (dims.marginLeft + dims.marginRight + 50, dims.marginTop + dims.marginBottom + 30)
  collect := fun layout =>
    let rect := layout.contentRect

    -- Use actual allocated size for responsive layout
    let actualWidth := rect.width
    let actualHeight := rect.height

    -- Calculate chart area
    let chartX := rect.x + dims.marginLeft
    let chartY := rect.y + dims.marginTop
    let chartWidth := actualWidth - dims.marginLeft - dims.marginRight
    let chartHeight := actualHeight - dims.marginTop - dims.marginBottom

    let (minX0, maxX0, minY0, maxY0) := dataBounds series
    let minX := config.xRange.min.getD minX0
    let maxX := config.xRange.max.getD maxX0
    let minY := config.yRange.min.getD minY0
    let maxY := config.yRange.max.getD maxY0

    let (niceMinX, niceMaxX) :=
      if config.xRange.min.isSome || config.xRange.max.isSome then (minX, maxX)
      else ChartUtils.niceAxisBounds minX maxX
    let (niceMinY, niceMaxY) :=
      if config.yRange.min.isSome || config.yRange.max.isSome then (minY, maxY)
      else ChartUtils.niceAxisBounds minY maxY

    let rangeX :=
      let r := niceMaxX - niceMinX
      if r <= 0.0 then 1.0 else r
    let rangeY :=
      let r := niceMaxY - niceMinY
      if r <= 0.0 then 1.0 else r

    let colors := ChartUtils.defaultColors theme
    let axisColor := Color.gray 0.5

    RenderM.build do
      -- Background
      RenderM.fillRect' rect.x rect.y actualWidth actualHeight (theme.panel.background.withAlpha 0.3) 6.0

      -- Grid lines
      if dims.showGridLines && dims.gridLineCount > 0 then
        for i in [0:dims.gridLineCount + 1] do
          let ratio := i.toFloat / dims.gridLineCount.toFloat
          let lineY := chartY + chartHeight - (ratio * chartHeight)
          RenderM.fillRect' chartX lineY chartWidth 1.0 (Color.gray 0.3) 0.0
        for i in [0:dims.gridLineCount + 1] do
          let ratio := i.toFloat / dims.gridLineCount.toFloat
          let lineX := chartX + (ratio * chartWidth)
          RenderM.fillRect' lineX chartY 1.0 chartHeight (Color.gray 0.3) 0.0

      -- Series rendering
      for si in [0:series.size] do
        let s := series[si]!
        let color := s.color.getD (colors[si % colors.size]!)
        let lineWidth := s.style.lineWidth.getD dims.lineWidth
        let pointRadius := s.style.pointRadius.getD dims.pointRadius
        let showMarkers := s.style.showMarkers.getD dims.showMarkers

        match s.style.kind with
        | .line =>
            if s.points.size > 0 then
              let path := Id.run do
                let mut path := Afferent.Path.empty
                for i in [0:s.points.size] do
                  let p := s.points[i]!
                  let px := chartX + ((p.x - niceMinX) / rangeX) * chartWidth
                  let py := chartY + chartHeight - ((p.y - niceMinY) / rangeY) * chartHeight
                  let pt := Arbor.Point.mk' px py
                  if i == 0 then
                    path := path.moveTo pt
                  else
                    path := path.lineTo pt
                path
              RenderM.strokePath path color lineWidth
              if showMarkers then
                for p in s.points do
                  let px := chartX + ((p.x - niceMinX) / rangeX) * chartWidth
                  let py := chartY + chartHeight - ((p.y - niceMinY) / rangeY) * chartHeight
                  RenderM.fillCircle' px py pointRadius color
        | .scatter =>
            for p in s.points do
              let px := chartX + ((p.x - niceMinX) / rangeX) * chartWidth
              let py := chartY + chartHeight - ((p.y - niceMinY) / rangeY) * chartHeight
              RenderM.fillCircle' px py pointRadius color

      -- Axis tick labels
      if dims.showAxisLabels && dims.gridLineCount > 0 then
        for i in [0:dims.gridLineCount + 1] do
          let ratio := i.toFloat / dims.gridLineCount.toFloat
          let valueY := niceMinY + ratio * rangeY
          let labelY := chartY + chartHeight - (ratio * chartHeight) - 6
          RenderM.fillText (ChartUtils.formatValue valueY) (rect.x + 4) labelY theme.smallFont theme.textMuted
        for i in [0:dims.gridLineCount + 1] do
          let ratio := i.toFloat / dims.gridLineCount.toFloat
          let valueX := niceMinX + ratio * rangeX
          let labelX := chartX + (ratio * chartWidth)
          let labelY := chartY + chartHeight + 16
          RenderM.fillText (ChartUtils.formatValue valueX) labelX labelY theme.smallFont theme.textMuted

      -- Axis titles
      match config.xLabel with
      | some label =>
          RenderM.fillText label (chartX + chartWidth / 2) (chartY + chartHeight + 32) theme.smallFont theme.text
      | none => pure ()
      match config.yLabel with
      | some label =>
          RenderM.fillText label (rect.x + 4) (rect.y + 4) theme.smallFont theme.text
      | none => pure ()

      -- Axes
      if dims.showAxes then
        RenderM.fillRect' chartX chartY 1.0 chartHeight axisColor 0.0
        RenderM.fillRect' chartX (chartY + chartHeight) chartWidth 1.0 axisColor 0.0

  draw := none
}

end MathPlot

/-- Build a math plot visual (WidgetBuilder version). -/
def mathPlotVisual (name : String) (series : Array MathPlot.Series) (theme : Theme)
    (dims : MathPlot.Dimensions := MathPlot.defaultDimensions)
    (config : MathPlot.Config := {}) : WidgetBuilder := do
  let wid ← freshId
  let chart ← custom (MathPlot.mathPlotSpec series theme dims config) {
    width := .percent 1.0
    height := .percent 1.0
    flexItem := some (Trellis.FlexItem.growing 1)
  }
  let style : BoxStyle := {
    width := .percent 1.0
    height := .percent 1.0
    minWidth := some dims.width
    minHeight := some dims.height
    flexItem := some (Trellis.FlexItem.growing 1)
  }
  let props : Trellis.FlexContainer := { Trellis.FlexContainer.column 0 with alignItems := .stretch }
  pure (.flex wid (some name) props style #[chart])

/-! ## Reactive MathPlot Components (FRP-based) -/

open Reactive Reactive.Host
open Afferent.Canopy.Reactive

/-- MathPlot result - provides access to chart state. -/
structure MathPlotResult where
  series : Dyn (Array MathPlot.Series)

/-- Create a math plot component using WidgetM with dynamic series.
    The plot automatically rebuilds when the series Dynamic changes. -/
def mathPlot (series : Dyn (Array MathPlot.Series))
    (dims : MathPlot.Dimensions := MathPlot.defaultDimensions)
    (config : MathPlot.Config := {}) : WidgetM MathPlotResult := do
  let theme ← getThemeW
  let _ ← dynWidget series fun currentSeries => do
    let name ← registerComponentW "math-plot" (isInteractive := false)
    emit do pure (mathPlotVisual name currentSeries theme dims config)
  pure { series }

end Afferent.Canopy

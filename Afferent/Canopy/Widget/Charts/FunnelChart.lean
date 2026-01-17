/-
  Canopy FunnelChart Widget
  Funnel chart for showing stages in a pipeline or conversion process.
-/
import Reactive
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Reactive.Component
import Afferent.Canopy.Widget.Charts.Core

namespace Afferent.Canopy

open Afferent.Arbor hiding Event

namespace FunnelChart

/-- Dimensions and styling for funnel chart rendering. -/
structure Dimensions extends ChartSize, ChartMargins where
  width := 350.0
  height := 280.0
  marginBottom := 20.0
  marginLeft := 20.0
  marginRight := 120.0  -- Space for labels
  stageGap : Float := 4.0
  minBottomWidth : Float := 60.0  -- Minimum width at bottom of funnel
  showLabels : Bool := true
  showValues : Bool := true
  showPercentages : Bool := true
deriving Repr, Inhabited

/-- Default funnel chart dimensions. -/
def defaultDimensions : Dimensions := {}

/-- A single stage in the funnel. -/
structure Stage where
  /-- Label for this stage. -/
  label : String
  /-- Value for this stage. -/
  value : Float
  /-- Optional color (uses default palette if none). -/
  color : Option Color := none
deriving Repr, Inhabited

/-- Funnel chart data. -/
structure Data where
  /-- Stages in the funnel (typically in decreasing order). -/
  stages : Array Stage
deriving Repr, Inhabited

/-- Default stage colors. -/
def defaultColors : Array Color := #[
  Color.rgba 0.29 0.53 0.91 1.0,   -- Blue
  Color.rgba 0.20 0.69 0.35 1.0,   -- Green
  Color.rgba 0.95 0.61 0.07 1.0,   -- Orange
  Color.rgba 0.84 0.24 0.29 1.0,   -- Red
  Color.rgba 0.58 0.40 0.74 1.0,   -- Purple
  Color.rgba 0.55 0.34 0.29 1.0,   -- Brown
  Color.rgba 0.89 0.47 0.76 1.0,   -- Pink
  Color.rgba 0.50 0.50 0.50 1.0    -- Gray
]

/-- Get color for a stage index. -/
def getStageColor (stage : Stage) (idx : Nat) : Color :=
  stage.color.getD (defaultColors[idx % defaultColors.size]!)

/-- Format a value for display. -/
private def formatValue (v : Float) : String :=
  if v >= 1000000 then
    s!"{(v / 1000000).floor.toUInt32}M"
  else if v >= 1000 then
    s!"{(v / 1000).floor.toUInt32}K"
  else if v == v.floor then
    s!"{v.floor.toUInt32}"
  else
    let whole := v.floor.toUInt32
    let frac := ((v - v.floor) * 10).floor.toUInt32
    s!"{whole}.{frac}"

/-- Format a percentage. -/
private def formatPercentage (frac : Float) : String :=
  let pct := (frac * 100).floor.toUInt32
  s!"{pct}%"

/-- Custom spec for funnel chart rendering. -/
def funnelChartSpec (data : Data) (theme : Theme)
    (dims : Dimensions := defaultDimensions) : CustomSpec := {
  measure := fun _ _ => (dims.marginLeft + dims.marginRight + 60, dims.marginTop + dims.marginBottom + 40)
  collect := fun layout => RenderM.build do
    let rect := layout.contentRect
    let actualWidth := rect.width
    let actualHeight := rect.height

    let numStages := data.stages.size
    if numStages == 0 then return

    -- Find maximum value for scaling widths
    let maxValue := data.stages.foldl (fun acc s => max acc s.value) 0.0
    let maxValue := if maxValue <= 0.0 then 1.0 else maxValue

    -- Calculate funnel area
    let funnelX := rect.x + dims.marginLeft
    let funnelY := rect.y + dims.marginTop
    let funnelWidth := actualWidth - dims.marginLeft - dims.marginRight
    let funnelHeight := actualHeight - dims.marginTop - dims.marginBottom

    -- Calculate stage height
    let totalGapHeight := dims.stageGap * (numStages - 1).toFloat
    let stageHeight := (funnelHeight - totalGapHeight) / numStages.toFloat

    -- Center X of funnel
    let centerX := funnelX + funnelWidth / 2

    -- Draw background
    let bgRect := Arbor.Rect.mk' rect.x rect.y actualWidth actualHeight
    RenderM.fillRect bgRect (theme.panel.background.withAlpha 0.3) 6.0

    -- Draw stages as trapezoids
    for i in [0:numStages] do
      let stage := data.stages[i]!
      let color := getStageColor stage i

      -- Calculate width based on value (proportional to max)
      let valueFrac := stage.value / maxValue
      let stageWidth := max dims.minBottomWidth (funnelWidth * valueFrac)

      -- Calculate next stage width (for trapezoid bottom)
      let nextWidth := if i + 1 < numStages then
        let nextStage := data.stages[i + 1]!
        let nextFrac := nextStage.value / maxValue
        max dims.minBottomWidth (funnelWidth * nextFrac)
      else
        dims.minBottomWidth

      -- Stage Y position
      let stageY := funnelY + i.toFloat * (stageHeight + dims.stageGap)

      -- Calculate trapezoid corners
      let topLeft := Arbor.Point.mk' (centerX - stageWidth / 2) stageY
      let topRight := Arbor.Point.mk' (centerX + stageWidth / 2) stageY
      let bottomRight := Arbor.Point.mk' (centerX + nextWidth / 2) (stageY + stageHeight)
      let bottomLeft := Arbor.Point.mk' (centerX - nextWidth / 2) (stageY + stageHeight)

      -- Draw trapezoid as path
      let trapPath := Arbor.Path.empty
        |>.moveTo topLeft
        |>.lineTo topRight
        |>.lineTo bottomRight
        |>.lineTo bottomLeft
        |>.closePath

      RenderM.fillPath trapPath color

      -- Draw label and value on the right side
      if dims.showLabels then
        let labelX := rect.x + actualWidth - dims.marginRight + 10
        let labelY := stageY + stageHeight / 2

        -- Stage label
        RenderM.fillText stage.label labelX labelY theme.smallFont theme.text

        -- Value and/or percentage
        if dims.showValues || dims.showPercentages then
          let valueY := labelY + 14
          let valueStr := if dims.showValues && dims.showPercentages then
            s!"{formatValue stage.value} ({formatPercentage valueFrac})"
          else if dims.showValues then
            formatValue stage.value
          else
            formatPercentage valueFrac
          RenderM.fillText valueStr labelX valueY theme.smallFont theme.textMuted

  draw := none
}

end FunnelChart

/-- Build a funnel chart visual (WidgetBuilder version).
    - `name`: Widget name for identification
    - `data`: Funnel chart data with stages
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def funnelChartVisual (name : String) (data : FunnelChart.Data)
    (theme : Theme) (dims : FunnelChart.Dimensions := FunnelChart.defaultDimensions)
    : WidgetBuilder := do
  let wid ← freshId
  let chart ← custom (FunnelChart.funnelChartSpec data theme dims) {
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

/-! ## Reactive FunnelChart Components (FRP-based)

These use WidgetM for declarative composition.
-/

open Reactive Reactive.Host
open Afferent.Canopy.Reactive

/-- FunnelChart result - provides access to chart state. -/
structure FunnelChartResult where
  /-- The data being displayed. -/
  data : Reactive.Dynamic Spider FunnelChart.Data

/-- Create a funnel chart component using WidgetM.
    - `data`: Funnel chart data with stages
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def funnelChart (data : FunnelChart.Data)
    (theme : Theme) (dims : FunnelChart.Dimensions := FunnelChart.defaultDimensions)
    : WidgetM FunnelChartResult := do
  let name ← registerComponentW "funnel-chart" (isInteractive := false)

  let dataDyn ← Dynamic.pureM data

  emit do
    pure (funnelChartVisual name data theme dims)

  pure { data := dataDyn }

/-- Create a funnel chart from simple arrays.
    - `labels`: Labels for each stage
    - `values`: Values for each stage
    - `colors`: Optional colors for each stage
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def funnelChartFromArrays (labels : Array String) (values : Array Float)
    (colors : Array Color := #[])
    (theme : Theme) (dims : FunnelChart.Dimensions := FunnelChart.defaultDimensions)
    : WidgetM FunnelChartResult := do
  let numStages := min labels.size values.size
  let stages := Id.run do
    let mut result : Array FunnelChart.Stage := #[]
    for i in [0:numStages] do
      let color := if i < colors.size then some colors[i]! else none
      result := result.push {
        label := labels[i]!
        value := values[i]!
        color
      }
    result
  let data : FunnelChart.Data := { stages }
  funnelChart data theme dims

end Afferent.Canopy

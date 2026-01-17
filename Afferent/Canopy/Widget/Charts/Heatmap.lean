/-
  Canopy Heatmap Widget
  2D color-coded matrix visualization for showing data intensity patterns.
-/
import Reactive
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Reactive.Component
import Afferent.Canopy.Widget.Charts.Core

namespace Afferent.Canopy

open Afferent.Arbor hiding Event

namespace Heatmap

/-- Color scale for heatmap values. -/
inductive ColorScale where
  /-- Blue to red through white (diverging, good for correlations). -/
  | blueWhiteRed
  /-- White to blue (sequential, good for counts). -/
  | whiteBlue
  /-- White to red (sequential, good for intensity). -/
  | whiteRed
  /-- White to green (sequential). -/
  | whiteGreen
  /-- Yellow to red (sequential, warm). -/
  | yellowRed
  /-- Green to yellow to red (traffic light). -/
  | greenYellowRed
  /-- Viridis-like (perceptually uniform). -/
  | viridis
  /-- Custom two-color gradient. -/
  | custom (low high : Color)
deriving Repr, Inhabited, BEq

/-- Dimensions and styling for heatmap rendering. -/
structure Dimensions extends ChartSize, ChartMargins where
  height := 300.0
  marginTop := 40.0
  marginBottom := 20.0
  marginLeft := 60.0
  cellGap : Float := 1.0
  cornerRadius : Float := 2.0
  showValues : Bool := false
  showRowLabels : Bool := true
  showColumnLabels : Bool := true
  showColorBar : Bool := true
  colorBarWidth : Float := 20.0
deriving Repr, Inhabited

/-- Default heatmap dimensions. -/
def defaultDimensions : Dimensions := {}

/-- Heatmap data structure. -/
structure Data where
  /-- 2D array of values (row-major: data[row][col]). -/
  values : Array (Array Float)
  /-- Optional row labels. -/
  rowLabels : Array String := #[]
  /-- Optional column labels. -/
  columnLabels : Array String := #[]
  /-- Optional minimum value for scaling (auto-computed if none). -/
  minValue : Option Float := none
  /-- Optional maximum value for scaling (auto-computed if none). -/
  maxValue : Option Float := none
deriving Repr, Inhabited, BEq

/-- Interpolate between two colors. -/
private def lerpColor (c1 c2 : Color) (t : Float) : Color :=
  let t := max 0.0 (min 1.0 t)
  Color.rgba
    (c1.r + t * (c2.r - c1.r))
    (c1.g + t * (c2.g - c1.g))
    (c1.b + t * (c2.b - c1.b))
    (c1.a + t * (c2.a - c1.a))

/-- Get color for a normalized value (0 to 1) based on color scale. -/
def colorForValue (scale : ColorScale) (t : Float) : Color :=
  let t := max 0.0 (min 1.0 t)
  match scale with
  | .blueWhiteRed =>
    if t < 0.5 then
      lerpColor (Color.rgba 0.0 0.0 1.0 1.0) (Color.rgba 1.0 1.0 1.0 1.0) (t * 2)
    else
      lerpColor (Color.rgba 1.0 1.0 1.0 1.0) (Color.rgba 1.0 0.0 0.0 1.0) ((t - 0.5) * 2)
  | .whiteBlue =>
    lerpColor (Color.rgba 1.0 1.0 1.0 1.0) (Color.rgba 0.0 0.3 0.8 1.0) t
  | .whiteRed =>
    lerpColor (Color.rgba 1.0 1.0 1.0 1.0) (Color.rgba 0.8 0.0 0.0 1.0) t
  | .whiteGreen =>
    lerpColor (Color.rgba 1.0 1.0 1.0 1.0) (Color.rgba 0.0 0.6 0.2 1.0) t
  | .yellowRed =>
    lerpColor (Color.rgba 1.0 1.0 0.4 1.0) (Color.rgba 0.8 0.0 0.0 1.0) t
  | .greenYellowRed =>
    if t < 0.5 then
      lerpColor (Color.rgba 0.0 0.7 0.2 1.0) (Color.rgba 1.0 1.0 0.0 1.0) (t * 2)
    else
      lerpColor (Color.rgba 1.0 1.0 0.0 1.0) (Color.rgba 0.8 0.0 0.0 1.0) ((t - 0.5) * 2)
  | .viridis =>
    -- Simplified viridis approximation
    if t < 0.25 then
      lerpColor (Color.rgba 0.27 0.0 0.33 1.0) (Color.rgba 0.28 0.36 0.51 1.0) (t * 4)
    else if t < 0.5 then
      lerpColor (Color.rgba 0.28 0.36 0.51 1.0) (Color.rgba 0.13 0.57 0.55 1.0) ((t - 0.25) * 4)
    else if t < 0.75 then
      lerpColor (Color.rgba 0.13 0.57 0.55 1.0) (Color.rgba 0.55 0.76 0.22 1.0) ((t - 0.5) * 4)
    else
      lerpColor (Color.rgba 0.55 0.76 0.22 1.0) (Color.rgba 0.99 0.91 0.15 1.0) ((t - 0.75) * 4)
  | .custom low high =>
    lerpColor low high t

/-- Format a float value for display. -/
private def formatValue (v : Float) : String :=
  if v == v.floor then
    s!"{v.floor.toInt32}"
  else
    let whole := v.floor.toInt32
    let frac := ((v - v.floor).abs * 10).floor.toUInt32
    s!"{whole}.{frac}"

/-- Custom spec for heatmap rendering. -/
def heatmapSpec (data : Data) (scale : ColorScale) (theme : Theme)
    (dims : Dimensions := defaultDimensions) : CustomSpec := {
  measure := fun _ _ => (dims.marginLeft + dims.marginRight + 50, dims.marginTop + dims.marginBottom + 30)
  collect := fun layout =>
    let rect := layout.contentRect

    -- Use actual container size for responsive layout
    let actualWidth := rect.width
    let actualHeight := rect.height

    -- Calculate chart area
    let colorBarSpace := if dims.showColorBar then dims.colorBarWidth + 10 else 0
    let chartX := rect.x + dims.marginLeft
    let chartY := rect.y + dims.marginTop
    let chartWidth := actualWidth - dims.marginLeft - dims.marginRight - colorBarSpace
    let chartHeight := actualHeight - dims.marginTop - dims.marginBottom

    -- Get matrix dimensions
    let numRows := data.values.size
    if numRows == 0 then #[] else
    let numCols := data.values.foldl (fun acc row => max acc row.size) 0
    if numCols == 0 then #[] else

    -- Find value range
    let (minVal, maxVal) := Id.run do
      let mut minV := match data.minValue with | some v => v | none => 0.0
      let mut maxV := match data.maxValue with | some v => v | none => 0.0
      let mut initialized := data.minValue.isSome && data.maxValue.isSome
      if !initialized then
        for row in data.values do
          for v in row do
            if !initialized then
              minV := v
              maxV := v
              initialized := true
            else
              if v < minV then minV := v
              if v > maxV then maxV := v
      (minV, maxV)

    let valueRange := maxVal - minVal
    let normalizeValue := fun v =>
      if valueRange <= 0.0 then 0.5
      else (v - minVal) / valueRange

    -- Calculate cell size
    let totalGapWidth := dims.cellGap * (numCols - 1).toFloat
    let totalGapHeight := dims.cellGap * (numRows - 1).toFloat
    let cellWidth := (chartWidth - totalGapWidth) / numCols.toFloat
    let cellHeight := (chartHeight - totalGapHeight) / numRows.toFloat

    RenderM.build do
      -- Draw background
      RenderM.fillRect' rect.x rect.y actualWidth actualHeight (theme.panel.background.withAlpha 0.3) 6.0

      -- Draw cells
      for ri in [0:numRows] do
        let row := data.values[ri]!
        for ci in [0:numCols] do
          let value := if ci < row.size then row[ci]! else 0.0
          let normalized := normalizeValue value
          let color := colorForValue scale normalized

          let cellX := chartX + ci.toFloat * (cellWidth + dims.cellGap)
          let cellY := chartY + ri.toFloat * (cellHeight + dims.cellGap)
          RenderM.fillRect' cellX cellY cellWidth cellHeight color dims.cornerRadius

          -- Optionally show value in cell
          if dims.showValues && cellWidth > 20 && cellHeight > 15 then
            let textColor := if normalized > 0.5 then Color.white else Color.black
            let labelX := cellX + cellWidth / 2
            let labelY := cellY + cellHeight / 2 + 4
            RenderM.fillText (formatValue value) labelX labelY theme.smallFont textColor

      -- Draw row labels
      if dims.showRowLabels && data.rowLabels.size > 0 then
        for ri in [0:min numRows data.rowLabels.size] do
          let label := data.rowLabels[ri]!
          let labelY := chartY + ri.toFloat * (cellHeight + dims.cellGap) + cellHeight / 2 + 4
          RenderM.fillText label (rect.x + 4) labelY theme.smallFont theme.text

      -- Draw column labels
      if dims.showColumnLabels && data.columnLabels.size > 0 then
        for ci in [0:min numCols data.columnLabels.size] do
          let label := data.columnLabels[ci]!
          let labelX := chartX + ci.toFloat * (cellWidth + dims.cellGap) + cellWidth / 2
          let labelY := rect.y + dims.marginTop - 8
          RenderM.fillText label labelX labelY theme.smallFont theme.text

      -- Draw color bar
      if dims.showColorBar then
        let barX := chartX + chartWidth + 10
        let barY := chartY
        let barHeight := chartHeight
        let numSteps := 20

        -- Draw gradient bar
        for i in [0:numSteps] do
          let t := i.toFloat / (numSteps - 1).toFloat
          let color := colorForValue scale (1.0 - t)  -- Invert so high values at top
          let stepHeight := barHeight / numSteps.toFloat
          let stepY := barY + i.toFloat * stepHeight
          RenderM.fillRect' barX stepY dims.colorBarWidth stepHeight color 0.0

        -- Draw min/max labels
        RenderM.fillText (formatValue maxVal) (barX + dims.colorBarWidth + 4) (barY + 4) theme.smallFont theme.textMuted
        RenderM.fillText (formatValue minVal) (barX + dims.colorBarWidth + 4) (barY + barHeight + 4) theme.smallFont theme.textMuted

  draw := none
}

end Heatmap

/-- Build a heatmap visual (WidgetBuilder version).
    - `name`: Widget name for identification
    - `data`: Heatmap data with values and labels
    - `scale`: Color scale to use
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def heatmapVisual (name : String) (data : Heatmap.Data) (scale : Heatmap.ColorScale)
    (theme : Theme) (dims : Heatmap.Dimensions := Heatmap.defaultDimensions)
    : WidgetBuilder := do
  let wid ← freshId
  let chart ← custom (Heatmap.heatmapSpec data scale theme dims) {
    width := .percent 1.0
    height := .percent 1.0
    flexItem := some (Trellis.FlexItem.growing 1)
  }
  let style : BoxStyle := { width := .percent 1.0, height := .percent 1.0, minWidth := some dims.width, minHeight := some dims.height, flexItem := some (Trellis.FlexItem.growing 1) }
  let props : Trellis.FlexContainer := { Trellis.FlexContainer.column 0 with alignItems := .stretch }
  pure (.flex wid (some name) props style #[chart])

/-! ## Reactive Heatmap Components (FRP-based)

These use WidgetM for declarative composition.
-/

open Reactive Reactive.Host
open Afferent.Canopy.Reactive

/-- Heatmap result - provides access to chart state. -/
structure HeatmapResult where
  /-- The data being displayed. -/
  data : Dyn Heatmap.Data

/-- Create a heatmap component using WidgetM with dynamic data.
    The chart automatically rebuilds when the data Dynamic changes.
    - `data`: Dynamic heatmap data with 2D values and optional labels
    - `scale`: Color scale for mapping values to colors
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def heatmap (data : Dyn Heatmap.Data) (scale : Heatmap.ColorScale := .viridis)
    (theme : Theme) (dims : Heatmap.Dimensions := Heatmap.defaultDimensions)
    : WidgetM HeatmapResult := do
  let _ ← dynWidget data fun currentData => do
    let name ← registerComponentW "heatmap" (isInteractive := false)
    emit do pure (heatmapVisual name currentData scale theme dims)

  pure { data }

/-- Create a heatmap from a dynamic 2D array of values.
    - `values`: Dynamic 2D array of values (row-major)
    - `rowLabels`: Optional row labels
    - `columnLabels`: Optional column labels
    - `scale`: Color scale
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def heatmapFromValues (values : Dyn (Array (Array Float)))
    (rowLabels : Array String := #[]) (columnLabels : Array String := #[])
    (scale : Heatmap.ColorScale := .viridis)
    (theme : Theme) (dims : Heatmap.Dimensions := Heatmap.defaultDimensions)
    : WidgetM HeatmapResult := do
  let dataDyn ← Dynamic.mapM (fun currentValues =>
    ({ values := currentValues, rowLabels, columnLabels } : Heatmap.Data)
  ) values
  heatmap dataDyn scale theme dims

/-- Create a correlation matrix heatmap with dynamic values (uses blue-white-red scale centered at 0).
    - `values`: Dynamic 2D array of correlation values (-1 to 1)
    - `labels`: Labels for both rows and columns
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def correlationMatrix (values : Dyn (Array (Array Float))) (labels : Array String := #[])
    (theme : Theme) (dims : Heatmap.Dimensions := Heatmap.defaultDimensions)
    : WidgetM HeatmapResult := do
  let dataDyn ← Dynamic.mapM (fun currentValues =>
    ({ values := currentValues, rowLabels := labels, columnLabels := labels,
       minValue := some (-1.0), maxValue := some 1.0 } : Heatmap.Data)
  ) values
  heatmap dataDyn .blueWhiteRed theme dims

end Afferent.Canopy

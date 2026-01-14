/-
  Canopy Chart Utilities
  Shared utilities for chart rendering: colors, formatting, legends.
-/
import Afferent.Canopy.Core
import Afferent.Canopy.Theme

namespace Afferent.Canopy.ChartUtils

open Afferent.Arbor hiding Event

/-- Default color palette for chart series. -/
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

/-- Get a color from the palette by index, cycling if needed. -/
def getColor (theme : Theme) (idx : Nat) (custom : Option Color := none) : Color :=
  custom.getD ((defaultColors theme)[idx % (defaultColors theme).size]!)

/-- Format a float value for display (axis labels, tooltips).
    Handles large numbers (K, M) and decimal places. -/
def formatValue (v : Float) : String :=
  if v >= 1000000 then
    s!"{(v / 1000000).floor.toUInt32}M"
  else if v >= 1000 then
    s!"{(v / 1000).floor.toUInt32}K"
  else if v == v.floor then
    s!"{v.floor.toUInt32}"
  else
    let whole := v.floor.toInt32
    let frac := ((v - v.floor).abs * 10).floor.toUInt32
    s!"{whole}.{frac}"

/-- Format a proportion (0.0-1.0) as a percentage string. -/
def formatPercent (v : Float) : String :=
  let pct := (v * 100).floor.toUInt32
  s!"{pct}%"

/-- Calculate a nice maximum value for axis scaling.
    Rounds up to common intervals (10, 50, 100, 500, 1000, etc). -/
def niceMax (maxVal : Float) : Float :=
  if maxVal <= 0.0 then 1.0
  else if maxVal <= 10 then 10.0
  else if maxVal <= 50 then 50.0
  else if maxVal <= 100 then 100.0
  else if maxVal <= 500 then 500.0
  else if maxVal <= 1000 then 1000.0
  else (maxVal / 100).ceil * 100

/-- Calculate nice axis bounds (min, max) for scaling with padding. -/
def niceAxisBounds (minVal maxVal : Float) : Float × Float :=
  let range := maxVal - minVal
  if range <= 0.0 then (minVal - 1.0, maxVal + 1.0)
  else
    let padding := range * 0.1
    let niceMin := if minVal >= 0.0 then 0.0 else minVal - padding
    let niceMax := maxVal + padding
    (niceMin, niceMax)

/-- Configuration for legend rendering. -/
structure LegendConfig where
  /-- Size of the color swatch square. -/
  swatchSize : Float := 14.0
  /-- Height of each legend item row. -/
  itemHeight : Float := 24.0
  /-- Spacing between swatch and label. -/
  spacing : Float := 8.0
  /-- Corner radius for swatch rectangles. -/
  cornerRadius : Float := 2.0
deriving Repr, Inhabited

/-- Default legend configuration. -/
def defaultLegendConfig : LegendConfig := {}

/-- A single item in a chart legend. -/
structure LegendItem where
  /-- Display label for this item. -/
  label : String
  /-- Color swatch color. -/
  color : Color
  /-- Optional suffix (e.g., percentage, value). -/
  suffix : Option String := none
deriving Repr, Inhabited

/-- Create legend items from labels and colors with optional percentages. -/
def makeLegendItems (labels : Array (Option String)) (colors : Array Color)
    (percentages : Array Float := #[]) : Array LegendItem := Id.run do
  let mut result : Array LegendItem := #[]
  for h : idx in [0:labels.size] do
    let labelOpt := labels[idx]
    let label := labelOpt.getD s!"Item {idx + 1}"
    let color := if idx < colors.size then colors[idx]! else Color.gray 0.5
    let suffix := if idx < percentages.size then some (formatPercent percentages[idx]!) else none
    result := result.push { label, color, suffix }
  result

/-- Draw a vertical legend at the specified position.
    Returns the total height used by the legend. -/
def drawLegend (items : Array LegendItem) (x y : Float)
    (theme : Theme) (config : LegendConfig := defaultLegendConfig) : RenderM Float := do
  for i in [0:items.size] do
    let item := items[i]!
    let itemY := y + i.toFloat * config.itemHeight

    -- Color swatch
    RenderM.fillRect' x itemY config.swatchSize config.swatchSize item.color config.cornerRadius

    -- Label text
    let labelX := x + config.swatchSize + config.spacing
    let labelY := itemY + config.swatchSize / 2 + 4

    let labelText := match item.suffix with
      | some suffix => s!"{item.label} ({suffix})"
      | none => item.label

    RenderM.fillText labelText labelX labelY theme.smallFont theme.text

  pure (items.size.toFloat * config.itemHeight)

/-- Draw a horizontal legend at the specified position.
    Items are laid out left-to-right with spacing.
    Returns the total width used by the legend. -/
def drawLegendHorizontal (items : Array LegendItem) (x y : Float)
    (theme : Theme) (itemWidth : Float := 100.0)
    (config : LegendConfig := defaultLegendConfig) : RenderM Float := do
  for i in [0:items.size] do
    let item := items[i]!
    let itemX := x + i.toFloat * itemWidth

    -- Color swatch
    RenderM.fillRect' itemX y config.swatchSize config.swatchSize item.color config.cornerRadius

    -- Label text
    let labelX := itemX + config.swatchSize + config.spacing
    let labelY := y + config.swatchSize / 2 + 4

    let labelText := match item.suffix with
      | some suffix => s!"{item.label} ({suffix})"
      | none => item.label

    RenderM.fillText labelText labelX labelY theme.smallFont theme.text

  pure (items.size.toFloat * itemWidth)

end Afferent.Canopy.ChartUtils

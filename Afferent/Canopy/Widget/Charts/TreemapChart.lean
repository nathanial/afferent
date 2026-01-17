/-
  Canopy TreemapChart Widget
  Treemap chart for displaying hierarchical data as nested rectangles.
-/
import Reactive
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Reactive.Component
import Afferent.Canopy.Widget.Charts.Core

namespace Afferent.Canopy

open Afferent.Arbor hiding Event

namespace TreemapChart

/-- Dimensions and styling for treemap chart rendering. -/
structure Dimensions extends ChartSize where
  height := 300.0
  padding : Float := 2.0
  labelPadding : Float := 4.0
  showLabels : Bool := true
  showValues : Bool := true
  maxDepth : Nat := 3  -- Maximum nesting depth to display
deriving Repr, Inhabited

/-- Default treemap chart dimensions. -/
def defaultDimensions : Dimensions := {}

/-- A node in the treemap hierarchy. -/
structure TreeNode where
  /-- Label for this node. -/
  label : String
  /-- Value for this node (if leaf) or 0 for branches (computed from children). -/
  value : Float := 0.0
  /-- Optional color (uses default palette if none). -/
  color : Option Color := none
  /-- Child nodes. -/
  children : Array TreeNode := #[]
deriving Repr, Inhabited

/-- Get the total value of a node (sum of children if branch, own value if leaf). -/
partial def TreeNode.totalValue (node : TreeNode) : Float :=
  if node.children.isEmpty then
    node.value
  else
    node.children.foldl (fun acc child => acc + child.totalValue) 0.0

/-- Treemap chart data. -/
structure Data where
  /-- Root nodes of the treemap. -/
  nodes : Array TreeNode
deriving Repr, Inhabited

/-- Default node colors. -/
def defaultColors : Array Color := #[
  Color.rgba 0.29 0.53 0.91 1.0,   -- Blue
  Color.rgba 0.20 0.69 0.35 1.0,   -- Green
  Color.rgba 0.95 0.61 0.07 1.0,   -- Orange
  Color.rgba 0.84 0.24 0.29 1.0,   -- Red
  Color.rgba 0.58 0.40 0.74 1.0,   -- Purple
  Color.rgba 0.55 0.34 0.29 1.0,   -- Brown
  Color.rgba 0.89 0.47 0.76 1.0,   -- Pink
  Color.rgba 0.00 0.73 0.73 1.0    -- Cyan
]

/-- Get color for a node index. -/
def getNodeColor (node : TreeNode) (idx : Nat) : Color :=
  node.color.getD (defaultColors[idx % defaultColors.size]!)

/-- Darken a color for nested levels. -/
def darkenForDepth (c : Color) (depth : Nat) : Color :=
  let factor := 1.0 - (depth.toFloat * 0.15)
  let factor := max 0.4 factor
  Color.rgba (c.r * factor) (c.g * factor) (c.b * factor) c.a

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

/-- Rectangle for layout calculations. -/
private structure LayoutRect where
  x : Float
  y : Float
  w : Float
  h : Float
deriving Repr

/-- Item with value for squarify algorithm. -/
private structure LayoutItem where
  node : TreeNode
  value : Float
  colorIdx : Nat
deriving Repr

/-- Calculate the worst aspect ratio for a row of items in a given width. -/
private def worstRatio (items : Array LayoutItem) (w : Float) : Float :=
  if items.isEmpty || w <= 0 then 1e30 else
  let totalArea := items.foldl (fun acc item => acc + item.value) 0.0
  let h := totalArea / w
  items.foldl (fun worst item =>
    let itemW := item.value / h
    let ratio := if itemW > h then itemW / h else h / itemW
    max worst ratio
  ) 0.0

/-- Squarify algorithm - lays out rectangles with good aspect ratios. -/
private partial def squarify (items : Array LayoutItem) (rect : LayoutRect)
    : Array (LayoutItem × LayoutRect) := Id.run do
  if items.isEmpty then return #[]
  if rect.w <= 0 || rect.h <= 0 then return #[]

  -- Sort by value descending
  let sorted := items.qsort (fun a b => a.value > b.value)

  -- Total value for scaling
  let totalValue := sorted.foldl (fun acc item => acc + item.value) 0.0
  if totalValue <= 0 then return #[]

  -- Scale values to fit rectangle area
  let scale := (rect.w * rect.h) / totalValue
  let scaled := sorted.map fun item => { item with value := item.value * scale }

  -- Determine if we're laying out horizontally or vertically
  let horizontal := rect.w >= rect.h
  let shortSide := if horizontal then rect.h else rect.w

  -- Greedy algorithm: add items to current row while aspect ratio improves
  let mut result : Array (LayoutItem × LayoutRect) := #[]
  let mut remaining := scaled
  let mut currentRect := rect

  while !remaining.isEmpty do
    let mut row : Array LayoutItem := #[]
    let mut rowArea : Float := 0.0
    let mut bestWorst : Float := 1e30

    -- Add items to row while aspect ratio improves
    for item in remaining do
      let testRow := row.push item
      let testArea := rowArea + item.value
      let rowLength := testArea / shortSide
      let worst := testRow.foldl (fun w r =>
        let itemLen := r.value / rowLength
        let ratio := if itemLen > shortSide then itemLen / shortSide else shortSide / itemLen
        max w ratio
      ) 0.0

      if row.isEmpty || worst <= bestWorst then
        row := testRow
        rowArea := testArea
        bestWorst := worst
      else
        break

    -- Lay out the row
    let rowLength := if shortSide > 0 then rowArea / shortSide else 0
    let mut offset : Float := 0.0

    for item in row do
      let itemLen := if rowLength > 0 then item.value / rowLength else 0
      let itemRect := if horizontal then
        { x := currentRect.x, y := currentRect.y + offset, w := rowLength, h := itemLen : LayoutRect }
      else
        { x := currentRect.x + offset, y := currentRect.y, w := itemLen, h := rowLength : LayoutRect }
      result := result.push (item, itemRect)
      offset := offset + itemLen

    -- Update remaining items and rectangle
    remaining := remaining.extract row.size remaining.size
    currentRect := if horizontal then
      { currentRect with x := currentRect.x + rowLength, w := currentRect.w - rowLength }
    else
      { currentRect with y := currentRect.y + rowLength, h := currentRect.h - rowLength }

  result

/-- Recursively render treemap nodes. -/
private partial def renderNodes (items : Array (LayoutItem × LayoutRect))
    (theme : Theme) (dims : Dimensions) (depth : Nat)
    : RenderM Unit := do
  for (item, rect) in items do
    -- Skip tiny rectangles
    if rect.w < 2 || rect.h < 2 then continue

    let color := darkenForDepth (getNodeColor item.node item.colorIdx) depth
    let nodeRect := Arbor.Rect.mk' rect.x rect.y rect.w rect.h
    RenderM.fillRect nodeRect color 2.0

    -- Draw border
    let borderColor := color.withAlpha 0.3
    RenderM.strokeRect nodeRect borderColor 1.0

    -- Draw label if space permits
    if dims.showLabels && rect.w > 30 && rect.h > 20 then
      let labelX := rect.x + dims.labelPadding
      let labelY := rect.y + dims.labelPadding + 12
      -- Use contrasting text color
      let textColor := if color.r + color.g + color.b > 1.5 then
        Color.rgba 0.1 0.1 0.1 1.0
      else
        Color.rgba 0.95 0.95 0.95 1.0
      RenderM.fillText item.node.label labelX labelY theme.smallFont textColor

      -- Draw value if space permits
      if dims.showValues && rect.h > 35 then
        let valueY := labelY + 14
        let valueStr := formatValue item.node.totalValue
        RenderM.fillText valueStr labelX valueY theme.smallFont (textColor.withAlpha 0.8)

    -- Render children if within depth limit and has children
    if depth < dims.maxDepth && !item.node.children.isEmpty then
      let childRect : LayoutRect := {
        x := rect.x + dims.padding
        y := rect.y + dims.padding + (if dims.showLabels && rect.h > 30 then 24 else 0)
        w := rect.w - dims.padding * 2
        h := rect.h - dims.padding * 2 - (if dims.showLabels && rect.h > 30 then 24 else 0)
      }
      if childRect.w > 10 && childRect.h > 10 then
        let childItems := item.node.children.mapIdx fun i child =>
          { node := child, value := child.totalValue, colorIdx := item.colorIdx + i + 1 : LayoutItem }
        let childLayout := squarify childItems childRect
        renderNodes childLayout theme dims (depth + 1)

/-- Custom spec for treemap chart rendering. -/
def treemapChartSpec (data : Data) (theme : Theme)
    (dims : Dimensions := defaultDimensions) : CustomSpec := {
  measure := fun _ _ => (dims.padding * 2 + 40, dims.padding * 2 + 40)
  collect := fun layout => RenderM.build do
    let rect := layout.contentRect
    let actualWidth := rect.width
    let actualHeight := rect.height

    if data.nodes.isEmpty then return

    -- Draw background
    let bgRect := Arbor.Rect.mk' rect.x rect.y actualWidth actualHeight
    RenderM.fillRect bgRect (theme.panel.background.withAlpha 0.3) 6.0

    -- Create layout items
    let items := data.nodes.mapIdx fun i node =>
      { node := node, value := node.totalValue, colorIdx := i : LayoutItem }

    -- Layout rectangle
    let layoutRect : LayoutRect := {
      x := rect.x + dims.padding
      y := rect.y + dims.padding
      w := actualWidth - dims.padding * 2
      h := actualHeight - dims.padding * 2
    }

    -- Run squarify algorithm
    let layout := squarify items layoutRect

    -- Render all nodes
    renderNodes layout theme dims 0

  draw := none
}

end TreemapChart

/-- Build a treemap chart visual (WidgetBuilder version).
    - `name`: Widget name for identification
    - `data`: Treemap chart data with nodes
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def treemapChartVisual (name : String) (data : TreemapChart.Data)
    (theme : Theme) (dims : TreemapChart.Dimensions := TreemapChart.defaultDimensions)
    : WidgetBuilder := do
  let wid ← freshId
  let chart ← custom (TreemapChart.treemapChartSpec data theme dims) {
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

/-! ## Reactive TreemapChart Components (FRP-based)

These use WidgetM for declarative composition.
-/

open Reactive Reactive.Host
open Afferent.Canopy.Reactive

/-- TreemapChart result - provides access to chart state. -/
structure TreemapChartResult where
  /-- The data being displayed. -/
  data : Reactive.Dynamic Spider TreemapChart.Data

/-- Create a treemap chart component using WidgetM.
    - `data`: Treemap chart data with nodes
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def treemapChart (data : TreemapChart.Data)
    (theme : Theme) (dims : TreemapChart.Dimensions := TreemapChart.defaultDimensions)
    : WidgetM TreemapChartResult := do
  let name ← registerComponentW "treemap-chart" (isInteractive := false)

  let dataDyn ← Dynamic.pureM data

  emit do
    pure (treemapChartVisual name data theme dims)

  pure { data := dataDyn }

/-- Create a treemap chart from simple flat data (no hierarchy).
    - `labels`: Labels for each node
    - `values`: Values for each node
    - `colors`: Optional colors for each node
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def treemapChartFromArrays (labels : Array String) (values : Array Float)
    (colors : Array Color := #[])
    (theme : Theme) (dims : TreemapChart.Dimensions := TreemapChart.defaultDimensions)
    : WidgetM TreemapChartResult := do
  let numNodes := min labels.size values.size
  let nodes := Id.run do
    let mut result : Array TreemapChart.TreeNode := #[]
    for i in [0:numNodes] do
      let color := if i < colors.size then some colors[i]! else none
      result := result.push {
        label := labels[i]!
        value := values[i]!
        color
      }
    result
  let data : TreemapChart.Data := { nodes }
  treemapChart data theme dims

end Afferent.Canopy

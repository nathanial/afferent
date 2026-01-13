/-
  Canopy SankeyDiagram Widget
  Sankey diagram for showing flow/movement between categories.
-/
import Reactive
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Reactive.Component

namespace Afferent.Canopy

open Afferent.Arbor hiding Event

namespace SankeyDiagram

/-- Dimensions and styling for Sankey diagram rendering. -/
structure Dimensions where
  width : Float := 500.0
  height : Float := 300.0
  marginTop : Float := 20.0
  marginBottom : Float := 20.0
  marginLeft : Float := 20.0
  marginRight : Float := 20.0
  nodeWidth : Float := 20.0
  nodePadding : Float := 10.0  -- Vertical padding between nodes
  linkOpacity : Float := 0.4
  showLabels : Bool := true
  showValues : Bool := true
deriving Repr, Inhabited

/-- Default Sankey diagram dimensions. -/
def defaultDimensions : Dimensions := {}

/-- A node in the Sankey diagram. -/
structure Node where
  /-- Unique identifier for this node. -/
  id : String
  /-- Display label. -/
  label : String
  /-- Column/level (0 = leftmost). -/
  column : Nat
  /-- Optional color. -/
  color : Option Color := none
deriving Repr, Inhabited, BEq

/-- A link/flow between two nodes. -/
structure Link where
  /-- Source node ID. -/
  source : String
  /-- Target node ID. -/
  target : String
  /-- Flow value (determines link width). -/
  value : Float
  /-- Optional color (defaults to source node color). -/
  color : Option Color := none
deriving Repr, Inhabited, BEq

/-- Sankey diagram data. -/
structure Data where
  /-- All nodes in the diagram. -/
  nodes : Array Node
  /-- All links between nodes. -/
  links : Array Link
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

/-- Get color for a node by index. -/
def getNodeColor (node : Node) (idx : Nat) : Color :=
  node.color.getD (defaultColors[idx % defaultColors.size]!)

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

/-- Computed layout for a node (positions relative to chart origin). -/
structure NodeLayout where
  node : Node
  x : Float
  y : Float
  height : Float
  color : Color
deriving Repr, Inhabited, BEq

/-- Computed layout for a link (positions relative to chart origin). -/
structure LinkLayout where
  link : Link
  sourceX : Float
  sourceY : Float
  sourceHeight : Float
  targetX : Float
  targetY : Float
  targetHeight : Float
  color : Color
deriving Repr, Inhabited, BEq

/-- Pre-computed layout for the entire diagram. -/
structure CachedLayout where
  nodeLayouts : Array NodeLayout
  linkLayouts : Array LinkLayout
  maxColumn : Nat
deriving Repr, Inhabited, BEq

/-- Calculate the total incoming value for a node. -/
private def nodeInValue (nodeId : String) (links : Array Link) : Float :=
  links.foldl (fun acc link =>
    if link.target == nodeId then acc + link.value else acc
  ) 0.0

/-- Calculate the total outgoing value for a node. -/
private def nodeOutValue (nodeId : String) (links : Array Link) : Float :=
  links.foldl (fun acc link =>
    if link.source == nodeId then acc + link.value else acc
  ) 0.0

/-- Calculate the total value (max of in/out) for a node. -/
private def nodeValue (nodeId : String) (links : Array Link) : Float :=
  max (nodeInValue nodeId links) (nodeOutValue nodeId links)

/-- Compute the layout for a Sankey diagram.
    Positions are relative to (0, 0) - add screen offset when rendering.
    This is called once and cached, not every frame. -/
def computeLayout (data : Data) (dims : Dimensions) : CachedLayout := Id.run do
  if data.nodes.isEmpty then return { nodeLayouts := #[], linkLayouts := #[], maxColumn := 0 }

  let chartWidth := dims.width - dims.marginLeft - dims.marginRight
  let chartHeight := dims.height - dims.marginTop - dims.marginBottom

  -- Find number of columns
  let maxColumn := data.nodes.foldl (fun acc n => max acc n.column) 0

  -- Calculate column X positions
  let columnWidth := if maxColumn > 0 then
    (chartWidth - dims.nodeWidth) / maxColumn.toFloat
  else chartWidth

  -- Group nodes by column and calculate their values
  let mut nodeLayouts : Array NodeLayout := #[]
  let mut nodeMap : Std.HashMap String NodeLayout := {}

  -- Process each column
  for col in [0:maxColumn + 1] do
    -- Get nodes in this column
    let colNodes := data.nodes.filter (fun n => n.column == col)
    if colNodes.isEmpty then continue

    -- Calculate total value for this column
    let colTotalValue := colNodes.foldl (fun acc n =>
      acc + nodeValue n.id data.links
    ) 0.0

    -- Calculate available height for nodes
    let totalPadding := dims.nodePadding * (colNodes.size - 1).toFloat
    let availableHeight := chartHeight - totalPadding

    -- Position nodes vertically (relative to 0, 0)
    let mut currentY : Float := 0
    let columnX := col.toFloat * columnWidth

    for i in [0:colNodes.size] do
      let node := colNodes[i]!
      let nValue := nodeValue node.id data.links
      let nodeHeight := if colTotalValue > 0 then
        (nValue / colTotalValue) * availableHeight
      else
        availableHeight / colNodes.size.toFloat
      let nodeHeight := max 4.0 nodeHeight  -- Minimum height

      let color := getNodeColor node i
      let layout : NodeLayout := {
        node := node
        x := columnX
        y := currentY
        height := nodeHeight
        color := color
      }
      nodeLayouts := nodeLayouts.push layout
      nodeMap := nodeMap.insert node.id layout

      currentY := currentY + nodeHeight + dims.nodePadding

  -- Create link layouts
  let mut linkLayouts : Array LinkLayout := #[]

  -- Track vertical offsets for stacking links at each node
  let mut sourceOffsets : Std.HashMap String Float := {}
  let mut targetOffsets : Std.HashMap String Float := {}

  for link in data.links do
    match nodeMap.get? link.source, nodeMap.get? link.target with
    | some srcLayout, some tgtLayout =>
      -- Get current offsets
      let srcOffset := sourceOffsets.getD link.source 0.0
      let tgtOffset := targetOffsets.getD link.target 0.0

      -- Calculate link height based on value proportion
      let srcValue := nodeValue link.source data.links
      let tgtValue := nodeValue link.target data.links
      let srcLinkHeight := if srcValue > 0 then
        (link.value / srcValue) * srcLayout.height
      else link.value
      let tgtLinkHeight := if tgtValue > 0 then
        (link.value / tgtValue) * tgtLayout.height
      else link.value

      let linkColor := link.color.getD (srcLayout.color.withAlpha dims.linkOpacity)

      let layout : LinkLayout := {
        link := link
        sourceX := srcLayout.x + dims.nodeWidth
        sourceY := srcLayout.y + srcOffset
        sourceHeight := srcLinkHeight
        targetX := tgtLayout.x
        targetY := tgtLayout.y + tgtOffset
        targetHeight := tgtLinkHeight
        color := linkColor
      }
      linkLayouts := linkLayouts.push layout

      -- Update offsets
      sourceOffsets := sourceOffsets.insert link.source (srcOffset + srcLinkHeight)
      targetOffsets := targetOffsets.insert link.target (tgtOffset + tgtLinkHeight)
    | _, _ => continue

  { nodeLayouts, linkLayouts, maxColumn }

/-- Draw a curved link path (Bezier curve) with offset. -/
private def linkPath (l : LinkLayout) (ox oy : Float) : Arbor.Path :=
  let sx := l.sourceX + ox
  let sy := l.sourceY + oy
  let tx := l.targetX + ox
  let ty := l.targetY + oy
  let midX := (sx + tx) / 2
  -- Top edge of the link
  Arbor.Path.empty
    |>.moveTo (Arbor.Point.mk' sx sy)
    |>.bezierCurveTo
      (Arbor.Point.mk' midX sy)
      (Arbor.Point.mk' midX ty)
      (Arbor.Point.mk' tx ty)
    |>.lineTo (Arbor.Point.mk' tx (ty + l.targetHeight))
    |>.bezierCurveTo
      (Arbor.Point.mk' midX (ty + l.targetHeight))
      (Arbor.Point.mk' midX (sy + l.sourceHeight))
      (Arbor.Point.mk' sx (sy + l.sourceHeight))
    |>.closePath

/-- Custom spec for Sankey diagram rendering with pre-computed cached layout.
    Only performs offset calculations and render command generation - no layout computation. -/
def sankeyDiagramSpecCached (cached : CachedLayout) (data : Data) (theme : Theme)
    (dims : Dimensions := defaultDimensions) : CustomSpec := {
  measure := fun _ _ => (dims.width, dims.height)
  collect := fun layout =>
    let rect := layout.contentRect
    let cmds : Array RenderCommand := #[]

    if cached.nodeLayouts.isEmpty then cmds else

    -- Offset from screen position
    let ox := rect.x + dims.marginLeft
    let oy := rect.y + dims.marginTop

    -- Draw background
    let bgRect := Arbor.Rect.mk' rect.x rect.y dims.width dims.height
    let cmds := cmds.push (.fillRect bgRect (theme.panel.background.withAlpha 0.3) 6.0)

    -- Draw links first (behind nodes) - just offset pre-computed positions
    let cmds := cached.linkLayouts.foldl (fun cmds l =>
      let path := linkPath l ox oy
      cmds.push (.fillPath path l.color)
    ) cmds

    -- Draw nodes - just offset pre-computed positions
    let cmds := cached.nodeLayouts.foldl (fun cmds n =>
      let nodeRect := Arbor.Rect.mk' (n.x + ox) (n.y + oy) dims.nodeWidth n.height
      cmds.push (.fillRect nodeRect n.color 2.0)
    ) cmds

    -- Draw labels - just offset pre-computed positions
    let cmds := if dims.showLabels then
      cached.nodeLayouts.foldl (fun cmds n =>
        let labelY := n.y + oy + n.height / 2 + 4
        let (labelX, _align) := if n.node.column == cached.maxColumn then
          (n.x + ox + dims.nodeWidth + 6, 0)  -- Right side
        else if n.node.column == 0 then
          (n.x + ox - 6, 1)  -- Left side (right-aligned)
        else
          (n.x + ox + dims.nodeWidth + 6, 0)  -- Default right

        let labelText := if dims.showValues then
          let v := nodeValue n.node.id data.links
          s!"{n.node.label} ({formatValue v})"
        else
          n.node.label

        cmds.push (.fillText labelText labelX labelY theme.smallFont theme.text)
      ) cmds
    else cmds

    cmds

  draw := none
}

end SankeyDiagram

/-- Build a Sankey diagram visual with pre-computed cached layout (WidgetBuilder version).
    - `name`: Widget name for identification
    - `cached`: Pre-computed layout (compute once, reuse every frame)
    - `data`: Original data (for label values)
    - `theme`: Theme for styling
    - `dims`: Diagram dimensions
-/
def sankeyDiagramVisualCached (name : String) (cached : SankeyDiagram.CachedLayout)
    (data : SankeyDiagram.Data) (theme : Theme)
    (dims : SankeyDiagram.Dimensions := SankeyDiagram.defaultDimensions)
    : WidgetBuilder := do
  let wid ← freshId
  let chart ← custom (SankeyDiagram.sankeyDiagramSpecCached cached data theme dims) {
    minWidth := some dims.width
    minHeight := some dims.height
  }
  let props : Trellis.FlexContainer := { Trellis.FlexContainer.column 0 with alignItems := .flexStart }
  pure (.flex wid (some name) props {} #[chart])

/-! ## Reactive SankeyDiagram Components (FRP-based)

These use WidgetM for declarative composition with FRP-based layout caching.
Layout is computed once and cached - only recomputed when data changes.
-/

open Reactive Reactive.Host
open Afferent.Canopy.Reactive

/-- SankeyDiagram result - provides access to diagram state. -/
structure SankeyDiagramResult where
  /-- The data being displayed. -/
  data : Reactive.Dynamic Spider SankeyDiagram.Data

/-- Create a Sankey diagram component using WidgetM with FRP-based layout caching.
    Layout is computed once at creation time and reused every frame.
    - `data`: Sankey diagram data with nodes and links
    - `theme`: Theme for styling
    - `dims`: Diagram dimensions
-/
def sankeyDiagram (data : SankeyDiagram.Data)
    (theme : Theme) (dims : SankeyDiagram.Dimensions := SankeyDiagram.defaultDimensions)
    : WidgetM SankeyDiagramResult := do
  let name ← registerComponentW "sankey-diagram" (isInteractive := false)

  -- Create data Dynamic
  let dataDyn ← Dynamic.pureM data

  -- Pre-compute layout once (cached via Dynamic.mapM - only recomputes if data changes)
  let layoutDyn ← Dynamic.mapM (fun d => SankeyDiagram.computeLayout d dims) dataDyn

  -- dynWidget rebuilds visual only when cached layout changes
  let _ ← dynWidget layoutDyn fun cached =>
    emit (pure (sankeyDiagramVisualCached name cached data theme dims))

  pure { data := dataDyn }

/-- Create a Sankey diagram that updates based on an external event stream.
    Layout is automatically recomputed when data changes via FRP caching.
    - `initialData`: Initial diagram data
    - `dataUpdates`: Event stream of data updates
    - `theme`: Theme for styling
    - `dims`: Diagram dimensions
-/
def sankeyDiagramWithEvents (initialData : SankeyDiagram.Data)
    (dataUpdates : Reactive.Event Spider SankeyDiagram.Data)
    (theme : Theme) (dims : SankeyDiagram.Dimensions := SankeyDiagram.defaultDimensions)
    : WidgetM SankeyDiagramResult := do
  let name ← registerComponentW "sankey-diagram" (isInteractive := false)

  -- Data Dynamic that updates on events
  let dataDyn ← Reactive.holdDyn initialData dataUpdates

  -- Cached layout - recomputes only when dataDyn changes
  let layoutDyn ← Dynamic.mapM (fun d => SankeyDiagram.computeLayout d dims) dataDyn

  -- dynWidget rebuilds visual only when cached layout changes
  let _ ← dynWidget layoutDyn fun cached => do
    -- Need current data for label values
    let d ← SpiderM.liftIO dataDyn.sample
    emit (pure (sankeyDiagramVisualCached name cached d theme dims))

  pure { data := dataDyn }

end Afferent.Canopy

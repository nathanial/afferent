/-
  Canopy TreeView Widget
  Hierarchical expandable/collapsible tree.
-/
import Std.Data.HashMap
import Reactive
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Reactive.Component
import Afferent.Canopy.Widget.Layout.Scroll

namespace Afferent.Canopy

open Afferent.Arbor hiding Event
open Reactive Reactive.Host
open Afferent.Canopy.Reactive
open Trellis

-- BEq instance for HashSet based on size and element containment
-- Used by Dynamic.zipWithM for change detection
instance [BEq α] [Hashable α] : BEq (Std.HashSet α) where
  beq a b := a.size == b.size && a.toArray.all (b.contains ·)

/-- A tree node can be a leaf (no children) or a branch (with children). -/
inductive TreeNode where
  | leaf (label : String) (enabled : Bool := true)
  | branch (label : String) (children : Array TreeNode) (enabled : Bool := true)
deriving Repr, Inhabited

/-- Path to a tree node (indices at each level). -/
abbrev TreePath := Array Nat

/-- Selection mode for tree view nodes. -/
inductive TreeViewSelectionMode where
  | single    -- Only one node at a time
  | multiple  -- Multiple nodes can be selected
deriving Repr, Inhabited, BEq

/-- Configuration for tree view appearance. -/
structure TreeViewConfig where
  itemHeight : Float := 28.0
  indentWidth : Float := 20.0
  iconWidth : Float := 16.0
  itemPadding : Float := 8.0
  maxVisibleItems : Nat := 10
  selectionMode : TreeViewSelectionMode := .single
deriving Repr, Inhabited

/-- Result from tree view widget. -/
structure TreeViewResult where
  /-- Fires when a node is selected (path to node). -/
  onNodeSelect : Reactive.Event Spider TreePath
  /-- Fires when a branch is expanded/collapsed (path to branch). -/
  onNodeToggle : Reactive.Event Spider TreePath
  /-- Set of currently expanded branch paths. -/
  expandedNodes : Reactive.Dynamic Spider (Std.HashSet TreePath)
  /-- Currently selected node path. -/
  selectedNode : Reactive.Dynamic Spider (Option TreePath)

/-- Get the label of a tree node. -/
def TreeNode.label : TreeNode → String
  | .leaf label _ => label
  | .branch label _ _ => label

/-- Check if a tree node is enabled. -/
def TreeNode.isEnabled : TreeNode → Bool
  | .leaf _ enabled => enabled
  | .branch _ _ enabled => enabled

/-- Check if a tree node is a branch. -/
def TreeNode.isBranch : TreeNode → Bool
  | .leaf _ _ => false
  | .branch _ _ _ => true

/-- Get children of a tree node (empty for leaves). -/
def TreeNode.children : TreeNode → Array TreeNode
  | .leaf _ _ => #[]
  | .branch _ children _ => children

/-- A flattened tree item for rendering. -/
structure FlatTreeItem where
  path : TreePath
  depth : Nat
  node : TreeNode
deriving Repr, Inhabited

namespace TreeView

/-- Default tree view configuration. -/
def defaultConfig : TreeViewConfig := {}

/-- Get the node at a given path. -/
partial def getNodeAtPath (nodes : Array TreeNode) (path : TreePath) : Option TreeNode :=
  match path.toList with
  | [] => none
  | [i] => nodes[i]?
  | i :: rest =>
    match nodes[i]? with
    | some (.branch _ children _) => getNodeAtPath children rest.toArray
    | _ => none

/-- Check if node at path is enabled. -/
def isEnabledAtPath (nodes : Array TreeNode) (path : TreePath) : Bool :=
  match getNodeAtPath nodes path with
  | some node => node.isEnabled
  | none => false

/-- Check if node at path is a branch. -/
def isBranchAtPath (nodes : Array TreeNode) (path : TreePath) : Bool :=
  match getNodeAtPath nodes path with
  | some node => node.isBranch
  | none => false

/-- Collect all node paths in a tree (for hit testing). -/
partial def collectAllPaths (nodes : Array TreeNode) (parentPath : TreePath := #[]) : List TreePath :=
  (List.range nodes.size).foldl (fun acc i =>
    let path := parentPath.push i
    let withPath := path :: acc
    match nodes.getD i (.leaf "" false) with
    | .branch _ children _ => collectAllPaths children path ++ withPath
    | .leaf _ _ => withPath
  ) []

/-- Helper to flatten tree with index tracking. -/
partial def flattenVisibleAux (nodes : Array TreeNode) (expanded : Std.HashSet TreePath)
    (parentPath : TreePath) (depth : Nat) (idx : Nat) (acc : Array FlatTreeItem) : Array FlatTreeItem :=
  if idx >= nodes.size then acc
  else
    let node := nodes.getD idx (.leaf "" false)
    let path := parentPath.push idx
    let item : FlatTreeItem := { path, depth, node }
    let withItem := acc.push item
    let withChildren :=
      match node with
      | .branch _ children _ =>
        if expanded.contains path then
          flattenVisibleAux children expanded path (depth + 1) 0 withItem
        else
          withItem
      | .leaf _ _ => withItem
    flattenVisibleAux nodes expanded parentPath depth (idx + 1) withChildren

/-- Flatten tree into visible items based on expanded state. -/
def flattenVisible (nodes : Array TreeNode) (expanded : Std.HashSet TreePath)
    (parentPath : TreePath := #[]) (depth : Nat := 0) : Array FlatTreeItem :=
  flattenVisibleAux nodes expanded parentPath depth 0 #[]

/-- Count total visible nodes. -/
def countVisibleNodes (nodes : Array TreeNode) (expanded : Std.HashSet TreePath) : Nat :=
  (flattenVisible nodes expanded).size

/-- Update expanded set when toggling a branch. -/
def toggleExpanded (path : TreePath) (current : Std.HashSet TreePath) : Std.HashSet TreePath :=
  if current.contains path then
    current.erase path
  else
    current.insert path

/-- Update selection based on click and selection mode. -/
def updateSelection (mode : TreeViewSelectionMode) (clickedPath : TreePath)
    (_current : Option TreePath) : Option TreePath :=
  match mode with
  | .single => some clickedPath
  | .multiple => some clickedPath  -- For now, same as single

end TreeView

/-- Build a single tree item visual.
    - `itemName`: Widget name for hit testing the whole row
    - `toggleName`: Widget name for hit testing the toggle icon
    - `item`: The flattened tree item to render
    - `isHovered`: Whether this item is being hovered
    - `isSelected`: Whether this item is selected
    - `isExpanded`: Whether this item (if branch) is expanded
    - `theme`: Theme for styling
    - `config`: Tree view configuration
-/
def treeNodeItemVisual (itemName : String) (toggleName : String) (item : FlatTreeItem)
    (isHovered : Bool) (isSelected : Bool) (isExpanded : Bool)
    (theme : Theme) (config : TreeViewConfig := TreeView.defaultConfig) : WidgetBuilder := do
  let bgColor :=
    if isSelected then theme.primary.background.withAlpha 0.15
    else if isHovered then theme.input.backgroundHover
    else Color.transparent

  let textColor := if item.node.isEnabled then theme.text else theme.textMuted

  let itemStyle : BoxStyle := {
    backgroundColor := some bgColor
    padding := EdgeInsets.symmetric config.itemPadding 4
    minHeight := some config.itemHeight
    width := .percent 1.0
  }

  -- Indentation spacer
  let indentWidth := item.depth.toFloat * config.indentWidth
  let indentSpacer ← spacer indentWidth 1

  -- Expand/collapse icon or empty spacer for leaves
  let iconWidget ←
    if item.node.isBranch then
      let iconChar := if isExpanded then "▼" else "▶"
      let iconStyle : BoxStyle := {
        minWidth := some config.iconWidth
        minHeight := some config.iconWidth
      }
      let iconWid ← freshId
      let iconText ← text' iconChar theme.smallFont theme.textMuted .center
      let iconProps : FlexContainer := {
        FlexContainer.row 0 with
        alignItems := .center
        justifyContent := .center
      }
      pure (.flex iconWid (some toggleName) iconProps iconStyle #[iconText])
    else
      spacer config.iconWidth 1

  -- Label text
  let labelWidget ← text' item.node.label theme.font textColor .left

  -- Row layout: [indent][icon][label]
  let wid ← freshId
  let props : FlexContainer := {
    FlexContainer.row 4 with
    alignItems := .center
  }
  pure (.flex wid (some itemName) props itemStyle #[indentSpacer, iconWidget, labelWidget])

/-- Build the complete tree view items visual (column of items). -/
def treeViewItemsVisual (itemNameFn : TreePath → String) (toggleNameFn : TreePath → String)
    (items : Array FlatTreeItem) (expanded : Std.HashSet TreePath)
    (selectedNode : Option TreePath) (hoveredNode : Option TreePath)
    (theme : Theme) (config : TreeViewConfig := TreeView.defaultConfig) : WidgetBuilder := do
  let mut itemWidgets : Array Widget := #[]
  for item in items do
    let isHovered := hoveredNode == some item.path
    let isSelected := selectedNode == some item.path
    let isExpanded := expanded.contains item.path
    let itemWidget ← treeNodeItemVisual (itemNameFn item.path) (toggleNameFn item.path)
      item isHovered isSelected isExpanded theme config
    itemWidgets := itemWidgets.push itemWidget

  let wid ← freshId
  let props : FlexContainer := { direction := .column, gap := 0 }
  let containerStyle : BoxStyle := { width := .percent 1.0 }
  pure (.flex wid none props containerStyle itemWidgets)

/-- Register names for all tree nodes recursively. Returns a map from path to name. -/
partial def registerTreeNodeNames (nodes : Array TreeNode) (parentPath : TreePath := #[])
    : WidgetM (Std.HashMap TreePath String) := do
  let mut names : Std.HashMap TreePath String := {}
  for i in [:nodes.size] do
    let path := parentPath.push i
    let name ← registerComponentW "tree-item"
    names := names.insert path name
    match nodes.getD i (.leaf "" false) with
    | .branch _ children _ =>
      let subNames ← registerTreeNodeNames children path
      for (subPath, subName) in subNames.toList do
        names := names.insert subPath subName
    | .leaf _ _ => pure ()
  pure names

/-- Register names for toggle icons on all branch nodes. -/
partial def registerToggleNames (nodes : Array TreeNode) (parentPath : TreePath := #[])
    : WidgetM (Std.HashMap TreePath String) := do
  let mut names : Std.HashMap TreePath String := {}
  for i in [:nodes.size] do
    let path := parentPath.push i
    match nodes.getD i (.leaf "" false) with
    | .branch _ children _ =>
      let name ← registerComponentW "tree-toggle"
      names := names.insert path name
      let subNames ← registerToggleNames children path
      for (subPath, subName) in subNames.toList do
        names := names.insert subPath subName
    | .leaf _ _ => pure ()
  pure names

/-- Create a reactive tree view widget.
    - `nodes`: Array of root-level tree nodes
    - `config`: Tree view configuration
-/
def treeView (nodes : Array TreeNode)
    (config : TreeViewConfig := TreeView.defaultConfig)
    : WidgetM TreeViewResult := do
  let theme ← getThemeW
  -- Register names for all nodes and toggles
  let itemNames ← registerTreeNodeNames nodes
  let toggleNames ← registerToggleNames nodes
  let itemNameFn (path : TreePath) : String := itemNames.getD path ""
  let toggleNameFn (path : TreePath) : String := toggleNames.getD path ""

  -- All paths for hit testing
  let allPaths := TreeView.collectAllPaths nodes
  let branchPaths := allPaths.filter (TreeView.isBranchAtPath nodes)

  -- Hooks
  let allClicks ← useAllClicks

  -- Create triggers for events
  let (selectTrigger, fireSelect) ← Reactive.newTriggerEvent (t := Spider) (a := TreePath)
  let (toggleTrigger, fireToggle) ← Reactive.newTriggerEvent (t := Spider) (a := TreePath)

  -- Find which toggle was clicked (for branch expansion)
  let findClickedToggle (data : ClickData) : Option TreePath :=
    branchPaths.findSome? fun path =>
      if hitWidget data (toggleNameFn path) then some path else none

  -- Find which item was clicked (for selection)
  let findClickedItem (data : ClickData) : Option TreePath :=
    allPaths.findSome? fun path =>
      if hitWidget data (itemNameFn path) && TreeView.isEnabledAtPath nodes path then some path else none

  -- Toggle clicks (expand/collapse branches)
  let toggleClicks ← Event.mapMaybeM findClickedToggle allClicks

  -- Item clicks (selection) - exclude toggle clicks
  let itemClicks ← Event.mapMaybeM (fun data =>
    match findClickedToggle data with
    | some _ => none  -- Click was on toggle, don't select
    | none => findClickedItem data
  ) allClicks

  -- Track expanded nodes
  let expandedNodes ← Reactive.foldDynM
    (fun (path : TreePath) (current : Std.HashSet TreePath) => do
      SpiderM.liftIO (fireToggle path)
      pure (TreeView.toggleExpanded path current)
    )
    ({} : Std.HashSet TreePath)
    toggleClicks

  -- Track selected node
  let selectedNode ← Reactive.foldDynM
    (fun (path : TreePath) (current : Option TreePath) => do
      SpiderM.liftIO (fireSelect path)
      pure (TreeView.updateSelection config.selectionMode path current)
    )
    (none : Option TreePath)
    itemClicks

  -- Track hovered node
  let hoverTargets := allPaths.toArray.map (fun path => (itemNameFn path, path))
  let hoverChanges ← StateT.lift (hoverEventForTargets hoverTargets)
  let hoveredNode ← Reactive.holdDyn (none : Option TreePath) hoverChanges

  -- Calculate scroll container size
  let visibleHeight := config.maxVisibleItems.toFloat * config.itemHeight
  let scrollConfig : ScrollContainerConfig := {
    width := 250  -- Default width, can be overridden by parent layout
    height := visibleHeight
    verticalScroll := true
    horizontalScroll := false
    scrollbarVisibility := .always
  }

  -- Use scroll container for scrolling
  let (_, _) ← scrollContainer scrollConfig do
    -- Use dynWidget for efficient change-driven rebuilds
    -- Chain zipWithM for 3 dynamics
    let renderState ← Dynamic.zipWithM (fun e s => (e, s)) expandedNodes selectedNode
    let renderState2 ← Dynamic.zipWithM (fun (e, s) h => (e, s, h)) renderState hoveredNode
    let _ ← dynWidget renderState2 fun (expanded, selected, hovered) => do
      let flatItems := TreeView.flattenVisible nodes expanded
      emit do pure (treeViewItemsVisual itemNameFn toggleNameFn flatItems expanded selected hovered theme config)
    pure ()

  pure { onNodeSelect := selectTrigger, onNodeToggle := toggleTrigger, expandedNodes, selectedNode }

/-- Create a tree view with initially expanded nodes.
    - `nodes`: Array of root-level tree nodes
    - `initialExpanded`: Initially expanded branch paths
    - `config`: Tree view configuration
-/
def treeViewWithExpanded (nodes : Array TreeNode) (initialExpanded : Array TreePath)
    (config : TreeViewConfig := TreeView.defaultConfig)
    : WidgetM TreeViewResult := do
  let theme ← getThemeW
  -- Register names for all nodes and toggles
  let itemNames ← registerTreeNodeNames nodes
  let toggleNames ← registerToggleNames nodes
  let itemNameFn (path : TreePath) : String := itemNames.getD path ""
  let toggleNameFn (path : TreePath) : String := toggleNames.getD path ""

  -- All paths for hit testing
  let allPaths := TreeView.collectAllPaths nodes
  let branchPaths := allPaths.filter (TreeView.isBranchAtPath nodes)

  -- Hooks
  let allClicks ← useAllClicks

  -- Create triggers for events
  let (selectTrigger, fireSelect) ← Reactive.newTriggerEvent (t := Spider) (a := TreePath)
  let (toggleTrigger, fireToggle) ← Reactive.newTriggerEvent (t := Spider) (a := TreePath)

  -- Find which toggle was clicked (for branch expansion)
  let findClickedToggle (data : ClickData) : Option TreePath :=
    branchPaths.findSome? fun path =>
      if hitWidget data (toggleNameFn path) then some path else none

  -- Find which item was clicked (for selection)
  let findClickedItem (data : ClickData) : Option TreePath :=
    allPaths.findSome? fun path =>
      if hitWidget data (itemNameFn path) && TreeView.isEnabledAtPath nodes path then some path else none

  -- Toggle clicks (expand/collapse branches)
  let toggleClicks ← Event.mapMaybeM findClickedToggle allClicks

  -- Item clicks (selection) - exclude toggle clicks
  let itemClicks ← Event.mapMaybeM (fun data =>
    match findClickedToggle data with
    | some _ => none  -- Click was on toggle, don't select
    | none => findClickedItem data
  ) allClicks

  -- Track expanded nodes (with initial state)
  let initialExpandedSet := initialExpanded.foldl (fun acc p => acc.insert p) ({} : Std.HashSet TreePath)
  let expandedNodes ← Reactive.foldDynM
    (fun (path : TreePath) (current : Std.HashSet TreePath) => do
      SpiderM.liftIO (fireToggle path)
      pure (TreeView.toggleExpanded path current)
    )
    initialExpandedSet
    toggleClicks

  -- Track selected node
  let selectedNode ← Reactive.foldDynM
    (fun (path : TreePath) (current : Option TreePath) => do
      SpiderM.liftIO (fireSelect path)
      pure (TreeView.updateSelection config.selectionMode path current)
    )
    (none : Option TreePath)
    itemClicks

  -- Track hovered node
  let hoverTargets := allPaths.toArray.map (fun path => (itemNameFn path, path))
  let hoverChanges ← StateT.lift (hoverEventForTargets hoverTargets)
  let hoveredNode ← Reactive.holdDyn (none : Option TreePath) hoverChanges

  -- Calculate scroll container size
  let visibleHeight := config.maxVisibleItems.toFloat * config.itemHeight
  let scrollConfig : ScrollContainerConfig := {
    width := 250
    height := visibleHeight
    verticalScroll := true
    horizontalScroll := false
    scrollbarVisibility := .always
  }

  -- Use scroll container for scrolling
  let (_, _) ← scrollContainer scrollConfig do
    -- Use dynWidget for efficient change-driven rebuilds
    -- Chain zipWithM for 3 dynamics
    let renderState ← Dynamic.zipWithM (fun e s => (e, s)) expandedNodes selectedNode
    let renderState2 ← Dynamic.zipWithM (fun (e, s) h => (e, s, h)) renderState hoveredNode
    let _ ← dynWidget renderState2 fun (expanded, selected, hovered) => do
      let flatItems := TreeView.flattenVisible nodes expanded
      emit do pure (treeViewItemsVisual itemNameFn toggleNameFn flatItems expanded selected hovered theme config)
    pure ()

  pure { onNodeSelect := selectTrigger, onNodeToggle := toggleTrigger, expandedNodes, selectedNode }

end Afferent.Canopy

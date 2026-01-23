/-
  TreeView Widget Tests
  Unit tests for the tree view widget functionality.
-/
import Afferent.Tests.Framework
import Afferent.Canopy.Widget.Data.TreeView
import Afferent.Canopy.Reactive.Component
import Afferent.Arbor

namespace Afferent.Tests.TreeViewTests

open Crucible
open Afferent.Tests
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Afferent.Arbor
open Reactive Reactive.Host

testSuite "TreeView Tests"

/-- Test font ID for widget building tests. -/
def testFont : FontId := { id := 0, name := "test", size := 14.0 }

/-- Test theme for widget tests. -/
def testTheme : Theme := { Theme.dark with font := testFont, smallFont := testFont }

/-! ## TreeNode Construction Tests -/

test "TreeNode.leaf construction" := do
  let node := TreeNode.leaf "Test Leaf"
  ensure (node.label == "Test Leaf") "Label should be 'Test Leaf'"
  ensure node.isEnabled "Should be enabled by default"
  ensure (!node.isBranch) "Leaf should not be a branch"

test "TreeNode.leaf disabled" := do
  let node := TreeNode.leaf "Disabled" (enabled := false)
  ensure (!node.isEnabled) "Should be disabled"

test "TreeNode.branch construction" := do
  let children := #[TreeNode.leaf "Child 1", TreeNode.leaf "Child 2"]
  let node := TreeNode.branch "Parent" children
  ensure (node.label == "Parent") "Label should be 'Parent'"
  ensure node.isEnabled "Should be enabled by default"
  ensure node.isBranch "Branch should be a branch"
  ensure (node.children.size == 2) "Should have 2 children"

test "TreeNode.branch disabled" := do
  let node := TreeNode.branch "Disabled" #[] (enabled := false)
  ensure (!node.isEnabled) "Should be disabled"

/-! ## TreePath Navigation Tests -/

test "getNodeAtPath single level" := do
  let nodes := #[TreeNode.leaf "A", TreeNode.leaf "B", TreeNode.leaf "C"]
  match TreeView.getNodeAtPath nodes #[0] with
  | some node => ensure (node.label == "A") s!"Expected 'A', got '{node.label}'"
  | none => ensure false "Should find node at path [0]"

  match TreeView.getNodeAtPath nodes #[2] with
  | some node => ensure (node.label == "C") s!"Expected 'C', got '{node.label}'"
  | none => ensure false "Should find node at path [2]"

test "getNodeAtPath nested" := do
  let nodes := #[
    TreeNode.branch "Parent" #[
      TreeNode.leaf "Child 1",
      TreeNode.leaf "Child 2"
    ]
  ]
  match TreeView.getNodeAtPath nodes #[0, 1] with
  | some node => ensure (node.label == "Child 2") s!"Expected 'Child 2', got '{node.label}'"
  | none => ensure false "Should find node at path [0, 1]"

test "getNodeAtPath invalid path" := do
  let nodes := #[TreeNode.leaf "A"]
  let result := TreeView.getNodeAtPath nodes #[5]
  ensure result.isNone "Should not find node at invalid path"

test "getNodeAtPath empty path" := do
  let nodes := #[TreeNode.leaf "A"]
  let result := TreeView.getNodeAtPath nodes #[]
  ensure result.isNone "Empty path should return none"

/-! ## collectAllPaths Tests -/

test "collectAllPaths single node" := do
  let nodes := #[TreeNode.leaf "A"]
  let paths := TreeView.collectAllPaths nodes
  ensure (paths.length == 1) s!"Should have 1 path, got {paths.length}"
  ensure (paths.contains #[0]) "Should contain path [0]"

test "collectAllPaths multiple roots" := do
  let nodes := #[TreeNode.leaf "A", TreeNode.leaf "B", TreeNode.leaf "C"]
  let paths := TreeView.collectAllPaths nodes
  ensure (paths.length == 3) s!"Should have 3 paths, got {paths.length}"
  ensure (paths.contains #[0]) "Should contain path [0]"
  ensure (paths.contains #[1]) "Should contain path [1]"
  ensure (paths.contains #[2]) "Should contain path [2]"

test "collectAllPaths nested" := do
  let nodes := #[
    TreeNode.branch "Parent" #[
      TreeNode.leaf "Child 1",
      TreeNode.leaf "Child 2"
    ],
    TreeNode.leaf "Sibling"
  ]
  let paths := TreeView.collectAllPaths nodes
  -- Should have: [0], [0,0], [0,1], [1]
  ensure (paths.length == 4) s!"Should have 4 paths, got {paths.length}"
  ensure (paths.contains #[0]) "Should contain path [0]"
  ensure (paths.contains #[0, 0]) "Should contain path [0, 0]"
  ensure (paths.contains #[0, 1]) "Should contain path [0, 1]"
  ensure (paths.contains #[1]) "Should contain path [1]"

/-! ## flattenVisible Tests -/

test "flattenVisible empty tree" := do
  let nodes : Array TreeNode := #[]
  let expanded : Std.HashSet TreePath := {}
  let flat := TreeView.flattenVisible nodes expanded
  ensure (flat.size == 0) "Should have 0 items"

test "flattenVisible collapsed branch" := do
  let nodes := #[
    TreeNode.branch "Parent" #[
      TreeNode.leaf "Child 1",
      TreeNode.leaf "Child 2"
    ]
  ]
  let expanded : Std.HashSet TreePath := {}
  let flat := TreeView.flattenVisible nodes expanded
  -- Only the parent should be visible when collapsed
  ensure (flat.size == 1) s!"Should have 1 item when collapsed, got {flat.size}"
  ensure (flat[0]!.node.label == "Parent") "Should be the parent"
  ensure (flat[0]!.depth == 0) "Parent should have depth 0"

test "flattenVisible expanded branch" := do
  let nodes := #[
    TreeNode.branch "Parent" #[
      TreeNode.leaf "Child 1",
      TreeNode.leaf "Child 2"
    ]
  ]
  let expanded : Std.HashSet TreePath := ({} : Std.HashSet TreePath).insert #[0]
  let flat := TreeView.flattenVisible nodes expanded
  -- Parent + 2 children should be visible
  ensure (flat.size == 3) s!"Should have 3 items when expanded, got {flat.size}"
  ensure (flat[0]!.node.label == "Parent") "First should be parent"
  ensure (flat[0]!.depth == 0) "Parent should have depth 0"
  ensure (flat[1]!.node.label == "Child 1") "Second should be Child 1"
  ensure (flat[1]!.depth == 1) "Child 1 should have depth 1"
  ensure (flat[2]!.node.label == "Child 2") "Third should be Child 2"
  ensure (flat[2]!.depth == 1) "Child 2 should have depth 1"

test "flattenVisible deeply nested" := do
  let nodes := #[
    TreeNode.branch "Level 0" #[
      TreeNode.branch "Level 1" #[
        TreeNode.leaf "Level 2"
      ]
    ]
  ]
  -- Expand both branches
  let expanded : Std.HashSet TreePath := ({} : Std.HashSet TreePath).insert #[0] |>.insert #[0, 0]
  let flat := TreeView.flattenVisible nodes expanded
  ensure (flat.size == 3) s!"Should have 3 items, got {flat.size}"
  ensure (flat[0]!.depth == 0) "Level 0 should have depth 0"
  ensure (flat[1]!.depth == 1) "Level 1 should have depth 1"
  ensure (flat[2]!.depth == 2) "Level 2 should have depth 2"

/-! ## toggleExpanded Tests -/

test "toggleExpanded adds path" := do
  let current : Std.HashSet TreePath := {}
  let result := TreeView.toggleExpanded #[0] current
  ensure (result.contains #[0]) "Should contain the toggled path"

test "toggleExpanded removes path" := do
  let current : Std.HashSet TreePath := ({} : Std.HashSet TreePath).insert #[0]
  let result := TreeView.toggleExpanded #[0] current
  ensure (!result.contains #[0]) "Should not contain the toggled path"

test "toggleExpanded preserves other paths" := do
  let current : Std.HashSet TreePath := ({} : Std.HashSet TreePath).insert #[0] |>.insert #[1]
  let result := TreeView.toggleExpanded #[0] current
  ensure (!result.contains #[0]) "Should not contain #[0]"
  ensure (result.contains #[1]) "Should still contain #[1]"

/-! ## Selection Tests -/

test "updateSelection single mode" := do
  let result := TreeView.updateSelection .single #[0] none
  ensure (result == some #[0]) "Should select clicked path"

  let result2 := TreeView.updateSelection .single #[1] (some #[0])
  ensure (result2 == some #[1]) "Should replace selection"

/-! ## TreeViewConfig Tests -/

test "TreeViewConfig default values" := do
  let config := TreeView.defaultConfig
  ensure (config.itemHeight == 28.0) s!"Default item height should be 28, got {config.itemHeight}"
  ensure (config.indentWidth == 20.0) s!"Default indent width should be 20, got {config.indentWidth}"
  ensure (config.iconWidth == 16.0) s!"Default icon width should be 16, got {config.iconWidth}"
  ensure (config.maxVisibleItems == 10) s!"Default max visible items should be 10, got {config.maxVisibleItems}"
  ensure (config.selectionMode == .single) "Default selection mode should be single"

test "TreeViewConfig custom values" := do
  let config : TreeViewConfig := {
    itemHeight := 32.0
    indentWidth := 24.0
    iconWidth := 20.0
    maxVisibleItems := 15
    selectionMode := .multiple
  }
  ensure (config.itemHeight == 32.0) "Item height should be 32"
  ensure (config.indentWidth == 24.0) "Indent width should be 24"
  ensure (config.iconWidth == 20.0) "Icon width should be 20"
  ensure (config.maxVisibleItems == 15) "Max visible items should be 15"
  ensure (config.selectionMode == .multiple) "Selection mode should be multiple"

/-! ## isBranchAtPath Tests -/

test "isBranchAtPath leaf" := do
  let nodes := #[TreeNode.leaf "Leaf"]
  let result := TreeView.isBranchAtPath nodes #[0]
  ensure (!result) "Leaf should not be a branch"

test "isBranchAtPath branch" := do
  let nodes := #[TreeNode.branch "Branch" #[TreeNode.leaf "Child"]]
  let result := TreeView.isBranchAtPath nodes #[0]
  ensure result "Branch should be a branch"

test "isBranchAtPath invalid path" := do
  let nodes := #[TreeNode.leaf "Leaf"]
  let result := TreeView.isBranchAtPath nodes #[5]
  ensure (!result) "Invalid path should return false"

/-! ## isEnabledAtPath Tests -/

test "isEnabledAtPath enabled node" := do
  let nodes := #[TreeNode.leaf "Enabled"]
  let result := TreeView.isEnabledAtPath nodes #[0]
  ensure result "Enabled node should be enabled"

test "isEnabledAtPath disabled node" := do
  let nodes := #[TreeNode.leaf "Disabled" (enabled := false)]
  let result := TreeView.isEnabledAtPath nodes #[0]
  ensure (!result) "Disabled node should not be enabled"

/-! ## Visual Structure Tests -/

test "treeNodeItemVisual creates widget with correct name" := do
  let item : FlatTreeItem := { path := #[0], depth := 0, node := TreeNode.leaf "Test" }
  let builder := treeNodeItemVisual "item-0" "toggle-0" item false false false testTheme
  let (widget, _) ← builder.run {}
  let widgetName := Widget.name? widget
  ensure (widgetName == some "item-0") s!"Expected name 'item-0', got {widgetName}"

test "treeNodeItemVisual branch has toggle name" := do
  let item : FlatTreeItem := { path := #[0], depth := 0, node := TreeNode.branch "Test" #[] }
  let builder := treeNodeItemVisual "item-0" "toggle-0" item false false false testTheme
  let (widget, _) ← builder.run {}
  let found := findWidgetIdByName widget "toggle-0"
  ensure found.isSome "Toggle widget should be findable"

test "treeViewItemsVisual creates column with items" := do
  let items : Array FlatTreeItem := #[
    { path := #[0], depth := 0, node := TreeNode.leaf "A" },
    { path := #[1], depth := 0, node := TreeNode.leaf "B" }
  ]
  let itemNameFn (p : TreePath) := s!"item-{p}"
  let toggleNameFn (p : TreePath) := s!"toggle-{p}"
  let builder := treeViewItemsVisual itemNameFn toggleNameFn items {} none none testTheme
  let (widget, _) ← builder.run {}
  match widget with
  | .flex _ _ props _ children =>
    ensure (props.direction == .column) "Should be a column"
    ensure (children.size == 2) s!"Should have 2 children, got {children.size}"
  | _ => ensure false "Expected flex widget"

/-! ## Tree Structure Tests -/

test "typical file tree structure" := do
  let nodes := #[
    TreeNode.branch "src" #[
      TreeNode.leaf "main.lean",
      TreeNode.branch "lib" #[
        TreeNode.leaf "utils.lean",
        TreeNode.leaf "types.lean"
      ]
    ],
    TreeNode.leaf "README.md"
  ]
  let paths := TreeView.collectAllPaths nodes
  -- src, main.lean, lib, utils.lean, types.lean, README.md = 6 paths
  ensure (paths.length == 6) s!"Should have 6 paths, got {paths.length}"

  -- Test flattening with src expanded but lib collapsed
  let expanded : Std.HashSet TreePath := ({} : Std.HashSet TreePath).insert #[0]
  let flat := TreeView.flattenVisible nodes expanded
  -- src, main.lean, lib, README.md = 4 visible
  ensure (flat.size == 4) s!"With src expanded, should have 4 visible, got {flat.size}"

  -- Test flattening with both expanded
  let expanded2 := expanded.insert #[0, 1]
  let flat2 := TreeView.flattenVisible nodes expanded2
  -- All 6 visible
  ensure (flat2.size == 6) s!"With both expanded, should have 6 visible, got {flat2.size}"



end Afferent.Tests.TreeViewTests

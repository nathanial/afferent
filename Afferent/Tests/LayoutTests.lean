/-
  Afferent Layout Tests
  Unit tests for Trellis grid layout behavior.
-/
import Afferent.Tests.Framework
import Afferent.Layout
import Afferent.Arbor.Widget.Core
import Afferent.Arbor.Widget.Measure
import Trellis

namespace Afferent.Tests.LayoutTests

open Crucible
open Afferent.Tests
open Trellis
open Afferent.Arbor

testSuite "Layout Tests"

/-! ## Grid Layout with fr units -/

test "2x3 grid with fr units fills available space" := do
  -- Create 2 columns, 3 rows grid with 1fr each
  let props := GridContainer.withTemplate
    #[.fr 1, .fr 1, .fr 1]  -- 3 rows
    #[.fr 1, .fr 1]          -- 2 columns
  let tree := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf 1 ContentSize.zero,
    LayoutNode.leaf 2 ContentSize.zero,
    LayoutNode.leaf 3 ContentSize.zero,
    LayoutNode.leaf 4 ContentSize.zero,
    LayoutNode.leaf 5 ContentSize.zero,
    LayoutNode.leaf 6 ContentSize.zero
  ]

  let result := layout tree 600 300

  -- Should have 7 layouts: 1 container + 6 leaves
  ensure (result.layouts.size == 7) s!"Expected 7 layouts, got {result.layouts.size}"

test "2x3 grid cells have correct dimensions" := do
  let props := GridContainer.withTemplate
    #[.fr 1, .fr 1, .fr 1]  -- 3 rows
    #[.fr 1, .fr 1]          -- 2 columns
  let tree := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf 1 ContentSize.zero,
    LayoutNode.leaf 2 ContentSize.zero,
    LayoutNode.leaf 3 ContentSize.zero,
    LayoutNode.leaf 4 ContentSize.zero,
    LayoutNode.leaf 5 ContentSize.zero,
    LayoutNode.leaf 6 ContentSize.zero
  ]

  let result := layout tree 600 300

  -- Each cell should be 300x100 (600/2 x 300/3)
  for cl in result.layouts do
    if cl.nodeId >= 1 && cl.nodeId <= 6 then
      let rect := cl.borderRect
      shouldBeNear rect.width 300.0
      shouldBeNear rect.height 100.0

test "2x3 grid cells have correct positions" := do
  let props := GridContainer.withTemplate
    #[.fr 1, .fr 1, .fr 1]  -- 3 rows
    #[.fr 1, .fr 1]          -- 2 columns
  let tree := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf 1 ContentSize.zero,
    LayoutNode.leaf 2 ContentSize.zero,
    LayoutNode.leaf 3 ContentSize.zero,
    LayoutNode.leaf 4 ContentSize.zero,
    LayoutNode.leaf 5 ContentSize.zero,
    LayoutNode.leaf 6 ContentSize.zero
  ]

  let result := layout tree 600 300

  -- Build a map of nodeId -> rect for easy lookup
  let mut positions : Array (Nat × Float × Float) := #[]
  for cl in result.layouts do
    if cl.nodeId >= 1 && cl.nodeId <= 6 then
      positions := positions.push (cl.nodeId, cl.borderRect.x, cl.borderRect.y)

  -- Cell 1: top-left (0, 0)
  -- Cell 2: top-right (300, 0)
  -- Cell 3: middle-left (0, 100)
  -- Cell 4: middle-right (300, 100)
  -- Cell 5: bottom-left (0, 200)
  -- Cell 6: bottom-right (300, 200)
  for (id, x, y) in positions do
    match id with
    | 1 => shouldBeNear x 0.0; shouldBeNear y 0.0
    | 2 => shouldBeNear x 300.0; shouldBeNear y 0.0
    | 3 => shouldBeNear x 0.0; shouldBeNear y 100.0
    | 4 => shouldBeNear x 300.0; shouldBeNear y 100.0
    | 5 => shouldBeNear x 0.0; shouldBeNear y 200.0
    | 6 => shouldBeNear x 300.0; shouldBeNear y 200.0
    | _ => pure ()

test "2x3 grid works with large screen dimensions" := do
  let props := GridContainer.withTemplate
    #[.fr 1, .fr 1, .fr 1]  -- 3 rows
    #[.fr 1, .fr 1]          -- 2 columns
  let tree := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf 1 ContentSize.zero,
    LayoutNode.leaf 2 ContentSize.zero,
    LayoutNode.leaf 3 ContentSize.zero,
    LayoutNode.leaf 4 ContentSize.zero,
    LayoutNode.leaf 5 ContentSize.zero,
    LayoutNode.leaf 6 ContentSize.zero
  ]

  -- Test with Retina-scale dimensions
  let result := layout tree 3840 2160

  -- Each cell should be 1920x720 (3840/2 x 2160/3)
  for cl in result.layouts do
    if cl.nodeId >= 1 && cl.nodeId <= 6 then
      let rect := cl.borderRect
      shouldBeNear rect.width 1920.0
      shouldBeNear rect.height 720.0

test "container node (id 0) covers full viewport" := do
  let props := GridContainer.withTemplate
    #[.fr 1, .fr 1, .fr 1]
    #[.fr 1, .fr 1]
  let tree := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf 1 ContentSize.zero,
    LayoutNode.leaf 2 ContentSize.zero,
    LayoutNode.leaf 3 ContentSize.zero,
    LayoutNode.leaf 4 ContentSize.zero,
    LayoutNode.leaf 5 ContentSize.zero,
    LayoutNode.leaf 6 ContentSize.zero
  ]

  let result := layout tree 800 600

  -- Find container node
  for cl in result.layouts do
    if cl.nodeId == 0 then
      shouldBeNear cl.borderRect.x 0.0
      shouldBeNear cl.borderRect.y 0.0
      shouldBeNear cl.borderRect.width 800.0
      shouldBeNear cl.borderRect.height 600.0

/-! ## Comparison: columns-only vs withTemplate -/

test "GridContainer.columns 2 does NOT specify row heights" := do
  -- This is what we had before - only specifies columns
  let props := GridContainer.columns 2
  let tree := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf 1 (ContentSize.mk' 0 50),  -- 50px content height
    LayoutNode.leaf 2 (ContentSize.mk' 0 50),
    LayoutNode.leaf 3 (ContentSize.mk' 0 50),
    LayoutNode.leaf 4 (ContentSize.mk' 0 50),
    LayoutNode.leaf 5 (ContentSize.mk' 0 50),
    LayoutNode.leaf 6 (ContentSize.mk' 0 50)
  ]

  let result := layout tree 600 300

  -- With columns-only, rows auto-size to content (50px each)
  -- NOT 100px (300/3) - that's the key difference!
  for cl in result.layouts do
    if cl.nodeId >= 1 && cl.nodeId <= 6 then
      let rect := cl.borderRect
      -- Width should still be 300 (600/2)
      shouldBeNear rect.width 300.0
      -- Height should be content height (50), not stretched
      shouldBeNear rect.height 50.0

/-! ## BoxConstraints Percentage Dimension Tests -/

test "leaf with height: percent 1.0 fills available height" := do
  let tree := LayoutNode.leaf 1 (ContentSize.mk' 50 30) { height := .percent 1.0 }
  let result := layout tree 200 300
  let cl := result.get! 1
  shouldBeNear cl.height 300.0

test "leaf with width: percent 1.0 fills available width" := do
  let tree := LayoutNode.leaf 1 (ContentSize.mk' 50 30) { width := .percent 1.0 }
  let result := layout tree 200 300
  let cl := result.get! 1
  shouldBeNear cl.width 200.0

test "flex column child with height: percent 1.0 fills column" := do
  let tree := LayoutNode.column 0 #[
    LayoutNode.leaf 1 (ContentSize.mk' 100 30) { height := .percent 1.0 }
  ]
  let result := layout tree 200 300
  let cl := result.get! 1
  shouldBeNear cl.height 300.0

test "flex row child with width: percent 1.0 fills row" := do
  let tree := LayoutNode.row 0 #[
    LayoutNode.leaf 1 (ContentSize.mk' 50 30) { width := .percent 1.0 }
  ]
  let result := layout tree 200 300
  let cl := result.get! 1
  shouldBeNear cl.width 200.0

test "grid child with height: percent 1.0 fills cell" := do
  let props := GridContainer.withTemplate #[.fr 1] #[.fr 1]
  let tree := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf 1 (ContentSize.mk' 50 30) { height := .percent 1.0 }
  ]
  let result := layout tree 200 300
  let cl := result.get! 1
  shouldBeNear cl.height 300.0

test "grid child with width and height: percent 1.0 fills cell" := do
  let props := GridContainer.withTemplate #[.fr 1] #[.fr 1]
  let tree := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf 1 (ContentSize.mk' 0 0) { width := .percent 1.0, height := .percent 1.0 }
  ]
  let result := layout tree 200 300
  let cl := result.get! 1
  shouldBeNear cl.width 200.0
  shouldBeNear cl.height 300.0

/-! ## Absolute Positioning Tests -/

-- TODO: Fix TextMeasurer Id instance issue
-- test "absolute children do not affect flex container intrinsic size" := do
--   let widget := Widget.flex 1 none FlexContainer.column {} #[
--     Widget.rect 2 none { minWidth := some 120, minHeight := some 24 },
--     Widget.rect 3 none {
--       minWidth := some 200
--       minHeight := some 100
--       position := .absolute
--       top := some 40
--       left := some 0
--     }
--   ]
--   let result : MeasureResult := (measureWidget (M := Id) widget 1000 1000 : Id _)
--   match result.node.content with
--   | some cs =>
--     shouldBeNear cs.width 120.0
--     shouldBeNear cs.height 24.0
--   | none =>
--     ensure false "Expected container content size to be set"

/-! ## Demo Grid Layout Chain Tests

These tests verify the layout chain used in the demo grid:
- Outer grid cells fill viewport (via fr units)
- cellWidget column: uses flex-grow for content to fill remaining space
- Inner gridFlex: height: 100% fills the flex-grown space
- Card columns in inner grid: height: 100% fills grid cells
-/

test "demo chain: outer grid cells fill viewport" := do
  -- 2x3 grid fills 600x300 viewport
  let props := GridContainer.withTemplate
    #[.fr 1, .fr 1, .fr 1]
    #[.fr 1, .fr 1]
  let tree := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf 1 ContentSize.zero { height := .percent 1.0 },
    LayoutNode.leaf 2 ContentSize.zero { height := .percent 1.0 },
    LayoutNode.leaf 3 ContentSize.zero { height := .percent 1.0 },
    LayoutNode.leaf 4 ContentSize.zero { height := .percent 1.0 },
    LayoutNode.leaf 5 ContentSize.zero { height := .percent 1.0 },
    LayoutNode.leaf 6 ContentSize.zero { height := .percent 1.0 }
  ]
  let result := layout tree 600 300
  for id in [1, 2, 3, 4, 5, 6] do
    let cell := result.get! id
    shouldBeNear cell.width 300.0   -- 600/2
    shouldBeNear cell.height 100.0  -- 300/3

test "demo chain: cellWidget column with flex-grow fills grid cell" := do
  -- Grid cell containing column: label (fixed) + content (flex-grow)
  let props := GridContainer.withTemplate #[.fr 1] #[.fr 1] 0
  let tree := LayoutNode.gridBox 0 props #[
    LayoutNode.column 1 #[
      LayoutNode.leaf 2 (ContentSize.mk' 100 20) {},  -- label (fixed height)
      LayoutNode.leaf 3 (ContentSize.mk' 100 50) {} (item := .flexChild (FlexItem.growing 1))
    ] (box := { height := .percent 1.0 })
  ]
  let result := layout tree 300 100
  let column := result.get! 1
  let label := result.get! 2
  let content := result.get! 3
  shouldBeNear column.height 100.0   -- fills grid cell
  shouldBeNear label.height 20.0     -- fixed
  shouldBeNear content.height 80.0   -- remaining (100 - 20) via flex-grow

test "demo chain: inner grid fills flex-grown space" := do
  -- Full chain: outer grid -> column (label + flex-grow grid) -> inner grid child
  let innerGridProps := GridContainer.withTemplate #[.fr 1, .fr 1] #[.fr 1, .fr 1] 0
  let outerGridProps := GridContainer.withTemplate #[.fr 1] #[.fr 1] 0
  let tree := LayoutNode.gridBox 0 outerGridProps #[
    LayoutNode.column 1 #[
      LayoutNode.leaf 2 (ContentSize.mk' 100 30) {},  -- label "Shapes"
      LayoutNode.gridBox 3 innerGridProps #[
        LayoutNode.leaf 4 (ContentSize.mk' 60 60) { height := .percent 1.0 },
        LayoutNode.leaf 5 (ContentSize.mk' 60 60) { height := .percent 1.0 },
        LayoutNode.leaf 6 (ContentSize.mk' 60 60) { height := .percent 1.0 },
        LayoutNode.leaf 7 (ContentSize.mk' 60 60) { height := .percent 1.0 }
      ] (item := .flexChild (FlexItem.growing 1))  -- flex-grow to fill
    ] (box := { height := .percent 1.0 })
  ]
  let result := layout tree 300 300
  let outerColumn := result.get! 1
  let label := result.get! 2
  let innerGrid := result.get! 3
  shouldBeNear outerColumn.height 300.0  -- fills outer grid cell
  shouldBeNear label.height 30.0         -- fixed label
  shouldBeNear innerGrid.height 270.0    -- fills remaining (300 - 30)
  -- Each inner grid cell: 135px (270/2)
  for id in [4, 5, 6, 7] do
    let card := result.get! id
    shouldBeNear card.height 135.0

test "demo chain: card column fills inner grid cell" := do
  -- Card column inside grid cell with height: 100%
  let gridProps := GridContainer.withTemplate #[.fr 1] #[.fr 1] 0
  let tree := LayoutNode.gridBox 0 gridProps #[
    LayoutNode.column 1 #[
      LayoutNode.leaf 2 (ContentSize.mk' 60 60) {} (item := .flexChild (FlexItem.growing 1)),
      LayoutNode.leaf 3 (ContentSize.mk' 60 15) {}   -- card label (fixed)
    ] (box := { height := .percent 1.0 })
  ]
  let result := layout tree 150 200
  let cardColumn := result.get! 1
  let shapeArea := result.get! 2
  let cardLabel := result.get! 3
  shouldBeNear cardColumn.height 200.0   -- fills grid cell
  shouldBeNear cardLabel.height 15.0     -- fixed label
  shouldBeNear shapeArea.height 185.0    -- fills remaining via flex-grow



end Afferent.Tests.LayoutTests

/-
  Afferent Layout Tests
  Unit tests for Trellis grid layout behavior.
-/
import Afferent.Tests.Framework
import Afferent.Layout
import Trellis

namespace Afferent.Tests.LayoutTests

open Crucible
open Afferent.Tests
open Trellis

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

#generate_tests

end Afferent.Tests.LayoutTests

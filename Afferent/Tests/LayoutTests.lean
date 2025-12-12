/-
  Afferent Layout Tests
  Unit tests for the CSS Flexbox and Grid layout system.
-/
import Afferent.Tests.Framework
import Afferent.Layout

namespace Afferent.Tests.LayoutTests

open Afferent.Tests
open Afferent.Layout

/-! ## Basic Layout Tests -/

def test_single_leaf_layout : TestCase := {
  name := "single leaf node takes available space"
  run := do
    let node := LayoutNode.leaf 1 ⟨100, 50⟩
    let result := layout node 400 300
    let cl := result.get! 1
    shouldBeNear cl.width 100 0.01
    shouldBeNear cl.height 50 0.01
}

def test_leaf_with_fixed_constraints : TestCase := {
  name := "leaf respects fixed width/height constraints"
  run := do
    let node := LayoutNode.leaf 1 ⟨100, 50⟩
      (BoxConstraints.fixed 200 100)
    let result := layout node 400 300
    let cl := result.get! 1
    shouldBeNear cl.width 200 0.01
    shouldBeNear cl.height 100 0.01
}

/-! ## Flex Row Tests -/

def test_flex_row_basic : TestCase := {
  name := "flex row places items horizontally"
  run := do
    let node := LayoutNode.row 0 #[
      LayoutNode.leaf 1 ⟨50, 30⟩,
      LayoutNode.leaf 2 ⟨60, 30⟩,
      LayoutNode.leaf 3 ⟨70, 30⟩
    ]
    let result := layout node 400 100
    let cl1 := result.get! 1
    let cl2 := result.get! 2
    let cl3 := result.get! 3
    -- Items should be placed left to right
    shouldSatisfy (cl2.x > cl1.x) "item 2 should be right of item 1"
    shouldSatisfy (cl3.x > cl2.x) "item 3 should be right of item 2"
    -- Widths should match content
    shouldBeNear cl1.width 50 0.01
    shouldBeNear cl2.width 60 0.01
    shouldBeNear cl3.width 70 0.01
}

def test_flex_row_with_gap : TestCase := {
  name := "flex row respects gap"
  run := do
    let node := LayoutNode.row 0 #[
      LayoutNode.leaf 1 ⟨50, 30⟩,
      LayoutNode.leaf 2 ⟨50, 30⟩
    ] (gap := 20)
    let result := layout node 400 100
    let cl1 := result.get! 1
    let cl2 := result.get! 2
    -- Second item should start after first + gap
    let expectedX2 := cl1.x + cl1.width + 20
    shouldBeNear cl2.x expectedX2 0.01
}

def test_flex_grow_equal : TestCase := {
  name := "flex-grow: 1 distributes space equally"
  run := do
    let node := LayoutNode.flexBox 0 (FlexContainer.row) #[
      LayoutNode.leaf' 1 0 30 {} (.flexChild (FlexItem.growing 1)),
      LayoutNode.leaf' 2 0 30 {} (.flexChild (FlexItem.growing 1))
    ]
    let result := layout node 200 100
    let cl1 := result.get! 1
    let cl2 := result.get! 2
    -- Both should get equal width (200/2 = 100)
    shouldBeNear cl1.width 100 0.01
    shouldBeNear cl2.width 100 0.01
}

def test_flex_grow_proportional : TestCase := {
  name := "flex-grow distributes space proportionally"
  run := do
    let node := LayoutNode.flexBox 0 (FlexContainer.row) #[
      LayoutNode.leaf' 1 0 30 {} (.flexChild (FlexItem.growing 1)),
      LayoutNode.leaf' 2 0 30 {} (.flexChild (FlexItem.growing 2))
    ]
    let result := layout node 300 100
    let cl1 := result.get! 1
    let cl2 := result.get! 2
    -- 1:2 ratio, so 100 and 200
    shouldBeNear cl1.width 100 0.01
    shouldBeNear cl2.width 200 0.01
}

def test_flex_grow_with_basis : TestCase := {
  name := "flex-grow distributes remaining space after basis"
  run := do
    let node := LayoutNode.flexBox 0 (FlexContainer.row) #[
      LayoutNode.leaf' 1 50 30 {} (.flexChild { grow := 1, basis := .length 50 }),
      LayoutNode.leaf' 2 50 30 {} (.flexChild { grow := 1, basis := .length 50 })
    ]
    let result := layout node 300 100
    let cl1 := result.get! 1
    let cl2 := result.get! 2
    -- Basis = 100 total, remaining = 200, split equally
    shouldBeNear cl1.width 150 0.01
    shouldBeNear cl2.width 150 0.01
}

/-! ## Flex Column Tests -/

def test_flex_column_basic : TestCase := {
  name := "flex column places items vertically"
  run := do
    let node := LayoutNode.column 0 #[
      LayoutNode.leaf 1 ⟨100, 40⟩,
      LayoutNode.leaf 2 ⟨100, 50⟩,
      LayoutNode.leaf 3 ⟨100, 60⟩
    ]
    let result := layout node 200 400
    let cl1 := result.get! 1
    let cl2 := result.get! 2
    let cl3 := result.get! 3
    -- Items should be placed top to bottom
    shouldSatisfy (cl2.y > cl1.y) "item 2 should be below item 1"
    shouldSatisfy (cl3.y > cl2.y) "item 3 should be below item 2"
    -- Heights should match content
    shouldBeNear cl1.height 40 0.01
    shouldBeNear cl2.height 50 0.01
    shouldBeNear cl3.height 60 0.01
}

/-! ## Justify Content Tests -/

def test_justify_content_center : TestCase := {
  name := "justify-content: center centers items"
  run := do
    let props := { FlexContainer.row with justifyContent := .center }
    let node := LayoutNode.flexBox 0 props #[
      LayoutNode.leaf 1 ⟨100, 30⟩
    ]
    let result := layout node 400 100
    let cl := result.get! 1
    -- Item should be centered: (400 - 100) / 2 = 150
    shouldBeNear cl.x 150 0.01
}

def test_justify_content_flex_end : TestCase := {
  name := "justify-content: flex-end aligns to end"
  run := do
    let props := { FlexContainer.row with justifyContent := .flexEnd }
    let node := LayoutNode.flexBox 0 props #[
      LayoutNode.leaf 1 ⟨100, 30⟩
    ]
    let result := layout node 400 100
    let cl := result.get! 1
    -- Item should be at right: 400 - 100 = 300
    shouldBeNear cl.x 300 0.01
}

def test_justify_content_space_between : TestCase := {
  name := "justify-content: space-between distributes space between items"
  run := do
    let props := { FlexContainer.row with justifyContent := .spaceBetween }
    let node := LayoutNode.flexBox 0 props #[
      LayoutNode.leaf 1 ⟨50, 30⟩,
      LayoutNode.leaf 2 ⟨50, 30⟩,
      LayoutNode.leaf 3 ⟨50, 30⟩
    ]
    let result := layout node 400 100
    let cl1 := result.get! 1
    let cl2 := result.get! 2
    let cl3 := result.get! 3
    -- First at start, last at end
    shouldBeNear cl1.x 0 0.01
    shouldBeNear (cl3.x + cl3.width) 400 0.01
    -- Middle is centered between gaps
    let gap := (400 - 150) / 2  -- 125
    shouldBeNear cl2.x (50 + gap) 0.01
}

/-! ## Align Items Tests -/

def test_align_items_center : TestCase := {
  name := "align-items: center centers items on cross axis"
  run := do
    let props := { FlexContainer.row with alignItems := .center }
    let node := LayoutNode.flexBox 0 props #[
      LayoutNode.leaf 1 ⟨50, 30⟩
    ]
    let result := layout node 200 100
    let cl := result.get! 1
    -- Item should be vertically centered: (100 - 30) / 2 = 35
    shouldBeNear cl.y 35 0.01
}

def test_align_items_flex_end : TestCase := {
  name := "align-items: flex-end aligns items to cross end"
  run := do
    let props := { FlexContainer.row with alignItems := .flexEnd }
    let node := LayoutNode.flexBox 0 props #[
      LayoutNode.leaf 1 ⟨50, 30⟩
    ]
    let result := layout node 200 100
    let cl := result.get! 1
    -- Item should be at bottom: 100 - 30 = 70
    shouldBeNear cl.y 70 0.01
}

def test_align_items_stretch : TestCase := {
  name := "align-items: stretch makes items fill cross axis"
  run := do
    let props := { FlexContainer.row with alignItems := .stretch }
    let node := LayoutNode.flexBox 0 props #[
      LayoutNode.leaf 1 ⟨50, 30⟩
    ]
    let result := layout node 200 100
    let cl := result.get! 1
    -- Item should stretch to fill height
    shouldBeNear cl.y 0 0.01
    shouldBeNear cl.height 100 0.01
}

/-! ## Nested Layout Tests -/

def test_nested_flex_containers : TestCase := {
  name := "nested flex containers layout correctly"
  run := do
    let inner := LayoutNode.column 10 #[
      LayoutNode.leaf 11 ⟨40, 20⟩,
      LayoutNode.leaf 12 ⟨40, 20⟩
    ]
    let outer := LayoutNode.row 0 #[
      LayoutNode.leaf 1 ⟨50, 50⟩,
      inner.withItem (.flexChild (FlexItem.growing 1))
    ]
    let result := layout outer 200 100
    let cl1 := result.get! 1
    let clInner := result.get! 10
    -- Outer leaf at start
    shouldBeNear cl1.x 0 0.01
    -- Inner container fills remaining space
    shouldBeNear clInner.x 50 0.01
    shouldBeNear clInner.width 150 0.01
}

/-! ## Edge Case Tests -/

def test_empty_container : TestCase := {
  name := "empty container produces no child layouts"
  run := do
    let node := LayoutNode.row 0 #[]
    let result := layout node 200 100
    -- Only the container itself
    shouldBe result.size 1
}

def test_single_item_space_between : TestCase := {
  name := "space-between with single item places at start"
  run := do
    let props := { FlexContainer.row with justifyContent := .spaceBetween }
    let node := LayoutNode.flexBox 0 props #[
      LayoutNode.leaf 1 ⟨100, 30⟩
    ]
    let result := layout node 400 100
    let cl := result.get! 1
    shouldBeNear cl.x 0 0.01
}

def test_zero_flex_grow : TestCase := {
  name := "zero flex-grow items keep their basis size"
  run := do
    let node := LayoutNode.flexBox 0 (FlexContainer.row) #[
      LayoutNode.leaf' 1 50 30 {} (.flexChild { grow := 0, basis := .length 50 }),
      LayoutNode.leaf' 2 50 30 {} (.flexChild { grow := 1, basis := .length 50 })
    ]
    let result := layout node 300 100
    let cl1 := result.get! 1
    let cl2 := result.get! 2
    -- First stays at 50, second gets remaining 250
    shouldBeNear cl1.width 50 0.01
    shouldBeNear cl2.width 250 0.01
}

/-! ## Test Runner -/

def allTests : List TestCase := [
  -- Basic tests
  test_single_leaf_layout,
  test_leaf_with_fixed_constraints,
  -- Flex row tests
  test_flex_row_basic,
  test_flex_row_with_gap,
  test_flex_grow_equal,
  test_flex_grow_proportional,
  test_flex_grow_with_basis,
  -- Flex column tests
  test_flex_column_basic,
  -- Justify content tests
  test_justify_content_center,
  test_justify_content_flex_end,
  test_justify_content_space_between,
  -- Align items tests
  test_align_items_center,
  test_align_items_flex_end,
  test_align_items_stretch,
  -- Nested tests
  test_nested_flex_containers,
  -- Edge cases
  test_empty_container,
  test_single_item_space_between,
  test_zero_flex_grow
]

def runAllTests : IO UInt32 :=
  runTests "Layout Tests" allTests

end Afferent.Tests.LayoutTests

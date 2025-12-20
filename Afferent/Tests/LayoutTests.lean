/-
  Afferent Layout Tests
  Unit tests for the CSS Flexbox and Grid layout system.
-/
import Afferent.Tests.Framework
import Afferent.Layout

namespace Afferent.Tests.LayoutTests

open Crucible
open Afferent.Tests
open Afferent.Layout

testSuite "Layout Tests"

/-! ## Basic Layout Tests -/

test "single leaf node takes available space" := do
  let node := LayoutNode.leaf 1 ⟨100, 50⟩
  let result := layout node 400 300
  let cl := result.get! 1
  shouldBeNear cl.width 100 0.01
  shouldBeNear cl.height 50 0.01

test "leaf respects fixed width/height constraints" := do
  let node := LayoutNode.leaf 1 ⟨100, 50⟩
    (BoxConstraints.fixed 200 100)
  let result := layout node 400 300
  let cl := result.get! 1
  shouldBeNear cl.width 200 0.01
  shouldBeNear cl.height 100 0.01

/-! ## Flex Row Tests -/

test "flex row places items horizontally" := do
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

test "flex row respects gap" := do
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

test "flex-grow: 1 distributes space equally" := do
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

test "flex-grow distributes space proportionally" := do
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

test "flex-grow distributes remaining space after basis" := do
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

/-! ## Flex Column Tests -/

test "flex column places items vertically" := do
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

/-! ## Justify Content Tests -/

test "justify-content: center centers items" := do
  let props := { FlexContainer.row with justifyContent := .center }
  let node := LayoutNode.flexBox 0 props #[
    LayoutNode.leaf 1 ⟨100, 30⟩
  ]
  let result := layout node 400 100
  let cl := result.get! 1
  -- Item should be centered: (400 - 100) / 2 = 150
  shouldBeNear cl.x 150 0.01

test "justify-content: flex-end aligns to end" := do
  let props := { FlexContainer.row with justifyContent := .flexEnd }
  let node := LayoutNode.flexBox 0 props #[
    LayoutNode.leaf 1 ⟨100, 30⟩
  ]
  let result := layout node 400 100
  let cl := result.get! 1
  -- Item should be at right: 400 - 100 = 300
  shouldBeNear cl.x 300 0.01

test "justify-content: space-between distributes space between items" := do
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

/-! ## Align Items Tests -/

test "align-items: center centers items on cross axis" := do
  let props := { FlexContainer.row with alignItems := .center }
  let node := LayoutNode.flexBox 0 props #[
    LayoutNode.leaf 1 ⟨50, 30⟩
  ]
  let result := layout node 200 100
  let cl := result.get! 1
  -- Item should be vertically centered: (100 - 30) / 2 = 35
  shouldBeNear cl.y 35 0.01

test "align-items: flex-end aligns items to cross end" := do
  let props := { FlexContainer.row with alignItems := .flexEnd }
  let node := LayoutNode.flexBox 0 props #[
    LayoutNode.leaf 1 ⟨50, 30⟩
  ]
  let result := layout node 200 100
  let cl := result.get! 1
  -- Item should be at bottom: 100 - 30 = 70
  shouldBeNear cl.y 70 0.01

test "align-items: stretch makes items fill cross axis" := do
  let props := { FlexContainer.row with alignItems := .stretch }
  let node := LayoutNode.flexBox 0 props #[
    LayoutNode.leaf 1 ⟨50, 30⟩
  ]
  let result := layout node 200 100
  let cl := result.get! 1
  -- Item should stretch to fill height
  shouldBeNear cl.y 0 0.01
  shouldBeNear cl.height 100 0.01

/-! ## Nested Layout Tests -/

test "nested flex containers layout correctly" := do
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

/-! ## Edge Case Tests -/

test "empty container produces no child layouts" := do
  let node := LayoutNode.row 0 #[]
  let result := layout node 200 100
  -- Only the container itself
  result.size ≡ 1

test "space-between with single item places at start" := do
  let props := { FlexContainer.row with justifyContent := .spaceBetween }
  let node := LayoutNode.flexBox 0 props #[
    LayoutNode.leaf 1 ⟨100, 30⟩
  ]
  let result := layout node 400 100
  let cl := result.get! 1
  shouldBeNear cl.x 0 0.01

test "zero flex-grow items keep their basis size" := do
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

/-! ## Grid Basic Tests -/

test "grid with 3 equal fr columns" := do
  let props := GridContainer.columns 3
  let node := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf 1 ⟨0, 30⟩,
    LayoutNode.leaf 2 ⟨0, 30⟩,
    LayoutNode.leaf 3 ⟨0, 30⟩
  ]
  let result := layout node 300 100
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  let cl3 := result.get! 3
  -- Each column should be 100px wide (300 / 3)
  shouldBeNear cl1.width 100 0.01
  shouldBeNear cl2.width 100 0.01
  shouldBeNear cl3.width 100 0.01
  -- Items placed left to right
  shouldBeNear cl1.x 0 0.01
  shouldBeNear cl2.x 100 0.01
  shouldBeNear cl3.x 200 0.01

test "grid with mixed track sizes (fixed + fr)" := do
  let props := GridContainer.withColumns #[.px 50, .fr 1, .fr 2]
  let node := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf 1 ⟨0, 30⟩,
    LayoutNode.leaf 2 ⟨0, 30⟩,
    LayoutNode.leaf 3 ⟨0, 30⟩
  ]
  let result := layout node 350 100
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  let cl3 := result.get! 3
  -- First column: fixed 50px
  -- Remaining: 300px, split 1:2 = 100px and 200px
  shouldBeNear cl1.width 50 0.01
  shouldBeNear cl2.width 100 0.01
  shouldBeNear cl3.width 200 0.01

test "grid respects column and row gaps" := do
  let props := GridContainer.columns 2 (gap := 20)
  let node := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf 1 ⟨0, 30⟩,
    LayoutNode.leaf 2 ⟨0, 30⟩,
    LayoutNode.leaf 3 ⟨0, 30⟩,
    LayoutNode.leaf 4 ⟨0, 30⟩
  ]
  let result := layout node 220 100
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  let cl3 := result.get! 3
  -- 220 - 20 gap = 200, split into 2 columns = 100 each
  shouldBeNear cl1.width 100 0.01
  shouldBeNear cl2.width 100 0.01
  -- Second column starts after first + gap
  shouldBeNear cl2.x 120 0.01
  -- Third item is on second row
  shouldSatisfy (cl3.y > cl1.y) "row 2 should be below row 1"

/-! ## Grid Auto-Placement Tests -/

test "grid auto-places items in row-major order" := do
  let props := GridContainer.columns 3
  let node := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf 1 ⟨0, 30⟩,
    LayoutNode.leaf 2 ⟨0, 30⟩,
    LayoutNode.leaf 3 ⟨0, 30⟩,
    LayoutNode.leaf 4 ⟨0, 30⟩,
    LayoutNode.leaf 5 ⟨0, 30⟩
  ]
  let result := layout node 300 200
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  let cl3 := result.get! 3
  let cl4 := result.get! 4
  let cl5 := result.get! 5
  -- First row: items 1, 2, 3
  shouldBeNear cl1.y cl2.y 0.01
  shouldBeNear cl2.y cl3.y 0.01
  -- Second row: items 4, 5
  shouldSatisfy (cl4.y > cl1.y) "item 4 should be on row 2"
  shouldBeNear cl4.y cl5.y 0.01
  -- Item 4 should be at column 0
  shouldBeNear cl4.x 0 0.01

/-! ## Grid Explicit Placement Tests -/

test "grid places items at explicit positions" := do
  let props := GridContainer.withTemplate
    #[.fr 1, .fr 1]  -- 2 rows
    #[.fr 1, .fr 1, .fr 1]  -- 3 columns
  let node := LayoutNode.gridBox 0 props #[
    -- Place at row 2, col 3 (bottom-right)
    LayoutNode.leaf' 1 0 0 {} (.gridChild (GridItem.atPosition 2 3))
  ]
  let result := layout node 300 200
  let cl := result.get! 1
  -- Should be in bottom-right cell (col 2, row 1 in 0-indexed)
  shouldBeNear cl.x 200 0.01  -- column 3 starts at 200
  shouldBeNear cl.y 100 0.01  -- row 2 starts at 100

/-! ## Grid Spanning Tests -/

test "grid item spanning multiple columns" := do
  let props := GridContainer.columns 3
  let spanItem := { GridItem.default with
    placement := { column := GridSpan.spanTracks 2 }
  }
  let node := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf' 1 0 30 {} (.gridChild spanItem),  -- spans 2 cols
    LayoutNode.leaf 2 ⟨0, 30⟩
  ]
  let result := layout node 300 100
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  -- First item spans 2 columns = 200px
  shouldBeNear cl1.width 200 0.01
  -- Second item is in column 3
  shouldBeNear cl2.x 200 0.01
  shouldBeNear cl2.width 100 0.01

test "grid item spanning multiple rows" := do
  let props := GridContainer.columns 2
  let spanItem := { GridItem.default with
    placement := { row := GridSpan.spanTracks 2 }
  }
  let node := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf' 1 0 0 {} (.gridChild spanItem),  -- spans 2 rows
    LayoutNode.leaf 2 ⟨0, 50⟩,
    LayoutNode.leaf 3 ⟨0, 50⟩
  ]
  let result := layout node 200 200
  let cl1 := result.get! 1
  let cl2 := result.get! 2
  let cl3 := result.get! 3
  -- First item spans 2 rows
  shouldBeNear cl1.height 100 0.01
  -- Items 2 and 3 are in column 2, stacked vertically
  shouldBeNear cl2.x 100 0.01
  shouldBeNear cl3.x 100 0.01
  shouldSatisfy (cl3.y > cl2.y) "item 3 should be below item 2"

/-! ## Grid Alignment Tests -/

test "grid justify-items: center centers items horizontally in cells" := do
  let props := { GridContainer.columns 1 with justifyItems := .center }
  let node := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf 1 ⟨50, 30⟩
  ]
  let result := layout node 200 100
  let cl := result.get! 1
  -- Item should be centered: (200 - 50) / 2 = 75
  shouldBeNear cl.x 75 0.01
  shouldBeNear cl.width 50 0.01

test "grid align-items: center centers items vertically in cells" := do
  -- Need explicit row template with fr to make row fill available space
  let props := { GridContainer.withTemplate #[.fr 1] #[.fr 1] with alignItems := .center }
  let node := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf 1 ⟨50, 30⟩
  ]
  let result := layout node 200 100
  let cl := result.get! 1
  -- Item should be centered: (100 - 30) / 2 = 35
  shouldBeNear cl.y 35 0.01
  shouldBeNear cl.height 30 0.01

test "grid stretch makes items fill cells" := do
  -- Need explicit row template with fr to make row fill available space
  let props := { GridContainer.withTemplate #[.fr 1] #[.fr 1, .fr 1] with
    justifyItems := .stretch
    alignItems := .stretch
  }
  let node := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf 1 ⟨30, 20⟩
  ]
  let result := layout node 200 100
  let cl := result.get! 1
  -- Item should stretch to fill cell
  shouldBeNear cl.width 100 0.01
  shouldBeNear cl.height 100 0.01

test "grid justify-self overrides container justify-items" := do
  let props := { GridContainer.columns 1 with justifyItems := .stretch }
  let itemProps := { GridItem.default with justifySelf := some .flexEnd }
  let node := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf' 1 50 30 {} (.gridChild itemProps)
  ]
  let result := layout node 200 100
  let cl := result.get! 1
  -- Item should be at end despite container stretch
  shouldBeNear cl.x 150 0.01
  shouldBeNear cl.width 50 0.01

/-! ## Grid Edge Cases -/

test "empty grid container produces no child layouts" := do
  let props := GridContainer.columns 3
  let node := LayoutNode.gridBox 0 props #[]
  let result := layout node 300 100
  -- Only the container itself
  result.size ≡ 1

test "single item grid fills available space with fr rows" := do
  -- Need explicit row template with fr to make row fill available space
  let props := GridContainer.withTemplate #[.fr 1] #[.fr 1]
  let node := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf 1 ⟨50, 30⟩
  ]
  let result := layout node 200 100
  let cl := result.get! 1
  -- With stretch (default) and fr row, should fill the cell
  shouldBeNear cl.width 200 0.01
  shouldBeNear cl.height 100 0.01

test "grid creates implicit rows for overflow items" := do
  -- Only 1 explicit row, but 4 items in 2 columns
  let props := GridContainer.withTemplate #[.fr 1] #[.fr 1, .fr 1]
  let node := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf 1 ⟨0, 50⟩,
    LayoutNode.leaf 2 ⟨0, 50⟩,
    LayoutNode.leaf 3 ⟨0, 50⟩,
    LayoutNode.leaf 4 ⟨0, 50⟩
  ]
  let result := layout node 200 200
  let cl3 := result.get! 3
  let cl4 := result.get! 4
  -- Items 3 and 4 should be on implicit row 2
  shouldSatisfy (cl3.y > 0) "item 3 should be on row 2"
  shouldBeNear cl3.y cl4.y 0.01

#generate_tests

end Afferent.Tests.LayoutTests

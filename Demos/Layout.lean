/-
  Layout Demo - CSS Flexbox and Grid layout visualization
-/
import Afferent
import Afferent.Layout

open Afferent CanvasM
open Afferent.Layout

namespace Demos

/-- Colors for layout cells -/
def cellColors : Array Color := #[
  Color.red,
  Color.green,
  Color.blue,
  Color.yellow,
  Color.cyan,
  Color.magenta,
  Color.orange,
  Color.purple,
  Color.hsv 0.9 0.6 1.0,   -- pink
  Color.hsv 0.5 0.7 0.8    -- teal
]

/-- Get a color for a node ID -/
def colorForId (id : Nat) : Color :=
  cellColors[id % cellColors.size]!

/-- Draw a layout result onto the canvas -/
def drawLayoutResult (result : LayoutResult) (offsetX offsetY : Float := 0) : CanvasM Unit := do
  for cl in result.layouts do
    let rect := cl.borderRect
    -- Draw fill
    setFillColor (colorForId cl.nodeId |>.withAlpha 0.7)
    fillRectXYWH (rect.x + offsetX) (rect.y + offsetY) rect.width rect.height
    -- Draw border
    setStrokeColor Color.white
    setLineWidth 2
    strokeRectXYWH (rect.x + offsetX) (rect.y + offsetY) rect.width rect.height

/-- Demo 1: Basic Flex Row -/
def demoFlexRow : CanvasM Unit := do
  -- Title
  -- setFillColor Color.white
  -- fillTextXY "Flex Row (justify-content: flex-start)" 50 30 font

  let tree := LayoutNode.row 0 #[
    LayoutNode.leaf 1 ⟨80, 60⟩,
    LayoutNode.leaf 2 ⟨100, 60⟩,
    LayoutNode.leaf 3 ⟨70, 60⟩
  ] (gap := 10)

  let result := layout tree 350 80
  drawLayoutResult result 50 50

/-- Demo 2: Flex Row with justify-content: center -/
def demoFlexRowCenter : CanvasM Unit := do
  let props := { FlexContainer.row with justifyContent := .center, gap := 10 }
  let tree := LayoutNode.flexBox 0 props #[
    LayoutNode.leaf 1 ⟨60, 50⟩,
    LayoutNode.leaf 2 ⟨60, 50⟩,
    LayoutNode.leaf 3 ⟨60, 50⟩
  ]

  let result := layout tree 350 70
  drawLayoutResult result 50 160

/-- Demo 3: Flex Row with space-between -/
def demoFlexRowSpaceBetween : CanvasM Unit := do
  let props := { FlexContainer.row with justifyContent := .spaceBetween }
  let tree := LayoutNode.flexBox 0 props #[
    LayoutNode.leaf 1 ⟨50, 50⟩,
    LayoutNode.leaf 2 ⟨50, 50⟩,
    LayoutNode.leaf 3 ⟨50, 50⟩
  ]

  let result := layout tree 350 70
  drawLayoutResult result 50 260

/-- Demo 4: Flex Grow -/
def demoFlexGrow : CanvasM Unit := do
  let tree := LayoutNode.flexBox 0 (FlexContainer.row 10) #[
    LayoutNode.leaf' 1 0 50 {} (.flexChild (FlexItem.growing 1)),
    LayoutNode.leaf' 2 0 50 {} (.flexChild (FlexItem.growing 2)),
    LayoutNode.leaf' 3 0 50 {} (.flexChild (FlexItem.growing 1))
  ]

  let result := layout tree 350 70
  drawLayoutResult result 50 360

/-- Demo 5: Flex Column -/
def demoFlexColumn : CanvasM Unit := do
  let tree := LayoutNode.column 0 #[
    LayoutNode.leaf 1 ⟨100, 40⟩,
    LayoutNode.leaf 2 ⟨120, 50⟩,
    LayoutNode.leaf 3 ⟨80, 45⟩
  ] (gap := 10)

  let result := layout tree 150 200
  drawLayoutResult result 450 50

/-- Demo 6: Align Items -/
def demoAlignItems : CanvasM Unit := do
  let props := { FlexContainer.row with alignItems := .center, gap := 10 }
  let tree := LayoutNode.flexBox 0 props #[
    LayoutNode.leaf 1 ⟨60, 30⟩,
    LayoutNode.leaf 2 ⟨60, 60⟩,
    LayoutNode.leaf 3 ⟨60, 45⟩
  ]

  let result := layout tree 250 80
  drawLayoutResult result 450 280

/-- Demo 7: Nested Containers -/
def demoNested : CanvasM Unit := do
  let innerColumn := LayoutNode.column 10 #[
    LayoutNode.leaf 11 ⟨50, 30⟩,
    LayoutNode.leaf 12 ⟨50, 30⟩
  ] (gap := 5)

  let tree := LayoutNode.row 0 #[
    LayoutNode.leaf 1 ⟨60, 80⟩,
    innerColumn.withItem (.flexChild (FlexItem.growing 1)),
    LayoutNode.leaf 2 ⟨60, 80⟩
  ] (gap := 10)

  let result := layout tree 300 100
  drawLayoutResult result 450 390

/-- Demo 8: Complex Layout -/
def demoComplex : CanvasM Unit := do
  -- Header row
  let header := LayoutNode.row 100 #[
    LayoutNode.leaf' 101 0 40 {} (.flexChild (FlexItem.growing 1))
  ]

  -- Content area with sidebar
  let sidebar := LayoutNode.column 200 #[
    LayoutNode.leaf 201 ⟨80, 50⟩,
    LayoutNode.leaf 202 ⟨80, 50⟩,
    LayoutNode.leaf 203 ⟨80, 50⟩
  ] (gap := 5)

  let mainContent := LayoutNode.flexBox 300
    { FlexContainer.column with alignItems := .stretch }
    #[
      LayoutNode.leaf' 301 0 0 {} (.flexChild (FlexItem.growing 1))
    ]

  let content := LayoutNode.row 400 #[
    sidebar,
    mainContent.withItem (.flexChild (FlexItem.growing 1))
  ] (gap := 10)

  -- Main layout
  let tree := LayoutNode.column 0 #[
    header,
    content.withItem (.flexChild (FlexItem.growing 1))
  ] (gap := 10)

  let result := layout tree 350 250
  drawLayoutResult result 50 500

/-- Draw all layout demos -/
def renderLayoutM : CanvasM Unit := do
  -- Background
  setFillColor (Color.mk 0.1 0.1 0.15 1.0)
  fillRectXYWH 0 0 1000 800

  -- Draw section labels as colored bars
  setFillColor ((Color.gray 0.5).withAlpha 0.3)
  fillRectXYWH 40 40 370 100  -- Row 1
  fillRectXYWH 40 150 370 90  -- Row 2 (center)
  fillRectXYWH 40 250 370 90  -- Row 3 (space-between)
  fillRectXYWH 40 350 370 90  -- Row 4 (grow)
  fillRectXYWH 440 40 170 220 -- Column
  fillRectXYWH 440 270 270 100 -- Align items
  fillRectXYWH 440 380 320 120 -- Nested
  fillRectXYWH 40 490 370 270  -- Complex

  -- Run demos
  demoFlexRow
  demoFlexRowCenter
  demoFlexRowSpaceBetween
  demoFlexGrow
  demoFlexColumn
  demoAlignItems
  demoNested
  demoComplex

end Demos

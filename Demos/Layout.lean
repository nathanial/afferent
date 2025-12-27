/-
  Layout Demo - CSS Flexbox and Grid layout visualization
-/
import Afferent
import Trellis

open Afferent CanvasM
open Trellis

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
    LayoutNode.leaf 1 (ContentSize.mk' 80 60),
    LayoutNode.leaf 2 (ContentSize.mk' 100 60),
    LayoutNode.leaf 3 (ContentSize.mk' 70 60)
  ] (gap := 10)

  let result := layout tree 350 80
  drawLayoutResult result 50 50

/-- Demo 2: Flex Row with justify-content: center -/
def demoFlexRowCenter : CanvasM Unit := do
  let props := { FlexContainer.row with justifyContent := .center, gap := 10 }
  let tree := LayoutNode.flexBox 0 props #[
    LayoutNode.leaf 1 (ContentSize.mk' 60 50),
    LayoutNode.leaf 2 (ContentSize.mk' 60 50),
    LayoutNode.leaf 3 (ContentSize.mk' 60 50)
  ]

  let result := layout tree 350 70
  drawLayoutResult result 50 160

/-- Demo 3: Flex Row with space-between -/
def demoFlexRowSpaceBetween : CanvasM Unit := do
  let props := { FlexContainer.row with justifyContent := .spaceBetween }
  let tree := LayoutNode.flexBox 0 props #[
    LayoutNode.leaf 1 (ContentSize.mk' 50 50),
    LayoutNode.leaf 2 (ContentSize.mk' 50 50),
    LayoutNode.leaf 3 (ContentSize.mk' 50 50)
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
    LayoutNode.leaf 1 (ContentSize.mk' 100 40),
    LayoutNode.leaf 2 (ContentSize.mk' 120 50),
    LayoutNode.leaf 3 (ContentSize.mk' 80 45)
  ] (gap := 10)

  let result := layout tree 150 200
  drawLayoutResult result 450 50

/-- Demo 6: Align Items -/
def demoAlignItems : CanvasM Unit := do
  let props := { FlexContainer.row with alignItems := .center, gap := 10 }
  let tree := LayoutNode.flexBox 0 props #[
    LayoutNode.leaf 1 (ContentSize.mk' 60 30),
    LayoutNode.leaf 2 (ContentSize.mk' 60 60),
    LayoutNode.leaf 3 (ContentSize.mk' 60 45)
  ]

  let result := layout tree 250 80
  drawLayoutResult result 450 280

/-- Demo 7: Nested Containers -/
def demoNested : CanvasM Unit := do
  let innerColumn := LayoutNode.column 10 #[
    LayoutNode.leaf 11 (ContentSize.mk' 50 30),
    LayoutNode.leaf 12 (ContentSize.mk' 50 30)
  ] (gap := 5)

  let tree := LayoutNode.row 0 #[
    LayoutNode.leaf 1 (ContentSize.mk' 60 80),
    innerColumn.withItem (.flexChild (FlexItem.growing 1)),
    LayoutNode.leaf 2 (ContentSize.mk' 60 80)
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
    LayoutNode.leaf 201 (ContentSize.mk' 80 50),
    LayoutNode.leaf 202 (ContentSize.mk' 80 50),
    LayoutNode.leaf 203 (ContentSize.mk' 80 50)
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

/-- Labels (text, x, y) in layout-canvas coordinates. -/
def layoutLabels : Array (String × Float × Float) := #[
  ("Row: 3 items left-aligned with 10px gap", 50, 38),
  ("Expected: [80px][10][100px][10][70px] touching left edge", 50, 135),
  ("Row + justify:center", 50, 148),
  ("Expected: 3 equal boxes (60px) centered horizontally", 50, 225),
  ("Row + justify:space-between", 50, 248),
  ("Expected: 3 boxes (50px) - first at left, last at right, middle centered", 50, 325),
  ("Row + flex-grow (1:2:1 ratio)", 50, 348),
  ("Expected: 3 boxes filling width, middle is 2x wider than sides", 50, 425),
  ("Column: 3 items", 450, 38),
  ("stacked vertically", 450, 52),
  ("with 10px gap", 450, 66),
  ("Row + align:center (cross-axis)", 450, 268),
  ("Expected: different heights, vertically centered", 450, 355),
  ("Nested: row containing column", 450, 378),
  ("Expected: [fixed][column expands][fixed]", 450, 495),
  ("Complex: header + (sidebar | main)", 50, 488),
  ("Expected: full-width header on top,", 50, 745),
  ("sidebar (3 items) left, main expands right", 200, 745)
]

/-- Draw the layout demo shapes (background, bars, and layout results). -/
def renderLayoutShapesM : CanvasM Unit := do
  -- Background
  setFillColor (Color.rgba 0.1 0.1 0.15 1.0)
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

/-- Draw layout demo labels in layout-canvas coordinates (affected by current transform). -/
def renderLayoutLabelsM (font : Font) : CanvasM Unit := do
  setFillColor Color.white
  for (text, x, y) in layoutLabels do
    fillTextXY text x y font

/-- Draw layout demo labels in screen coordinates, mapping from layout-canvas space. -/
def renderLayoutLabelsMappedM (font : Font) (offsetX offsetY scale : Float) : CanvasM Unit := do
  setFillColor Color.white
  for (text, x, y) in layoutLabels do
    fillTextXY text (offsetX + x * scale) (offsetY + y * scale) font

/-- Draw all layout demos (shapes + labels). -/
def renderLayoutM (font : Font) : CanvasM Unit := do
  renderLayoutShapesM
  renderLayoutLabelsM font

end Demos

/-
  Grid Demo - CSS Grid layout visualization
-/
import Afferent
import Trellis

open Afferent CanvasM
open Trellis

namespace Demos

/-- Colors for layout cells -/
def gridCellColors : Array Color := #[
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
def gridColorForId (id : Nat) : Color :=
  gridCellColors[id % gridCellColors.size]!

/-- Draw a layout result onto the canvas -/
def drawGridResult (result : LayoutResult) (offsetX offsetY : Float := 0) : CanvasM Unit := do
  for cl in result.layouts do
    let rect := cl.borderRect
    -- Draw fill
    setFillColor (gridColorForId cl.nodeId |>.withAlpha 0.7)
    fillRectXYWH (rect.x + offsetX) (rect.y + offsetY) rect.width rect.height
    -- Draw border
    setStrokeColor Color.white
    setLineWidth 2
    strokeRectXYWH (rect.x + offsetX) (rect.y + offsetY) rect.width rect.height

/-- Demo 1: Simple 3-column grid with equal fr units -/
def demoGrid3Col : CanvasM Unit := do
  -- Grid with 3 equal columns (1fr 1fr 1fr)
  let props := GridContainer.columns 3 (gap := 10)
  let tree := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf 1 (ContentSize.mk' 0 60),  -- Content size doesn't matter with stretch
    LayoutNode.leaf 2 (ContentSize.mk' 0 60),
    LayoutNode.leaf 3 (ContentSize.mk' 0 60)
  ]

  let result := layout tree 350 80
  drawGridResult result 50 50

/-- Demo 2: Mixed track sizes (100px, 1fr, 2fr) -/
def demoGridMixed : CanvasM Unit := do
  let props := GridContainer.withColumns #[.px 80, .fr 1, .fr 2] (gap := 10)
  let tree := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf 1 (ContentSize.mk' 0 60),
    LayoutNode.leaf 2 (ContentSize.mk' 0 60),
    LayoutNode.leaf 3 (ContentSize.mk' 0 60)
  ]

  let result := layout tree 350 80
  drawGridResult result 50 150

/-- Demo 3: Auto-placement (6 items in 3 cols) -/
def demoGridAuto : CanvasM Unit := do
  let props := GridContainer.columns 3 (gap := 10)
  let tree := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf 1 (ContentSize.mk' 0 50),
    LayoutNode.leaf 2 (ContentSize.mk' 0 50),
    LayoutNode.leaf 3 (ContentSize.mk' 0 50),
    LayoutNode.leaf 4 (ContentSize.mk' 0 50),
    LayoutNode.leaf 5 (ContentSize.mk' 0 50),
    LayoutNode.leaf 6 (ContentSize.mk' 0 50)
  ]

  let result := layout tree 350 130
  drawGridResult result 50 250

/-- Demo 4: Explicit item placement -/
def demoGridExplicit : CanvasM Unit := do
  let props := GridContainer.withTemplate
    #[.fr 1, .fr 1]  -- 2 rows
    #[.fr 1, .fr 1, .fr 1]  -- 3 cols
    (gap := 10)

  -- Place items at specific positions
  let tree := LayoutNode.gridBox 0 props #[
    -- Item at row 1, col 1
    LayoutNode.leaf' 1 0 0 {} (.gridChild (GridItem.atPosition 1 1)),
    -- Item at row 2, col 3
    LayoutNode.leaf' 2 0 0 {} (.gridChild (GridItem.atPosition 2 3)),
    -- Item at row 1, col 3
    LayoutNode.leaf' 3 0 0 {} (.gridChild (GridItem.atPosition 1 3))
  ]

  let result := layout tree 300 120
  drawGridResult result 450 50

/-- Demo 5: Items spanning multiple cells -/
def demoGridSpan : CanvasM Unit := do
  let props := GridContainer.columns 3 (gap := 10)

  -- First item spans 2 columns
  let spanItem := { GridItem.default with
    placement := { column := GridSpan.spanTracks 2 }
  }

  let tree := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf' 1 0 50 {} (.gridChild spanItem),  -- spans 2 cols
    LayoutNode.leaf 2 (ContentSize.mk' 0 50),
    LayoutNode.leaf 3 (ContentSize.mk' 0 50),
    LayoutNode.leaf 4 (ContentSize.mk' 0 50)
  ]

  let result := layout tree 350 130
  drawGridResult result 450 200

/-- Demo 6: Item alignment (justifySelf, alignSelf) -/
def demoGridAlign : CanvasM Unit := do
  let props := { GridContainer.columns 3 (gap := 10) with
    justifyItems := .stretch
    alignItems := .stretch
  }

  let tree := LayoutNode.gridBox 0 props #[
    -- Stretch (default)
    LayoutNode.leaf' 1 50 40 {} (.gridChild GridItem.default),
    -- Center
    LayoutNode.leaf' 2 50 40 {} (.gridChild { placement := {}, justifySelf := some .center, alignSelf := some .center }),
    -- End
    LayoutNode.leaf' 3 50 40 {} (.gridChild { placement := {}, justifySelf := some .flexEnd, alignSelf := some .flexEnd })
  ]

  let result := layout tree 350 80
  drawGridResult result 450 360

/-- Demo 7: Complex grid layout (header, sidebar, main, footer) -/
def demoGridComplex : CanvasM Unit := do
  let props := GridContainer.withTemplate
    #[.px 40, .fr 1, .px 30]  -- header, content, footer
    #[.px 80, .fr 1]          -- sidebar, main
    (gap := 10)

  -- Header spans both columns (row 1, cols 1-2)
  let headerPlacement := { GridItem.default with
    placement := {
      row := GridSpan.lines 1 2
      column := GridSpan.lines 1 3  -- span both columns
    }
  }

  -- Sidebar (row 2, col 1)
  let sidebarPlacement := { GridItem.default with
    placement := GridPlacement.atPosition 2 1
  }

  -- Main content (row 2, col 2)
  let mainPlacement := { GridItem.default with
    placement := GridPlacement.atPosition 2 2
  }

  -- Footer spans both columns (row 3, cols 1-2)
  let footerPlacement := { GridItem.default with
    placement := {
      row := GridSpan.lines 3 4
      column := GridSpan.lines 1 3
    }
  }

  let tree := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf' 1 0 0 {} (.gridChild headerPlacement),   -- header
    LayoutNode.leaf' 2 0 0 {} (.gridChild sidebarPlacement),  -- sidebar
    LayoutNode.leaf' 3 0 0 {} (.gridChild mainPlacement),     -- main
    LayoutNode.leaf' 4 0 0 {} (.gridChild footerPlacement)    -- footer
  ]

  let result := layout tree 350 220
  drawGridResult result 50 420

/-- Labels for grid demos -/
def gridLabels : Array (String × Float × Float) := #[
  ("Grid: 3 equal columns (1fr 1fr 1fr)", 50, 38),
  ("Expected: 3 cells of equal width", 50, 125),
  ("Grid: Mixed sizes (80px 1fr 2fr)", 50, 138),
  ("Expected: fixed + 1 part + 2 parts", 50, 225),
  ("Grid: Auto-placement (6 items, 3 cols)", 50, 238),
  ("Expected: 2 rows x 3 cols, items flow left-to-right", 50, 375),
  ("Explicit Placement", 450, 38),
  ("Items placed at specific row/col positions", 450, 165),
  ("Spanning Cells", 450, 188),
  ("First item spans 2 columns", 450, 325),
  ("Alignment within cells", 450, 348),
  ("stretch / center / end", 450, 435),
  ("Complex: header + sidebar + main + footer", 50, 408),
  ("Header and footer span 2 columns", 50, 635)
]

/-- Draw the grid demo shapes (background and layout results). -/
def renderGridShapesM : CanvasM Unit := do
  -- Background
  setFillColor (Color.rgba 0.1 0.1 0.15 1.0)
  fillRectXYWH 0 0 1000 800

  -- Draw section labels as colored bars
  setFillColor ((Color.gray 0.5).withAlpha 0.3)
  fillRectXYWH 40 40 370 100   -- 3-col grid
  fillRectXYWH 40 140 370 100  -- mixed sizes
  fillRectXYWH 40 240 370 150  -- auto-placement
  fillRectXYWH 440 40 370 150  -- explicit placement
  fillRectXYWH 440 200 370 150 -- spanning
  fillRectXYWH 440 360 370 90  -- alignment
  fillRectXYWH 40 410 370 240  -- complex

  -- Run demos
  demoGrid3Col
  demoGridMixed
  demoGridAuto
  demoGridExplicit
  demoGridSpan
  demoGridAlign
  demoGridComplex

/-- Draw grid demo labels in layout-canvas coordinates. -/
def renderGridLabelsM (font : Font) : CanvasM Unit := do
  setFillColor Color.white
  for (text, x, y) in gridLabels do
    fillTextXY text x y font

/-- Draw grid demo labels in screen coordinates, mapping from layout-canvas space. -/
def renderGridLabelsMappedM (font : Font) (offsetX offsetY scale : Float) : CanvasM Unit := do
  setFillColor Color.white
  for (text, x, y) in gridLabels do
    fillTextXY text (offsetX + x * scale) (offsetY + y * scale) font

/-- Draw all grid demos (shapes + labels). -/
def renderGridM (font : Font) : CanvasM Unit := do
  renderGridShapesM
  renderGridLabelsM font

end Demos

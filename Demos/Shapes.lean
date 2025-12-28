/-
  Shapes Demo - Basic shapes, stars, hearts, bezier curves
-/
import Afferent

open Afferent CanvasM Linalg

namespace Demos

/-- Render shapes demo content to canvas using CanvasM -/
def renderShapesM : CanvasM Unit := do
  -- Row 1: Basic rectangles
  setFillColor Color.red
  fillRectXYWH 50 30 120 80
  setFillColor Color.green
  fillRectXYWH 200 30 120 80
  setFillColor Color.blue
  fillRectXYWH 350 30 120 80

  -- Row 1: Circles
  setFillColor Color.yellow
  fillCircle ⟨550, 70⟩ 40
  setFillColor Color.cyan
  fillCircle ⟨650, 70⟩ 40
  setFillColor Color.magenta
  fillCircle ⟨750, 70⟩ 40

  -- Row 1: Rounded rectangle
  setFillColor Color.white
  fillRoundedRect (Rect.mk' 820 30 130 80) 15

  -- Row 2: Stars
  setFillColor Color.yellow
  fillPath (Path.star ⟨100, 200⟩ 50 25 5)
  setFillColor Color.orange
  fillPath (Path.star ⟨220, 200⟩ 45 25 6)
  setFillColor Color.red
  fillPath (Path.star ⟨340, 200⟩ 40 25 8)

  -- Row 2: Regular polygons
  setFillColor Color.green
  fillPath (Path.polygon ⟨480, 200⟩ 45 3)
  setFillColor Color.cyan
  fillPath (Path.polygon ⟨600, 200⟩ 45 5)
  setFillColor Color.blue
  fillPath (Path.polygon ⟨720, 200⟩ 45 6)
  setFillColor Color.purple
  fillPath (Path.polygon ⟨850, 200⟩ 45 8)

  -- Row 3: Hearts and ellipses
  setFillColor Color.red
  fillPath (Path.heart ⟨100, 350⟩ 80)
  setFillColor Color.magenta
  fillPath (Path.heart ⟨230, 350⟩ 60)
  setFillColor Color.orange
  fillEllipse ⟨380, 350⟩ 70 40
  setFillColor Color.green
  fillEllipse ⟨520, 350⟩ 40 60

  -- Row 3: Pie slices
  setFillColor Color.red
  fillPath (Path.pie ⟨680, 350⟩ 60 0 Float.halfPi)
  setFillColor Color.green
  fillPath (Path.pie ⟨680, 350⟩ 60 Float.halfPi Float.pi)
  setFillColor Color.blue
  fillPath (Path.pie ⟨680, 350⟩ 60 Float.pi (Float.pi * 1.5))
  setFillColor Color.yellow
  fillPath (Path.pie ⟨680, 350⟩ 60 (Float.pi * 1.5) Float.twoPi)

  -- Row 3: Semicircle
  setFillColor Color.purple
  fillPath (Path.semicircle ⟨850, 350⟩ 50 0)

  -- Row 4: Bezier curves
  let banner := Path.empty
    |>.moveTo ⟨50, 480⟩
    |>.lineTo ⟨200, 480⟩
    |>.quadraticCurveTo ⟨250, 530⟩ ⟨200, 580⟩
    |>.lineTo ⟨50, 580⟩
    |>.quadraticCurveTo ⟨0, 530⟩ ⟨50, 480⟩
    |>.closePath
  setFillColor Color.cyan
  fillPath banner

  let teardrop := Path.empty
    |>.moveTo ⟨350, 480⟩
    |>.bezierCurveTo ⟨420, 450⟩ ⟨420, 600⟩ ⟨350, 580⟩
    |>.bezierCurveTo ⟨280, 600⟩ ⟨280, 450⟩ ⟨350, 480⟩
    |>.closePath
  setFillColor Color.orange
  fillPath teardrop

  -- Row 4: Arc paths
  setFillColor Color.green
  fillPath (Path.arcPath ⟨550, 530⟩ 50 0 (Float.pi * 1.5) |>.closePath)

  -- Row 4: More rounded rectangles
  setFillColor Color.red
  fillRoundedRect (Rect.mk' 650 470 100 80) 5
  setFillColor Color.blue
  fillRoundedRect (Rect.mk' 780 470 100 80) 30

  -- Row 5: Custom triangle
  setFillColor Color.yellow
  fillPath (Path.triangle ⟨100, 650⟩ ⟨180, 750⟩ ⟨20, 750⟩)

  -- Row 5: Equilateral triangles
  setFillColor Color.green
  fillPath (Path.equilateralTriangle ⟨280, 700⟩ 50)
  setFillColor Color.cyan
  fillPath (Path.equilateralTriangle ⟨380, 700⟩ 40)

  -- Row 5: Speech bubble
  let bubble := Path.empty
    |>.moveTo ⟨500, 650⟩
    |>.lineTo ⟨700, 650⟩
    |>.bezierCurveTo ⟨730, 650⟩ ⟨730, 680⟩ ⟨730, 700⟩
    |>.lineTo ⟨730, 730⟩
    |>.bezierCurveTo ⟨730, 760⟩ ⟨700, 760⟩ ⟨670, 760⟩
    |>.lineTo ⟨570, 760⟩
    |>.lineTo ⟨550, 790⟩
    |>.lineTo ⟨560, 760⟩
    |>.lineTo ⟨530, 760⟩
    |>.bezierCurveTo ⟨500, 760⟩ ⟨470, 760⟩ ⟨470, 730⟩
    |>.lineTo ⟨470, 700⟩
    |>.bezierCurveTo ⟨470, 670⟩ ⟨470, 650⟩ ⟨500, 650⟩
    |>.closePath
  setFillColor Color.white
  fillPath bubble

  -- Row 5: Diamond shape
  let diamond := Path.empty
    |>.moveTo ⟨850, 650⟩
    |>.lineTo ⟨900, 700⟩
    |>.lineTo ⟨850, 760⟩
    |>.lineTo ⟨800, 700⟩
    |>.closePath
  setFillColor Color.cyan
  fillPath diamond

end Demos

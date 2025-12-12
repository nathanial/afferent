/-
  Shapes Demo - Basic shapes, stars, hearts, bezier curves
-/
import Afferent

open Afferent

namespace Demos

/-- Render shapes demo content to canvas using CanvasM -/
def renderShapesM : CanvasM Unit := do
  let pi := 3.14159265358979323846

  -- Row 1: Basic rectangles
  CanvasM.setFillColor Color.red
  CanvasM.fillRectXYWH 50 30 120 80
  CanvasM.setFillColor Color.green
  CanvasM.fillRectXYWH 200 30 120 80
  CanvasM.setFillColor Color.blue
  CanvasM.fillRectXYWH 350 30 120 80

  -- Row 1: Circles
  CanvasM.setFillColor Color.yellow
  CanvasM.fillCircle ⟨550, 70⟩ 40
  CanvasM.setFillColor Color.cyan
  CanvasM.fillCircle ⟨650, 70⟩ 40
  CanvasM.setFillColor Color.magenta
  CanvasM.fillCircle ⟨750, 70⟩ 40

  -- Row 1: Rounded rectangle
  CanvasM.setFillColor Color.white
  CanvasM.fillRoundedRect (Rect.mk' 820 30 130 80) 15

  -- Row 2: Stars
  CanvasM.setFillColor Color.yellow
  CanvasM.fillPath (Path.star ⟨100, 200⟩ 50 25 5)
  CanvasM.setFillColor Color.orange
  CanvasM.fillPath (Path.star ⟨220, 200⟩ 45 25 6)
  CanvasM.setFillColor Color.red
  CanvasM.fillPath (Path.star ⟨340, 200⟩ 40 25 8)

  -- Row 2: Regular polygons
  CanvasM.setFillColor Color.green
  CanvasM.fillPath (Path.polygon ⟨480, 200⟩ 45 3)
  CanvasM.setFillColor Color.cyan
  CanvasM.fillPath (Path.polygon ⟨600, 200⟩ 45 5)
  CanvasM.setFillColor Color.blue
  CanvasM.fillPath (Path.polygon ⟨720, 200⟩ 45 6)
  CanvasM.setFillColor Color.purple
  CanvasM.fillPath (Path.polygon ⟨850, 200⟩ 45 8)

  -- Row 3: Hearts and ellipses
  CanvasM.setFillColor Color.red
  CanvasM.fillPath (Path.heart ⟨100, 350⟩ 80)
  CanvasM.setFillColor Color.magenta
  CanvasM.fillPath (Path.heart ⟨230, 350⟩ 60)
  CanvasM.setFillColor Color.orange
  CanvasM.fillEllipse ⟨380, 350⟩ 70 40
  CanvasM.setFillColor Color.green
  CanvasM.fillEllipse ⟨520, 350⟩ 40 60

  -- Row 3: Pie slices
  CanvasM.setFillColor Color.red
  CanvasM.fillPath (Path.pie ⟨680, 350⟩ 60 0 (pi * 0.5))
  CanvasM.setFillColor Color.green
  CanvasM.fillPath (Path.pie ⟨680, 350⟩ 60 (pi * 0.5) pi)
  CanvasM.setFillColor Color.blue
  CanvasM.fillPath (Path.pie ⟨680, 350⟩ 60 pi (pi * 1.5))
  CanvasM.setFillColor Color.yellow
  CanvasM.fillPath (Path.pie ⟨680, 350⟩ 60 (pi * 1.5) (pi * 2))

  -- Row 3: Semicircle
  CanvasM.setFillColor Color.purple
  CanvasM.fillPath (Path.semicircle ⟨850, 350⟩ 50 0)

  -- Row 4: Bezier curves
  let banner := Path.empty
    |>.moveTo ⟨50, 480⟩
    |>.lineTo ⟨200, 480⟩
    |>.quadraticCurveTo ⟨250, 530⟩ ⟨200, 580⟩
    |>.lineTo ⟨50, 580⟩
    |>.quadraticCurveTo ⟨0, 530⟩ ⟨50, 480⟩
    |>.closePath
  CanvasM.setFillColor Color.cyan
  CanvasM.fillPath banner

  let teardrop := Path.empty
    |>.moveTo ⟨350, 480⟩
    |>.bezierCurveTo ⟨420, 450⟩ ⟨420, 600⟩ ⟨350, 580⟩
    |>.bezierCurveTo ⟨280, 600⟩ ⟨280, 450⟩ ⟨350, 480⟩
    |>.closePath
  CanvasM.setFillColor Color.orange
  CanvasM.fillPath teardrop

  -- Row 4: Arc paths
  CanvasM.setFillColor Color.green
  CanvasM.fillPath (Path.arcPath ⟨550, 530⟩ 50 0 (pi * 1.5) |>.closePath)

  -- Row 4: More rounded rectangles
  CanvasM.setFillColor Color.red
  CanvasM.fillRoundedRect (Rect.mk' 650 470 100 80) 5
  CanvasM.setFillColor Color.blue
  CanvasM.fillRoundedRect (Rect.mk' 780 470 100 80) 30

  -- Row 5: Custom triangle
  CanvasM.setFillColor Color.yellow
  CanvasM.fillPath (Path.triangle ⟨100, 650⟩ ⟨180, 750⟩ ⟨20, 750⟩)

  -- Row 5: Equilateral triangles
  CanvasM.setFillColor Color.green
  CanvasM.fillPath (Path.equilateralTriangle ⟨280, 700⟩ 50)
  CanvasM.setFillColor Color.cyan
  CanvasM.fillPath (Path.equilateralTriangle ⟨380, 700⟩ 40)

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
  CanvasM.setFillColor Color.white
  CanvasM.fillPath bubble

  -- Row 5: Diamond shape
  let diamond := Path.empty
    |>.moveTo ⟨850, 650⟩
    |>.lineTo ⟨900, 700⟩
    |>.lineTo ⟨850, 760⟩
    |>.lineTo ⟨800, 700⟩
    |>.closePath
  CanvasM.setFillColor Color.cyan
  CanvasM.fillPath diamond

end Demos

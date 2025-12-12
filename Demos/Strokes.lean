/-
  Strokes Demo - Line widths, stroked paths, combined fill and stroke
-/
import Afferent

open Afferent

namespace Demos

/-- Render strokes demo content to canvas using CanvasM -/
def renderStrokesM : CanvasM Unit := do
  let pi := 3.14159265358979323846

  -- Row 1: Stroked rectangles with different line widths
  CanvasM.setStrokeColor Color.white
  CanvasM.setLineWidth 1.0
  CanvasM.strokeRectXYWH 50 30 100 70
  CanvasM.setStrokeColor Color.yellow
  CanvasM.setLineWidth 2.0
  CanvasM.strokeRectXYWH 180 30 100 70
  CanvasM.setStrokeColor Color.cyan
  CanvasM.setLineWidth 4.0
  CanvasM.strokeRectXYWH 310 30 100 70
  CanvasM.setStrokeColor Color.magenta
  CanvasM.setLineWidth 8.0
  CanvasM.strokeRectXYWH 440 30 100 70

  -- Stroked circles
  CanvasM.setStrokeColor Color.red
  CanvasM.setLineWidth 2.0
  CanvasM.strokeCircle ⟨620, 65⟩ 35
  CanvasM.setStrokeColor Color.green
  CanvasM.setLineWidth 4.0
  CanvasM.strokeCircle ⟨720, 65⟩ 35
  CanvasM.setStrokeColor Color.blue
  CanvasM.setLineWidth 6.0
  CanvasM.strokeCircle ⟨820, 65⟩ 35

  -- Row 2: Lines with different widths
  CanvasM.setStrokeColor Color.white
  CanvasM.setLineWidth 1.0
  CanvasM.drawLine ⟨50, 140⟩ ⟨200, 140⟩
  CanvasM.setLineWidth 2.0
  CanvasM.drawLine ⟨50, 160⟩ ⟨200, 160⟩
  CanvasM.setLineWidth 4.0
  CanvasM.drawLine ⟨50, 185⟩ ⟨200, 185⟩
  CanvasM.setLineWidth 8.0
  CanvasM.drawLine ⟨50, 215⟩ ⟨200, 215⟩

  -- Diagonal lines
  CanvasM.setStrokeColor Color.yellow
  CanvasM.setLineWidth 2.0
  CanvasM.drawLine ⟨250, 130⟩ ⟨350, 220⟩
  CanvasM.setStrokeColor Color.cyan
  CanvasM.setLineWidth 3.0
  CanvasM.drawLine ⟨280, 130⟩ ⟨380, 220⟩
  CanvasM.setStrokeColor Color.magenta
  CanvasM.setLineWidth 4.0
  CanvasM.drawLine ⟨310, 130⟩ ⟨410, 220⟩

  -- Stroked rounded rectangles
  CanvasM.setStrokeColor Color.orange
  CanvasM.setLineWidth 3.0
  CanvasM.strokeRoundedRect (Rect.mk' 450 130 120 80) 10
  CanvasM.setStrokeColor Color.green
  CanvasM.setLineWidth 4.0
  CanvasM.strokeRoundedRect (Rect.mk' 600 130 120 80) 20
  CanvasM.setStrokeColor Color.purple
  CanvasM.setLineWidth 5.0
  CanvasM.strokeRoundedRect (Rect.mk' 750 130 120 80) 30

  -- Row 3: Stroked ellipses
  CanvasM.setStrokeColor Color.red
  CanvasM.setLineWidth 2.0
  CanvasM.strokeEllipse ⟨100, 300⟩ 60 30
  CanvasM.setStrokeColor Color.green
  CanvasM.setLineWidth 3.0
  CanvasM.strokeEllipse ⟨250, 300⟩ 30 50
  CanvasM.setStrokeColor Color.blue
  CanvasM.setLineWidth 4.0
  CanvasM.strokeEllipse ⟨400, 300⟩ 50 50

  -- Stroked stars
  CanvasM.setStrokeColor Color.yellow
  CanvasM.setLineWidth 2.0
  CanvasM.strokePath (Path.star ⟨550, 300⟩ 50 25 5)
  CanvasM.setStrokeColor Color.cyan
  CanvasM.setLineWidth 3.0
  CanvasM.strokePath (Path.star ⟨680, 300⟩ 45 20 6)
  CanvasM.setStrokeColor Color.magenta
  CanvasM.setLineWidth 4.0
  CanvasM.strokePath (Path.star ⟨810, 300⟩ 40 18 8)

  -- Row 4: Stroked polygons
  CanvasM.setStrokeColor Color.red
  CanvasM.setLineWidth 2.0
  CanvasM.strokePath (Path.polygon ⟨80, 420⟩ 40 3)
  CanvasM.setStrokeColor Color.orange
  CanvasM.strokePath (Path.polygon ⟨170, 420⟩ 40 4)
  CanvasM.setStrokeColor Color.yellow
  CanvasM.strokePath (Path.polygon ⟨260, 420⟩ 40 5)
  CanvasM.setStrokeColor Color.green
  CanvasM.strokePath (Path.polygon ⟨350, 420⟩ 40 6)
  CanvasM.setStrokeColor Color.cyan
  CanvasM.strokePath (Path.polygon ⟨440, 420⟩ 40 8)

  -- Stroked heart
  CanvasM.setStrokeColor Color.red
  CanvasM.setLineWidth 3.0
  CanvasM.strokePath (Path.heart ⟨560, 420⟩ 60)

  -- Row 4: Combined fill and stroke
  CanvasM.setFillColor (Color.rgba 0.2 0.2 0.8 1.0)
  CanvasM.fillCircle ⟨700, 420⟩ 40
  CanvasM.setStrokeColor Color.white
  CanvasM.setLineWidth 3.0
  CanvasM.strokeCircle ⟨700, 420⟩ 40

  CanvasM.setFillColor (Color.rgba 0.8 0.2 0.2 1.0)
  CanvasM.fillRoundedRect (Rect.mk' 770 380 100 80) 15
  CanvasM.setStrokeColor Color.white
  CanvasM.setLineWidth 2.0
  CanvasM.strokeRoundedRect (Rect.mk' 770 380 100 80) 15

  -- Row 5: Custom stroked paths
  let zigzag := Path.empty
    |>.moveTo ⟨50, 520⟩
    |>.lineTo ⟨80, 480⟩
    |>.lineTo ⟨110, 520⟩
    |>.lineTo ⟨140, 480⟩
    |>.lineTo ⟨170, 520⟩
    |>.lineTo ⟨200, 480⟩
    |>.lineTo ⟨230, 520⟩
  CanvasM.setStrokeColor Color.yellow
  CanvasM.setLineWidth 3.0
  CanvasM.strokePath zigzag

  -- Wave using bezier curves
  let wave := Path.empty
    |>.moveTo ⟨280, 500⟩
    |>.bezierCurveTo ⟨320, 460⟩ ⟨360, 540⟩ ⟨400, 500⟩
    |>.bezierCurveTo ⟨440, 460⟩ ⟨480, 540⟩ ⟨520, 500⟩
  CanvasM.setStrokeColor Color.cyan
  CanvasM.setLineWidth 4.0
  CanvasM.strokePath wave

  -- Spiral-like path
  let spiral := Path.empty
    |>.moveTo ⟨620, 500⟩
    |>.quadraticCurveTo ⟨680, 460⟩ ⟨720, 500⟩
    |>.quadraticCurveTo ⟨760, 540⟩ ⟨800, 500⟩
    |>.quadraticCurveTo ⟨840, 460⟩ ⟨860, 520⟩
  CanvasM.setStrokeColor Color.magenta
  CanvasM.setLineWidth 3.0
  CanvasM.strokePath spiral

  -- Row 6: Arc strokes
  CanvasM.setStrokeColor Color.red
  CanvasM.setLineWidth 3.0
  CanvasM.strokePath (Path.arcPath ⟨100, 620⟩ 50 0 pi)
  CanvasM.setStrokeColor Color.green
  CanvasM.strokePath (Path.arcPath ⟨230, 620⟩ 50 0 (pi * 1.5))
  CanvasM.setStrokeColor Color.blue
  CanvasM.setLineWidth 4.0
  CanvasM.strokePath (Path.semicircle ⟨360, 620⟩ 50 0)

  -- Pie slice outlines
  CanvasM.setStrokeColor Color.yellow
  CanvasM.setLineWidth 2.0
  CanvasM.strokePath (Path.pie ⟨500, 620⟩ 50 0 (pi * 0.5))
  CanvasM.setStrokeColor Color.cyan
  CanvasM.strokePath (Path.pie ⟨620, 620⟩ 50 (pi * 0.25) (pi * 1.25))

  -- Custom arrow shape
  let arrow := Path.empty
    |>.moveTo ⟨720, 600⟩
    |>.lineTo ⟨780, 620⟩
    |>.lineTo ⟨720, 640⟩
    |>.moveTo ⟨720, 620⟩
    |>.lineTo ⟨780, 620⟩
  CanvasM.setStrokeColor Color.white
  CanvasM.setLineWidth 3.0
  CanvasM.strokePath arrow

  -- Cross/plus shape
  let cross := Path.empty
    |>.moveTo ⟨830, 590⟩
    |>.lineTo ⟨830, 650⟩
    |>.moveTo ⟨800, 620⟩
    |>.lineTo ⟨860, 620⟩
  CanvasM.setStrokeColor Color.red
  CanvasM.setLineWidth 4.0
  CanvasM.strokePath cross

end Demos

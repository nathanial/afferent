/-
  Strokes Demo - Line widths, stroked paths, combined fill and stroke
-/
import Afferent

open Afferent CanvasM Linalg

namespace Demos

/-- Render strokes demo content to canvas using CanvasM -/
def renderStrokesM : CanvasM Unit := do
  -- Row 1: Stroked rectangles with different line widths
  setStrokeColor Color.white
  setLineWidth 1.0
  strokeRectXYWH 50 30 100 70
  setStrokeColor Color.yellow
  setLineWidth 2.0
  strokeRectXYWH 180 30 100 70
  setStrokeColor Color.cyan
  setLineWidth 4.0
  strokeRectXYWH 310 30 100 70
  setStrokeColor Color.magenta
  setLineWidth 8.0
  strokeRectXYWH 440 30 100 70

  -- Stroked circles
  setStrokeColor Color.red
  setLineWidth 2.0
  strokeCircle ⟨620, 65⟩ 35
  setStrokeColor Color.green
  setLineWidth 4.0
  strokeCircle ⟨720, 65⟩ 35
  setStrokeColor Color.blue
  setLineWidth 6.0
  strokeCircle ⟨820, 65⟩ 35

  -- Row 2: Lines with different widths
  setStrokeColor Color.white
  setLineWidth 1.0
  drawLine ⟨50, 140⟩ ⟨200, 140⟩
  setLineWidth 2.0
  drawLine ⟨50, 160⟩ ⟨200, 160⟩
  setLineWidth 4.0
  drawLine ⟨50, 185⟩ ⟨200, 185⟩
  setLineWidth 8.0
  drawLine ⟨50, 215⟩ ⟨200, 215⟩

  -- Diagonal lines
  setStrokeColor Color.yellow
  setLineWidth 2.0
  drawLine ⟨250, 130⟩ ⟨350, 220⟩
  setStrokeColor Color.cyan
  setLineWidth 3.0
  drawLine ⟨280, 130⟩ ⟨380, 220⟩
  setStrokeColor Color.magenta
  setLineWidth 4.0
  drawLine ⟨310, 130⟩ ⟨410, 220⟩

  -- Stroked rounded rectangles
  setStrokeColor Color.orange
  setLineWidth 3.0
  strokeRoundedRect (Rect.mk' 450 130 120 80) 10
  setStrokeColor Color.green
  setLineWidth 4.0
  strokeRoundedRect (Rect.mk' 600 130 120 80) 20
  setStrokeColor Color.purple
  setLineWidth 5.0
  strokeRoundedRect (Rect.mk' 750 130 120 80) 30

  -- Row 3: Stroked ellipses
  setStrokeColor Color.red
  setLineWidth 2.0
  strokeEllipse ⟨100, 300⟩ 60 30
  setStrokeColor Color.green
  setLineWidth 3.0
  strokeEllipse ⟨250, 300⟩ 30 50
  setStrokeColor Color.blue
  setLineWidth 4.0
  strokeEllipse ⟨400, 300⟩ 50 50

  -- Stroked stars
  setStrokeColor Color.yellow
  setLineWidth 2.0
  strokePath (Path.star ⟨550, 300⟩ 50 25 5)
  setStrokeColor Color.cyan
  setLineWidth 3.0
  strokePath (Path.star ⟨680, 300⟩ 45 20 6)
  setStrokeColor Color.magenta
  setLineWidth 4.0
  strokePath (Path.star ⟨810, 300⟩ 40 18 8)

  -- Row 4: Stroked polygons
  setStrokeColor Color.red
  setLineWidth 2.0
  strokePath (Path.polygon ⟨80, 420⟩ 40 3)
  setStrokeColor Color.orange
  strokePath (Path.polygon ⟨170, 420⟩ 40 4)
  setStrokeColor Color.yellow
  strokePath (Path.polygon ⟨260, 420⟩ 40 5)
  setStrokeColor Color.green
  strokePath (Path.polygon ⟨350, 420⟩ 40 6)
  setStrokeColor Color.cyan
  strokePath (Path.polygon ⟨440, 420⟩ 40 8)

  -- Stroked heart
  setStrokeColor Color.red
  setLineWidth 3.0
  strokePath (Path.heart ⟨560, 420⟩ 60)

  -- Row 4: Combined fill and stroke
  setFillColor (Color.hsva 0.667 0.75 0.8 1.0)  -- blue
  fillCircle ⟨700, 420⟩ 40
  setStrokeColor Color.white
  setLineWidth 3.0
  strokeCircle ⟨700, 420⟩ 40

  setFillColor (Color.hsva 0.0 0.75 0.8 1.0)  -- red
  fillRoundedRect (Rect.mk' 770 380 100 80) 15
  setStrokeColor Color.white
  setLineWidth 2.0
  strokeRoundedRect (Rect.mk' 770 380 100 80) 15

  -- Row 5: Custom stroked paths
  let zigzag := Path.empty
    |>.moveTo ⟨50, 520⟩
    |>.lineTo ⟨80, 480⟩
    |>.lineTo ⟨110, 520⟩
    |>.lineTo ⟨140, 480⟩
    |>.lineTo ⟨170, 520⟩
    |>.lineTo ⟨200, 480⟩
    |>.lineTo ⟨230, 520⟩
  setStrokeColor Color.yellow
  setLineWidth 3.0
  strokePath zigzag

  -- Wave using bezier curves
  let wave := Path.empty
    |>.moveTo ⟨280, 500⟩
    |>.bezierCurveTo ⟨320, 460⟩ ⟨360, 540⟩ ⟨400, 500⟩
    |>.bezierCurveTo ⟨440, 460⟩ ⟨480, 540⟩ ⟨520, 500⟩
  setStrokeColor Color.cyan
  setLineWidth 4.0
  strokePath wave

  -- Spiral-like path
  let spiral := Path.empty
    |>.moveTo ⟨620, 500⟩
    |>.quadraticCurveTo ⟨680, 460⟩ ⟨720, 500⟩
    |>.quadraticCurveTo ⟨760, 540⟩ ⟨800, 500⟩
    |>.quadraticCurveTo ⟨840, 460⟩ ⟨860, 520⟩
  setStrokeColor Color.magenta
  setLineWidth 3.0
  strokePath spiral

  -- Row 6: Arc strokes
  setStrokeColor Color.red
  setLineWidth 3.0
  strokePath (Path.arcPath ⟨100, 620⟩ 50 0 Float.pi)
  setStrokeColor Color.green
  strokePath (Path.arcPath ⟨230, 620⟩ 50 0 (Float.pi * 1.5))
  setStrokeColor Color.blue
  setLineWidth 4.0
  strokePath (Path.semicircle ⟨360, 620⟩ 50 0)

  -- Pie slice outlines
  setStrokeColor Color.yellow
  setLineWidth 2.0
  strokePath (Path.pie ⟨500, 620⟩ 50 0 Float.halfPi)
  setStrokeColor Color.cyan
  strokePath (Path.pie ⟨620, 620⟩ 50 (Float.pi * 0.25) (Float.pi * 1.25))

  -- Custom arrow shape
  let arrow := Path.empty
    |>.moveTo ⟨720, 600⟩
    |>.lineTo ⟨780, 620⟩
    |>.lineTo ⟨720, 640⟩
    |>.moveTo ⟨720, 620⟩
    |>.lineTo ⟨780, 620⟩
  setStrokeColor Color.white
  setLineWidth 3.0
  strokePath arrow

  -- Cross/plus shape
  let cross := Path.empty
    |>.moveTo ⟨830, 590⟩
    |>.lineTo ⟨830, 650⟩
    |>.moveTo ⟨800, 620⟩
    |>.lineTo ⟨860, 620⟩
  setStrokeColor Color.red
  setLineWidth 4.0
  strokePath cross

end Demos

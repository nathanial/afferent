/-
  Transforms Demo - Rotations, scales, translations, nested transforms
-/
import Afferent

open Afferent

namespace Demos

/-- Render transforms demo content to canvas using CanvasM -/
def renderTransformsM : CanvasM Unit := do
  let pi := 3.14159265358979323846

  -- Row 1: Basic shapes without transform (reference)
  CanvasM.setFillColor Color.white
  CanvasM.fillRectXYWH 50 30 60 40
  CanvasM.fillCircle ⟨180, 50⟩ 25

  -- Row 1: Translated shapes
  CanvasM.save
  CanvasM.translate 250 0
  CanvasM.setFillColor Color.red
  CanvasM.fillRectXYWH 50 30 60 40
  CanvasM.fillCircle ⟨180, 50⟩ 25
  CanvasM.restore

  -- Row 1: Scaled shapes (1.5x)
  CanvasM.save
  CanvasM.translate 500 50
  CanvasM.scale 1.5 1.5
  CanvasM.setFillColor Color.green
  CanvasM.fillRectXYWH (-30) (-20) 60 40
  CanvasM.fillCircle ⟨50, 0⟩ 25
  CanvasM.restore

  -- Row 2: Rotated rectangles (rotation fan)
  CanvasM.save
  CanvasM.translate 150 200
  for i in [:8] do
    CanvasM.save
    let angle := i.toFloat * (pi / 4.0)
    CanvasM.rotate angle
    CanvasM.setFillColor (Color.rgba
      (0.5 + 0.5 * Float.cos angle)
      (0.5 + 0.5 * Float.sin angle)
      0.5
      0.8)
    CanvasM.fillRectXYWH 30 (-10) 50 20
    CanvasM.restore
  CanvasM.restore

  -- Row 2: Scaled circles
  CanvasM.save
  CanvasM.translate 400 200
  for i in [:5] do
    CanvasM.save
    let s := 0.5 + i.toFloat * 0.3
    CanvasM.translate (i.toFloat * 50) 0
    CanvasM.scale s s
    CanvasM.setFillColor (Color.rgba (1.0 - i.toFloat * 0.15) (i.toFloat * 0.2) (0.5 + i.toFloat * 0.1) 1.0)
    CanvasM.fillCircle ⟨0, 0⟩ 30
    CanvasM.restore
  CanvasM.restore

  -- Row 3: Combined transforms - rotating star
  CanvasM.save
  CanvasM.translate 150 380
  CanvasM.rotate (pi / 6.0)
  CanvasM.scale 1.2 0.8
  CanvasM.setFillColor Color.yellow
  CanvasM.fillPath (Path.star ⟨0, 0⟩ 60 30 5)
  CanvasM.restore

  -- Row 3: Nested transforms
  CanvasM.save
  CanvasM.translate 350 380
  CanvasM.setFillColor Color.blue
  CanvasM.fillCircle ⟨0, 0⟩ 50

  CanvasM.save
  CanvasM.translate 0 0
  CanvasM.scale 0.6 0.6
  CanvasM.setFillColor Color.cyan
  CanvasM.fillCircle ⟨0, 0⟩ 50

  CanvasM.save
  CanvasM.scale 0.5 0.5
  CanvasM.setFillColor Color.white
  CanvasM.fillCircle ⟨0, 0⟩ 50
  CanvasM.restore
  CanvasM.restore
  CanvasM.restore

  -- Row 3: Global alpha demo
  CanvasM.save
  CanvasM.translate 550 380
  CanvasM.setFillColor Color.red
  CanvasM.fillRectXYWH (-40) (-30) 80 60

  CanvasM.setGlobalAlpha 0.5
  CanvasM.setFillColor Color.blue
  CanvasM.fillRectXYWH (-20) (-10) 80 60

  CanvasM.setGlobalAlpha 0.3
  CanvasM.setFillColor Color.green
  CanvasM.fillRectXYWH 0 10 80 60
  CanvasM.restore

  -- Row 4: Orbiting shapes
  CanvasM.save
  CanvasM.translate 200 520
  for i in [:6] do
    CanvasM.save
    let angle := i.toFloat * (pi / 3.0)
    CanvasM.rotate angle
    CanvasM.translate 60 0
    CanvasM.rotate (-angle)
    CanvasM.setFillColor (Color.rgba
      (if i % 2 == 0 then 1.0 else 0.5)
      (if i % 3 == 0 then 1.0 else 0.3)
      (if i % 2 == 1 then 1.0 else 0.2)
      1.0)
    CanvasM.fillRectXYWH (-15) (-15) 30 30
    CanvasM.restore
  CanvasM.restore

  -- Row 4: Skewed/sheared effect
  CanvasM.save
  CanvasM.translate 450 520
  CanvasM.rotate (pi / 12.0)
  CanvasM.scale 1.5 0.7
  CanvasM.setFillColor Color.magenta
  CanvasM.fillRectXYWH (-40) (-25) 80 50
  CanvasM.restore

  -- Row 4: Hearts with different transforms
  CanvasM.save
  CanvasM.translate 620 520
  CanvasM.setFillColor Color.red
  CanvasM.fillPath (Path.heart ⟨0, 0⟩ 50)

  CanvasM.translate 100 0
  CanvasM.rotate (pi / 8.0)
  CanvasM.scale 0.7 0.7
  CanvasM.setFillColor Color.magenta
  CanvasM.fillPath (Path.heart ⟨0, 0⟩ 50)
  CanvasM.restore

end Demos

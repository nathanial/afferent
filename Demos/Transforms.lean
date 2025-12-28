/-
  Transforms Demo - Rotations, scales, translations, nested transforms
-/
import Afferent

open Afferent CanvasM Linalg

namespace Demos

/-- Render transforms demo content to canvas using CanvasM -/
def renderTransformsM : CanvasM Unit := do
  -- Row 1: Basic shapes without transform (reference)
  setFillColor Color.white
  fillRectXYWH 50 30 60 40
  fillCircle ⟨180, 50⟩ 25

  -- Row 1: Translated shapes
  save
  translate 250 0
  setFillColor Color.red
  fillRectXYWH 50 30 60 40
  fillCircle ⟨180, 50⟩ 25
  restore

  -- Row 1: Scaled shapes (1.5x)
  save
  translate 500 50
  scale 1.5 1.5
  setFillColor Color.green
  fillRectXYWH (-30) (-20) 60 40
  fillCircle ⟨50, 0⟩ 25
  restore

  -- Row 2: Rotated rectangles (rotation fan)
  save
  translate 150 200
  for i in [:8] do
    save
    let angle := i.toFloat * (Float.pi / 4.0)
    rotate angle
    setFillColor (Color.rgba
      (0.5 + 0.5 * Float.cos angle)
      (0.5 + 0.5 * Float.sin angle)
      0.5
      0.8)
    fillRectXYWH 30 (-10) 50 20
    restore
  restore

  -- Row 2: Scaled circles
  save
  translate 400 200
  for i in [:5] do
    save
    let s := 0.5 + i.toFloat * 0.3
    translate (i.toFloat * 50) 0
    scale s s
    setFillColor (Color.rgba (1.0 - i.toFloat * 0.15) (i.toFloat * 0.2) (0.5 + i.toFloat * 0.1) 1.0)
    fillCircle ⟨0, 0⟩ 30
    restore
  restore

  -- Row 3: Combined transforms - rotating star
  save
  translate 150 380
  rotate (Float.pi / 6.0)
  scale 1.2 0.8
  setFillColor Color.yellow
  fillPath (Path.star ⟨0, 0⟩ 60 30 5)
  restore

  -- Row 3: Nested transforms
  save
  translate 350 380
  setFillColor Color.blue
  fillCircle ⟨0, 0⟩ 50

  save
  translate 0 0
  scale 0.6 0.6
  setFillColor Color.cyan
  fillCircle ⟨0, 0⟩ 50

  save
  scale 0.5 0.5
  setFillColor Color.white
  fillCircle ⟨0, 0⟩ 50
  restore
  restore
  restore

  -- Row 3: Global alpha demo
  save
  translate 550 380
  setFillColor Color.red
  fillRectXYWH (-40) (-30) 80 60

  setGlobalAlpha 0.5
  setFillColor Color.blue
  fillRectXYWH (-20) (-10) 80 60

  setGlobalAlpha 0.3
  setFillColor Color.green
  fillRectXYWH 0 10 80 60
  restore

  -- Row 4: Orbiting shapes
  save
  translate 200 520
  for i in [:6] do
    save
    let angle := i.toFloat * (Float.pi / 3.0)
    rotate angle
    translate 60 0
    rotate (-angle)
    setFillColor (Color.rgba
      (if i % 2 == 0 then 1.0 else 0.5)
      (if i % 3 == 0 then 1.0 else 0.3)
      (if i % 2 == 1 then 1.0 else 0.2)
      1.0)
    fillRectXYWH (-15) (-15) 30 30
    restore
  restore

  -- Row 4: Skewed/sheared effect
  save
  translate 450 520
  rotate (Float.pi / 12.0)
  scale 1.5 0.7
  setFillColor Color.magenta
  fillRectXYWH (-40) (-25) 80 50
  restore

  -- Row 4: Hearts with different transforms
  save
  translate 620 520
  setFillColor Color.red
  fillPath (Path.heart ⟨0, 0⟩ 50)

  translate 100 0
  rotate (Float.pi / 8.0)
  scale 0.7 0.7
  setFillColor Color.magenta
  fillPath (Path.heart ⟨0, 0⟩ 50)
  restore

end Demos

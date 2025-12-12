/-
  Afferent - A Lean 4 2D vector graphics library
  Main executable - demonstrates collimator optics and unified visual demo
-/
import Afferent
import Collimator.Prelude

open Afferent
open Collimator
open scoped Collimator.Operators

-- Demo: Using collimator lenses for data access
structure Person where
  name : String
  age : Nat
deriving Repr

def nameLens : Lens' Person String :=
  lens' (fun p => p.name) (fun p n => { p with name := n })

def ageLens : Lens' Person Nat :=
  lens' (fun p => p.age) (fun p a => { p with age := a })

def collimatorDemo : IO Unit := do
  IO.println "Collimator Optics Demo"
  IO.println "----------------------"

  let alice : Person := { name := "Alice", age := 30 }

  -- View through a lens
  IO.println s!"Name: {alice ^. nameLens}"
  IO.println s!"Age: {alice ^. ageLens}"

  -- Modify through a lens
  let older := over' ageLens (· + 1) alice
  IO.println s!"After birthday: {older ^. ageLens}"

  -- Set through a lens
  let renamed := set' nameLens "Alicia" alice
  IO.println s!"Renamed: {renamed ^. nameLens}"

  IO.println ""

/-! ## Render Functions for Each Demo -/

/-- Render shapes demo content to canvas -/
def renderShapes (c : Canvas) : IO Canvas := do
  let pi := 3.14159265358979323846

  -- Row 1: Basic rectangles
  let c := c.setFillColor Color.red
  let c ← c.fillRectXYWH 50 30 120 80
  let c := c.setFillColor Color.green
  let c ← c.fillRectXYWH 200 30 120 80
  let c := c.setFillColor Color.blue
  let c ← c.fillRectXYWH 350 30 120 80

  -- Row 1: Circles
  let c := c.setFillColor Color.yellow
  let c ← c.fillCircle ⟨550, 70⟩ 40
  let c := c.setFillColor Color.cyan
  let c ← c.fillCircle ⟨650, 70⟩ 40
  let c := c.setFillColor Color.magenta
  let c ← c.fillCircle ⟨750, 70⟩ 40

  -- Row 1: Rounded rectangle
  let c := c.setFillColor Color.white
  let c ← c.fillRoundedRect (Rect.mk' 820 30 130 80) 15

  -- Row 2: Stars
  let c := c.setFillColor Color.yellow
  let c ← c.fillPath (Path.star ⟨100, 200⟩ 50 25 5)
  let c := c.setFillColor Color.orange
  let c ← c.fillPath (Path.star ⟨220, 200⟩ 45 25 6)
  let c := c.setFillColor Color.red
  let c ← c.fillPath (Path.star ⟨340, 200⟩ 40 25 8)

  -- Row 2: Regular polygons
  let c := c.setFillColor Color.green
  let c ← c.fillPath (Path.polygon ⟨480, 200⟩ 45 3)
  let c := c.setFillColor Color.cyan
  let c ← c.fillPath (Path.polygon ⟨600, 200⟩ 45 5)
  let c := c.setFillColor Color.blue
  let c ← c.fillPath (Path.polygon ⟨720, 200⟩ 45 6)
  let c := c.setFillColor Color.purple
  let c ← c.fillPath (Path.polygon ⟨850, 200⟩ 45 8)

  -- Row 3: Hearts and ellipses
  let c := c.setFillColor Color.red
  let c ← c.fillPath (Path.heart ⟨100, 350⟩ 80)
  let c := c.setFillColor Color.magenta
  let c ← c.fillPath (Path.heart ⟨230, 350⟩ 60)
  let c := c.setFillColor Color.orange
  let c ← c.fillEllipse ⟨380, 350⟩ 70 40
  let c := c.setFillColor Color.green
  let c ← c.fillEllipse ⟨520, 350⟩ 40 60

  -- Row 3: Pie slices
  let c := c.setFillColor Color.red
  let c ← c.fillPath (Path.pie ⟨680, 350⟩ 60 0 (pi * 0.5))
  let c := c.setFillColor Color.green
  let c ← c.fillPath (Path.pie ⟨680, 350⟩ 60 (pi * 0.5) pi)
  let c := c.setFillColor Color.blue
  let c ← c.fillPath (Path.pie ⟨680, 350⟩ 60 pi (pi * 1.5))
  let c := c.setFillColor Color.yellow
  let c ← c.fillPath (Path.pie ⟨680, 350⟩ 60 (pi * 1.5) (pi * 2))

  -- Row 3: Semicircle
  let c := c.setFillColor Color.purple
  let c ← c.fillPath (Path.semicircle ⟨850, 350⟩ 50 0)

  -- Row 4: Bezier curves
  let banner := Path.empty
    |>.moveTo ⟨50, 480⟩
    |>.lineTo ⟨200, 480⟩
    |>.quadraticCurveTo ⟨250, 530⟩ ⟨200, 580⟩
    |>.lineTo ⟨50, 580⟩
    |>.quadraticCurveTo ⟨0, 530⟩ ⟨50, 480⟩
    |>.closePath
  let c := c.setFillColor Color.cyan
  let c ← c.fillPath banner

  let teardrop := Path.empty
    |>.moveTo ⟨350, 480⟩
    |>.bezierCurveTo ⟨420, 450⟩ ⟨420, 600⟩ ⟨350, 580⟩
    |>.bezierCurveTo ⟨280, 600⟩ ⟨280, 450⟩ ⟨350, 480⟩
    |>.closePath
  let c := c.setFillColor Color.orange
  let c ← c.fillPath teardrop

  -- Row 4: Arc paths
  let c := c.setFillColor Color.green
  let c ← c.fillPath (Path.arcPath ⟨550, 530⟩ 50 0 (pi * 1.5) |>.closePath)

  -- Row 4: More rounded rectangles
  let c := c.setFillColor Color.red
  let c ← c.fillRoundedRect (Rect.mk' 650 470 100 80) 5
  let c := c.setFillColor Color.blue
  let c ← c.fillRoundedRect (Rect.mk' 780 470 100 80) 30

  -- Row 5: Custom triangle
  let c := c.setFillColor Color.yellow
  let c ← c.fillPath (Path.triangle ⟨100, 650⟩ ⟨180, 750⟩ ⟨20, 750⟩)

  -- Row 5: Equilateral triangles
  let c := c.setFillColor Color.green
  let c ← c.fillPath (Path.equilateralTriangle ⟨280, 700⟩ 50)
  let c := c.setFillColor Color.cyan
  let c ← c.fillPath (Path.equilateralTriangle ⟨380, 700⟩ 40)

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
  let c := c.setFillColor Color.white
  let c ← c.fillPath bubble

  -- Row 5: Diamond shape
  let diamond := Path.empty
    |>.moveTo ⟨850, 650⟩
    |>.lineTo ⟨900, 700⟩
    |>.lineTo ⟨850, 760⟩
    |>.lineTo ⟨800, 700⟩
    |>.closePath
  let c := c.setFillColor Color.cyan
  let c ← c.fillPath diamond

  pure c

/-- Render transforms demo content to canvas -/
def renderTransforms (c : Canvas) : IO Canvas := do
  let pi := 3.14159265358979323846

  -- Row 1: Basic shapes without transform (reference)
  let c := c.setFillColor Color.white
  let c ← c.fillRectXYWH 50 30 60 40
  let c ← c.fillCircle ⟨180, 50⟩ 25

  -- Row 1: Translated shapes
  let c := c.save
  let c := c.translate 250 0
  let c := c.setFillColor Color.red
  let c ← c.fillRectXYWH 50 30 60 40
  let c ← c.fillCircle ⟨180, 50⟩ 25
  let c := c.restore

  -- Row 1: Scaled shapes (1.5x)
  let c := c.save
  let c := c.translate 500 50
  let c := c.scale 1.5 1.5
  let c := c.setFillColor Color.green
  let c ← c.fillRectXYWH (-30) (-20) 60 40
  let c ← c.fillCircle ⟨50, 0⟩ 25
  let c := c.restore

  -- Row 2: Rotated rectangles (rotation fan)
  let c := c.save
  let c := c.translate 150 200
  let mut canvas := c
  for i in [:8] do
    let c' := canvas.save
    let angle := i.toFloat * (pi / 4.0)
    let c' := c'.rotate angle
    let c' := c'.setFillColor (Color.rgba
      (0.5 + 0.5 * Float.cos angle)
      (0.5 + 0.5 * Float.sin angle)
      0.5
      0.8)
    let c' ← c'.fillRectXYWH 30 (-10) 50 20
    canvas := c'.restore
  let c := canvas.restore

  -- Row 2: Scaled circles
  let c := c.save
  let c := c.translate 400 200
  let mut cv := c
  for i in [:5] do
    let c' := cv.save
    let s := 0.5 + i.toFloat * 0.3
    let c' := c'.translate (i.toFloat * 50) 0
    let c' := c'.scale s s
    let c' := c'.setFillColor (Color.rgba (1.0 - i.toFloat * 0.15) (i.toFloat * 0.2) (0.5 + i.toFloat * 0.1) 1.0)
    let c' ← c'.fillCircle ⟨0, 0⟩ 30
    cv := c'.restore
  let c := cv.restore

  -- Row 3: Combined transforms - rotating star
  let c := c.save
  let c := c.translate 150 380
  let c := c.rotate (pi / 6.0)
  let c := c.scale 1.2 0.8
  let c := c.setFillColor Color.yellow
  let c ← c.fillPath (Path.star ⟨0, 0⟩ 60 30 5)
  let c := c.restore

  -- Row 3: Nested transforms
  let c := c.save
  let c := c.translate 350 380
  let c := c.setFillColor Color.blue
  let c ← c.fillCircle ⟨0, 0⟩ 50

  let c := c.save
  let c := c.translate 0 0
  let c := c.scale 0.6 0.6
  let c := c.setFillColor Color.cyan
  let c ← c.fillCircle ⟨0, 0⟩ 50

  let c := c.save
  let c := c.scale 0.5 0.5
  let c := c.setFillColor Color.white
  let c ← c.fillCircle ⟨0, 0⟩ 50
  let c := c.restore
  let c := c.restore
  let c := c.restore

  -- Row 3: Global alpha demo
  let c := c.save
  let c := c.translate 550 380
  let c := c.setFillColor Color.red
  let c ← c.fillRectXYWH (-40) (-30) 80 60

  let c := c.setGlobalAlpha 0.5
  let c := c.setFillColor Color.blue
  let c ← c.fillRectXYWH (-20) (-10) 80 60

  let c := c.setGlobalAlpha 0.3
  let c := c.setFillColor Color.green
  let c ← c.fillRectXYWH 0 10 80 60
  let c := c.restore

  -- Row 4: Orbiting shapes
  let c := c.save
  let c := c.translate 200 520
  let mut cv2 := c
  for i in [:6] do
    let c' := cv2.save
    let angle := i.toFloat * (pi / 3.0)
    let c' := c'.rotate angle
    let c' := c'.translate 60 0
    let c' := c'.rotate (-angle)
    let c' := c'.setFillColor (Color.rgba
      (if i % 2 == 0 then 1.0 else 0.5)
      (if i % 3 == 0 then 1.0 else 0.3)
      (if i % 2 == 1 then 1.0 else 0.2)
      1.0)
    let c' ← c'.fillRectXYWH (-15) (-15) 30 30
    cv2 := c'.restore
  let c := cv2.restore

  -- Row 4: Skewed/sheared effect
  let c := c.save
  let c := c.translate 450 520
  let c := c.rotate (pi / 12.0)
  let c := c.scale 1.5 0.7
  let c := c.setFillColor Color.magenta
  let c ← c.fillRectXYWH (-40) (-25) 80 50
  let c := c.restore

  -- Row 4: Hearts with different transforms
  let c := c.save
  let c := c.translate 620 520
  let c := c.setFillColor Color.red
  let c ← c.fillPath (Path.heart ⟨0, 0⟩ 50)

  let c := c.translate 100 0
  let c := c.rotate (pi / 8.0)
  let c := c.scale 0.7 0.7
  let c := c.setFillColor Color.magenta
  let c ← c.fillPath (Path.heart ⟨0, 0⟩ 50)
  let c := c.restore

  pure c

/-- Render strokes demo content to canvas -/
def renderStrokes (c : Canvas) : IO Canvas := do
  let pi := 3.14159265358979323846

  -- Row 1: Stroked rectangles with different line widths
  let c := c.setStrokeColor Color.white
  let c := c.setLineWidth 1.0
  let c ← c.strokeRectXYWH 50 30 100 70
  let c := c.setStrokeColor Color.yellow
  let c := c.setLineWidth 2.0
  let c ← c.strokeRectXYWH 180 30 100 70
  let c := c.setStrokeColor Color.cyan
  let c := c.setLineWidth 4.0
  let c ← c.strokeRectXYWH 310 30 100 70
  let c := c.setStrokeColor Color.magenta
  let c := c.setLineWidth 8.0
  let c ← c.strokeRectXYWH 440 30 100 70

  -- Stroked circles
  let c := c.setStrokeColor Color.red
  let c := c.setLineWidth 2.0
  let c ← c.strokeCircle ⟨620, 65⟩ 35
  let c := c.setStrokeColor Color.green
  let c := c.setLineWidth 4.0
  let c ← c.strokeCircle ⟨720, 65⟩ 35
  let c := c.setStrokeColor Color.blue
  let c := c.setLineWidth 6.0
  let c ← c.strokeCircle ⟨820, 65⟩ 35

  -- Row 2: Lines with different widths
  let c := c.setStrokeColor Color.white
  let c := c.setLineWidth 1.0
  let c ← c.drawLine ⟨50, 140⟩ ⟨200, 140⟩
  let c := c.setLineWidth 2.0
  let c ← c.drawLine ⟨50, 160⟩ ⟨200, 160⟩
  let c := c.setLineWidth 4.0
  let c ← c.drawLine ⟨50, 185⟩ ⟨200, 185⟩
  let c := c.setLineWidth 8.0
  let c ← c.drawLine ⟨50, 215⟩ ⟨200, 215⟩

  -- Diagonal lines
  let c := c.setStrokeColor Color.yellow
  let c := c.setLineWidth 2.0
  let c ← c.drawLine ⟨250, 130⟩ ⟨350, 220⟩
  let c := c.setStrokeColor Color.cyan
  let c := c.setLineWidth 3.0
  let c ← c.drawLine ⟨280, 130⟩ ⟨380, 220⟩
  let c := c.setStrokeColor Color.magenta
  let c := c.setLineWidth 4.0
  let c ← c.drawLine ⟨310, 130⟩ ⟨410, 220⟩

  -- Stroked rounded rectangles
  let c := c.setStrokeColor Color.orange
  let c := c.setLineWidth 3.0
  let c ← c.strokeRoundedRect (Rect.mk' 450 130 120 80) 10
  let c := c.setStrokeColor Color.green
  let c := c.setLineWidth 4.0
  let c ← c.strokeRoundedRect (Rect.mk' 600 130 120 80) 20
  let c := c.setStrokeColor Color.purple
  let c := c.setLineWidth 5.0
  let c ← c.strokeRoundedRect (Rect.mk' 750 130 120 80) 30

  -- Row 3: Stroked ellipses
  let c := c.setStrokeColor Color.red
  let c := c.setLineWidth 2.0
  let c ← c.strokeEllipse ⟨100, 300⟩ 60 30
  let c := c.setStrokeColor Color.green
  let c := c.setLineWidth 3.0
  let c ← c.strokeEllipse ⟨250, 300⟩ 30 50
  let c := c.setStrokeColor Color.blue
  let c := c.setLineWidth 4.0
  let c ← c.strokeEllipse ⟨400, 300⟩ 50 50

  -- Stroked stars
  let c := c.setStrokeColor Color.yellow
  let c := c.setLineWidth 2.0
  let c ← c.strokePath (Path.star ⟨550, 300⟩ 50 25 5)
  let c := c.setStrokeColor Color.cyan
  let c := c.setLineWidth 3.0
  let c ← c.strokePath (Path.star ⟨680, 300⟩ 45 20 6)
  let c := c.setStrokeColor Color.magenta
  let c := c.setLineWidth 4.0
  let c ← c.strokePath (Path.star ⟨810, 300⟩ 40 18 8)

  -- Row 4: Stroked polygons
  let c := c.setStrokeColor Color.red
  let c := c.setLineWidth 2.0
  let c ← c.strokePath (Path.polygon ⟨80, 420⟩ 40 3)
  let c := c.setStrokeColor Color.orange
  let c ← c.strokePath (Path.polygon ⟨170, 420⟩ 40 4)
  let c := c.setStrokeColor Color.yellow
  let c ← c.strokePath (Path.polygon ⟨260, 420⟩ 40 5)
  let c := c.setStrokeColor Color.green
  let c ← c.strokePath (Path.polygon ⟨350, 420⟩ 40 6)
  let c := c.setStrokeColor Color.cyan
  let c ← c.strokePath (Path.polygon ⟨440, 420⟩ 40 8)

  -- Stroked heart
  let c := c.setStrokeColor Color.red
  let c := c.setLineWidth 3.0
  let c ← c.strokePath (Path.heart ⟨560, 420⟩ 60)

  -- Row 4: Combined fill and stroke
  let c := c.setFillColor (Color.rgba 0.2 0.2 0.8 1.0)
  let c ← c.fillCircle ⟨700, 420⟩ 40
  let c := c.setStrokeColor Color.white
  let c := c.setLineWidth 3.0
  let c ← c.strokeCircle ⟨700, 420⟩ 40

  let c := c.setFillColor (Color.rgba 0.8 0.2 0.2 1.0)
  let c ← c.fillRoundedRect (Rect.mk' 770 380 100 80) 15
  let c := c.setStrokeColor Color.white
  let c := c.setLineWidth 2.0
  let c ← c.strokeRoundedRect (Rect.mk' 770 380 100 80) 15

  -- Row 5: Custom stroked paths
  let zigzag := Path.empty
    |>.moveTo ⟨50, 520⟩
    |>.lineTo ⟨80, 480⟩
    |>.lineTo ⟨110, 520⟩
    |>.lineTo ⟨140, 480⟩
    |>.lineTo ⟨170, 520⟩
    |>.lineTo ⟨200, 480⟩
    |>.lineTo ⟨230, 520⟩
  let c := c.setStrokeColor Color.yellow
  let c := c.setLineWidth 3.0
  let c ← c.strokePath zigzag

  -- Wave using bezier curves
  let wave := Path.empty
    |>.moveTo ⟨280, 500⟩
    |>.bezierCurveTo ⟨320, 460⟩ ⟨360, 540⟩ ⟨400, 500⟩
    |>.bezierCurveTo ⟨440, 460⟩ ⟨480, 540⟩ ⟨520, 500⟩
  let c := c.setStrokeColor Color.cyan
  let c := c.setLineWidth 4.0
  let c ← c.strokePath wave

  -- Spiral-like path
  let spiral := Path.empty
    |>.moveTo ⟨620, 500⟩
    |>.quadraticCurveTo ⟨680, 460⟩ ⟨720, 500⟩
    |>.quadraticCurveTo ⟨760, 540⟩ ⟨800, 500⟩
    |>.quadraticCurveTo ⟨840, 460⟩ ⟨860, 520⟩
  let c := c.setStrokeColor Color.magenta
  let c := c.setLineWidth 3.0
  let c ← c.strokePath spiral

  -- Row 6: Arc strokes
  let c := c.setStrokeColor Color.red
  let c := c.setLineWidth 3.0
  let c ← c.strokePath (Path.arcPath ⟨100, 620⟩ 50 0 pi)
  let c := c.setStrokeColor Color.green
  let c ← c.strokePath (Path.arcPath ⟨230, 620⟩ 50 0 (pi * 1.5))
  let c := c.setStrokeColor Color.blue
  let c := c.setLineWidth 4.0
  let c ← c.strokePath (Path.semicircle ⟨360, 620⟩ 50 0)

  -- Pie slice outlines
  let c := c.setStrokeColor Color.yellow
  let c := c.setLineWidth 2.0
  let c ← c.strokePath (Path.pie ⟨500, 620⟩ 50 0 (pi * 0.5))
  let c := c.setStrokeColor Color.cyan
  let c ← c.strokePath (Path.pie ⟨620, 620⟩ 50 (pi * 0.25) (pi * 1.25))

  -- Custom arrow shape
  let arrow := Path.empty
    |>.moveTo ⟨720, 600⟩
    |>.lineTo ⟨780, 620⟩
    |>.lineTo ⟨720, 640⟩
    |>.moveTo ⟨720, 620⟩
    |>.lineTo ⟨780, 620⟩
  let c := c.setStrokeColor Color.white
  let c := c.setLineWidth 3.0
  let c ← c.strokePath arrow

  -- Cross/plus shape
  let cross := Path.empty
    |>.moveTo ⟨830, 590⟩
    |>.lineTo ⟨830, 650⟩
    |>.moveTo ⟨800, 620⟩
    |>.lineTo ⟨860, 620⟩
  let c := c.setStrokeColor Color.red
  let c := c.setLineWidth 4.0
  let c ← c.strokePath cross

  pure c

/-- Render gradients demo content to canvas -/
def renderGradients (c : Canvas) : IO Canvas := do
  -- Row 1: Linear gradients - horizontal
  let redYellow : Array GradientStop := #[
    { position := 0.0, color := Color.red },
    { position := 1.0, color := Color.yellow }
  ]
  let c := c.setFillLinearGradient ⟨50, 70⟩ ⟨200, 70⟩ redYellow
  let c ← c.fillRect (Rect.mk' 50 30 150 80)

  let blueCyan : Array GradientStop := #[
    { position := 0.0, color := Color.blue },
    { position := 1.0, color := Color.cyan }
  ]
  let c := c.setFillLinearGradient ⟨230, 70⟩ ⟨380, 70⟩ blueCyan
  let c ← c.fillRect (Rect.mk' 230 30 150 80)

  let greenWhite : Array GradientStop := #[
    { position := 0.0, color := Color.green },
    { position := 1.0, color := Color.white }
  ]
  let c := c.setFillLinearGradient ⟨410, 70⟩ ⟨560, 70⟩ greenWhite
  let c ← c.fillRect (Rect.mk' 410 30 150 80)

  -- Row 1: Vertical gradient
  let purpleOrange : Array GradientStop := #[
    { position := 0.0, color := Color.purple },
    { position := 1.0, color := Color.orange }
  ]
  let c := c.setFillLinearGradient ⟨640, 30⟩ ⟨640, 110⟩ purpleOrange
  let c ← c.fillRect (Rect.mk' 590 30 100 80)

  -- Diagonal gradient
  let magentaCyan : Array GradientStop := #[
    { position := 0.0, color := Color.magenta },
    { position := 1.0, color := Color.cyan }
  ]
  let c := c.setFillLinearGradient ⟨720, 30⟩ ⟨870, 110⟩ magentaCyan
  let c ← c.fillRect (Rect.mk' 720 30 150 80)

  -- Row 2: Multi-stop gradients (rainbow)
  let rainbow : Array GradientStop := #[
    { position := 0.0, color := Color.red },
    { position := 0.17, color := Color.orange },
    { position := 0.33, color := Color.yellow },
    { position := 0.5, color := Color.green },
    { position := 0.67, color := Color.blue },
    { position := 0.83, color := Color.purple },
    { position := 1.0, color := Color.magenta }
  ]
  let c := c.setFillLinearGradient ⟨50, 180⟩ ⟨450, 180⟩ rainbow
  let c ← c.fillRect (Rect.mk' 50 140 400 80)

  -- Sunset gradient
  let sunset : Array GradientStop := #[
    { position := 0.0, color := Color.rgba 0.1 0.1 0.3 1.0 },
    { position := 0.3, color := Color.rgba 0.5 0.2 0.5 1.0 },
    { position := 0.5, color := Color.rgba 0.9 0.3 0.2 1.0 },
    { position := 0.7, color := Color.rgba 1.0 0.6 0.2 1.0 },
    { position := 1.0, color := Color.rgba 1.0 0.9 0.4 1.0 }
  ]
  let c := c.setFillLinearGradient ⟨570, 140⟩ ⟨570, 220⟩ sunset
  let c ← c.fillRect (Rect.mk' 480 140 180 80)

  -- Grayscale
  let grayscale : Array GradientStop := #[
    { position := 0.0, color := Color.black },
    { position := 1.0, color := Color.white }
  ]
  let c := c.setFillLinearGradient ⟨690, 180⟩ ⟨870, 180⟩ grayscale
  let c ← c.fillRect (Rect.mk' 690 140 180 80)

  -- Row 3: Radial gradients
  let whiteBlue : Array GradientStop := #[
    { position := 0.0, color := Color.white },
    { position := 1.0, color := Color.blue }
  ]
  let c := c.setFillRadialGradient ⟨120, 320⟩ 70 whiteBlue
  let c ← c.fillCircle ⟨120, 320⟩ 70

  let sunGlow : Array GradientStop := #[
    { position := 0.0, color := Color.yellow },
    { position := 0.5, color := Color.orange },
    { position := 1.0, color := Color.red }
  ]
  let c := c.setFillRadialGradient ⟨280, 320⟩ 70 sunGlow
  let c ← c.fillCircle ⟨280, 320⟩ 70

  let spotlight : Array GradientStop := #[
    { position := 0.0, color := Color.white },
    { position := 0.7, color := Color.rgba 1.0 1.0 1.0 0.3 },
    { position := 1.0, color := Color.rgba 1.0 1.0 1.0 0.0 }
  ]
  let c := c.setFillRadialGradient ⟨440, 320⟩ 70 spotlight
  let c ← c.fillCircle ⟨440, 320⟩ 70

  let greenGlow : Array GradientStop := #[
    { position := 0.0, color := Color.rgba 0.5 1.0 0.5 1.0 },
    { position := 0.5, color := Color.green },
    { position := 1.0, color := Color.rgba 0.0 0.3 0.0 1.0 }
  ]
  let c := c.setFillRadialGradient ⟨600, 320⟩ 70 greenGlow
  let c ← c.fillCircle ⟨600, 320⟩ 70

  let cyanMagenta : Array GradientStop := #[
    { position := 0.0, color := Color.cyan },
    { position := 1.0, color := Color.magenta }
  ]
  let c := c.setFillRadialGradient ⟨760, 320⟩ 70 cyanMagenta
  let c ← c.fillCircle ⟨760, 320⟩ 70

  -- Row 4: Gradients on different shapes
  let c := c.setFillLinearGradient ⟨50, 420⟩ ⟨200, 520⟩ #[
    { position := 0.0, color := Color.red },
    { position := 1.0, color := Color.blue }
  ]
  let c ← c.fillRoundedRect (Rect.mk' 50 420 150 100) 20

  let c := c.setFillRadialGradient ⟨330, 470⟩ 80 #[
    { position := 0.0, color := Color.yellow },
    { position := 1.0, color := Color.purple }
  ]
  let c ← c.fillEllipse ⟨330, 470⟩ 80 50

  let c := c.setFillLinearGradient ⟨460, 410⟩ ⟨580, 530⟩ #[
    { position := 0.0, color := Color.yellow },
    { position := 0.5, color := Color.orange },
    { position := 1.0, color := Color.red }
  ]
  let c ← c.fillPath (Path.star ⟨520, 470⟩ 60 30 5)

  let c := c.setFillRadialGradient ⟨700, 450⟩ 80 #[
    { position := 0.0, color := Color.rgba 1.0 0.5 0.5 1.0 },
    { position := 0.5, color := Color.red },
    { position := 1.0, color := Color.rgba 0.5 0.0 0.0 1.0 }
  ]
  let c ← c.fillPath (Path.heart ⟨700, 470⟩ 70)

  -- Row 5: More gradient variations
  let stripes : Array GradientStop := #[
    { position := 0.0, color := Color.red },
    { position := 0.33, color := Color.red },
    { position := 0.34, color := Color.white },
    { position := 0.66, color := Color.white },
    { position := 0.67, color := Color.blue },
    { position := 1.0, color := Color.blue }
  ]
  let c := c.setFillLinearGradient ⟨50, 610⟩ ⟨200, 610⟩ stripes
  let c ← c.fillRect (Rect.mk' 50 560 150 100)

  let chrome : Array GradientStop := #[
    { position := 0.0, color := Color.rgba 0.3 0.3 0.3 1.0 },
    { position := 0.2, color := Color.rgba 0.9 0.9 0.9 1.0 },
    { position := 0.4, color := Color.rgba 0.5 0.5 0.5 1.0 },
    { position := 0.6, color := Color.rgba 0.8 0.8 0.8 1.0 },
    { position := 0.8, color := Color.rgba 0.4 0.4 0.4 1.0 },
    { position := 1.0, color := Color.rgba 0.6 0.6 0.6 1.0 }
  ]
  let c := c.setFillLinearGradient ⟨230, 560⟩ ⟨230, 660⟩ chrome
  let c ← c.fillRect (Rect.mk' 230 560 150 100)

  let gold : Array GradientStop := #[
    { position := 0.0, color := Color.rgba 0.6 0.4 0.1 1.0 },
    { position := 0.3, color := Color.rgba 1.0 0.85 0.4 1.0 },
    { position := 0.5, color := Color.rgba 0.8 0.6 0.2 1.0 },
    { position := 0.7, color := Color.rgba 1.0 0.9 0.5 1.0 },
    { position := 1.0, color := Color.rgba 0.5 0.35 0.1 1.0 }
  ]
  let c := c.setFillLinearGradient ⟨410, 560⟩ ⟨410, 660⟩ gold
  let c ← c.fillRect (Rect.mk' 410 560 150 100)

  let c := c.setFillRadialGradient ⟨655, 610⟩ 100 #[
    { position := 0.0, color := Color.rgba 0.0 1.0 1.0 1.0 },
    { position := 0.4, color := Color.rgba 0.0 0.5 1.0 0.8 },
    { position := 1.0, color := Color.rgba 0.0 0.0 0.3 1.0 }
  ]
  let c ← c.fillRect (Rect.mk' 590 560 130 100)

  let purplePink : Array GradientStop := #[
    { position := 0.0, color := Color.rgba 0.4 0.0 0.6 1.0 },
    { position := 1.0, color := Color.rgba 1.0 0.4 0.6 1.0 }
  ]
  let c := c.setFillLinearGradient ⟨750, 660⟩ ⟨870, 560⟩ purplePink
  let c ← c.fillRect (Rect.mk' 750 560 120 100)

  pure c

/-- Font bundle for text demo -/
structure Fonts where
  small : Font
  medium : Font
  large : Font
  huge : Font

/-- Render text demo content to canvas -/
def renderText (c : Canvas) (fonts : Fonts) : IO Canvas := do
  -- Row 1: Basic text in different sizes
  let c := c.setFillColor Color.white
  let c ← c.fillTextXY "Small (16pt)" 50 50 fonts.small
  let c ← c.fillTextXY "Medium (24pt)" 50 90 fonts.medium
  let c ← c.fillTextXY "Large (36pt)" 50 140 fonts.large
  let c ← c.fillTextXY "Huge (48pt)" 50 200 fonts.huge

  -- Row 2: Text in different colors
  let c := c.setFillColor Color.red
  let c ← c.fillTextXY "Red Text" 500 50 fonts.medium
  let c := c.setFillColor Color.green
  let c ← c.fillTextXY "Green Text" 500 90 fonts.medium
  let c := c.setFillColor Color.blue
  let c ← c.fillTextXY "Blue Text" 500 130 fonts.medium
  let c := c.setFillColor Color.yellow
  let c ← c.fillTextXY "Yellow Text" 500 170 fonts.medium
  let c := c.setFillColor Color.cyan
  let c ← c.fillTextXY "Cyan Text" 500 210 fonts.medium
  let c := c.setFillColor Color.magenta
  let c ← c.fillTextXY "Magenta Text" 500 250 fonts.medium

  -- Row 3: Showcase text content
  let c := c.setFillColor Color.white
  let c ← c.fillTextXY "Afferent - A Lean 4 2D Graphics Library" 50 300 fonts.large

  -- Row 4: Mixed content - text with shapes
  let c := c.setFillColor Color.blue
  let c ← c.fillRect (Rect.mk' 50 350 150 40)
  let c := c.setFillColor Color.white
  let c ← c.fillTextXY "Text on Shape" 60 380 fonts.small

  let c := c.setFillColor Color.red
  let c ← c.fillCircle ⟨350, 370⟩ 30
  let c := c.setFillColor Color.white
  let c ← c.fillTextXY "Labels" 320 420 fonts.small

  let c := c.setFillColor Color.green
  let c ← c.fillRoundedRect (Rect.mk' 450 350 180 40) 10
  let c := c.setFillColor Color.black
  let c ← c.fillTextXY "Rounded Button" 460 380 fonts.small

  -- Row 5: Character set sample
  let c := c.setFillColor Color.white
  let c ← c.fillTextXY "ABCDEFGHIJKLMNOPQRSTUVWXYZ" 50 470 fonts.medium
  let c ← c.fillTextXY "abcdefghijklmnopqrstuvwxyz" 50 510 fonts.medium
  let c ← c.fillTextXY "0123456789 !@#$%^&*()_+-=" 50 550 fonts.medium

  -- Row 6: Semi-transparent text
  let c := c.setFillColor (Color.rgba 1.0 1.0 1.0 0.7)
  let c ← c.fillTextXY "Semi-transparent" 50 600 fonts.medium
  let c := c.setFillColor (Color.rgba 1.0 1.0 1.0 0.4)
  let c ← c.fillTextXY "More transparent" 300 600 fonts.medium
  let c := c.setFillColor (Color.rgba 1.0 1.0 1.0 0.2)
  let c ← c.fillTextXY "Very faint" 550 600 fonts.medium

  -- Row 7: Colored backgrounds with text
  let c := c.setFillColor (Color.rgba 0.8 0.2 0.2 1.0)
  let c ← c.fillRect (Rect.mk' 50 640 200 40)
  let c := c.setFillColor Color.white
  let c ← c.fillTextXY "Error Message" 60 670 fonts.small

  let c := c.setFillColor (Color.rgba 0.2 0.6 0.2 1.0)
  let c ← c.fillRect (Rect.mk' 280 640 200 40)
  let c := c.setFillColor Color.white
  let c ← c.fillTextXY "Success!" 330 670 fonts.small

  let c := c.setFillColor (Color.rgba 0.8 0.6 0.1 1.0)
  let c ← c.fillRect (Rect.mk' 510 640 200 40)
  let c := c.setFillColor Color.black
  let c ← c.fillTextXY "Warning" 570 670 fonts.small

  pure c

/-! ## Animation Helpers -/

/-- HSV to RGB color conversion (h in [0,1], s,v in [0,1]) -/
def hsvToRgb (h s v : Float) : Color :=
  let h6 := h * 6.0
  let i := h6.floor
  let f := h6 - i
  let p := v * (1.0 - s)
  let q := v * (1.0 - s * f)
  let t := v * (1.0 - s * (1.0 - f))
  let mod := (i.toUInt32 % 6).toNat
  match mod with
  | 0 => Color.rgb v t p
  | 1 => Color.rgb q v p
  | 2 => Color.rgb p v t
  | 3 => Color.rgb p q v
  | 4 => Color.rgb t p v
  | _ => Color.rgb v p q

/-- Render psychedelic animation cell -/
def renderAnimations (c : Canvas) (t : Float) : IO Canvas := do
  let pi := 3.14159265358979323846

  -- Spinning star cluster
  let c := c.save
  let c := c.translate 150 150
  let mut cv := c
  for i in [:7] do
    let c' := cv.save
    let angle := t * 2.0 + i.toFloat * (pi * 2.0 / 7.0)
    let dist := 60 + 20 * Float.sin (t * 3.0 + i.toFloat)
    let c' := c'.translate (dist * Float.cos angle) (dist * Float.sin angle)
    let c' := c'.rotate (t * 4.0 + i.toFloat)
    let hue := (t * 0.5 + i.toFloat / 7.0) - (t * 0.5 + i.toFloat / 7.0).floor
    let c' := c'.setFillColor (hsvToRgb hue 1.0 1.0)
    let c' ← c'.fillPath (Path.star ⟨0, 0⟩ (20 + 10 * Float.sin (t * 5.0)) 10 5)
    cv := c'.restore
  let c := cv.restore

  -- Pulsing rainbow circles
  let c := c.save
  let c := c.translate 400 150
  let mut cv2 := c
  for i in [:12] do
    let angle := i.toFloat * (pi / 6.0)
    let pulse := 0.5 + 0.5 * Float.sin (t * 4.0 + i.toFloat * 0.5)
    let radius := 20 + 30 * pulse
    let x := 80 * Float.cos (angle + t)
    let y := 80 * Float.sin (angle + t)
    let hue := (i.toFloat / 12.0 + t * 0.3) - (i.toFloat / 12.0 + t * 0.3).floor
    let c' := cv2.setFillColor (hsvToRgb hue 1.0 1.0)
    cv2 ← c'.fillCircle ⟨x, y⟩ radius
  let c := cv2.restore

  -- Wiggling lines (sine wave with moving phase)
  let c := c.save
  let c := c.translate 650 100
  let mut cv3 := c
  for row in [:5] do
    let rowOffset := row.toFloat * 0.5
    let c' := cv3.setLineWidth (2 + row.toFloat)
    let hue := (row.toFloat / 5.0 + t * 0.2) - (row.toFloat / 5.0 + t * 0.2).floor
    let c' := c'.setStrokeColor (hsvToRgb hue 1.0 1.0)
    let mut path := Path.empty
    path := path.moveTo ⟨0, row.toFloat * 50⟩
    for i in [:20] do
      let x := i.toFloat * 15
      let y := row.toFloat * 50 + 20 * Float.sin (t * 6.0 + x * 0.05 + rowOffset)
      path := path.lineTo ⟨x, y⟩
    cv3 ← c'.strokePath path
  let c := cv3.restore

  -- Morphing polygon (changing number of sides smoothly via rotation)
  let c := c.save
  let c := c.translate 150 300
  let c := c.rotate (t * 1.5)
  let sides := 3 + ((t * 0.5).floor.toUInt32 % 6).toNat
  let hue := (t * 0.4) - (t * 0.4).floor
  let c := c.setFillColor (hsvToRgb hue 0.8 0.9)
  let c ← c.fillPath (Path.polygon ⟨0, 0⟩ (40 + 20 * Float.sin t) sides)
  let c := c.restore

  -- Orbiting hearts with trail effect
  let c := c.save
  let c := c.translate 400 320
  let mut cv4 := c
  for i in [:8] do
    let trailT := t - i.toFloat * 0.05
    let angle := trailT * 2.0
    let x := 60 * Float.cos angle
    let y := 40 * Float.sin angle
    let alpha := 1.0 - i.toFloat * 0.12
    let hue := (trailT * 0.3) - (trailT * 0.3).floor
    let color := hsvToRgb hue 1.0 1.0
    let c' := cv4.setFillColor (Color.rgba color.r color.g color.b alpha)
    let c' := c'.save
    let c' := c'.translate x y
    let c' := c'.scale (0.3 + 0.1 * Float.sin (t * 3.0)) (0.3 + 0.1 * Float.sin (t * 3.0))
    let c' ← c'.fillPath (Path.heart ⟨0, 0⟩ 80)
    cv4 := c'.restore
  let c := cv4.restore

  -- Bouncing rectangles with color cycling
  let c := c.save
  let c := c.translate 650 280
  let mut cv5 := c
  for i in [:6] do
    let phase := i.toFloat * 0.8
    let bounce := Float.abs (Float.sin (t * 3.0 + phase)) * 60
    let x := i.toFloat * 45
    let rotation := t * 2.0 + phase
    let c' := cv5.save
    let c' := c'.translate x (-bounce)
    let c' := c'.rotate rotation
    let hue := (t * 0.5 + i.toFloat / 6.0) - (t * 0.5 + i.toFloat / 6.0).floor
    let c' := c'.setFillColor (hsvToRgb hue 0.9 1.0)
    let c' ← c'.fillRectXYWH (-15) (-15) 30 30
    cv5 := c'.restore
  let c := cv5.restore

  pure c

/-! ## Performance Test -/

/-- Render grid spinning squares using unified Dynamic module.
    Static grid positions, GPU does color + NDC conversion. -/
def renderGridTest (c : Canvas) (t : Float) (font : Font) (particles : Render.Dynamic.ParticleState)
    (halfSize : Float) : IO Canvas := do
  let c := c.setFillColor Color.white
  let c ← c.fillTextXY s!"Grid: {particles.count} dynamic squares (Space to advance)" 20 30 font
  Render.Dynamic.drawRectsAnimated c.ctx.renderer particles halfSize t 3.0
  pure c

/-- Render grid of spinning triangles using unified Dynamic module. -/
def renderTriangleTest (c : Canvas) (t : Float) (font : Font) (particles : Render.Dynamic.ParticleState)
    (halfSize : Float) : IO Canvas := do
  let c := c.setFillColor Color.white
  let c ← c.fillTextXY s!"Triangles: {particles.count} dynamic triangles (Space to advance)" 20 30 font
  Render.Dynamic.drawTrianglesAnimated c.ctx.renderer particles halfSize t 2.0
  pure c

/-- Render bouncing circles using unified Dynamic module.
    CPU updates positions (physics), GPU does color + NDC conversion. -/
def renderCircleTest (c : Canvas) (t : Float) (font : Font) (particles : Render.Dynamic.ParticleState)
    (radius : Float) : IO Canvas := do
  let c := c.setFillColor Color.white
  let c ← c.fillTextXY s!"Circles: {particles.count} dynamic circles (Space to advance)" 20 30 font
  Render.Dynamic.drawCircles c.ctx.renderer particles radius t
  pure c

/-- Render textured sprites using FloatBuffer (high-performance Bunnymark).
    Lean physics, FloatBuffer for zero-copy GPU rendering. -/
def renderSpriteTestFast (c : Canvas) (font : Font) (particles : Render.Dynamic.ParticleState)
    (spriteBuffer : FFI.FloatBuffer) (texture : FFI.Texture) (halfSize : Float) : IO Canvas := do
  let c := c.setFillColor Color.white
  let c ← c.fillTextXY s!"Sprites: {particles.count} textured sprites [FloatBuffer] (Space to advance)" 20 30 font
  -- Write particle positions to FloatBuffer (1 FFI call per sprite)
  Render.Dynamic.writeSpritesToBuffer particles spriteBuffer halfSize
  -- Render from FloatBuffer (zero-copy to GPU)
  Render.Dynamic.drawSpritesFromBuffer c.ctx.renderer texture spriteBuffer particles.count.toUInt32 halfSize particles.screenWidth particles.screenHeight
  pure c

/-! ## Unified Visual Demo -/

def unifiedDemo : IO Unit := do
  IO.println "Unified Visual Demo (with Animations!)"
  IO.println "--------------------------------------"

  -- Create a single large canvas: 1920x1080
  let canvas ← Canvas.create 1920 1080 "Afferent - Visual Demos (LSD Disco Party Edition)"

  IO.println "Loading fonts..."
  let fontSmall ← Font.load "/System/Library/Fonts/Monaco.ttf" 16
  let fontMedium ← Font.load "/System/Library/Fonts/Monaco.ttf" 24
  let fontLarge ← Font.load "/System/Library/Fonts/Monaco.ttf" 36
  let fontHuge ← Font.load "/System/Library/Fonts/Monaco.ttf" 48
  let fonts : Fonts := { small := fontSmall, medium := fontMedium, large := fontLarge, huge := fontHuge }

  IO.println "Loading sprite texture..."
  let spriteTexture ← FFI.Texture.load "nibble.png"
  let (texWidth, texHeight) ← FFI.Texture.getSize spriteTexture
  IO.println s!"Loaded nibble.png: {texWidth}x{texHeight}"

  IO.println "Rendering animated demo... (close window to exit)"
  IO.println "Press SPACE to toggle performance test mode (10000 spinning squares)"

  -- Grid layout: 2x3, cell size 960x360
  -- Scale factors (uniform, based on original demo sizes):
  -- - Shapes: 1000x800 -> scale by min(960/1000, 360/800) = min(0.96, 0.45) = 0.45
  -- - Transforms: 800x600 -> scale by min(960/800, 360/600) = min(1.2, 0.6) = 0.6
  -- - Strokes: 900x700 -> scale by min(960/900, 360/700) = min(1.07, 0.51) = 0.51
  -- - Gradients: 900x700 -> 0.51
  -- - Text: 900x700 -> 0.51

  -- Grid cell dimensions (in logical canvas coordinates)
  let cellWidth : Float := 960
  let cellHeight : Float := 360

  -- Background colors for each cell (animated versions will vary with time)
  let bg00 := Color.rgba 0.15 0.15 0.20 1.0  -- Dark blue-gray
  let bg10 := Color.rgba 0.20 0.15 0.15 1.0  -- Dark red-gray
  let bg01 := Color.rgba 0.15 0.20 0.15 1.0  -- Dark green-gray
  let bg11 := Color.rgba 0.20 0.18 0.12 1.0  -- Dark warm gray
  let bg02 := Color.rgba 0.18 0.15 0.20 1.0  -- Dark purple-gray

  -- Pre-compute particle data ONCE at startup using unified Dynamic module
  let halfSize := 1.5  -- Smaller for 100k
  let circleRadius := 2.0
  let spriteHalfSize := 15.0  -- Size for sprite rendering

  -- Grid particles (316x316 ≈ 100k grid of spinning squares/triangles)
  let gridCols := 316 * 3
  let gridRows := 316 * 2
  let gridSpacing := 2.0
  let gridStartX := (1920.0 - (gridCols.toFloat - 1) * gridSpacing) / 2.0
  let gridStartY := (1080.0 - (gridRows.toFloat - 1) * gridSpacing) / 2.0
  let gridParticles := Render.Dynamic.ParticleState.createGrid gridCols gridRows gridStartX gridStartY gridSpacing 1920.0 1080.0
  IO.println s!"Created {gridParticles.count} grid particles"

  -- Bouncing circles using Dynamic.ParticleState
  let bouncingParticles := Render.Dynamic.ParticleState.create 1000000 1920.0 1080.0 42
  IO.println s!"Created {bouncingParticles.count} bouncing circles"

  -- Sprite particles for Bunnymark-style benchmark (Lean physics, FloatBuffer rendering)
  let spriteParticles := Render.Dynamic.ParticleState.create 1000000 1920.0 1080.0 123
  let spriteBuffer ← FFI.FloatBuffer.create (spriteParticles.count.toUSize * 5)  -- 5 floats per sprite
  let circleBuffer ← FFI.FloatBuffer.create (bouncingParticles.count.toUSize * 4)  -- 4 floats per circle
  IO.println s!"Created {spriteParticles.count} bouncing sprites (Lean physics, FloatBuffer rendering)"

  -- No GPU upload needed! Dynamic module sends positions each frame.
  IO.println "Using unified Dynamic rendering - CPU positions, GPU color/NDC."

  -- Display modes: 0 = demo, 1 = grid squares, 2 = triangles, 3 = circles, 4 = sprites
  let startTime ← IO.monoMsNow
  let mut c := canvas
  let mut displayMode : Nat := 0
  let mut msaaEnabled : Bool := true
  let mut lastTime := startTime
  let mut bouncingState := bouncingParticles
  let mut spriteState := spriteParticles
  -- FPS counter (smoothed over multiple frames)
  let mut frameCount : Nat := 0
  let mut fpsAccumulator : Float := 0.0
  let mut displayFps : Float := 0.0

  while !(← c.shouldClose) do
    c.pollEvents

    -- Check for Space key (key code 49) to cycle through modes
    let keyCode ← c.getKeyCode
    if keyCode == 49 then  -- Space bar
      displayMode := (displayMode + 1) % 5
      c.clearKey
      -- Disable MSAA only for sprite benchmark mode to maximize throughput
      msaaEnabled := displayMode != 4
      FFI.Renderer.setMSAAEnabled c.ctx.renderer msaaEnabled
      -- Also disable Retina (render at 1x drawable scale) for sprite benchmark
      if displayMode == 4 then
        FFI.Renderer.setDrawableScale c.ctx.renderer 1.0
      else
        FFI.Renderer.setDrawableScale c.ctx.renderer 0.0
      match displayMode with
      | 0 => IO.println "Switched to DEMO mode"
      | 1 => IO.println "Switched to GRID (squares) performance test"
      | 2 => IO.println "Switched to TRIANGLES performance test"
      | 3 => IO.println "Switched to CIRCLES (bouncing) performance test"
      | _ => IO.println "Switched to SPRITES (Bunnymark) performance test"

    let ok ← c.beginFrame Color.darkGray
    if ok then
      let now ← IO.monoMsNow
      let t := (now - startTime).toFloat / 1000.0  -- Elapsed seconds
      let dt := (now - lastTime).toFloat / 1000.0  -- Delta time
      lastTime := now

      -- Update FPS counter (update display every 10 frames for stability)
      frameCount := frameCount + 1
      if dt > 0.0 then
        fpsAccumulator := fpsAccumulator + (1.0 / dt)
      if frameCount >= 10 then
        displayFps := fpsAccumulator / frameCount.toFloat
        fpsAccumulator := 0.0
        frameCount := 0

      let c' := c.resetTransform

      if displayMode == 1 then
        -- Grid performance test: squares spinning in a grid
        c ← renderGridTest c' t fontMedium gridParticles halfSize
      else if displayMode == 2 then
        -- Triangle performance test: triangles spinning in a grid
        c ← renderTriangleTest c' t fontMedium gridParticles halfSize
      else if displayMode == 3 then
        -- Circle performance test: bouncing circles
        bouncingState ← bouncingState.updateBouncingAndWriteCircles dt circleRadius circleBuffer
        let c1 := c'.setFillColor Color.white
        let c1 ← c1.fillTextXY s!"Circles: {bouncingState.count} dynamic circles [fused] (Space to advance)" 20 30 fontMedium
        Render.Dynamic.drawCirclesFromBuffer c1.ctx.renderer circleBuffer bouncingState.count.toUInt32 t bouncingState.screenWidth bouncingState.screenHeight
        c := c1
      else if displayMode == 4 then
        -- Sprite performance test: bouncing textured sprites (Bunnymark)
        -- Physics runs in Lean, rendering uses FloatBuffer for zero-copy GPU upload
        spriteState ← spriteState.updateBouncingAndWriteSprites dt spriteHalfSize spriteBuffer
        let c1 := c'.setFillColor Color.white
        let c1 ← c1.fillTextXY s!"Sprites: {spriteState.count} textured sprites [fused] (Space to advance)" 20 30 fontMedium
        Render.Dynamic.drawSpritesFromBuffer c1.ctx.renderer spriteTexture spriteBuffer spriteState.count.toUInt32 spriteHalfSize spriteState.screenWidth spriteState.screenHeight
        c := c1
      else
        -- Normal demo mode: grid of demos
        let canvas := c'

        -- Cell 0,0: Shapes demo (top-left)
        let cellRect00 := Rect.mk' 0 0 cellWidth cellHeight
        canvas.clip cellRect00
        let canvas := canvas.setFillColor bg00
        let canvas ← canvas.fillRect cellRect00
        let canvas := canvas.setFillColor (Color.rgba 1.0 1.0 1.0 0.5)
        let canvas ← canvas.fillTextXY "Cell: 0,0 - Shapes" 10 20 fontSmall
        let canvas := canvas.save
        let canvas := canvas.scale 0.45 0.45
        let canvas ← renderShapes canvas
        let canvas := canvas.restore
        canvas.unclip

        -- Cell 1,0: Transforms demo (top-right)
        let cellRect10 := Rect.mk' cellWidth 0 cellWidth cellHeight
        canvas.clip cellRect10
        let canvas := canvas.setFillColor bg10
        let canvas ← canvas.fillRect cellRect10
        let canvas := canvas.setFillColor (Color.rgba 1.0 1.0 1.0 0.5)
        let canvas ← canvas.fillTextXY "Cell: 1,0 - Transforms" (cellWidth + 10) 20 fontSmall
        let canvas := canvas.save
        let canvas := canvas.translate 960 0
        let canvas := canvas.scale 0.6 0.6
        let canvas ← renderTransforms canvas
        let canvas := canvas.restore
        canvas.unclip

        -- Cell 0,1: Strokes demo (middle-left)
        let cellRect01 := Rect.mk' 0 cellHeight cellWidth cellHeight
        canvas.clip cellRect01
        let canvas := canvas.setFillColor bg01
        let canvas ← canvas.fillRect cellRect01
        let canvas := canvas.setFillColor (Color.rgba 1.0 1.0 1.0 0.5)
        let canvas ← canvas.fillTextXY "Cell: 0,1 - Strokes" 10 (cellHeight + 20) fontSmall
        let canvas := canvas.save
        let canvas := canvas.translate 0 360
        let canvas := canvas.scale 0.51 0.51
        let canvas ← renderStrokes canvas
        let canvas := canvas.restore
        canvas.unclip

        -- Cell 1,1: Gradients demo (middle-right)
        let cellRect11 := Rect.mk' cellWidth cellHeight cellWidth cellHeight
        canvas.clip cellRect11
        let canvas := canvas.setFillColor bg11
        let canvas ← canvas.fillRect cellRect11
        let canvas := canvas.setFillColor (Color.rgba 1.0 1.0 1.0 0.5)
        let canvas ← canvas.fillTextXY "Cell: 1,1 - Gradients" (cellWidth + 10) (cellHeight + 20) fontSmall
        let canvas := canvas.save
        let canvas := canvas.translate 960 360
        let canvas := canvas.scale 0.51 0.51
        let canvas ← renderGradients canvas
        let canvas := canvas.restore
        canvas.unclip

        -- Cell 0,2: Text demo (bottom-left)
        let cellRect02 := Rect.mk' 0 (cellHeight * 2) cellWidth cellHeight
        canvas.clip cellRect02
        let canvas := canvas.setFillColor bg02
        let canvas ← canvas.fillRect cellRect02
        let canvas := canvas.setFillColor (Color.rgba 1.0 1.0 1.0 0.5)
        let canvas ← canvas.fillTextXY "Cell: 0,2 - Text" 10 (cellHeight * 2 + 20) fontSmall
        let canvas := canvas.save
        let canvas := canvas.translate 0 720
        let canvas := canvas.scale 0.51 0.51
        let canvas ← renderText canvas fonts
        let canvas := canvas.restore
        canvas.unclip

        -- Cell 1,2: Animations demo (bottom-right) - THE DISCO PARTY!
        let cellRect12 := Rect.mk' cellWidth (cellHeight * 2) cellWidth cellHeight
        canvas.clip cellRect12
        -- Animated background color
        let bgHue := (t * 0.1) - (t * 0.1).floor
        let bg12 := hsvToRgb bgHue 0.3 0.15
        let canvas := canvas.setFillColor bg12
        let canvas ← canvas.fillRect cellRect12
        let canvas := canvas.setFillColor (Color.rgba 1.0 1.0 1.0 0.7)
        let canvas ← canvas.fillTextXY "Cell: 1,2 - DISCO PARTY!" (cellWidth + 10) (cellHeight * 2 + 20) fontSmall
        let canvas := canvas.save
        let canvas := canvas.translate cellWidth (cellHeight * 2 + 30)
        let canvas ← renderAnimations canvas t
        let _canvas := canvas.restore
        canvas.unclip

        pure ()

      -- Render FPS counter in top-right corner (after all other rendering)
      let fpsText := s!"{displayFps.toUInt32} FPS"
      let (textWidth, _) ← fontSmall.measureText fpsText
      let c' := c.resetTransform
      let c' := c'.setFillColor (Color.rgba 0.0 0.0 0.0 0.6)
      let c' ← c'.fillRectXYWH (1920.0 - textWidth - 20.0) 5.0 (textWidth + 15.0) 25.0
      let c' := c'.setFillColor Color.white
      let _c' ← c'.fillTextXY fpsText (1920.0 - textWidth - 12.0) 22.0 fontSmall

      c.endFrame

  IO.println "Cleaning up..."
  fontSmall.destroy
  fontMedium.destroy
  fontLarge.destroy
  fontHuge.destroy
  canvas.destroy

def main : IO Unit := do
  IO.println "Afferent - 2D Vector Graphics Library"
  IO.println "======================================"
  IO.println ""

  -- Run collimator demo first
  collimatorDemo

  -- Run unified visual demo (single window with all demos)
  unifiedDemo

  IO.println ""
  IO.println "Done!"

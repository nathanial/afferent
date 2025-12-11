/-
  Afferent - A Lean 4 2D vector graphics library
  Main executable - demonstrates collimator optics and basic shape rendering
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

def graphicsDemo : IO Unit := do
  IO.println "Shapes & Curves Demo"
  IO.println "--------------------"

  -- Create drawing context (larger window for more shapes)
  let ctx ← DrawContext.create 1000 800 "Afferent - Shapes & Curves"

  IO.println "Rendering shapes... (close window to exit)"

  -- Run the render loop
  ctx.runLoop Color.darkGray fun ctx => do
    -- Row 1: Basic rectangles (original demo)
    ctx.fillRectXYWH 50 30 120 80 Color.red
    ctx.fillRectXYWH 200 30 120 80 Color.green
    ctx.fillRectXYWH 350 30 120 80 Color.blue

    -- Row 1: Circles (original demo)
    ctx.fillCircle ⟨550, 70⟩ 40 Color.yellow
    ctx.fillCircle ⟨650, 70⟩ 40 Color.cyan
    ctx.fillCircle ⟨750, 70⟩ 40 Color.magenta

    -- Row 1: Rounded rectangle (original demo)
    ctx.fillRoundedRect (Rect.mk' 820 30 130 80) 15 Color.white

    -- Row 2: Stars
    ctx.fillPath (Path.star ⟨100, 200⟩ 50 25 5) Color.yellow      -- 5-point star
    ctx.fillPath (Path.star ⟨220, 200⟩ 45 25 6) Color.orange      -- 6-point star
    ctx.fillPath (Path.star ⟨340, 200⟩ 40 25 8) Color.red         -- 8-point star

    -- Row 2: Regular polygons
    ctx.fillPath (Path.polygon ⟨480, 200⟩ 45 3) Color.green       -- Triangle
    ctx.fillPath (Path.polygon ⟨600, 200⟩ 45 5) Color.cyan        -- Pentagon
    ctx.fillPath (Path.polygon ⟨720, 200⟩ 45 6) Color.blue        -- Hexagon
    ctx.fillPath (Path.polygon ⟨850, 200⟩ 45 8) Color.purple      -- Octagon

    -- Row 3: Hearts and ellipses
    ctx.fillPath (Path.heart ⟨100, 350⟩ 80) Color.red
    ctx.fillPath (Path.heart ⟨230, 350⟩ 60) Color.magenta
    ctx.fillEllipse ⟨380, 350⟩ 70 40 Color.orange
    ctx.fillEllipse ⟨520, 350⟩ 40 60 Color.green

    -- Row 3: Pie slices (like pie chart)
    let pi := 3.14159265358979323846
    ctx.fillPath (Path.pie ⟨680, 350⟩ 60 0 (pi * 0.5)) Color.red
    ctx.fillPath (Path.pie ⟨680, 350⟩ 60 (pi * 0.5) pi) Color.green
    ctx.fillPath (Path.pie ⟨680, 350⟩ 60 pi (pi * 1.5)) Color.blue
    ctx.fillPath (Path.pie ⟨680, 350⟩ 60 (pi * 1.5) (pi * 2)) Color.yellow

    -- Row 3: Semicircle
    ctx.fillPath (Path.semicircle ⟨850, 350⟩ 50 0) Color.purple

    -- Row 4: Bezier curves (as filled shapes)
    -- Quadratic curve demo - a curved banner shape
    let banner := Path.empty
      |>.moveTo ⟨50, 480⟩
      |>.lineTo ⟨200, 480⟩
      |>.quadraticCurveTo ⟨250, 530⟩ ⟨200, 580⟩
      |>.lineTo ⟨50, 580⟩
      |>.quadraticCurveTo ⟨0, 530⟩ ⟨50, 480⟩
      |>.closePath
    ctx.fillPath banner Color.cyan

    -- Cubic bezier curve demo - a teardrop shape
    let teardrop := Path.empty
      |>.moveTo ⟨350, 480⟩
      |>.bezierCurveTo ⟨420, 450⟩ ⟨420, 600⟩ ⟨350, 580⟩
      |>.bezierCurveTo ⟨280, 600⟩ ⟨280, 450⟩ ⟨350, 480⟩
      |>.closePath
    ctx.fillPath teardrop Color.orange

    -- Row 4: Arc paths
    ctx.fillPath (Path.arcPath ⟨550, 530⟩ 50 0 (pi * 1.5) |>.closePath) Color.green

    -- Row 4: More rounded rectangles with different radii
    ctx.fillRoundedRect (Rect.mk' 650 470 100 80) 5 Color.red
    ctx.fillRoundedRect (Rect.mk' 780 470 100 80) 30 Color.blue

    -- Row 5: Custom triangle
    ctx.fillPath (Path.triangle ⟨100, 650⟩ ⟨180, 750⟩ ⟨20, 750⟩) Color.yellow

    -- Row 5: Equilateral triangles
    ctx.fillPath (Path.equilateralTriangle ⟨280, 700⟩ 50) Color.green
    ctx.fillPath (Path.equilateralTriangle ⟨380, 700⟩ 40) Color.cyan

    -- Row 5: More complex custom path - a speech bubble
    let bubble := Path.empty
      |>.moveTo ⟨500, 650⟩
      |>.lineTo ⟨700, 650⟩
      |>.bezierCurveTo ⟨730, 650⟩ ⟨730, 680⟩ ⟨730, 700⟩
      |>.lineTo ⟨730, 730⟩
      |>.bezierCurveTo ⟨730, 760⟩ ⟨700, 760⟩ ⟨670, 760⟩
      |>.lineTo ⟨570, 760⟩
      |>.lineTo ⟨550, 790⟩  -- Speech bubble pointer
      |>.lineTo ⟨560, 760⟩
      |>.lineTo ⟨530, 760⟩
      |>.bezierCurveTo ⟨500, 760⟩ ⟨470, 760⟩ ⟨470, 730⟩
      |>.lineTo ⟨470, 700⟩
      |>.bezierCurveTo ⟨470, 670⟩ ⟨470, 650⟩ ⟨500, 650⟩
      |>.closePath
    ctx.fillPath bubble Color.white

    -- Row 5: Diamond shape using polygon rotation
    let diamond := Path.empty
      |>.moveTo ⟨850, 650⟩
      |>.lineTo ⟨900, 700⟩
      |>.lineTo ⟨850, 760⟩
      |>.lineTo ⟨800, 700⟩
      |>.closePath
    ctx.fillPath diamond Color.cyan

  IO.println "Cleaning up..."
  ctx.destroy

def transformDemo : IO Unit := do
  IO.println "Transform & State Demo"
  IO.println "----------------------"

  -- Create a stateful canvas
  let canvas ← Canvas.create 800 600 "Afferent - Transforms"

  IO.println "Rendering transformed shapes... (close window to exit)"

  let pi := 3.14159265358979323846

  -- Run the stateful render loop
  canvas.runLoop Color.darkGray fun c => do
    -- Reset transform at start of each frame
    let c := c.resetTransform

    -- Row 1: Basic shapes without transform (reference)
    let c := c.setFillColor Color.white
    c.fillRectXYWH 50 30 60 40
    c.fillCircle ⟨180, 50⟩ 25

    -- Row 1: Translated shapes
    let c := c.save  -- Save state before transform
    let c := c.translate 250 0
    let c := c.setFillColor Color.red
    c.fillRectXYWH 50 30 60 40
    c.fillCircle ⟨180, 50⟩ 25
    let c := c.restore  -- Restore to remove translation

    -- Row 1: Scaled shapes (2x)
    let c := c.save
    let c := c.translate 500 50  -- Move to position first
    let c := c.scale 1.5 1.5     -- Then scale
    let c := c.setFillColor Color.green
    c.fillRectXYWH (-30) (-20) 60 40  -- Draw centered at origin
    c.fillCircle ⟨50, 0⟩ 25
    let c := c.restore

    -- Row 2: Rotated rectangles (rotation fan)
    let c := c.save
    let c := c.translate 150 200  -- Center point for rotation
    for i in [:8] do
      let c := c.save
      let angle := i.toFloat * (pi / 4.0)  -- 45 degree increments
      let c := c.rotate angle
      let c := c.setFillColor (Color.rgba
        (0.5 + 0.5 * Float.cos angle)
        (0.5 + 0.5 * Float.sin angle)
        0.5
        0.8)
      c.fillRectXYWH 30 (-10) 50 20
      pure ()  -- Need pure for the loop
    let c := c.restore

    -- Row 2: Scaled circles (size variation)
    let c := c.save
    let c := c.translate 400 200
    for i in [:5] do
      let c := c.save
      let s := 0.5 + i.toFloat * 0.3
      let c := c.translate (i.toFloat * 50) 0
      let c := c.scale s s
      let c := c.setFillColor (Color.rgba (1.0 - i.toFloat * 0.15) (i.toFloat * 0.2) (0.5 + i.toFloat * 0.1) 1.0)
      c.fillCircle ⟨0, 0⟩ 30
      pure ()
    let c := c.restore

    -- Row 3: Combined transforms - rotating star
    let c := c.save
    let c := c.translate 150 380
    let c := c.rotate (pi / 6.0)  -- 30 degree rotation
    let c := c.scale 1.2 0.8       -- Squash it
    let c := c.setFillColor Color.yellow
    c.fillPath (Path.star ⟨0, 0⟩ 60 30 5)
    let c := c.restore

    -- Row 3: Nested transforms
    let c := c.save
    let c := c.translate 350 380
    let c := c.setFillColor Color.blue
    c.fillCircle ⟨0, 0⟩ 50  -- Outer circle

    let c := c.save
    let c := c.translate 0 0
    let c := c.scale 0.6 0.6
    let c := c.setFillColor Color.cyan
    c.fillCircle ⟨0, 0⟩ 50  -- Inner circle (scaled down)

    let c := c.save
    let c := c.scale 0.5 0.5
    let c := c.setFillColor Color.white
    c.fillCircle ⟨0, 0⟩ 50  -- Innermost circle
    let c := c.restore
    let c := c.restore
    let c := c.restore

    -- Row 3: Global alpha demo
    let c := c.save
    let c := c.translate 550 380
    let c := c.setFillColor Color.red
    c.fillRectXYWH (-40) (-30) 80 60

    let c := c.setGlobalAlpha 0.5  -- 50% transparent
    let c := c.setFillColor Color.blue
    c.fillRectXYWH (-20) (-10) 80 60  -- Overlapping semi-transparent

    let c := c.setGlobalAlpha 0.3  -- 30% transparent
    let c := c.setFillColor Color.green
    c.fillRectXYWH 0 10 80 60  -- More transparent
    let c := c.restore

    -- Row 4: Transform composition demo - orbiting shapes
    let c := c.save
    let c := c.translate 200 520
    for i in [:6] do
      let c := c.save
      let angle := i.toFloat * (pi / 3.0)
      let c := c.rotate angle
      let c := c.translate 60 0  -- Move out from center
      let c := c.rotate (-angle)  -- Counter-rotate to keep upright
      let c := c.setFillColor (Color.rgba
        (if i % 2 == 0 then 1.0 else 0.5)
        (if i % 3 == 0 then 1.0 else 0.3)
        (if i % 2 == 1 then 1.0 else 0.2)
        1.0)
      c.fillRectXYWH (-15) (-15) 30 30
      pure ()
    let c := c.restore

    -- Row 4: Skewed/sheared effect via non-uniform scale + rotation
    let c := c.save
    let c := c.translate 450 520
    let c := c.rotate (pi / 12.0)  -- Slight rotation
    let c := c.scale 1.5 0.7        -- Non-uniform scale creates shear-like effect
    let c := c.setFillColor Color.magenta
    c.fillRectXYWH (-40) (-25) 80 50
    let c := c.restore

    -- Row 4: Hearts with different transforms
    let c := c.save
    let c := c.translate 620 520
    let c := c.setFillColor Color.red
    c.fillPath (Path.heart ⟨0, 0⟩ 50)

    let c := c.translate 100 0
    let c := c.rotate (pi / 8.0)
    let c := c.scale 0.7 0.7
    let c := c.setFillColor Color.magenta
    c.fillPath (Path.heart ⟨0, 0⟩ 50)
    let c := c.restore

    pure c

  IO.println "Cleaning up..."
  canvas.destroy

def main : IO Unit := do
  IO.println "Afferent - 2D Vector Graphics Library"
  IO.println "======================================"
  IO.println ""

  -- Run collimator demo first
  collimatorDemo

  -- Run shapes demo
  graphicsDemo

  -- Run transform demo
  transformDemo

  IO.println ""
  IO.println "Done!"

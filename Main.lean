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

def main : IO Unit := do
  IO.println "Afferent - 2D Vector Graphics Library"
  IO.println "======================================"
  IO.println ""

  -- Run collimator demo first
  collimatorDemo

  -- Then run graphics demo
  graphicsDemo

  IO.println ""
  IO.println "Done!"

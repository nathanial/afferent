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
  IO.println "Basic Shapes Demo"
  IO.println "-----------------"

  -- Create drawing context
  let ctx ← DrawContext.create 800 600 "Afferent - Basic Shapes"

  IO.println "Rendering shapes... (close window to exit)"

  -- Run the render loop
  ctx.runLoop Color.darkGray fun ctx => do
    -- Draw a red rectangle
    ctx.fillRectXYWH 50 50 200 150 Color.red

    -- Draw a green rectangle
    ctx.fillRectXYWH 300 50 200 150 Color.green

    -- Draw a blue rectangle
    ctx.fillRectXYWH 550 50 200 150 Color.blue

    -- Draw a yellow circle
    ctx.fillCircle ⟨150, 350⟩ 80 Color.yellow

    -- Draw a cyan circle
    ctx.fillCircle ⟨400, 350⟩ 80 Color.cyan

    -- Draw a magenta circle
    ctx.fillCircle ⟨650, 350⟩ 80 Color.magenta

    -- Draw a white rounded rectangle
    ctx.fillRoundedRect (Rect.mk' 275 450 250 100) 20 Color.white

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

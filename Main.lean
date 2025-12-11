/-
  Afferent - A Lean 4 2D vector graphics library
  Main executable - demonstrates collimator optics and Metal rendering
-/
import Afferent.FFI.Metal
import Collimator.Prelude

open Afferent.FFI
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
  IO.println "Metal Graphics Demo"
  IO.println "-------------------"

  -- Initialize the native library
  init

  -- Create window
  IO.println "Creating window..."
  let window ← Window.create 800 600 "Afferent"

  -- Create renderer
  IO.println "Creating renderer..."
  let renderer ← Renderer.create window

  -- Define a triangle in NDC coordinates (-1 to 1)
  -- Each vertex: x, y, r, g, b, a
  let vertices : Array Float := #[
    -- Top vertex (red)
     0.0,  0.5,   1.0, 0.0, 0.0, 1.0,
    -- Bottom left (green)
    -0.5, -0.5,   0.0, 1.0, 0.0, 1.0,
    -- Bottom right (blue)
     0.5, -0.5,   0.0, 0.0, 1.0, 1.0
  ]

  let indices : Array UInt32 := #[0, 1, 2]

  -- Create GPU buffers
  IO.println "Creating buffers..."
  let vertexBuffer ← Buffer.createVertex renderer vertices
  let indexBuffer ← Buffer.createIndex renderer indices

  IO.println "Rendering... (close window to exit)"

  -- Main render loop
  let mut frameCount : Nat := 0
  while !(← window.shouldClose) do
    -- Poll window events
    window.pollEvents

    -- Begin frame with dark gray background
    let frameOk ← renderer.beginFrame 0.1 0.1 0.1 1.0

    if frameOk then
      -- Draw the triangle
      renderer.drawTriangles vertexBuffer indexBuffer 3

      -- End frame (present)
      renderer.endFrame

    frameCount := frameCount + 1

  IO.println s!"Rendered {frameCount} frames"
  IO.println "Cleaning up..."

  -- Cleanup
  Buffer.destroy indexBuffer
  Buffer.destroy vertexBuffer
  Renderer.destroy renderer
  Window.destroy window

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

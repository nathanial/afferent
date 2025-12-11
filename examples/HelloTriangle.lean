/-
  Hello Triangle - Minimal example proving Metal + FFI pipeline works.
  Renders a colored triangle in a window.
-/
import Afferent.FFI.Metal

open Afferent.FFI

def main : IO Unit := do
  IO.println "Initializing Afferent..."

  -- Initialize the native library
  init

  -- Create window
  IO.println "Creating window..."
  let window ← Window.create 800 600 "Hello Triangle - Afferent"

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

  IO.println "Entering render loop... (close window to exit)"

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

  IO.println "Done!"

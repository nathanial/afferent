/-
  Spinning Cubes Demo
  Demonstrates 3D rendering with perspective projection, depth testing, and lighting.
  Shows a 5x5 grid of colored cubes rotating at different phases.
-/
import Afferent

open Afferent Afferent.FFI Afferent.Render

namespace Demos

/-- Render a 5x5 grid of spinning cubes.
    t: elapsed time in seconds
    renderer: the FFI renderer
    screenWidth/screenHeight: for aspect ratio calculation -/
def renderSpinningCubes (renderer : Renderer) (t : Float) (screenWidth screenHeight : Float) : IO Unit := do
  -- Camera setup (pulled back to see 5x5 grid)
  let aspect := screenWidth / screenHeight
  let fovY := 3.14159265358979 / 4.0  -- pi/4 radians = 45 degrees
  let proj := Matrix4.perspective fovY aspect 0.1 100.0
  let view := Matrix4.lookAt (0, 0, 12) (0, 0, 0) (0, 1, 0)

  -- Light direction (normalized, pointing from upper-right-front)
  let lightDir := #[0.5, 0.7, 0.5]

  -- Draw 5x5 grid of cubes
  for row in [:5] do
    for col in [:5] do
      -- Position in grid
      let x := (col.toFloat - 2.0) * 2.0
      let y := (row.toFloat - 2.0) * 2.0

      -- Phase offset for staggered rotation
      let phase := (row * 5 + col).toFloat * 0.25

      -- Build model matrix: translate then rotate
      let translateMat := Matrix4.translate x y 0
      let rotateYMat := Matrix4.rotateY (t + phase)
      let rotateXMat := Matrix4.rotateX (t * 0.7 + phase)

      -- Combine: model = translate * rotateY * rotateX
      let model := Matrix4.multiply translateMat (Matrix4.multiply rotateYMat rotateXMat)

      -- MVP = proj * view * model
      let viewModel := Matrix4.multiply view model
      let mvp := Matrix4.multiply proj viewModel

      -- Draw the cube
      Renderer.drawMesh3D renderer
        Mesh.cubeVertices
        Mesh.cubeIndices
        mvp.toArray
        model.toArray
        lightDir
        0.5  -- ambient light factor

end Demos

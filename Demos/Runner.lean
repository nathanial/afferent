/-
  Demo Runner - Main orchestration for all demos
-/
import Afferent
import Demos.Shapes
import Demos.Transforms
import Demos.Strokes
import Demos.Gradients
import Demos.Text
import Demos.Animations
import Demos.Layout
import Demos.Collimator
import Demos.GridPerf
import Demos.TrianglesPerf
import Demos.CirclesPerf
import Demos.SpritesPerf

set_option maxRecDepth 1024

open Afferent CanvasM

namespace Demos

/-- Unified visual demo - runs all demos in a grid layout -/
def unifiedDemo : IO Unit := do
  IO.println "Unified Visual Demo (with Animations!)"
  IO.println "--------------------------------------"

  -- Query the actual screen scale factor (e.g., 1.5 for 150%, 2.0 for Retina)
  let screenScale ← FFI.getScreenScale
  IO.println s!"Screen scale factor: {screenScale}"

  -- Base logical dimensions
  let baseWidth : Float := 1920.0
  let baseHeight : Float := 1080.0

  -- Physical pixel dimensions
  let physWidth := (baseWidth * screenScale).toUInt32
  let physHeight := (baseHeight * screenScale).toUInt32
  IO.println s!"Physical resolution: {physWidth}x{physHeight}"

  -- Create canvas at physical resolution
  let canvas ← Canvas.create physWidth physHeight "Afferent - Visual Demos (LSD Disco Party Edition)"

  IO.println "Loading fonts..."
  -- Font sizes scaled for physical resolution
  let fontSmall ← Font.load "/System/Library/Fonts/Monaco.ttf" (16 * screenScale).toUInt32
  let fontMedium ← Font.load "/System/Library/Fonts/Monaco.ttf" (24 * screenScale).toUInt32
  let fontLarge ← Font.load "/System/Library/Fonts/Monaco.ttf" (36 * screenScale).toUInt32
  let fontHuge ← Font.load "/System/Library/Fonts/Monaco.ttf" (48 * screenScale).toUInt32
  let fonts : Fonts := { small := fontSmall, medium := fontMedium, large := fontLarge, huge := fontHuge }

  IO.println "Loading sprite texture..."
  let spriteTexture ← FFI.Texture.load "nibble.png"
  let (texWidth, texHeight) ← FFI.Texture.getSize spriteTexture
  IO.println s!"Loaded nibble.png: {texWidth}x{texHeight}"

  IO.println "Rendering animated demo... (close window to exit)"
  IO.println "Press SPACE to toggle performance test mode (10000 spinning squares)"

  -- Grid layout: 2x3, cell dimensions scaled for physical resolution
  let cellWidth : Float := 960 * screenScale
  let cellHeight : Float := 360 * screenScale

  -- Background colors for each cell (animated versions will vary with time)
  let bg00 := Color.hsva 0.667 0.25 0.20 1.0  -- Dark blue-gray
  let bg10 := Color.hsva 0.0 0.25 0.20 1.0    -- Dark red-gray
  let bg01 := Color.hsva 0.333 0.25 0.20 1.0  -- Dark green-gray
  let bg11 := Color.hsva 0.125 0.4 0.20 1.0   -- Dark warm gray
  let bg02 := Color.hsva 0.767 0.25 0.20 1.0  -- Dark purple-gray

  -- Pre-compute particle data ONCE at startup using unified Dynamic module
  -- Sizes scaled for physical resolution
  let halfSize := 1.5 * screenScale
  let circleRadius := 2.0 * screenScale
  let spriteHalfSize := 15.0 * screenScale  -- Size for sprite rendering

  -- Physical dimensions as floats
  let physWidthF := baseWidth * screenScale
  let physHeightF := baseHeight * screenScale

  -- Grid particles (316x316 ≈ 100k grid of spinning squares/triangles)
  let gridCols := 316 * 3
  let gridRows := 316 * 2
  let gridSpacing := 2.0 * screenScale
  let gridStartX := (physWidthF - (gridCols.toFloat - 1) * gridSpacing) / 2.0
  let gridStartY := (physHeightF - (gridRows.toFloat - 1) * gridSpacing) / 2.0
  let gridParticles := Render.Dynamic.ParticleState.createGrid gridCols gridRows gridStartX gridStartY gridSpacing physWidthF physHeightF
  IO.println s!"Created {gridParticles.count} grid particles"

  -- Bouncing circles using Dynamic.ParticleState
  let bouncingParticles := Render.Dynamic.ParticleState.create 1000000 physWidthF physHeightF 42
  IO.println s!"Created {bouncingParticles.count} bouncing circles"

  -- Sprite particles for Bunnymark-style benchmark (Lean physics, FloatBuffer rendering)
  let spriteParticles := Render.Dynamic.ParticleState.create 1000000 physWidthF physHeightF 123
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
      displayMode := (displayMode + 1) % 6
      c.clearKey
      -- Disable MSAA only for sprite benchmark mode to maximize throughput.
      -- Keep Retina/native drawable scaling enabled.
      msaaEnabled := displayMode != 4
      FFI.Renderer.setMSAAEnabled c.ctx.renderer msaaEnabled
      match displayMode with
      | 0 => IO.println "Switched to DEMO mode"
      | 1 => IO.println "Switched to GRID (squares) performance test"
      | 2 => IO.println "Switched to TRIANGLES performance test"
      | 3 => IO.println "Switched to CIRCLES (bouncing) performance test"
      | 4 => IO.println "Switched to SPRITES (Bunnymark) performance test"
      | _ => IO.println "Switched to LAYOUT demo (full-size)"

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

      if displayMode == 1 then
        -- Grid performance test: squares spinning in a grid
        c ← renderGridTest (c.resetTransform) t fontMedium gridParticles halfSize
      else if displayMode == 2 then
        -- Triangle performance test: triangles spinning in a grid
        c ← renderTriangleTest (c.resetTransform) t fontMedium gridParticles halfSize
      else if displayMode == 3 then
        -- Circle performance test: bouncing circles
        bouncingState ← bouncingState.updateBouncingAndWriteCircles dt circleRadius circleBuffer
        c ← run' (c.resetTransform) do
          setFillColor Color.white
          fillTextXY s!"Circles: {bouncingState.count} dynamic circles [fused] (Space to advance)" (20 * screenScale) (30 * screenScale) fontMedium
        Render.Dynamic.drawCirclesFromBuffer c.ctx.renderer circleBuffer bouncingState.count.toUInt32 t bouncingState.screenWidth bouncingState.screenHeight
      else if displayMode == 4 then
        -- Sprite performance test: bouncing textured sprites (Bunnymark)
        -- Physics runs in Lean, rendering uses FloatBuffer for zero-copy GPU upload
        spriteState ← spriteState.updateBouncingAndWriteSprites dt spriteHalfSize spriteBuffer
        c ← run' (c.resetTransform) do
          setFillColor Color.white
          fillTextXY s!"Sprites: {spriteState.count} textured sprites [fused] (Space to advance)" (20 * screenScale) (30 * screenScale) fontMedium
        Render.Dynamic.drawSpritesFromBuffer c.ctx.renderer spriteTexture spriteBuffer spriteState.count.toUInt32 spriteHalfSize spriteState.screenWidth spriteState.screenHeight
      else if displayMode == 5 then
        -- Full-size Layout demo
        c ← run' (c.resetTransform) do
          save
          scale (2.0 * screenScale) (1.3 * screenScale)
          renderLayoutM fontSmall
          restore
          setFillColor Color.white
          fillTextXY "CSS Flexbox Layout Demo (Space to advance)" (20 * screenScale) (30 * screenScale) fontMedium
      else
        -- Normal demo mode: grid of demos using CanvasM for proper state threading
        c ← run' (c.resetTransform) do
          -- Cell 0,0: Shapes demo (top-left)
          let cellRect00 := Rect.mk' 0 0 cellWidth cellHeight
          clip cellRect00
          setFillColor bg00
          fillRect cellRect00
          setFillColor (Color.hsva 0.0 0.0 1.0 0.5)
          fillTextXY "Cell: 0,0 - Shapes" (10 * screenScale) (20 * screenScale) fontSmall
          save
          scale (0.45 * screenScale) (0.45 * screenScale)
          renderShapesM
          restore
          unclip

          -- Cell 1,0: Transforms demo (top-right)
          let cellRect10 := Rect.mk' cellWidth 0 cellWidth cellHeight
          clip cellRect10
          setFillColor bg10
          fillRect cellRect10
          setFillColor (Color.hsva 0.0 0.0 1.0 0.5)
          fillTextXY "Cell: 1,0 - Transforms" (cellWidth + 10 * screenScale) (20 * screenScale) fontSmall
          save
          translate cellWidth 0
          scale (0.6 * screenScale) (0.6 * screenScale)
          renderTransformsM
          restore
          unclip

          -- Cell 0,1: Strokes demo (middle-left)
          let cellRect01 := Rect.mk' 0 cellHeight cellWidth cellHeight
          clip cellRect01
          setFillColor bg01
          fillRect cellRect01
          setFillColor (Color.hsva 0.0 0.0 1.0 0.5)
          fillTextXY "Cell: 0,1 - Strokes" (10 * screenScale) (cellHeight + 20 * screenScale) fontSmall
          save
          translate 0 cellHeight
          scale (0.51 * screenScale) (0.51 * screenScale)
          renderStrokesM
          restore
          unclip

          -- Cell 1,1: Gradients demo (middle-right)
          let cellRect11 := Rect.mk' cellWidth cellHeight cellWidth cellHeight
          clip cellRect11
          setFillColor bg11
          fillRect cellRect11
          setFillColor (Color.hsva 0.0 0.0 1.0 0.5)
          fillTextXY "Cell: 1,1 - Gradients" (cellWidth + 10 * screenScale) (cellHeight + 20 * screenScale) fontSmall
          save
          translate cellWidth cellHeight
          scale (0.51 * screenScale) (0.51 * screenScale)
          renderGradientsM
          restore
          unclip

          -- Cell 0,2: Text demo (bottom-left)
          let cellRect02 := Rect.mk' 0 (cellHeight * 2) cellWidth cellHeight
          clip cellRect02
          setFillColor bg02
          fillRect cellRect02
          setFillColor (Color.hsva 0.0 0.0 1.0 0.5)
          fillTextXY "Cell: 0,2 - Text" (10 * screenScale) (cellHeight * 2 + 20 * screenScale) fontSmall
          save
          translate 0 (cellHeight * 2)
          scale (0.51 * screenScale) (0.51 * screenScale)
          renderTextM fonts
          restore
          unclip

          -- Cell 1,2: Layout demo (bottom-right)
          let cellRect12 := Rect.mk' cellWidth (cellHeight * 2) cellWidth cellHeight
          clip cellRect12
          let bg12 := Color.hsva 0.75 0.25 0.20 1.0  -- Dark purple-gray
          setFillColor bg12
          fillRect cellRect12
          setFillColor (Color.hsva 0.0 0.0 1.0 0.5)
          fillTextXY "Cell: 1,2 - Layout" (cellWidth + 10 * screenScale) (cellHeight * 2 + 20 * screenScale) fontSmall
          save
          translate cellWidth (cellHeight * 2)
          scale (0.45 * screenScale) (0.45 * screenScale)
          renderLayoutM fontSmall
          restore
          unclip

      -- Render FPS counter in top-right corner (after all other rendering)
      let fpsText := s!"{displayFps.toUInt32} FPS"
      let (textWidth, _) ← fontSmall.measureText fpsText
      c ← run' (c.resetTransform) do
        setFillColor (Color.hsva 0.0 0.0 0.0 0.6)
        fillRectXYWH (physWidthF - textWidth - 20 * screenScale) (5 * screenScale) (textWidth + 15 * screenScale) (25 * screenScale)
        setFillColor Color.white
        fillTextXY fpsText (physWidthF - textWidth - 12 * screenScale) (22 * screenScale) fontSmall

      c ← c.endFrame

  IO.println "Cleaning up..."
  fontSmall.destroy
  fontMedium.destroy
  fontLarge.destroy
  fontHuge.destroy
  canvas.destroy

/-- Main entry point - runs all demos -/
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

end Demos

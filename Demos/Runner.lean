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
import Demos.Collimator
import Demos.GridPerf
import Demos.TrianglesPerf
import Demos.CirclesPerf
import Demos.SpritesPerf

open Afferent

namespace Demos

/-- Unified visual demo - runs all demos in a grid layout -/
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
      -- Disable MSAA only for sprite benchmark mode to maximize throughput.
      -- Keep Retina/native drawable scaling enabled.
      msaaEnabled := displayMode != 4
      FFI.Renderer.setMSAAEnabled c.ctx.renderer msaaEnabled
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
        -- Normal demo mode: grid of demos using CanvasM for proper state threading
        c ← CanvasM.run' c' do
          -- Cell 0,0: Shapes demo (top-left)
          let cellRect00 := Rect.mk' 0 0 cellWidth cellHeight
          CanvasM.clip cellRect00
          CanvasM.setFillColor bg00
          CanvasM.fillRect cellRect00
          CanvasM.setFillColor (Color.rgba 1.0 1.0 1.0 0.5)
          CanvasM.fillTextXY "Cell: 0,0 - Shapes" 10 20 fontSmall
          CanvasM.save
          CanvasM.scale 0.45 0.45
          renderShapesM
          CanvasM.restore
          CanvasM.unclip

          -- Cell 1,0: Transforms demo (top-right)
          let cellRect10 := Rect.mk' cellWidth 0 cellWidth cellHeight
          CanvasM.clip cellRect10
          CanvasM.setFillColor bg10
          CanvasM.fillRect cellRect10
          CanvasM.setFillColor (Color.rgba 1.0 1.0 1.0 0.5)
          CanvasM.fillTextXY "Cell: 1,0 - Transforms" (cellWidth + 10) 20 fontSmall
          CanvasM.save
          CanvasM.translate 960 0
          CanvasM.scale 0.6 0.6
          renderTransformsM
          CanvasM.restore
          CanvasM.unclip

          -- Cell 0,1: Strokes demo (middle-left)
          let cellRect01 := Rect.mk' 0 cellHeight cellWidth cellHeight
          CanvasM.clip cellRect01
          CanvasM.setFillColor bg01
          CanvasM.fillRect cellRect01
          CanvasM.setFillColor (Color.rgba 1.0 1.0 1.0 0.5)
          CanvasM.fillTextXY "Cell: 0,1 - Strokes" 10 (cellHeight + 20) fontSmall
          CanvasM.save
          CanvasM.translate 0 360
          CanvasM.scale 0.51 0.51
          renderStrokesM
          CanvasM.restore
          CanvasM.unclip

          -- Cell 1,1: Gradients demo (middle-right)
          let cellRect11 := Rect.mk' cellWidth cellHeight cellWidth cellHeight
          CanvasM.clip cellRect11
          CanvasM.setFillColor bg11
          CanvasM.fillRect cellRect11
          CanvasM.setFillColor (Color.rgba 1.0 1.0 1.0 0.5)
          CanvasM.fillTextXY "Cell: 1,1 - Gradients" (cellWidth + 10) (cellHeight + 20) fontSmall
          CanvasM.save
          CanvasM.translate 960 360
          CanvasM.scale 0.51 0.51
          renderGradientsM
          CanvasM.restore
          CanvasM.unclip

          -- Cell 0,2: Text demo (bottom-left)
          let cellRect02 := Rect.mk' 0 (cellHeight * 2) cellWidth cellHeight
          CanvasM.clip cellRect02
          CanvasM.setFillColor bg02
          CanvasM.fillRect cellRect02
          CanvasM.setFillColor (Color.rgba 1.0 1.0 1.0 0.5)
          CanvasM.fillTextXY "Cell: 0,2 - Text" 10 (cellHeight * 2 + 20) fontSmall
          CanvasM.save
          CanvasM.translate 0 720
          CanvasM.scale 0.51 0.51
          renderTextM fonts
          CanvasM.restore
          CanvasM.unclip

          -- Cell 1,2: Animations demo (bottom-right) - THE DISCO PARTY!
          let cellRect12 := Rect.mk' cellWidth (cellHeight * 2) cellWidth cellHeight
          CanvasM.clip cellRect12
          -- Animated background color
          let bgHue := (t * 0.1) - (t * 0.1).floor
          let bg12 := hsvToRgb bgHue 0.3 0.15
          CanvasM.setFillColor bg12
          CanvasM.fillRect cellRect12
          CanvasM.setFillColor (Color.rgba 1.0 1.0 1.0 0.7)
          CanvasM.fillTextXY "Cell: 1,2 - DISCO PARTY!" (cellWidth + 10) (cellHeight * 2 + 20) fontSmall
          CanvasM.save
          CanvasM.translate cellWidth (cellHeight * 2 + 30)
          renderAnimationsM t
          CanvasM.restore
          CanvasM.unclip

      -- Render FPS counter in top-right corner (after all other rendering)
      let fpsText := s!"{displayFps.toUInt32} FPS"
      let (textWidth, _) ← fontSmall.measureText fpsText
      let c' := c.resetTransform
      let c' := c'.setFillColor (Color.rgba 0.0 0.0 0.0 0.6)
      let c' ← c'.fillRectXYWH (1920.0 - textWidth - 20.0) 5.0 (textWidth + 15.0) 25.0
      let c' := c'.setFillColor Color.white
      let c' ← c'.fillTextXY fpsText (1920.0 - textWidth - 12.0) 22.0 fontSmall

      c ← c'.endFrame

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

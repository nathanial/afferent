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
import Demos.Grid
import Demos.Collimator
import Demos.GridPerf
import Demos.TrianglesPerf
import Demos.CirclesPerf
import Demos.SpritesPerf
import Demos.Widgets
import Demos.Interactive
import Demos.SpinningCubes
import Demos.DemoGrid
import Demos.Seascape

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

  -- Pre-compute particle data ONCE at startup using unified Dynamic module
  -- Sizes scaled for physical resolution
  let halfSize := 1.5 * screenScale
  let circleRadius := 2.0 * screenScale
  let spriteHalfSize := 15.0 * screenScale  -- Size for sprite rendering

  -- Physical dimensions as floats
  let physWidthF := baseWidth * screenScale
  let physHeightF := baseHeight * screenScale

  -- Precompute layout-demo fit so mode 5 can scale uniformly and keep text readable.
  let layoutW : Float := 1000.0
  let layoutH : Float := 800.0
  let layoutPadTop : Float := 60.0 * screenScale
  let layoutAvailW : Float := physWidthF
  let layoutAvailH : Float := max 1.0 (physHeightF - layoutPadTop)
  let layoutScale : Float := min (layoutAvailW / layoutW) (layoutAvailH / layoutH)
  let layoutOffsetX : Float := (layoutAvailW - layoutW * layoutScale) / 2.0
  let layoutOffsetY : Float := layoutPadTop + (layoutAvailH - layoutH * layoutScale) / 2.0

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

  -- Layout demo labels are drawn in screen pixels (not scaled with the demo), so size is stable.
  let layoutLabelPt : Float := 12.0
  let layoutFontPx : UInt32 := (max 8.0 (layoutLabelPt * screenScale)).toUInt32
  let layoutFont ← Font.load "/System/Library/Fonts/Monaco.ttf" layoutFontPx

  -- Create interactive demo runner
  let interactiveRunner ← Widget.AppRunner.create (counterApp fontMedium)

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
  -- Interactive demo debug state (persist last event so it stays visible)
  let mut lastClick : Option FFI.ClickEvent := none
  let mut lastClickHit : Option Widget.WidgetId := none
  let mut lastHoverHit : Option Widget.WidgetId := none
  let mut lastInteractiveMsgs : Array CounterMsg := #[]
  -- FPS camera for 3D demo (mode 9)
  let mut fpsCamera : Render.FPSCamera := default
  -- Seascape camera (mode 10)
  let mut seascapeCamera : Render.FPSCamera := Demos.seascapeCamera

  while !(← c.shouldClose) do
    c.pollEvents

    -- Check for Space key (key code 49) to cycle through modes
    let keyCode ← c.getKeyCode
    if keyCode == 49 then  -- Space bar
      -- Release pointer lock when leaving mode 9 or 10
      if displayMode == 9 || displayMode == 10 then
        FFI.Window.setPointerLock c.ctx.window false
      displayMode := (displayMode + 1) % 11
      c.clearKey
      -- Disable MSAA for throughput-heavy benchmarks and the seascape demo.
      -- (Seascape is usually fill-rate bound; MSAA can be a big hit at Retina resolutions.)
      msaaEnabled := displayMode != 4 && displayMode != 10
      FFI.Renderer.setMSAAEnabled c.ctx.renderer msaaEnabled
      match displayMode with
      | 0 => IO.println "Switched to DEMO mode"
      | 1 => IO.println "Switched to GRID (squares) performance test"
      | 2 => IO.println "Switched to TRIANGLES performance test"
      | 3 => IO.println "Switched to CIRCLES (bouncing) performance test"
      | 4 => IO.println "Switched to SPRITES (Bunnymark) performance test"
      | 5 => IO.println "Switched to LAYOUT demo (full-size)"
      | 6 => IO.println "Switched to CSS GRID demo (full-size)"
      | 7 => IO.println "Switched to WIDGET demo (full-size)"
      | 8 => IO.println "Switched to INTERACTIVE demo (click the buttons!)"
      | 9 => IO.println "Switched to 3D SPINNING CUBES demo"
      | _ => IO.println "Switched to SEASCAPE demo (Gerstner waves)"

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
          translate layoutOffsetX layoutOffsetY
          scale layoutScale layoutScale
          renderLayoutShapesM
          restore
          -- Draw labels in screen space to avoid texture upscaling artifacts.
          renderLayoutLabelsMappedM layoutFont layoutOffsetX layoutOffsetY layoutScale
          setFillColor Color.white
          fillTextXY "CSS Flexbox Layout Demo (Space to advance)" (20 * screenScale) (30 * screenScale) fontMedium
      else if displayMode == 6 then
        -- Full-size CSS Grid demo
        c ← run' (c.resetTransform) do
          save
          translate layoutOffsetX layoutOffsetY
          scale layoutScale layoutScale
          renderGridShapesM
          restore
          -- Draw labels in screen space to avoid texture upscaling artifacts.
          renderGridLabelsMappedM layoutFont layoutOffsetX layoutOffsetY layoutScale
          setFillColor Color.white
          fillTextXY "CSS Grid Layout Demo (Space to advance)" (20 * screenScale) (30 * screenScale) fontMedium
      else if displayMode == 7 then
        -- Widget system demo
        c ← run' (c.resetTransform) do
          renderWidgetShapesDebugM fontMedium fontSmall physWidthF physHeightF screenScale
          setFillColor Color.white
          fillTextXY "Widget System Demo (Space to advance)" (20 * screenScale) (30 * screenScale) fontMedium
      else if displayMode == 8 then
        -- Interactive demo with event handling
        -- Collect input for this frame
        let input ← Widget.InputState.collect c.ctx.window
        -- Process events (clicks, etc.). This re-prepares layout as needed so multiple clicks
        -- in one frame stay aligned with the updated UI.
        let msgs ← interactiveRunner.processInput physWidthF physHeightF input
        lastInteractiveMsgs := msgs

        -- Capture debug info before clearing input (use latest view/layout)
        let interactiveAfter ← interactiveRunner.getView
        let preparedAfter ← Widget.prepareUI interactiveAfter.widget physWidthF physHeightF
        lastHoverHit := Widget.hitTestId preparedAfter.widget preparedAfter.layoutResult input.mousePos.1 input.mousePos.2
        if let some ce := input.clicks.back? then
          lastClick := some ce
          lastClickHit := Widget.hitTestId preparedAfter.widget preparedAfter.layoutResult ce.x ce.y
          let hitStr := match lastClickHit with
            | some wid => toString wid
            | none => "none"
          IO.println s!"[interactive] clicks={input.clicks.size} btn={ce.button} x={ce.x} y={ce.y} hit={hitStr} msgs={toString (repr msgs)}"
        -- Clear consumed input
        Widget.InputState.clear c.ctx.window

        -- Render
        c ← run' (c.resetTransform) do
          Widget.renderPreparedUI preparedAfter
          setFillColor Color.white
          fillTextXY "Interactive Demo - Click the buttons! (Space to advance)" (20 * screenScale) (30 * screenScale) fontMedium

          -- Debug overlay (top-left)
          let showOptNat : Option Nat → String
            | some n => toString n
            | none => "none"

          let clickStr :=
            match lastClick with
            | some ce => s!"click: btn={ce.button} x={ce.x.toUInt32} y={ce.y.toUInt32} mods={ce.modifiers}"
            | none => "click: none"

          let hoverStr := s!"hover hit: {showOptNat lastHoverHit}"
          let clickHitStr := s!"click hit: {showOptNat lastClickHit}"
          let mouseStr := s!"mouse: x={input.mousePos.1.toUInt32} y={input.mousePos.2.toUInt32} buttons={input.mouseButtons} inWindow={input.mouseInWindow}"
          let clickCountStr := s!"clicks: {input.clicks.size}"
          let msgStr := s!"msgs: {toString (repr lastInteractiveMsgs)}"

          setFillColor (Color.hsva 0.0 0.0 0.0 0.6)
          fillRectXYWH (10 * screenScale) (40 * screenScale) (physWidthF - 20 * screenScale) (130 * screenScale)
          setFillColor Color.white
          fillTextXY mouseStr (20 * screenScale) (65 * screenScale) fontSmall
          fillTextXY clickStr (20 * screenScale) (85 * screenScale) fontSmall
          fillTextXY hoverStr (20 * screenScale) (105 * screenScale) fontSmall
          fillTextXY clickHitStr (20 * screenScale) (125 * screenScale) fontSmall
          fillTextXY clickCountStr (20 * screenScale) (145 * screenScale) fontSmall
          fillTextXY msgStr (20 * screenScale) (165 * screenScale) fontSmall
      else if displayMode == 9 then
        -- 3D Spinning Cubes demo with FPS camera controls
        -- Pointer lock controls:
        -- - Escape toggles capture/release
        -- - Left click captures (when not captured)
        let mut locked ← FFI.Window.getPointerLock c.ctx.window
        if keyCode == 53 then  -- Escape
          FFI.Window.setPointerLock c.ctx.window (!locked)
          locked := !locked
          c.clearKey
        else if !locked then
          -- Convenience: click to capture mouse so users don't have to discover Escape first.
          let click ← FFI.Window.getClick c.ctx.window
          match click with
          | some ce =>
            FFI.Window.clearClick c.ctx.window
            if ce.button == 0 then
              FFI.Window.setPointerLock c.ctx.window true
              locked := true
          | none => pure ()

        -- Check movement keys (WASD + Q/E) using continuous key state.
        -- Movement works even when not pointer-locked; mouse-look only when locked.
        let wDown ← FFI.Window.isKeyDown c.ctx.window 13  -- W
        let aDown ← FFI.Window.isKeyDown c.ctx.window 0   -- A
        let sDown ← FFI.Window.isKeyDown c.ctx.window 1   -- S
        let dDown ← FFI.Window.isKeyDown c.ctx.window 2   -- D
        let qDown ← FFI.Window.isKeyDown c.ctx.window 12  -- Q
        let eDown ← FFI.Window.isKeyDown c.ctx.window 14  -- E

        let (dx, dy) ←
          if locked then
            FFI.Window.getMouseDelta c.ctx.window
          else
            pure (0.0, 0.0)

        -- Update camera (forward, back, left, right, up, down)
        fpsCamera := fpsCamera.update dt wDown sDown aDown dDown eDown qDown dx dy

        -- Render with camera.
        renderSpinningCubesWithCamera c.ctx.renderer t physWidthF physHeightF fpsCamera
        c ← run' (c.resetTransform) do
          setFillColor Color.white
          if locked then
            fillTextXY "3D Spinning Cubes - WASD+Q/E to move, mouse to look, Escape to release (Space to advance)" (20 * screenScale) (30 * screenScale) fontMedium
          else
            fillTextXY "3D Spinning Cubes - WASD+Q/E to move, click or Escape to capture mouse (Space to advance)" (20 * screenScale) (30 * screenScale) fontMedium

          -- Debug line (helps confirm input is arriving)
          fillTextXY s!"lock={locked} dt={dt} w={wDown} a={aDown} s={sDown} d={dDown} q={qDown} e={eDown} dx={dx} dy={dy} pos=({fpsCamera.x},{fpsCamera.y},{fpsCamera.z}) yaw={fpsCamera.yaw} pitch={fpsCamera.pitch}" (20 * screenScale) (55 * screenScale) fontSmall
      else if displayMode == 10 then
        -- Seascape demo with Gerstner waves and FPS camera
        let mut locked ← FFI.Window.getPointerLock c.ctx.window
        if keyCode == 53 then  -- Escape
          FFI.Window.setPointerLock c.ctx.window (!locked)
          locked := !locked
          c.clearKey
        else if !locked then
          let click ← FFI.Window.getClick c.ctx.window
          match click with
          | some ce =>
            FFI.Window.clearClick c.ctx.window
            if ce.button == 0 then
              FFI.Window.setPointerLock c.ctx.window true
              locked := true
          | none => pure ()

        -- Check movement keys (WASD + Q/E)
        let wDown ← FFI.Window.isKeyDown c.ctx.window 13  -- W
        let aDown ← FFI.Window.isKeyDown c.ctx.window 0   -- A
        let sDown ← FFI.Window.isKeyDown c.ctx.window 1   -- S
        let dDown ← FFI.Window.isKeyDown c.ctx.window 2   -- D
        let qDown ← FFI.Window.isKeyDown c.ctx.window 12  -- Q
        let eDown ← FFI.Window.isKeyDown c.ctx.window 14  -- E

        let (dx, dy) ←
          if locked then
            FFI.Window.getMouseDelta c.ctx.window
          else
            pure (0.0, 0.0)

        -- Update seascape camera
        seascapeCamera := seascapeCamera.update dt wDown sDown aDown dDown eDown qDown dx dy

        -- Render seascape
        renderSeascape c.ctx.renderer t physWidthF physHeightF seascapeCamera
        c ← run' (c.resetTransform) do
          setFillColor Color.white
          if locked then
            fillTextXY "Seascape - WASD+Q/E to move, mouse to look, Escape to release (Space to advance)" (20 * screenScale) (30 * screenScale) fontMedium
          else
            fillTextXY "Seascape - WASD+Q/E to move, click or Escape to capture mouse (Space to advance)" (20 * screenScale) (30 * screenScale) fontMedium
      else
        -- Normal demo mode: grid of demos using CanvasM for proper state threading
        c ← run' (c.resetTransform) do
          renderDemoGridM screenScale cellWidth cellHeight fontSmall fonts t

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
  layoutFont.destroy
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

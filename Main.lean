/-
  Afferent UI Demo
  Demonstrates the immediate-mode widget framework.
-/
import Afferent

open Afferent
open Afferent.UI

def main : IO Unit := do
  IO.println "Afferent UI Demo"
  IO.println "================"

  -- Create drawing context (includes window, renderer)
  let ctx ← DrawContext.create 800 600 "Afferent UI Demo"

  -- Load a font
  let font ← Font.load "/System/Library/Fonts/Helvetica.ttc" 16

  -- Application state
  let mut counter : Nat := 0
  let mut checkEnabled := false
  let mut checkVerbose := true
  let mut userName := "World"
  let mut volume : Float := 0.5
  let mut brightness : Float := 0.75

  IO.println "Running UI demo... (close window to exit)"

  while !(← ctx.shouldClose) do
    -- Begin input frame (reset per-frame signals)
    ctx.window.newFrame
    ctx.pollEvents

    -- Query input state
    let input ← InputState.query ctx.window

    let ok ← ctx.beginFrame Color.darkGray
    if ok then
      -- Create/update UI context
      let mut ui := UIContext.create ctx input font

      -- Title
      ui ← label ui "Immediate Mode UI Demo" ⟨50, 40⟩

      -- Button row
      let (clicked, ui') ← button ui "Click Me!" (Rect.mk' 50 70 120 35)
      ui := ui'
      if clicked then
        counter := counter + 1

      let (resetClicked, ui') ← button ui "Reset" (Rect.mk' 180 70 80 35)
      ui := ui'
      if resetClicked then
        counter := 0

      ui ← label ui s!"Counter: {counter}" ⟨280, 95⟩

      -- Checkboxes
      ui ← label ui "Settings:" ⟨50, 140⟩

      let (newEnabled, ui') ← checkboxLabeled ui "Enable feature" checkEnabled ⟨50, 160⟩
      ui := ui'
      checkEnabled := newEnabled

      let (newVerbose, ui') ← checkboxLabeled ui "Verbose logging" checkVerbose ⟨50, 190⟩
      ui := ui'
      checkVerbose := newVerbose

      -- Status text based on checkboxes
      let status := if checkEnabled then
        if checkVerbose then "Feature ON (verbose)" else "Feature ON"
      else "Feature OFF"
      ui ← labelColored ui status ⟨220, 180⟩
        (if checkEnabled then Color.green else Color.gray 0.5)

      -- Text input
      ui ← label ui "Your name:" ⟨50, 250⟩
      let (newName, ui') ← textBox ui "name" userName (Rect.mk' 140 230 200 30)
      ui := ui'
      userName := newName

      ui ← label ui s!"Hello, {userName}!" ⟨360, 255⟩

      -- Sliders
      ui ← label ui "Volume:" ⟨50, 310⟩
      let (newVol, ui') ← slider ui "volume" volume 0.0 1.0 (Rect.mk' 120 300 200 30)
      ui := ui'
      volume := newVol
      ui ← label ui s!"{(volume * 100).toUInt32}%" ⟨340, 320⟩

      ui ← label ui "Brightness:" ⟨50, 360⟩
      let (newBright, ui') ← slider ui "brightness" brightness 0.0 1.0 (Rect.mk' 140 350 200 30)
      ui := ui'
      brightness := newBright
      ui ← label ui s!"{(brightness * 100).toUInt32}%" ⟨360, 370⟩

      -- Visual feedback for brightness
      let previewRect := Rect.mk' 50 410 300 50
      let gray := brightness * 0.8 + 0.1
      ctx.fillRoundedRect previewRect 8.0 (Color.rgb gray gray gray)
      ui ← label ui "Brightness preview" ⟨130, 445⟩

      -- Mouse position display
      ui ← labelColored ui s!"Mouse: ({input.mouseX.toUInt32}, {input.mouseY.toUInt32})" ⟨50, 500⟩ (Color.gray 0.6)

      -- Instructions
      let _ ← labelColored ui "Click buttons, toggle checkboxes, edit text, drag sliders!" ⟨50, 540⟩ (Color.gray 0.5)

      ctx.endFrame

  IO.println "Cleaning up..."
  ctx.destroy

  IO.println "Done!"

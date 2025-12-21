/-
  Afferent Widget Backend
  Implementation of Arbor rendering using Afferent's CanvasM.
  Converts abstract RenderCommands to Metal-backed drawing calls.
-/
import Afferent.Canvas.Context
import Afferent.Text.Font
import Afferent.Text.Measurer
import Arbor

namespace Afferent.Widget

open Afferent
open Arbor

/-- Convert Arbor Rect to Afferent Rect. -/
def toAfferentRect (r : Arbor.Rect) : Afferent.Rect :=
  Afferent.Rect.mk' r.origin.x r.origin.y r.size.width r.size.height

/-- Convert Arbor Color to Afferent Color.
    Arbor uses Tincture.Color which is the same as Afferent's Color. -/
def toAfferentColor (c : Arbor.Color) : Afferent.Color := c

/-- Execute a single RenderCommand using CanvasM.
    Requires a FontRegistry to resolve FontIds to Font handles. -/
def executeCommand (reg : FontRegistry) (cmd : Arbor.RenderCommand) : CanvasM Unit := do
  match cmd with
  | .fillRect rect color cornerRadius =>
    let afferentRect := toAfferentRect rect
    if cornerRadius > 0 then
      CanvasM.setFillColor (toAfferentColor color)
      CanvasM.fillRoundedRect afferentRect cornerRadius
    else
      CanvasM.setFillColor (toAfferentColor color)
      CanvasM.fillRect afferentRect

  | .strokeRect rect color lineWidth cornerRadius =>
    let afferentRect := toAfferentRect rect
    CanvasM.setStrokeColor (toAfferentColor color)
    CanvasM.setLineWidth lineWidth
    if cornerRadius > 0 then
      CanvasM.strokeRoundedRect afferentRect cornerRadius
    else
      CanvasM.strokeRect afferentRect

  | .fillText text x y fontId color =>
    match reg.get fontId with
    | some font =>
      CanvasM.fillTextColor text ⟨x, y⟩ font (toAfferentColor color)
    | none =>
      -- Font not found, skip rendering
      pure ()

  | .fillTextBlock text rect fontId color align valign =>
    match reg.get fontId with
    | some font =>
      -- Measure text to calculate alignment
      let (textWidth, textHeight) ← CanvasM.measureText text font
      let x := match align with
        | .left => rect.origin.x
        | .center => rect.origin.x + (rect.size.width - textWidth) / 2
        | .right => rect.origin.x + rect.size.width - textWidth
      let y := match valign with
        | .top => rect.origin.y + font.ascender
        | .middle => rect.origin.y + (rect.size.height - textHeight) / 2 + font.ascender
        | .bottom => rect.origin.y + rect.size.height - font.descender
      CanvasM.fillTextColor text ⟨x, y⟩ font (toAfferentColor color)
    | none =>
      pure ()

  | .pushClip rect =>
    let afferentRect := toAfferentRect rect
    CanvasM.clip afferentRect

  | .popClip =>
    CanvasM.unclip

  | .pushTranslate dx dy =>
    CanvasM.translate dx dy

  | .popTransform =>
    CanvasM.restore

  | .save =>
    CanvasM.save

  | .restore =>
    CanvasM.restore

/-- Execute an array of RenderCommands using CanvasM. -/
def executeCommands (reg : FontRegistry) (cmds : Array Arbor.RenderCommand) : CanvasM Unit := do
  for cmd in cmds do
    executeCommand reg cmd

/-- Render an Arbor widget tree using CanvasM.
    This is the main entry point for rendering Arbor widgets with Afferent's Metal backend.

    Steps:
    1. Measure the widget tree (computes text layouts)
    2. Compute layout using Trellis
    3. Collect render commands
    4. Execute commands using CanvasM -/
def renderArborWidget (reg : FontRegistry) (widget : Arbor.Widget)
    (availWidth availHeight : Float) : CanvasM Unit := do
  -- Measure widget and get layout nodes
  let measureResult ← runWithFonts reg (Arbor.measureWidget widget availWidth availHeight)
  let layoutNode := measureResult.node
  let measuredWidget := measureResult.widget

  -- Compute layout
  let layouts := Trellis.layout layoutNode availWidth availHeight

  -- Collect render commands
  let commands := Arbor.collectCommands measuredWidget layouts

  -- Execute commands
  executeCommands reg commands

/-- Convenience function to render a widget built with Arbor's DSL.
    Takes a WidgetBuilder and executes the full render pipeline. -/
def renderArborBuilder (reg : FontRegistry) (builder : Arbor.WidgetBuilder)
    (availWidth availHeight : Float) : CanvasM Unit := do
  let widget := Arbor.build builder
  renderArborWidget reg widget availWidth availHeight

end Afferent.Widget

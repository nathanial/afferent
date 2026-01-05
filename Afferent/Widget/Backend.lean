/-
  Afferent Widget Backend
  Implementation of Arbor rendering using Afferent's CanvasM.
  Converts abstract RenderCommands to Metal-backed drawing calls.
-/
import Afferent.Canvas.Context
import Afferent.Core.Path
import Afferent.Text.Font
import Afferent.Text.Measurer
import Afferent.Arbor
import Afferent.Arbor.Core.Path

namespace Afferent.Widget

open Afferent
open Afferent.Arbor

/-- Convert Arbor Rect to Afferent Rect. -/
def toAfferentRect (r : Afferent.Arbor.Rect) : Afferent.Rect :=
  Afferent.Rect.mk' r.origin.x r.origin.y r.size.width r.size.height

/-- Convert Arbor Point to Afferent Point. -/
def toAfferentPoint (p : Afferent.Arbor.Point) : Afferent.Point :=
  Afferent.Point.mk' p.x p.y

/-- Convert a polygon to an Afferent Path. -/
def polygonToPath (points : Array Afferent.Arbor.Point) : Afferent.Path :=
  Id.run do
    if points.size > 0 then
      let first := points[0]!
      let mut path := (Afferent.Path.empty).moveTo (toAfferentPoint first)
      for i in [1:points.size] do
        let p := points[i]!
        path := path.lineTo (toAfferentPoint p)
      return path.closePath
    else
      return Afferent.Path.empty

/-- Convert Arbor Color to Afferent Color.
    Arbor uses Tincture.Color which is the same as Afferent's Color. -/
def toAfferentColor (c : Afferent.Arbor.Color) : Afferent.Color := c

/-- Convert Arbor FillRule to Afferent FillRule. -/
def toAfferentFillRule (rule : Afferent.Arbor.FillRule) : Afferent.FillRule :=
  match rule with
  | .nonZero => .nonZero
  | .evenOdd => .evenOdd

/-- Convert Arbor Path to Afferent Path. -/
def toAfferentPath (path : Afferent.Arbor.Path) : Afferent.Path :=
  let base := Afferent.Path.empty
  let built := path.commands.foldl (init := base) fun acc cmd =>
    match cmd with
    | .moveTo p =>
      acc.moveTo (toAfferentPoint p)
    | .lineTo p =>
      acc.lineTo (toAfferentPoint p)
    | .quadraticCurveTo cp p =>
      acc.quadraticCurveTo (toAfferentPoint cp) (toAfferentPoint p)
    | .bezierCurveTo cp1 cp2 p =>
      acc.bezierCurveTo (toAfferentPoint cp1) (toAfferentPoint cp2) (toAfferentPoint p)
    | .arcTo p1 p2 radius =>
      acc.arcTo (toAfferentPoint p1) (toAfferentPoint p2) radius
    | .arc center radius startAngle endAngle counterclockwise =>
      acc.arc (toAfferentPoint center) radius startAngle endAngle counterclockwise
    | .rect r =>
      acc.rect (toAfferentRect r)
    | .closePath =>
      acc.closePath
  built.withFillRule (toAfferentFillRule path.fillRule)

/-- Execute a single RenderCommand using CanvasM.
    Requires a FontRegistry to resolve FontIds to Font handles. -/
def executeCommand (reg : FontRegistry) (cmd : Afferent.Arbor.RenderCommand) : CanvasM Unit := do
  match cmd with
  | .fillRect rect color cornerRadius =>
    let afferentRect := toAfferentRect rect
    if cornerRadius > 0 then
      CanvasM.setFillColor (toAfferentColor color)
      CanvasM.fillRoundedRect afferentRect cornerRadius
    else
      CanvasM.setFillColor (toAfferentColor color)
      CanvasM.fillRect afferentRect

  | .fillRectStyle rect style cornerRadius =>
    let afferentRect := toAfferentRect rect
    CanvasM.save
    CanvasM.setFillStyle style
    if cornerRadius > 0 then
      CanvasM.fillRoundedRect afferentRect cornerRadius
    else
      CanvasM.fillRect afferentRect
    CanvasM.restore

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

  | .fillPolygon points color =>
    if points.size >= 3 then
      let path := polygonToPath points
      CanvasM.setFillColor (toAfferentColor color)
      CanvasM.fillPath path
    else
      pure ()

  | .strokePolygon points color lineWidth =>
    if points.size >= 3 then
      let path := polygonToPath points
      CanvasM.setStrokeColor (toAfferentColor color)
      CanvasM.setLineWidth lineWidth
      CanvasM.strokePath path
    else
      pure ()

  | .fillPath path color =>
    let afferentPath := toAfferentPath path
    CanvasM.setFillColor (toAfferentColor color)
    CanvasM.fillPath afferentPath

  | .fillPathStyle path style =>
    let afferentPath := toAfferentPath path
    CanvasM.save
    CanvasM.setFillStyle style
    CanvasM.fillPath afferentPath
    CanvasM.restore

  | .strokePath path color lineWidth =>
    let afferentPath := toAfferentPath path
    CanvasM.setStrokeColor (toAfferentColor color)
    CanvasM.setLineWidth lineWidth
    CanvasM.strokePath afferentPath

  | .pushClip rect =>
    let afferentRect := toAfferentRect rect
    CanvasM.clip afferentRect

  | .popClip =>
    CanvasM.unclip

  | .pushTranslate dx dy =>
    CanvasM.save
    CanvasM.translate dx dy

  | .pushRotate angle =>
    CanvasM.save
    CanvasM.rotate angle

  | .pushScale sx sy =>
    CanvasM.save
    CanvasM.scale sx sy

  | .popTransform =>
    CanvasM.restore

  | .save =>
    CanvasM.save

  | .restore =>
    CanvasM.restore

/-- Execute an array of RenderCommands using CanvasM. -/
def executeCommands (reg : FontRegistry) (cmds : Array Afferent.Arbor.RenderCommand) : CanvasM Unit := do
  for cmd in cmds do
    executeCommand reg cmd

/-- Render an Arbor widget tree using CanvasM.
    This is the main entry point for rendering Arbor widgets with Afferent's Metal backend.

    Steps:
    1. Measure the widget tree (computes text layouts)
    2. Compute layout using Trellis
    3. Collect render commands
    4. Execute commands using CanvasM -/
def renderArborWidget (reg : FontRegistry) (widget : Afferent.Arbor.Widget)
    (availWidth availHeight : Float) : CanvasM Unit := do
  -- Measure widget and get layout nodes
  let measureResult ← runWithFonts reg (Afferent.Arbor.measureWidget widget availWidth availHeight)
  let layoutNode := measureResult.node
  let measuredWidget := measureResult.widget

  -- Compute layout
  let layouts := Trellis.layout layoutNode availWidth availHeight

  -- Collect render commands
  let commands := Afferent.Arbor.collectCommands measuredWidget layouts

  -- Execute commands
  executeCommands reg commands

/-- Convenience function to render a widget built with Arbor's DSL.
    Takes a WidgetBuilder and executes the full render pipeline. -/
def renderArborBuilder (reg : FontRegistry) (builder : Afferent.Arbor.WidgetBuilder)
    (availWidth availHeight : Float) : CanvasM Unit := do
  let widget := Afferent.Arbor.build builder
  renderArborWidget reg widget availWidth availHeight

/-- Render an Arbor widget tree centered on screen.
    Computes intrinsic size and offsets rendering to center the widget. -/
def renderArborWidgetCentered (reg : FontRegistry) (widget : Afferent.Arbor.Widget)
    (screenWidth screenHeight : Float) : CanvasM Unit := do
  -- Measure widget to get intrinsic size
  let (intrinsicWidth, intrinsicHeight) ← runWithFonts reg (Afferent.Arbor.intrinsicSize widget)

  -- Measure widget for layout
  let measureResult ← runWithFonts reg (Afferent.Arbor.measureWidget widget intrinsicWidth intrinsicHeight)
  let layoutNode := measureResult.node
  let measuredWidget := measureResult.widget

  -- Compute layout at intrinsic size
  let layouts := Trellis.layout layoutNode intrinsicWidth intrinsicHeight

  -- Calculate offset to center
  let offsetX := (screenWidth - intrinsicWidth) / 2
  let offsetY := (screenHeight - intrinsicHeight) / 2

  -- Collect render commands
  let commands := Afferent.Arbor.collectCommands measuredWidget layouts

  -- Save state, translate, render, restore
  CanvasM.save
  CanvasM.translate offsetX offsetY
  executeCommands reg commands
  CanvasM.restore

/-- Render an Arbor widget tree centered with debug borders.
    Shows colored borders around each layout cell for debugging. -/
def renderArborWidgetDebug (reg : FontRegistry) (widget : Afferent.Arbor.Widget)
    (screenWidth screenHeight : Float)
    (borderColor : Afferent.Arbor.Color := ⟨0.5, 1.0, 0.5, 0.5⟩) : CanvasM Unit := do
  -- Measure widget to get intrinsic size
  let (intrinsicWidth, intrinsicHeight) ← runWithFonts reg (Afferent.Arbor.intrinsicSize widget)

  -- Measure widget for layout
  let measureResult ← runWithFonts reg (Afferent.Arbor.measureWidget widget intrinsicWidth intrinsicHeight)
  let layoutNode := measureResult.node
  let measuredWidget := measureResult.widget

  -- Compute layout at intrinsic size
  let layouts := Trellis.layout layoutNode intrinsicWidth intrinsicHeight

  -- Calculate offset to center
  let offsetX := (screenWidth - intrinsicWidth) / 2
  let offsetY := (screenHeight - intrinsicHeight) / 2

  -- Collect render commands with debug borders
  let commands := Afferent.Arbor.collectCommandsWithDebug measuredWidget layouts borderColor

  -- Save state, translate, render, restore
  CanvasM.save
  CanvasM.translate offsetX offsetY
  executeCommands reg commands
  CanvasM.restore

end Afferent.Widget

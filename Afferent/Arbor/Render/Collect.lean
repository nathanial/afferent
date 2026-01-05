/-
  Arbor Render Command Collector
  Convert widget trees + layouts into RenderCommand arrays.
  This is the key abstraction that makes rendering backend-independent.
-/
import Afferent.Arbor.Widget.Core
import Afferent.Arbor.Render.Command
import Trellis

namespace Afferent.Arbor

/-- Render command collector state. -/
structure CollectState where
  commands : Array RenderCommand := #[]
deriving Inhabited

/-- Collector monad for accumulating render commands. -/
abbrev CollectM := StateM CollectState

namespace CollectM

/-- Emit a single render command. -/
def emit (cmd : RenderCommand) : CollectM Unit := do
  modify fun s => { s with commands := s.commands.push cmd }

/-- Emit multiple render commands. -/
def emitAll (cmds : Array RenderCommand) : CollectM Unit := do
  modify fun s => { s with commands := s.commands ++ cmds }

/-- Run the collector and return the commands. -/
def execute {α : Type} (m : CollectM α) : Array RenderCommand :=
  (StateT.run m {}).2.commands

end CollectM

/-- Collect box background and border render commands based on BoxStyle. -/
def collectBoxStyle (rect : Trellis.LayoutRect) (style : BoxStyle) : CollectM Unit := do
  let r : Rect := ⟨⟨rect.x, rect.y⟩, ⟨rect.width, rect.height⟩⟩

  -- Background
  if let some bg := style.backgroundColor then
    CollectM.emit (.fillRect r bg style.cornerRadius)

  -- Border
  if let some bc := style.borderColor then
    if style.borderWidth > 0 then
      CollectM.emit (.strokeRect r bc style.borderWidth style.cornerRadius)

/-- Collect render commands for wrapped text with alignment.
    Text is vertically centered within the content rect.
    Baseline = top + verticalOffset + ascender (where verticalOffset centers the text block). -/
def collectWrappedText (contentRect : Trellis.LayoutRect) (font : FontId)
    (color : Color) (align : TextAlign) (textLayout : TextLayout) : CollectM Unit := do
  let lineHeight := textLayout.lineHeight
  let ascender := textLayout.ascender
  -- Vertical centering: offset to center the text block within the content rect
  let verticalOffset := (contentRect.height - textLayout.totalHeight) / 2
  -- First baseline: top of content + vertical offset + ascender
  let mut y := contentRect.y + verticalOffset + ascender

  for line in textLayout.lines do
    -- Calculate x based on alignment
    let x := match align with
      | .left => contentRect.x
      | .center => contentRect.x + (contentRect.width - line.width) / 2
      | .right => contentRect.x + contentRect.width - line.width

    CollectM.emit (.fillText line.text x y font color)
    y := y + lineHeight

/-- Collect render commands for single-line text (no wrapping).
    Text is vertically centered within the content rect. -/
def collectSingleLineText (contentRect : Trellis.LayoutRect) (text : String)
    (font : FontId) (color : Color) (align : TextAlign) (textWidth : Float)
    (lineHeight : Float) : CollectM Unit := do
  -- Calculate x based on alignment
  let x := match align with
    | .left => contentRect.x
    | .center => contentRect.x + (contentRect.width - textWidth) / 2
    | .right => contentRect.x + contentRect.width - textWidth

  -- Vertical centering with estimated ascender (0.8 * lineHeight)
  let ascender := lineHeight * 0.8
  let verticalOffset := (contentRect.height - lineHeight) / 2
  CollectM.emit (.fillText text x (contentRect.y + verticalOffset + ascender) font color)

/-- Collect render commands for a widget tree using computed layout positions.
    The widget should have been measured (text layouts computed) before calling this.
    Returns an array of RenderCommands that can be executed by any backend. -/
partial def collectWidget (w : Widget) (layouts : Trellis.LayoutResult) : CollectM Unit := do
  let some computed := layouts.get w.id | return
  let borderRect := computed.borderRect
  let contentRect := computed.contentRect

  match w with
  | .rect _ _ style =>
    collectBoxStyle borderRect style

  | .text _ _ content font color align _ textLayoutOpt =>
    match textLayoutOpt with
    | some textLayout =>
      collectWrappedText contentRect font color align textLayout
    | none =>
      -- Fallback to single-line rendering with estimated dimensions
      -- (this path shouldn't normally be hit if measureWidget was called)
      collectSingleLineText contentRect content font color align contentRect.width 16.0

  | .spacer _ _ _ _ =>
    -- Spacers don't render anything
    pure ()

  | .custom _ _ style spec =>
    collectBoxStyle borderRect style
    CollectM.emitAll (spec.collect computed)

  | .flex _ _ _ style children =>
    collectBoxStyle borderRect style
    for child in children do
      collectWidget child layouts

  | .grid _ _ _ style children =>
    collectBoxStyle borderRect style
    for child in children do
      collectWidget child layouts

  | .scroll _ _ style scrollState _ _ child =>
    -- Render background
    collectBoxStyle borderRect style

    -- Set up clipping to content area
    let clipRect : Rect := ⟨⟨contentRect.x, contentRect.y⟩, ⟨contentRect.width, contentRect.height⟩⟩
    CollectM.emit (.pushClip clipRect)

    -- Save state and apply scroll offset
    CollectM.emit .save
    CollectM.emit (.pushTranslate (-scrollState.offsetX) (-scrollState.offsetY))

    -- Render child
    collectWidget child layouts

    -- Restore state
    CollectM.emit .popTransform
    CollectM.emit .restore
    CollectM.emit .popClip

/-- Collect render commands for a widget tree.
    This is the main entry point for converting a widget tree to render commands. -/
def collectCommands (w : Widget) (layouts : Trellis.LayoutResult) : Array RenderCommand :=
  CollectM.execute (collectWidget w layouts)

/-- Collect render commands with an initial save/restore wrapper. -/
def collectCommandsWithSave (w : Widget) (layouts : Trellis.LayoutResult) : Array RenderCommand :=
  CollectM.execute do
    CollectM.emit .save
    collectWidget w layouts
    CollectM.emit .restore

/-- Collect debug border commands for all layout cells.
    Draws a colored stroke rect around each widget's border rect.
    Useful for debugging layout issues. -/
partial def collectDebugBorders (w : Widget) (layouts : Trellis.LayoutResult)
    (color : Color := ⟨0.5, 1.0, 0.5, 0.5⟩) (lineWidth : Float := 1.0) : CollectM Unit := do
  let some computed := layouts.get w.id | return
  let r := computed.borderRect
  let rect : Rect := ⟨⟨r.x, r.y⟩, ⟨r.width, r.height⟩⟩
  CollectM.emit (.strokeRect rect color lineWidth 0)

  -- Recurse into children
  for child in w.children do
    collectDebugBorders child layouts color lineWidth

/-- Collect both regular widget commands and debug borders.
    Returns commands that render the widget with debug borders overlaid. -/
def collectCommandsWithDebug (w : Widget) (layouts : Trellis.LayoutResult)
    (borderColor : Color := ⟨0.5, 1.0, 0.5, 0.5⟩) : Array RenderCommand :=
  CollectM.execute do
    collectWidget w layouts
    collectDebugBorders w layouts borderColor

end Afferent.Arbor

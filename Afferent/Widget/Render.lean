/-
  Afferent Widget Render
  Render widgets using computed layout positions.
-/
import Afferent.Widget.Core
import Afferent.Widget.Measure
import Afferent.Canvas.Context

namespace Afferent.Widget

open Afferent CanvasM

/-- Render a box background and border based on BoxStyle. -/
def renderBoxStyle (rect : Layout.LayoutRect) (style : BoxStyle) : CanvasM Unit := do
  -- Background
  if let some bg := style.backgroundColor then
    setFillColor bg
    if style.cornerRadius > 0 then
      fillRoundedRect rect.toAfferentRect style.cornerRadius
    else
      fillRectXYWH rect.x rect.y rect.width rect.height

  -- Border
  if let some bc := style.borderColor then
    if style.borderWidth > 0 then
      setStrokeColor bc
      setLineWidth style.borderWidth
      if style.cornerRadius > 0 then
        strokeRoundedRect rect.toAfferentRect style.cornerRadius
      else
        strokeRectXYWH rect.x rect.y rect.width rect.height

/-- Render wrapped text with alignment. -/
def renderWrappedText (contentRect : Layout.LayoutRect) (font : Font)
    (color : Color) (align : TextAlign) (textLayout : TextLayout) : CanvasM Unit := do
  setFillColor color
  let ascender := font.ascender
  let glyphHeight := font.glyphHeight
  let lineAdvance := max font.lineHeight glyphHeight

  -- Start y at baseline of first line (rect.y + ascender)
  let mut y := contentRect.y + ascender

  for line in textLayout.lines do
    -- Calculate x based on alignment
    let x := match align with
      | .left => contentRect.x
      | .center => contentRect.x + (contentRect.width - line.width) / 2
      | .right => contentRect.x + contentRect.width - line.width

    fillTextXY line.text x y font
    y := y + lineAdvance

/-- Render a single-line text (no wrapping). -/
def renderSingleLineText (contentRect : Layout.LayoutRect) (text : String)
    (font : Font) (color : Color) (align : TextAlign) : CanvasM Unit := do
  setFillColor color
  let (textWidth, _) ← measureText text font
  let ascender := font.ascender

  -- Calculate x based on alignment
  let x := match align with
    | .left => contentRect.x
    | .center => contentRect.x + (contentRect.width - textWidth) / 2
    | .right => contentRect.x + contentRect.width - textWidth

  -- y at baseline (rect.y + ascender)
  let y := contentRect.y + ascender

  fillTextXY text x y font

/-- Render a widget tree using computed layout positions.
    The widget should have been measured (text layouts computed) before calling this. -/
partial def renderWidget (w : Widget) (layouts : Layout.LayoutResult) : CanvasM Unit := do
  let some computed := layouts.get w.id | return
  let borderRect := computed.borderRect
  let contentRect := computed.contentRect

  match w with
  | .rect _ style =>
    renderBoxStyle borderRect style

  | .text _ content font color align _ textLayoutOpt =>
    match textLayoutOpt with
    | some textLayout =>
      renderWrappedText contentRect font color align textLayout
    | none =>
      -- Fallback to single-line rendering
      renderSingleLineText contentRect content font color align

  | .spacer _ _ _ =>
    -- Spacers don't render anything
    pure ()

  | .flex _ _ style children =>
    renderBoxStyle borderRect style
    for child in children do
      renderWidget child layouts

  | .grid _ _ style children =>
    renderBoxStyle borderRect style
    for child in children do
      renderWidget child layouts

  | .scroll _ style scrollState _ _ child =>
    -- Render background
    renderBoxStyle borderRect style

    -- Set up clipping to content area
    clip contentRect.toAfferentRect

    -- Save transform state
    save

    -- Apply scroll offset (negative because scrolling down means content moves up)
    translate (-scrollState.offsetX) (-scrollState.offsetY)

    -- Render child
    renderWidget child layouts

    -- Restore transform
    restore

    -- Remove clipping
    unclip

end Afferent.Widget

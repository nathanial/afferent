/-
  Afferent Widget UI
  High-level widget rendering API.
-/
import Afferent.Widget.Core
import Afferent.Widget.Measure
import Afferent.Widget.Render
import Afferent.Widget.DSL

namespace Afferent.Widget

open Afferent CanvasM

/-- Result of preparing a widget for rendering.
    Contains the measured widget and layout result. -/
structure PreparedUI where
  widget : Widget
  layoutResult : Layout.LayoutResult
deriving Inhabited

/-- Prepare a widget for rendering by measuring and computing layout.
    This is the first phase of the render pipeline. -/
def prepareUI (widget : Widget) (availWidth availHeight : Float) : IO PreparedUI := do
  -- Phase 1: Measure widgets and convert to LayoutNode tree
  let measureResult ← measureWidget widget availWidth availHeight
  -- Phase 2: Run layout algorithm
  let layoutResult := Layout.layout measureResult.node availWidth availHeight
  pure { widget := measureResult.widget, layoutResult }

/-- Render a prepared UI.
    This is the second phase of the render pipeline. -/
def renderPreparedUI (prepared : PreparedUI) : CanvasM Unit :=
  renderWidget prepared.widget prepared.layoutResult

/-- Complete widget rendering pipeline in a single call.
    Measures the widget, computes layout, and renders.

    Parameters:
    - widget: The widget tree to render
    - availWidth: Available width for layout (typically canvas width)
    - availHeight: Available height for layout (typically canvas height)
-/
def renderUI (widget : Widget) (availWidth availHeight : Float) : CanvasM Unit := do
  let prepared ← prepareUI widget availWidth availHeight
  renderPreparedUI prepared

/-- Render a widget builder directly.
    Convenience function that builds and renders in one step. -/
def renderBuilder (builder : WidgetBuilder) (availWidth availHeight : Float) : CanvasM Unit := do
  let widget := build builder
  renderUI widget availWidth availHeight

/-- Render a widget at a specific offset position.
    Useful for rendering widgets at non-origin positions. -/
def renderUIAt (widget : Widget) (x y : Float) (availWidth availHeight : Float) : CanvasM Unit := do
  save
  translate x y
  renderUI widget availWidth availHeight
  restore

/-- Render a widget centered in the available space.
    The widget is measured at the available size, then centered. -/
def renderUICentered (widget : Widget) (availWidth availHeight : Float) : CanvasM Unit := do
  let prepared ← prepareUI widget availWidth availHeight

  -- Find the root widget's bounds to center it
  if let some rootLayout := prepared.layoutResult.get widget.id then
    let widgetWidth := rootLayout.borderRect.width
    let widgetHeight := rootLayout.borderRect.height
    let offsetX := (availWidth - widgetWidth) / 2
    let offsetY := (availHeight - widgetHeight) / 2

    save
    translate offsetX offsetY
    renderPreparedUI prepared
    restore
  else
    -- Fallback: render at origin
    renderPreparedUI prepared

/-! ## Debug Rendering -/

/-- Render widget bounds for debugging.
    Draws rectangles around each widget's border and content areas. -/
def renderUIDebug (widget : Widget) (availWidth availHeight : Float)
    (borderColor : Color := Color.red) (contentColor : Color := Color.blue) : CanvasM Unit := do
  let prepared ← prepareUI widget availWidth availHeight

  -- Render normal widgets first
  renderPreparedUI prepared

  -- Then draw debug overlays
  setLineWidth 1
  for layout in prepared.layoutResult.layouts do
    -- Border rect in one color
    setStrokeColor borderColor
    strokeRectXYWH layout.borderRect.x layout.borderRect.y layout.borderRect.width layout.borderRect.height
    -- Content rect in another color (slightly inset for visibility)
    setStrokeColor contentColor
    strokeRectXYWH layout.contentRect.x layout.contentRect.y layout.contentRect.width layout.contentRect.height

end Afferent.Widget

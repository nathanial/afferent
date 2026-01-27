/-
  Afferent UI Runner
  High-level loop for Arbor UI with event dispatch and rendering.
-/
import Afferent.Canvas.Context
import Afferent.Text.Measurer
import Afferent.Widget
import Afferent.Arbor.App.UI
import Afferent.Arbor.Widget.Measure
import Afferent.Arbor.Widget.MeasureCache
import Trellis

namespace Afferent.App

open Afferent.Arbor

inductive LayoutMode where
  | centeredIntrinsic
  | fullscreen
deriving Repr

structure UIApp (Model Msg : Type) where
  view : Model → UI Msg
  update : Msg → Model → Model
  background : Color := Color.black
  layout : LayoutMode := .centeredIntrinsic
  sendHover : Bool := true

structure LayoutInfo where
  widget : Widget
  layouts : Trellis.LayoutResult
  offsetX : Float
  offsetY : Float
  renderWidth : Float
  renderHeight : Float

private def layoutUI (reg : FontRegistry) (widget : Widget) (mode : LayoutMode)
    (screenW screenH : Float)
    (measureCache : IO.Ref MeasureCache) (intrinsicCache : IO.Ref IntrinsicCache) : IO LayoutInfo := do
  match mode with
  | .centeredIntrinsic =>
    -- Use cached intrinsic size computation (returns widget with TextLayouts)
    let intrResult ← runWithFonts reg (Afferent.Arbor.intrinsicSizeCached intrinsicCache widget)
    let intrW := intrResult.width
    let intrH := intrResult.height
    -- Use cached measurement on the widget with pre-computed TextLayouts
    let measureResult ← runWithFonts reg (Afferent.Arbor.measureWidgetCached measureCache intrResult.widget intrW intrH)
    let layouts := Trellis.layout measureResult.node intrW intrH
    let offsetX := (screenW - intrW) / 2
    let offsetY := (screenH - intrH) / 2
    pure { widget := measureResult.widget, layouts, offsetX, offsetY, renderWidth := intrW, renderHeight := intrH }
  | .fullscreen =>
    let measureResult ← runWithFonts reg (Afferent.Arbor.measureWidgetCached measureCache widget screenW screenH)
    let layouts := Trellis.layout measureResult.node screenW screenH
    pure { widget := measureResult.widget, layouts, offsetX := 0, offsetY := 0, renderWidth := screenW, renderHeight := screenH }

private def buildPointerEvents (window : FFI.Window) (offsetX offsetY : Float)
    (prevLeftDown : Bool) (sendHover : Bool) : IO (Array Event × Bool) := do
  let (mx, my) ← window.getMousePos
  let buttons ← window.getMouseButtons
  let modsBits ← window.getModifiers
  let leftDown := (buttons &&& (1 : UInt8)) != (0 : UInt8)
  let mods := Modifiers.fromBitmask modsBits
  let localX := mx - offsetX
  let localY := my - offsetY
  let mut events : Array Event := #[]
  if leftDown && !prevLeftDown then
    events := events.push (.mouseDown (MouseEvent.mk' localX localY .left mods))
  if leftDown || sendHover then
    events := events.push (.mouseMove (MouseEvent.mk' localX localY .left mods))
  if !leftDown && prevLeftDown then
    events := events.push (.mouseUp (MouseEvent.mk' localX localY .left mods))
  let (sx, sy) ← window.getScrollDelta
  if sx != 0.0 || sy != 0.0 then
    events := events.push (.scroll { x := localX, y := localY, deltaX := sx, deltaY := sy, modifiers := mods })
    window.clearScroll
  pure (events, leftDown)

def run (canvas : Canvas) (fontReg : FontRegistry) (initial : Model) (app : UIApp Model Msg) : IO Unit := do
  -- Create caches that persist across frames
  let measureCache ← IO.mkRef MeasureCache.empty
  let intrinsicCache ← IO.mkRef IntrinsicCache.empty
  let renderLoop := do
    let mut c := canvas
    let mut model := initial
    let mut capture : CaptureState := {}
    let mut prevLeftDown := false
    while !(← c.shouldClose) do
      let ok ← c.beginFrame app.background
      if ok then
        let ui := app.view model
        let (screenW, screenH) ← c.ctx.getCurrentSize
        let layoutInfo ← layoutUI fontReg ui.widget app.layout screenW screenH measureCache intrinsicCache
        let (events, leftDown) ←
          buildPointerEvents c.ctx.window layoutInfo.offsetX layoutInfo.offsetY prevLeftDown app.sendHover
        prevLeftDown := leftDown
        for ev in events do
          let (cap', msgs) := dispatchEvent ev layoutInfo.widget layoutInfo.layouts ui.handlers capture
          capture := cap'
          model := msgs.foldl (fun s m => app.update m s) model

        c ← CanvasM.run' c do
          match app.layout with
          | .centeredIntrinsic =>
            Afferent.Widget.renderArborWidgetCentered fontReg ui.widget screenW screenH
          | .fullscreen =>
            Afferent.Widget.renderArborWidget fontReg ui.widget screenW screenH
        c ← c.endFrame
  let task ← IO.asTask (prio := .dedicated) renderLoop
  canvas.ctx.window.runEventLoop
  match task.get with
  | .ok _ => pure ()
  | .error err => throw err

end Afferent.App

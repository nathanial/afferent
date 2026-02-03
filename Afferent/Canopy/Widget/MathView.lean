/-
  Canopy Interactive MathView Widgets
  Pan/zoom for 2D and orbit/zoom for 3D using standard mouse bindings.
-/
import Reactive
import Afferent.Canopy.Core
import Afferent.Canopy.Reactive.Component
import Afferent.Widget.MathView2D
import Afferent.Widget.MathView3D

namespace Afferent.Canopy

open Reactive Reactive.Host
open Afferent.Canopy.Reactive
open Afferent.Arbor hiding Event
open Afferent.Widget

/-- Interaction settings for 2D math views. -/
structure MathView2DControls where
  panButton : UInt8 := 0
  zoomSpeed : Float := 0.02
  minScale : Float := 10.0
  maxScale : Float := 400.0
  deriving Inhabited

/-- Internal pan/zoom state for interactive 2D views. -/
structure MathView2DState where
  config : MathView2D.Config
  dragging : Bool := false
  lastMouseX : Float := 0.0
  lastMouseY : Float := 0.0
  deriving Inhabited

/-- Result for interactive 2D math view. -/
structure MathView2DResult where
  config : Dyn MathView2D.Config

/-- Interactive 2D math view with pan (drag) and zoom (scroll). -/
def mathView2DInteractive (config : MathView2D.Config := {})
    (controls : MathView2DControls := {})
    (font : Afferent.Font)
    (drawContent : MathView2D.View → CanvasM Unit)
    : WidgetM MathView2DResult := do
  let name ← registerComponentW "mathview2d"

  let clickEvents ← useClickData name
  let clickUpdates ← Event.mapM (fun data =>
    fun (s : MathView2DState) =>
      if data.click.button == controls.panButton then
        { s with dragging := true, lastMouseX := data.click.x, lastMouseY := data.click.y }
      else s
    ) clickEvents

  let hoverEvents ← useAllHovers
  let hoverUpdates ← Event.mapM (fun data =>
    fun (s : MathView2DState) =>
      if s.dragging then
        let dx := data.x - s.lastMouseX
        let dy := data.y - s.lastMouseY
        let newConfig := MathView2D.pan s.config dx dy
        { s with config := newConfig, lastMouseX := data.x, lastMouseY := data.y }
      else s
    ) hoverEvents

  let mouseUpEvents ← useAllMouseUp
  let mouseUpUpdates ← Event.mapM (fun data =>
    fun (s : MathView2DState) =>
      if s.dragging && data.button == controls.panButton then
        { s with dragging := false }
      else s
    ) mouseUpEvents

  let scrollEvents ← useScroll name
  let scrollUpdates ← Event.mapM (fun data =>
    fun (s : MathView2DState) =>
      match findWidgetIdByName data.widget name with
      | some widgetId =>
          match data.layouts.get widgetId with
          | some layout =>
              let rect := layout.contentRect
              let localX := data.scroll.x - rect.x
              let localY := data.scroll.y - rect.y
              let factor := Float.exp (-data.scroll.deltaY * controls.zoomSpeed)
              let newConfig := MathView2D.zoomAt s.config rect.width rect.height (localX, localY)
                factor controls.minScale controls.maxScale
              { s with config := newConfig }
          | none => s
      | none => s
    ) scrollEvents

  let allUpdates ← Event.mergeAllListM [clickUpdates, hoverUpdates, mouseUpUpdates, scrollUpdates]
  let state ← Reactive.foldDyn (fun f s => f s) ({ config } : MathView2DState) allUpdates
  let configDyn ← Dynamic.mapM (fun s => s.config) state

  let _ ← dynWidget configDyn fun cfg => do
    emit do
      pure (MathView2D.mathView2DVisual (some name) cfg font drawContent)

  pure { config := configDyn }

/-- Interaction settings for 3D math views. -/
structure MathView3DControls where
  orbitButton : UInt8 := 0
  deriving Inhabited

/-- Internal orbit/zoom state for interactive 3D views. -/
structure MathView3DState where
  config : MathView3D.Config
  dragging : Bool := false
  lastMouseX : Float := 0.0
  lastMouseY : Float := 0.0
  deriving Inhabited

/-- Result for interactive 3D math view. -/
structure MathView3DResult where
  config : Dyn MathView3D.Config

/-- Interactive 3D math view with orbit (drag) and zoom (scroll). -/
def mathView3DInteractive (config : MathView3D.Config := {})
    (controls : MathView3DControls := {})
    (font : Afferent.Font)
    (drawContent : MathView3D.View → CanvasM Unit)
    : WidgetM MathView3DResult := do
  let name ← registerComponentW "mathview3d"

  let clickEvents ← useClickData name
  let clickUpdates ← Event.mapM (fun data =>
    fun (s : MathView3DState) =>
      if data.click.button == controls.orbitButton then
        { s with dragging := true, lastMouseX := data.click.x, lastMouseY := data.click.y }
      else s
    ) clickEvents

  let hoverEvents ← useAllHovers
  let hoverUpdates ← Event.mapM (fun data =>
    fun (s : MathView3DState) =>
      if s.dragging then
        let dx := data.x - s.lastMouseX
        let dy := data.y - s.lastMouseY
        let newConfig := MathView3D.orbitConfig s.config dx dy
        { s with config := newConfig, lastMouseX := data.x, lastMouseY := data.y }
      else s
    ) hoverEvents

  let mouseUpEvents ← useAllMouseUp
  let mouseUpUpdates ← Event.mapM (fun data =>
    fun (s : MathView3DState) =>
      if s.dragging && data.button == controls.orbitButton then
        { s with dragging := false }
      else s
    ) mouseUpEvents

  let scrollEvents ← useScroll name
  let scrollUpdates ← Event.mapM (fun data =>
    fun (s : MathView3DState) =>
      let newConfig := MathView3D.zoomConfig s.config data.scroll.deltaY
      { s with config := newConfig }
    ) scrollEvents

  let allUpdates ← Event.mergeAllListM [clickUpdates, hoverUpdates, mouseUpUpdates, scrollUpdates]
  let state ← Reactive.foldDyn (fun f s => f s) ({ config } : MathView3DState) allUpdates
  let configDyn ← Dynamic.mapM (fun s => s.config) state

  let _ ← dynWidget configDyn fun cfg => do
    emit do
      pure (MathView3D.mathView3DVisual (some name) cfg font drawContent)

  pure { config := configDyn }

end Afferent.Canopy

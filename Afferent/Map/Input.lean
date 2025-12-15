/-
  Map Input Handling (Pan and Zoom)
  Ported from heavenly-host to Afferent
  Adapted to use Afferent.FFI.Window for input
-/
import Afferent.Map.State
import Afferent.Map.Zoom
import Afferent.FFI.Window

namespace Afferent.Map

open Afferent.FFI
open Afferent.Map.Zoom (floatClamp screenToGeo)

/-- Check if left mouse button is down from button mask -/
def isLeftButtonDown (buttons : UInt8) : Bool :=
  (buttons &&& 1) != 0

/-- Handle mouse input for panning -/
def handlePanInput (window : Window) (state : MapState) : IO MapState := do
  let (mouseX, mouseY) ← Window.getMousePos window
  let buttons ← Window.getMouseButtons window
  let leftDown := isLeftButtonDown buttons

  if leftDown then
    if state.isDragging then
      -- Continue dragging - update center based on delta
      let dx := state.dragStartX - mouseX
      let dy := state.dragStartY - mouseY
      let (dLon, dLat) := state.viewport.pixelsToDegrees dx dy
      let newLat := floatClamp (state.dragStartLat - dLat) (-85.0) 85.0
      let newLon := state.dragStartLon + dLon
      -- Wrap longitude
      let newLon := if newLon > 180.0 then newLon - 360.0
                    else if newLon < -180.0 then newLon + 360.0
                    else newLon
      pure { state with
        viewport := { state.viewport with centerLat := newLat, centerLon := newLon }
      }
    else
      -- Start dragging
      pure (state.startDrag mouseX mouseY)
  else
    pure state.stopDrag

/-- Handle mouse wheel for zooming at cursor position.
    Starts zoom animation - the geographic point under the cursor stays fixed. -/
def handleZoomInput (window : Window) (state : MapState) : IO MapState := do
  let (_, wheelY) ← Window.getScrollDelta window
  if wheelY != 0.0 then
    -- `getScrollDelta` reports accumulated scroll since last clear; consume it exactly once.
    Window.clearScroll window
    let (mouseX, mouseY) ← Window.getMousePos window
    let delta := if wheelY > 0.0 then 1 else -1
    -- Accumulate: add delta to current target (not viewport.zoom)
    let newTarget := clampZoom (state.targetZoom + delta)

    if state.isAnimatingZoom then
      -- Already animating: just update target, keep existing anchor
      pure { state with targetZoom := newTarget }
    else
      -- Not animating: capture anchor point and start animation
      -- Get geographic coordinates of cursor position
      let (anchorLat, anchorLon) := screenToGeo state.viewport mouseX mouseY
      pure { state with
        targetZoom := newTarget
        isAnimatingZoom := true
        zoomAnchorScreenX := mouseX
        zoomAnchorScreenY := mouseY
        zoomAnchorLat := anchorLat
        zoomAnchorLon := anchorLon
      }
  else
    pure state

/-- Combined input handler -/
def handleInput (window : Window) (state : MapState) : IO MapState := do
  let state ← handlePanInput window state
  handleZoomInput window state

end Afferent.Map

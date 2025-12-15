/-
  Complete Map State
  Ported from heavenly-host to Afferent
-/
import Afferent.Map.TileCache
import Afferent.Map.Viewport
import Afferent.Map.Zoom
import Afferent.DiskCache.Config
import Std.Data.HashMap

namespace Afferent.Map

open Afferent.DiskCache (DiskCacheConfig DiskCacheIndex)
open Afferent.Map.Zoom (floatClamp)

/-- Complete map state -/
structure MapState where
  viewport : MapViewport
  cache : TileCache
  resultQueue : IO.Ref (Array FetchResult)  -- Shared queue for async fetch results
  frameCount : Nat := 0                      -- Abstract time counter for retry scheduling
  isDragging : Bool := false
  dragStartX : Float := 0.0
  dragStartY : Float := 0.0
  dragStartLat : Float := 0.0
  dragStartLon : Float := 0.0
  -- Zoom animation state
  targetZoom : Int                           -- User's desired zoom level
  displayZoom : Float                        -- Current animated zoom (fractional)
  zoomAnchorScreenX : Float := 0.0           -- Screen position to keep fixed
  zoomAnchorScreenY : Float := 0.0
  zoomAnchorLat : Float := 0.0               -- Geographic point to keep fixed
  zoomAnchorLon : Float := 0.0
  isAnimatingZoom : Bool := false            -- Whether animation is in progress
  -- Disk cache state
  diskCacheConfig : DiskCacheConfig := {}
  diskCacheIndex : IO.Ref DiskCacheIndex
  -- Active task cancellation flags
  activeTasks : IO.Ref (Std.HashMap TileCoord (IO.Ref Bool))

namespace MapState

/-- Initialize map state centered on a location -/
def init (lat lon : Float) (zoom : Int) (width height : Int)
    (diskConfig : DiskCacheConfig := {}) : IO MapState := do
  let queue ← IO.mkRef #[]
  let diskIndex ← IO.mkRef (DiskCacheIndex.empty diskConfig)
  let activeTasks ← IO.mkRef {}
  let clampedZoom := clampZoom zoom
  IO.println s!"[MapState.init] zoom={zoom} clampedZoom={clampedZoom} width={width} height={height}"
  pure {
    viewport := {
      centerLat := lat
      centerLon := lon
      zoom := clampedZoom
      screenWidth := width
      screenHeight := height
    }
    cache := TileCache.empty
    resultQueue := queue
    targetZoom := clampedZoom
    displayZoom := intToFloat clampedZoom
    diskCacheConfig := diskConfig
    diskCacheIndex := diskIndex
    activeTasks := activeTasks
  }

/-- Update viewport center -/
def setCenter (state : MapState) (lat lon : Float) : MapState :=
  let clampedLat := floatClamp lat (-85.0) 85.0
  let wrappedLon := if lon > 180.0 then lon - 360.0
                    else if lon < -180.0 then lon + 360.0
                    else lon
  { state with viewport := { state.viewport with
      centerLat := clampedLat
      centerLon := wrappedLon
    }
  }

/-- Update zoom level (also updates animation state) -/
def setZoom (state : MapState) (zoom : Int) : MapState :=
  let clamped := clampZoom zoom
  { state with
      viewport := { state.viewport with zoom := clamped }
      targetZoom := clamped
      displayZoom := intToFloat clamped
      isAnimatingZoom := false
  }

/-- Start dragging -/
def startDrag (state : MapState) (mouseX mouseY : Float) : MapState :=
  { state with
    isDragging := true
    dragStartX := mouseX
    dragStartY := mouseY
    dragStartLat := state.viewport.centerLat
    dragStartLon := state.viewport.centerLon
  }

/-- Stop dragging -/
def stopDrag (state : MapState) : MapState :=
  { state with isDragging := false }

end MapState

end Afferent.Map

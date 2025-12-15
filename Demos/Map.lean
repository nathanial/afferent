/-
  Map Demo - Tile-based map viewer with Web Mercator projection

  Features:
  - Pan: Click and drag to pan the map
  - Zoom: Mouse wheel to zoom (zooms toward cursor position)
  - Async tile loading with disk caching
  - Exponential backoff retry logic
  - Parent tile fallback rendering
-/
import Afferent
import Afferent.Map

namespace Demos

open Afferent Afferent.FFI Afferent.Map

/-- Map demo state maintained across frames -/
structure MapDemoState where
  mapState : MapState
  initialized : Bool := false
  httpInitialized : Bool := false

/-- Initialize map demo state centered on San Francisco -/
def MapDemoState.create (screenWidth screenHeight : Float) : IO MapDemoState := do
  -- Initialize HTTP (curl global state)
  Afferent.FFI.HTTP.globalInit

  -- Disk cache config - use a reasonable cache size and path
  let diskConfig : Afferent.DiskCache.DiskCacheConfig := {
    cacheDir := "/tmp/afferent_map_cache"
    maxSizeBytes := 100 * 1024 * 1024  -- 100MB disk cache
  }

  -- Initialize map state centered on San Francisco
  let mapState ← MapState.init
    37.7749  -- latitude
    (-122.4194)  -- longitude
    12  -- initial zoom level
    screenWidth.toInt64.toInt
    screenHeight.toInt64.toInt
    diskConfig

  pure { mapState, initialized := true, httpInitialized := true }

/-- Clean up map demo resources -/
def MapDemoState.cleanup (state : MapDemoState) : IO Unit := do
  if state.httpInitialized then
    Afferent.FFI.HTTP.globalCleanup

/-- Update map demo state for one frame -/
def MapDemoState.update (state : MapDemoState) (window : Window) : IO MapDemoState := do
  -- Handle input (pan and zoom)
  let mapState ← handleInput window state.mapState

  -- Update zoom animation
  let mapState := updateZoomAnimation mapState

  -- Cancel tasks for tiles no longer needed
  cancelStaleTasks mapState

  -- Update tile cache (spawn fetches, process results, handle retries)
  let mapState ← updateTileCache mapState

  pure { state with mapState }

/-- Render the map -/
def MapDemoState.render (state : MapDemoState) (renderer : Renderer) : IO Unit := do
  Afferent.Map.render renderer state.mapState

/-- Get current map info for overlay display -/
def MapDemoState.getInfo (state : MapDemoState) : String :=
  let vp := state.mapState.viewport
  let lat := vp.centerLat
  let lon := vp.centerLon
  let vpZoom := vp.zoom  -- viewport.zoom (used for tile fetching)
  let targetZoom := state.mapState.targetZoom
  let displayZoom := state.mapState.displayZoom
  let cacheCount := state.mapState.cache.tiles.size
  s!"Map: lat={lat} lon={lon} vpZoom={vpZoom} target={targetZoom} display={displayZoom} tiles={cacheCount}"

/-- Global map demo state (for integration with Runner) -/
initialize mapDemoStateRef : IO.Ref (Option MapDemoState) ← IO.mkRef none

/-- Initialize map demo (called once at startup) -/
def initMapDemo (screenWidth screenHeight : Float) : IO Unit := do
  let state ← MapDemoState.create screenWidth screenHeight
  mapDemoStateRef.set (some state)

/-- Cleanup map demo (called at shutdown) -/
def cleanupMapDemo : IO Unit := do
  match ← mapDemoStateRef.get with
  | some state => state.cleanup
  | none => pure ()
  mapDemoStateRef.set none

/-- Update map demo (called each frame when map mode is active) -/
def updateMapDemo (window : Window) : IO Unit := do
  match ← mapDemoStateRef.get with
  | some state =>
    let state ← state.update window
    mapDemoStateRef.set (some state)
  | none => pure ()

/-- Render map demo (called each frame when map mode is active) -/
def renderMapDemo (renderer : Renderer) : IO String := do
  match ← mapDemoStateRef.get with
  | some state =>
    state.render renderer
    pure state.getInfo
  | none =>
    pure "Map not initialized"

/-- Standalone map demo (can run independently) -/
def standaloneMapDemo : IO Unit := do
  IO.println "Map Demo - Tile-based Map Viewer"
  IO.println "================================"
  IO.println "Controls:"
  IO.println "  - Drag to pan"
  IO.println "  - Scroll wheel to zoom"
  IO.println "  - Close window to exit"
  IO.println ""

  -- Get screen scale
  let screenScale ← FFI.getScreenScale

  -- Dimensions
  let baseWidth : Float := 1280.0
  let baseHeight : Float := 720.0
  let physWidth := (baseWidth * screenScale).toUInt32
  let physHeight := (baseHeight * screenScale).toUInt32

  -- Create window and renderer
  let canvas ← Canvas.create physWidth physHeight "Afferent - Map Demo"

  -- Load font for overlay
  let font ← Afferent.Font.load "/System/Library/Fonts/Monaco.ttf" (14 * screenScale).toUInt32

  -- Initialize map
  let physWidthF := baseWidth * screenScale
  let physHeightF := baseHeight * screenScale
  initMapDemo physWidthF physHeightF

  -- Main loop
  let mut c := canvas
  while !(← c.shouldClose) do
    c.pollEvents

    let ok ← c.beginFrame Color.darkGray
    if ok then
      -- Update map
      updateMapDemo c.ctx.window

      -- Render map
      let info ← renderMapDemo c.ctx.renderer

      -- Render overlay
      c ← CanvasM.run' (c.resetTransform) do
        CanvasM.setFillColor (Color.hsva 0.0 0.0 0.0 0.6)
        CanvasM.fillRectXYWH (10 * screenScale) (10 * screenScale) (400 * screenScale) (25 * screenScale)
        CanvasM.setFillColor Color.white
        CanvasM.fillTextXY info (20 * screenScale) (27 * screenScale) font

      c ← c.endFrame

  -- Cleanup
  IO.println "Cleaning up..."
  cleanupMapDemo
  font.destroy
  canvas.destroy
  IO.println "Done!"

end Demos

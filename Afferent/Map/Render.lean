/-
  Tile Map Rendering and Async Loading
  Ported from heavenly-host to Afferent
  Uses Afferent's drawTexturedRect and HTTP/DiskCache FFI
-/
import Afferent.Map.State
import Afferent.Map.Input
import Afferent.Map.RetryLogic
import Afferent.Map.Zoom
import Afferent.FFI.HTTP
import Afferent.FFI.DiskCache
import Afferent.FFI.Texture
import Afferent.FFI.Renderer
import Afferent.DiskCache.Config
import Afferent.DiskCache.LRU

namespace Afferent.Map

open Afferent.FFI.HTTP (httpGetBinary)
open Afferent.FFI.DiskCache (fileExists readFile writeFile touchFile deleteFile nowMs)
open Afferent.FFI (Texture Renderer)
open Afferent.Map.RetryLogic
open Afferent.Map.Zoom (floatClamp centerForAnchor)
open Afferent.DiskCache (DiskCacheConfig DiskCacheIndex TileCacheEntry tilePath)
open Afferent.DiskCache.LRU

/-- Lerp factor for zoom animation (per frame at 60fps) -/
def zoomLerpFactor : Float := 0.15

/-- Threshold for snapping to target zoom -/
def zoomSnapThreshold : Float := 0.01

/-- Update zoom animation state.
    Lerps displayZoom toward targetZoom, keeping the anchor point fixed on screen. -/
def updateZoomAnimation (state : MapState) : MapState :=
  if !state.isAnimatingZoom then state
  else
    let target := intToFloat state.targetZoom
    let diff := target - state.displayZoom
    if Float.abs diff < zoomSnapThreshold then
      -- Snap to target and stop animation
      let (newLat, newLon) := centerForAnchor
          state.zoomAnchorLat state.zoomAnchorLon
          state.zoomAnchorScreenX state.zoomAnchorScreenY
          state.viewport.screenWidth state.viewport.screenHeight
          state.viewport.tileSize target
      let clampedLat := floatClamp newLat (-85.0) 85.0
      let wrappedLon := if newLon > 180.0 then newLon - 360.0
                        else if newLon < -180.0 then newLon + 360.0
                        else newLon
      { state with
          displayZoom := target
          isAnimatingZoom := false
          viewport := { state.viewport with
            centerLat := clampedLat
            centerLon := wrappedLon
            zoom := state.targetZoom
          }
      }
    else
      -- Lerp toward target
      let newDisplayZoom := state.displayZoom + diff * zoomLerpFactor
      -- Recompute center to keep anchor fixed
      let (newLat, newLon) := centerForAnchor
          state.zoomAnchorLat state.zoomAnchorLon
          state.zoomAnchorScreenX state.zoomAnchorScreenY
          state.viewport.screenWidth state.viewport.screenHeight
          state.viewport.tileSize newDisplayZoom
      let clampedLat := floatClamp newLat (-85.0) 85.0
      let wrappedLon := if newLon > 180.0 then newLon - 360.0
                        else if newLon < -180.0 then newLon + 360.0
                        else newLon
      -- Update viewport.zoom to floor of displayZoom for tile fetching
      let tileZoom := clampZoom (natToInt newDisplayZoom.floor.toUInt64.toNat)
      { state with
          displayZoom := newDisplayZoom
          viewport := { state.viewport with
            centerLat := clampedLat
            centerLon := wrappedLon
            zoom := tileZoom
          }
      }

/-- Spawn a background task to fetch and decode a tile (checks disk cache first) -/
def spawnFetchTask (coord : TileCoord) (queue : IO.Ref (Array FetchResult))
    (diskConfig : DiskCacheConfig) (diskIndex : IO.Ref DiskCacheIndex)
    (cancelFlag : IO.Ref Bool)
    (wasRetry : Bool := false) : IO Unit := do
  let _ ← IO.asTask do
    -- Check cancellation before starting
    if ← cancelFlag.get then return

    let cachePath := tilePath diskConfig coord

    -- Try disk cache first, then fall back to network
    let pngData ← if ← fileExists cachePath then
      -- Disk cache hit
      match ← readFile cachePath with
      | .ok data =>
        -- Update access time for LRU
        touchFile cachePath
        let now ← nowMs
        diskIndex.modify fun idx => touchEntry idx coord now
        pure (Except.ok data)
      | .error _ =>
        -- Disk read failed, check cancellation before network fallback
        if ← cancelFlag.get then pure (Except.error "Cancelled")
        else httpGetBinary (tileUrl coord)
    else
      -- Disk cache miss, check cancellation before network fetch
      if ← cancelFlag.get then pure (Except.error "Cancelled")
      else httpGetBinary (tileUrl coord)

    -- Check cancellation before writing to cache
    if ← cancelFlag.get then return

    -- Write to disk cache if we got data and not already cached
    match pngData with
    | .ok data =>
      let alreadyCached ← fileExists cachePath
      unless alreadyCached do
        match ← writeFile cachePath data with
        | .ok () =>
          -- Update disk cache index with LRU eviction
          let now ← nowMs
          let fileSize := data.size
          -- Atomically update index and get list of files to evict
          let evictions ← diskIndex.modifyGet fun idx =>
            let evictions := selectEvictions idx fileSize
            let idx' := removeEntries idx evictions
            let entry : TileCacheEntry := {
              coord := coord
              filePath := cachePath
              sizeBytes := fileSize
              lastAccessTime := now
            }
            (evictions, addEntry idx' entry)
          -- Schedule deletions (fire and forget) - outside the pure modify
          for entry in evictions do
            let _ ← IO.asTask (deleteFile entry.filePath)
        | .error _ => pure ()  -- Disk write failed, continue anyway
    | .error _ => pure ()

    -- Decode PNG bytes to a CPU texture on the background thread.
    let result : Except String (Texture × ByteArray) ←
      match pngData with
      | .ok data =>
        try
          let texture ← Afferent.FFI.Texture.loadFromMemory data
          pure (.ok (texture, data))
        catch e =>
          pure (.error s!"Texture load failed: {e}")
      | .error msg => pure (.error msg)

    -- If cancelled after decode, ensure we don't leak the texture.
    if ← cancelFlag.get then
      match result with
      | .ok (texture, _) => Afferent.FFI.Texture.destroy texture
      | .error _ => pure ()
      return

    -- Only push result if not cancelled
    unless ← cancelFlag.get do
      queue.modify fun arr => arr.push { coord := coord, result := result, wasRetry := wasRetry }
  pure ()

/-- Spawn a background task to decode cached PNG bytes into a CPU texture. -/
def spawnDecodeTask (coord : TileCoord) (pngData : ByteArray) (queue : IO.Ref (Array FetchResult))
    (cancelFlag : IO.Ref Bool) : IO Unit := do
  let _ ← IO.asTask do
    if ← cancelFlag.get then return
    let result : Except String (Texture × ByteArray) ←
      try
        let texture ← Afferent.FFI.Texture.loadFromMemory pngData
        pure (.ok (texture, pngData))
      catch e =>
        pure (.error s!"Texture load failed: {e}")
    if ← cancelFlag.get then
      match result with
      | .ok (texture, _) => Afferent.FFI.Texture.destroy texture
      | .error _ => pure ()
      return
    queue.modify fun arr => arr.push { coord := coord, result := result }
  pure ()

/-- Process any completed fetch results (main thread) -/
def processCompletedFetches (state : MapState) : IO MapState := do
  -- Atomically take all pending results
  let results ← state.resultQueue.modifyGet fun arr => (arr, #[])

  let tau := state.frameCount
  let config := state.cache.retryConfig
  let mut cache := state.cache

  for res in results do
    -- Remove from active tasks tracking (task completed)
    state.activeTasks.modify fun m => m.erase res.coord

    match res.result with
    | .ok (texture, pngData) =>
      cache := cache.insert res.coord (.loaded texture pngData)
    | .error msg =>
      -- Network failure - handle based on whether this was a retry
      if res.wasRetry then
        -- This was a retry that failed - get previous state and increment
        match cache.get res.coord with
        | some (.retrying rs) =>
          let rs' := rs.recordRetryFailure tau msg
          if rs'.isExhausted config then
            cache := cache.insert res.coord (.exhausted rs')
          else
            cache := cache.insert res.coord (.failed rs')
        | _ =>
          -- Shouldn't happen, but handle gracefully
          let rs := RetryState.initialFailure tau msg
          cache := cache.insert res.coord (.failed rs)
      else
        -- Initial failure
        let rs := RetryState.initialFailure tau msg
        cache := cache.insert res.coord (.failed rs)

  pure { state with cache := cache }

/-- Schedule retries for failed tiles that are ready to retry -/
def scheduleRetries (state : MapState) : IO MapState := do
  let tau := state.frameCount
  let config := state.cache.retryConfig
  let visible := state.viewport.visibleTiles

  let mut cache := state.cache

  for coord in visible do
    match cache.get coord with
    | some (.failed rs) =>
      -- Use the proven shouldRetry function to decide
      if rs.shouldRetry config tau then
        -- Transition to retrying state and spawn task with cancellation token
        cache := cache.insert coord (.retrying rs)
        let cancelFlag ← IO.mkRef false
        state.activeTasks.modify fun m => m.insert coord cancelFlag
        spawnFetchTask coord state.resultQueue state.diskCacheConfig state.diskCacheIndex cancelFlag (wasRetry := true)
    | _ => pure ()

  pure { state with cache := cache }

/-- Unload tiles outside the visible + buffer zone -/
def unloadDistantTiles (state : MapState) (keepSet : Std.HashSet TileCoord) : IO MapState := do
  let frameCount := state.frameCount

  -- Unload GPU textures outside buffer, but keep PNG data in RAM as cached
  let toUnload := state.cache.tilesToUnload keepSet
  let mut cache := state.cache
  for (coord, texture, pngData) in toUnload do
    -- Match heavenly-host behavior: free GPU memory but keep data in RAM for fast reload.
    Afferent.FFI.Texture.destroy texture
    cache := cache.insert coord (.cached pngData frameCount)

  -- Remove stale non-loaded entries (pending/failed/retrying/exhausted outside buffer)
  let stale := cache.staleTiles keepSet
  let cache' := cache.removeCoords stale

  pure { state with cache := cache' }

/-- Evict oldest cached images when RAM cache exceeds limit -/
def evictCachedImages (state : MapState) (keepSet : Std.HashSet TileCoord) : IO MapState := do
  let maxCached := state.cache.unloadConfig.maxCachedImages

  -- Get list of cached images to evict (oldest first, excluding those near viewport)
  let toEvict := state.cache.cachedImagesToEvict keepSet maxCached

  -- Remove them from cache
  let cache := state.cache.removeCoords toEvict

  pure { state with cache := cache }

/-- Reload cached tiles that are now visible back to GPU -/
def reloadCachedTiles (state : MapState) : IO MapState := do
  let visible := state.viewport.visibleTiles
  let visibleSet : Std.HashSet TileCoord := visible.foldl (fun s t => s.insert t) {}
  let toReload := state.cache.cachedTilesToReload visibleSet

  let mut cache := state.cache
  for (coord, pngData) in toReload do
    if pngData.size > 0 then
      let tasks ← state.activeTasks.get
      unless tasks.contains coord do
        -- Kick decoding to a background task; keep the tile in `.pending` while it runs.
        cache := cache.insert coord .pending
        let cancelFlag ← IO.mkRef false
        state.activeTasks.modify fun m => m.insert coord cancelFlag
        spawnDecodeTask coord pngData state.resultQueue cancelFlag

  pure { state with cache := cache }

/-- Compute keep set based on which ancestors are actually loaded and needed -/
def computeKeepSet (vp : MapViewport) (cache : TileCache) (buffer : Int)
    : Std.HashSet TileCoord :=
  let baseTiles := vp.visibleTilesWithBuffer buffer

  -- Start with all visible tiles at current zoom (with buffer)
  let keepSet : Std.HashSet TileCoord := baseTiles.foldl (fun s t => s.insert t) {}

  -- Always include parent tiles for proactive fallback (prevents background visibility)
  let keepSet := if vp.zoom <= 0 then keepSet
    else baseTiles.foldl (fun s t => s.insert t.parentTile) keepSet

  -- For each non-loaded visible tile, add its loaded ancestors and loaded children
  baseTiles.foldl (fun s coord =>
    if cache.isLoaded coord then
      s  -- Already loaded, no fallback needed
    else
      -- Add loaded ancestors (for zoom-in fallback)
      let withAncestors := cache.getLoadedAncestors coord |>.foldl (fun s' a => s'.insert a) s
      -- Add loaded children (for zoom-out fallback)
      coord.childTiles.foldl (fun s' child =>
        if cache.isLoaded child then s'.insert child else s'
      ) withAncestors
  ) keepSet

/-- Cancel pending tasks for tiles outside the current viewport -/
def cancelStaleTasks (state : MapState) : IO Unit := do
  let buffer := state.cache.unloadConfig.bufferTiles
  let keepSet := computeKeepSet state.viewport state.cache buffer
  let tasks ← state.activeTasks.get
  for (coord, cancelFlag) in tasks.toList do
    unless keepSet.contains coord do
      cancelFlag.set true
      -- Drop bookkeeping immediately; task will observe cancelFlag and skip queueing.
      state.activeTasks.modify fun m => m.erase coord

/-- Update cache: spawn fetches for missing tiles and schedule retries (non-blocking) -/
def updateTileCache (state : MapState) : IO MapState := do
  -- Compute keepSet based on actual tile state
  let buffer := state.cache.unloadConfig.bufferTiles
  let keepSet := computeKeepSet state.viewport state.cache buffer

  -- First, unload tiles outside buffer zone
  let state ← unloadDistantTiles state keepSet

  -- Evict oldest cached images if RAM cache is over limit
  let state ← evictCachedImages state keepSet

  -- Reload any cached tiles that are now visible
  let state ← reloadCachedTiles state

  -- Process any completed fetches
  let state ← processCompletedFetches state

  -- Schedule retries for failed tiles
  let state ← scheduleRetries state

  -- Get visible tiles sorted by distance from center (prioritize center tiles)
  let visible := state.viewport.visibleTiles
  let (centerX, centerY) := state.viewport.centerTilePos
  let sortedVisible := visible.toArray.qsort (fun a b =>
    let dxA := intToFloat a.x - centerX
    let dyA := intToFloat a.y - centerY
    let dxB := intToFloat b.x - centerX
    let dyB := intToFloat b.y - centerY
    let distA := dxA * dxA + dyA * dyA
    let distB := dxB * dxB + dyB * dyB
    distA < distB
  )

  let mut cache := state.cache

  -- Fetch parent tiles FIRST (highest priority - ensures fallback is always available)
  if state.viewport.zoom > 0 then
    let parentSet : Std.HashSet TileCoord := visible.foldl
      (fun s t => s.insert t.parentTile) {}
    for parentCoord in parentSet.toList do
      unless cache.contains parentCoord do
        cache := cache.insert parentCoord .pending
        let cancelFlag ← IO.mkRef false
        state.activeTasks.modify fun m => m.insert parentCoord cancelFlag
        spawnFetchTask parentCoord state.resultQueue state.diskCacheConfig state.diskCacheIndex cancelFlag

  -- Then fetch visible tiles (sorted by distance from center)
  for coord in sortedVisible do
    unless cache.contains coord do
      cache := cache.insert coord .pending
      let cancelFlag ← IO.mkRef false
      state.activeTasks.modify fun m => m.insert coord cancelFlag
      spawnFetchTask coord state.resultQueue state.diskCacheConfig state.diskCacheIndex cancelFlag

  -- Increment frame counter (abstract time advances)
  pure { state with cache := cache, frameCount := state.frameCount + 1 }

/-- Find a loaded fallback tile at a lower zoom level (parent, grandparent, etc.) -/
def findParentFallback (cache : TileCache) (coord : TileCoord) (maxLevels : Nat := 3)
    : Option (TileCoord × Texture × Nat) :=
  go coord 1 maxLevels
where
  go (c : TileCoord) (delta : Nat) (remaining : Nat) : Option (TileCoord × Texture × Nat) :=
    match remaining with
    | 0 => none
    | remaining' + 1 =>
      if c.z <= 0 then none
      else
        let parent := c.parentTile
        match cache.get parent with
        | some (.loaded tex _) => some (parent, tex, delta)
        | _ => go parent (delta + 1) remaining'

/-- Compute source rectangle within ancestor tile for rendering a descendant -/
def computeAncestorOffset (target ancestor : TileCoord) (delta : Nat) : (Float × Float) :=
  let scale := Float.pow 2.0 (intToFloat (natToInt delta))
  let ancestorScaledX := (intToFloat ancestor.x) * scale
  let ancestorScaledY := (intToFloat ancestor.y) * scale
  let offsetX := (intToFloat target.x) - ancestorScaledX
  let offsetY := (intToFloat target.y) - ancestorScaledY
  (offsetX / scale, offsetY / scale)

/-- Compute screen position for a tile with fractional zoom support -/
def tileScreenPosFrac (vp : MapViewport) (tile : TileCoord) (displayZoom : Float) : (Float × Float) :=
  let n := Float.pow 2.0 displayZoom
  let centerTileX := (vp.centerLon + 180.0) / 360.0 * n
  let latRad := vp.centerLat * pi / 180.0
  let centerTileY := (1.0 - Float.log (Float.tan latRad + 1.0 / Float.cos latRad) / pi) / 2.0 * n
  let scale := Float.pow 2.0 (displayZoom - intToFloat tile.z)
  let tileX := (intToFloat tile.x) * scale
  let tileY := (intToFloat tile.y) * scale
  let offsetX := (tileX - centerTileX) * (intToFloat vp.tileSize) + (intToFloat vp.screenWidth) / 2.0
  let offsetY := (tileY - centerTileY) * (intToFloat vp.tileSize) + (intToFloat vp.screenHeight) / 2.0
  (offsetX, offsetY)

/-- Render all visible tiles with fractional zoom scaling -/
def renderTiles (renderer : Renderer) (state : MapState) : IO Unit := do
  let visible := state.viewport.visibleTiles
  let textureSize : Float := 512.0  -- @2x retina tiles are 512px
  let canvasWidth := (intToFloat state.viewport.screenWidth)
  let canvasHeight := (intToFloat state.viewport.screenHeight)

  -- Compute scale factor for fractional zoom
  let tileZoom := state.viewport.zoom
  let scale := Float.pow 2.0 (state.displayZoom - intToFloat tileZoom)
  let scaledTileSize := (intToFloat state.viewport.tileSize) * scale

  -- PASS 1: Render parent tiles as background layer (scaled up 2x)
  if state.viewport.zoom > 0 then
    let parentSet : Std.HashSet TileCoord := visible.foldl
      (fun s t => s.insert t.parentTile) {}
    let parentTileSize := scaledTileSize * 2.0
    for parentCoord in parentSet.toList do
      match state.cache.get parentCoord with
      | some (.loaded texture _) =>
        let (px, py) := tileScreenPosFrac state.viewport parentCoord state.displayZoom
        Renderer.drawTexturedRect renderer texture
          0.0 0.0 textureSize textureSize  -- Source (full texture)
          px py parentTileSize parentTileSize  -- Destination
          canvasWidth canvasHeight
          1.0  -- Alpha
      | _ => pure ()

  -- PASS 2: Render visible tiles on top (higher resolution)
  for coord in visible do
    let (x, y) := tileScreenPosFrac state.viewport coord state.displayZoom
    match state.cache.get coord with
    | some (.loaded texture _) =>
      Renderer.drawTexturedRect renderer texture
        0.0 0.0 textureSize textureSize  -- Source
        x y scaledTileSize scaledTileSize  -- Destination
        canvasWidth canvasHeight
        1.0  -- Alpha
    | _ =>
      -- Not loaded - try fallback from parent
      match findParentFallback state.cache coord with
      | some (ancestor, tex, delta) =>
        let (offsetX, offsetY) := computeAncestorOffset coord ancestor delta
        let srcScale := Float.pow 2.0 (intToFloat (natToInt delta))
        let srcSize := textureSize / srcScale
        let srcX := offsetX * textureSize
        let srcY := offsetY * textureSize
        Renderer.drawTexturedRect renderer tex
          srcX srcY srcSize srcSize  -- Source (sub-region of ancestor)
          x y scaledTileSize scaledTileSize  -- Destination
          canvasWidth canvasHeight
          1.0
      | none => pure ()

/-- Main render function -/
def render (renderer : Renderer) (state : MapState) : IO Unit := do
  renderTiles renderer state

end Afferent.Map

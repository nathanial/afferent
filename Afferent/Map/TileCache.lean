/-
  Tile Cache with Retry Support
  Ported from heavenly-host to Afferent
  Adapted to use Afferent.FFI.Texture instead of Raylib types
-/
import Std.Data.HashMap
import Std.Data.HashSet
import Afferent.Map.TileCoord
import Afferent.Map.RetryLogic
import Afferent.FFI.Texture

namespace Afferent.Map

open Std (HashMap)
open Afferent.FFI (Texture)
open Afferent.Map.RetryLogic (RetryConfig RetryState)

/-- Tile loading state with retry support
    Note: Unlike Raylib version, we don't separate Image and Texture
    since Afferent's texture loading is handled differently -/
inductive TileState where
  | pending : TileState                         -- Initial fetch in flight
  | loaded : Texture → ByteArray → TileState    -- Loaded on GPU + raw PNG bytes for later reload
  | cached : ByteArray → Nat → TileState        -- Unloaded from GPU, raw PNG in RAM (Nat = lastAccess frame)
  | failed : RetryState → TileState             -- Failed, can be retried
  | retrying : RetryState → TileState           -- Retry fetch in flight
  | exhausted : RetryState → TileState          -- All retries exhausted, permanent failure

/-- Result from a background fetch task (contains raw PNG bytes) -/
structure FetchResult where
  coord : TileCoord
  result : Except String (Texture × ByteArray) -- Decoded CPU texture + original PNG bytes
  wasRetry : Bool := false                     -- Track if this was a retry attempt

/-- Default retry configuration: 3 retries with exponential backoff starting at 60 frames (1 sec) -/
def defaultRetryConfig : RetryConfig :=
  { maxRetries := 3, baseDelay := 60 }

/-- Configuration for tile unloading behavior -/
structure UnloadConfig where
  bufferTiles : Int := 3      -- Extra tiles beyond visible area to keep GPU textures loaded
  maxCachedImages : Nat := 1500  -- Max images to keep in RAM (includes parent tiles for fallback)
  deriving Repr, Inhabited

def defaultUnloadConfig : UnloadConfig := {}

/-- Tile cache using HashMap with retry configuration -/
structure TileCache where
  tiles : HashMap TileCoord TileState
  retryConfig : RetryConfig := defaultRetryConfig
  unloadConfig : UnloadConfig := defaultUnloadConfig
  deriving Inhabited

namespace TileCache

def empty : TileCache := { tiles := {} }

def get (cache : TileCache) (coord : TileCoord) : Option TileState :=
  cache.tiles[coord]?

def insert (cache : TileCache) (coord : TileCoord) (state : TileState) : TileCache :=
  { cache with tiles := cache.tiles.insert coord state }

def contains (cache : TileCache) (coord : TileCoord) : Bool :=
  cache.tiles.contains coord

/-- Get all loaded textures (for cleanup) -/
def getLoadedTextures (cache : TileCache) : List Texture :=
  cache.tiles.toList.filterMap fun (_, state) =>
    match state with
    | .loaded tex _ => some tex
    | _ => none

/-- Count of all tiles in cache -/
def size (cache : TileCache) : Nat :=
  cache.tiles.size

/-- Count tiles by state category -/
def stateCounts (cache : TileCache) : (Nat × Nat × Nat) :=
  cache.tiles.toList.foldl (fun (gpu, ram, other) (_, state) =>
    match state with
    | .loaded _ _ => (gpu + 1, ram, other)
    | .cached _ _ => (gpu, ram + 1, other)
    | _ => (gpu, ram, other + 1)
  ) (0, 0, 0)

/-- Identify loaded tiles outside the keep zone, returning coord, texture, and PNG bytes. -/
def tilesToUnload (cache : TileCache) (keepSet : Std.HashSet TileCoord) : List (TileCoord × Texture × ByteArray) :=
  cache.tiles.toList.filterMap fun (coord, state) =>
    match state with
    | .loaded tex pngData => if keepSet.contains coord then none else some (coord, tex, pngData)
    | _ => none

/-- Identify non-loaded, non-cached tiles outside the keep zone (cheap to remove) -/
def staleTiles (cache : TileCache) (keepSet : Std.HashSet TileCoord) : List TileCoord :=
  cache.tiles.toList.filterMap fun (coord, state) =>
    match state with
    | .loaded _ _ => none  -- Handled by tilesToUnload
    | .cached _ _ => none  -- Keep cached images in RAM
    | _ => if keepSet.contains coord then none else some coord

/-- Remove tiles from cache by coordinates -/
def removeCoords (cache : TileCache) (coords : List TileCoord) : TileCache :=
  { cache with tiles := coords.foldl (fun m c => m.erase c) cache.tiles }

/-- Get cached tiles that are in the visible set (need GPU reload) -/
def cachedTilesToReload (cache : TileCache) (visibleSet : Std.HashSet TileCoord) : List (TileCoord × ByteArray) :=
  cache.tiles.toList.filterMap fun (coord, state) =>
    match state with
    | .cached pngData _ => if visibleSet.contains coord then some (coord, pngData) else none
    | _ => none

/-- Count of cached images in RAM -/
def cachedImageCount (cache : TileCache) : Nat :=
  cache.tiles.toList.foldl (fun count (_, state) =>
    match state with
    | .cached _ _ => count + 1
    | _ => count
  ) 0

/-- Get oldest cached images to evict (returns coords sorted by lastAccess, oldest first) -/
def cachedImagesToEvict (cache : TileCache) (keepSet : Std.HashSet TileCoord) (maxToKeep : Nat) : List TileCoord :=
  -- Get all cached tiles with their access times, excluding those in keepSet
  let cached := cache.tiles.toList.filterMap fun (coord, state) =>
    match state with
    | .cached _ lastAccess =>
      if keepSet.contains coord then none else some (coord, lastAccess)
    | _ => none
  -- Sort by lastAccess (oldest first)
  let sorted := cached.toArray.qsort (fun a b => a.2 < b.2) |>.toList
  -- Calculate how many to evict
  let currentCount := cache.cachedImageCount
  if currentCount <= maxToKeep then
    []
  else
    let toEvict := currentCount - maxToKeep
    sorted.take toEvict |>.map Prod.fst

/-- Check if a tile is loaded on GPU -/
def isLoaded (cache : TileCache) (coord : TileCoord) : Bool :=
  match cache.get coord with
  | some (.loaded _ _) => true
  | _ => false

/-- Get all loaded ancestors of a tile (walks up through ALL ancestors, collecting loaded ones).
    Used to determine which ancestors should be kept for fallback rendering. -/
def getLoadedAncestors (cache : TileCache) (coord : TileCoord) (maxLevels : Nat := 8)
    : List TileCoord :=
  go coord maxLevels []
where
  go (c : TileCoord) (remaining : Nat) (acc : List TileCoord) : List TileCoord :=
    match remaining with
    | 0 => acc
    | remaining' + 1 =>
      if c.z <= 0 then acc
      else
        let parent := c.parentTile
        -- Keep climbing even if parent is not loaded - grandparent might be!
        let acc' := if cache.isLoaded parent then parent :: acc else acc
        go parent remaining' acc'

end TileCache

end Afferent.Map

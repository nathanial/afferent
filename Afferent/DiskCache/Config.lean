/-
  Disk Cache Configuration and Types
  Ported from heavenly-host to Afferent
-/
import Std.Data.HashMap
import Afferent.Map.TileCoord

namespace Afferent.DiskCache

open Afferent.Map (TileCoord)

/-- Configuration for disk tile cache -/
structure DiskCacheConfig where
  cacheDir : String := "./tile_cache"
  tilesetName : String := "cartodb_dark"
  maxSizeBytes : Nat := 2000 * 1024 * 1024  -- 2 GB default
  deriving Repr, Inhabited

/-- Metadata for a cached tile file (used for LRU tracking) -/
structure TileCacheEntry where
  coord : TileCoord
  filePath : String
  sizeBytes : Nat
  lastAccessTime : Nat  -- Monotonic timestamp in ms
  deriving Repr, Inhabited, BEq

/-- In-memory index of cached tiles for LRU tracking -/
structure DiskCacheIndex where
  entries : Std.HashMap TileCoord TileCacheEntry
  totalSizeBytes : Nat
  config : DiskCacheConfig
  deriving Inhabited

namespace DiskCacheIndex

def empty (config : DiskCacheConfig) : DiskCacheIndex :=
  { entries := {}, totalSizeBytes := 0, config := config }

end DiskCacheIndex

/-- Compute file path for a tile: {cacheDir}/{tilesetName}/{z}/{x}/{y}.png -/
def tilePath (config : DiskCacheConfig) (coord : TileCoord) : String :=
  s!"{config.cacheDir}/{config.tilesetName}/{coord.z}/{coord.x}/{coord.y}.png"

end Afferent.DiskCache

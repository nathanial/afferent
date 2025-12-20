/-
  Tile Disk Cache - Disk cache for map tiles using Cellar
  Specializes Cellar's generic cache for TileCoord keys.
-/
import Cellar
import Afferent.Map.TileCoord

namespace Afferent.Map

open Afferent.Map (TileCoord)

/-- Configuration for tile disk cache -/
structure TileDiskCacheConfig where
  /-- Base directory for cached tiles -/
  cacheDir : String := "./tile_cache"
  /-- Tileset name (used in path) -/
  tilesetName : String := "cartodb_dark"
  /-- Maximum total size of cached tiles in bytes -/
  maxSizeBytes : Nat := 2000 * 1024 * 1024  -- 2 GB default
  deriving Repr, Inhabited

/-- Convert TileDiskCacheConfig to Cellar.CacheConfig -/
def TileDiskCacheConfig.toCellarConfig (config : TileDiskCacheConfig) : Cellar.CacheConfig :=
  { cacheDir := config.cacheDir, maxSizeBytes := config.maxSizeBytes }

/-- Alias for tile cache entry -/
abbrev TileDiskCacheEntry := Cellar.CacheEntry TileCoord

/-- Alias for tile cache index -/
abbrev TileDiskCacheIndex := Cellar.CacheIndex TileCoord

/-- Create an empty tile cache index -/
def TileDiskCacheIndex.empty (config : TileDiskCacheConfig) : TileDiskCacheIndex :=
  Cellar.CacheIndex.empty config.toCellarConfig

/-- Compute file path for a tile: {cacheDir}/{tilesetName}/{z}/{x}/{y}.png -/
def tilePath (config : TileDiskCacheConfig) (coord : TileCoord) : String :=
  s!"{config.cacheDir}/{config.tilesetName}/{coord.z}/{coord.x}/{coord.y}.png"

-- Re-export cellar functions for convenience
export Cellar (fileExists readFile writeFile deleteFile nowMs)
export Cellar (selectEvictions addEntry removeEntries touchEntry wouldExceedLimit)

end Afferent.Map

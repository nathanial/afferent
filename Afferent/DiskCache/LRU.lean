/-
  LRU (Least Recently Used) Cache Eviction Logic
  Ported from heavenly-host to Afferent
-/
import Afferent.DiskCache.Config

namespace Afferent.DiskCache.LRU

open Afferent.DiskCache
open Afferent.Map (TileCoord)

/-- Select tiles to evict to bring cache under size limit.
    Returns list of entries to evict (oldest first). -/
def selectEvictions (index : DiskCacheIndex) (newFileSize : Nat) : List TileCacheEntry :=
  let targetMax := index.config.maxSizeBytes
  if index.totalSizeBytes + newFileSize <= targetMax then
    []  -- No eviction needed
  else
    -- Sort entries by lastAccessTime (oldest first)
    let sorted := index.entries.toList.map Prod.snd
      |>.toArray.qsort (fun a b => a.lastAccessTime < b.lastAccessTime)
      |>.toList

    -- Calculate how much space we need to free
    let currentTotal := index.totalSizeBytes + newFileSize
    let needToFree := currentTotal - targetMax

    -- Accumulate entries to evict until we have enough space
    Id.run do
      let mut toEvict : List TileCacheEntry := []
      let mut freedBytes : Nat := 0
      for entry in sorted do
        if freedBytes >= needToFree then
          break
        toEvict := entry :: toEvict
        freedBytes := freedBytes + entry.sizeBytes
      return toEvict.reverse  -- Return in oldest-first order

/-- Update index after adding a new entry -/
def addEntry (index : DiskCacheIndex) (entry : TileCacheEntry) : DiskCacheIndex :=
  { index with
    entries := index.entries.insert entry.coord entry
    totalSizeBytes := index.totalSizeBytes + entry.sizeBytes
  }

/-- Update index after evicting entries -/
def removeEntries (index : DiskCacheIndex) (evicted : List TileCacheEntry) : DiskCacheIndex :=
  let entries' := evicted.foldl (fun m e => m.erase e.coord) index.entries
  let removedSize := evicted.foldl (fun acc e => acc + e.sizeBytes) 0
  { index with
    entries := entries'
    totalSizeBytes := index.totalSizeBytes - removedSize
  }

/-- Update access time for a tile (on cache hit) -/
def touchEntry (index : DiskCacheIndex) (coord : TileCoord) (newTime : Nat) : DiskCacheIndex :=
  match index.entries[coord]? with
  | some entry =>
    let entry' := { entry with lastAccessTime := newTime }
    { index with entries := index.entries.insert coord entry' }
  | none => index  -- Entry not in index (shouldn't happen)

/-- Check if adding a file would exceed the cache limit -/
def wouldExceedLimit (index : DiskCacheIndex) (newFileSize : Nat) : Bool :=
  index.totalSizeBytes + newFileSize > index.config.maxSizeBytes

end Afferent.DiskCache.LRU

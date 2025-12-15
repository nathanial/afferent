/-
  DiskCache Module - LRU disk tile cache
  Ported from heavenly-host to Afferent
-/
import Afferent.DiskCache.Config
import Afferent.DiskCache.LRU

namespace Afferent.DiskCache

-- Re-export main types and functions
export Config (DiskCacheConfig DiskCacheIndex TileCacheEntry tilePath)
export LRU (selectEvictions addEntry removeEntries touchEntry)

end Afferent.DiskCache

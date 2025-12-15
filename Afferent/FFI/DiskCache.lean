/-
  Afferent FFI DiskCache
  File I/O operations for disk-based caching.
-/

namespace Afferent.FFI.DiskCache

/-- Check if a file exists -/
@[extern "lean_disk_cache_exists"]
opaque fileExists (path : @& String) : IO Bool

/-- Read file contents as ByteArray -/
@[extern "lean_disk_cache_read"]
opaque readFile (path : @& String) : IO (Except String ByteArray)

/-- Write ByteArray to file (creates directories, atomic via temp+rename) -/
@[extern "lean_disk_cache_write"]
opaque writeFile (path : @& String) (data : @& ByteArray) : IO (Except String Unit)

/-- Get file size in bytes -/
@[extern "lean_disk_cache_file_size"]
opaque getFileSize (path : @& String) : IO (Except String Nat)

/-- Get file modification time (seconds since epoch) -/
@[extern "lean_disk_cache_mod_time"]
opaque getModTime (path : @& String) : IO (Except String Nat)

/-- Update file access/modification time to now -/
@[extern "lean_disk_cache_touch"]
opaque touchFile (path : @& String) : IO Unit

/-- Delete a file -/
@[extern "lean_disk_cache_delete"]
opaque deleteFile (path : @& String) : IO (Except String Unit)

/-- Get current monotonic time in milliseconds -/
@[extern "lean_disk_cache_now_ms"]
opaque nowMs : IO Nat

end Afferent.FFI.DiskCache

/-
  Afferent DiskCache IO
  File I/O operations for disk-based caching.
  Pure Lean implementation using standard library.
-/

namespace Afferent.DiskCache

/-- Check if a file exists -/
def fileExists (path : String) : IO Bool :=
  System.FilePath.pathExists path

/-- Read file contents as ByteArray -/
def readFile (path : String) : IO (Except String ByteArray) := do
  try
    let data ← IO.FS.readBinFile path
    pure (.ok data)
  catch e =>
    pure (.error (toString e))

/-- Write ByteArray to file (creates directories, atomic via temp+rename) -/
def writeFile (path : String) (data : ByteArray) : IO (Except String Unit) := do
  try
    -- Create parent directories if needed
    let filePath : System.FilePath := path
    if let some parent := filePath.parent then
      IO.FS.createDirAll parent

    -- Write to temp file first for atomic operation
    let tmpPath := path ++ ".tmp." ++ toString (← IO.monoNanosNow)
    IO.FS.writeBinFile tmpPath data

    -- Atomic rename
    IO.FS.rename tmpPath path
    pure (.ok ())
  catch e =>
    pure (.error (toString e))

/-- Get file size in bytes -/
def getFileSize (path : String) : IO (Except String Nat) := do
  try
    let metadata ← (path : System.FilePath).metadata
    pure (.ok metadata.byteSize.toNat)
  catch e =>
    pure (.error (toString e))

/-- Get file modification time (seconds since epoch) -/
def getModTime (path : String) : IO (Except String Nat) := do
  try
    let metadata ← (path : System.FilePath).metadata
    pure (.ok metadata.modified.sec.toNat)
  catch e =>
    pure (.error (toString e))

/-- Delete a file (best effort - ignores errors like file not found) -/
def deleteFile (path : String) : IO (Except String Unit) := do
  try
    IO.FS.removeFile path
    pure (.ok ())
  catch _ =>
    -- Ignore errors (file may not exist, which is fine for cache eviction)
    pure (.ok ())

/-- Get current monotonic time in milliseconds -/
def nowMs : IO Nat := do
  let ns ← IO.monoNanosNow
  pure (ns / 1000000)

end Afferent.DiskCache

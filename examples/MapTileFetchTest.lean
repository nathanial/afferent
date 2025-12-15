import Afferent
import Afferent.FFI.HTTP
import Afferent.FFI.Texture
import Afferent.Map.TileCoord

open Afferent
open Afferent.FFI
open Afferent.Map

def main : IO Unit := do
  HTTP.globalInit

  let coord : TileCoord := { x := 655, y := 1583, z := 12 }
  let url := tileUrl coord
  IO.println s!"Fetching z={coord.z} x={coord.x} y={coord.y} -> {url}"

  let res ← HTTP.httpGetBinary url
  match res with
  | .ok bytes =>
      IO.println s!"HTTP ok: {bytes.size} bytes"
      try
        let tex ← Texture.loadFromMemory bytes
        let (w, h) ← Texture.getSize tex
        IO.println s!"Decoded texture: {w}x{h}"
        Texture.destroy tex
      catch e =>
        IO.println s!"Texture.loadFromMemory failed: {e}"
  | .error err =>
      IO.println s!"HTTP error: {err}"

  HTTP.globalCleanup

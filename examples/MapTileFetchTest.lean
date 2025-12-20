import Afferent
import Afferent.FFI.Texture
import Afferent.Map.TileCoord
import Wisp

open Afferent
open Afferent.FFI
open Afferent.Map

def main : IO Unit := do
  Wisp.FFI.globalInit

  let coord : TileCoord := { x := 655, y := 1583, z := 12 }
  let url := tileUrl coord
  IO.println s!"Fetching z={coord.z} x={coord.x} y={coord.y} -> {url}"

  let client := Wisp.HTTP.Client.new
  let task ← client.get url
  match task.get with
  | .ok response =>
    if response.isSuccess then
      IO.println s!"HTTP ok: {response.body.size} bytes"
      try
        let tex ← Texture.loadFromMemory response.body
        let (w, h) ← Texture.getSize tex
        IO.println s!"Decoded texture: {w}x{h}"
        Texture.destroy tex
      catch e =>
        IO.println s!"Texture.loadFromMemory failed: {e}"
    else
      IO.println s!"HTTP error: {response.status}"
  | .error err =>
    IO.println s!"HTTP error: {err}"

  Wisp.FFI.globalCleanup

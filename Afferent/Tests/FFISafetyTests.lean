/-
  Afferent FFI Safety Tests
  Regression tests for common FFI footguns (e.g., missing init).
-/
import Afferent.Tests.Framework
import Afferent.FFI

namespace Afferent.Tests.FFISafetyTests

open Crucible
open Afferent.Tests

testSuite "FFI Safety Tests"

test "Texture can be cached in IO.Ref" := do
  -- This used to segfault if external-class registration hadn't run yet.
  let tex ← Afferent.FFI.Texture.load "nibble.png"
  let texForDestroy := tex

  let r : IO.Ref (Option Afferent.FFI.Texture) ← IO.mkRef none
  r.set (some tex)

  match (← r.get) with
  | none => throw <| IO.userError "expected cached texture"
  | some t =>
      let (w, h) ← Afferent.FFI.Texture.getSize t
      ensure (w > 0 && h > 0) s!"unexpected texture size {w}x{h}"

  -- Drop cache reference before explicit destroy.
  r.set none
  Afferent.FFI.Texture.destroy texForDestroy

#generate_tests

end Afferent.Tests.FFISafetyTests

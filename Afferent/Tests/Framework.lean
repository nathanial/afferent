/-
  Afferent Test Framework
  Float comparison helpers for unit testing.
  Core test infrastructure is provided by Crucible.
-/
import Crucible

namespace Afferent.Tests

open Crucible

/-- Check if two floats are approximately equal within epsilon. -/
def floatNear (a b : Float) (eps : Float := 0.0001) : Bool :=
  (a - b).abs < eps

/-- Assert that two floats are approximately equal. -/
def shouldBeNear (actual expected : Float) (eps : Float := 0.0001) : IO Unit := do
  if !floatNear actual expected eps then
    throw <| IO.userError s!"Expected {expected} (±{eps}), got {actual}"

end Afferent.Tests

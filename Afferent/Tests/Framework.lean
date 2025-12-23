/-
  Afferent Test Framework
  Float comparison helpers for unit testing.
  Core test infrastructure is provided by Crucible.
-/
import Crucible

namespace Afferent.Tests

open Crucible

/-- Check if two floats are approximately equal within epsilon. -/
abbrev floatNear := Crucible.floatNear

/-- Assert that two floats are approximately equal. -/
abbrev shouldBeNear := Crucible.shouldBeNear

/-- Alias for approximate float equality assertions. -/
abbrev shouldBeApprox := Crucible.shouldBeApprox

end Afferent.Tests

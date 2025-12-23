/-
  Afferent Test Runner
  Entry point for running all tests.
-/
import Afferent.Tests.TessellationTests
import Afferent.Tests.FFISafetyTests
import Afferent.Tests.AssetLoadingTests
import Afferent.Tests.SeascapeSmokeTests
import Crucible

open Crucible
open Afferent.Tests

def main : IO UInt32 := do
  IO.println "╔════════════════════════════════════════╗"
  IO.println "║       Afferent Test Suite              ║"
  IO.println "╚════════════════════════════════════════╝"

  let exitCode ← runAllSuites

  -- Summary
  IO.println ""
  if exitCode == 0 then
    IO.println "✓ All test suites passed!"
  else
    IO.println "✗ Some tests failed"

  return if exitCode > 0 then 1 else 0

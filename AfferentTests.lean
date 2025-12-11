/-
  Afferent Test Runner
  Entry point for running all tests.
-/
import Afferent.Tests.TessellationTests

open Afferent.Tests

def main : IO UInt32 := do
  IO.println "╔════════════════════════════════════════╗"
  IO.println "║       Afferent Test Suite              ║"
  IO.println "╚════════════════════════════════════════╝"

  let mut exitCode : UInt32 := 0

  -- Run tessellation tests
  let tessResult ← TessellationTests.runAllTests
  if tessResult != 0 then exitCode := 1

  -- Summary
  IO.println ""
  if exitCode == 0 then
    IO.println "✓ All test suites passed!"
  else
    IO.println "✗ Some tests failed"

  return exitCode

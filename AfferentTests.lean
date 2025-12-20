/-
  Afferent Test Runner
  Entry point for running all tests.
-/
import Afferent.Tests.TessellationTests
import Afferent.Tests.LayoutTests
import Afferent.Tests.WidgetTests
import Afferent.Tests.FFISafetyTests
import Afferent.Tests.AssetLoadingTests
import Afferent.Tests.SeascapeSmokeTests

open Crucible
open Afferent.Tests

def main : IO UInt32 := do
  IO.println "╔════════════════════════════════════════╗"
  IO.println "║       Afferent Test Suite              ║"
  IO.println "╚════════════════════════════════════════╝"

  let mut exitCode : UInt32 := 0

  -- Run tessellation tests
  exitCode := exitCode + (← runTests "Tessellation Tests" TessellationTests.cases)

  -- Run layout tests
  exitCode := exitCode + (← runTests "Layout Tests" LayoutTests.cases)

  -- Run widget tests
  exitCode := exitCode + (← runTests "Widget Tests" WidgetTests.cases)

  -- Run FFI safety tests
  exitCode := exitCode + (← runTests "FFI Safety Tests" FFISafetyTests.cases)

  -- Run asset loading tests
  exitCode := exitCode + (← runTests "Asset Loading Tests" AssetLoadingTests.cases)

  -- Run Seascape smoke tests (skipped unless AFFERENT_RUN_GPU_TESTS=1)
  exitCode := exitCode + (← runTests "Seascape Smoke Tests" SeascapeSmokeTests.cases)

  -- Summary
  IO.println ""
  if exitCode == 0 then
    IO.println "✓ All test suites passed!"
  else
    IO.println "✗ Some tests failed"

  return if exitCode > 0 then 1 else 0

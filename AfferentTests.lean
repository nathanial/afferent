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

open Afferent.Tests

def main : IO UInt32 := do
  IO.println "╔════════════════════════════════════════╗"
  IO.println "║       Afferent Test Suite              ║"
  IO.println "╚════════════════════════════════════════╝"

  let mut exitCode : UInt32 := 0

  -- Run tessellation tests
  let tessResult ← TessellationTests.runAllTests
  if tessResult != 0 then exitCode := 1

  -- Run layout tests
  let layoutResult ← LayoutTests.runAllTests
  if layoutResult != 0 then exitCode := 1

  -- Run widget tests
  let widgetResult ← WidgetTests.runAllTests
  if widgetResult != 0 then exitCode := 1

  -- Run FFI safety tests
  let ffiResult ← FFISafetyTests.runAllTests
  if ffiResult != 0 then exitCode := 1

  -- Run asset loading tests
  let assetResult ← AssetLoadingTests.runAllTests
  if assetResult != 0 then exitCode := 1

  -- Run Seascape smoke tests (skipped unless AFFERENT_RUN_GPU_TESTS=1)
  let seascapeResult ← SeascapeSmokeTests.runAllTests
  if seascapeResult != 0 then exitCode := 1

  -- Summary
  IO.println ""
  if exitCode == 0 then
    IO.println "✓ All test suites passed!"
  else
    IO.println "✗ Some tests failed"

  return exitCode

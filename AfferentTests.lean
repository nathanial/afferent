/-
  Afferent Test Runner
  Entry point for running all tests.
-/
import Afferent.Tests.TessellationTests
import Afferent.Tests.AssetLoadingTests
import Afferent.Tests.LayoutTests
import Afferent.Tests.CanvasStateTests
import Afferent.Tests.FontTests
import Afferent.Tests.RenderSmokeTests
import Afferent.Tests.CSSTests
import Afferent.Tests.ScrollContainerTests
import Afferent.Tests.TooltipTests
import Afferent.Tests.MenuTests
import Afferent.Tests.MenuBarTests
import Afferent.Tests.DropdownTests
import Afferent.Tests.TableTests
import Afferent.Tests.ListBoxTests
import Afferent.Tests.TreeViewTests
import Afferent.Tests.ColorPickerTests
import Afferent.Tests.ReactiveLayoutTests
import Afferent.Tests.DynWidgetTests
import Afferent.Tests.CoalescingTests
import Afferent.Tests.ShaderDSLTests
import Afferent.Tests.TextInputTests
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

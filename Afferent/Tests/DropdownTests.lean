/-
  Dropdown Widget Tests
  Hit testing and scroll interaction for dropdown menus.
-/
import Afferent.Tests.Framework
import Afferent.Arbor
import Afferent.Arbor.Widget.DSL
import Afferent.Canopy
import Afferent.Canopy.Reactive.Component
import Afferent.Canopy.Widget.Input.Dropdown
import Trellis

namespace Afferent.Tests.DropdownTests

open Crucible
open Afferent.Tests
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Trellis

testSuite "Dropdown Tests"

/-- Test font ID for widget building tests. -/
def testFont : FontId := { id := 0, name := "test", size := 14.0 }

/-- Test theme for widget tests. -/
def testTheme : Theme := { Theme.dark with font := testFont, smallFont := testFont }

/-- Default option name generator for dropdown items. -/
def optionNameFn (i : Nat) : String := s!"dropdown-option-{i}"

/-- Build an open dropdown visual for testing. -/
def openDropdown (options : Array String) (selectedIndex : Nat := 0) : WidgetBuilder := do
  dropdownVisual "dropdown" "dropdown-trigger" optionNameFn options selectedIndex true none testTheme {}

test "dropdown hitTestPath finds open menu item" := do
  let options := #["One", "Two", "Three"]
  let builder := openDropdown options
  let (widget, _) ← builder.run {}

  let viewportW := 240.0
  let viewportH := 180.0
  let measureResult : MeasureResult := measureWidget (M := Id) widget viewportW viewportH
  let layouts := Trellis.layout measureResult.node viewportW viewportH

  let optionId ← match findWidgetIdByName measureResult.widget (optionNameFn 1) with
    | some wid => pure wid
    | none =>
        ensure false "Expected dropdown option 1 to exist"
        pure 0

  let optionLayout := layouts.get! optionId
  let clickX := optionLayout.borderRect.x + optionLayout.borderRect.width / 2
  let clickY := optionLayout.borderRect.y + optionLayout.borderRect.height / 2

  let path := hitTestPath measureResult.widget layouts clickX clickY
  ensure (path.any (· == optionId))
    s!"Expected hit path to include option id {optionId}, got {path}"

test "dropdown hitTestPathIndexed finds open menu item" := do
  let options := #["One", "Two", "Three"]
  let builder := openDropdown options
  let (widget, _) ← builder.run {}

  let viewportW := 240.0
  let viewportH := 180.0
  let measureResult : MeasureResult := measureWidget (M := Id) widget viewportW viewportH
  let layouts := Trellis.layout measureResult.node viewportW viewportH
  let hitIndex := buildHitTestIndex measureResult.widget layouts

  let optionId ← match findWidgetIdByName measureResult.widget (optionNameFn 1) with
    | some wid => pure wid
    | none =>
        ensure false "Expected dropdown option 1 to exist"
        pure 0

  let optionLayout := layouts.get! optionId
  let clickX := optionLayout.borderRect.x + optionLayout.borderRect.width / 2
  let clickY := optionLayout.borderRect.y + optionLayout.borderRect.height / 2

  let path := hitTestPathIndexed hitIndex clickX clickY
  ensure (path.any (· == optionId))
    s!"Expected indexed hit path to include option id {optionId}, got {path}"

test "dropdown hit testing accounts for scroll offset" := do
  let options := #["One", "Two", "Three"]
  let dropdownBuilder := openDropdown options
  let spacerHeight := 180.0
  let contentBuilder := column (gap := 0) (style := { width := .percent 1.0 }) #[
    spacer 0 spacerHeight,
    dropdownBuilder
  ]

  let viewportW := 240.0
  let viewportH := 140.0
  let contentH := 400.0
  let scrollOffset := 120.0
  let scrollStyle : BoxStyle := { minWidth := some viewportW, minHeight := some viewportH }
  let scrollState : ScrollState := { offsetY := scrollOffset }
  let scrollBuilder := scroll scrollStyle viewportW contentH scrollState {} contentBuilder
  let (widget, _) ← scrollBuilder.run {}

  let measureResult : MeasureResult := measureWidget (M := Id) widget viewportW viewportH
  let layouts := Trellis.layout measureResult.node viewportW viewportH

  let optionId ← match findWidgetIdByName measureResult.widget (optionNameFn 1) with
    | some wid => pure wid
    | none =>
        ensure false "Expected dropdown option 1 to exist"
        pure 0

  let optionLayout := layouts.get! optionId
  let clickX := optionLayout.borderRect.x + optionLayout.borderRect.width / 2
  let clickY := optionLayout.borderRect.y + optionLayout.borderRect.height / 2 - scrollOffset

  let path := hitTestPath measureResult.widget layouts clickX clickY
  ensure (path.any (· == optionId))
    s!"Expected scrolled hit path to include option id {optionId}, got {path}"

test "dropdown hitTestPathIndexed accounts for scroll offset" := do
  let options := #["One", "Two", "Three"]
  let dropdownBuilder := openDropdown options
  let spacerHeight := 180.0
  let contentBuilder := column (gap := 0) (style := { width := .percent 1.0 }) #[
    spacer 0 spacerHeight,
    dropdownBuilder
  ]

  let viewportW := 240.0
  let viewportH := 140.0
  let contentH := 400.0
  let scrollOffset := 120.0
  let scrollStyle : BoxStyle := { minWidth := some viewportW, minHeight := some viewportH }
  let scrollState : ScrollState := { offsetY := scrollOffset }
  let scrollBuilder := scroll scrollStyle viewportW contentH scrollState {} contentBuilder
  let (widget, _) ← scrollBuilder.run {}

  let measureResult : MeasureResult := measureWidget (M := Id) widget viewportW viewportH
  let layouts := Trellis.layout measureResult.node viewportW viewportH
  let hitIndex := buildHitTestIndex measureResult.widget layouts

  let optionId ← match findWidgetIdByName measureResult.widget (optionNameFn 1) with
    | some wid => pure wid
    | none =>
        ensure false "Expected dropdown option 1 to exist"
        pure 0

  let optionLayout := layouts.get! optionId
  let clickX := optionLayout.borderRect.x + optionLayout.borderRect.width / 2
  let clickY := optionLayout.borderRect.y + optionLayout.borderRect.height / 2 - scrollOffset

  let path := hitTestPathIndexed hitIndex clickX clickY
  ensure (path.any (· == optionId))
    s!"Expected indexed scrolled hit path to include option id {optionId}, got {path}"

end Afferent.Tests.DropdownTests

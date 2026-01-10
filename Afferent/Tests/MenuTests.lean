/-
  Menu Widget Tests
  Unit tests for the menu widget functionality.
-/
import Afferent.Tests.Framework
import Afferent.Canopy.Widget.Menu

namespace Afferent.Tests.MenuTests

open Crucible
open Afferent.Tests
open Afferent.Canopy

testSuite "Menu Tests"

/-! ## MenuItem Tests -/

test "MenuItem.action default enabled" := do
  let item := MenuItem.action "Test"
  match item with
  | .action label enabled =>
    ensure (label == "Test") "Label should be 'Test'"
    ensure enabled "Default should be enabled"
  | .separator => ensure false "Should not be separator"

test "MenuItem.action can be disabled" := do
  let item := MenuItem.action "Disabled" (enabled := false)
  match item with
  | .action _ enabled =>
    ensure (!enabled) "Should be disabled"
  | .separator => ensure false "Should not be separator"

test "MenuItem.separator" := do
  let item : MenuItem := .separator
  match item with
  | .separator => ensure true "Should be separator"
  | .action .. => ensure false "Should not be action"

/-! ## MenuConfig Tests -/

test "MenuConfig default values" := do
  let config : MenuConfig := {}
  ensure (config.minWidth == 180.0) s!"Default minWidth should be 180, got {config.minWidth}"
  ensure (config.itemHeight == 32.0) s!"Default itemHeight should be 32, got {config.itemHeight}"
  ensure (config.separatorHeight == 9.0) s!"Default separatorHeight should be 9, got {config.separatorHeight}"
  ensure (config.cornerRadius == 4.0) s!"Default cornerRadius should be 4, got {config.cornerRadius}"

test "MenuConfig custom values" := do
  let config : MenuConfig := {
    minWidth := 200.0
    itemHeight := 40.0
    separatorHeight := 12.0
    cornerRadius := 8.0
  }
  ensure (config.minWidth == 200.0) "minWidth should be 200"
  ensure (config.itemHeight == 40.0) "itemHeight should be 40"
  ensure (config.separatorHeight == 12.0) "separatorHeight should be 12"
  ensure (config.cornerRadius == 8.0) "cornerRadius should be 8"

/-! ## Menu.calculateHeight Tests -/

test "calculateHeight with only actions" := do
  let config := Menu.defaultConfig
  let items := #[MenuItem.action "A", MenuItem.action "B", MenuItem.action "C"]
  let height := Menu.calculateHeight items config
  let expected := config.itemHeight * 3
  ensure (height == expected) s!"Expected height {expected}, got {height}"

test "calculateHeight with only separators" := do
  let config := Menu.defaultConfig
  let items := #[MenuItem.separator, MenuItem.separator]
  let height := Menu.calculateHeight items config
  let expected := config.separatorHeight * 2
  ensure (height == expected) s!"Expected height {expected}, got {height}"

test "calculateHeight with mixed items" := do
  let config := Menu.defaultConfig
  let items := #[
    MenuItem.action "Cut",
    MenuItem.action "Copy",
    MenuItem.separator,
    MenuItem.action "Paste"
  ]
  let height := Menu.calculateHeight items config
  let expected := config.itemHeight * 3 + config.separatorHeight
  ensure (height == expected) s!"Expected height {expected}, got {height}"

test "calculateHeight empty array" := do
  let config := Menu.defaultConfig
  let items : Array MenuItem := #[]
  let height := Menu.calculateHeight items config
  ensure (height == 0.0) s!"Expected height 0, got {height}"

/-! ## Menu.isEnabledAction Tests -/

test "isEnabledAction returns true for enabled action" := do
  let items := #[MenuItem.action "Test" (enabled := true)]
  let result := Menu.isEnabledAction items 0
  ensure result "Should return true for enabled action"

test "isEnabledAction returns false for disabled action" := do
  let items := #[MenuItem.action "Test" (enabled := false)]
  let result := Menu.isEnabledAction items 0
  ensure (!result) "Should return false for disabled action"

test "isEnabledAction returns false for separator" := do
  let items := #[MenuItem.separator]
  let result := Menu.isEnabledAction items 0
  ensure (!result) "Should return false for separator"

test "isEnabledAction returns false for out of bounds" := do
  let items := #[MenuItem.action "Test"]
  let result := Menu.isEnabledAction items 5
  ensure (!result) "Should return false for out of bounds index"

/-! ## Menu Item Pattern Tests -/

test "typical edit menu items" := do
  let items := #[
    MenuItem.action "Cut",
    MenuItem.action "Copy",
    MenuItem.action "Paste",
    MenuItem.separator,
    MenuItem.action "Select All",
    MenuItem.separator,
    MenuItem.action "Delete" (enabled := false)
  ]
  ensure (items.size == 7) "Should have 7 items"
  let config := Menu.defaultConfig
  let height := Menu.calculateHeight items config
  let expectedHeight := config.itemHeight * 5 + config.separatorHeight * 2
  ensure (height == expectedHeight) s!"Expected height {expectedHeight}, got {height}"
  -- Check that Delete is disabled
  ensure (!Menu.isEnabledAction items 6) "Delete should be disabled"
  -- Check that Cut is enabled
  ensure (Menu.isEnabledAction items 0) "Cut should be enabled"
  -- Check that separators are not enabled actions
  ensure (!Menu.isEnabledAction items 3) "Separator should not be enabled action"

#generate_tests

end Afferent.Tests.MenuTests

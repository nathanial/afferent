/-
  Menu Widget Tests
  Unit tests for the menu widget functionality.
-/
import Afferent.Tests.Framework
import Afferent.Canopy.Widget.Navigation.Menu

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
  | .submenu .. => ensure false "Should not be submenu"

test "MenuItem.action can be disabled" := do
  let item := MenuItem.action "Disabled" (enabled := false)
  match item with
  | .action _ enabled =>
    ensure (!enabled) "Should be disabled"
  | .separator => ensure false "Should not be separator"
  | .submenu .. => ensure false "Should not be submenu"

test "MenuItem.separator" := do
  let item : MenuItem := .separator
  match item with
  | .separator => ensure true "Should be separator"
  | .action .. => ensure false "Should not be action"
  | .submenu .. => ensure false "Should not be submenu"

test "MenuItem.submenu default enabled" := do
  let item := MenuItem.submenu "Format" #[MenuItem.action "Bold"]
  match item with
  | .submenu label items enabled =>
    ensure (label == "Format") "Label should be 'Format'"
    ensure enabled "Default should be enabled"
    ensure (items.size == 1) "Should have 1 item"
  | .action .. => ensure false "Should not be action"
  | .separator => ensure false "Should not be separator"

test "MenuItem.submenu can be disabled" := do
  let item := MenuItem.submenu "Disabled" #[] (enabled := false)
  match item with
  | .submenu _ _ enabled =>
    ensure (!enabled) "Should be disabled"
  | .action .. => ensure false "Should not be action"
  | .separator => ensure false "Should not be separator"

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

/-! ## Submenu Tests -/

test "calculateHeight with submenus" := do
  let config := Menu.defaultConfig
  let items := #[
    MenuItem.action "Action",
    MenuItem.submenu "Submenu" #[MenuItem.action "Sub1", MenuItem.action "Sub2"],
    MenuItem.separator
  ]
  -- Root menu height: 1 action + 1 submenu (treated as itemHeight) + 1 separator
  let height := Menu.calculateHeight items config
  let expected := config.itemHeight * 2 + config.separatorHeight
  ensure (height == expected) s!"Expected height {expected}, got {height}"

test "isEnabledAction returns false for submenu" := do
  let items := #[MenuItem.submenu "Test" #[MenuItem.action "Sub"]]
  let result := Menu.isEnabledAction items 0
  ensure (!result) "Should return false for submenu"

/-! ## MenuPath Helper Tests -/

test "getItemAtPath returns root item" := do
  let items := #[MenuItem.action "A", MenuItem.action "B"]
  match Menu.getItemAtPath items #[1] with
  | some (.action label _) => ensure (label == "B") "Should be item B"
  | _ => ensure false "Should find action B"

test "getItemAtPath returns nested item" := do
  let items := #[
    MenuItem.submenu "Format" #[
      MenuItem.action "Bold",
      MenuItem.action "Italic"
    ]
  ]
  match Menu.getItemAtPath items #[0, 1] with
  | some (.action label _) => ensure (label == "Italic") "Should be Italic"
  | _ => ensure false "Should find Italic"

test "getItemAtPath returns none for invalid path" := do
  let items := #[MenuItem.action "A"]
  match Menu.getItemAtPath items #[5] with
  | none => ensure true "Should return none for invalid index"
  | some _ => ensure false "Should not find item"

test "getItemAtPath returns none for empty path" := do
  let items := #[MenuItem.action "A"]
  match Menu.getItemAtPath items #[] with
  | none => ensure true "Should return none for empty path"
  | some _ => ensure false "Should not find item"

test "isEnabledActionAtPath for nested action" := do
  let items := #[
    MenuItem.submenu "Format" #[
      MenuItem.action "Bold" (enabled := true),
      MenuItem.action "Disabled" (enabled := false)
    ]
  ]
  ensure (Menu.isEnabledActionAtPath items #[0, 0]) "Bold should be enabled"
  ensure (!Menu.isEnabledActionAtPath items #[0, 1]) "Disabled should not be enabled"

test "isEnabledSubmenuAtPath" := do
  let items := #[
    MenuItem.submenu "Enabled" #[] (enabled := true),
    MenuItem.submenu "Disabled" #[] (enabled := false),
    MenuItem.action "Action"
  ]
  ensure (Menu.isEnabledSubmenuAtPath items #[0]) "First submenu should be enabled"
  ensure (!Menu.isEnabledSubmenuAtPath items #[1]) "Second submenu should be disabled"
  ensure (!Menu.isEnabledSubmenuAtPath items #[2]) "Action should not be a submenu"

test "getItemsAtPath returns root items" := do
  let items := #[MenuItem.action "A", MenuItem.action "B"]
  let result := Menu.getItemsAtPath items #[]
  ensure (result.size == 2) "Should return both root items"

test "getItemsAtPath returns submenu items" := do
  let items := #[
    MenuItem.submenu "Format" #[
      MenuItem.action "Bold",
      MenuItem.action "Italic",
      MenuItem.action "Underline"
    ]
  ]
  let result := Menu.getItemsAtPath items #[0]
  ensure (result.size == 3) s!"Should return 3 submenu items, got {result.size}"

test "isPathPrefix" := do
  ensure (Menu.isPathPrefix #[] #[0]) "Empty is prefix of any"
  ensure (Menu.isPathPrefix #[0] #[0, 1]) "[0] is prefix of [0,1]"
  ensure (Menu.isPathPrefix #[0, 1] #[0, 1]) "[0,1] is prefix of [0,1]"
  ensure (!Menu.isPathPrefix #[0, 1] #[0]) "[0,1] is not prefix of [0]"
  ensure (!Menu.isPathPrefix #[1] #[0, 1]) "[1] is not prefix of [0,1]"

test "calculateHeightAtPath" := do
  let config := Menu.defaultConfig
  let items := #[
    MenuItem.submenu "Format" #[
      MenuItem.action "Bold",
      MenuItem.separator,
      MenuItem.action "Italic"
    ]
  ]
  let height := Menu.calculateHeightAtPath items #[0] config
  let expected := config.itemHeight * 2 + config.separatorHeight
  ensure (height == expected) s!"Expected height {expected}, got {height}"



end Afferent.Tests.MenuTests

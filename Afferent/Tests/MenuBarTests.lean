/-
  MenuBar Widget Tests
  Unit tests for the menu bar widget functionality.
-/
import Afferent.Tests.Framework
import Afferent.Canopy.Widget.Navigation.MenuBar

namespace Afferent.Tests.MenuBarTests

open Crucible
open Afferent.Tests
open Afferent.Canopy

testSuite "MenuBar Tests"

/-! ## MenuBarMenu Tests -/

test "MenuBarMenu default enabled" := do
  let menu : MenuBarMenu := { label := "File", items := #[] }
  ensure menu.enabled "Default should be enabled"
  ensure (menu.label == "File") "Label should be 'File'"
  ensure (menu.items.size == 0) "Items should be empty"

test "MenuBarMenu with items" := do
  let menu : MenuBarMenu := {
    label := "Edit"
    items := #[MenuItem.action "Cut", MenuItem.action "Copy"]
    enabled := true
  }
  ensure (menu.items.size == 2) "Should have 2 items"

test "MenuBarMenu can be disabled" := do
  let menu : MenuBarMenu := { label := "Help", items := #[], enabled := false }
  ensure (!menu.enabled) "Should be disabled"

/-! ## MenuBarPath Tests -/

test "MenuBarPath construction" := do
  let path : MenuBarPath := { menuIndex := 1, itemPath := #[2, 0] }
  ensure (path.menuIndex == 1) "Menu index should be 1"
  ensure (path.itemPath == #[2, 0]) "Item path should be [2, 0]"

test "MenuBarPath to root item" := do
  let path : MenuBarPath := { menuIndex := 0, itemPath := #[3] }
  ensure (path.menuIndex == 0) "Menu index should be 0"
  ensure (path.itemPath.size == 1) "Path should have 1 element"

test "MenuBarPath to nested submenu item" := do
  let path : MenuBarPath := { menuIndex := 2, itemPath := #[0, 1, 2] }
  ensure (path.itemPath.size == 3) "Path should have 3 elements for nested item"

/-! ## MenuBarConfig Tests -/

test "MenuBarConfig defaults" := do
  let config := MenuBar.defaultConfig
  ensure (config.triggerHeight == 28.0) s!"Default trigger height should be 28, got {config.triggerHeight}"
  ensure (config.triggerPadding == 12.0) s!"Default trigger padding should be 12, got {config.triggerPadding}"
  ensure (config.menuGap == 4.0) s!"Default menu gap should be 4, got {config.menuGap}"

test "MenuBarConfig custom values" := do
  let config : MenuBarConfig := {
    triggerHeight := 32.0
    triggerPadding := 16.0
    menuGap := 8.0
  }
  ensure (config.triggerHeight == 32.0) "Trigger height should be 32"
  ensure (config.triggerPadding == 16.0) "Trigger padding should be 16"
  ensure (config.menuGap == 8.0) "Menu gap should be 8"

/-! ## Typical Menu Bar Configuration Tests -/

test "typical application menu bar" := do
  let fileMenu : MenuBarMenu := {
    label := "File"
    items := #[
      MenuItem.action "New",
      MenuItem.action "Open",
      MenuItem.action "Save",
      MenuItem.separator,
      MenuItem.action "Exit"
    ]
  }
  let editMenu : MenuBarMenu := {
    label := "Edit"
    items := #[
      MenuItem.action "Cut",
      MenuItem.action "Copy",
      MenuItem.action "Paste",
      MenuItem.separator,
      MenuItem.submenu "Format" #[
        MenuItem.action "Bold",
        MenuItem.action "Italic"
      ]
    ]
  }
  let viewMenu : MenuBarMenu := {
    label := "View"
    items := #[
      MenuItem.action "Zoom In",
      MenuItem.action "Zoom Out"
    ]
  }
  let menus := #[fileMenu, editMenu, viewMenu]
  ensure (menus.size == 3) "Should have 3 menus"
  ensure (fileMenu.items.size == 5) "File menu should have 5 items"
  ensure (editMenu.items.size == 5) "Edit menu should have 5 items"
  ensure (viewMenu.items.size == 2) "View menu should have 2 items"

test "menu bar with disabled menu" := do
  let menus : Array MenuBarMenu := #[
    { label := "File", items := #[MenuItem.action "New"] },
    { label := "Edit", items := #[MenuItem.action "Cut"], enabled := false },
    { label := "View", items := #[MenuItem.action "Zoom"] }
  ]
  ensure menus[0]!.enabled "File should be enabled"
  ensure (!menus[1]!.enabled) "Edit should be disabled"
  ensure menus[2]!.enabled "View should be enabled"



end Afferent.Tests.MenuBarTests

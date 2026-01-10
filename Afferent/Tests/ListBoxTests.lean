/-
  ListBox Widget Tests
  Unit tests for the list box widget functionality.
-/
import Afferent.Tests.Framework
import Afferent.Canopy.Widget.ListBox

namespace Afferent.Tests.ListBoxTests

open Crucible
open Afferent.Tests
open Afferent.Canopy

testSuite "ListBox Tests"

/-! ## ListBoxSelectionMode Tests -/

test "ListBoxSelectionMode.single" := do
  let mode := ListBoxSelectionMode.single
  ensure (mode == .single) "Should be single"

test "ListBoxSelectionMode.multiple" := do
  let mode := ListBoxSelectionMode.multiple
  ensure (mode == .multiple) "Should be multiple"

/-! ## ListBoxConfig Tests -/

test "ListBoxConfig default values" := do
  let config := ListBox.defaultConfig
  ensure (config.itemHeight == 32.0) s!"Default item height should be 32, got {config.itemHeight}"
  ensure (config.itemPadding == 12.0) s!"Default item padding should be 12, got {config.itemPadding}"
  ensure (config.maxVisibleItems == 6) s!"Default max visible items should be 6, got {config.maxVisibleItems}"
  ensure (config.selectionMode == .single) "Default selection mode should be single"
  ensure (config.borderWidth == 1.0) s!"Default border width should be 1, got {config.borderWidth}"

test "ListBoxConfig custom values" := do
  let config : ListBoxConfig := {
    itemHeight := 40.0
    itemPadding := 16.0
    maxVisibleItems := 10
    selectionMode := .multiple
    borderWidth := 2.0
  }
  ensure (config.itemHeight == 40.0) "Item height should be 40"
  ensure (config.itemPadding == 16.0) "Item padding should be 16"
  ensure (config.maxVisibleItems == 10) "Max visible items should be 10"
  ensure (config.selectionMode == .multiple) "Selection mode should be multiple"
  ensure (config.borderWidth == 2.0) "Border width should be 2"

/-! ## Selection Logic Tests -/

test "updateSelection single mode replaces selection" := do
  let result := ListBox.updateSelection .single 0 #[]
  ensure (result == #[0]) "Single mode should select clicked item"
  let result2 := ListBox.updateSelection .single 2 #[0]
  ensure (result2 == #[2]) "Single mode should replace selection"
  let result3 := ListBox.updateSelection .single 1 #[1]
  ensure (result3 == #[1]) "Clicking same item should keep it selected"

test "updateSelection multiple mode toggles selection" := do
  let result := ListBox.updateSelection .multiple 0 #[]
  ensure (result == #[0]) "Multiple mode should add to selection"
  let result2 := ListBox.updateSelection .multiple 2 #[0]
  ensure (result2 == #[0, 2]) "Multiple mode should add second item"
  let result3 := ListBox.updateSelection .multiple 0 #[0, 2]
  ensure (result3 == #[2]) "Multiple mode should remove clicked item"

test "updateSelection multiple mode with many items" := do
  let result := ListBox.updateSelection .multiple 1 #[0, 2, 4]
  ensure (result == #[0, 2, 4, 1]) "Should add item to existing selection"
  let result2 := ListBox.updateSelection .multiple 2 #[0, 2, 4]
  ensure (result2 == #[0, 4]) "Should remove item from selection"

/-! ## Typical ListBox Configuration Tests -/

test "typical fruits list" := do
  let items : Array String := #["Apple", "Banana", "Cherry", "Date", "Elderberry"]
  ensure (items.size == 5) "Should have 5 items"
  ensure (items[0]! == "Apple") "First item should be 'Apple'"
  ensure (items[4]! == "Elderberry") "Last item should be 'Elderberry'"

test "empty list" := do
  let items : Array String := #[]
  ensure (items.size == 0) "Should have 0 items"

test "single item list" := do
  let items : Array String := #["Only One"]
  ensure (items.size == 1) "Should have 1 item"
  ensure (items[0]! == "Only One") "Item should be 'Only One'"

test "selection on empty array" := do
  let result := ListBox.updateSelection .single 0 #[]
  ensure (result == #[0]) "Should select first item"
  let result2 := ListBox.updateSelection .multiple 5 #[]
  ensure (result2 == #[5]) "Should select item at index 5"

#generate_tests

end Afferent.Tests.ListBoxTests

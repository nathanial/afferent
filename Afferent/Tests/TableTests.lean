/-
  Table Widget Tests
  Unit tests for the table widget functionality.
-/
import Afferent.Tests.Framework
import Afferent.Canopy.Widget.Data.Table

namespace Afferent.Tests.TableTests

open Crucible
open Afferent.Tests
open Afferent.Canopy

testSuite "Table Tests"

/-! ## SelectionMode Tests -/

test "SelectionMode.none" := do
  let mode := SelectionMode.none
  ensure (mode == .none) "Should be none"

test "SelectionMode.single" := do
  let mode := SelectionMode.single
  ensure (mode == .single) "Should be single"

test "SelectionMode.multiple" := do
  let mode := SelectionMode.multiple
  ensure (mode == .multiple) "Should be multiple"

/-! ## TableColumn Tests -/

test "TableColumn default width is none" := do
  let col : TableColumn := { header := "Name" }
  ensure col.width.isNone "Default width should be none"
  ensure (col.header == "Name") "Header should be 'Name'"

test "TableColumn with explicit width" := do
  let col : TableColumn := { header := "Email", width := some 200.0 }
  ensure (col.width == some 200.0) "Width should be 200"

/-! ## TableConfig Tests -/

test "TableConfig default values" := do
  let config := Table.defaultConfig
  ensure (config.headerHeight == 36.0) s!"Default header height should be 36, got {config.headerHeight}"
  ensure (config.rowHeight == 32.0) s!"Default row height should be 32, got {config.rowHeight}"
  ensure (config.cellPadding == 12.0) s!"Default cell padding should be 12, got {config.cellPadding}"
  ensure (config.borderWidth == 1.0) s!"Default border width should be 1, got {config.borderWidth}"
  ensure (!config.showRowNumbers) "Default showRowNumbers should be false"
  ensure (config.selectionMode == .single) "Default selection mode should be single"

test "TableConfig custom values" := do
  let config : TableConfig := {
    headerHeight := 40.0
    rowHeight := 36.0
    cellPadding := 16.0
    borderWidth := 2.0
    showRowNumbers := true
    selectionMode := .multiple
  }
  ensure (config.headerHeight == 40.0) "Header height should be 40"
  ensure (config.rowHeight == 36.0) "Row height should be 36"
  ensure (config.cellPadding == 16.0) "Cell padding should be 16"
  ensure (config.borderWidth == 2.0) "Border width should be 2"
  ensure config.showRowNumbers "showRowNumbers should be true"
  ensure (config.selectionMode == .multiple) "Selection mode should be multiple"

/-! ## Selection Logic Tests -/

test "updateSelection none mode ignores clicks" := do
  let result := Table.updateSelection .none 0 #[]
  ensure (result.size == 0) "None mode should not add selection"
  let result2 := Table.updateSelection .none 1 #[0, 2]
  ensure (result2 == #[0, 2]) "None mode should preserve existing selection"

test "updateSelection single mode replaces selection" := do
  let result := Table.updateSelection .single 0 #[]
  ensure (result == #[0]) "Single mode should select clicked row"
  let result2 := Table.updateSelection .single 2 #[0]
  ensure (result2 == #[2]) "Single mode should replace selection"
  let result3 := Table.updateSelection .single 1 #[1]
  ensure (result3 == #[1]) "Clicking same row should keep it selected"

test "updateSelection multiple mode toggles selection" := do
  let result := Table.updateSelection .multiple 0 #[]
  ensure (result == #[0]) "Multiple mode should add to selection"
  let result2 := Table.updateSelection .multiple 2 #[0]
  ensure (result2 == #[0, 2]) "Multiple mode should add second row"
  let result3 := Table.updateSelection .multiple 0 #[0, 2]
  ensure (result3 == #[2]) "Multiple mode should remove clicked row"

/-! ## Typical Table Configuration Tests -/

test "typical users table" := do
  let columns : Array TableColumn := #[
    { header := "Name" },
    { header := "Email", width := some 180.0 },
    { header := "Role" },
    { header := "Status" }
  ]
  let rows : Array (Array String) := #[
    #["Alice", "alice@example.com", "Admin", "Active"],
    #["Bob", "bob@example.com", "User", "Active"],
    #["Carol", "carol@example.com", "User", "Inactive"]
  ]
  ensure (columns.size == 4) "Should have 4 columns"
  ensure (rows.size == 3) "Should have 3 rows"
  ensure (rows[0]!.size == 4) "Each row should have 4 cells"
  ensure (columns[1]!.width == some 180.0) "Email column should have fixed width"

test "empty table" := do
  let columns : Array TableColumn := #[{ header := "Column" }]
  let rows : Array (Array String) := #[]
  ensure (columns.size == 1) "Should have 1 column"
  ensure (rows.size == 0) "Should have 0 rows"

test "single cell table" := do
  let columns : Array TableColumn := #[{ header := "Value" }]
  let rows : Array (Array String) := #[#["42"]]
  ensure (columns.size == 1) "Should have 1 column"
  ensure (rows.size == 1) "Should have 1 row"
  ensure (rows[0]![0]! == "42") "Cell value should be '42'"



end Afferent.Tests.TableTests

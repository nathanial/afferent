/-
  Canopy Table Widget
  Displays tabular data with headers, rows, and row selection.
-/
import Reactive
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Reactive.Component

namespace Afferent.Canopy

open Afferent.Arbor hiding Event
open Reactive Reactive.Host
open Afferent.Canopy.Reactive
open Trellis

/-- Selection mode for table rows. -/
inductive SelectionMode where
  | none      -- No selection allowed
  | single    -- Only one row at a time
  | multiple  -- Multiple rows can be selected (toggle)
deriving Repr, Inhabited, BEq

/-- Column definition for a table. -/
structure TableColumn where
  header : String
  width : Option Float := none  -- None = flexible (equal distribution)
deriving Repr, Inhabited

/-- Configuration for table appearance. -/
structure TableConfig where
  headerHeight : Float := 36.0
  rowHeight : Float := 32.0
  cellPadding : Float := 12.0
  borderWidth : Float := 1.0
  showRowNumbers : Bool := false
  selectionMode : SelectionMode := .single
deriving Repr, Inhabited

/-- Result from table widget. -/
structure TableResult where
  /-- Fires when a row is clicked (row index). -/
  onRowSelect : Reactive.Event Spider Nat
  /-- Currently selected row indices. -/
  selectedRows : Reactive.Dynamic Spider (Array Nat)
  /-- Currently hovered row index. -/
  hoveredRow : Reactive.Dynamic Spider (Option Nat)

namespace Table

/-- Default table configuration. -/
def defaultConfig : TableConfig := {}

/-- Update selection based on click and selection mode. -/
def updateSelection (mode : SelectionMode) (clickedRow : Nat) (current : Array Nat) : Array Nat :=
  match mode with
  | .none => current
  | .single => #[clickedRow]
  | .multiple =>
    if current.contains clickedRow then
      current.filter (· != clickedRow)
    else
      current.push clickedRow

end Table

/-- Build a single table cell. -/
def tableCellVisual (name : String) (content : String) (isHeader : Bool)
    (colWidth : Option Float) (theme : Theme)
    (config : TableConfig := Table.defaultConfig) : WidgetBuilder := do
  let textColor := theme.text
  let cellStyle : BoxStyle := {
    padding := EdgeInsets.symmetric config.cellPadding 8
    minHeight := some (if isHeader then config.headerHeight else config.rowHeight)
    width := match colWidth with
      | some w => .length w
      | none => .auto
    flexItem := if colWidth.isNone then some (FlexItem.growing 1) else none
  }
  let wid ← freshId
  let props : FlexContainer := {
    FlexContainer.row 0 with
    alignItems := .center
  }
  let textWidget ← text' content theme.font textColor .left
  pure (.flex wid (some name) props cellStyle #[textWidget])

/-- Build a table row (header or data). -/
def tableRowVisual (rowName : String) (cellNameFn : Nat → String)
    (cells : Array String) (columns : Array TableColumn) (isHeader : Bool)
    (isHovered : Bool) (isSelected : Bool) (isAlternate : Bool)
    (theme : Theme) (config : TableConfig := Table.defaultConfig) : WidgetBuilder := do
  -- Determine background color
  let bgColor :=
    if isHeader then theme.panel.background
    else if isSelected then theme.primary.background.withAlpha 0.15
    else if isHovered then theme.input.backgroundHover
    else if isAlternate then theme.panel.background.withAlpha 0.3
    else Color.transparent

  let rowStyle : BoxStyle := {
    backgroundColor := some bgColor
    borderColor := some (theme.input.border.withAlpha 0.3)
    borderWidth := if isHeader then 0 else config.borderWidth
    width := .percent 1.0
  }

  -- Build cells
  let mut cellWidgets : Array Widget := #[]
  for i in [:cells.size] do
    let content := cells.getD i ""
    let colWidth := match columns[i]? with
      | some col => col.width
      | none => none
    let cellWidget ← tableCellVisual (cellNameFn i) content isHeader colWidth theme config
    cellWidgets := cellWidgets.push cellWidget

  let wid ← freshId
  let props : FlexContainer := { direction := .row, gap := 0 }
  pure (.flex wid (some rowName) props rowStyle cellWidgets)

/-- Build the header row. -/
def tableHeaderVisual (rowName : String) (cellNameFn : Nat → String)
    (columns : Array TableColumn) (theme : Theme)
    (config : TableConfig := Table.defaultConfig) : WidgetBuilder := do
  let headerStyle : BoxStyle := {
    backgroundColor := some theme.panel.background
    borderColor := some (theme.input.border.withAlpha 0.5)
    borderWidth := config.borderWidth
    width := .percent 1.0
  }

  let mut cellWidgets : Array Widget := #[]
  for i in [:columns.size] do
    match columns[i]? with
    | some col =>
      let cellWidget ← tableCellVisual (cellNameFn i) col.header true col.width theme config
      cellWidgets := cellWidgets.push cellWidget
    | none => pure ()

  let wid ← freshId
  let props : FlexContainer := { direction := .row, gap := 0 }
  pure (.flex wid (some rowName) props headerStyle cellWidgets)

/-- Build the complete table visual. -/
def tableVisual (containerName : String) (headerRowName : String)
    (headerCellNameFn : Nat → String) (rowNameFn : Nat → String)
    (cellNameFn : Nat → Nat → String) (columns : Array TableColumn)
    (rows : Array (Array String)) (selectedRows : Array Nat)
    (hoveredRow : Option Nat) (theme : Theme)
    (config : TableConfig := Table.defaultConfig) : WidgetBuilder := do
  -- Build header row
  let headerWidget ← tableHeaderVisual headerRowName headerCellNameFn columns theme config

  -- Build data rows
  let mut rowWidgets : Array Widget := #[headerWidget]
  for i in [:rows.size] do
    let rowData := rows.getD i #[]
    let isHovered := hoveredRow == some i
    let isSelected := selectedRows.contains i
    let isAlternate := i % 2 == 1
    let rowWidget ← tableRowVisual (rowNameFn i) (cellNameFn i) rowData columns
      false isHovered isSelected isAlternate theme config
    rowWidgets := rowWidgets.push rowWidget

  let tableStyle : BoxStyle := {
    borderColor := some (theme.input.border.withAlpha 0.5)
    borderWidth := config.borderWidth
    cornerRadius := theme.cornerRadius
    width := .percent 1.0
  }

  let wid ← freshId
  let props : FlexContainer := { direction := .column, gap := 0 }
  pure (.flex wid (some containerName) props tableStyle rowWidgets)

/-- Create a reactive table widget.
    - `columns`: Column definitions (headers and optional widths)
    - `rows`: Array of row data (each row is an array of cell strings)
    - `config`: Table configuration
-/
def table (columns : Array TableColumn) (rows : Array (Array String))
    (config : TableConfig := Table.defaultConfig)
    : WidgetM TableResult := do
  let theme ← getThemeW
  -- Register container name
  let containerName ← registerComponentW "table"
  let headerRowName ← registerComponentW "table-header" (isInteractive := false)

  -- Register header cell names
  let mut headerCellNames : Array String := #[]
  for i in [:columns.size] do
    let name ← registerComponentW s!"table-header-cell-{i}" (isInteractive := false)
    headerCellNames := headerCellNames.push name
  let headerCellNameFn (i : Nat) : String := headerCellNames.getD i ""

  -- Register row names
  let mut rowNames : Array String := #[]
  for i in [:rows.size] do
    let name ← registerComponentW s!"table-row-{i}"
    rowNames := rowNames.push name
  let rowNameFn (i : Nat) : String := rowNames.getD i ""

  -- Register cell names (row × col)
  let mut cellNames : Array (Array String) := #[]
  for i in [:rows.size] do
    let mut rowCellNames : Array String := #[]
    for j in [:columns.size] do
      let name ← registerComponentW s!"table-cell-{i}-{j}" (isInteractive := false)
      rowCellNames := rowCellNames.push name
    cellNames := cellNames.push rowCellNames
  let cellNameFn (rowIdx colIdx : Nat) : String :=
    (cellNames.getD rowIdx #[]).getD colIdx ""

  -- Hooks
  let allClicks ← useAllClicks

  -- Find which row was clicked
  let findClickedRow (data : ClickData) : Option Nat :=
    (List.range rows.size).findSome? fun i =>
      if hitWidget data (rowNameFn i) then some i else none

  -- Find which row is hovered
  -- Row click events
  let rowClicks ← Event.mapMaybeM findClickedRow allClicks

  -- Track selected rows
  let selectedRows ← Reactive.foldDyn
    (fun clickedRow current => Table.updateSelection config.selectionMode clickedRow current)
    #[] rowClicks

  -- Track hovered row
  let hoveredRowEvents ← StateT.lift (hoverIndexEvent rowNames)
  let hoveredRow ← Reactive.holdDyn none hoveredRowEvents

  -- Use dynWidget for efficient change-driven rebuilds
  let renderState ← Dynamic.zipWithM (fun s h => (s, h)) selectedRows hoveredRow
  let _ ← dynWidget renderState fun (selected, hovered) => do
    emit do pure (tableVisual containerName headerRowName headerCellNameFn rowNameFn cellNameFn
      columns rows selected hovered theme config)

  pure { onRowSelect := rowClicks, selectedRows, hoveredRow }

end Afferent.Canopy

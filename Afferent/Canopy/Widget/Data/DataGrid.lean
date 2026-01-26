/-
  Canopy DataGrid Widget
  Editable table with cell editing.
-/
import Reactive
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Widget.Display.Label
import Afferent.Canopy.Widget.Input.TextInput
import Afferent.Canopy.Reactive.Component

namespace Afferent.Canopy

open Afferent.Arbor hiding Event
open Reactive Reactive.Host
open Afferent.Canopy.Reactive
open Trellis

/-- Column definition for a data grid. -/
structure DataGridColumn where
  header : String
  width : Option Float := none
deriving Repr, Inhabited

/-- Configuration for data grid appearance. -/
structure DataGridConfig where
  headerHeight : Float := 36.0
  rowHeight : Float := 32.0
  cellPadding : Float := 10.0
  borderWidth : Float := 1.0
  cornerRadius : Float := 6.0
deriving Repr, Inhabited

namespace DataGrid

def defaultConfig : DataGridConfig := {}

def normalizeRows (rows : Array (Array String)) (colCount : Nat) : Array (Array String) :=
  rows.map fun row =>
    if row.size >= colCount then
      row
    else
      row ++ Array.replicate (colCount - row.size) ""

def cellValue (rows : Array (Array String)) (row col : Nat) : String :=
  (rows.getD row #[]).getD col ""

def updateCell (rows : Array (Array String)) (row col : Nat) (value : String) : Array (Array String) :=
  if row < rows.size then
    let rowData := rows[row]!
    if col < rowData.size then
      let newRow := rowData.set! col value
      rows.set! row newRow
    else
      rows
  else
    rows

end DataGrid

/-- Build a single data grid cell. -/
def dataGridCellVisual (name : String) (content : String)
    (isHeader : Bool) (isSelected : Bool) (isHovered : Bool)
    (isEditing : Bool) (editor : TextInputState) (editorFocused : Bool)
    (colWidth : Option Float) (theme : Theme)
    (config : DataGridConfig := DataGrid.defaultConfig) : WidgetBuilder := do
  let bgColor :=
    if isHeader then theme.panel.background
    else if isEditing then theme.input.background
    else if isSelected then theme.primary.background.withAlpha 0.15
    else if isHovered then theme.input.backgroundHover
    else Color.transparent
  let borderColor :=
    if isEditing then theme.input.borderFocused
    else theme.input.border.withAlpha 0.3

  let cellStyle : BoxStyle := {
    backgroundColor := some bgColor
    borderColor := some borderColor
    borderWidth := config.borderWidth
    cornerRadius := if isHeader then 0 else config.cornerRadius
    padding := EdgeInsets.symmetric config.cellPadding 6
    minHeight := some (if isHeader then config.headerHeight else config.rowHeight)
    width := match colWidth with
      | some w => .length w
      | none => .auto
    flexItem := if colWidth.isNone then some (FlexItem.growing 1) else none
  }

  let wid ← freshId
  let props : FlexContainer := {
    direction := .row
    alignItems := .center
    justifyContent := .flexStart
  }

  let child ←
    if isEditing then
      custom (TextInput.inputSpec editor.value "" false editor.cursorPixelX editorFocused theme) {}
    else if isHeader then
      label content theme .heading3 (align := .left)
    else
      text' content theme.font theme.text .left

  pure (.flex wid (some name) props cellStyle #[child])

/-- Build a data grid row (header or data). -/
def dataGridRowVisual (rowName : String) (cellNameFn : Nat → String)
    (cells : Array String) (columns : Array DataGridColumn) (rowIdx : Nat)
    (selected : Option (Nat × Nat)) (hovered : Option (Nat × Nat))
    (editing : Option (Nat × Nat)) (editor : TextInputState) (editorFocused : Bool)
    (theme : Theme) (config : DataGridConfig := DataGrid.defaultConfig)
    (isHeader : Bool := false) : WidgetBuilder := do
  let mut cellWidgets : Array Widget := #[]
  for i in [:columns.size] do
    let content := cells.getD i ""
    let colWidth := (columns.getD i { header := "" }).width
    let isSelected := match selected with
      | some (r, c) => r == rowIdx && c == i && !isHeader
      | none => false
    let isHovered := match hovered with
      | some (r, c) => r == rowIdx && c == i && !isHeader
      | none => false
    let isEditing := match editing with
      | some (r, c) => r == rowIdx && c == i && !isHeader
      | none => false
    let cellWidget ← dataGridCellVisual (cellNameFn i) content isHeader
      isSelected isHovered isEditing editor editorFocused colWidth theme config
    cellWidgets := cellWidgets.push cellWidget

  let rowStyle : BoxStyle := {
    width := .percent 1.0
  }
  let wid ← freshId
  let props : FlexContainer := { direction := .row, gap := 0 }
  pure (.flex wid (some rowName) props rowStyle cellWidgets)

/-- Build the complete data grid visual. -/
def dataGridVisual (containerName : String)
    (headerCellNameFn : Nat → String) (rowCellNameFn : Nat → Nat → String)
    (columns : Array DataGridColumn) (rows : Array (Array String))
    (selected : Option (Nat × Nat)) (hovered : Option (Nat × Nat))
    (editing : Option (Nat × Nat)) (editor : TextInputState) (editorFocused : Bool)
    (theme : Theme) (config : DataGridConfig := DataGrid.defaultConfig) : WidgetBuilder := do
  let headerCells := columns.map (·.header)
  let headerRow ← dataGridRowVisual "datagrid-header" headerCellNameFn
    headerCells columns 0 selected hovered editing editor editorFocused theme config true

  let mut rowWidgets : Array Widget := #[headerRow]
  for r in [:rows.size] do
    let rowCells := rows.getD r #[]
    let cellNameFn (c : Nat) : String := rowCellNameFn r c
    let rowWidget ← dataGridRowVisual s!"datagrid-row-{r}" cellNameFn
      rowCells columns r selected hovered editing editor editorFocused theme config false
    rowWidgets := rowWidgets.push rowWidget

  let outerStyle : BoxStyle := {
    borderColor := some theme.panel.border
    borderWidth := 1
    cornerRadius := config.cornerRadius
  }
  let outerWid ← freshId
  let outerProps : FlexContainer := {
    direction := .column
    gap := 0
  }
  pure (.flex outerWid (some containerName) outerProps outerStyle rowWidgets)

/-! ## Reactive DataGrid Components (FRP-based) -/

structure DataGridState where
  rows : Array (Array String) := #[]
  selected : Option (Nat × Nat) := none
  hovered : Option (Nat × Nat) := none
  editing : Option (Nat × Nat) := none
  editor : TextInputState := {}
deriving Repr, BEq, Inhabited

structure DataGridResult where
  onSelect : Reactive.Event Spider (Nat × Nat)
  onEdit : Reactive.Event Spider (Nat × Nat × String)
  data : Reactive.Dynamic Spider (Array (Array String))
  selected : Reactive.Dynamic Spider (Option (Nat × Nat))

inductive DataGridInputEvent where
  | click (data : ClickData)
  | hover (cell : Option (Nat × Nat))
  | key (data : KeyData)

/-- Create a reactive data grid component using WidgetM.
    Uses the default font from WidgetM context (set via createInputs).
    - `columns`: Column definitions
    - `rows`: Initial row data
    - `config`: Optional configuration
-/
def dataGrid (columns : Array DataGridColumn) (rows : Array (Array String))
    (config : DataGridConfig := {}) : WidgetM DataGridResult := do
  let theme ← getThemeW
  let font ← getFontW
  let gridName ← registerComponentW "datagrid" (isInput := true)
  let events ← getEventsW
  let focusedInput := events.registry.focusedInput
  let fireFocus := events.registry.fireFocus

  let rowCount := rows.size
  let colCount := columns.size

  -- Register cell names for hit testing (header uses its own names)
  let mut headerNames : Array String := #[]
  for _ in [:colCount] do
    let name ← registerComponentW "datagrid-header-cell"
    headerNames := headerNames.push name
  let headerCellNameFn (c : Nat) : String := headerNames.getD c ""

  let mut cellNames : Array String := #[]
  for _ in [:rowCount] do
    for _ in [:colCount] do
      let name ← registerComponentW "datagrid-cell"
      cellNames := cellNames.push name
  let cellNameFn (r c : Nat) : String :=
    cellNames.getD (r * colCount + c) ""

  let allClicks ← useAllClicks
  let keyEvents ← useKeyboard

  let liftSpider {α : Type} : SpiderM α → WidgetM α := fun m => StateT.lift (liftM m)
  let clickEvents ← liftSpider (Event.mapM DataGridInputEvent.click allClicks)
  let mut hoverTargets : Array (String × (Nat × Nat)) := #[]
  for r in [:rowCount] do
    for c in [:colCount] do
      hoverTargets := hoverTargets.push (cellNameFn r c, (r, c))
  let hoverChanges ← StateT.lift (hoverEventForTargets hoverTargets)
  let hoverEvents ← liftSpider (Event.mapM DataGridInputEvent.hover hoverChanges)
  let keyEvents ← liftSpider (Event.mapM DataGridInputEvent.key keyEvents)
  let allInputEvents ← liftSpider (Event.leftmostM [clickEvents, hoverEvents, keyEvents])

  let (selectTrigger, fireSelect) ← Reactive.newTriggerEvent (t := Spider) (a := Nat × Nat)
  let (editTrigger, fireEdit) ← Reactive.newTriggerEvent (t := Spider) (a := Nat × Nat × String)

  let initialRows := DataGrid.normalizeRows rows colCount
  let initialEditor : TextInputState := { value := "", cursor := 0, cursorPixelX := 0 }
  let initialEditor ← SpiderM.liftIO (TextInput.computeCursorPixelX font initialEditor)
  let initialState : DataGridState := {
    rows := initialRows
    selected := none
    hovered := none
    editing := none
    editor := initialEditor
  }

  let findClickedCell (data : ClickData) : Option (Nat × Nat) :=
    (List.range rowCount).findSome? fun r =>
      (List.range colCount).findSome? fun c =>
        if hitWidget data (cellNameFn r c) then some (r, c) else none

  let combinedState ← Reactive.foldDynM
    (fun event state => do
      match event with
      | .click data =>
        if data.click.button != 0 then
          pure state
        else
          match findClickedCell data with
          | some (r, c) =>
            SpiderM.liftIO (fireFocus (some gridName))
            SpiderM.liftIO (fireSelect (r, c))
            let cellText := DataGrid.cellValue state.rows r c
            let editor := { state.editor with value := cellText, cursor := cellText.length }
            let editor ← SpiderM.liftIO (TextInput.computeCursorPixelX font editor)
            pure { state with selected := some (r, c), editing := some (r, c), editor := editor }
          | none =>
            pure { state with editing := none, hovered := none }

      | .hover hoveredCell =>
        pure { state with hovered := hoveredCell }

      | .key data =>
        if !data.event.isPress then
          pure state
        else
          match state.editing with
          | some (r, c) =>
            match data.event.key with
            | .enter =>
              let newRows := DataGrid.updateCell state.rows r c state.editor.value
              SpiderM.liftIO (fireEdit (r, c, state.editor.value))
              pure { state with rows := newRows, editing := none }
            | .escape =>
              pure { state with editing := none }
            | _ =>
              let updated := TextInput.handleKeyPress data.event state.editor none
              let updated ← SpiderM.liftIO (TextInput.computeCursorPixelX font updated)
              pure { state with editor := updated }
          | none => pure state
    )
    initialState
    allInputEvents

  let dataDyn ← Dynamic.mapM (fun s => s.rows) combinedState
  let selectedDyn ← Dynamic.mapM (fun s => s.selected) combinedState

  let isFocused ← Dynamic.mapM (· == some gridName) focusedInput

  -- Use dynWidget for efficient change-driven rebuilds
  let renderState ← Dynamic.zipWithM (fun s f => (s, f)) combinedState isFocused
  let _ ← dynWidget renderState fun (state, focused) => do
    emit do pure (dataGridVisual gridName headerCellNameFn cellNameFn columns state.rows
      state.selected state.hovered state.editing state.editor focused theme config)

  pure { onSelect := selectTrigger, onEdit := editTrigger, data := dataDyn, selected := selectedDyn }

end Afferent.Canopy

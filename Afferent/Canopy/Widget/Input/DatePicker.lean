/-
  Canopy DatePicker Widget
  Calendar-based date selection widget.
-/
import Reactive
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Widget.Display.Label
import Afferent.Canopy.Reactive.Component

namespace Afferent.Canopy

open Afferent.Arbor hiding Event
open Reactive Reactive.Host
open Afferent.Canopy.Reactive
open Trellis

/-- Date value for picker widgets. -/
structure DatePickerDate where
  year : Nat
  month : Nat
  day : Nat
deriving Repr, BEq, Inhabited

/-- Configuration for date picker. -/
structure DatePickerConfig where
  /-- Fixed width for the calendar panel. -/
  width : Float := 280.0
  /-- Day cell size (square). -/
  cellSize : Float := 32.0
  /-- Gap between day cells. -/
  cellGap : Float := 4.0
  /-- Header row height. -/
  headerHeight : Float := 36.0
  /-- Weekday label row height. -/
  weekdayHeight : Float := 22.0
  /-- Padding inside the panel. -/
  padding : Float := 8.0
  /-- Vertical gap between sections. -/
  sectionGap : Float := 6.0
  /-- Corner radius for panel and buttons. -/
  cornerRadius : Float := 6.0
deriving Repr, Inhabited

namespace DatePickerConfig

def default : DatePickerConfig := {}

end DatePickerConfig

namespace DatePicker

inductive ArrowDirection where
  | left
  | right
deriving Repr, BEq, Inhabited

def monthNames : Array String := #[
  "January", "February", "March", "April", "May", "June",
  "July", "August", "September", "October", "November", "December"
]

def weekdayNames : Array String := #[
  "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"
]

def monthName (month : Nat) : String :=
  monthNames.getD (month - 1) s!"Month {month}"

def isLeapYear (year : Nat) : Bool :=
  (year % 4 == 0 && year % 100 != 0) || (year % 400 == 0)

def daysInMonth (year : Nat) (month : Nat) : Nat :=
  match month with
  | 1 | 3 | 5 | 7 | 8 | 10 | 12 => 31
  | 4 | 6 | 9 | 11 => 30
  | 2 => if isLeapYear year then 29 else 28
  | _ => 30

def prevMonth (year : Nat) (month : Nat) : Nat × Nat :=
  if month <= 1 then (year - 1, 12) else (year, month - 1)

def nextMonth (year : Nat) (month : Nat) : Nat × Nat :=
  if month >= 12 then (year + 1, 1) else (year, month + 1)

/-- Day of week for a date (0 = Sunday, 6 = Saturday). -/
def dayOfWeek (date : DatePickerDate) : Nat :=
  let m := date.month
  let d := date.day
  let y := date.year
  let (y', m') := if m < 3 then (y - 1, m + 12) else (y, m)
  let k := y' % 100
  let j := y' / 100
  let h := (d + (13 * (m' + 1) / 5) + k + (k / 4) + (j / 4) + 5 * j) % 7
  -- Zeller: 0=Saturday, 1=Sunday, ..., 6=Friday
  (h + 6) % 7

/-- Build the 6x7 calendar grid for a month. -/
def monthGrid (year : Nat) (month : Nat) : Array (Option DatePickerDate) := Id.run do
  let days := daysInMonth year month
  let firstDow := dayOfWeek { year, month, day := 1 }
  let total := 42
  let mut cells : Array (Option DatePickerDate) := #[]
  for i in [:total] do
    let offset : Int := (Int.ofNat i) - (Int.ofNat firstDow) + 1
    let maxDay : Int := Int.ofNat days
    if offset >= 1 && offset <= maxDay then
      let day := Int.toNat offset
      cells := cells.push (some { year, month, day })
    else
      cells := cells.push none
  return cells

/-- Chevron path for navigation arrows. -/
def arrowSpec (dir : ArrowDirection) (theme : Theme) (size : Float) : CustomSpec := {
  measure := fun _ _ => (size, size)
  collect := fun layout =>
    let rect := layout.contentRect
    let half := size * 0.22
    let midX := rect.x + rect.width / 2
    let midY := rect.y + rect.height / 2
    let (p1, p2, p3) := match dir with
      | .left =>
          (⟨midX + half, midY - half⟩, ⟨midX - half, midY⟩, ⟨midX + half, midY + half⟩)
      | .right =>
          (⟨midX - half, midY - half⟩, ⟨midX + half, midY⟩, ⟨midX - half, midY + half⟩)
    let path := Afferent.Path.empty
      |>.moveTo p1
      |>.lineTo p2
      |>.lineTo p3
    RenderM.build do
      RenderM.strokePath path theme.text 2.0
  draw := none
}

end DatePicker

/-- Build a navigation button for date picker header. -/
def datePickerNavButtonVisual (name : String) (dir : DatePicker.ArrowDirection)
    (theme : Theme) (hovered : Bool) (config : DatePickerConfig := {}) : WidgetBuilder := do
  let bg := if hovered then theme.secondary.backgroundHover else theme.panel.background
  let style : BoxStyle := {
    backgroundColor := some bg
    cornerRadius := config.cornerRadius
    width := .length config.headerHeight
    height := .length config.headerHeight
  }
  let icon ← custom (DatePicker.arrowSpec dir theme config.headerHeight) {
    minWidth := some config.headerHeight
    minHeight := some config.headerHeight
  }
  let wid ← freshId
  let props : FlexContainer := {
    direction := .row
    alignItems := .center
    justifyContent := .center
  }
  pure (.flex wid (some name) props style #[icon])

/-- Build the weekday label row. -/
def datePickerWeekdayRowVisual (theme : Theme) (config : DatePickerConfig := {}) : WidgetBuilder := do
  let mut labels : Array WidgetBuilder := #[]
  for i in [:DatePicker.weekdayNames.size] do
    let labelText := DatePicker.weekdayNames.getD i ""
    let cellStyle : BoxStyle := {
      width := .length config.cellSize
      height := .length config.weekdayHeight
    }
    let wid ← freshId
    let props : FlexContainer := {
      direction := .row
      alignItems := .center
      justifyContent := .center
    }
    let text ← label labelText theme .caption (align := .center)
    let cell : Widget := .flex wid none props cellStyle #[text]
    labels := labels.push (pure cell)
  row (gap := config.cellGap) (style := {}) labels

/-- Build a single day cell. -/
def datePickerDayCellVisual (name : String) (labelText : String)
    (isSelected isHovered isCurrentMonth : Bool)
    (theme : Theme) (config : DatePickerConfig := {}) : WidgetBuilder := do
  let bgColor :=
    if isSelected then theme.primary.background.withAlpha 0.2
    else if isHovered then theme.input.backgroundHover
    else Color.transparent
  let textColor :=
    if isSelected then theme.primary.foreground
    else if isCurrentMonth then theme.text
    else theme.textMuted
  let style : BoxStyle := {
    backgroundColor := some bgColor
    cornerRadius := config.cornerRadius
    width := .length config.cellSize
    height := .length config.cellSize
  }
  let wid ← freshId
  let props : FlexContainer := {
    direction := .row
    alignItems := .center
    justifyContent := .center
  }
  let text ← text' labelText theme.font textColor .center
  pure (.flex wid (some name) props style #[text])

/-- Build the date picker visual. -/
def datePickerVisual (containerName prevName nextName : String)
    (cellNameFn : Nat → String)
    (viewYear viewMonth : Nat) (selected : Option DatePickerDate)
    (hoveredCell : Option Nat) (prevHovered nextHovered : Bool)
    (theme : Theme) (config : DatePickerConfig := {}) : WidgetBuilder := do
  let grid := DatePicker.monthGrid viewYear viewMonth

  -- Header row
  let prevButton ← datePickerNavButtonVisual prevName .left theme prevHovered config
  let nextButton ← datePickerNavButtonVisual nextName .right theme nextHovered config
  let titleText := s!"{DatePicker.monthName viewMonth} {viewYear}"
  let title ← label titleText theme .heading3 (align := .center)
  let titleStyle : BoxStyle := {
    minHeight := some config.headerHeight
    flexItem := some (FlexItem.growing 1)
  }
  let titleWid ← freshId
  let titleProps : FlexContainer := {
    direction := .row
    alignItems := .center
    justifyContent := .center
  }
  let titleBox : Widget := .flex titleWid none titleProps titleStyle #[title]

  let headerStyle : BoxStyle := { minHeight := some config.headerHeight }
  let headerWid ← freshId
  let headerProps : FlexContainer := {
    direction := .row
    gap := 6
    alignItems := .center
    justifyContent := .spaceBetween
  }
  let header : Widget := .flex headerWid none headerProps headerStyle #[prevButton, titleBox, nextButton]

  -- Weekday labels
  let weekdayRow ← datePickerWeekdayRowVisual theme config

  -- Day grid (6 rows)
  let mut rows : Array WidgetBuilder := #[]
  for week in [:6] do
    let mut cells : Array WidgetBuilder := #[]
    for dayIdx in [:7] do
      let idx := week * 7 + dayIdx
      let cellDate := grid.getD idx none
      let labelText := match cellDate with
        | some d => toString d.day
        | none => ""
      let isCurrent := cellDate.isSome
      let isSel := match (cellDate, selected) with
        | (some d, some s) => d == s
        | _ => false
      let isHov := hoveredCell == some idx
      let cellWidget := datePickerDayCellVisual (cellNameFn idx) labelText isSel isHov isCurrent theme config
      cells := cells.push cellWidget
    rows := rows.push (row (gap := config.cellGap) (style := {}) cells)
  let gridWidget ← column (gap := config.cellGap) (style := {}) rows

  -- Outer container
  let totalGridHeight := config.cellSize * 6 + config.cellGap * 5
  let totalHeight := config.headerHeight + config.weekdayHeight +
    totalGridHeight + config.sectionGap * 2 + config.padding * 2
  let outerStyle : BoxStyle := {
    backgroundColor := some theme.panel.background
    borderColor := some theme.panel.border
    borderWidth := 1
    cornerRadius := config.cornerRadius
    padding := EdgeInsets.uniform config.padding
    width := .length config.width
    minHeight := some totalHeight
  }
  let outerWid ← freshId
  let outerProps : FlexContainer := {
    direction := .column
    gap := config.sectionGap
    alignItems := .stretch
  }
  pure (.flex outerWid (some containerName) outerProps outerStyle #[header, weekdayRow, gridWidget])

/-! ## Reactive DatePicker Components (FRP-based) -/

structure DatePickerState where
  viewYear : Nat := 2024
  viewMonth : Nat := 1
  selected : Option DatePickerDate := none
  hovered : Option Nat := none
deriving Repr, BEq, Inhabited

structure DatePickerResult where
  onSelect : Reactive.Event Spider DatePickerDate
  selected : Reactive.Dynamic Spider (Option DatePickerDate)

inductive DatePickerInputEvent where
  | click (data : ClickData)
  | hover (cell : Option Nat)

/-- Create a reactive date picker component using WidgetM.
    - `initialDate`: Initial selected date (also sets initial view month)
    - `config`: Optional configuration
-/
def datePicker (initialDate : DatePickerDate) (config : DatePickerConfig := {})
    : WidgetM DatePickerResult := do
  let theme ← getThemeW
  let containerName ← registerComponentW "date-picker" (isInteractive := false)
  let prevName ← registerComponentW "date-picker-prev"
  let nextName ← registerComponentW "date-picker-next"

  let cellCount := 42
  let mut cellNames : Array String := #[]
  for _ in [:cellCount] do
    let name ← registerComponentW "date-picker-day"
    cellNames := cellNames.push name
  let cellNameFn (i : Nat) : String := cellNames.getD i ""

  let allClicks ← useAllClicks
  let prevHover ← useHover prevName
  let nextHover ← useHover nextName

  let (selectTrigger, fireSelect) ← Reactive.newTriggerEvent (t := Spider) (a := DatePickerDate)

  let liftSpider {α : Type} : SpiderM α → WidgetM α := fun m => StateT.lift (liftM m)
  let clickEvents ← liftSpider (Event.mapM DatePickerInputEvent.click allClicks)
  let hoverChanges ← StateT.lift (hoverIndexEvent cellNames)
  let hoverEvents ← liftSpider (Event.mapM DatePickerInputEvent.hover hoverChanges)
  let allInputEvents ← liftSpider (Event.leftmostM [clickEvents, hoverEvents])

  let findClickedCell (data : ClickData) : Option Nat :=
    (List.range cellCount).findSome? fun i =>
      if hitWidget data (cellNameFn i) then some i else none

  let clickedDate (data : ClickData) (year month : Nat) : Option DatePickerDate :=
    if data.click.button != 0 then none
    else
      match findClickedCell data with
      | some idx =>
          let grid := DatePicker.monthGrid year month
          grid.getD idx none
      | none => none

  let initialState : DatePickerState := {
    viewYear := initialDate.year
    viewMonth := initialDate.month
    selected := some initialDate
    hovered := none
  }

  let combinedState ← Reactive.foldDynM
    (fun event state => do
      match event with
      | .click data =>
        if hitWidget data prevName then
          let (y, m) := DatePicker.prevMonth state.viewYear state.viewMonth
          pure { state with viewYear := y, viewMonth := m, hovered := none }
        else if hitWidget data nextName then
          let (y, m) := DatePicker.nextMonth state.viewYear state.viewMonth
          pure { state with viewYear := y, viewMonth := m, hovered := none }
        else
          match clickedDate data state.viewYear state.viewMonth with
          | some date =>
              SpiderM.liftIO (fireSelect date)
              pure { state with selected := some date }
          | none => pure state

      | .hover hoveredCell =>
        match hoveredCell with
        | some idx =>
          let grid := DatePicker.monthGrid state.viewYear state.viewMonth
          if (grid.getD idx none).isSome then
            pure { state with hovered := some idx }
          else
            pure { state with hovered := none }
        | none =>
          pure { state with hovered := none }
    )
    initialState
    allInputEvents

  let selectedDyn ← Dynamic.mapM (fun s => s.selected) combinedState
  let onSelect := selectTrigger

  -- Use dynWidget for efficient change-driven rebuilds
  let renderState1 ← Dynamic.zipWithM (fun s p => (s, p)) combinedState prevHover
  let renderState2 ← Dynamic.zipWithM (fun (s, p) n => (s, p, n)) renderState1 nextHover
  let _ ← dynWidget renderState2 fun (state, prevH, nextH) => do
    emit do pure (datePickerVisual containerName prevName nextName cellNameFn
      state.viewYear state.viewMonth state.selected state.hovered prevH nextH theme config)

  pure { onSelect, selected := selectedDyn }

end Afferent.Canopy

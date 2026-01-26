/-
  Canopy TimePicker Widget
  Hour/minute/second selection widget for time input.
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

/-- Time value for picker widgets. -/
structure TimeValue where
  hours : Nat := 0    -- 0-23 internally (converted for display in 12-hour mode)
  minutes : Nat := 0  -- 0-59
  seconds : Nat := 0  -- 0-59
deriving Repr, BEq, Inhabited

namespace TimeValue

/-- Check if time is in the AM period (0-11). -/
def isAM (t : TimeValue) : Bool := t.hours < 12

/-- Convert to 12-hour display format (1-12). -/
def hours12 (t : TimeValue) : Nat :=
  let h := t.hours % 12
  if h == 0 then 12 else h

/-- Create a time value from 12-hour format. -/
def from12Hour (hours12 : Nat) (minutes seconds : Nat) (isAM : Bool) : TimeValue :=
  let hours :=
    if isAM then
      if hours12 == 12 then 0 else hours12
    else
      if hours12 == 12 then 12 else hours12 + 12
  { hours, minutes, seconds }

/-- Format time in 24-hour format (HH:MM:SS). -/
def format24 (t : TimeValue) (showSeconds : Bool := true) : String :=
  let hh := if t.hours < 10 then s!"0{t.hours}" else toString t.hours
  let mm := if t.minutes < 10 then s!"0{t.minutes}" else toString t.minutes
  if showSeconds then
    let ss := if t.seconds < 10 then s!"0{t.seconds}" else toString t.seconds
    s!"{hh}:{mm}:{ss}"
  else
    s!"{hh}:{mm}"

/-- Format time in 12-hour format (HH:MM:SS AM/PM). -/
def format12 (t : TimeValue) (showSeconds : Bool := true) : String :=
  let h := t.hours12
  let hh := if h < 10 then s!"0{h}" else toString h
  let mm := if t.minutes < 10 then s!"0{t.minutes}" else toString t.minutes
  let period := if t.isAM then "AM" else "PM"
  if showSeconds then
    let ss := if t.seconds < 10 then s!"0{t.seconds}" else toString t.seconds
    s!"{hh}:{mm}:{ss} {period}"
  else
    s!"{hh}:{mm} {period}"

end TimeValue

/-- Configuration for time picker appearance and behavior. -/
structure TimePickerConfig where
  use24Hour : Bool := false        -- 24-hour vs 12-hour format
  showSeconds : Bool := true       -- Show seconds component
  spinnerWidth : Float := 48.0     -- Width of each spinner
  spinnerHeight : Float := 32.0
  buttonSize : Float := 24.0       -- Increment/decrement button size
  gap : Float := 4.0               -- Gap between elements
  cornerRadius : Float := 6.0
deriving Repr, Inhabited

namespace TimePickerConfig

def default : TimePickerConfig := {}

end TimePickerConfig

namespace TimePicker

/-- Increment hours (wraps 23->0 or handles 12-hour mode). -/
def incHours (t : TimeValue) (use24Hour : Bool) : TimeValue :=
  if use24Hour then
    { t with hours := (t.hours + 1) % 24 }
  else
    -- In 12-hour mode, increment within 1-12 range while preserving AM/PM
    let h12 := t.hours12
    let newH12 := if h12 == 12 then 1 else h12 + 1
    TimeValue.from12Hour newH12 t.minutes t.seconds t.isAM

/-- Decrement hours (wraps 0->23 or handles 12-hour mode). -/
def decHours (t : TimeValue) (use24Hour : Bool) : TimeValue :=
  if use24Hour then
    { t with hours := if t.hours == 0 then 23 else t.hours - 1 }
  else
    let h12 := t.hours12
    let newH12 := if h12 == 1 then 12 else h12 - 1
    TimeValue.from12Hour newH12 t.minutes t.seconds t.isAM

/-- Increment minutes (wraps 59->0). -/
def incMinutes (t : TimeValue) : TimeValue :=
  { t with minutes := (t.minutes + 1) % 60 }

/-- Decrement minutes (wraps 0->59). -/
def decMinutes (t : TimeValue) : TimeValue :=
  { t with minutes := if t.minutes == 0 then 59 else t.minutes - 1 }

/-- Increment seconds (wraps 59->0). -/
def incSeconds (t : TimeValue) : TimeValue :=
  { t with seconds := (t.seconds + 1) % 60 }

/-- Decrement seconds (wraps 0->59). -/
def decSeconds (t : TimeValue) : TimeValue :=
  { t with seconds := if t.seconds == 0 then 59 else t.seconds - 1 }

/-- Toggle AM/PM. -/
def togglePeriod (t : TimeValue) : TimeValue :=
  { t with hours := (t.hours + 12) % 24 }

/-- Display hours value. -/
def displayHours (t : TimeValue) (use24Hour : Bool) : String :=
  let h := if use24Hour then t.hours else t.hours12
  if h < 10 then s!"0{h}" else toString h

/-- Display minutes value. -/
def displayMinutes (t : TimeValue) : String :=
  if t.minutes < 10 then s!"0{t.minutes}" else toString t.minutes

/-- Display seconds value. -/
def displaySeconds (t : TimeValue) : String :=
  if t.seconds < 10 then s!"0{t.seconds}" else toString t.seconds

end TimePicker

/-- Build a spinner button (up or down arrow). -/
def timeSpinnerButtonVisual (name : String) (isUp hovered : Bool)
    (theme : Theme) (config : TimePickerConfig := {}) : WidgetBuilder := do
  let bgColor := if hovered then theme.secondary.backgroundHover else theme.secondary.background
  let textColor := theme.text
  let labelText := if isUp then "▲" else "▼"

  let style : BoxStyle := {
    backgroundColor := some bgColor
    borderColor := some theme.input.border
    borderWidth := 1
    cornerRadius := config.cornerRadius
    width := .length config.buttonSize
    height := .length config.buttonSize
  }

  let wid ← freshId
  let props : FlexContainer := {
    direction := .row
    alignItems := .center
    justifyContent := .center
  }
  let text ← text' labelText theme.font textColor .center
  pure (.flex wid (some name) props style #[text])

/-- Build the value display box for a spinner. -/
def timeSpinnerValueVisual (value : String) (theme : Theme)
    (config : TimePickerConfig := {}) : WidgetBuilder := do
  let style : BoxStyle := {
    backgroundColor := some theme.input.background
    borderColor := some theme.input.border
    borderWidth := 1
    cornerRadius := config.cornerRadius
    width := .length config.spinnerWidth
    height := .length config.spinnerHeight
  }
  let wid ← freshId
  let props : FlexContainer := {
    direction := .row
    alignItems := .center
    justifyContent := .center
  }
  let text ← text' value theme.font theme.text .center
  pure (.flex wid none props style #[text])

/-- Build a single time spinner (up button, value, down button). -/
def timeSpinnerVisual (upName downName : String) (value : String)
    (upHovered downHovered : Bool) (theme : Theme)
    (config : TimePickerConfig := {}) : WidgetBuilder := do
  let upButton ← timeSpinnerButtonVisual upName true upHovered theme config
  let valueBox ← timeSpinnerValueVisual value theme config
  let downButton ← timeSpinnerButtonVisual downName false downHovered theme config

  let outerStyle : BoxStyle := {}
  let outerWid ← freshId
  let outerProps : FlexContainer := {
    direction := .column
    gap := config.gap
    alignItems := .center
  }
  pure (.flex outerWid none outerProps outerStyle #[upButton, valueBox, downButton])

/-- Build the colon separator between time components. -/
def timeColonVisual (theme : Theme) (config : TimePickerConfig := {}) : WidgetBuilder := do
  let style : BoxStyle := {
    width := .length 12
    height := .length config.spinnerHeight
  }
  let wid ← freshId
  let props : FlexContainer := {
    direction := .row
    alignItems := .center
    justifyContent := .center
  }
  -- Position colon in the middle of the spinner (accounting for button height)
  let text ← text' ":" theme.font theme.text .center
  pure (.flex wid none props style #[text])

/-- Build the AM/PM toggle button. -/
def ampmButtonVisual (name : String) (isAM hovered : Bool)
    (theme : Theme) (config : TimePickerConfig := {}) : WidgetBuilder := do
  let bgColor := if hovered then theme.secondary.backgroundHover else theme.secondary.background
  let labelText := if isAM then "AM" else "PM"

  let style : BoxStyle := {
    backgroundColor := some bgColor
    borderColor := some theme.input.border
    borderWidth := 1
    cornerRadius := config.cornerRadius
    width := .length config.spinnerWidth
    height := .length config.spinnerHeight
  }

  let wid ← freshId
  let props : FlexContainer := {
    direction := .row
    alignItems := .center
    justifyContent := .center
  }
  let text ← text' labelText theme.font theme.text .center
  pure (.flex wid (some name) props style #[text])

/-- Build the complete time picker visual. -/
def timePickerVisual (containerName : String)
    (hoursUpName hoursDownName minutesUpName minutesDownName
     secondsUpName secondsDownName ampmName : String)
    (time : TimeValue)
    (hoursUpHover hoursDownHover minutesUpHover minutesDownHover
     secondsUpHover secondsDownHover ampmHover : Bool)
    (theme : Theme) (config : TimePickerConfig := {}) : WidgetBuilder := do
  let hoursSpinner ← timeSpinnerVisual hoursUpName hoursDownName
    (TimePicker.displayHours time config.use24Hour) hoursUpHover hoursDownHover theme config
  let colonWidget1 ← timeColonVisual theme config
  let minutesSpinner ← timeSpinnerVisual minutesUpName minutesDownName
    (TimePicker.displayMinutes time) minutesUpHover minutesDownHover theme config

  let mut widgets : Array Widget := #[hoursSpinner, colonWidget1, minutesSpinner]

  if config.showSeconds then
    let colonWidget2 ← timeColonVisual theme config
    let secondsSpinner ← timeSpinnerVisual secondsUpName secondsDownName
      (TimePicker.displaySeconds time) secondsUpHover secondsDownHover theme config
    widgets := widgets.push colonWidget2
    widgets := widgets.push secondsSpinner

  if !config.use24Hour then
    let ampmButton ← ampmButtonVisual ampmName time.isAM ampmHover theme config
    widgets := widgets.push ampmButton

  let outerStyle : BoxStyle := {}
  let outerWid ← freshId
  let outerProps : FlexContainer := {
    direction := .row
    gap := config.gap
    alignItems := .center
  }
  pure (.flex outerWid (some containerName) outerProps outerStyle widgets)

/-! ## Reactive TimePicker Components (FRP-based) -/

/-- TimePicker result - events and dynamics. -/
structure TimePickerResult where
  onChange : Reactive.Event Spider TimeValue
  value : Reactive.Dynamic Spider TimeValue

/-- Input events for the time picker. -/
inductive TimePickerInputEvent where
  | hoursUp
  | hoursDown
  | minutesUp
  | minutesDown
  | secondsUp
  | secondsDown
  | toggleAmPm

/-- Create a reactive time picker component using WidgetM.
    - `initialValue`: Initial time value (default: 00:00:00)
    - `config`: Optional configuration
-/
def timePicker (initialValue : TimeValue := {}) (config : TimePickerConfig := {})
    : WidgetM TimePickerResult := do
  let theme ← getThemeW
  let containerName ← registerComponentW "time-picker" (isInteractive := false)
  let hoursUpName ← registerComponentW "time-picker-hours-up"
  let hoursDownName ← registerComponentW "time-picker-hours-down"
  let minutesUpName ← registerComponentW "time-picker-minutes-up"
  let minutesDownName ← registerComponentW "time-picker-minutes-down"
  let secondsUpName ← registerComponentW "time-picker-seconds-up"
  let secondsDownName ← registerComponentW "time-picker-seconds-down"
  let ampmName ← registerComponentW "time-picker-ampm"

  -- Hover states for all buttons
  let hoursUpHover ← useHover hoursUpName
  let hoursDownHover ← useHover hoursDownName
  let minutesUpHover ← useHover minutesUpName
  let minutesDownHover ← useHover minutesDownName
  let secondsUpHover ← useHover secondsUpName
  let secondsDownHover ← useHover secondsDownName
  let ampmHover ← useHover ampmName

  -- Click events for all buttons
  let hoursUpClick ← useClick hoursUpName
  let hoursDownClick ← useClick hoursDownName
  let minutesUpClick ← useClick minutesUpName
  let minutesDownClick ← useClick minutesDownName
  let secondsUpClick ← useClick secondsUpName
  let secondsDownClick ← useClick secondsDownName
  let ampmClick ← useClick ampmName

  -- Map clicks to input events
  let hoursUpEvents ← Event.mapM (fun _ => TimePickerInputEvent.hoursUp) hoursUpClick
  let hoursDownEvents ← Event.mapM (fun _ => TimePickerInputEvent.hoursDown) hoursDownClick
  let minutesUpEvents ← Event.mapM (fun _ => TimePickerInputEvent.minutesUp) minutesUpClick
  let minutesDownEvents ← Event.mapM (fun _ => TimePickerInputEvent.minutesDown) minutesDownClick
  let secondsUpEvents ← Event.mapM (fun _ => TimePickerInputEvent.secondsUp) secondsUpClick
  let secondsDownEvents ← Event.mapM (fun _ => TimePickerInputEvent.secondsDown) secondsDownClick
  let ampmEvents ← Event.mapM (fun _ => TimePickerInputEvent.toggleAmPm) ampmClick

  let allInputEvents ← Event.leftmostM [
    hoursUpEvents, hoursDownEvents,
    minutesUpEvents, minutesDownEvents,
    secondsUpEvents, secondsDownEvents,
    ampmEvents
  ]

  -- Fold over input events to update time value
  let configRef := config
  let valueDyn ← Reactive.foldDyn
    (fun event time =>
      match event with
      | .hoursUp => TimePicker.incHours time configRef.use24Hour
      | .hoursDown => TimePicker.decHours time configRef.use24Hour
      | .minutesUp => TimePicker.incMinutes time
      | .minutesDown => TimePicker.decMinutes time
      | .secondsUp => TimePicker.incSeconds time
      | .secondsDown => TimePicker.decSeconds time
      | .toggleAmPm => TimePicker.togglePeriod time
    )
    initialValue
    allInputEvents

  let valueChanges ← Dynamic.changesM valueDyn
  let onChange ← Event.mapMaybeM
    (fun (old, new) => if old != new then some new else none)
    valueChanges

  -- Combine all hover states for rendering
  let hover1 ← Dynamic.zipWithM (fun a b => (a, b)) hoursUpHover hoursDownHover
  let hover2 ← Dynamic.zipWithM (fun a b => (a, b)) minutesUpHover minutesDownHover
  let hover3 ← Dynamic.zipWithM (fun a b => (a, b)) secondsUpHover secondsDownHover
  let hover12 ← Dynamic.zipWithM (fun a b => (a, b)) hover1 hover2
  let hover123 ← Dynamic.zipWithM (fun a b => (a, b)) hover12 hover3
  let hoverAll ← Dynamic.zipWithM (fun a b => (a, b)) hover123 ampmHover

  let renderState ← Dynamic.zipWithM (fun v h => (v, h)) valueDyn hoverAll

  let _ ← dynWidget renderState fun (time, ((((hu, hd), (mu, md)), (su, sd)), ap)) => do
    emit do pure (timePickerVisual containerName
      hoursUpName hoursDownName minutesUpName minutesDownName
      secondsUpName secondsDownName ampmName
      time hu hd mu md su sd ap theme config)

  pure { onChange, value := valueDyn }

end Afferent.Canopy

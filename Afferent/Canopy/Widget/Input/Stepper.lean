/-
  Canopy Stepper Widget
  Increment/decrement control for numeric values.
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

/-- Configuration for stepper appearance and behavior. -/
structure StepperConfig where
  min : Int := 0
  max : Int := 100
  step : Int := 1
  width : Float := 140.0
  height : Float := 32.0
  buttonWidth : Float := 32.0
  cornerRadius : Float := 6.0
deriving Repr, Inhabited

namespace StepperConfig

def default : StepperConfig := {}

end StepperConfig

namespace Stepper

def clamp (value : Int) (config : StepperConfig) : Int :=
  if value < config.min then config.min
  else if value > config.max then config.max
  else value

def valueWidth (config : StepperConfig) : Float :=
  let raw := config.width - config.buttonWidth * 2
  if raw < 0 then 0 else raw

end Stepper

/-- Build a stepper button. -/
def stepperButtonVisual (name : String) (labelText : String)
    (enabled hovered : Bool) (theme : Theme) (config : StepperConfig := {}) : WidgetBuilder := do
  let bgColor :=
    if !enabled then theme.input.backgroundDisabled
    else if hovered then theme.secondary.backgroundHover
    else theme.secondary.background
  let textColor := if enabled then theme.text else theme.textMuted

  let style : BoxStyle := {
    backgroundColor := some bgColor
    borderColor := some theme.input.border
    borderWidth := 1
    cornerRadius := config.cornerRadius
    width := .length config.buttonWidth
    height := .length config.height
  }

  let wid ← freshId
  let props : FlexContainer := {
    direction := .row
    alignItems := .center
    justifyContent := .center
  }
  let text ← text' labelText theme.font textColor .center
  pure (.flex wid (some name) props style #[text])

/-- Build the value display for a stepper. -/
def stepperValueVisual (value : Int) (theme : Theme)
    (config : StepperConfig := {}) : WidgetBuilder := do
  let style : BoxStyle := {
    backgroundColor := some theme.input.background
    borderColor := some theme.input.border
    borderWidth := 1
    width := .length (Stepper.valueWidth config)
    height := .length config.height
  }
  let wid ← freshId
  let props : FlexContainer := {
    direction := .row
    alignItems := .center
    justifyContent := .center
  }
  let text ← text' (toString value) theme.font theme.text .center
  pure (.flex wid none props style #[text])

/-- Build the visual stepper widget. -/
def stepperVisual (name decName incName : String) (value : Int)
    (decHovered incHovered : Bool) (theme : Theme) (config : StepperConfig := {}) : WidgetBuilder := do
  let decEnabled := value > config.min
  let incEnabled := value < config.max
  let decButton ← stepperButtonVisual decName "-" decEnabled decHovered theme config
  let incButton ← stepperButtonVisual incName "+" incEnabled incHovered theme config
  let valueBox ← stepperValueVisual value theme config

  let outerStyle : BoxStyle := {
    width := .length config.width
    height := .length config.height
  }
  let outerWid ← freshId
  let outerProps : FlexContainer := { direction := .row, gap := 0, alignItems := .center }
  pure (.flex outerWid (some name) outerProps outerStyle #[decButton, valueBox, incButton])

/-! ## Reactive Stepper Components (FRP-based) -/

/-- Stepper result - events and dynamics. -/
structure StepperResult where
  onChange : Reactive.Event Spider Int
  value : Reactive.Dynamic Spider Int

/-- Create a reactive stepper component using WidgetM.
    - `initialValue`: Initial value
    - `config`: Optional configuration
-/
def stepper (initialValue : Int := 0) (config : StepperConfig := {})
    : WidgetM StepperResult := do
  let theme ← getThemeW
  let name ← registerComponentW "stepper"
  let decName ← registerComponentW "stepper-dec"
  let incName ← registerComponentW "stepper-inc"

  let decHovered ← useHover decName
  let incHovered ← useHover incName
  let decClicks ← useClick decName
  let incClicks ← useClick incName

  let decDeltas ← Event.mapM (fun _ => -config.step) decClicks
  let incDeltas ← Event.mapM (fun _ => config.step) incClicks
  let deltas ← Event.leftmostM [decDeltas, incDeltas]

  let initial := Stepper.clamp initialValue config
  let valueDyn ← Reactive.foldDyn
    (fun delta value => Stepper.clamp (value + delta) config)
    initial
    deltas

  let valueChanges ← Dynamic.changesM valueDyn
  let onChange ← Event.mapMaybeM
    (fun (old, new) => if old != new then some new else none)
    valueChanges

  -- Use dynWidget for efficient change-driven rebuilds
  let hoverState ← Dynamic.zipWithM (fun d i => (d, i)) decHovered incHovered
  let renderState ← Dynamic.zipWithM (fun v h => (v, h)) valueDyn hoverState
  let _ ← dynWidget renderState fun (value, (decH, incH)) => do
    emit do pure (stepperVisual name decName incName value decH incH theme config)

  pure { onChange, value := valueDyn }

end Afferent.Canopy

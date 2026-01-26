/-
  Canopy Slider Widget
  Horizontal slider for selecting a value within a range.
-/
import Reactive
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Reactive.Component

namespace Afferent.Canopy

open Afferent.Arbor hiding Event

/-- Extended state for slider widgets. -/
structure SliderState extends WidgetState where
  value : Float := 0.5  -- Normalized 0.0-1.0
deriving Repr, BEq, Inhabited

namespace Slider

/-- Dimensions for slider rendering. -/
structure Dimensions where
  trackWidth : Float := 200.0
  trackHeight : Float := 6.0
  thumbSize : Float := 18.0
deriving Repr, Inhabited

/-- Default slider dimensions. -/
def defaultDimensions : Dimensions := {}

/-- Custom spec for slider track and thumb rendering.
    `value` is 0.0 to 1.0, representing position along track. -/
def trackSpec (value : Float) (hovered : Bool) (focused : Bool)
    (theme : Theme) (dims : Dimensions := defaultDimensions) : CustomSpec := {
  measure := fun _ _ => (dims.trackWidth, dims.thumbSize)
  collect := fun layout =>
    let rect := layout.contentRect
    RenderM.build do
      -- Clamp value to valid range
      let v := if value < 0.0 then 0.0 else if value > 1.0 then 1.0 else value

      -- Track vertical center
      let trackY := rect.y + (rect.height - dims.trackHeight) / 2
      let trackRect := Arbor.Rect.mk' rect.x trackY dims.trackWidth dims.trackHeight

      -- Background track (gray)
      let trackBg := Color.gray 0.3
      RenderM.fillRect trackRect trackBg (dims.trackHeight / 2)

      -- Filled portion (primary color)
      let filledWidth := dims.trackWidth * v
      if filledWidth > 0 then
        let filledRect := Arbor.Rect.mk' rect.x trackY filledWidth dims.trackHeight
        RenderM.fillRect filledRect theme.primary.background (dims.trackHeight / 2)

      -- Thumb position (centered on value position)
      let thumbX := rect.x + (dims.trackWidth - dims.thumbSize) * v
      let thumbY := rect.y + (rect.height - dims.thumbSize) / 2
      let thumbRect := Arbor.Rect.mk' thumbX thumbY dims.thumbSize dims.thumbSize

      -- Thumb color: white normally, slightly gray when hovered
      let thumbColor := if hovered then Color.gray 0.95 else Color.white
      RenderM.fillRect thumbRect thumbColor (dims.thumbSize / 2)

      -- Focus ring on thumb
      if focused then
        let focusRect := Arbor.Rect.mk' (thumbX - 2) (thumbY - 2)
                                         (dims.thumbSize + 4) (dims.thumbSize + 4)
        RenderM.strokeRect focusRect theme.focusRing 2.0 ((dims.thumbSize + 4) / 2)
  draw := none
}

end Slider

/-- Build a visual slider (WidgetBuilder version).
    - `name`: Widget name for hit testing
    - `labelText`: Optional text to display next to slider
    - `theme`: Theme for styling
    - `value`: Current value (0.0-1.0)
    - `state`: Widget interaction state (hover, focus, etc.)
-/
def sliderVisual (name : String) (labelText : Option String) (theme : Theme)
    (value : Float) (state : WidgetState := {}) : WidgetBuilder := do
  let dims := Slider.defaultDimensions

  let sliderTrack : WidgetBuilder := do
    custom (Slider.trackSpec value state.hovered state.focused theme dims) {
      minWidth := some dims.trackWidth
      minHeight := some dims.thumbSize
    }

  -- Use custom flex container with alignItems := .center to prevent stretching
  let wid ← freshId
  let props : Trellis.FlexContainer := { Trellis.FlexContainer.row 8 with alignItems := .center }
  let track ← sliderTrack
  match labelText with
  | some text =>
    let label ← text' text theme.font theme.text .left
    pure (.flex wid (some name) props {} #[track, label])
  | none =>
    pure (.flex wid (some name) props {} #[track])

/-- Build a visual slider without label (WidgetBuilder version). -/
def sliderOnlyVisual (name : String) (theme : Theme)
    (value : Float) (state : WidgetState := {}) : WidgetBuilder :=
  sliderVisual name none theme value state

/-! ## Reactive Slider Components (FRP-based)

These use WidgetM for declarative composition with automatic value tracking.
-/

open Reactive Reactive.Host
open Afferent.Canopy.Reactive

/-- Slider result - events and dynamics. -/
structure SliderResult where
  onChange : Reactive.Event Spider Float
  value : Reactive.Dynamic Spider Float

inductive SliderInputEvent where
  | click (data : ClickData)
  | hover (data : HoverData)
  | mouseUp

/-- Create a reactive slider component using WidgetM.
    Emits the slider widget and returns value state.
    - `label`: Optional label text displayed next to slider
    - `initialValue`: Initial value (0.0-1.0)
-/
def slider (label : Option String) (initialValue : Float := 0.5)
    : WidgetM SliderResult := do
  let theme ← getThemeW
  let name ← registerComponentW "slider"
  let isHovered ← useHover name
  let allClicks ← useAllClicks
  let allHovers ← useAllHovers
  let allMouseUp ← useAllMouseUp

  let trackWidth := Slider.defaultDimensions.trackWidth
  let liftSpider {α : Type} : SpiderM α → WidgetM α := fun m => StateT.lift (liftM m)
  let allInputEvents ← liftSpider do
    let clickEvents ← Event.mapM SliderInputEvent.click allClicks
    let hoverEvents ← Event.mapM SliderInputEvent.hover allHovers
    let mouseUpEvents ← Event.mapM (fun _ => SliderInputEvent.mouseUp) allMouseUp
    Event.leftmostM [clickEvents, hoverEvents, mouseUpEvents]

  let combinedState ← Reactive.foldDynM
    (fun event state => do
      match event with
      | .click data =>
        if !hitWidget data name || data.click.button != 0 then
          pure state
        else
          match calculateSliderValue data.click.x data.layouts data.widget name trackWidth with
          | some v => pure { state with value := v, pressed := true }
          | none => pure state
      | .hover data =>
        if state.pressed then
          match calculateSliderValue data.x data.layouts data.widget name trackWidth with
          | some v => pure { state with value := v }
          | none => pure state
        else
          pure state
      | .mouseUp =>
        pure { state with pressed := false }
    )
    ({ value := initialValue, pressed := false, hovered := false, focused := false, disabled := false } : SliderState)
    allInputEvents

  let valueDyn ← Dynamic.mapM (·.value) combinedState
  let valueChanges ← Dynamic.changesM valueDyn
  let onChange ← Event.mapMaybeM
    (fun (old, new) => if old != new then some new else none) valueChanges

  -- Use dynWidget for efficient change-driven rebuilds
  let renderState ← Dynamic.zipWithM (fun h s => (h, s)) isHovered combinedState
  let _ ← dynWidget renderState fun (hovered, s) => do
    let state : WidgetState := { hovered, pressed := s.pressed, focused := false }
    emit do pure (sliderVisual name label theme s.value state)

  pure { onChange, value := valueDyn }

end Afferent.Canopy

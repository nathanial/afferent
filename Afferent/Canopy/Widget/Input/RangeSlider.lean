/-
  Canopy RangeSlider Widget
  Dual-handle slider for selecting a min/max range.
-/
import Reactive
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Reactive.Component

namespace Afferent.Canopy

open Afferent.Arbor hiding Event
open Reactive Reactive.Host
open Afferent.Canopy.Reactive

/-- Drag target for range slider. -/
inductive RangeSliderTarget where
  | none
  | low
  | high
deriving Repr, BEq, Inhabited

/-- Range slider state. -/
structure RangeSliderState extends WidgetState where
  low : Float := 0.25
  high : Float := 0.75
  dragTarget : RangeSliderTarget := .none
deriving Repr, BEq, Inhabited

namespace RangeSlider

/-- Dimensions for range slider rendering. -/
structure Dimensions where
  trackWidth : Float := 220.0
  trackHeight : Float := 6.0
  thumbSize : Float := 18.0
deriving Repr, Inhabited

/-- Default range slider dimensions. -/
def defaultDimensions : Dimensions := {}

/-- Clamp a Float to [0, 1]. -/
def clamp01 (x : Float) : Float :=
  if x < 0.0 then 0.0 else if x > 1.0 then 1.0 else x

/-- Ensure low/high ordering after clamping. -/
def clampRange (low high : Float) : Float × Float :=
  let l := clamp01 low
  let h := clamp01 high
  if l <= h then (l, h) else (h, l)

/-- Update range based on target thumb. -/
def applyDrag (target : RangeSliderTarget) (value : Float) (low high : Float) : Float × Float :=
  let v := clamp01 value
  match target with
  | .low => (min v high, high)
  | .high => (low, max v low)
  | .none => (low, high)

/-- Custom spec for range slider track + thumbs. -/
def trackSpec (low high : Float) (hovered : Bool) (target : RangeSliderTarget)
    (theme : Theme) (dims : Dimensions := defaultDimensions) : CustomSpec := {
  measure := fun _ _ => (dims.trackWidth, dims.thumbSize)
  collect := fun layout =>
    let rect := layout.contentRect
    let (l, h) := clampRange low high
    RenderM.build do
      -- Track
      let trackY := rect.y + (rect.height - dims.trackHeight) / 2
      let trackRect := Arbor.Rect.mk' rect.x trackY dims.trackWidth dims.trackHeight
      RenderM.fillRect trackRect (Color.gray 0.3) (dims.trackHeight / 2)

      -- Filled range
      let rangeStart := rect.x + dims.trackWidth * l
      let rangeWidth := dims.trackWidth * (h - l)
      if rangeWidth > 0 then
        let rangeRect := Arbor.Rect.mk' rangeStart trackY rangeWidth dims.trackHeight
        RenderM.fillRect rangeRect theme.primary.background (dims.trackHeight / 2)

      -- Thumb positions
      let thumbY := rect.y + (rect.height - dims.thumbSize) / 2
      let lowX := rect.x + (dims.trackWidth - dims.thumbSize) * l
      let highX := rect.x + (dims.trackWidth - dims.thumbSize) * h
      let baseThumb := if hovered then Color.gray 0.95 else Color.white
      let lowColor := if target == .low then theme.primary.background else baseThumb
      let highColor := if target == .high then theme.primary.background else baseThumb

      let lowRect := Arbor.Rect.mk' lowX thumbY dims.thumbSize dims.thumbSize
      let highRect := Arbor.Rect.mk' highX thumbY dims.thumbSize dims.thumbSize
      RenderM.fillRect lowRect lowColor (dims.thumbSize / 2)
      RenderM.fillRect highRect highColor (dims.thumbSize / 2)

      if target == .low then
        RenderM.strokeRect (Arbor.Rect.mk' (lowX - 2) (thumbY - 2)
          (dims.thumbSize + 4) (dims.thumbSize + 4)) theme.focusRing 2.0 ((dims.thumbSize + 4) / 2)
      if target == .high then
        RenderM.strokeRect (Arbor.Rect.mk' (highX - 2) (thumbY - 2)
          (dims.thumbSize + 4) (dims.thumbSize + 4)) theme.focusRing 2.0 ((dims.thumbSize + 4) / 2)
  draw := none
}

end RangeSlider

/-- Build a visual range slider widget. -/
def rangeSliderVisual (name : String) (theme : Theme)
    (low high : Float) (hovered : Bool) (target : RangeSliderTarget)
    (dims : RangeSlider.Dimensions := {}) : WidgetBuilder := do
  let sliderTrack : WidgetBuilder := do
    custom (RangeSlider.trackSpec low high hovered target theme dims) {
      minWidth := some dims.trackWidth
      minHeight := some dims.thumbSize
    }

  let wid ← freshId
  let props : Trellis.FlexContainer := { Trellis.FlexContainer.row 0 with alignItems := .center }
  let track ← sliderTrack
  pure (.flex wid (some name) props {} #[track])

/-! ## Reactive RangeSlider Components (FRP-based) -/

/-- Range slider result - events and dynamics. -/
structure RangeSliderResult where
  onChange : Reactive.Event Spider (Float × Float)
  low : Reactive.Dynamic Spider Float
  high : Reactive.Dynamic Spider Float

inductive RangeSliderInputEvent where
  | click (data : ClickData)
  | hover (data : HoverData)
  | mouseUp

/-- Create a reactive range slider component using WidgetM.
    Emits the range slider widget and returns range state.
    - `initialLow`: Initial low value (0.0-1.0)
    - `initialHigh`: Initial high value (0.0-1.0)
-/
def rangeSlider (initialLow : Float := 0.25) (initialHigh : Float := 0.75)
    : WidgetM RangeSliderResult := do
  let theme ← getThemeW
  let name ← registerComponentW "range-slider"
  let isHovered ← useHover name
  let allClicks ← useAllClicks
  let allHovers ← useAllHovers
  let allMouseUp ← useAllMouseUp

  let dims := RangeSlider.defaultDimensions

  let liftSpider {α : Type} : SpiderM α → WidgetM α := fun m => StateT.lift (liftM m)
  let allInputEvents ← liftSpider do
    let clickEvents ← Event.mapM RangeSliderInputEvent.click allClicks
    let hoverEvents ← Event.mapM RangeSliderInputEvent.hover allHovers
    let mouseUpEvents ← Event.mapM (fun _ => RangeSliderInputEvent.mouseUp) allMouseUp
    Event.leftmostM [clickEvents, hoverEvents, mouseUpEvents]

  let initialState : RangeSliderState := {
    low := initialLow
    high := initialHigh
    dragTarget := .none
  }

  let combinedState ← Reactive.foldDynM
    (fun (event : RangeSliderInputEvent) state => do
      match event with
      | .click clickData =>
        if !hitWidget clickData name || clickData.click.button != 0 then
          pure state
        else
          match calculateSliderValue clickData.click.x clickData.layouts clickData.widget name dims.trackWidth with
          | some value =>
            let (l, h) := RangeSlider.clampRange state.low state.high
            let distLow := (value - l).abs
            let distHigh := (value - h).abs
            let target := if distLow <= distHigh then RangeSliderTarget.low else RangeSliderTarget.high
            let (newLow, newHigh) := RangeSlider.applyDrag target value l h
            pure { state with low := newLow, high := newHigh, dragTarget := target }
          | none => pure state
      | .hover hoverData =>
        if state.dragTarget == .none then
          pure state
        else
          match calculateSliderValue hoverData.x hoverData.layouts hoverData.widget name dims.trackWidth with
          | some value =>
            let (newLow, newHigh) := RangeSlider.applyDrag state.dragTarget value state.low state.high
            pure { state with low := newLow, high := newHigh }
          | none => pure state
      | .mouseUp =>
        pure { state with dragTarget := .none }
    )
    initialState
    allInputEvents

  let lowDyn ← Dynamic.mapM (·.low) combinedState
  let highDyn ← Dynamic.mapM (·.high) combinedState
  let rangeDyn ← Dynamic.mapM (fun s => (s.low, s.high)) combinedState
  let rangeChanges ← Dynamic.changesM rangeDyn
  let onChange ← Event.mapM (fun (_, new) => new) rangeChanges

  -- Use dynWidget for efficient change-driven rebuilds
  let renderState ← Dynamic.zipWithM (fun s h => (s, h)) combinedState isHovered
  let _ ← dynWidget renderState fun (state, hovered) => do
    emit do pure (rangeSliderVisual name theme state.low state.high hovered state.dragTarget dims)

  pure { onChange, low := lowDyn, high := highDyn }

end Afferent.Canopy

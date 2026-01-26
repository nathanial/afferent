/-
  Canopy Switch Widget
  iOS-style on/off toggle switch.
-/
import Reactive
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Widget.Input.Button
import Afferent.Canopy.Reactive.Component

namespace Afferent.Canopy

open Afferent.Arbor hiding Event

/-- Extended state for switch widgets. -/
structure SwitchState extends WidgetState where
  on : Bool := false
deriving Repr, BEq, Inhabited

namespace Switch

/-- Dimensions for switch rendering. -/
structure Dimensions where
  trackWidth : Float := 44.0
  trackHeight : Float := 24.0
  thumbSize : Float := 20.0
  thumbPadding : Float := 2.0
deriving Repr, Inhabited

/-- Default switch dimensions. -/
def defaultDimensions : Dimensions := {}

/-- Custom spec for switch track and thumb rendering (boolean version). -/
def trackSpec (isOn : Bool) (hovered : Bool) (_theme : Theme) (dims : Dimensions := defaultDimensions) : CustomSpec := {
  measure := fun _ _ => (dims.trackWidth, dims.trackHeight)
  collect := fun layout =>
    let rect := layout.contentRect
    RenderM.build do
      -- Draw the thumb (circular knob)
      let thumbX := if isOn then
        rect.x + dims.trackWidth - dims.thumbSize - dims.thumbPadding
      else
        rect.x + dims.thumbPadding
      let thumbY := rect.y + (dims.trackHeight - dims.thumbSize) / 2
      let thumbRect := Arbor.Rect.mk' thumbX thumbY dims.thumbSize dims.thumbSize
      -- Thumb color: white normally, slightly gray when hovered
      let thumbColor := if hovered then Color.gray 0.95 else Color.white
      RenderM.fillRect thumbRect thumbColor (dims.thumbSize / 2)
  draw := none
}

/-- Custom spec for animated switch track and thumb rendering.
    `progress` is 0.0 (off) to 1.0 (on), allowing smooth animation. -/
def animatedTrackSpec (progress : Float) (hovered : Bool) (dims : Dimensions := defaultDimensions) : CustomSpec := {
  measure := fun _ _ => (dims.trackWidth, dims.trackHeight)
  collect := fun layout =>
    let rect := layout.contentRect
    RenderM.build do
      -- Interpolate thumb X position based on progress
      let leftX := rect.x + dims.thumbPadding
      let rightX := rect.x + dims.trackWidth - dims.thumbSize - dims.thumbPadding
      let thumbX := leftX + (rightX - leftX) * progress
      let thumbY := rect.y + (dims.trackHeight - dims.thumbSize) / 2
      let thumbRect := Arbor.Rect.mk' thumbX thumbY dims.thumbSize dims.thumbSize
      -- Thumb color: white normally, slightly gray when hovered
      let thumbColor := if hovered then Color.gray 0.95 else Color.white
      RenderM.fillRect thumbRect thumbColor (dims.thumbSize / 2)
  draw := none
}

end Switch

/-- Build a visual switch (WidgetBuilder version).
    - `name`: Widget name for hit testing
    - `labelText`: Optional text to display next to switch
    - `theme`: Theme for styling
    - `isOn`: Whether the switch is currently on
    - `state`: Widget interaction state (hover, focus, etc.)
-/
def switchVisual (name : String) (labelText : Option String) (theme : Theme)
    (isOn : Bool) (state : WidgetState := {}) : WidgetBuilder := do
  let dims := Switch.defaultDimensions
  -- Track background color: primary when on, gray when off
  let trackBg := if isOn then theme.primary.background else Color.gray 0.3
  let borderColor := if state.focused then theme.input.borderFocused else trackBg

  let switchTrack : WidgetBuilder := do
    custom (Switch.trackSpec isOn state.hovered theme dims) {
      minWidth := some dims.trackWidth
      minHeight := some dims.trackHeight
      cornerRadius := dims.trackHeight / 2  -- Fully rounded ends
      borderColor := some borderColor
      borderWidth := if state.focused then 2 else 0
      backgroundColor := some trackBg
    }

  -- Use custom flex container with alignItems := .center to prevent stretching
  let wid ← freshId
  let props : Trellis.FlexContainer := { Trellis.FlexContainer.row 8 with alignItems := .center }
  let track ← switchTrack
  match labelText with
  | some text =>
    let label ← text' text theme.font theme.text .left
    pure (.flex wid (some name) props {} #[track, label])
  | none =>
    pure (.flex wid (some name) props {} #[track])

/-- Build a visual switch without label (WidgetBuilder version). -/
def switchOnlyVisual (name : String) (theme : Theme)
    (isOn : Bool) (state : WidgetState := {}) : WidgetBuilder :=
  switchVisual name none theme isOn state

/-- Build an animated visual switch (WidgetBuilder version).
    - `name`: Widget name for hit testing
    - `labelText`: Optional text to display next to switch
    - `theme`: Theme for styling
    - `progress`: Animation progress 0.0 (off) to 1.0 (on)
    - `state`: Widget interaction state (hover, focus, etc.)
-/
def animatedSwitchVisual (name : String) (labelText : Option String) (theme : Theme)
    (progress : Float) (state : WidgetState := {}) : WidgetBuilder := do
  let dims := Switch.defaultDimensions
  -- Interpolate track background color based on progress
  let offColor := Color.gray 0.3
  let onColor := theme.primary.background
  let trackBg := Color.lerp offColor onColor progress
  let borderColor := if state.focused then theme.input.borderFocused else trackBg

  let switchTrack : WidgetBuilder := do
    custom (Switch.animatedTrackSpec progress state.hovered dims) {
      minWidth := some dims.trackWidth
      minHeight := some dims.trackHeight
      cornerRadius := dims.trackHeight / 2  -- Fully rounded ends
      borderColor := some borderColor
      borderWidth := if state.focused then 2 else 0
      backgroundColor := some trackBg
    }

  -- Use custom flex container with alignItems := .center to prevent stretching
  let wid ← freshId
  let props : Trellis.FlexContainer := { Trellis.FlexContainer.row 8 with alignItems := .center }
  let track ← switchTrack
  match labelText with
  | some text =>
    let label ← text' text theme.font theme.text .left
    pure (.flex wid (some name) props {} #[track, label])
  | none =>
    pure (.flex wid (some name) props {} #[track])

/-! ## Reactive Switch Components (FRP-based)

These use WidgetM for declarative composition with automatic state management and animation.
-/

open Reactive Reactive.Host
open Afferent.Canopy.Reactive

/-- Switch result - events and dynamics. -/
structure SwitchResult where
  onToggle : Reactive.Event Spider Bool
  isOn : Reactive.Dynamic Spider Bool
  animProgress : Reactive.Dynamic Spider Float

/-- Create a reactive switch component using WidgetM with animation.
    Emits the switch widget and returns toggle state.
    - `label`: Optional label text displayed next to switch
    - `initialOn`: Initial on/off state
-/
def switch (label : Option String) (initialOn : Bool := false)
    : WidgetM SwitchResult := do
  let theme ← getThemeW
  let name ← registerComponentW "switch"
  let isHovered ← useHover name
  let clicks ← useClick name
  let animFrames ← useAnimationFrame

  let isOn ← Reactive.foldDyn (fun _ on => !on) initialOn clicks
  let onToggle := isOn.updated

  -- Only subscribe to animation frames while the switch is animating
  let ctx ← SpiderM.getTimelineCtx
  let neverFrames ← SpiderM.liftIO (Reactive.Event.never ctx)
  let (frameSourceUpdates, fireFrameSource) ← Reactive.newTriggerEvent (t := Spider) (a := Reactive.Event Spider Float)
  let frameSourceDyn ← Reactive.foldDyn (fun new _ => new) neverFrames frameSourceUpdates
  let activeFrames ← Event.switchDynM frameSourceDyn

  let initialAnim := if initialOn then 1.0 else 0.0
  let animProgress ← SpiderM.fixDynM fun animBehavior => do
    let updateEvent ← Event.attachWithM
      (fun (anim, on) dt =>
        let animSpeed := 8.0
        let rawFactor := animSpeed * dt
        let lerpFactor := if rawFactor > 1.0 then 1.0 else rawFactor
        let target := if on then 1.0 else 0.0
        let diff := target - anim
        if diff.abs < 0.01 then target else anim + diff * lerpFactor)
      (Reactive.Behavior.zipWith Prod.mk animBehavior isOn.current)
      activeFrames
    Reactive.holdDyn initialAnim updateEvent

  -- Start animation frames on toggle
  let startFrames ← Event.mapM (fun _ => fireFrameSource animFrames) onToggle
  let _ ← performEvent_ startFrames

  -- Stop animation frames when progress reaches the target
  let doneCandidates ← Event.attachWithM
    (fun on anim =>
      let target := if on then 1.0 else 0.0
      if (target - anim).abs < 0.01 then some () else none)
    isOn.current
    animProgress.updated
  let animDone ← Event.mapMaybeM id doneCandidates
  let stopFrames ← Event.mapM (fun _ => fireFrameSource neverFrames) animDone
  let _ ← performEvent_ stopFrames

  -- Combine dynamics for efficient change-driven rebuilds
  let renderState ← Dynamic.zipWithM (fun h a => (h, a)) isHovered animProgress

  -- Only rebuild widget when hover or animation state actually changes
  let _ ← dynWidget renderState fun (hovered, anim) => do
    let state : WidgetState := { hovered, pressed := false, focused := false }
    emit do pure (animatedSwitchVisual name label theme anim state)

  pure { onToggle, isOn, animProgress }

end Afferent.Canopy

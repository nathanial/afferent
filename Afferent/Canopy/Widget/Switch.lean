/-
  Canopy Switch Widget
  iOS-style on/off toggle switch.
-/
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Widget.Button

namespace Afferent.Canopy

open Afferent.Arbor

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
    let cmds : Array RenderCommand := #[]
    -- Draw the thumb (circular knob)
    let thumbX := if isOn then
      rect.x + dims.trackWidth - dims.thumbSize - dims.thumbPadding
    else
      rect.x + dims.thumbPadding
    let thumbY := rect.y + (dims.trackHeight - dims.thumbSize) / 2
    let thumbRect := Arbor.Rect.mk' thumbX thumbY dims.thumbSize dims.thumbSize
    -- Thumb color: white normally, slightly gray when hovered
    let thumbColor := if hovered then Color.gray 0.95 else Color.white
    cmds.push (.fillRect thumbRect thumbColor (dims.thumbSize / 2))
  draw := none
}

/-- Custom spec for animated switch track and thumb rendering.
    `progress` is 0.0 (off) to 1.0 (on), allowing smooth animation. -/
def animatedTrackSpec (progress : Float) (hovered : Bool) (dims : Dimensions := defaultDimensions) : CustomSpec := {
  measure := fun _ _ => (dims.trackWidth, dims.trackHeight)
  collect := fun layout =>
    let rect := layout.contentRect
    let cmds : Array RenderCommand := #[]
    -- Interpolate thumb X position based on progress
    let leftX := rect.x + dims.thumbPadding
    let rightX := rect.x + dims.trackWidth - dims.thumbSize - dims.thumbPadding
    let thumbX := leftX + (rightX - leftX) * progress
    let thumbY := rect.y + (dims.trackHeight - dims.thumbSize) / 2
    let thumbRect := Arbor.Rect.mk' thumbX thumbY dims.thumbSize dims.thumbSize
    -- Thumb color: white normally, slightly gray when hovered
    let thumbColor := if hovered then Color.gray 0.95 else Color.white
    cmds.push (.fillRect thumbRect thumbColor (dims.thumbSize / 2))
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

/-- Create an interactive switch (UIBuilder version).
    - `name`: Unique name for state tracking
    - `labelText`: Optional text to display next to switch
    - `onToggle`: Message to emit with new on state when toggled
    - `theme`: Theme for styling
    - `state`: Current switch state (includes on boolean)
-/
def switch {Msg : Type} (name : String) (labelText : Option String)
    (onToggle : Bool → Msg)
    (theme : Theme)
    (state : SwitchState := {})
    : UIBuilder (WidgetMsg Msg) Widget := do
  let wid ← UIBuilder.freshId

  -- Register event handlers
  UIBuilder.register wid fun _ event =>
    match event with
    | .mouseEnter _ => { msgs := #[.hover name true] }
    | .mouseLeave _ => { msgs := #[.hover name false] }
    | .mouseClick _ =>
        if !state.disabled then
          let newOn := !state.on
          { msgs := #[.app (onToggle newOn)] }
        else {}
    | .keyPress e =>
        if state.focused && (e.key == .space || e.key == .enter) && !state.disabled then
          let newOn := !state.on
          { msgs := #[.app (onToggle newOn)] }
        else {}
    | _ => {}

  UIBuilder.lift (switchVisual name labelText theme state.on state.toWidgetState)

/-- Create an interactive switch without label. -/
def switchOnly {Msg : Type} (name : String)
    (onToggle : Bool → Msg)
    (theme : Theme)
    (state : SwitchState := {})
    : UIBuilder (WidgetMsg Msg) Widget :=
  switch name none onToggle theme state

end Afferent.Canopy

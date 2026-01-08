/-
  Canopy Slider Widget
  Horizontal slider for selecting a value within a range.
-/
import Afferent.Canopy.Core
import Afferent.Canopy.Theme

namespace Afferent.Canopy

open Afferent.Arbor

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
    let cmds : Array RenderCommand := #[]

    -- Clamp value to valid range
    let v := if value < 0.0 then 0.0 else if value > 1.0 then 1.0 else value

    -- Track vertical center
    let trackY := rect.y + (rect.height - dims.trackHeight) / 2
    let trackRect := Arbor.Rect.mk' rect.x trackY dims.trackWidth dims.trackHeight

    -- Background track (gray)
    let trackBg := Color.gray 0.3
    let cmds := cmds.push (.fillRect trackRect trackBg (dims.trackHeight / 2))

    -- Filled portion (primary color)
    let filledWidth := dims.trackWidth * v
    if filledWidth > 0 then
      let filledRect := Arbor.Rect.mk' rect.x trackY filledWidth dims.trackHeight
      let cmds := cmds.push (.fillRect filledRect theme.primary.background (dims.trackHeight / 2))

      -- Thumb position (centered on value position)
      let thumbX := rect.x + (dims.trackWidth - dims.thumbSize) * v
      let thumbY := rect.y + (rect.height - dims.thumbSize) / 2
      let thumbRect := Arbor.Rect.mk' thumbX thumbY dims.thumbSize dims.thumbSize

      -- Thumb color: white normally, slightly gray when hovered
      let thumbColor := if hovered then Color.gray 0.95 else Color.white
      let cmds := cmds.push (.fillRect thumbRect thumbColor (dims.thumbSize / 2))

      -- Focus ring on thumb
      if focused then
        let focusRect := Arbor.Rect.mk' (thumbX - 2) (thumbY - 2)
                                         (dims.thumbSize + 4) (dims.thumbSize + 4)
        cmds.push (.strokeRect focusRect theme.focusRing 2.0 ((dims.thumbSize + 4) / 2))
      else
        cmds
    else
      -- Thumb at start position
      let thumbX := rect.x
      let thumbY := rect.y + (rect.height - dims.thumbSize) / 2
      let thumbRect := Arbor.Rect.mk' thumbX thumbY dims.thumbSize dims.thumbSize
      let thumbColor := if hovered then Color.gray 0.95 else Color.white
      let cmds := cmds.push (.fillRect thumbRect thumbColor (dims.thumbSize / 2))

      if focused then
        let focusRect := Arbor.Rect.mk' (thumbX - 2) (thumbY - 2)
                                         (dims.thumbSize + 4) (dims.thumbSize + 4)
        cmds.push (.strokeRect focusRect theme.focusRing 2.0 ((dims.thumbSize + 4) / 2))
      else
        cmds
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

end Afferent.Canopy

/-
  Canopy Toast Widget
  Temporary notification messages with auto-dismiss and variants.
-/
import Afferent.Canopy.Core
import Afferent.Canopy.Theme

namespace Afferent.Canopy

open Afferent.Arbor

/-- Toast variant for different notification types. -/
inductive ToastVariant where
  | info
  | success
  | warning
  | error
deriving Repr, BEq, Inhabited

namespace Toast

/-- Dimensions for toast rendering. -/
structure Dimensions where
  minWidth : Float := 280.0
  maxWidth : Float := 400.0
  padding : Float := 12.0
  cornerRadius : Float := 8.0
  iconSize : Float := 20.0
  gap : Float := 10.0
deriving Repr, Inhabited

/-- Default toast dimensions. -/
def defaultDimensions : Dimensions := {}

/-- Get colors for a toast variant. -/
def variantColors (variant : ToastVariant) : Color × Color × Color :=
  match variant with
  | .info => (Color.rgba 0.2 0.5 0.9 1.0, Color.rgba 0.1 0.15 0.25 0.95, Color.white)
  | .success => (Color.rgba 0.2 0.75 0.4 1.0, Color.rgba 0.1 0.2 0.12 0.95, Color.white)
  | .warning => (Color.rgba 0.95 0.7 0.2 1.0, Color.rgba 0.25 0.2 0.1 0.95, Color.rgba 0.1 0.1 0.1 1.0)
  | .error => (Color.rgba 0.9 0.25 0.25 1.0, Color.rgba 0.25 0.1 0.1 0.95, Color.white)

/-- Get icon character for a toast variant. -/
def variantIcon (variant : ToastVariant) : String :=
  match variant with
  | .info => "i"
  | .success => "✓"
  | .warning => "!"
  | .error => "✕"

end Toast

/-- Build a toast notification visual.
    - `name`: Widget name for identification
    - `message`: The notification message
    - `variant`: Type of notification (info, success, warning, error)
    - `theme`: Theme for styling
    - `dismissName`: Optional widget name for dismiss button
    - `opacity`: Opacity for fade in/out animation (0.0 to 1.0)
-/
def toastVisual (name : String) (message : String)
    (variant : ToastVariant := .info) (theme : Theme)
    (dismissName : Option String := none) (opacity : Float := 1.0)
    (dims : Toast.Dimensions := Toast.defaultDimensions) : WidgetBuilder := do
  let (accentColor, bgColor, textColor) := Toast.variantColors variant
  let iconChar := Toast.variantIcon variant

  -- Icon circle with variant color
  let iconCircle : WidgetBuilder := do
    let circleId ← freshId
    let iconStyle : BoxStyle := {
      backgroundColor := some accentColor
      cornerRadius := dims.iconSize / 2
      minWidth := some dims.iconSize
      minHeight := some dims.iconSize
    }
    let iconText ← text' iconChar theme.smallFont textColor .center
    let iconProps : Trellis.FlexContainer := { Trellis.FlexContainer.row 0 with justifyContent := .center, alignItems := .center }
    pure (.flex circleId none iconProps iconStyle #[iconText])

  -- Message text
  let messageText ← text' message theme.font textColor .left

  -- Build content row (icon + message)
  let contentId ← freshId
  let contentProps : Trellis.FlexContainer := { Trellis.FlexContainer.row dims.gap with alignItems := .center }
  let icon ← iconCircle
  let content := Widget.flex contentId none contentProps {} #[icon, messageText]

  -- Main container ID
  let wid ← freshId

  -- Dismiss button (X) if provided
  let finalContent ← match dismissName with
  | some dName =>
    let dismissId ← freshId
    let dismissStyle : BoxStyle := {
      padding := Trellis.EdgeInsets.uniform 4
      cornerRadius := 4
    }
    let dismissText ← text' "✕" theme.smallFont (textColor.withAlpha 0.7) .center
    let dismissBtn := Widget.rect dismissId (some dName) dismissStyle

    let rowId ← freshId
    let rowProps : Trellis.FlexContainer := {
      Trellis.FlexContainer.row dims.gap with
      justifyContent := .spaceBetween
      alignItems := .center
    }
    pure (Widget.flex rowId none rowProps {} #[content, dismissBtn, dismissText])
  | none => pure content

  -- Toast container with background
  let containerStyle : BoxStyle := {
    backgroundColor := some (bgColor.withAlpha (bgColor.a * opacity))
    cornerRadius := dims.cornerRadius
    padding := Trellis.EdgeInsets.uniform dims.padding
    minWidth := some dims.minWidth
    borderColor := some (accentColor.withAlpha (0.5 * opacity))
    borderWidth := 1.0
  }

  let containerProps : Trellis.FlexContainer := { Trellis.FlexContainer.row 0 with alignItems := .center }
  pure (.flex wid (some name) containerProps containerStyle #[finalContent])

/-- Build a toast container that positions toasts at the bottom-right of the screen.
    Uses absolute positioning to overlay on top of other content.
    - `name`: Widget name for the container
    - `toasts`: Array of toast widgets to display
    - `gap`: Vertical gap between toasts
-/
def toastContainerVisual (name : String) (toasts : Array Widget)
    (gap : Float := 8.0) : WidgetBuilder := do
  let wid ← freshId

  let containerStyle : BoxStyle := {
    position := .absolute
    bottom := some 16
    right := some 16
    padding := Trellis.EdgeInsets.uniform 0
  }

  let props : Trellis.FlexContainer := {
    Trellis.FlexContainer.column gap with
    alignItems := .flexEnd
  }

  pure (.flex wid (some name) props containerStyle toasts)

end Afferent.Canopy

/-
  Canopy Toast Widget
  Temporary notification messages with auto-dismiss and variants.
-/
import Reactive
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Reactive.Component

namespace Afferent.Canopy

open Afferent.Arbor hiding Event

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
    layer := .overlay
    bottom := some 16
    right := some 16
    padding := Trellis.EdgeInsets.uniform 0
  }

  let props : Trellis.FlexContainer := {
    Trellis.FlexContainer.column gap with
    alignItems := .flexEnd
  }

  pure (.flex wid (some name) props containerStyle toasts)

/-! ## Reactive Toast Components (FRP-based)

These use WidgetM for declarative composition with auto-dismiss.
-/

open Reactive Reactive.Host
open Afferent.Canopy.Reactive

/-- A single toast notification with its metadata. -/
structure ToastItem where
  id : Nat
  message : String
  variant : ToastVariant
  createdAt : Float  -- Time when created (for auto-dismiss)
  duration : Float   -- How long to show (in seconds)
deriving Repr, BEq, Inhabited

/-- State for the toast manager. -/
structure ToastState where
  toasts : Array ToastItem
  nextId : Nat
  currentTime : Float
deriving Repr, BEq, Inhabited

/-- Toast manager result - provides functions to show toasts and current state. -/
structure ToastManagerResult where
  /-- Show an info toast. -/
  showInfo : String → IO Unit
  /-- Show a success toast. -/
  showSuccess : String → IO Unit
  /-- Show a warning toast. -/
  showWarning : String → IO Unit
  /-- Show an error toast. -/
  showError : String → IO Unit
  /-- Current list of active toasts. -/
  toasts : Reactive.Dynamic Spider (Array ToastItem)

/-- Create a toast manager component using WidgetM.
    Manages a stack of toast notifications with auto-dismiss.
    Emits toast visuals and provides functions to show new toasts.
    - `defaultDuration`: How long toasts are shown (in seconds)
-/
def toastManager (defaultDuration : Float := 3.0) : WidgetM ToastManagerResult := do
  let theme ← getThemeW
  let containerName ← registerComponentW "toast-container" (isInteractive := false)

  -- Animation frames for timing
  let animFrame ← useAnimationFrame

  -- Trigger events for showing toasts (message, variant)
  let (showTrigger, fireShow) ← Reactive.newTriggerEvent (t := Spider) (a := String × ToastVariant)

  -- Combined state: toasts, next ID, and current time
  let state ← Reactive.foldDyn
    (fun (event : Float ⊕ (String × ToastVariant)) (s : ToastState) =>
      match event with
      | .inl dt =>
        -- Animation frame: update time and remove expired toasts
        let newTime := s.currentTime + dt
        let activeToasts := s.toasts.filter fun t => newTime - t.createdAt < t.duration
        { s with currentTime := newTime, toasts := activeToasts }
      | .inr (msg, variant) =>
        -- New toast: add to list
        let item : ToastItem := {
          id := s.nextId
          message := msg
          variant := variant
          createdAt := s.currentTime
          duration := defaultDuration
        }
        { s with toasts := s.toasts.push item, nextId := s.nextId + 1 })
    { toasts := #[], nextId := 0, currentTime := 0.0 }
    (← Event.mergeM (← Event.mapM Sum.inl animFrame) (← Event.mapM Sum.inr showTrigger))

  let toasts ← Dynamic.mapM (·.toasts) state

  -- Helper functions to show toasts
  let showToast (msg : String) (variant : ToastVariant) : IO Unit :=
    fireShow (msg, variant)

  -- Use dynWidget for efficient change-driven rebuilds
  let _ ← dynWidget toasts fun currentToasts => do
    emit do
      if currentToasts.isEmpty then
        pure (spacer 0 0)
      else
        pure do
          let mut toastWidgets : Array Widget := #[]
          for toast in currentToasts do
            let toastName := s!"toast-{toast.id}"
            let widget ← toastVisual toastName toast.message toast.variant theme
            toastWidgets := toastWidgets.push widget
          toastContainerVisual containerName toastWidgets

  pure {
    showInfo := fun msg => showToast msg .info
    showSuccess := fun msg => showToast msg .success
    showWarning := fun msg => showToast msg .warning
    showError := fun msg => showToast msg .error
    toasts
  }

end Afferent.Canopy

/-
  Canopy Button Widget
  Interactive button with hover/press states and multiple variants.
-/
import Reactive
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Reactive.Component

namespace Afferent.Canopy

open Afferent.Arbor hiding Event

/-- Button visual variants. -/
inductive ButtonVariant where
  | primary    -- Filled, prominent (for primary actions)
  | secondary  -- Filled, less prominent (for secondary actions)
  | outline    -- Border only (for tertiary actions)
  | ghost      -- Text only, minimal (for subtle actions)
deriving Repr, BEq, Inhabited

/-- Standard widget message type for Canopy widgets. -/
inductive WidgetMsg (Msg : Type) where
  | app (msg : Msg)                           -- User application message
  | hover (name : String) (hovered : Bool)    -- Hover state change
  | focus (name : String) (focused : Bool)    -- Focus state change
  | press (name : String) (pressed : Bool)    -- Press state change
deriving Repr

namespace Button

/-- Get colors for a button variant from theme. -/
def variantColors (theme : Theme) : ButtonVariant → InteractiveColors
  | .primary   => theme.primary
  | .secondary => theme.secondary
  | .outline   => theme.outline
  | .ghost     => theme.outline

/-- Compute background color based on state. -/
def backgroundColor (colors : InteractiveColors) (state : WidgetState) : Color :=
  if state.disabled then colors.backgroundDisabled
  else if state.pressed then colors.backgroundPressed
  else if state.hovered then colors.backgroundHover
  else colors.background

/-- Compute foreground color based on state. -/
def foregroundColor (colors : InteractiveColors) (state : WidgetState) : Color :=
  if state.disabled then colors.foregroundDisabled
  else colors.foreground

/-- Compute border width for variant. -/
def borderWidth : ButtonVariant → Float
  | .outline => 1.0
  | .ghost   => 0.0
  | _        => 0.0

end Button

/-- Create an interactive button.
    - `name`: Unique name for state tracking
    - `label`: Button text
    - `onClick`: Message to emit when clicked
    - `theme`: Theme for styling
    - `state`: Current interaction state (from WidgetStates)
    - `variant`: Visual variant (primary, secondary, outline, ghost)
-/
def button {Msg : Type} (name : String) (label : String)
    (onClick : Msg)
    (theme : Theme)
    (state : WidgetState := {})
    (variant : ButtonVariant := .primary)
    : UIBuilder (WidgetMsg Msg) Widget := do
  let wid ← UIBuilder.freshId

  let colors := Button.variantColors theme variant
  let bgColor := Button.backgroundColor colors state
  let fgColor := Button.foregroundColor colors state
  let bw := Button.borderWidth variant

  let style : BoxStyle := {
    backgroundColor := some bgColor
    borderColor := if bw > 0 then some colors.border else none
    borderWidth := bw
    cornerRadius := theme.cornerRadius
    padding := Trellis.EdgeInsets.symmetric theme.padding (theme.padding * 0.6)
  }

  -- Register event handlers
  UIBuilder.register wid fun _ event =>
    match event with
    | .mouseEnter _ => { msgs := #[.hover name true] }
    | .mouseLeave _ => { msgs := #[.hover name false, .press name false] }
    | .mouseDown _ =>
        if !state.disabled then
          { msgs := #[.press name true], capture := some wid }
        else {}
    | .mouseUp _ =>
        if state.pressed && !state.disabled then
          { msgs := #[.press name false, .app onClick], releaseCapture := true }
        else
          { msgs := #[.press name false], releaseCapture := true }
    | _ => {}

  -- Build visual structure
  UIBuilder.lift do
    center (style := style) do
      text' label theme.font fgColor .center

/-- Create a primary button. -/
def primaryButton {Msg : Type} (name : String) (label : String)
    (onClick : Msg) (theme : Theme) (state : WidgetState := {})
    : UIBuilder (WidgetMsg Msg) Widget :=
  button name label onClick theme state .primary

/-- Create a secondary button. -/
def secondaryButton {Msg : Type} (name : String) (label : String)
    (onClick : Msg) (theme : Theme) (state : WidgetState := {})
    : UIBuilder (WidgetMsg Msg) Widget :=
  button name label onClick theme state .secondary

/-- Create an outline button. -/
def outlineButton {Msg : Type} (name : String) (label : String)
    (onClick : Msg) (theme : Theme) (state : WidgetState := {})
    : UIBuilder (WidgetMsg Msg) Widget :=
  button name label onClick theme state .outline

/-- Create a ghost button. -/
def ghostButton {Msg : Type} (name : String) (label : String)
    (onClick : Msg) (theme : Theme) (state : WidgetState := {})
    : UIBuilder (WidgetMsg Msg) Widget :=
  button name label onClick theme state .ghost

/-! ## Reactive Button Components (FRP-based)

These use WidgetM for declarative composition with automatic event handling.
-/

open Reactive Reactive.Host
open Afferent.Canopy.Reactive

/-- Build the visual for a button given its state (pure WidgetBuilder). -/
def buttonVisual (name : String) (labelText : String) (theme : Theme)
    (variant : ButtonVariant) (state : WidgetState) : WidgetBuilder := do
  let colors := Button.variantColors theme variant
  let bgColor := Button.backgroundColor colors state
  let fgColor := Button.foregroundColor colors state
  let bw := Button.borderWidth variant

  let style : BoxStyle := {
    backgroundColor := some bgColor
    borderColor := if bw > 0 then some colors.border else none
    borderWidth := bw
    cornerRadius := theme.cornerRadius
    padding := Trellis.EdgeInsets.symmetric theme.padding (theme.padding * 0.6)
  }

  namedCenter name (style := style) do
    text' labelText theme.font fgColor .center

/-- Create a reactive button component using WidgetM.
    Emits the button widget and returns the onClick event.
    - `label`: Button text
    - `theme`: Theme for styling
    - `variant`: Visual variant (primary, secondary, outline, ghost)
-/
def Button.reactive (label : String) (theme : Theme) (variant : ButtonVariant := .primary)
    : WidgetM (Reactive.Event Spider Unit) := do
  let name ← registerComponentW "button"
  let isHovered ← useHover name
  let onClick ← useClick name

  emit do
    let hovered ← isHovered.sample
    let state : WidgetState := { hovered, pressed := false, focused := false }
    pure (buttonVisual name label theme variant state)

  pure onClick

end Afferent.Canopy

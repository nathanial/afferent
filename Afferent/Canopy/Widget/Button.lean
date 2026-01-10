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
def button (label : String) (theme : Theme) (variant : ButtonVariant := .primary)
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

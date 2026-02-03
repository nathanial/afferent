/-
  Canopy Button Widget
  Interactive button with hover/press states and multiple variants.
-/
import Reactive
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Reactive.Component
import Afferent.Canopy.Widget.Display.Spinner.Component
import Afferent.Canopy.Widget.Display.Link
import Afferent.Canopy.Widget.Input.Dropdown

namespace Afferent.Canopy

open Afferent.Arbor hiding Event

/-- Button visual variants. -/
inductive ButtonVariant where
  | primary    -- Filled, prominent (for primary actions)
  | secondary  -- Filled, less prominent (for secondary actions)
  | danger     -- Destructive action (red-toned)
  | success    -- Positive/confirm action (green-toned)
  | outline    -- Border only (for tertiary actions)
  | ghost      -- Text only, minimal (for subtle actions)
deriving Repr, BEq, Inhabited

/-- Icon placement for icon+label buttons. -/
inductive IconPosition where
  | leading
  | trailing
deriving Repr, BEq, Inhabited

namespace Button

/-- Get colors for a button variant from theme. -/
def variantColors (theme : Theme) : ButtonVariant → InteractiveColors
  | .primary   => theme.primary
  | .secondary => theme.secondary
  | .danger    => theme.danger
  | .success   => theme.success
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

/-- Build button content (label + optional icon). -/
def content (label : String) (icon : Option String) (iconPosition : IconPosition)
    (font : FontId) (color : Color) (gap : Float := 6.0) : WidgetBuilder := do
  match icon with
  | none => text' label font color .center
  | some iconText =>
      if label.isEmpty then
        text' iconText font color .center
      else
        let iconWidget : WidgetBuilder := text' iconText font color .center
        let labelWidget : WidgetBuilder := text' label font color .center
        let children := if iconPosition == .leading
          then #[iconWidget, labelWidget]
          else #[labelWidget, iconWidget]
        rowCenter (gap := gap) (style := {}) children

/-- Build the visual for a button with optional icon and custom dimensions. -/
def buttonVisualWith (name : String) (label : String) (icon : Option String)
    (iconPosition : IconPosition) (theme : Theme)
    (variant : ButtonVariant) (state : WidgetState)
    (paddingX paddingY : Float) (cornerRadius : Float)
    (font : FontId := theme.font)
    (minWidth : Option Float := none) (minHeight : Option Float := none)
    (width : Option Float := none) (height : Option Float := none) : WidgetBuilder := do
  let colors := Button.variantColors theme variant
  let bgColor := Button.backgroundColor colors state
  let fgColor := Button.foregroundColor colors state
  let bw := Button.borderWidth variant
  let widthDim := match width with
    | some value => .length value
    | none => .auto
  let heightDim := match height with
    | some value => .length value
    | none => .auto

  let style : BoxStyle := {
    backgroundColor := some bgColor
    borderColor := if bw > 0 then some colors.border else none
    borderWidth := bw
    cornerRadius := cornerRadius
    padding := Trellis.EdgeInsets.symmetric paddingX paddingY
    minWidth := minWidth
    minHeight := minHeight
    width := widthDim
    height := heightDim
  }

  namedCenter name (style := style) do
    content label icon iconPosition font fgColor

end Button

/-! ## Reactive Button Components (FRP-based)

These use WidgetM for declarative composition with automatic event handling.
-/

open Reactive Reactive.Host
open Afferent.Canopy.Reactive

/-! ## Button Hover/Press State -/

/-- Track hover state for a widget name using the hover fan. -/
private def buttonHoverState (name : String) : WidgetM (Reactive.Dynamic Spider Bool) := do
  useHover name

/-- Track pressed state for a widget name (left mouse button). -/
private def buttonPressState (name : String) : WidgetM (Reactive.Dynamic Spider Bool) := do
  let clickData ← useClickData name
  let allMouseUp ← useAllMouseUp

  let pressDown ← Event.mapMaybeM (fun data =>
    if data.click.button == 0 then some true else none) clickData
  let pressUp ← Event.mapM (fun _ => false) allMouseUp
  let transitions ← Event.leftmostM [pressDown, pressUp]
  Reactive.holdDyn false transitions

/-- Shared helper for hover-driven button rendering. -/
private def buttonWithVisual (namePrefix : String)
    (render : String → Theme → WidgetState → WidgetBuilder)
    : WidgetM (Reactive.Event Spider Unit) := do
  let theme ← getThemeW
  let name ← registerComponentW namePrefix
  let isHovered ← buttonHoverState name
  let isPressed ← buttonPressState name
  let onClick ← useClick name

  let renderState ← Dynamic.zipWithM (fun hovered pressed => (hovered, pressed)) isHovered isPressed
  let _ ← dynWidget renderState fun (hovered, pressed) => do
    let state : WidgetState := { hovered, pressed, focused := false }
    emit do pure (render name theme state)

  pure onClick

/-- Build the visual for a button given its state (pure WidgetBuilder). -/
def buttonVisual (name : String) (labelText : String) (theme : Theme)
    (variant : ButtonVariant) (state : WidgetState) : WidgetBuilder := do
  Button.buttonVisualWith name labelText none .leading theme variant state
    theme.padding (theme.padding * 0.6) theme.cornerRadius

/-- Create a reactive button component using WidgetM.
    Emits the button widget and returns the onClick event.
    - `label`: Button text
    - `variant`: Visual variant (primary, secondary, outline, ghost)
-/
def button (label : String) (variant : ButtonVariant := .primary)
    : WidgetM (Reactive.Event Spider Unit) := do
  buttonWithVisual "button" fun name theme state =>
    buttonVisual name label theme variant state

/-- Icon-only button (square). -/
def iconButton (icon : String) (variant : ButtonVariant := .secondary)
    (size : Float := 32.0) : WidgetM (Reactive.Event Spider Unit) := do
  buttonWithVisual "icon-button" fun name theme state =>
    Button.buttonVisualWith name "" (some icon) .leading theme variant state
      (theme.padding * 0.5) (theme.padding * 0.5) theme.cornerRadius
      (minWidth := some size) (minHeight := some size)

/-- Icon + label button. -/
def iconLabelButton (label : String) (icon : String)
    (variant : ButtonVariant := .primary)
    (iconPosition : IconPosition := .leading)
    : WidgetM (Reactive.Event Spider Unit) := do
  buttonWithVisual "icon-label-button" fun name theme state =>
    Button.buttonVisualWith name label (some icon) iconPosition theme variant state
      theme.padding (theme.padding * 0.6) theme.cornerRadius

/-- Floating Action Button (FAB). -/
def fabButton (icon : String) (variant : ButtonVariant := .primary)
    (size : Float := 56.0) : WidgetM (Reactive.Event Spider Unit) := do
  buttonWithVisual "fab-button" fun name theme state =>
    Button.buttonVisualWith name "" (some icon) .leading theme variant state
      0 0 (size / 2)
      (width := some size) (height := some size)

/-- Mini Floating Action Button. -/
def miniFabButton (icon : String) (variant : ButtonVariant := .primary)
    (size : Float := 40.0) : WidgetM (Reactive.Event Spider Unit) := do
  buttonWithVisual "mini-fab-button" fun name theme state =>
    Button.buttonVisualWith name "" (some icon) .leading theme variant state
      0 0 (size / 2)
      (width := some size) (height := some size)

/-- Extended FAB with icon + label. -/
def extendedFabButton (label : String) (icon : String)
    (variant : ButtonVariant := .primary)
    (height : Float := 48.0) : WidgetM (Reactive.Event Spider Unit) := do
  buttonWithVisual "extended-fab-button" fun name theme state =>
    Button.buttonVisualWith name label (some icon) .leading theme variant state
      (theme.padding * 1.2) (theme.padding * 0.6) (height / 2)
      (minHeight := some height)

/-- Pill-shaped button (fully rounded corners). -/
def pillButton (label : String) (variant : ButtonVariant := .primary)
    : WidgetM (Reactive.Event Spider Unit) := do
  buttonWithVisual "pill-button" fun name theme state =>
    Button.buttonVisualWith name label none .leading theme variant state
      theme.padding (theme.padding * 0.6) 999.0

/-- Compact button with reduced padding and smaller font. -/
def compactButton (label : String) (variant : ButtonVariant := .primary)
    (icon : Option String := none) (iconPosition : IconPosition := .leading)
    : WidgetM (Reactive.Event Spider Unit) := do
  buttonWithVisual "compact-button" fun name theme state =>
    Button.buttonVisualWith name label icon iconPosition theme variant state
      (theme.padding * 0.6) (theme.padding * 0.35) theme.cornerRadius
      (font := theme.smallFont)

/-- Convenience: Danger button. -/
def dangerButton (label : String) : WidgetM (Reactive.Event Spider Unit) :=
  button label .danger

/-- Convenience: Success button. -/
def successButton (label : String) : WidgetM (Reactive.Event Spider Unit) :=
  button label .success

/-- Link-style button (inline text with underline on hover). -/
def linkButton (label : String) (color : Option Color := none)
    : WidgetM (Reactive.Event Spider Unit) :=
  link label color

/-- Link-style button with an icon prefix. -/
def linkButtonWithIcon (label : String) (icon : String)
    (color : Option Color := none) : WidgetM (Reactive.Event Spider Unit) :=
  linkWithIcon label icon color

/-! ## Toggle Buttons -/

structure ToggleButtonResult where
  onToggle : Reactive.Event Spider Bool
  isOn : Reactive.Dynamic Spider Bool

/-- Toggle button that stays pressed when active. -/
def toggleButton (label : String) (variant : ButtonVariant := .secondary)
    (initialOn : Bool := false) : WidgetM ToggleButtonResult := do
  let theme ← getThemeW
  let name ← registerComponentW "toggle-button"
  let isHovered ← buttonHoverState name
  let isPressed ← buttonPressState name
  let clicks ← useClick name

  let isOn ← Reactive.foldDyn (fun _ s => !s) initialOn clicks
  let onToggle := isOn.updated
  let renderState1 ← Dynamic.zipWithM (fun hovered on => (hovered, on)) isHovered isOn
  let renderState ← Dynamic.zipWithM (fun (hovered, on) pressed => (hovered, on, pressed))
    renderState1 isPressed

  let _ ← dynWidget renderState fun (hovered, on, pressed) => do
    let state : WidgetState := { hovered, pressed := (on || pressed), focused := false }
    emit do pure (buttonVisual name label theme variant state)

  pure { onToggle, isOn }

structure ToggleGroupResult where
  onSelect : Reactive.Event Spider Nat
  selection : Reactive.Dynamic Spider Nat

/-- Row of mutually exclusive toggle buttons. -/
def toggleGroup (labels : Array String) (initialSelection : Nat := 0)
    (activeVariant : ButtonVariant := .primary)
    (inactiveVariant : ButtonVariant := .outline) : WidgetM ToggleGroupResult := do
  let theme ← getThemeW
  let mut buttonNames : Array String := #[]
  for _ in labels do
    let name ← registerComponentW "toggle-group-btn"
    buttonNames := buttonNames.push name

  let allClicks ← useAllClicks
  let allMouseUp ← useAllMouseUp
  let findClicked (data : ClickData) : Option Nat :=
    if data.click.button != 0 then none
    else
      (List.range labels.size).findSome? fun i =>
        let name := buttonNames.getD i ""
        if hitWidget data name then some i else none
  let onSelect ← Event.mapMaybeM findClicked allClicks
  let selection ← Reactive.holdDyn initialSelection onSelect

  let allHovers ← useAllHovers
  let hoverChanges ← Event.mapM (fun data =>
    (List.range labels.size).findSome? fun i =>
      let name := buttonNames.getD i ""
      if hitWidgetHover data name then some i else none) allHovers
  let hoveredIdx ← Reactive.holdDyn none hoverChanges
  let pressDown ← Event.mapM (fun idx => some idx) onSelect
  let pressUp ← Event.mapM (fun _ => (none : Option Nat)) allMouseUp
  let pressedEvents ← Event.leftmostM [pressDown, pressUp]
  let pressedIdx ← Reactive.holdDyn none pressedEvents
  let renderState1 ← Dynamic.zipWithM (fun sel hov => (sel, hov)) selection hoveredIdx
  let renderState ← Dynamic.zipWithM (fun (sel, hov) pressed => (sel, hov, pressed))
    renderState1 pressedIdx

  let labelsRef := labels
  let buttonNamesRef := buttonNames

  let _ ← dynWidget renderState fun (sel, hov, pressedOpt) => do
    let containerStyle : BoxStyle := {
      backgroundColor := some theme.panel.background
      borderColor := some theme.panel.border
      borderWidth := 1
      cornerRadius := theme.cornerRadius
      padding := Trellis.EdgeInsets.uniform 2
    }

    row' (gap := 0) (style := containerStyle) do
      for i in [:labelsRef.size] do
        if i > 0 then
          emit do
            let dividerStyle : BoxStyle := {
              backgroundColor := some (theme.panel.border.withAlpha 0.6)
              width := .length 1.0
              height := .percent 1.0
            }
            let dividerBuilder : WidgetBuilder := do
              let dividerWid ← freshId
              pure (.rect dividerWid none dividerStyle)
            pure dividerBuilder

        let label := labelsRef.getD i ""
        let name := buttonNamesRef.getD i ""
        let isActive := i == sel
        let isHovered := hov == some i
        let isPressed := pressedOpt == some i
        let variant := if isActive then activeVariant else inactiveVariant
        let colors := Button.variantColors theme variant
        let state : WidgetState := { hovered := isHovered, pressed := isPressed, focused := false }
        let bgColor := Button.backgroundColor colors state
        let fgColor := Button.foregroundColor colors state

        let style : BoxStyle := {
          backgroundColor := some bgColor
          borderWidth := 0
          cornerRadius := 0
          padding := Trellis.EdgeInsets.symmetric (theme.padding * 0.8) (theme.padding * 0.45)
          minWidth := some 64
          minHeight := some 32
        }

        emit do
          pure (namedCenter name (style := style) do
            text' label theme.font fgColor .center)

  pure { onSelect, selection }

/-! ## Split & Dropdown Buttons -/

structure SplitButtonResult where
  onPrimary : Reactive.Event Spider Unit
  onMenu : Reactive.Event Spider Unit

private def splitButtonVisual (primaryName menuName : String) (label : String)
    (theme : Theme) (variant : ButtonVariant)
    (primaryState menuState : WidgetState) : WidgetBuilder := do
  let colors := Button.variantColors theme variant
  let bw := Button.borderWidth variant
  let dividerColor := colors.foreground.withAlpha 0.2

  let outerStyle : BoxStyle := {
    borderColor := if bw > 0 then some colors.border else none
    borderWidth := bw
    cornerRadius := theme.cornerRadius
  }

  let primaryStyle : BoxStyle := {
    backgroundColor := some (Button.backgroundColor colors primaryState)
    padding := Trellis.EdgeInsets.symmetric theme.padding (theme.padding * 0.6)
  }
  let menuStyle : BoxStyle := {
    backgroundColor := some (Button.backgroundColor colors menuState)
    padding := Trellis.EdgeInsets.symmetric (theme.padding * 0.6) (theme.padding * 0.6)
  }

  let dividerStyle : BoxStyle := {
    backgroundColor := some dividerColor
    width := .length 1.0
    height := .percent 1.0
  }

  let leftText ← text' label theme.font (Button.foregroundColor colors primaryState) .center
  let caretText ← text' "v" theme.font (Button.foregroundColor colors menuState) .center

  let left ← namedCenter primaryName (style := primaryStyle) (pure leftText)
  let right ← namedCenter menuName (style := menuStyle) (pure caretText)
  let dividerWid ← freshId
  let divider : Widget := .rect dividerWid none dividerStyle

  let wid ← freshId
  let props : Trellis.FlexContainer := { Trellis.FlexContainer.row 0 with alignItems := .stretch }
  pure (.flex wid none props outerStyle #[left, divider, right])

/-- Split button (primary action + menu trigger). -/
def splitButton (label : String) (variant : ButtonVariant := .primary)
    : WidgetM SplitButtonResult := do
  let theme ← getThemeW
  let primaryName ← registerComponentW "split-primary"
  let menuName ← registerComponentW "split-menu"
  let primaryHover ← buttonHoverState primaryName
  let menuHover ← buttonHoverState menuName
  let primaryPressed ← buttonPressState primaryName
  let menuPressed ← buttonPressState menuName
  let onPrimary ← useClick primaryName
  let onMenu ← useClick menuName

  let renderState1 ← Dynamic.zipWithM (fun h1 h2 => (h1, h2)) primaryHover menuHover
  let renderState2 ← Dynamic.zipWithM (fun p1 p2 => (p1, p2)) primaryPressed menuPressed
  let renderState ← Dynamic.zipWithM (fun (h1, h2) (p1, p2) => (h1, h2, p1, p2))
    renderState1 renderState2
  let _ ← dynWidget renderState fun (h1, h2, p1, p2) => do
    let primaryState : WidgetState := { hovered := h1, pressed := p1, focused := false }
    let menuState : WidgetState := { hovered := h2, pressed := p2, focused := false }
    emit do pure (splitButtonVisual primaryName menuName label theme variant primaryState menuState)

  pure { onPrimary, onMenu }

/-- Dropdown button (select-style). -/
def dropdownButton (options : Array String) (initialSelection : Nat := 0)
    : WidgetM DropdownResult :=
  dropdown options initialSelection

/-! ## Loading Button -/

/-- Button that swaps its label for a spinner when loading. -/
def loadingButton (label : String) (isLoading : Reactive.Dynamic Spider Bool)
    (variant : ButtonVariant := .primary) (spinnerSize : Float := 16.0)
    : WidgetM (Reactive.Event Spider Unit) := do
  let theme ← getThemeW
  let name ← registerComponentW "loading-button"
  let isHovered ← buttonHoverState name
  let isPressed ← buttonPressState name
  let onClick ← useClick name
  let elapsedTime ← useElapsedTime

  let renderState1 ← Dynamic.zipWithM (fun hovered loading => (hovered, loading)) isHovered isLoading
  let renderState2 ← Dynamic.zipWithM (fun (hovered, loading) pressed => (hovered, loading, pressed))
    renderState1 isPressed
  let renderState ← Dynamic.zipWithM (fun (hovered, loading, pressed) t => (hovered, loading, pressed, t))
    renderState2 elapsedTime

  let _ ← dynWidget renderState fun (hovered, loading, pressed, t) => do
    let state : WidgetState := {
      hovered
      pressed := pressed
      focused := false
      disabled := loading
    }
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

    emit do
      if loading then
        let spinnerConfig : Spinner.Config := {
          variant := .ring
          color := some fgColor
          dims := { size := spinnerSize }
        }
        pure (namedCenter name (style := style) do
          spinnerVisual (name ++ "-spinner") t spinnerConfig theme)
      else
        pure (namedCenter name (style := style) do
          text' label theme.font fgColor .center)

  let canClick := isLoading.current.map (fun loading => !loading)
  let gatedClick ← Event.gateM canClick onClick
  pure gatedClick

end Afferent.Canopy

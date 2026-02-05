/-
  Canopy Button Widget
  Interactive button with hover/press states and multiple variants.
-/
import Reactive
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Reactive.Component
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

/-- Build the visual for a button with optional overlay layers. -/
def buttonVisualLayered (name : String) (label : String) (icon : Option String)
    (iconPosition : IconPosition) (theme : Theme)
    (variant : ButtonVariant) (state : WidgetState)
    (paddingX paddingY : Float) (cornerRadius : Float)
    (font : FontId := theme.font)
    (minWidth : Option Float := none) (minHeight : Option Float := none)
    (width : Option Float := none) (height : Option Float := none)
    (layers : Array WidgetBuilder := #[]) : WidgetBuilder := do
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

  let layerWidgets ← layers.mapM fun layer => layer
  let contentWidget ← content label icon iconPosition font fgColor
  let wid ← freshId
  let props := Trellis.FlexContainer.centered
  pure (.flex wid (some name) props style (layerWidgets.push contentWidget))

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

/-! ## Animated Button Helpers -/

private def clamp (x lo hi : Float) : Float :=
  if x < lo then lo else if x > hi then hi else x

private def lerp (a b t : Float) : Float :=
  a + (b - a) * t

private def floatMod (x y : Float) : Float :=
  x - y * (x / y).floor

private def pi : Float := 3.14159265

private structure HoverAnim where
  hovered : Bool
  changedAt : Float
deriving BEq, Inhabited

private def rectFromLayout (rect : Trellis.LayoutRect) : Arbor.Rect :=
  Arbor.Rect.mk' rect.x rect.y rect.width rect.height

private def hoverAnimState (name : String) : WidgetM (Reactive.Dynamic Spider HoverAnim) := do
  let isHovered ← buttonHoverState name
  let elapsedTime ← useElapsedTime
  let hoverChanges := isHovered.updated
  let hoverEvents ← Event.attachWithM
    (fun t hovered => { hovered, changedAt := t }) elapsedTime.current hoverChanges
  Reactive.holdDyn { hovered := false, changedAt := 0.0 } hoverEvents

private def hoverProgress (anim : HoverAnim) (t : Float) (duration : Float) : Float :=
  if duration <= 0 then
    if anim.hovered then 1.0 else 0.0
  else
    let dt := t - anim.changedAt
    let pct := clamp (dt / duration) 0.0 1.0
    if anim.hovered then pct else 1.0 - pct

private def layoutForName (data : ClickData) (name : String)
    : Option Trellis.ComputedLayout :=
  match data.nameMap.get? name with
  | some wid => data.layouts.get wid
  | none =>
      match findWidgetIdByName data.widget name with
      | some wid => data.layouts.get wid
      | none => none

private def clickLocalPoint (data : ClickData) (name : String) : Option Arbor.Point := do
  let layout ← layoutForName data name
  let rect := layout.contentRect
  pure (Arbor.Point.mk' (data.click.x - rect.x) (data.click.y - rect.y))

private def overlayWidget (spec : CustomSpec) : WidgetBuilder := do
  custom spec {
    position := .absolute
    top := some 0
    left := some 0
    width := .percent 1.0
    height := .percent 1.0
  }

private structure RippleState where
  center : Arbor.Point
  startTime : Float
deriving BEq, Inhabited

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

  let renderState1 ← Dynamic.zipWithM (fun hovered loading => (hovered, loading)) isHovered isLoading
  let renderState ← Dynamic.zipWithM (fun (hovered, loading) pressed => (hovered, loading, pressed))
    renderState1 isPressed

  let _ ← dynWidget renderState fun (hovered, loading, pressed) => do
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

    let loadingPadding := max (theme.padding * 0.6) (spinnerSize * 0.25)
    let style : BoxStyle := {
      backgroundColor := some bgColor
      borderColor := if bw > 0 then some colors.border else none
      borderWidth := bw
      cornerRadius := theme.cornerRadius
      padding := Trellis.EdgeInsets.symmetric theme.padding loadingPadding
    }

    emit do
      if loading then
        pure (namedCenter name (style := style) do
          text' "..." theme.font fgColor .center)
      else
        pure (namedCenter name (style := style) do
          text' label theme.font fgColor .center)

  let canClick := isLoading.current.map (fun loading => !loading)
  let gatedClick ← Event.gateM canClick onClick
  pure gatedClick

/-! ## Tier 2: Animated Buttons -/

private def rippleOverlaySpec (progress : Float) (center : Arbor.Point)
    (color : Color) : CustomSpec := {
  measure := fun _ _ => (0, 0)
  collect := fun layout =>
    RenderM.build do
      if progress <= 0.0 || progress >= 1.0 then
        pure ()
      else
        let layoutRect := layout.contentRect
        let rect := rectFromLayout layoutRect
        let maxRadius := Float.sqrt (layoutRect.width * layoutRect.width + layoutRect.height * layoutRect.height)
        let radius := maxRadius * progress
        let alpha := (1.0 - progress) * 0.35
        let rippleColor := color.withAlpha (color.a * alpha)
        let absCenter := Arbor.Point.mk' (rect.x + center.x) (rect.y + center.y)
        RenderM.withClip rect do
          RenderM.strokeCircle absCenter radius rippleColor 2.0
  draw := none
  skipCache := true
}

private def pulseOverlaySpec (intensity : Float) (color : Color) : CustomSpec := {
  measure := fun _ _ => (0, 0)
  collect := fun layout =>
    RenderM.build do
      if intensity <= 0.0 then
        pure ()
      else
        let rect := rectFromLayout layout.contentRect
        let overlayColor := color.withAlpha (color.a * intensity)
        RenderM.fillRect rect overlayColor 0
  draw := none
  skipCache := true
}

private def glowOverlaySpec (progress : Float) (color : Color) (cornerRadius : Float) : CustomSpec := {
  measure := fun _ _ => (0, 0)
  collect := fun layout =>
    RenderM.build do
      if progress <= 0.0 then
        pure ()
      else
        let rect := rectFromLayout layout.contentRect
        let glowColor := color.withAlpha (color.a * progress * 0.6)
        RenderM.strokeRect rect glowColor 4.0 cornerRadius
  draw := none
  skipCache := true
}

private def borderTraceSpec (progress : Float) (color : Color) (lineWidth : Float := 2.0)
    : CustomSpec := {
  measure := fun _ _ => (0, 0)
  collect := fun layout =>
    RenderM.build do
      if progress <= 0.0 then
        pure ()
      else
        let layoutRect := layout.contentRect
        let rect := rectFromLayout layoutRect
        let w := layoutRect.width
        let h := layoutRect.height
        let perimeter := (w + h) * 2.0
        let distance := clamp (perimeter * progress) 0.0 perimeter
        let topLen := clamp distance 0.0 w
        let rightLen := clamp (distance - w) 0.0 h
        let bottomLen := clamp (distance - w - h) 0.0 w
        let leftLen := clamp (distance - w - h - w) 0.0 h
        if topLen > 0.0 then
          RenderM.fillRect' rect.x rect.y topLen lineWidth color 0
        if rightLen > 0.0 then
          RenderM.fillRect' (rect.x + w - lineWidth) rect.y lineWidth rightLen color 0
        if bottomLen > 0.0 then
          RenderM.fillRect' (rect.x + w - bottomLen) (rect.y + h - lineWidth)
            bottomLen lineWidth color 0
        if leftLen > 0.0 then
          RenderM.fillRect' rect.x (rect.y + h - leftLen) lineWidth leftLen color 0
  draw := none
  skipCache := true
}

private def shimmerOverlaySpec (phase : Float) (color : Color) : CustomSpec := {
  measure := fun _ _ => (0, 0)
  collect := fun layout =>
    RenderM.build do
      let layoutRect := layout.contentRect
      let rect := rectFromLayout layoutRect
      let bandWidth := layoutRect.width * 0.35
      let travel := layoutRect.width + bandWidth * 2.0
      let offset := phase * travel - bandWidth
      let shimmerColor := color.withAlpha (color.a * 0.25)
      RenderM.withClip rect do
        RenderM.fillRect' (rect.x + offset) rect.y bandWidth rect.height shimmerColor 0
  draw := none
  skipCache := true
}

private def slideRevealSpec (progress : Float) (color : Color) : CustomSpec := {
  measure := fun _ _ => (0, 0)
  collect := fun layout =>
    RenderM.build do
      if progress <= 0.0 then
        pure ()
      else
        let layoutRect := layout.contentRect
        let rect := rectFromLayout layoutRect
        let revealWidth := layoutRect.width * progress
        RenderM.withClip rect do
          RenderM.fillRect' rect.x rect.y revealWidth rect.height color 0
  draw := none
  skipCache := true
}

private def heartbeatOverlaySpec (intensity : Float) (color : Color) : CustomSpec := {
  measure := fun _ _ => (0, 0)
  collect := fun layout =>
    RenderM.build do
      if intensity <= 0.0 then
        pure ()
      else
        let rect := rectFromLayout layout.contentRect
        let overlayColor := color.withAlpha (color.a * intensity)
        RenderM.fillRect rect overlayColor 0
  draw := none
  skipCache := true
}

private def takeChars (s : String) (n : Nat) : String :=
  String.ofList (s.toList.take n)

/-- Ripple button: expanding ink ripple from click point. -/
def rippleButton (label : String) (variant : ButtonVariant := .primary)
    : WidgetM (Reactive.Event Spider Unit) := do
  let theme ← getThemeW
  let name ← registerComponentW "ripple-button"
  let isHovered ← buttonHoverState name
  let isPressed ← buttonPressState name
  let onClick ← useClick name
  let clickData ← useClickData name
  let elapsedTime ← useElapsedTime

  let rippleStart ← Event.attachWithM (fun t data =>
    match clickLocalPoint data name with
    | some center => some ({ center := center, startTime := t } : RippleState)
    | none => none) elapsedTime.current clickData
  let rippleEvent ← Event.mapMaybeM (fun v => v) rippleStart
  let rippleEventSome ← Event.mapM (fun r => some r) rippleEvent
  let rippleState ← Reactive.holdDyn (none : Option RippleState) rippleEventSome

  let renderState1 ← Dynamic.zipWithM (fun hovered pressed => (hovered, pressed)) isHovered isPressed
  let renderState2 ← Dynamic.zipWithM (fun (hovered, pressed) ripple => (hovered, pressed, ripple))
    renderState1 rippleState
  let renderState ← Dynamic.zipWithM (fun (hovered, pressed, ripple) t => (hovered, pressed, ripple, t))
    renderState2 elapsedTime

  let _ ← dynWidget renderState fun (hovered, pressed, ripple, t) => do
    let state : WidgetState := { hovered, pressed, focused := false }
    let colors := Button.variantColors theme variant
    let rippleLayer :=
      match ripple with
      | some r =>
          let progress := (t - r.startTime) / 0.6
          if progress <= 0.0 || progress >= 1.0 then
            #[]
          else
            #[overlayWidget (rippleOverlaySpec progress r.center (colors.foreground.withAlpha 0.9))]
      | none => #[]
    emit do
      pure (Button.buttonVisualLayered name label none .leading theme variant state
        theme.padding (theme.padding * 0.6) theme.cornerRadius
        (layers := rippleLayer))

  pure onClick

/-- Pulse button: gentle breathing highlight. -/
def pulseButton (label : String) (variant : ButtonVariant := .primary)
    : WidgetM (Reactive.Event Spider Unit) := do
  let theme ← getThemeW
  let name ← registerComponentW "pulse-button"
  let isHovered ← buttonHoverState name
  let isPressed ← buttonPressState name
  let onClick ← useClick name
  let elapsedTime ← useElapsedTime

  let renderState1 ← Dynamic.zipWithM (fun hovered pressed => (hovered, pressed)) isHovered isPressed
  let renderState ← Dynamic.zipWithM (fun (hovered, pressed) t => (hovered, pressed, t))
    renderState1 elapsedTime

  let _ ← dynWidget renderState fun (hovered, pressed, t) => do
    let state : WidgetState := { hovered, pressed, focused := false }
    let colors := Button.variantColors theme variant
    let wave := (Float.sin (t * 2.0) + 1.0) * 0.5
    let intensity := 0.12 * wave
    let overlay := #[overlayWidget (pulseOverlaySpec intensity (colors.foreground.withAlpha 0.6))]
    emit do
      pure (Button.buttonVisualLayered name label none .leading theme variant state
        theme.padding (theme.padding * 0.6) theme.cornerRadius
        (layers := overlay))

  pure onClick

/-- Glow-on-hover button: soft outer glow fades in on hover. -/
def glowOnHoverButton (label : String) (variant : ButtonVariant := .primary)
    : WidgetM (Reactive.Event Spider Unit) := do
  let theme ← getThemeW
  let name ← registerComponentW "glow-button"
  let isHovered ← buttonHoverState name
  let isPressed ← buttonPressState name
  let hoverAnim ← hoverAnimState name
  let onClick ← useClick name
  let elapsedTime ← useElapsedTime

  let renderState1 ← Dynamic.zipWithM (fun hovered pressed => (hovered, pressed)) isHovered isPressed
  let renderState2 ← Dynamic.zipWithM (fun (hovered, pressed) anim => (hovered, pressed, anim))
    renderState1 hoverAnim
  let renderState ← Dynamic.zipWithM (fun (hovered, pressed, anim) t => (hovered, pressed, anim, t))
    renderState2 elapsedTime

  let _ ← dynWidget renderState fun (hovered, pressed, anim, t) => do
    let state : WidgetState := { hovered, pressed, focused := false }
    let colors := Button.variantColors theme variant
    let progress := hoverProgress anim t 0.2
    let overlay := if progress <= 0.0 then #[] else
      #[overlayWidget (glowOverlaySpec progress colors.border theme.cornerRadius)]
    emit do
      pure (Button.buttonVisualLayered name label none .leading theme variant state
        theme.padding (theme.padding * 0.6) theme.cornerRadius
        (layers := overlay))

  pure onClick

/-- Border trace button: animated border draws around the perimeter on hover. -/
def borderTraceButton (label : String) (variant : ButtonVariant := .primary)
    : WidgetM (Reactive.Event Spider Unit) := do
  let theme ← getThemeW
  let name ← registerComponentW "border-trace-button"
  let isHovered ← buttonHoverState name
  let isPressed ← buttonPressState name
  let hoverAnim ← hoverAnimState name
  let onClick ← useClick name
  let elapsedTime ← useElapsedTime

  let renderState1 ← Dynamic.zipWithM (fun hovered pressed => (hovered, pressed)) isHovered isPressed
  let renderState2 ← Dynamic.zipWithM (fun (hovered, pressed) anim => (hovered, pressed, anim))
    renderState1 hoverAnim
  let renderState ← Dynamic.zipWithM (fun (hovered, pressed, anim) t => (hovered, pressed, anim, t))
    renderState2 elapsedTime

  let _ ← dynWidget renderState fun (hovered, pressed, anim, t) => do
    let state : WidgetState := { hovered, pressed, focused := false }
    let colors := Button.variantColors theme variant
    let progress := if anim.hovered then
      let phase := floatMod (t - anim.changedAt) 1.2
      clamp (phase / 1.2) 0.0 1.0
    else 0.0
    let overlay := if progress <= 0.0 then #[] else
      #[overlayWidget (borderTraceSpec progress colors.border)]
    emit do
      pure (Button.buttonVisualLayered name label none .leading theme variant state
        theme.padding (theme.padding * 0.6) theme.cornerRadius
        (layers := overlay))

  pure onClick

/-- Shimmer loading button: diagonal highlight sweeps across surface. -/
def shimmerLoadingButton (label : String) (variant : ButtonVariant := .primary)
    : WidgetM (Reactive.Event Spider Unit) := do
  let theme ← getThemeW
  let name ← registerComponentW "shimmer-button"
  let isHovered ← buttonHoverState name
  let isPressed ← buttonPressState name
  let onClick ← useClick name
  let elapsedTime ← useElapsedTime

  let renderState1 ← Dynamic.zipWithM (fun hovered pressed => (hovered, pressed)) isHovered isPressed
  let renderState ← Dynamic.zipWithM (fun (hovered, pressed) t => (hovered, pressed, t))
    renderState1 elapsedTime

  let _ ← dynWidget renderState fun (hovered, pressed, t) => do
    let state : WidgetState := { hovered, pressed, focused := false }
    let colors := Button.variantColors theme variant
    let cycle := 1.4
    let phase := floatMod t cycle / cycle
    let overlay := #[overlayWidget (shimmerOverlaySpec phase (colors.foreground.withAlpha 0.8))]
    emit do
      pure (Button.buttonVisualLayered name label none .leading theme variant state
        theme.padding (theme.padding * 0.6) theme.cornerRadius
        (layers := overlay))

  pure onClick

/-- Bounce button: compress on press, springy overshoot on release. -/
def bounceButton (label : String) (variant : ButtonVariant := .primary)
    : WidgetM (Reactive.Event Spider Unit) := do
  let theme ← getThemeW
  let name ← registerComponentW "bounce-button"
  let isHovered ← buttonHoverState name
  let isPressed ← buttonPressState name
  let onClick ← useClick name
  let elapsedTime ← useElapsedTime

  let pressChanges := isPressed.updated
  let releaseCandidates ← Event.attachWithM
    (fun wasPressed now => if wasPressed && !now then some () else none)
    isPressed.current pressChanges
  let releaseEvent ← Event.mapMaybeM (fun v => v) releaseCandidates
  let releaseTimes ← Event.attachWithM (fun t _ => t) elapsedTime.current releaseEvent
  let lastReleaseTime ← Reactive.holdDyn 0.0 releaseTimes

  let renderState1 ← Dynamic.zipWithM (fun hovered pressed => (hovered, pressed)) isHovered isPressed
  let renderState2 ← Dynamic.zipWithM (fun (hovered, pressed) releaseTime => (hovered, pressed, releaseTime))
    renderState1 lastReleaseTime
  let renderState ← Dynamic.zipWithM
    (fun (hovered, pressed, releaseTime) t => (hovered, pressed, releaseTime, t))
    renderState2 elapsedTime

  let _ ← dynWidget renderState fun (hovered, pressed, releaseTime, t) => do
    let state : WidgetState := { hovered, pressed, focused := false }
    let dt := t - releaseTime
    let bounce :=
      if pressed then 0.94
      else if dt <= 0.5 then
        let decay := Float.exp (-dt * 8.0)
        let oscillation := Float.sin (dt * 16.0)
        clamp (1.0 + 0.08 * oscillation * decay) 0.9 1.1
      else 1.0
    emit do
      pure (Button.buttonVisualLayered name label none .leading theme variant state
        (theme.padding * bounce) (theme.padding * 0.6 * bounce) theme.cornerRadius)

  pure onClick

/-- Jelly button: squish on press, wobbly recovery. -/
def jellyButton (label : String) (variant : ButtonVariant := .primary)
    : WidgetM (Reactive.Event Spider Unit) := do
  let theme ← getThemeW
  let name ← registerComponentW "jelly-button"
  let isHovered ← buttonHoverState name
  let isPressed ← buttonPressState name
  let onClick ← useClick name
  let elapsedTime ← useElapsedTime

  let pressChanges := isPressed.updated
  let releaseCandidates ← Event.attachWithM
    (fun wasPressed now => if wasPressed && !now then some () else none)
    isPressed.current pressChanges
  let releaseEvent ← Event.mapMaybeM (fun v => v) releaseCandidates
  let releaseTimes ← Event.attachWithM (fun t _ => t) elapsedTime.current releaseEvent
  let lastReleaseTime ← Reactive.holdDyn 0.0 releaseTimes

  let renderState1 ← Dynamic.zipWithM (fun hovered pressed => (hovered, pressed)) isHovered isPressed
  let renderState2 ← Dynamic.zipWithM (fun (hovered, pressed) releaseTime => (hovered, pressed, releaseTime))
    renderState1 lastReleaseTime
  let renderState ← Dynamic.zipWithM
    (fun (hovered, pressed, releaseTime) t => (hovered, pressed, releaseTime, t))
    renderState2 elapsedTime

  let _ ← dynWidget renderState fun (hovered, pressed, releaseTime, t) => do
    let state : WidgetState := { hovered, pressed, focused := false }
    let dt := t - releaseTime
    let wobble :=
      if pressed then 0.0
      else if dt <= 0.6 then
        let decay := Float.exp (-dt * 6.0)
        Float.sin (dt * 12.0) * decay
      else 0.0
    let scaleX := if pressed then 1.08 else 1.0 + 0.06 * wobble
    let scaleY := if pressed then 0.92 else 1.0 - 0.06 * wobble
    emit do
      pure (Button.buttonVisualLayered name label none .leading theme variant state
        (theme.padding * scaleX) (theme.padding * 0.6 * scaleY) theme.cornerRadius)

  pure onClick

/-- Typewriter button: label types in on hover with blinking cursor. -/
def typewriterButton (label : String) (variant : ButtonVariant := .primary)
    : WidgetM (Reactive.Event Spider Unit) := do
  let theme ← getThemeW
  let name ← registerComponentW "typewriter-button"
  let isHovered ← buttonHoverState name
  let isPressed ← buttonPressState name
  let hoverAnim ← hoverAnimState name
  let onClick ← useClick name
  let elapsedTime ← useElapsedTime

  let renderState1 ← Dynamic.zipWithM (fun hovered pressed => (hovered, pressed)) isHovered isPressed
  let renderState2 ← Dynamic.zipWithM (fun (hovered, pressed) anim => (hovered, pressed, anim))
    renderState1 hoverAnim
  let renderState ← Dynamic.zipWithM (fun (hovered, pressed, anim) t => (hovered, pressed, anim, t))
    renderState2 elapsedTime

  let _ ← dynWidget renderState fun (hovered, pressed, anim, t) => do
    let state : WidgetState := { hovered, pressed, focused := false }
    let speed := 18.0
    let total := label.length
    let typed :=
      if anim.hovered then
        let dt := max 0.0 (t - anim.changedAt)
        let count := (dt * speed).floor.toUInt32.toNat
        let clipped := min count total
        takeChars label clipped
      else
        label
    let blinkOn :=
      if anim.hovered then
        ((t * 2.0).floor.toUInt32.toNat) % 2 == 0
      else
        false
    let cursor := if blinkOn && typed.length < total then "|" else ""
    let typedLabel := typed ++ cursor
    emit do
      pure (Button.buttonVisualLayered name typedLabel none .leading theme variant state
        theme.padding (theme.padding * 0.6) theme.cornerRadius)

  pure onClick

/-- Slide reveal button: background color wipes in on hover. -/
def slideRevealButton (label : String) (variant : ButtonVariant := .primary)
    : WidgetM (Reactive.Event Spider Unit) := do
  let theme ← getThemeW
  let name ← registerComponentW "slide-reveal-button"
  let isHovered ← buttonHoverState name
  let isPressed ← buttonPressState name
  let hoverAnim ← hoverAnimState name
  let onClick ← useClick name
  let elapsedTime ← useElapsedTime

  let renderState1 ← Dynamic.zipWithM (fun hovered pressed => (hovered, pressed)) isHovered isPressed
  let renderState2 ← Dynamic.zipWithM (fun (hovered, pressed) anim => (hovered, pressed, anim))
    renderState1 hoverAnim
  let renderState ← Dynamic.zipWithM (fun (hovered, pressed, anim) t => (hovered, pressed, anim, t))
    renderState2 elapsedTime

  let _ ← dynWidget renderState fun (hovered, pressed, anim, t) => do
    let state : WidgetState := { hovered, pressed, focused := false }
    let colors := Button.variantColors theme variant
    let progress := hoverProgress anim t 0.18
    let overlay := if progress <= 0.0 then #[] else
      #[overlayWidget (slideRevealSpec progress colors.backgroundHover)]
    emit do
      pure (Button.buttonVisualLayered name label none .leading theme variant state
        theme.padding (theme.padding * 0.6) theme.cornerRadius
        (layers := overlay))

  pure onClick

/-- Heartbeat button: double-pulse rhythm on idle. -/
def heartbeatButton (label : String) (variant : ButtonVariant := .primary)
    : WidgetM (Reactive.Event Spider Unit) := do
  let theme ← getThemeW
  let name ← registerComponentW "heartbeat-button"
  let isHovered ← buttonHoverState name
  let isPressed ← buttonPressState name
  let onClick ← useClick name
  let elapsedTime ← useElapsedTime

  let renderState1 ← Dynamic.zipWithM (fun hovered pressed => (hovered, pressed)) isHovered isPressed
  let renderState ← Dynamic.zipWithM (fun (hovered, pressed) t => (hovered, pressed, t))
    renderState1 elapsedTime

  let _ ← dynWidget renderState fun (hovered, pressed, t) => do
    let state : WidgetState := { hovered, pressed, focused := false }
    let colors := Button.variantColors theme variant
    let cycle := 1.6
    let phase := floatMod t cycle / cycle
    let pulse1 := if phase < 0.18 then Float.sin (phase / 0.18 * pi) else 0.0
    let pulse2 := if phase > 0.3 && phase < 0.46 then
      Float.sin ((phase - 0.3) / 0.16 * pi) else 0.0
    let intensity := clamp (pulse1 * 0.35 + pulse2 * 0.25) 0.0 0.4
    let overlay := #[overlayWidget (heartbeatOverlaySpec intensity (colors.foreground.withAlpha 0.7))]
    emit do
      pure (Button.buttonVisualLayered name label none .leading theme variant state
        theme.padding (theme.padding * 0.6) theme.cornerRadius
        (layers := overlay))

  pure onClick

end Afferent.Canopy

/-
  Canopy Dropdown Widget
  Single-selection dropdown/select widget.
-/
import Reactive
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Reactive.Component

namespace Afferent.Canopy

open Afferent.Arbor hiding Event

/-- Extended state for dropdown widgets. -/
structure DropdownState extends WidgetState where
  selectedIndex : Nat := 0
  isOpen : Bool := false
  hoveredOption : Option Nat := none
deriving Repr, BEq, Inhabited

namespace Dropdown

/-- Dimensions for dropdown rendering. -/
structure Dimensions where
  minWidth : Float := 180.0
  itemHeight : Float := 32.0
  padding : Float := 10.0
  arrowWidth : Float := 20.0
deriving Repr, Inhabited

/-- Default dropdown dimensions. -/
def defaultDimensions : Dimensions := {}

/-- Build chevron points (V shape pointing down or up). -/
def chevronPoints (x y : Float) (isOpen : Bool) : Arbor.Point × Arbor.Point × Arbor.Point :=
  let chevronSize : Float := 6.0
  let halfSize := chevronSize / 2
  let (y1, y2) := if isOpen then
    (y + halfSize * 0.5, y - halfSize * 0.5)  -- Pointing up
  else
    (y - halfSize * 0.5, y + halfSize * 0.5)  -- Pointing down
  let p1 : Arbor.Point := ⟨x - chevronSize, y1⟩
  let p2 : Arbor.Point := ⟨x, y2⟩
  let p3 : Arbor.Point := ⟨x + chevronSize, y1⟩
  (p1, p2, p3)

/-- Custom spec for dropdown arrow (downward chevron). -/
def arrowSpec (isOpen : Bool) (theme : Theme) (dims : Dimensions := defaultDimensions) : CustomSpec := {
  measure := fun _ _ => (dims.arrowWidth, dims.itemHeight)
  collect := fun layout =>
    let rect := layout.contentRect
    let centerX := rect.x + rect.width / 2
    let centerY := rect.y + rect.height / 2
    let (p1, p2, p3) := chevronPoints centerX centerY isOpen
    let c := theme.textMuted
    let data : Array Float := #[
      p1.x, p1.y, p2.x, p2.y, c.r, c.g, c.b, c.a, 0.0,
      p2.x, p2.y, p3.x, p3.y, c.r, c.g, c.b, c.a, 0.0
    ]
    RenderM.build do
      RenderM.strokeLineBatch data 2 2.0
  draw := none
}

/-- Build checkmark points for menu items. -/
def checkmarkPoints (x y : Float) : Arbor.Point × Arbor.Point × Arbor.Point :=
  let p1 : Arbor.Point := ⟨x - 5, y⟩
  let p2 : Arbor.Point := ⟨x - 1, y + 4⟩
  let p3 : Arbor.Point := ⟨x + 6, y - 4⟩
  (p1, p2, p3)

/-- Custom spec for checkmark in menu item. -/
def checkmarkSpec (theme : Theme) : CustomSpec := {
  measure := fun _ _ => (20.0, 20.0)
  collect := fun layout =>
    let rect := layout.contentRect
    let centerX := rect.x + rect.width / 2
    let centerY := rect.y + rect.height / 2
    let (p1, p2, p3) := checkmarkPoints centerX centerY
    let c := theme.primary.foreground
    let data : Array Float := #[
      p1.x, p1.y, p2.x, p2.y, c.r, c.g, c.b, c.a, 0.0,
      p2.x, p2.y, p3.x, p3.y, c.r, c.g, c.b, c.a, 0.0
    ]
    RenderM.build do
      RenderM.strokeLineBatch data 2 2.0
  draw := none
}

end Dropdown

/-- Build a visual dropdown trigger button.
    - `name`: Widget name for hit testing (should be the trigger name)
    - `selectedText`: Text to display (the selected option)
    - `isOpen`: Whether dropdown is currently open
    - `theme`: Theme for styling
    - `state`: Widget interaction state (hover, focus, etc.)
-/
def dropdownTriggerVisual (name : String) (selectedText : String) (isOpen : Bool)
    (theme : Theme) (state : WidgetState := {}) : WidgetBuilder := do
  let dims := Dropdown.defaultDimensions
  let bgColor := if state.hovered || isOpen then theme.input.backgroundHover else theme.input.background
  let borderColor := if state.focused then theme.input.borderFocused else theme.input.border

  let triggerStyle : BoxStyle := {
    backgroundColor := some bgColor
    borderColor := some borderColor
    borderWidth := 1
    cornerRadius := theme.cornerRadius
    padding := Trellis.EdgeInsets.symmetric dims.padding (dims.padding * 0.6)
    minWidth := some dims.minWidth
    minHeight := some dims.itemHeight
  }

  let wid ← freshId
  let props : Trellis.FlexContainer := {
    Trellis.FlexContainer.row 0 with
    alignItems := .center
    justifyContent := .spaceBetween
  }

  let textWidget ← text' selectedText theme.font theme.text .left
  let arrowWidget ← custom (Dropdown.arrowSpec isOpen theme dims) {
    minWidth := some dims.arrowWidth
    minHeight := some dims.itemHeight
  }

  pure (.flex wid (some name) props triggerStyle #[textWidget, arrowWidget])

/-- Build a visual dropdown menu item.
    - `name`: Widget name for hit testing (should be option-specific)
    - `optionText`: Text to display
    - `isSelected`: Whether this option is currently selected
    - `isHovered`: Whether this option is being hovered
    - `isFirst`: Whether this is the first item (for rounded corners)
    - `isLast`: Whether this is the last item (for rounded corners)
    - `theme`: Theme for styling
-/
def dropdownMenuItemVisual (name : String) (optionText : String)
    (isSelected : Bool) (isHovered : Bool) (isFirst : Bool) (isLast : Bool)
    (theme : Theme) : WidgetBuilder := do
  let dims := Dropdown.defaultDimensions
  let bgColor := if isHovered then theme.input.backgroundHover
    else if isSelected then theme.primary.background.withAlpha 0.15
    else theme.input.background
  let textColor := if isSelected then theme.primary.foreground else theme.text

  -- Calculate corner radius for first/last items
  let cornerRadius := if isFirst && isLast then theme.cornerRadius
    else if isFirst then 0  -- Actually want top corners only, but we will simplify
    else if isLast then 0   -- Actually want bottom corners only
    else 0

  let itemStyle : BoxStyle := {
    backgroundColor := some bgColor
    cornerRadius := cornerRadius
    padding := Trellis.EdgeInsets.symmetric dims.padding (dims.padding * 0.5)
    minWidth := some dims.minWidth
    minHeight := some dims.itemHeight
  }

  let wid ← freshId
  let props : Trellis.FlexContainer := {
    Trellis.FlexContainer.row 8 with
    alignItems := .center
  }

  let textWidget ← text' optionText theme.font textColor .left

  if isSelected then
    let checkWidget ← custom (Dropdown.checkmarkSpec theme) {
      minWidth := some 20
      minHeight := some 20
    }
    pure (.flex wid (some name) props itemStyle #[checkWidget, textWidget])
  else
    -- Add spacer for alignment when no checkmark
    let spacerWidget ← spacer 20 0
    pure (.flex wid (some name) props itemStyle #[spacerWidget, textWidget])

/-- Build a complete visual dropdown widget.
    - `name`: Base widget name for the dropdown
    - `triggerName`: Widget name for the trigger button
    - `optionNameFn`: Function to generate option widget names from index
    - `options`: Array of option strings
    - `selectedIndex`: Currently selected option index
    - `isOpen`: Whether dropdown menu is open
    - `hoveredOption`: Currently hovered option index (if any)
    - `theme`: Theme for styling
    - `state`: Widget interaction state for trigger
-/
def dropdownVisual (name : String) (triggerName : String)
    (optionNameFn : Nat → String) (options : Array String) (selectedIndex : Nat)
    (isOpen : Bool) (hoveredOption : Option Nat) (theme : Theme)
    (state : WidgetState := {}) : WidgetBuilder := do
  let dims := Dropdown.defaultDimensions
  let selectedText := options.getD selectedIndex "Select..."

  let trigger ← dropdownTriggerVisual triggerName selectedText isOpen theme state

  if isOpen then
    -- Build menu items
    let mut menuItems : Array Widget := #[]
    for i in [:options.size] do
      let optText := options.getD i ""
      let isSel := i == selectedIndex
      let isHov := hoveredOption == some i
      let isFirst := i == 0
      let isLast := i == options.size - 1
      let itemWidget ← dropdownMenuItemVisual (optionNameFn i) optText isSel isHov isFirst isLast theme
      menuItems := menuItems.push itemWidget

    -- Menu container with border
    let menuOffset := dims.itemHeight + 4
    let menuHeight := dims.itemHeight * options.size.toFloat
    let menuStyle : BoxStyle := {
      backgroundColor := some theme.input.background
      borderColor := some theme.input.border
      borderWidth := 1
      cornerRadius := theme.cornerRadius
      width := .percent 1.0
      height := .length menuHeight
      position := .absolute
      layer := .overlay
      top := some menuOffset
      left := some 0
      -- No padding - items handle their own padding
    }

    let menuWid ← freshId
    let menuProps : Trellis.FlexContainer := {
      direction := .column
      gap := 0
    }
    let menu : Widget := .flex menuWid none menuProps menuStyle menuItems

    -- Outer container (column with trigger + menu)
    let outerWid ← freshId
    let outerProps : Trellis.FlexContainer := {
      direction := .column
      gap := 0
    }
    pure (.flex outerWid (some name) outerProps {} #[trigger, menu])
  else
    -- Just the trigger when closed
    let outerWid ← freshId
    let outerProps : Trellis.FlexContainer := {
      direction := .column
      gap := 0
    }
    pure (.flex outerWid (some name) outerProps {} #[trigger])

/-! ## Reactive Dropdown Components (FRP-based)

These use WidgetM for declarative composition with automatic open/close and selection.
-/

open Reactive Reactive.Host
open Afferent.Canopy.Reactive

/-- Dropdown result - events and dynamics. -/
structure DropdownResult where
  onSelect : Reactive.Event Spider Nat
  selection : Reactive.Dynamic Spider Nat
  isOpen : Reactive.Dynamic Spider Bool

/-- Create a reactive dropdown component using WidgetM.
    Emits the dropdown widget and returns selection state.
    - `options`: Array of option strings
    - `initialSelection`: Initial selected index
-/
def dropdown (options : Array String) (initialSelection : Nat := 0)
    : WidgetM DropdownResult := do
  let theme ← getThemeW
  let containerName ← registerComponentW "dropdown" (isInteractive := false)
  let triggerName ← registerComponentW "dropdown-trigger"

  let mut optionNames : Array String := #[]
  for _ in options do
    let name ← registerComponentW "dropdown-option"
    optionNames := optionNames.push name
  let optionNameFn (i : Nat) : String := optionNames.getD i ""

  let events ← getEventsW
  let isTriggerHovered ← useHover triggerName
  let triggerClicks ← useClick triggerName
  let allClicks ← useAllClicks

  let findClickedOption (data : ClickData) : Option Nat :=
    (List.range options.size).findSome? fun i =>
      if hitWidget data (optionNameFn i) then some i else none

  let isClickOutside (data : ClickData) : Bool :=
    !hitWidget data containerName && !hitWidget data triggerName

  let optionClicks ← Event.mapMaybeM findClickedOption allClicks
  let selection ← Reactive.holdDyn initialSelection optionClicks
  let onSelect := optionClicks

  let isOpen ← SpiderM.fixDynM fun isOpenBehavior => do
    let toggleEvents ← Event.mapM (fun _ => fun open_ => !open_) triggerClicks
    let closeOnOption ← Event.mapM (fun _ => fun _ => false) optionClicks
    let outsideClicks ← Event.filterM isClickOutside allClicks
    let gatedOutside ← Event.gateM isOpenBehavior outsideClicks
    let closeOnOutside ← Event.mapM (fun _ => fun _ => false) gatedOutside
    let allTransitions ← Event.leftmostM [closeOnOption, closeOnOutside, toggleEvents]
    Reactive.foldDyn (fun f s => f s) false allTransitions

  let mut optionHoverEvents : Array (Reactive.Event Spider (Option Nat)) := #[]
  for i in [:options.size] do
    let hoverChanges ← Event.selectM events.hoverFan (optionNameFn i)
    let enter ← Event.mapMaybeM (fun hovered => if hovered then some (some i) else none) hoverChanges
    let leave ← Event.mapMaybeM (fun hovered => if hovered then some (none : Option Nat) else none) hoverChanges
    optionHoverEvents := optionHoverEvents.push enter
    optionHoverEvents := optionHoverEvents.push leave
  let ctx ← SpiderM.getTimelineCtx
  let neverHover ← SpiderM.liftIO (Reactive.Event.never ctx)
  let hoverEvents ← match optionHoverEvents.toList with
    | [] => pure neverHover
    | events => Event.leftmostM events
  let gatedHover ← Event.gateM isOpen.current hoverEvents
  let closeEvents ← Event.filterM (fun open_ => !open_) isOpen.updated
  let resetHover ← Event.mapM (fun _ => (none : Option Nat)) closeEvents
  let mergedHover ← Event.mergeM gatedHover resetHover
  let hoveredOption ← Reactive.holdDyn none mergedHover

  -- Use dynWidget for efficient change-driven rebuilds
  let renderState1 ← Dynamic.zipWithM (fun h o => (h, o)) isTriggerHovered isOpen
  let renderState2 ← Dynamic.zipWithM (fun (h, o) s => (h, o, s)) renderState1 selection
  let renderState3 ← Dynamic.zipWithM (fun (h, o, s) hOpt => (h, o, s, hOpt)) renderState2 hoveredOption
  let _ ← dynWidget renderState3 fun (triggerHovered, open_, sel, hoverOpt) => do
    let triggerState : WidgetState := { hovered := triggerHovered, pressed := false, focused := false }
    emit do pure (dropdownVisual containerName triggerName optionNameFn options sel open_ hoverOpt theme triggerState)

  pure { onSelect, selection, isOpen }

end Afferent.Canopy

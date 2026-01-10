/-
  Canopy Menu Widget
  Displays a list of actionable items in a popup overlay.
-/
import Reactive
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Reactive.Component

namespace Afferent.Canopy

open Afferent.Arbor hiding Event
open Reactive Reactive.Host
open Afferent.Canopy.Reactive

/-- A menu item can be an action or separator. -/
inductive MenuItem where
  | action (label : String) (enabled : Bool := true)
  | separator
deriving Repr, Inhabited

/-- Configuration for menu appearance. -/
structure MenuConfig where
  minWidth : Float := 180.0
  itemHeight : Float := 32.0
  separatorHeight : Float := 9.0
  cornerRadius : Float := 4.0
deriving Repr, Inhabited

/-- Result from menu widget. -/
structure MenuResult where
  /-- Fires when an item is selected (raw index into items array). -/
  onSelect : Reactive.Event Spider Nat
  /-- Whether the menu is currently open. -/
  isOpen : Reactive.Dynamic Spider Bool

namespace Menu

/-- Default menu configuration. -/
def defaultConfig : MenuConfig := {}

/-- Calculate total menu height based on items. -/
def calculateHeight (items : Array MenuItem) (config : MenuConfig := defaultConfig) : Float :=
  items.foldl (fun acc item =>
    match item with
    | .separator => acc + config.separatorHeight
    | .action .. => acc + config.itemHeight
  ) 0.0

/-- Check if a menu item at given index is an enabled action. -/
def isEnabledAction (items : Array MenuItem) (index : Nat) : Bool :=
  match items.getD index .separator with
  | .action _ enabled => enabled
  | .separator => false

end Menu

/-- Build a visual menu item.
    - `name`: Widget name for hit testing
    - `item`: The menu item to render
    - `isHovered`: Whether this item is being hovered
    - `theme`: Theme for styling
    - `config`: Menu configuration
-/
def menuItemVisual (name : String) (item : MenuItem) (isHovered : Bool)
    (theme : Theme) (config : MenuConfig := Menu.defaultConfig) : WidgetBuilder := do
  match item with
  | .separator =>
    -- Horizontal line separator
    let containerStyle : BoxStyle := {
      height := .length config.separatorHeight
      padding := Trellis.EdgeInsets.symmetric 8 4
      width := .percent 1.0
    }
    let lineStyle : BoxStyle := {
      backgroundColor := some (theme.input.border.withAlpha 0.5)
      height := .length 1
      width := .percent 1.0
    }
    let wid ← freshId
    let lineWid ← freshId
    let lineWidget : Widget := .rect lineWid none lineStyle
    let props : Trellis.FlexContainer := { direction := .column, gap := 0 }
    pure (.flex wid (some name) props containerStyle #[lineWidget])
  | .action label enabled =>
    let bgColor := if !enabled then theme.input.background
      else if isHovered then theme.input.backgroundHover
      else theme.input.background
    let textColor := if enabled then theme.text else theme.textMuted
    let itemStyle : BoxStyle := {
      backgroundColor := some bgColor
      padding := Trellis.EdgeInsets.symmetric 12 8
      minHeight := some config.itemHeight
      width := .percent 1.0
    }
    let wid ← freshId
    let props : Trellis.FlexContainer := {
      Trellis.FlexContainer.row 0 with
      alignItems := .center
    }
    let textWidget ← text' label theme.font textColor .left
    pure (.flex wid (some name) props itemStyle #[textWidget])

/-- Build a complete visual menu widget.
    - `containerName`: Widget name for the menu container
    - `triggerName`: Widget name for the trigger
    - `itemNameFn`: Function to get item name by index
    - `items`: Array of menu items
    - `isOpen`: Whether the menu is open
    - `hoveredIndex`: Currently hovered item index (if any)
    - `theme`: Theme for styling
    - `config`: Menu configuration
    - `triggerHeight`: Height of the trigger widget for positioning
    - `triggerBuilders`: Trigger widget builders to compose
-/
def menuVisual (containerName : String) (triggerName : String)
    (itemNameFn : Nat → String) (items : Array MenuItem) (isOpen : Bool)
    (hoveredIndex : Option Nat) (theme : Theme) (config : MenuConfig := Menu.defaultConfig)
    (triggerHeight : Float := 32.0) (triggerBuilders : Array WidgetBuilder) : WidgetBuilder := do
  -- Build trigger widgets within our builder context (preserves ID state)
  let mut triggerWidgets : Array Widget := #[]
  for builder in triggerBuilders do
    let widget ← builder
    triggerWidgets := triggerWidgets.push widget

  -- Build trigger container
  let triggerWid ← freshId
  let triggerProps : Trellis.FlexContainer := { direction := .column, gap := 0 }
  let trigger : Widget := .flex triggerWid (some triggerName) triggerProps {} triggerWidgets

  if isOpen then
    -- Build menu items
    let mut menuItems : Array Widget := #[]
    for i in [:items.size] do
      let item := items.getD i .separator
      let isHov := hoveredIndex == some i
      let itemWidget ← menuItemVisual (itemNameFn i) item isHov theme config
      menuItems := menuItems.push itemWidget

    -- Calculate total height
    let totalHeight := Menu.calculateHeight items config

    let menuStyle : BoxStyle := {
      backgroundColor := some theme.input.background
      borderColor := some theme.input.border
      borderWidth := 1
      cornerRadius := config.cornerRadius
      minWidth := some config.minWidth
      height := .length totalHeight
      position := .absolute
      top := some (triggerHeight + 4)
      left := some 0
    }

    let menuWid ← freshId
    let menuProps : Trellis.FlexContainer := { direction := .column, gap := 0 }
    let menu : Widget := .flex menuWid (some containerName) menuProps menuStyle menuItems

    -- Outer container with trigger + menu
    let outerWid ← freshId
    let outerProps : Trellis.FlexContainer := { direction := .column, gap := 0 }
    pure (.flex outerWid none outerProps {} #[trigger, menu])
  else
    -- Just the trigger when closed
    let outerWid ← freshId
    let outerProps : Trellis.FlexContainer := { direction := .column, gap := 0 }
    pure (.flex outerWid none outerProps {} #[trigger])

/-- Create a reactive menu component using WidgetM.
    The menu appears when clicking the trigger widget.
    - `items`: Array of menu items
    - `theme`: Theme for styling
    - `config`: Menu configuration
    - `trigger`: The widget(s) that trigger the menu on click
-/
def menu (items : Array MenuItem) (theme : Theme)
    (config : MenuConfig := Menu.defaultConfig)
    (trigger : WidgetM α) : WidgetM (α × MenuResult) := do
  let containerName ← registerComponentW "menu" (isInteractive := false)
  let triggerName ← registerComponentW "menu-trigger"

  -- Register names for each item
  let mut itemNames : Array String := #[]
  for _ in items do
    let name ← registerComponentW "menu-item"
    itemNames := itemNames.push name
  let itemNameFn (i : Nat) : String := itemNames.getD i ""

  -- Run trigger widget to get its renders
  let (triggerResult, triggerRenders) ← runWidgetChildren trigger

  -- Store trigger dimensions from hover data
  let triggerDimsRef ← SpiderM.liftIO (IO.mkRef (120.0, 32.0))

  -- Hooks
  let triggerClicks ← useClick triggerName
  let allClicks ← useAllClicks
  let allHovers ← useAllHovers
  let keyEvents ← useKeyboard

  -- Update trigger dimensions when hovering
  let _ ← performEvent_ (← Event.mapM (fun data => do
    if hitWidgetHover data triggerName then
      match findWidgetIdByName data.widget triggerName with
      | some widgetId =>
        match data.layouts.get widgetId with
        | some layout =>
          triggerDimsRef.set (layout.contentRect.width, layout.contentRect.height)
        | none => pure ()
      | none => pure ()
  ) allHovers)

  -- Find clicked enabled action item (returns raw index)
  let findClickedAction (data : ClickData) : Option Nat :=
    (List.range items.size).findSome? fun i =>
      if hitWidget data (itemNameFn i) && Menu.isEnabledAction items i then some i else none

  -- Click-outside detection
  let isClickOutside (data : ClickData) : Bool :=
    !hitWidget data containerName && !hitWidget data triggerName &&
    -- Also not clicking on any menu item
    (List.range items.size).all fun i => !hitWidget data (itemNameFn i)

  -- Open/close state machine
  let isOpen ← SpiderM.fixDynM fun isOpenBehavior => do
    -- Toggle on trigger click
    let toggleEvents ← Event.mapM (fun _ => fun open_ => !open_) triggerClicks

    -- Close on enabled item click
    let itemClicks ← Event.mapMaybeM findClickedAction allClicks
    let closeOnItem ← Event.mapM (fun _ => fun _ => false) itemClicks

    -- Close on outside click (gated by open state)
    let outsideClicks ← Event.filterM isClickOutside allClicks
    let gatedOutside ← Event.gateM isOpenBehavior outsideClicks
    let closeOnOutside ← Event.mapM (fun _ => fun _ => false) gatedOutside

    -- Close on Escape key
    let escapeKeys ← Event.filterM (fun k => k.event.key == .escape && k.event.isPress) keyEvents
    let gatedEscape ← Event.gateM isOpenBehavior escapeKeys
    let closeOnEscape ← Event.mapM (fun _ => fun _ => false) gatedEscape

    let allTransitions ← Event.leftmostM [closeOnItem, closeOnOutside, closeOnEscape, toggleEvents]
    Reactive.foldDyn (fun f s => f s) false allTransitions

  -- Track hovered item (only when menu is open)
  let findHoveredItem (data : HoverData) : Option Nat :=
    (List.range items.size).findSome? fun i =>
      if hitWidgetHover data (itemNameFn i) then some i else none

  let hoverChanges ← Event.mapM findHoveredItem allHovers
  let gatedHover ← Event.gateM isOpen.current hoverChanges
  let closeEvents ← Event.filterM (fun open_ => !open_) isOpen.updated
  let resetHover ← Event.mapM (fun _ => (none : Option Nat)) closeEvents
  let mergedHover ← Event.mergeM gatedHover resetHover
  let hoveredIndex ← Reactive.holdDyn none mergedHover

  -- Selection event (only fires for enabled action items)
  let onSelect ← Event.mapMaybeM findClickedAction allClicks

  emit do
    let open_ ← isOpen.sample
    let hoverIdx ← hoveredIndex.sample
    let (_, triggerHeight) ← triggerDimsRef.get
    -- Get trigger widget builders (ComponentRender = IO WidgetBuilder)
    let triggerBuilders ← triggerRenders.mapM id
    pure (menuVisual containerName triggerName itemNameFn items open_ hoverIdx theme config triggerHeight triggerBuilders)

  pure (triggerResult, { onSelect, isOpen })

end Afferent.Canopy

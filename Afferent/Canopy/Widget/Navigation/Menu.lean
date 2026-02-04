/-
  Canopy Menu Widget
  Displays a list of actionable items in a popup overlay.
-/
import Std.Data.HashMap
import Reactive
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Reactive.Component

namespace Afferent.Canopy

open Afferent.Arbor hiding Event
open Reactive Reactive.Host
open Afferent.Canopy.Reactive

/-- A menu item can be an action, separator, or submenu. -/
inductive MenuItem where
  | action (label : String) (enabled : Bool := true)
  | separator
  | submenu (label : String) (items : Array MenuItem) (enabled : Bool := true)
deriving Repr, Inhabited

/-- Path to a menu item (indices at each level). -/
abbrev MenuPath := Array Nat

/-- Configuration for menu appearance. -/
structure MenuConfig where
  minWidth : Float := 180.0
  itemHeight : Float := 32.0
  separatorHeight : Float := 9.0
  cornerRadius : Float := 4.0
deriving Repr, Inhabited

/-- Result from menu widget. -/
structure MenuResult where
  /-- Fires when an action item is selected (path to item). -/
  onSelect : Reactive.Event Spider MenuPath
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
    | .submenu .. => acc + config.itemHeight
  ) 0.0

/-- Check if a menu item at given index is an enabled action. -/
def isEnabledAction (items : Array MenuItem) (index : Nat) : Bool :=
  match items.getD index .separator with
  | .action _ enabled => enabled
  | .separator => false
  | .submenu .. => false

/-- Get the item at a given path. -/
partial def getItemAtPath (items : Array MenuItem) (path : MenuPath) : Option MenuItem :=
  match path.toList with
  | [] => none
  | [i] => items[i]?
  | i :: rest =>
    match items[i]? with
    | some (.submenu _ subItems _) => getItemAtPath subItems rest.toArray
    | _ => none

/-- Check if item at path is an enabled action. -/
def isEnabledActionAtPath (items : Array MenuItem) (path : MenuPath) : Bool :=
  match getItemAtPath items path with
  | some (.action _ enabled) => enabled
  | _ => false

/-- Check if item at path is an enabled submenu. -/
def isEnabledSubmenuAtPath (items : Array MenuItem) (path : MenuPath) : Bool :=
  match getItemAtPath items path with
  | some (.submenu _ _ enabled) => enabled
  | _ => false

/-- Get items at a given parent path (empty path = root items). -/
partial def getItemsAtPath (items : Array MenuItem) (path : MenuPath) : Array MenuItem :=
  match path.toList with
  | [] => items
  | i :: rest =>
    match items[i]? with
    | some (.submenu _ subItems _) => getItemsAtPath subItems rest.toArray
    | _ => #[]

/-- Calculate height for items at a path. -/
def calculateHeightAtPath (items : Array MenuItem) (path : MenuPath)
    (config : MenuConfig := defaultConfig) : Float :=
  calculateHeight (getItemsAtPath items path) config

/-- Count total items recursively (for name registration). -/
partial def countAllItems (items : Array MenuItem) : Nat :=
  items.foldl (fun acc item =>
    match item with
    | .submenu _ subItems _ => acc + 1 + countAllItems subItems
    | _ => acc + 1
  ) 0

/-- Check if a path is a prefix of another path. -/
def isPathPrefix (prefix_ : MenuPath) (path : MenuPath) : Bool :=
  if prefix_.size > path.size then false
  else (List.range prefix_.size).all fun i =>
    prefix_.getD i 0 == path.getD i 0

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
  | .submenu label _ enabled =>
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
      justifyContent := .spaceBetween
    }
    let textWidget ← text' label theme.font textColor .left
    let arrowWidget ← text' "›" theme.font textColor .right
    pure (.flex wid (some name) props itemStyle #[textWidget, arrowWidget])

/-- Build a submenu popup at a given path.
    - `containerNameFn`: Function to get container name by path
    - `itemNameFn`: Function to get item name by path
    - `items`: Root menu items
    - `path`: Path to this submenu (empty = root menu)
    - `openSubmenuPath`: Which submenus are open
    - `hoveredPath`: Currently hovered item path
    - `theme`: Theme for styling
    - `config`: Menu configuration
    - `offsetX`: X offset for positioning (accumulated from parent menus)
    - `offsetY`: Y offset for positioning
-/
partial def submenuVisual (containerNameFn : MenuPath → String) (itemNameFn : MenuPath → String)
    (items : Array MenuItem) (path : MenuPath) (openSubmenuPath : MenuPath)
    (hoveredPath : Option MenuPath) (theme : Theme) (config : MenuConfig := Menu.defaultConfig)
    (offsetX : Float := 0) (offsetY : Float := 0) : WidgetBuilder := do
  let subItems := Menu.getItemsAtPath items path
  let totalHeight := Menu.calculateHeight subItems config

  -- Build menu items at this level
  let mut menuWidgets : Array Widget := #[]
  for i in [:subItems.size] do
    let itemPath := path.push i
    let item := subItems.getD i .separator
    let isHov := hoveredPath == some itemPath
    let itemWidget ← menuItemVisual (itemNameFn itemPath) item isHov theme config
    menuWidgets := menuWidgets.push itemWidget

  let menuStyle : BoxStyle := {
    backgroundColor := some theme.input.background
    borderColor := some theme.input.border
    borderWidth := 1
    cornerRadius := config.cornerRadius
    minWidth := some config.minWidth
    height := .length totalHeight
    position := .absolute
    top := some offsetY
    left := some offsetX
  }

  let menuWid ← freshId
  let menuProps : Trellis.FlexContainer := { direction := .column, gap := 0 }
  let menuWidget : Widget := .flex menuWid (some (containerNameFn path)) menuProps menuStyle menuWidgets

  -- Check if we need to render any open submenus at this level
  -- A submenu at index i (path ++ [i]) is open if openSubmenuPath starts with path ++ [i]
  let mut allMenus : Array Widget := #[menuWidget]
  for i in [:subItems.size] do
    let itemPath := path.push i
    -- Check if this item's submenu is open
    if Menu.isPathPrefix itemPath openSubmenuPath && Menu.isEnabledSubmenuAtPath items itemPath then
      -- Calculate position for nested submenu
      let itemY := (List.range i).foldl (fun acc j =>
        let item := subItems.getD j .separator
        match item with
        | .separator => acc + config.separatorHeight
        | _ => acc + config.itemHeight
      ) 0.0
      let nestedSubmenu ← submenuVisual containerNameFn itemNameFn items itemPath
        openSubmenuPath hoveredPath theme config
        (offsetX + config.minWidth + 2) (offsetY + itemY)
      allMenus := allMenus.push nestedSubmenu

  -- If multiple menus, wrap in a container with same size as root menu
  -- The wrapper is in normal flow, and the menus inside are absolute
  if allMenus.size == 1 then
    pure menuWidget
  else
    -- Wrapper takes the space of the root menu so layout doesn't shift
    let wrapperStyle : BoxStyle := {
      minWidth := some config.minWidth
      height := .length totalHeight
    }
    let wrapperWid ← freshId
    let wrapperProps : Trellis.FlexContainer := { direction := .column, gap := 0 }
    pure (.flex wrapperWid none wrapperProps wrapperStyle allMenus)

/-- Build a complete visual menu widget with submenu support.
    - `containerNameFn`: Function to get container name by path
    - `triggerName`: Widget name for the trigger
    - `itemNameFn`: Function to get item name by path
    - `items`: Array of menu items
    - `isOpen`: Whether the menu is open
    - `openSubmenuPath`: Path to currently open submenu
    - `hoveredPath`: Currently hovered item path
    - `theme`: Theme for styling
    - `config`: Menu configuration
    - `triggerHeight`: Height of the trigger widget for positioning
    - `triggerBuilders`: Trigger widget builders to compose
-/
def menuVisual (containerNameFn : MenuPath → String) (triggerName : String)
    (itemNameFn : MenuPath → String) (items : Array MenuItem) (isOpen : Bool)
    (openSubmenuPath : MenuPath) (hoveredPath : Option MenuPath) (theme : Theme)
    (config : MenuConfig := Menu.defaultConfig) (triggerHeight : Float := 32.0)
    (triggerBuilders : Array WidgetBuilder) : WidgetBuilder := do
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
    -- Build the root menu (and any open submenus)
    -- Root menu is at (0, 0) relative to its container - the container handles positioning
    let menuTree ← submenuVisual containerNameFn itemNameFn items #[] openSubmenuPath
      hoveredPath theme config 0 0

    -- Menu container - positioned below trigger with position:relative for absolute children
    let menuContainerStyle : BoxStyle := {
      position := .absolute
      layer := .overlay
      top := some (triggerHeight + 4)
      left := some 0
    }
    let menuContainerWid ← freshId
    let menuContainerProps : Trellis.FlexContainer := { direction := .column, gap := 0 }
    let menuContainer := Widget.flex menuContainerWid none menuContainerProps menuContainerStyle #[menuTree]

    -- Outer container with trigger + positioned menu container
    let outerWid ← freshId
    let outerProps : Trellis.FlexContainer := { direction := .column, gap := 0 }
    pure (.flex outerWid none outerProps {} #[trigger, menuContainer])
  else
    -- Just the trigger when closed
    let outerWid ← freshId
    let outerProps : Trellis.FlexContainer := { direction := .column, gap := 0 }
    pure (.flex outerWid none outerProps {} #[trigger])

/-- Register names for all menu items recursively. Returns a map from path to name. -/
partial def registerMenuItemNames (items : Array MenuItem) (parentPath : MenuPath := #[])
    : WidgetM (Std.HashMap MenuPath String) := do
  let mut names : Std.HashMap MenuPath String := {}
  for i in [:items.size] do
    let path := parentPath.push i
    let name ← registerComponentW "menu-item"
    names := names.insert path name
    match items.getD i .separator with
    | .submenu _ subItems _ =>
      let subNames ← registerMenuItemNames subItems path
      for (subPath, subName) in subNames.toList do
        names := names.insert subPath subName
    | _ => pure ()
  pure names

/-- Register container names for all submenus recursively. -/
partial def registerSubmenuContainerNames (items : Array MenuItem) (parentPath : MenuPath := #[])
    : WidgetM (Std.HashMap MenuPath String) := do
  let mut names : Std.HashMap MenuPath String := {}
  -- Root container
  let rootName ← registerComponentW "menu" (isInteractive := false)
  names := names.insert parentPath rootName
  for i in [:items.size] do
    let path := parentPath.push i
    match items.getD i .separator with
    | .submenu _ subItems _ =>
      let subName ← registerComponentW "submenu" (isInteractive := false)
      names := names.insert path subName
      let subNames ← registerSubmenuContainerNames subItems path
      for (subPath, subSubName) in subNames.toList do
        names := names.insert subPath subSubName
    | _ => pure ()
  pure names

/-- Collect all item paths in a menu tree. -/
partial def collectAllPaths (items : Array MenuItem) (parentPath : MenuPath := #[]) : List MenuPath :=
  (List.range items.size).foldl (fun acc i =>
    let path := parentPath.push i
    let withPath := path :: acc
    match items.getD i .separator with
    | .submenu _ subItems _ => collectAllPaths subItems path ++ withPath
    | _ => withPath
  ) []

/-- Create a reactive menu component using WidgetM.
    The menu appears when clicking the trigger widget.
    - `items`: Array of menu items (can include submenus)
    - `config`: Menu configuration
    - `trigger`: The widget(s) that trigger the menu on click
-/
def menu (items : Array MenuItem)
    (config : MenuConfig := Menu.defaultConfig)
    (trigger : WidgetM α) : WidgetM (α × MenuResult) := do
  let theme ← getThemeW
  let triggerName ← registerComponentW "menu-trigger"

  -- Register names for all items and containers recursively
  let itemNames ← registerMenuItemNames items
  let containerNames ← registerSubmenuContainerNames items
  let itemNameFn (path : MenuPath) : String := itemNames.getD path ""
  let containerNameFn (path : MenuPath) : String := containerNames.getD path ""

  -- All paths for hit testing
  let allPaths := collectAllPaths items

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

  -- Find clicked enabled action item (returns path)
  let findClickedAction (data : ClickData) : Option MenuPath :=
    allPaths.findSome? fun path =>
      if hitWidget data (itemNameFn path) && Menu.isEnabledActionAtPath items path then some path else none

  -- Check if click is on any menu container or item
  let isClickInMenu (data : ClickData) : Bool :=
    -- Check all containers
    containerNames.toList.any (fun (_, name) => hitWidget data name) ||
    -- Check all items
    allPaths.any fun path => hitWidget data (itemNameFn path)

  -- Click-outside detection
  let isClickOutside (data : ClickData) : Bool :=
    !hitWidget data triggerName && !isClickInMenu data

  -- Open/close state machine
  let isOpen ← SpiderM.fixDynM fun isOpenBehavior => do
    -- Toggle on trigger click
    let toggleEvents ← Event.mapM (fun _ => fun open_ => !open_) triggerClicks

    -- Close on enabled action item click
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

  -- Track hovered path (only when menu is open)
  let hoverTargets := allPaths.toArray.map (fun path => (itemNameFn path, path))
  let hoverPathChanges ← StateT.lift (hoverEventForTargets hoverTargets)
  let gatedHoverPath ← Event.gateM isOpen.current hoverPathChanges
  let closeEvents ← Event.filterM (fun open_ => !open_) isOpen.updated
  let resetHoverPath ← Event.mapM (fun _ => (none : Option MenuPath)) closeEvents
  let mergedHoverPath ← Event.mergeM gatedHoverPath resetHoverPath
  let hoveredPath ← Reactive.holdDyn none mergedHoverPath

  -- Track which submenus are open based on hover
  -- Use foldDyn to maintain state - don't close on momentary no-hover
  let computeOpenPath (hovered : Option MenuPath) (currentOpen : MenuPath) : MenuPath :=
    match hovered with
    | none => currentOpen  -- Keep current state when not hovering anything
    | some path =>
      -- If hovering a submenu item, open that submenu
      if Menu.isEnabledSubmenuAtPath items path then
        path  -- Open this submenu
      else
        -- Compute what the open path should be based on hovered item
        let newOpen := if path.size > 0 then path.pop else #[]
        -- Check if we're still within the currently open subtree
        if Menu.isPathPrefix newOpen currentOpen || newOpen.size >= currentOpen.size then
          newOpen
        else
          -- Hovering outside current subtree, close to new level
          newOpen

  let openPathUpdates ← Event.mapM (fun hp currentOpen => computeOpenPath hp currentOpen) gatedHoverPath
  let resetOpenPath ← Event.mapM (fun _ _ => (#[] : MenuPath)) closeEvents
  let mergedOpenPath ← Event.mergeM openPathUpdates resetOpenPath
  let openSubmenuPath ← Reactive.foldDyn (fun f s => f s) #[] mergedOpenPath

  -- Selection event (only fires for enabled action items)
  let onSelect ← Event.mapMaybeM findClickedAction allClicks

  -- Use dynWidget for efficient change-driven rebuilds
  -- Chain zipWithM to combine 3 dynamics into a tuple
  let renderState ← Dynamic.zipWithM (fun o h => (o, h)) isOpen hoveredPath
  let renderState2 ← Dynamic.zipWithM (fun (o, h) p => (o, h, p)) renderState openSubmenuPath
  let _ ← dynWidget renderState2 fun (open_, hoverPath, openPath) => do
    emit do
      let (_, triggerHeight) ← triggerDimsRef.get
      -- Get trigger widget builders (ComponentRender = IO WidgetBuilder)
      let triggerBuilders ← triggerRenders.mapM id
      pure (menuVisual containerNameFn triggerName itemNameFn items open_ openPath hoverPath theme config triggerHeight triggerBuilders)

  pure (triggerResult, { onSelect, isOpen })

end Afferent.Canopy

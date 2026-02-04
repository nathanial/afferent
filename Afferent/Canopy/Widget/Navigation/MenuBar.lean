/-
  Canopy MenuBar Widget
  Horizontal menu bar with dropdown menus (File, Edit, View, etc.)
-/
import Std.Data.HashMap
import Reactive
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Reactive.Component
import Afferent.Canopy.Widget.Navigation.Menu

namespace Afferent.Canopy

open Afferent.Arbor hiding Event
open Reactive Reactive.Host
open Afferent.Canopy.Reactive

/-- Configuration for a single menu in the bar. -/
structure MenuBarMenu where
  label : String
  items : Array MenuItem
  enabled : Bool := true
deriving Repr, Inhabited

/-- Selection path: menu index + path within that menu. -/
structure MenuBarPath where
  menuIndex : Nat
  itemPath : MenuPath
deriving Repr, Inhabited

/-- Configuration for menu bar appearance. -/
structure MenuBarConfig where
  triggerHeight : Float := 28.0
  triggerPadding : Float := 12.0
  menuGap : Float := 4.0
deriving Repr, Inhabited

/-- Result from menuBar widget. -/
structure MenuBarResult where
  /-- Fires when an action item is selected. -/
  onSelect : Reactive.Event Spider MenuBarPath
  /-- Which menu is currently open (if any). -/
  openMenu : Reactive.Dynamic Spider (Option Nat)

namespace MenuBar

/-- Default menu bar configuration. -/
def defaultConfig : MenuBarConfig := {}

end MenuBar

/-- Build a single menu bar trigger button. -/
def menuBarTriggerVisual (name : String) (label : String) (isOpen : Bool)
    (isHovered : Bool) (enabled : Bool) (theme : Theme)
    (config : MenuBarConfig := MenuBar.defaultConfig) : WidgetBuilder := do
  let bgColor :=
    if !enabled then theme.panel.background
    else if isOpen then theme.primary.background
    else if isHovered then theme.panel.backgroundHover
    else theme.panel.background
  let textColor :=
    if !enabled then theme.textMuted
    else if isOpen then theme.primary.foreground
    else theme.text
  let style : BoxStyle := {
    backgroundColor := some bgColor
    padding := Trellis.EdgeInsets.symmetric config.triggerPadding 6
    minHeight := some config.triggerHeight
  }
  let wid ← freshId
  let props : Trellis.FlexContainer := {
    Trellis.FlexContainer.row 0 with
    alignItems := .center
  }
  let textWidget ← text' label theme.font textColor .center
  pure (.flex wid (some name) props style #[textWidget])

/-- Build the complete menu bar visual with triggers and open menu. -/
def menuBarVisual (triggerNames : Array String)
    (containerNameFn : Nat → MenuPath → String)
    (itemNameFn : Nat → MenuPath → String)
    (menus : Array MenuBarMenu)
    (openMenuIdx : Option Nat) (openSubmenuPath : MenuPath)
    (hoveredPath : Option MenuPath) (hoveredTrigger : Option Nat)
    (triggerWidths : Array Float)
    (theme : Theme) (config : MenuBarConfig := MenuBar.defaultConfig) : WidgetBuilder := do
  -- Build trigger row
  let mut triggers : Array Widget := #[]
  for i in [:menus.size] do
    let menu := menus.getD i { label := "", items := #[] }
    let isOpen := openMenuIdx == some i
    let isHovered := hoveredTrigger == some i
    let trigger ← menuBarTriggerVisual (triggerNames.getD i "") menu.label
      isOpen isHovered menu.enabled theme config
    triggers := triggers.push trigger

  let triggerRowStyle : BoxStyle := {
    backgroundColor := some theme.panel.background
  }
  let triggerRowWid ← freshId
  let triggerRowProps : Trellis.FlexContainer := { direction := .row, gap := 0 }
  let triggerRow := Widget.flex triggerRowWid none triggerRowProps triggerRowStyle triggers

  -- Build open menu if any
  match openMenuIdx with
  | none =>
    -- Just the trigger row when no menu open
    let outerWid ← freshId
    let outerProps : Trellis.FlexContainer := { direction := .column, gap := 0 }
    pure (.flex outerWid none outerProps {} #[triggerRow])
  | some idx =>
    let menu := menus.getD idx { label := "", items := #[] }
    let menuTree ← submenuVisual
      (containerNameFn idx) (itemNameFn idx)
      menu.items #[] openSubmenuPath hoveredPath theme Menu.defaultConfig 0 0

    -- Calculate X offset for menu (sum of trigger widths before this one)
    let menuOffsetX := (List.range idx).foldl (fun acc i =>
      acc + triggerWidths.getD i 100.0
    ) 0.0

    let menuContainerStyle : BoxStyle := {
      position := .absolute
      layer := .overlay
      top := some (config.triggerHeight + config.menuGap)
      left := some menuOffsetX
    }
    let menuContainerWid ← freshId
    let menuContainerProps : Trellis.FlexContainer := { direction := .column, gap := 0 }
    let menuContainer := Widget.flex menuContainerWid none menuContainerProps menuContainerStyle #[menuTree]

    let outerWid ← freshId
    let outerProps : Trellis.FlexContainer := { direction := .column, gap := 0 }
    pure (.flex outerWid none outerProps {} #[triggerRow, menuContainer])

/-- Create a reactive menu bar widget.
    - `menus`: Array of menu configurations
    - `config`: Menu bar configuration
-/
def menuBar (menus : Array MenuBarMenu)
    (config : MenuBarConfig := MenuBar.defaultConfig) : WidgetM MenuBarResult := do
  let theme ← getThemeW
  -- Register trigger names
  let mut triggerNames : Array String := #[]
  for i in [:menus.size] do
    let name ← registerComponentW s!"menubar-trigger-{i}"
    triggerNames := triggerNames.push name

  -- Register item names and container names for all menus
  let mut allItemNames : Std.HashMap (Nat × MenuPath) String := {}
  let mut allContainerNames : Std.HashMap (Nat × MenuPath) String := {}
  for i in [:menus.size] do
    let menu := menus.getD i { label := "", items := #[] }
    let itemNames ← registerMenuItemNames menu.items
    let containerNames ← registerSubmenuContainerNames menu.items
    for (path, name) in itemNames.toList do
      allItemNames := allItemNames.insert (i, path) name
    for (path, name) in containerNames.toList do
      allContainerNames := allContainerNames.insert (i, path) name

  let itemNameFn (menuIdx : Nat) (path : MenuPath) : String :=
    allItemNames.getD (menuIdx, path) ""
  let containerNameFn (menuIdx : Nat) (path : MenuPath) : String :=
    allContainerNames.getD (menuIdx, path) ""

  -- Collect all paths for each menu
  let allPathsByMenu : Array (List MenuPath) := menus.map fun menu =>
    collectAllPaths menu.items

  -- Track trigger widths (estimated initially, updated from layout)
  let triggerWidthsRef ← SpiderM.liftIO (IO.mkRef (menus.map (fun m => m.label.length.toFloat * 8.0 + config.triggerPadding * 2)))

  -- Hooks
  let allClicks ← useAllClicks
  let allHovers ← useAllHovers
  let keyEvents ← useKeyboard

  let triggerHoverTargets := triggerNames.mapIdx fun i name => (name, i)
  let hoveredTriggerChanges ← StateT.lift (hoverEventForTargets triggerHoverTargets)

  let mut itemHoverTargets : Array (String × (Nat × MenuPath)) := #[]
  for menuIdx in [:menus.size] do
    let paths := allPathsByMenu.getD menuIdx []
    for path in paths do
      itemHoverTargets := itemHoverTargets.push (itemNameFn menuIdx path, (menuIdx, path))
  let hoverPathChanges ← StateT.lift (hoverEventForTargets itemHoverTargets)

  -- Update trigger widths from hover data
  let _ ← performEvent_ (← Event.mapM (fun data => do
    for i in [:triggerNames.size] do
      let name := triggerNames.getD i ""
      if hitWidgetHover data name then
        match findWidgetIdByName data.widget name with
        | some widgetId =>
          match data.layouts.get widgetId with
          | some layout =>
            let widths ← triggerWidthsRef.get
            triggerWidthsRef.set (widths.set! i layout.contentRect.width)
          | none => pure ()
        | none => pure ()
  ) allHovers)

  -- Find which trigger was clicked
  let findClickedTrigger (data : ClickData) : Option Nat :=
    (List.range menus.size).findSome? fun i =>
      if hitWidget data (triggerNames.getD i "") then
        let menu := menus.getD i { label := "", items := #[] }
        if menu.enabled then some i else none
      else none

  -- Find clicked enabled action item (returns menu index and path)
  let findClickedAction (data : ClickData) : Option MenuBarPath :=
    (List.range menus.size).findSome? fun menuIdx =>
      let menu := menus.getD menuIdx { label := "", items := #[] }
      let paths := allPathsByMenu.getD menuIdx []
      paths.findSome? fun path =>
        if hitWidget data (itemNameFn menuIdx path) && Menu.isEnabledActionAtPath menu.items path then
          some { menuIndex := menuIdx, itemPath := path }
        else none

  -- Check if click is on any menu container or item
  let isClickInMenu (data : ClickData) : Bool :=
    (List.range menus.size).any fun menuIdx =>
      -- Check containers
      let containers := allContainerNames.toList.filter (fun ((idx, _), _) => idx == menuIdx)
      let inContainer := containers.any (fun (_, name) => hitWidget data name)
      -- Check items
      let paths := allPathsByMenu.getD menuIdx []
      let inItem := paths.any fun path => hitWidget data (itemNameFn menuIdx path)
      inContainer || inItem

  -- Check if click is on any trigger
  let isClickOnTrigger (data : ClickData) : Bool :=
    (List.range triggerNames.size).any fun i => hitWidget data (triggerNames.getD i "")

  -- Click-outside detection
  let isClickOutside (data : ClickData) : Bool :=
    !isClickOnTrigger data && !isClickInMenu data

  -- Open menu state machine
  let openMenu ← SpiderM.fixDynM fun openMenuBehavior => do
    -- Click trigger toggles/switches menu
    let triggerClickEvents ← Event.mapMaybeM findClickedTrigger allClicks
    let toggleMenu ← Event.mapM (fun idx currentOpen =>
      if currentOpen == some idx then none else some idx
    ) triggerClickEvents

    -- Hover trigger while menu open switches immediately
    let hoveredTriggerEvents ← Event.mapMaybeM (fun idxOpt =>
      match idxOpt with
      | some idx =>
        let menu := menus.getD idx { label := "", items := #[] }
        if menu.enabled then some idx else none
      | none => none
    ) hoveredTriggerChanges
    let switchOnHover ← Event.gateM (openMenuBehavior.map (·.isSome)) hoveredTriggerEvents
    let switchMenu ← Event.mapM (fun idx _ => some idx) switchOnHover

    -- Close on enabled action item click
    let itemClicks ← Event.mapMaybeM findClickedAction allClicks
    let closeOnItem ← Event.mapM (fun _ _ => (none : Option Nat)) itemClicks

    -- Close on outside click (gated by open state)
    let outsideClicks ← Event.filterM isClickOutside allClicks
    let gatedOutside ← Event.gateM (openMenuBehavior.map (·.isSome)) outsideClicks
    let closeOnOutside ← Event.mapM (fun _ _ => (none : Option Nat)) gatedOutside

    -- Close on Escape key
    let escapeKeys ← Event.filterM (fun k => k.event.key == .escape && k.event.isPress) keyEvents
    let gatedEscape ← Event.gateM (openMenuBehavior.map (·.isSome)) escapeKeys
    let closeOnEscape ← Event.mapM (fun _ _ => (none : Option Nat)) gatedEscape

    let allTransitions ← Event.leftmostM [closeOnItem, closeOnOutside, closeOnEscape, switchMenu, toggleMenu]
    Reactive.foldDyn (fun f s => f s) none allTransitions

  -- Track hovered trigger (for visual feedback)
  let hoveredTrigger ← Reactive.holdDyn none hoveredTriggerChanges

  -- Track hover path within all menus (we'll filter by open menu at render time)
  -- For each menu, find if any of its items are hovered
  let closeEvents ← Event.filterM (fun open_ => open_.isNone) openMenu.updated
  let resetHoverPath ← Event.mapM (fun _ => (none : Option (Nat × MenuPath))) closeEvents
  let mergedHoverPath ← Event.mergeM hoverPathChanges resetHoverPath
  let hoveredPathWithMenu ← Reactive.holdDyn none mergedHoverPath

  -- Track which submenus are open based on hover
  let computeOpenPath (hoveredWithMenu : Option (Nat × MenuPath)) (currentOpen : MenuPath) : MenuPath :=
    match hoveredWithMenu with
    | none => currentOpen  -- Keep current state when not hovering
    | some (menuIdx, path) =>
      let menu := menus.getD menuIdx { label := "", items := #[] }
      if Menu.isEnabledSubmenuAtPath menu.items path then
        path  -- Open this submenu
      else
        let newOpen := if path.size > 0 then path.pop else #[]
        if Menu.isPathPrefix newOpen currentOpen || newOpen.size >= currentOpen.size then
          newOpen
        else
          newOpen

  let openPathUpdates ← Event.mapM (fun hp currentOpen => computeOpenPath hp currentOpen) hoverPathChanges
  let resetOpenPath ← Event.mapM (fun _ _ => (#[] : MenuPath)) closeEvents
  let mergedOpenPath ← Event.mergeM openPathUpdates resetOpenPath
  let openSubmenuPath ← Reactive.foldDyn (fun f s => f s) #[] mergedOpenPath

  -- Selection event
  let onSelect ← Event.mapMaybeM findClickedAction allClicks

  -- Use dynWidget for efficient change-driven rebuilds
  -- Chain zipWithM to combine 4 dynamics into a tuple
  let renderState ← Dynamic.zipWithM (fun o p => (o, p)) openMenu openSubmenuPath
  let renderState2 ← Dynamic.zipWithM (fun (o, p) h => (o, p, h)) renderState hoveredPathWithMenu
  let renderState3 ← Dynamic.zipWithM (fun (o, p, h) t => (o, p, h, t)) renderState2 hoveredTrigger
  let _ ← dynWidget renderState3 fun (openIdx, openPath, hoveredWithMenu, hovTrigger) => do
    emit do
      let widths ← triggerWidthsRef.get
      -- Extract hovered path only if it's in the currently open menu
      let hoveredPath : Option MenuPath := match openIdx, hoveredWithMenu with
        | some idx, some (menuIdx, path) => if idx == menuIdx then some path else none
        | _, _ => none
      pure (menuBarVisual triggerNames containerNameFn itemNameFn menus
        openIdx openPath hoveredPath hovTrigger widths theme config)

  pure { onSelect, openMenu }

end Afferent.Canopy

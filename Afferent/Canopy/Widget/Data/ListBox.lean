/-
  Canopy ListBox Widget
  Scrollable list with single/multi selection.
-/
import Reactive
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Reactive.Component
import Afferent.Canopy.Widget.Layout.Scroll

namespace Afferent.Canopy

open Afferent.Arbor hiding Event
open Reactive Reactive.Host
open Afferent.Canopy.Reactive
open Trellis

/-- Selection mode for list box items. -/
inductive ListBoxSelectionMode where
  | single    -- Only one item at a time
  | multiple  -- Multiple items can be selected (toggle)
deriving Repr, Inhabited, BEq

/-- Configuration for list box appearance. -/
structure ListBoxConfig where
  itemHeight : Float := 32.0
  itemPadding : Float := 12.0
  maxVisibleItems : Nat := 6
  selectionMode : ListBoxSelectionMode := .single
  borderWidth : Float := 1.0
  /-- Fill available height instead of using maxVisibleItems. -/
  fillHeight : Bool := false
deriving Repr, Inhabited

/-- Result from list box widget. -/
structure ListBoxResult where
  /-- Fires when an item is clicked (item index). -/
  onSelect : Reactive.Event Spider Nat
  /-- Currently selected item indices. -/
  selectedItems : Reactive.Dynamic Spider (Array Nat)
  /-- Currently hovered item index. -/
  hoveredItem : Reactive.Dynamic Spider (Option Nat)

namespace ListBox

/-- Default list box configuration. -/
def defaultConfig : ListBoxConfig := {}

/-- Update selection based on click and selection mode. -/
def updateSelection (mode : ListBoxSelectionMode) (clickedItem : Nat) (current : Array Nat) : Array Nat :=
  match mode with
  | .single => #[clickedItem]
  | .multiple =>
    if current.contains clickedItem then
      current.filter (· != clickedItem)
    else
      current.push clickedItem

end ListBox

/-- Build a single list box item visual. -/
def listBoxItemVisual (name : String) (text : String) (isHovered : Bool)
    (isSelected : Bool) (theme : Theme)
    (config : ListBoxConfig := ListBox.defaultConfig) : WidgetBuilder := do
  let bgColor :=
    if isSelected then theme.primary.background.withAlpha 0.15
    else if isHovered then theme.input.backgroundHover
    else Color.transparent

  let itemStyle : BoxStyle := {
    backgroundColor := some bgColor
    padding := EdgeInsets.symmetric config.itemPadding 8
    minHeight := some config.itemHeight
    width := .percent 1.0
  }

  let wid ← freshId
  let props : FlexContainer := {
    FlexContainer.row 0 with
    alignItems := .center
  }
  let textWidget ← text' text theme.font theme.text .left
  pure (.flex wid (some name) props itemStyle #[textWidget])

/-- Build the complete list box visual (items column). -/
def listBoxItemsVisual (itemNameFn : Nat → String) (items : Array String)
    (selectedItems : Array Nat) (hoveredItem : Option Nat)
    (theme : Theme) (config : ListBoxConfig := ListBox.defaultConfig) : WidgetBuilder := do
  let mut itemWidgets : Array Widget := #[]
  for i in [:items.size] do
    let itemText := items.getD i ""
    let isHovered := hoveredItem == some i
    let isSelected := selectedItems.contains i
    let itemWidget ← listBoxItemVisual (itemNameFn i) itemText isHovered isSelected theme config
    itemWidgets := itemWidgets.push itemWidget

  let wid ← freshId
  let props : FlexContainer := { direction := .column, gap := 0 }
  let containerStyle : BoxStyle := { width := .percent 1.0 }
  pure (.flex wid none props containerStyle itemWidgets)

/-- Create a reactive list box widget.
    - `items`: Array of item labels to display
    - `config`: List box configuration
-/
def listBox (items : Array String)
    (config : ListBoxConfig := ListBox.defaultConfig)
    : WidgetM ListBoxResult := do
  let theme ← getThemeW
  -- Register item names for hit testing
  let mut itemNames : Array String := #[]
  for i in [:items.size] do
    let name ← registerComponentW s!"listbox-item-{i}"
    itemNames := itemNames.push name
  let itemNameFn (i : Nat) : String := itemNames.getD i ""

  -- Hooks
  let allClicks ← useAllClicks

  -- Calculate visible height (used when not filling available space)
  let visibleHeight := (min items.size config.maxVisibleItems).toFloat * config.itemHeight
  let scrollConfig : ScrollContainerConfig := {
    width := 200  -- Default width, can be overridden by parent layout
    height := visibleHeight
    verticalScroll := true
    horizontalScroll := false
    scrollbarVisibility := if config.fillHeight || items.size > config.maxVisibleItems then .always else .hidden
    fillHeight := config.fillHeight
  }

  -- Create trigger for item clicks
  let (itemClickTrigger, fireItemClick) ← Reactive.newTriggerEvent (t := Spider) (a := Nat)

  -- Find which item was clicked (like Table pattern)
  let findClickedItem (data : ClickData) : Option Nat :=
    (List.range items.size).findSome? fun i =>
      if hitWidget data (itemNameFn i) then some i else none

  -- Find which item is hovered
  -- Item click events
  let itemClicks ← Event.mapMaybeM findClickedItem allClicks

  -- Track selected items
  let selectedItems ← Reactive.foldDynM
    (fun (clickedItem : Nat) (current : Array Nat) => do
      SpiderM.liftIO (fireItemClick clickedItem)
      pure (ListBox.updateSelection config.selectionMode clickedItem current)
    )
    (#[] : Array Nat)
    itemClicks

  -- Track hovered item
  let hoverChanges ← StateT.lift (hoverIndexEvent itemNames)
  let hoveredItem ← Reactive.holdDyn (none : Option Nat) hoverChanges

  -- Use scroll container for scrolling - items are emitted INSIDE
  let (_, _) ← scrollContainer scrollConfig do
    -- Use dynWidget for efficient change-driven rebuilds
    let renderState ← Dynamic.zipWithM (fun s h => (s, h)) selectedItems hoveredItem
    let _ ← dynWidget renderState fun (selected, hovered) => do
      emit do pure (listBoxItemsVisual itemNameFn items selected hovered theme config)
    pure ()

  pure { onSelect := itemClickTrigger, selectedItems, hoveredItem }

/-- Create a list box with initial selection.
    - `items`: Array of item labels to display
    - `initialSelection`: Initially selected item indices
    - `config`: List box configuration
-/
def listBoxWithSelection (items : Array String) (initialSelection : Array Nat)
    (config : ListBoxConfig := ListBox.defaultConfig)
    : WidgetM ListBoxResult := do
  let theme ← getThemeW
  -- Register item names for hit testing
  let mut itemNames : Array String := #[]
  for i in [:items.size] do
    let name ← registerComponentW s!"listbox-item-{i}"
    itemNames := itemNames.push name
  let itemNameFn (i : Nat) : String := itemNames.getD i ""

  -- Hooks
  let allClicks ← useAllClicks

  -- Calculate visible height (used when not filling available space)
  let visibleHeight := (min items.size config.maxVisibleItems).toFloat * config.itemHeight
  let scrollConfig : ScrollContainerConfig := {
    width := 200
    height := visibleHeight
    verticalScroll := true
    horizontalScroll := false
    scrollbarVisibility := if config.fillHeight || items.size > config.maxVisibleItems then .always else .hidden
    fillHeight := config.fillHeight
  }

  -- Create trigger for item clicks
  let (itemClickTrigger, fireItemClick) ← Reactive.newTriggerEvent (t := Spider) (a := Nat)

  -- Find which item was clicked (like Table pattern)
  let findClickedItem (data : ClickData) : Option Nat :=
    (List.range items.size).findSome? fun i =>
      if hitWidget data (itemNameFn i) then some i else none

  -- Find which item is hovered
  -- Item click events
  let itemClicks ← Event.mapMaybeM findClickedItem allClicks

  -- Track selected items (with initial selection)
  let selectedItems ← Reactive.foldDynM
    (fun (clickedItem : Nat) (current : Array Nat) => do
      SpiderM.liftIO (fireItemClick clickedItem)
      pure (ListBox.updateSelection config.selectionMode clickedItem current)
    )
    initialSelection
    itemClicks

  -- Track hovered item
  let hoverChanges ← StateT.lift (hoverIndexEvent itemNames)
  let hoveredItem ← Reactive.holdDyn (none : Option Nat) hoverChanges

  -- Use scroll container for scrolling - items are emitted INSIDE
  let (_, _) ← scrollContainer scrollConfig do
    -- Use dynWidget for efficient change-driven rebuilds
    let renderState ← Dynamic.zipWithM (fun s h => (s, h)) selectedItems hoveredItem
    let _ ← dynWidget renderState fun (selected, hovered) => do
      emit do pure (listBoxItemsVisual itemNameFn items selected hovered theme config)
    pure ()

  pure { onSelect := itemClickTrigger, selectedItems, hoveredItem }

end Afferent.Canopy

/-
  Canopy ListBox Widget
  Scrollable list with single/multi selection.
-/
import Reactive
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Reactive.Component
import Afferent.Canopy.Widget.Scroll

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
  pure (.flex wid none props {} itemWidgets)

/-- Create a reactive list box widget.
    - `items`: Array of item labels to display
    - `theme`: Theme for styling
    - `config`: List box configuration
-/
def listBox (items : Array String) (theme : Theme)
    (config : ListBoxConfig := ListBox.defaultConfig)
    : WidgetM ListBoxResult := do
  -- Register item names
  let mut itemNames : Array String := #[]
  for i in [:items.size] do
    let name ← registerComponentW s!"listbox-item-{i}"
    itemNames := itemNames.push name
  let itemNameFn (i : Nat) : String := itemNames.getD i ""

  -- Hooks
  let allClicks ← useAllClicks
  let allHovers ← useAllHovers

  -- Find which item was clicked
  let findClickedItem (data : ClickData) : Option Nat :=
    (List.range items.size).findSome? fun i =>
      if hitWidget data (itemNameFn i) then some i else none

  -- Find which item is hovered
  let findHoveredItem (data : HoverData) : Option Nat :=
    (List.range items.size).findSome? fun i =>
      if hitWidgetHover data (itemNameFn i) then some i else none

  -- Item click events
  let itemClicks ← Event.mapMaybeM findClickedItem allClicks

  -- Track selected items
  let selectedItems ← Reactive.foldDyn
    (fun clickedItem current => ListBox.updateSelection config.selectionMode clickedItem current)
    #[] itemClicks

  -- Track hovered item
  let hoveredItemEvents ← Event.mapM findHoveredItem allHovers
  let hoveredItem ← Reactive.holdDyn none hoveredItemEvents

  -- Calculate visible height
  let visibleHeight := (min items.size config.maxVisibleItems).toFloat * config.itemHeight
  let scrollConfig : ScrollContainerConfig := {
    width := 200  -- Default width, can be overridden by parent layout
    height := visibleHeight
    verticalScroll := true
    horizontalScroll := false
    scrollbarVisibility := if items.size > config.maxVisibleItems then .always else .hidden
  }

  -- Use scroll container for scrolling
  let (_, _scrollResult) ← scrollContainer scrollConfig theme do
    emit do
      let selected ← selectedItems.sample
      let hovered ← hoveredItem.sample
      pure (listBoxItemsVisual itemNameFn items selected hovered theme config)
    pure ()

  pure { onSelect := itemClicks, selectedItems, hoveredItem }

/-- Create a list box with initial selection.
    - `items`: Array of item labels to display
    - `initialSelection`: Initially selected item indices
    - `theme`: Theme for styling
    - `config`: List box configuration
-/
def listBoxWithSelection (items : Array String) (initialSelection : Array Nat)
    (theme : Theme) (config : ListBoxConfig := ListBox.defaultConfig)
    : WidgetM ListBoxResult := do
  -- Register item names
  let mut itemNames : Array String := #[]
  for i in [:items.size] do
    let name ← registerComponentW s!"listbox-item-{i}"
    itemNames := itemNames.push name
  let itemNameFn (i : Nat) : String := itemNames.getD i ""

  -- Hooks
  let allClicks ← useAllClicks
  let allHovers ← useAllHovers

  -- Find which item was clicked
  let findClickedItem (data : ClickData) : Option Nat :=
    (List.range items.size).findSome? fun i =>
      if hitWidget data (itemNameFn i) then some i else none

  -- Find which item is hovered
  let findHoveredItem (data : HoverData) : Option Nat :=
    (List.range items.size).findSome? fun i =>
      if hitWidgetHover data (itemNameFn i) then some i else none

  -- Item click events
  let itemClicks ← Event.mapMaybeM findClickedItem allClicks

  -- Track selected items with initial selection
  let selectedItems ← Reactive.foldDyn
    (fun clickedItem current => ListBox.updateSelection config.selectionMode clickedItem current)
    initialSelection itemClicks

  -- Track hovered item
  let hoveredItemEvents ← Event.mapM findHoveredItem allHovers
  let hoveredItem ← Reactive.holdDyn none hoveredItemEvents

  -- Calculate visible height
  let visibleHeight := (min items.size config.maxVisibleItems).toFloat * config.itemHeight
  let scrollConfig : ScrollContainerConfig := {
    width := 200
    height := visibleHeight
    verticalScroll := true
    horizontalScroll := false
    scrollbarVisibility := if items.size > config.maxVisibleItems then .always else .hidden
  }

  -- Use scroll container for scrolling
  let (_, _scrollResult) ← scrollContainer scrollConfig theme do
    emit do
      let selected ← selectedItems.sample
      let hovered ← hoveredItem.sample
      pure (listBoxItemsVisual itemNameFn items selected hovered theme config)
    pure ()

  pure { onSelect := itemClicks, selectedItems, hoveredItem }

end Afferent.Canopy

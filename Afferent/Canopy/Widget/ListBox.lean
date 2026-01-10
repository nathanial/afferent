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

/-- Combined state for list box selection and hover. -/
structure ListBoxState where
  selectedItems : Array Nat := #[]
  hoveredItem : Option Nat := none
  lastClickedItem : Option Nat := none
deriving Repr, BEq, Inhabited

/-- Event type for list box inputs. -/
inductive ListBoxInputEvent where
  | click (data : ClickData)
  | hover (data : HoverData)

/-- Create a reactive list box widget.
    - `items`: Array of item labels to display
    - `theme`: Theme for styling
    - `config`: List box configuration
-/
def listBox (items : Array String) (theme : Theme)
    (config : ListBoxConfig := ListBox.defaultConfig)
    : WidgetM ListBoxResult := do
  -- Register container name for hit testing
  let containerName ← registerComponentW "listbox-container"

  -- Register item names (still needed for rendering)
  let mut itemNames : Array String := #[]
  for i in [:items.size] do
    let name ← registerComponentW s!"listbox-item-{i}"
    itemNames := itemNames.push name
  let itemNameFn (i : Nat) : String := itemNames.getD i ""

  -- Hooks
  let allClicks ← useAllClicks
  let allHovers ← useAllHovers

  -- Calculate visible height
  let visibleHeight := (min items.size config.maxVisibleItems).toFloat * config.itemHeight
  let scrollConfig : ScrollContainerConfig := {
    width := 200  -- Default width, can be overridden by parent layout
    height := visibleHeight
    verticalScroll := true
    horizontalScroll := false
    scrollbarVisibility := if items.size > config.maxVisibleItems then .always else .hidden
  }

  -- Use scroll container for scrolling - get the scroll state
  let (_, scrollResult) ← scrollContainer scrollConfig theme do
    pure ()

  -- Create trigger for item clicks
  let (itemClickTrigger, fireItemClick) ← Reactive.newTriggerEvent (t := Spider) (a := Nat)

  -- Helper to compute item index from position
  let computeItemIndex (containerRect : LayoutRect) (posX posY scrollOffset : Float) : Option Nat :=
    if posX >= containerRect.x && posX <= containerRect.x + containerRect.width &&
       posY >= containerRect.y && posY <= containerRect.y + containerRect.height then
      let relativeY := posY - containerRect.y + scrollOffset
      let itemIndex := (relativeY / config.itemHeight).floor.toUInt64.toNat
      if itemIndex < items.size then some itemIndex else none
    else none

  -- Lift events to unified type
  let liftSpider {α : Type} : SpiderM α → WidgetM α := fun m => StateT.lift (liftM m)
  let clickEvents ← liftSpider (Event.mapM ListBoxInputEvent.click allClicks)
  let hoverEvents ← liftSpider (Event.mapM ListBoxInputEvent.hover allHovers)
  let allInputEvents ← liftSpider (Event.leftmostM [clickEvents, hoverEvents])

  -- Fold all events into combined state
  let combinedState ← Reactive.foldDynM
    (fun (event : ListBoxInputEvent) (state : ListBoxState) => do
      let scrollState ← scrollResult.scrollState.sample
      let scrollOffset := scrollState.offsetY
      match event with
      | .click clickData =>
        match findWidgetIdByName clickData.widget containerName with
        | some widgetId =>
          match clickData.layouts.get widgetId with
          | some layout =>
            let containerRect := layout.contentRect
            match computeItemIndex containerRect clickData.click.x clickData.click.y scrollOffset with
            | some itemIndex =>
              -- Fire the trigger event
              SpiderM.liftIO (fireItemClick itemIndex)
              let newSelection := ListBox.updateSelection config.selectionMode itemIndex state.selectedItems
              pure { state with selectedItems := newSelection, lastClickedItem := some itemIndex }
            | none => pure state
          | none => pure state
        | none => pure state
      | .hover hoverData =>
        match findWidgetIdByName hoverData.widget containerName with
        | some widgetId =>
          match hoverData.layouts.get widgetId with
          | some layout =>
            let containerRect := layout.contentRect
            let hoveredIdx := computeItemIndex containerRect hoverData.x hoverData.y scrollOffset
            pure { state with hoveredItem := hoveredIdx }
          | none => pure state
        | none => pure state
    )
    ({} : ListBoxState)
    allInputEvents

  -- Extract dynamics from combined state
  let selectedItems ← Dynamic.mapM (fun s => s.selectedItems) combinedState
  let hoveredItem ← Dynamic.mapM (fun s => s.hoveredItem) combinedState

  -- Emit the actual visual
  emit do
    let selected ← selectedItems.sample
    let hovered ← hoveredItem.sample
    pure (listBoxItemsVisual itemNameFn items selected hovered theme config)

  pure { onSelect := itemClickTrigger, selectedItems, hoveredItem }

/-- Create a list box with initial selection.
    - `items`: Array of item labels to display
    - `initialSelection`: Initially selected item indices
    - `theme`: Theme for styling
    - `config`: List box configuration
-/
def listBoxWithSelection (items : Array String) (initialSelection : Array Nat)
    (theme : Theme) (config : ListBoxConfig := ListBox.defaultConfig)
    : WidgetM ListBoxResult := do
  -- Register container name for hit testing
  let containerName ← registerComponentW "listbox-container"

  -- Register item names (still needed for rendering)
  let mut itemNames : Array String := #[]
  for i in [:items.size] do
    let name ← registerComponentW s!"listbox-item-{i}"
    itemNames := itemNames.push name
  let itemNameFn (i : Nat) : String := itemNames.getD i ""

  -- Hooks
  let allClicks ← useAllClicks
  let allHovers ← useAllHovers

  -- Calculate visible height
  let visibleHeight := (min items.size config.maxVisibleItems).toFloat * config.itemHeight
  let scrollConfig : ScrollContainerConfig := {
    width := 200
    height := visibleHeight
    verticalScroll := true
    horizontalScroll := false
    scrollbarVisibility := if items.size > config.maxVisibleItems then .always else .hidden
  }

  -- Use scroll container for scrolling - get the scroll state
  let (_, scrollResult) ← scrollContainer scrollConfig theme do
    pure ()

  -- Create trigger for item clicks
  let (itemClickTrigger, fireItemClick) ← Reactive.newTriggerEvent (t := Spider) (a := Nat)

  -- Helper to compute item index from position
  let computeItemIndex (containerRect : LayoutRect) (posX posY scrollOffset : Float) : Option Nat :=
    if posX >= containerRect.x && posX <= containerRect.x + containerRect.width &&
       posY >= containerRect.y && posY <= containerRect.y + containerRect.height then
      let relativeY := posY - containerRect.y + scrollOffset
      let itemIndex := (relativeY / config.itemHeight).floor.toUInt64.toNat
      if itemIndex < items.size then some itemIndex else none
    else none

  -- Lift events to unified type
  let liftSpider {α : Type} : SpiderM α → WidgetM α := fun m => StateT.lift (liftM m)
  let clickEvents ← liftSpider (Event.mapM ListBoxInputEvent.click allClicks)
  let hoverEvents ← liftSpider (Event.mapM ListBoxInputEvent.hover allHovers)
  let allInputEvents ← liftSpider (Event.leftmostM [clickEvents, hoverEvents])

  -- Fold all events into combined state (with initial selection)
  let combinedState ← Reactive.foldDynM
    (fun (event : ListBoxInputEvent) (state : ListBoxState) => do
      let scrollState ← scrollResult.scrollState.sample
      let scrollOffset := scrollState.offsetY
      match event with
      | .click clickData =>
        match findWidgetIdByName clickData.widget containerName with
        | some widgetId =>
          match clickData.layouts.get widgetId with
          | some layout =>
            let containerRect := layout.contentRect
            match computeItemIndex containerRect clickData.click.x clickData.click.y scrollOffset with
            | some itemIndex =>
              -- Fire the trigger event
              SpiderM.liftIO (fireItemClick itemIndex)
              let newSelection := ListBox.updateSelection config.selectionMode itemIndex state.selectedItems
              pure { state with selectedItems := newSelection, lastClickedItem := some itemIndex }
            | none => pure state
          | none => pure state
        | none => pure state
      | .hover hoverData =>
        match findWidgetIdByName hoverData.widget containerName with
        | some widgetId =>
          match hoverData.layouts.get widgetId with
          | some layout =>
            let containerRect := layout.contentRect
            let hoveredIdx := computeItemIndex containerRect hoverData.x hoverData.y scrollOffset
            pure { state with hoveredItem := hoveredIdx }
          | none => pure state
        | none => pure state
    )
    ({ selectedItems := initialSelection } : ListBoxState)
    allInputEvents

  -- Extract dynamics from combined state
  let selectedItems ← Dynamic.mapM (fun s => s.selectedItems) combinedState
  let hoveredItem ← Dynamic.mapM (fun s => s.hoveredItem) combinedState

  -- Emit the actual visual
  emit do
    let selected ← selectedItems.sample
    let hovered ← hoveredItem.sample
    pure (listBoxItemsVisual itemNameFn items selected hovered theme config)

  pure { onSelect := itemClickTrigger, selectedItems, hoveredItem }

end Afferent.Canopy

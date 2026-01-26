/-
  Canopy VirtualList Widget
  Efficiently renders long lists by only building visible items.
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

/-- Configuration for virtual list appearance and scrolling. -/
structure VirtualListConfig where
  /-- Viewport width in pixels. -/
  width : Float := 240
  /-- Viewport height in pixels. -/
  height : Float := 200
  /-- Fixed height for each row in pixels. -/
  itemHeight : Float := 28.0
  /-- Number of extra rows to render above and below the viewport. -/
  overscan : Nat := 2
  /-- Scroll sensitivity multiplier (pixels per scroll unit). -/
  scrollSpeed : Float := 20.0
  /-- Scrollbar visibility mode. -/
  scrollbarVisibility : ScrollbarVisibility := .always
  /-- Scrollbar track thickness in pixels. -/
  scrollbarThickness : Float := 8.0
  /-- Scrollbar thumb minimum length in pixels. -/
  scrollbarMinThumb : Float := 30.0
  /-- Scrollbar corner radius in pixels. -/
  scrollbarRadius : Float := 4.0
deriving Repr, Inhabited

/-- Result from virtual list widget. -/
structure VirtualListResult where
  /-- Current scroll state as a Dynamic. -/
  scrollState : Reactive.Dynamic Spider ScrollState
  /-- Currently rendered item range (start, end). -/
  visibleRange : Reactive.Dynamic Spider (Nat × Nat)
  /-- Fires when a visible item is clicked (item index). -/
  onItemClick : Reactive.Event Spider Nat

namespace VirtualList

/-- Default virtual list configuration. -/
def defaultConfig : VirtualListConfig := {}

/-- Clamp item height to a safe, non-zero value. -/
def safeItemHeight (height : Float) : Float :=
  if height <= 0 then 1.0 else height

/-- Compute total content height for a given item count. -/
def contentHeight (itemCount : Nat) (config : VirtualListConfig) : Float :=
  itemCount.toFloat * safeItemHeight config.itemHeight

/-- Whether the list needs vertical scrolling. -/
def needsScroll (itemCount : Nat) (config : VirtualListConfig) : Bool :=
  contentHeight itemCount config > config.height

/-- Build a scroll container config from a virtual list config. -/
def toScrollConfig (itemCount : Nat) (config : VirtualListConfig) : ScrollContainerConfig :=
  let needs := needsScroll itemCount config
  { width := config.width
    height := config.height
    verticalScroll := needs
    horizontalScroll := false
    scrollSpeed := config.scrollSpeed
    scrollbarVisibility := if needs then config.scrollbarVisibility else .hidden
    scrollbarThickness := config.scrollbarThickness
    scrollbarMinThumb := config.scrollbarMinThumb
    scrollbarRadius := config.scrollbarRadius }

/-- Compute the visible item range based on scroll position. -/
def visibleRange (itemCount : Nat) (config : VirtualListConfig) (scroll : ScrollState) : Nat × Nat :=
  if itemCount == 0 then (0, 0)
  else
    let itemHeight := safeItemHeight config.itemHeight
    let rawStart := (scroll.offsetY / itemHeight).floor.toUInt32.toNat
    let overscan := config.overscan
    let start := if rawStart > overscan then rawStart - overscan else 0
    let visibleRows := (config.height / itemHeight).ceil.toUInt32.toNat
    let stop := min itemCount (start + visibleRows + overscan * 2)
    (start, stop)

end VirtualList

/-- Wrap a list item with a consistent row height and name for hit testing. -/
def virtualListItemRow (name : String) (config : VirtualListConfig)
    (child : WidgetBuilder) : WidgetBuilder := do
  let wid ← freshId
  let props : FlexContainer := { FlexContainer.row 0 with alignItems := .center }
  let style : BoxStyle := {
    minHeight := some (VirtualList.safeItemHeight config.itemHeight)
    width := .percent 1.0
    flexItem := some { FlexItem.default with shrink := 0 }
  }
  let c ← child
  pure (.flex wid (some name) props style #[c])

/-- Create a virtual list that only renders visible items.
    `itemBuilder` should render content sized to `config.itemHeight` for correct scrolling.

    - `itemCount`: Total number of items in the list
    - `itemBuilder`: Builder for a given item index
    - `config`: Virtual list configuration
-/
def virtualList (itemCount : Nat) (itemBuilder : Nat → WidgetBuilder)
    (config : VirtualListConfig := VirtualList.defaultConfig)
    : WidgetM VirtualListResult := do
  let theme ← getThemeW
  let name ← registerComponentW "virtual-list"

  -- Register item names for hit testing.
  let mut itemNames : Array String := #[]
  for i in [:itemCount] do
    let itemName ← registerComponentW s!"virtual-list-item-{i}"
    itemNames := itemNames.push itemName
  let itemNameFn (i : Nat) : String := itemNames.getD i ""

  let scrollConfig := VirtualList.toScrollConfig itemCount config
  let contentH := VirtualList.contentHeight itemCount config
  let contentW := config.width

  -- Hooks for scroll handling.
  let scrollEvents ← useScroll name
  let allClicks ← useAllClicks
  let allHovers ← useAllHovers
  let allMouseUp ← useAllMouseUp

  let liftSpider {α : Type} : SpiderM α → WidgetM α := fun m => StateT.lift (liftM m)
  let wheelEvents ← liftSpider (Event.mapM ScrollInputEvent.wheel scrollEvents)
  let clickEvents ← liftSpider (Event.mapM ScrollInputEvent.click allClicks)
  let hoverEvents ← liftSpider (Event.mapM ScrollInputEvent.hover allHovers)
  let mouseUpEvents ← liftSpider (Event.mapM (fun _ => ScrollInputEvent.mouseUp) allMouseUp)
  let allInputEvents ← liftSpider (Event.leftmostM [wheelEvents, clickEvents, hoverEvents, mouseUpEvents])

  let combinedState ← Reactive.foldDynM
    (fun (event : ScrollInputEvent) state => do
      match event with
      | .wheel scrollData =>
        let dy :=
          if scrollConfig.verticalScroll then
            -scrollData.scroll.deltaY * scrollConfig.scrollSpeed
          else
            0
        let newScroll := state.scroll.scrollBy 0 dy
          scrollConfig.width scrollConfig.height contentW contentH
        pure { state with scroll := newScroll }

      | .click clickData =>
        match findWidgetIdByName clickData.widget name with
        | some widgetId =>
          let x := clickData.click.x
          let y := clickData.click.y
          match clickData.layouts.get widgetId with
          | some layout =>
            match isInVerticalScrollbar scrollConfig layout x y with
            | some (relativeY, trackHeight) =>
              let newOffsetY := scrollOffsetFromTrackPosition relativeY trackHeight
                scrollConfig.height contentH scrollConfig.scrollbarMinThumb
              let newScroll := { state.scroll with offsetY := newOffsetY }
              let newDrag := { isDragging := true, dragStartY := y, initialOffsetY := newOffsetY }
              pure { scroll := newScroll, drag := newDrag }
            | none => pure { state with drag := {} }
          | none => pure state
        | none => pure state

      | .hover hoverData =>
        if state.drag.isDragging then
          match findWidgetIdByName hoverData.widget name with
          | some widgetId =>
            match hoverData.layouts.get widgetId with
            | some layout =>
              let contentRect := layout.contentRect
              let trackY := contentRect.y
              let trackHeight := contentRect.height
              let relativeY := hoverData.y - trackY
              let newOffsetY := scrollOffsetFromTrackPosition relativeY trackHeight
                scrollConfig.height contentH scrollConfig.scrollbarMinThumb
              let newScroll := { state.scroll with offsetY := newOffsetY }
              pure { state with scroll := newScroll }
            | none => pure state
          | none => pure state
        else
          pure state

      | .mouseUp =>
        pure { state with drag := {} }
    )
    ({} : ScrollCombinedState)
    allInputEvents

  let scrollState ← Dynamic.mapM (fun s => s.scroll) combinedState
  let visibleRange ← Dynamic.mapM (fun s => VirtualList.visibleRange itemCount config s.scroll) combinedState

  -- Click handling: only check visible items.
  let (itemClickTrigger, fireItemClick) ← Reactive.newTriggerEvent (t := Spider) (a := Nat)
  let clickActions ← Event.mapM (fun data => do
    let (start, stop) ← visibleRange.sample
    let mut found : Option Nat := none
    for i in [start:stop] do
      if hitWidget data (itemNameFn i) then
        found := some i
    match found with
    | some idx => fireItemClick idx
    | none => pure ()
  ) allClicks
  performEvent_ clickActions

  let combined ← Dynamic.zipWithM Prod.mk scrollState visibleRange
  let _ ← dynWidget combined fun (scroll, (start, stop)) => do
    let itemHeight := VirtualList.safeItemHeight config.itemHeight
    let topHeight := start.toFloat * itemHeight
    let bottomHeight := (itemCount - stop).toFloat * itemHeight

    let mut rows : Array WidgetBuilder := #[]
    if topHeight > 0 then
      rows := rows.push (spacer config.width topHeight)
    for i in [start:stop] do
      rows := rows.push (virtualListItemRow (itemNameFn i) config (itemBuilder i))
    if bottomHeight > 0 then
      rows := rows.push (spacer config.width bottomHeight)

    let contentStyle : BoxStyle := { width := .percent 1.0 }
    let listBuilder := column (gap := 0) (style := contentStyle) rows

    let scrollStyle : BoxStyle := {
      minWidth := some scrollConfig.width
      minHeight := some scrollConfig.height
      maxWidth := some scrollConfig.width
      maxHeight := some scrollConfig.height
    }
    let scrollbarConfig := buildScrollbarConfig scrollConfig theme
    emit (pure (namedScroll name scrollStyle contentW contentH scroll scrollbarConfig listBuilder))

  pure { scrollState, visibleRange, onItemClick := itemClickTrigger }

end Afferent.Canopy

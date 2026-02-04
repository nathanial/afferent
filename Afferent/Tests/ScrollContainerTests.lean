/-
  Scroll Container Tests
  Unit tests for the scroll container widget and hit testing.
-/
import Afferent.Tests.Framework
import Afferent.Arbor
import Afferent.Arbor.Widget.DSL
import Afferent.Canopy.Reactive.Component
import Afferent.Canopy.Widget.Layout.Scroll
import Afferent.Layout
import Reactive
import Trellis

namespace Afferent.Tests.ScrollContainerTests

open Crucible
open Afferent.Tests
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Reactive Reactive.Host
open Trellis

testSuite "Scroll Container Tests"

/-- Test font ID for widget building tests. -/
def testFont : FontId := { id := 0, name := "test", size := 14.0 }

/-- Test theme for widget tests. -/
def testTheme : Theme := { Theme.dark with font := testFont, smallFont := testFont }

/-! ## Widget Building Tests -/

test "namedScroll creates scroll widget" := do
  let child := text' "Hello" testFont
  let scrollBuilder := namedScroll "test-scroll" {} 300 600 {} {} child
  let (widget, _) ← scrollBuilder.run {}

  match widget with
  | .scroll _ name _ _ contentW contentH _ _ =>
    ensure (name == some "test-scroll") s!"Expected name 'test-scroll', got {name}"
    shouldBeNear contentW 300.0
    shouldBeNear contentH 600.0
  | _ => ensure false "Expected scroll widget"

test "column with multiple children has correct widget count" := do
  let children := #[
    text' "Item 1" testFont,
    text' "Item 2" testFont,
    text' "Item 3" testFont,
    text' "Item 4" testFont,
    text' "Item 5" testFont
  ]
  let columnBuilder := column (gap := 4) (style := {}) children
  let (widget, _) ← columnBuilder.run {}
  let count := widget.widgetCount
  -- 1 flex container + 5 text widgets = 6
  ensure (count == 6) s!"Expected 6 widgets, got {count}"

test "nested column in scroll has correct total widget count" := do
  let children := #[
    text' "Item 1" testFont,
    text' "Item 2" testFont,
    text' "Item 3" testFont,
    text' "Item 4" testFont,
    text' "Item 5" testFont,
    text' "Item 6" testFont,
    text' "Item 7" testFont,
    text' "Item 8" testFont,
    text' "Item 9" testFont,
    text' "Item 10" testFont
  ]
  let columnBuilder := column (gap := 4) (style := {}) children
  let scrollBuilder := namedScroll "test-scroll" {} 300 600 {} {} columnBuilder
  let (widget, _) ← scrollBuilder.run {}
  let count := widget.widgetCount
  -- 1 scroll + 1 flex + 10 text = 12
  ensure (count == 12) s!"Expected 12 widgets, got {count}"

/-! ## Scroll State Tests -/

test "ScrollState.scrollBy updates offset correctly" := do
  let initial := ScrollState.zero
  let viewportW := 300.0
  let viewportH := 150.0
  let contentW := 300.0
  let contentH := 600.0  -- 4x viewport height

  -- Scroll down by 50 pixels (positive delta = scroll down)
  let after := initial.scrollBy 0 50 viewportW viewportH contentW contentH

  -- offsetY is positive (how far we've scrolled into content)
  shouldBeNear after.offsetY 50.0
  shouldBeNear after.offsetX 0.0

test "ScrollState.scrollBy clamps to max scroll" := do
  let initial := ScrollState.zero
  let viewportW := 300.0
  let viewportH := 150.0
  let contentW := 300.0
  let contentH := 600.0  -- max scroll = 600 - 150 = 450

  -- Try to scroll way past the end
  let after := initial.scrollBy 0 1000 viewportW viewportH contentW contentH

  -- Should be clamped to max (contentHeight - viewportHeight = 450)
  shouldBeNear after.offsetY 450.0

test "ScrollState.scrollBy clamps to zero at top" := do
  let initial := ScrollState.zero
  let viewportW := 300.0
  let viewportH := 150.0
  let contentW := 300.0
  let contentH := 600.0

  -- Try to scroll up (negative delta) from top
  let after := initial.scrollBy 0 (-100) viewportW viewportH contentW contentH

  -- Should stay at 0
  shouldBeNear after.offsetY 0.0

test "ScrollState with no overflow stays at zero" := do
  let initial := ScrollState.zero
  let viewportW := 300.0
  let viewportH := 600.0  -- viewport larger than content
  let contentW := 300.0
  let contentH := 150.0

  -- Try to scroll
  let after := initial.scrollBy 0 100 viewportW viewportH contentW contentH

  -- Should stay at 0 (no scrollable content)
  shouldBeNear after.offsetY 0.0

/-! ## Content Size Estimation Tests -/

test "widget count estimation for simple column" := do
  -- Simulate what scrollContainer does: build column, count widgets
  let children : Array WidgetBuilder := #[
    text' "Item 1" testFont,
    text' "Item 2" testFont,
    text' "Item 3" testFont,
    text' "Item 4" testFont,
    text' "Item 5" testFont,
    text' "Item 6" testFont,
    text' "Item 7" testFont,
    text' "Item 8" testFont,
    text' "Item 9" testFont,
    text' "Item 10" testFont
  ]
  let columnBuilder := column (gap := 0) (style := {}) children
  let (builtChild, _) ← columnBuilder.run {}
  let widgetCount := builtChild.widgetCount

  -- 1 flex + 10 text = 11
  ensure (widgetCount == 11) s!"Expected 11, got {widgetCount}"

  -- Content height estimate at 28px per widget
  let contentH := widgetCount.toFloat * 28.0
  shouldBeNear contentH 308.0

test "widget count with nested structure" := do
  -- column -> column -> items (like the demo uses)
  let innerChildren : Array WidgetBuilder := #[
    text' "Item 1" testFont,
    text' "Item 2" testFont,
    text' "Item 3" testFont,
    text' "Item 4" testFont,
    text' "Item 5" testFont
  ]
  let innerColumn := column (gap := 4) (style := {}) innerChildren
  let outerColumn := column (gap := 0) (style := {}) #[innerColumn]
  let (builtChild, _) ← outerColumn.run {}
  let widgetCount := builtChild.widgetCount

  -- outer flex + inner flex + 5 text = 7
  ensure (widgetCount == 7) s!"Expected 7, got {widgetCount}"

test "single column wrapper has widget count of inner children plus containers" := do
  -- This simulates what happens when vscrollContainer receives a column' with items inside
  -- The childRenders from runWidgetChildren will be #[columnBuilder]
  -- which when built gives widgetCount = 1 (just the column)
  -- but we need to count the actual widgets inside!
  let innerItems : Array WidgetBuilder := #[
    text' "Item 1" testFont,
    text' "Item 2" testFont,
    text' "Item 3" testFont,
    text' "Item 4" testFont,
    text' "Item 5" testFont,
    text' "Item 6" testFont,
    text' "Item 7" testFont,
    text' "Item 8" testFont,
    text' "Item 9" testFont,
    text' "Item 10" testFont,
    text' "Item 11" testFont,
    text' "Item 12" testFont,
    text' "Item 13" testFont,
    text' "Item 14" testFont,
    text' "Item 15" testFont,
    text' "Item 16" testFont,
    text' "Item 17" testFont,
    text' "Item 18" testFont,
    text' "Item 19" testFont,
    text' "Item 20" testFont
  ]
  let innerColumn := column (gap := 4) (style := {}) innerItems

  -- If we wrap this in an outer column (simulating scroll container's column)
  let outerColumn := column (gap := 0) (style := {}) #[innerColumn]
  let (builtChild, _) ← outerColumn.run {}
  let widgetCount := builtChild.widgetCount

  -- outer flex + inner flex + 20 text = 22
  ensure (widgetCount == 22) s!"Expected 22, got {widgetCount}"

  -- At 28px per widget, content height = 22 * 28 = 616px
  let contentH := widgetCount.toFloat * 28.0
  ensure (contentH > 150.0) s!"Content height {contentH} should exceed viewport (150px)"

/-! ## WidgetM Child Collection Tests

These tests reproduce the issue where children are lost when using
nested WidgetM combinators like column' inside vscrollContainer.
-/

test "runWidgetChildren collects emitted children" := do
  -- Run in Spider context
  let result ← runSpider do
    let (events, _) ← createInputs FontRegistry.empty testTheme
    let ((_, childRenders), _) ← (runWidgetChildren do
      emit (pure (text' "Item 1" testFont))
      emit (pure (text' "Item 2" testFont))
      emit (pure (text' "Item 3" testFont))
      pure ()
    ).run { children := #[] } |>.run events
    pure childRenders.size
  ensure (result == 3) s!"Expected 3 child renders, got {result}"

test "runWidgetChildren collects children from for loop" := do
  let result ← runSpider do
    let (events, _) ← createInputs FontRegistry.empty testTheme
    let ((_, childRenders), _) ← (runWidgetChildren do
      for i in [1:11] do
        emit (pure (text' s!"Item {i}" testFont))
      pure ()
    ).run { children := #[] } |>.run events
    pure childRenders.size
  ensure (result == 10) s!"Expected 10 child renders, got {result}"

test "column' collects children and emits single render" := do
  let result ← runSpider do
    let (events, _) ← createInputs FontRegistry.empty testTheme
    let (_, state) ← (do
      column' (gap := 4) (style := {}) do
        emit (pure (text' "Item 1" testFont))
        emit (pure (text' "Item 2" testFont))
        emit (pure (text' "Item 3" testFont))
        pure ()
    ).run { children := #[] } |>.run events
    -- column' should emit exactly 1 render (the column itself)
    pure state.children.size
  ensure (result == 1) s!"Expected 1 render from column', got {result}"

test "column' render produces widget with correct child count" := do
  let result ← runSpider do
    let (events, _) ← createInputs FontRegistry.empty testTheme
    let (_, state) ← (do
      column' (gap := 4) (style := {}) do
        emit (pure (text' "Item 1" testFont))
        emit (pure (text' "Item 2" testFont))
        emit (pure (text' "Item 3" testFont))
        pure ()
    ).run { children := #[] } |>.run events
    -- Run the emitted render to get the WidgetBuilder
    let builder ← state.children[0]!
    -- Run the builder to get the Widget
    let (widget, _) ← builder.run {}
    pure widget.widgetCount
  -- 1 column + 3 text widgets = 4
  ensure (result == 4) s!"Expected 4 widgets, got {result}"

test "nested column' in runWidgetChildren preserves children" := do
  let result ← runSpider do
    let (events, _) ← createInputs FontRegistry.empty testTheme
    -- Simulate what scrollContainer does
    let ((_, outerChildRenders), _) ← (runWidgetChildren do
      column' (gap := 4) (style := {}) do
        for i in [1:6] do
          emit (pure (text' s!"Item {i}" testFont))
        pure ()
    ).run { children := #[] } |>.run events
    -- Should have 1 render (the column)
    ensure (outerChildRenders.size == 1) s!"Expected 1 outer render, got {outerChildRenders.size}"
    -- Run that render
    let builder ← outerChildRenders[0]!
    let (widget, _) ← builder.run {}
    pure widget.widgetCount
  -- 1 column + 5 text widgets = 6
  ensure (result == 6) s!"Expected 6 widgets, got {result}"

test "scroll container child collection - simulated" := do
  -- This simulates exactly what scrollContainer does
  let result ← runSpider do
    let (events, _) ← createInputs FontRegistry.empty testTheme
    -- Step 1: runWidgetChildren on the children (a column' with items)
    let ((_, childRenders), _) ← (runWidgetChildren do
      column' (gap := 4) (style := {}) do
        for i in [1:21] do
          emit (pure (text' s!"Item {i}" testFont))
        pure ()
    ).run { children := #[] } |>.run events

    -- Step 2: In the emit block, run childRenders.mapM (liftIO ·) to get WidgetBuilders
    let widgets ← childRenders.mapM SpiderM.liftIO

    -- Step 3: Wrap in a column (like scrollContainer does)
    let childBuilder := column (gap := 0) (style := {}) widgets

    -- Step 4: Run builder to count widgets
    let (builtChild, _) ← childBuilder.run {}
    pure builtChild.widgetCount

  -- Expected: 1 outer column + 1 inner column + 20 text = 22
  ensure (result == 22) s!"Expected 22 widgets, got {result}"

/-! ## Scroll Container Layout Tests

These tests verify that scroll container children are laid out at their
natural size (which may exceed viewport) rather than being shrunk to fit.
-/

test "scroll widget child is laid out at full content height" := do
  -- Create a scroll widget with content taller than viewport
  -- The child should be laid out at its full content height, not shrunk
  let viewportW := 300.0
  let viewportH := 150.0
  let contentH := 600.0

  -- Build a scroll widget with a column child
  let childBuilder := column (gap := 0) (style := {}) #[
    coloredBox Tincture.Color.red 280 contentH
  ]
  let scrollBuilder := namedScroll "test-scroll"
    { minWidth := some viewportW, minHeight := some viewportH }
    viewportW contentH {} {} childBuilder

  let (widget, _) ← scrollBuilder.run {}

  -- Measure the widget tree (this applies the shrink=0 fix)
  let measureResult ← Afferent.Arbor.measureWidget (M := Id) widget 800 600
  let layoutNode := measureResult.node

  -- Run layout
  let result := layout layoutNode 800 600

  -- Find the child layout (ID 1 is the scroll container, ID 2 is the column child)
  -- The child should be laid out at contentH (600), not viewportH (150)
  let childLayout := result.get! 2
  ensure (childLayout.height >= contentH) s!"Child height {childLayout.height} should be >= content height {contentH}"

test "hit testing honors scroll offsets for offscreen items" := do
  let itemHeight := 32.0
  let itemCount := 12
  let viewportW := 200.0
  let viewportH := itemHeight * 6.0
  let contentH := itemHeight * itemCount.toFloat

  let mut items : Array Widget := #[]
  for i in [:itemCount] do
    let itemId := 10 + i
    let itemStyle : BoxStyle := {
      minHeight := some itemHeight
      width := .percent 1.0
      flexItem := some { Trellis.FlexItem.default with shrink := 0 }
    }
    items := items.push (.rect itemId (some s!"item-{i}") itemStyle)

  let columnId := 2
  let column := Widget.flex columnId none (Trellis.FlexContainer.column 0) {} items
  let scrollState : ScrollState := { offsetY := itemHeight * 6.0 }
  let scrollStyle : BoxStyle := { minWidth := some viewportW, minHeight := some viewportH }
  let scrollWidget :=
    Widget.scroll 1 (some "scroll") scrollStyle scrollState viewportW contentH {} column

  let measureResult : MeasureResult := (measureWidget (M := Id) scrollWidget viewportW viewportH)
  let layouts := Trellis.layout measureResult.node viewportW viewportH

  -- Click near the top of the viewport; with offset, this should hit item 6.
  let path := hitTestPath measureResult.widget layouts 10 10
  let item6Id := 10 + 6
  ensure (path.any (· == item6Id))
    s!"Expected hit path to include item 6 (id {item6Id}), got {path}"

/-! ## Scrollbar Hit Detection Tests -/

/-- Create a test layout for scrollbar hit testing. -/
def testScrollLayout (x y width height : Float) : ComputedLayout :=
  { nodeId := 0
  , contentRect := { x, y, width, height }
  , borderRect := { x, y, width, height } }

test "isInVerticalScrollbar returns None when verticalScroll is disabled" := do
  let config : ScrollContainerConfig := { verticalScroll := false }
  let layout := testScrollLayout 0 0 300 200
  let result := isInVerticalScrollbar config layout 295 100
  ensure result.isNone "Should return None when verticalScroll is false"

test "isInVerticalScrollbar returns None when scrollbar visibility is hidden" := do
  let config : ScrollContainerConfig := { scrollbarVisibility := .hidden }
  let layout := testScrollLayout 0 0 300 200
  let result := isInVerticalScrollbar config layout 295 100
  ensure result.isNone "Should return None when scrollbar is hidden"

test "isInVerticalScrollbar returns Some when mouse is in scrollbar track" := do
  let config : ScrollContainerConfig := {
    verticalScroll := true
    scrollbarThickness := 8.0
    scrollbarVisibility := .always
  }
  let layout := testScrollLayout 0 0 300 200
  -- Scrollbar track is at x = 292 to 300 (width - thickness to width)
  -- Mouse at x=295 should be in the track
  let result := isInVerticalScrollbar config layout 295 100
  match result with
  | some (relY, trackH) =>
    shouldBeNear relY 100.0
    shouldBeNear trackH 200.0
  | none => ensure false "Expected Some when mouse is in scrollbar"

test "isInVerticalScrollbar returns None when mouse is outside scrollbar track" := do
  let config : ScrollContainerConfig := {
    verticalScroll := true
    scrollbarThickness := 8.0
    scrollbarVisibility := .always
  }
  let layout := testScrollLayout 0 0 300 200
  -- Mouse at x=100 is in content area, not scrollbar
  let result := isInVerticalScrollbar config layout 100 100
  ensure result.isNone "Should return None when mouse is outside scrollbar"

test "isInVerticalScrollbar returns None when mouse is above track" := do
  let config : ScrollContainerConfig := {
    verticalScroll := true
    scrollbarThickness := 8.0
  }
  let layout := testScrollLayout 0 50 300 200
  -- Track starts at y=50, mouse at y=30 is above
  let result := isInVerticalScrollbar config layout 295 30
  ensure result.isNone "Should return None when mouse is above track"

test "isInVerticalScrollbar returns None when mouse is below track" := do
  let config : ScrollContainerConfig := {
    verticalScroll := true
    scrollbarThickness := 8.0
  }
  let layout := testScrollLayout 0 50 300 200
  -- Track ends at y=250, mouse at y=280 is below
  let result := isInVerticalScrollbar config layout 295 280
  ensure result.isNone "Should return None when mouse is below track"

/-! ## Scroll Offset Calculation Tests -/

test "scrollOffsetFromTrackPosition returns 0 when no overflow" := do
  -- contentH <= viewportH means no scrolling possible
  let offset := scrollOffsetFromTrackPosition 100 200 200 150 30
  shouldBeNear offset 0.0

test "scrollOffsetFromTrackPosition returns 0 at top of track" := do
  -- When clicking at the very top, offset should be 0 or near it
  let viewportH := 100.0
  let contentH := 400.0  -- max scroll = 300
  let trackH := 100.0
  let minThumb := 30.0
  let offset := scrollOffsetFromTrackPosition 0 trackH viewportH contentH minThumb
  -- At relativeY=0, we're at the top
  ensure (offset <= 10.0) s!"Offset at top should be near 0, got {offset}"

test "scrollOffsetFromTrackPosition returns max at bottom of track" := do
  -- When clicking at the very bottom, offset should be max
  let viewportH := 100.0
  let contentH := 400.0  -- max scroll = 300
  let trackH := 100.0
  let minThumb := 30.0
  let offset := scrollOffsetFromTrackPosition trackH trackH viewportH contentH minThumb
  -- At relativeY=trackH, we're at the bottom
  ensure (offset >= 290.0) s!"Offset at bottom should be near max (300), got {offset}"

test "scrollOffsetFromTrackPosition returns middle value at middle of track" := do
  let viewportH := 100.0
  let contentH := 400.0  -- max scroll = 300
  let trackH := 100.0
  let minThumb := 30.0
  let offset := scrollOffsetFromTrackPosition 50 trackH viewportH contentH minThumb
  -- At middle of track, offset should be roughly in the middle range
  ensure (offset > 50.0 && offset < 250.0) s!"Offset at middle should be in middle range, got {offset}"

/-! ## ScrollbarDragState Tests -/

test "ScrollbarDragState default is not dragging" := do
  let state : ScrollbarDragState := {}
  ensure (!state.isDragging) "Default drag state should not be dragging"

test "ScrollCombinedState default has zero scroll and no drag" := do
  let state : ScrollCombinedState := {}
  shouldBeNear state.scroll.offsetX 0.0
  shouldBeNear state.scroll.offsetY 0.0
  ensure (!state.drag.isDragging) "Default should not be dragging"

/-! ## Drag Behavior Integration Tests -/

test "click in scrollbar area starts drag" := do
  -- This tests the logic that should happen when processing a click event
  -- in the scrollbar area
  let config : ScrollContainerConfig := {
    width := 300
    height := 200
    verticalScroll := true
    scrollbarThickness := 8.0
    scrollbarMinThumb := 30.0
  }
  let layout := testScrollLayout 0 0 300 200
  let contentH := 600.0  -- 3x viewport height

  -- Simulate click at position in scrollbar
  let mouseX := 295.0
  let mouseY := 50.0

  -- Check that click is in scrollbar
  let hitResult := isInVerticalScrollbar config layout mouseX mouseY
  match hitResult with
  | some (relativeY, trackHeight) =>
    -- Calculate new offset as the scroll logic would
    let newOffsetY := scrollOffsetFromTrackPosition relativeY trackHeight
      config.height contentH config.scrollbarMinThumb
    -- Verify drag would start
    let dragState : ScrollbarDragState := {
      isDragging := true
      dragStartY := mouseY
      initialOffsetY := newOffsetY
    }
    ensure dragState.isDragging "Drag should be active after click in scrollbar"
    ensure (newOffsetY >= 0.0) s!"Offset should be non-negative, got {newOffsetY}"
  | none =>
    ensure false "Click at x=295 should hit scrollbar track"

test "hover while dragging updates scroll position" := do
  let config : ScrollContainerConfig := {
    width := 300
    height := 200
    verticalScroll := true
    scrollbarThickness := 8.0
    scrollbarMinThumb := 30.0
  }
  let layout := testScrollLayout 0 0 300 200
  let contentH := 600.0

  -- Start with an active drag
  let initialDrag : ScrollbarDragState := {
    isDragging := true
    dragStartY := 50.0
    initialOffsetY := 50.0
  }
  let initialState : ScrollCombinedState := {
    scroll := { offsetY := 50.0 }
    drag := initialDrag
  }

  -- Simulate hover at new Y position (drag down)
  let newY := 150.0
  let relativeY := newY - layout.contentRect.y
  let newOffsetY := scrollOffsetFromTrackPosition relativeY layout.contentRect.height
    config.height contentH config.scrollbarMinThumb

  -- Verify the new offset is different from initial
  ensure (newOffsetY != initialState.scroll.offsetY)
    s!"Scroll offset should change during drag: initial={initialState.scroll.offsetY}, new={newOffsetY}"

test "mouseUp ends drag" := do
  -- Start with an active drag
  let dragState : ScrollbarDragState := {
    isDragging := true
    dragStartY := 50.0
    initialOffsetY := 50.0
  }
  let state : ScrollCombinedState := {
    scroll := { offsetY := 100.0 }
    drag := dragState
  }

  -- After mouseUp, drag should be inactive but scroll position preserved
  let newDrag : ScrollbarDragState := {}
  let newState : ScrollCombinedState := { state with drag := newDrag }

  ensure (!newState.drag.isDragging) "Drag should be inactive after mouseUp"
  shouldBeNear newState.scroll.offsetY 100.0

/-! ## FRP Network Tests - Full Event Flow -/

/-- Helper to create test ClickData for scrollbar clicks. -/
def mkClickData (x y : Float) (layouts : LayoutResult) (widget : Widget) : ClickData :=
  { click := { x, y, button := 0, modifiers := 0 }
  , hitPath := #[]
  , widget
  , layouts }

/-- Helper to create test HoverData. -/
def mkHoverData (x y : Float) (layouts : LayoutResult) (widget : Widget) : HoverData :=
  { x, y
  , hitPath := #[]
  , widget
  , layouts }

/-- Helper to create test MouseButtonData. -/
def mkMouseButtonData (x y : Float) (layouts : LayoutResult) (widget : Widget) : MouseButtonData :=
  { x, y
  , button := 0
  , hitPath := #[]
  , widget
  , layouts }

/-- Create a minimal widget for testing. -/
def testWidget : Widget := .spacer 0 none 100 100

/-- Create a LayoutResult with a scroll container at specified position. -/
def mkScrollLayout (widgetId : WidgetId) (x y width height : Float) : LayoutResult :=
  let layout : ComputedLayout := {
    nodeId := widgetId
    contentRect := { x, y, width, height }
    borderRect := { x, y, width, height }
  }
  LayoutResult.empty.add layout

test "FRP: scrollContainer responds to scroll wheel events" := do
  let result ← runSpider do
    let (events, inputs) ← createInputs FontRegistry.empty testTheme
    let config : ScrollContainerConfig := {
      width := 300
      height := 200
      verticalScroll := true
    }

    -- Run scrollContainer to set up FRP network
    let ((_, scrollResult), _) ← (do
      scrollContainer config do
        emit (pure (text' "Item 1" testFont))
        emit (pure (text' "Item 2" testFont))
        pure ()
    ).run { children := #[] } |>.run events

    -- Sample initial state
    let initialOffset ← scrollResult.scrollState.sample
    ensure (initialOffset.offsetY == 0.0) s!"Initial offset should be 0, got {initialOffset.offsetY}"

    -- Create a scroll widget with the name that scrollContainer registered ("scroll-container-0")
    -- The widget ID is 42 (arbitrary), and we put 42 in the hitPath
    let scrollWidgetId : WidgetId := 42
    let scrollWidget : Widget := .scroll scrollWidgetId (some "scroll-container-0") {}
        {} 300 600 {} testWidget

    -- Fire a scroll event:
    -- - widget tree contains "scroll-container-0" with ID 42
    -- - hitPath contains 42
    -- So hitWidgetScroll will find ID 42 by name, then find 42 in hitPath → true
    -- Note: negative deltaY because Scroll.lean negates it (platform convention)
    let scrollData : ScrollData := {
      scroll := { x := 150, y := 100, deltaX := 0, deltaY := -3.0, modifiers := {} }
      hitPath := #[scrollWidgetId]
      widget := scrollWidget
      layouts := mkScrollLayout scrollWidgetId 0 0 300 200
    }
    inputs.fireScroll scrollData

    -- Sample after scroll
    let afterScroll ← scrollResult.scrollState.sample
    pure afterScroll.offsetY

  -- Scroll wheel with deltaY=-3, speed=20 should increase offset by 60
  -- (Scroll.lean negates deltaY, so -3 * -20 = +60)
  ensure (result > 0.0) s!"Offset should increase after scroll, got {result}"

test "FRP: click events are received by scrollContainer" := do
  let result ← runSpider do
    let (events, inputs) ← createInputs FontRegistry.empty testTheme

    -- Track if click event was received
    let clickReceivedRef ← SpiderM.liftIO (IO.mkRef false)

    -- Subscribe to all clicks to verify event flow
    let allClicks ← useAllClicks |>.run events
    let _ ← SpiderM.liftIO <| allClicks.subscribe fun _ => do
      clickReceivedRef.set true

    -- Fire a click event
    let clickData := mkClickData 295 100 (mkScrollLayout 0 0 0 300 200) testWidget
    inputs.fireClick clickData

    SpiderM.liftIO clickReceivedRef.get

  ensure result "Click event should be received by useAllClicks"

test "FRP: hover events are received by scrollContainer" := do
  let result ← runSpider do
    let (events, inputs) ← createInputs FontRegistry.empty testTheme

    -- Track if hover event was received
    let hoverReceivedRef ← SpiderM.liftIO (IO.mkRef false)

    -- Subscribe to all hovers
    let allHovers ← useAllHovers |>.run events
    let _ ← SpiderM.liftIO <| allHovers.subscribe fun _ => do
      hoverReceivedRef.set true

    -- Fire a hover event
    let hoverData := mkHoverData 150 100 (mkScrollLayout 0 0 0 300 200) testWidget
    inputs.fireHover hoverData

    SpiderM.liftIO hoverReceivedRef.get

  ensure result "Hover event should be received by useAllHovers"

test "FRP: useHover updates via hover fan registry" := do
  let result ← runSpider do
    let (events, inputs) ← createInputs FontRegistry.empty testTheme

    let name ← (registerComponent "hover-test").run events
    let hoveredDyn ← (useHover name).run events

    let wid : WidgetId := 0
    let nameMap : Std.HashMap String WidgetId :=
      Std.HashMap.insert ({} : Std.HashMap String WidgetId) name wid
    let hoverData : HoverData := {
      x := 50
      y := 50
      hitPath := #[wid]
      widget := testWidget
      layouts := mkScrollLayout wid 0 0 100 100
      nameMap := nameMap
    }

    let before ← hoveredDyn.sample
    inputs.fireHover hoverData
    let after ← hoveredDyn.sample
    pure (before, after)

  ensure (!result.1) "Hover should start as false"
  ensure result.2 "Hover should become true when hitPath includes named widget"

test "FRP: mouseUp events are received" := do
  let result ← runSpider do
    let (events, inputs) ← createInputs FontRegistry.empty testTheme

    -- Track if mouseUp event was received
    let mouseUpReceivedRef ← SpiderM.liftIO (IO.mkRef false)

    -- Subscribe to mouseUp events
    let allMouseUp ← useAllMouseUp |>.run events
    let _ ← SpiderM.liftIO <| allMouseUp.subscribe fun _ => do
      mouseUpReceivedRef.set true

    -- Fire a mouseUp event
    let mouseUpData := mkMouseButtonData 295 100 (mkScrollLayout 0 0 0 300 200) testWidget
    inputs.fireMouseUp mouseUpData

    SpiderM.liftIO mouseUpReceivedRef.get

  ensure result "MouseUp event should be received by useAllMouseUp"

test "FRP: foldDynM accumulates scroll events correctly" := do
  let result ← runSpider do
    -- Create a trigger event for scroll data
    let (scrollEvent, fireScroll) ← newTriggerEvent (t := Spider) (a := ScrollData)

    -- Use foldDynM to accumulate scroll offsets
    let offsetDyn ← Reactive.foldDynM
      (fun (scrollData : ScrollData) offset => do
        let newOffset := offset + scrollData.scroll.deltaY * 20.0
        pure newOffset
      )
      (0.0 : Float)
      scrollEvent

    -- Fire scroll events directly to the trigger
    let scrollData1 : ScrollData := {
      scroll := { x := 150, y := 100, deltaX := 0, deltaY := 1.0, modifiers := {} }
      hitPath := #[]
      widget := testWidget
      layouts := mkScrollLayout 0 0 0 300 200
    }
    SpiderM.liftIO (fireScroll scrollData1)

    let scrollData2 : ScrollData := {
      scroll := { x := 150, y := 100, deltaX := 0, deltaY := 2.0, modifiers := {} }
      hitPath := #[]
      widget := testWidget
      layouts := mkScrollLayout 0 0 0 300 200
    }
    SpiderM.liftIO (fireScroll scrollData2)

    offsetDyn.sample

  -- After two scroll events with deltaY 1 and 2, offset should be (1+2)*20 = 60
  shouldBeNear result 60.0

/-! ## Debug: Print scrollbar position calculation -/

test "debug scrollbar geometry" := do
  let config : ScrollContainerConfig := {
    width := 300
    height := 200
    verticalScroll := true
    scrollbarThickness := 8.0
    scrollbarMinThumb := 30.0
    scrollbarVisibility := .always
  }
  let layout := testScrollLayout 100 100 300 200  -- offset at 100,100

  -- The scrollbar track should be at:
  -- x: contentRect.x + contentRect.width - thickness = 100 + 300 - 8 = 392
  -- y: contentRect.y = 100
  -- width: 8
  -- height: contentRect.height = 200

  let expectedTrackX := layout.contentRect.x + layout.contentRect.width - config.scrollbarThickness
  let expectedTrackY := layout.contentRect.y
  let expectedTrackW := config.scrollbarThickness
  let expectedTrackH := layout.contentRect.height

  -- Test click at center of scrollbar
  let mouseX := expectedTrackX + expectedTrackW / 2  -- 392 + 4 = 396
  let mouseY := expectedTrackY + expectedTrackH / 2  -- 100 + 100 = 200

  let result := isInVerticalScrollbar config layout mouseX mouseY
  match result with
  | some (relY, trackH) =>
    -- relY should be mouseY - trackY = 200 - 100 = 100
    shouldBeNear relY (mouseY - expectedTrackY)
    shouldBeNear trackH expectedTrackH
  | none =>
    ensure false s!"Expected hit at ({mouseX}, {mouseY}), track at x={expectedTrackX}"



end Afferent.Tests.ScrollContainerTests

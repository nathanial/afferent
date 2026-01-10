/-
  Scroll Container Tests
  Unit tests for the scroll container widget and hit testing.
-/
import Afferent.Tests.Framework
import Afferent.Arbor
import Afferent.Arbor.Widget.DSL
import Afferent.Canopy.Reactive.Component
import Afferent.Canopy.Widget.Scroll
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
  let scrollBuilder := namedScroll "test-scroll" {} 300 600 {} child
  let (widget, _) ← scrollBuilder.run {}

  match widget with
  | .scroll _ name _ _ contentW contentH _ =>
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
  let scrollBuilder := namedScroll "test-scroll" {} 300 600 {} columnBuilder
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
    let (events, _) ← createInputs
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
    let (events, _) ← createInputs
    let ((_, childRenders), _) ← (runWidgetChildren do
      for i in [1:11] do
        emit (pure (text' s!"Item {i}" testFont))
      pure ()
    ).run { children := #[] } |>.run events
    pure childRenders.size
  ensure (result == 10) s!"Expected 10 child renders, got {result}"

test "column' collects children and emits single render" := do
  let result ← runSpider do
    let (events, _) ← createInputs
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
    let (events, _) ← createInputs
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
    let (events, _) ← createInputs
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
    let (events, _) ← createInputs
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
    viewportW contentH {} childBuilder

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

#generate_tests

end Afferent.Tests.ScrollContainerTests

/-
  Canopy ScrollContainer Widget
  Scrollable viewport for content that exceeds available space.
-/
import Reactive
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Reactive.Component

namespace Afferent.Canopy

open Afferent.Arbor hiding Event
open Reactive Reactive.Host
open Afferent.Canopy.Reactive

/-- Configuration for scroll container. -/
structure ScrollContainerConfig where
  /-- Viewport width in pixels. -/
  width : Float := 300
  /-- Viewport height in pixels. -/
  height : Float := 200
  /-- Enable vertical scrolling. -/
  verticalScroll : Bool := true
  /-- Enable horizontal scrolling. -/
  horizontalScroll : Bool := false
  /-- Scroll sensitivity multiplier. -/
  scrollSpeed : Float := 1.0
deriving Repr, Inhabited

namespace ScrollContainerConfig

def default : ScrollContainerConfig := {}

/-- Create a vertical-only scroll config. -/
def vertical (height : Float) : ScrollContainerConfig :=
  { height, verticalScroll := true, horizontalScroll := false }

/-- Create a horizontal-only scroll config. -/
def horizontal (width : Float) : ScrollContainerConfig :=
  { width, verticalScroll := false, horizontalScroll := true }

/-- Create a config for both directions. -/
def both (width height : Float) : ScrollContainerConfig :=
  { width, height, verticalScroll := true, horizontalScroll := true }

end ScrollContainerConfig

/-- Result from scrollContainer widget. -/
structure ScrollContainerResult where
  /-- Current scroll state as a Dynamic. -/
  scrollState : Reactive.Dynamic Spider ScrollState

/-- Build the visual representation of a scroll container. -/
def scrollContainerVisual (name : String) (config : ScrollContainerConfig)
    (scrollState : ScrollState) (contentWidth contentHeight : Float)
    (child : WidgetBuilder) : WidgetBuilder := do
  let style : BoxStyle := {
    minWidth := some config.width
    minHeight := some config.height
    maxWidth := some config.width
    maxHeight := some config.height
  }
  namedScroll name style contentWidth contentHeight scrollState child

/-- Create a reactive scroll container using WidgetM.
    Wraps children in a scrollable viewport that responds to scroll wheel events.

    - `config`: Scroll container configuration (dimensions, directions)
    - `theme`: Theme for styling (currently unused, reserved for scrollbars)
    - `children`: Child widgets to render inside the scrollable area

    Returns a tuple of the children's result and scroll container result.
-/
def scrollContainer (config : ScrollContainerConfig) (_theme : Theme)
    (children : WidgetM α) : WidgetM (α × ScrollContainerResult) := do
  let name ← registerComponentW "scroll-container"
  let scrollEvents ← useScroll name

  -- Run children to get their renders
  let (result, childRenders) ← runWidgetChildren children

  -- Track content size via ref (updated each render)
  let contentSizeRef ← SpiderM.liftIO (IO.mkRef (config.width, config.height))

  -- Fold scroll events into scroll state, clamped to bounds
  let scrollState ← Reactive.foldDynM
    (fun scrollData state => do
      let (contentW, contentH) ← SpiderM.liftIO contentSizeRef.get
      let dx := if config.horizontalScroll then scrollData.scroll.deltaX * config.scrollSpeed else 0
      let dy := if config.verticalScroll then scrollData.scroll.deltaY * config.scrollSpeed else 0
      pure (state.scrollBy dx dy config.width config.height contentW contentH))
    ScrollState.zero
    scrollEvents

  emit do
    let state ← scrollState.sample
    let widgets ← childRenders.mapM id
    -- Build the child column
    let childBuilder := column (gap := 0) (style := {}) widgets
    -- Run the builder to measure actual widget count
    let (builtChild, _) ← childBuilder.run {}
    let widgetCount := builtChild.widgetCount
    -- Estimate height based on actual widget count (28px per widget)
    let contentH := max config.height (widgetCount.toFloat * 28.0)
    let contentW := config.width
    contentSizeRef.set (contentW, contentH)
    -- Debug: log content size and scroll state
    dbg_trace s!"[ScrollContainer] widgetCount={widgetCount} contentH={contentH} offsetY={state.offsetY}"
    -- Pass the builder (not the built widget) so IDs are fresh
    pure (scrollContainerVisual name config state contentW contentH childBuilder)

  pure (result, { scrollState })

/-- Vertical-only scroll container (convenience wrapper).
    - `height`: Viewport height in pixels
    - `theme`: Theme for styling
    - `children`: Child widgets
-/
def vscrollContainer (height : Float) (theme : Theme)
    (children : WidgetM α) : WidgetM (α × ScrollContainerResult) :=
  scrollContainer (ScrollContainerConfig.vertical height) theme children

/-- Horizontal-only scroll container (convenience wrapper).
    - `width`: Viewport width in pixels
    - `theme`: Theme for styling
    - `children`: Child widgets
-/
def hscrollContainer (width : Float) (theme : Theme)
    (children : WidgetM α) : WidgetM (α × ScrollContainerResult) :=
  scrollContainer (ScrollContainerConfig.horizontal width) theme children

end Afferent.Canopy

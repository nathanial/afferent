/-
  Canopy TabView Widget
  Tabbed content panels with tab bar.
-/
import Reactive
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Widget.Display.Label
import Afferent.Canopy.Reactive.Component

namespace Afferent.Canopy

open Afferent.Arbor hiding Event

/-- Extended state for tab view widgets. -/
structure TabViewState extends WidgetState where
  activeTab : Nat := 0
  hoveredTab : Option Nat := none
deriving Repr, BEq, Inhabited

namespace TabView

/-- Dimensions for tab view rendering. -/
structure Dimensions where
  tabHeight : Float := 36.0
  tabPadding : Float := 16.0
  indicatorHeight : Float := 3.0
  gap : Float := 0.0
  contentPadding : Float := 16.0
deriving Repr, Inhabited

/-- Default tab view dimensions. -/
def defaultDimensions : Dimensions := {}

end TabView

/-- Build a visual tab header button.
    - `name`: Widget name for hit testing
    - `label`: Tab label text
    - `isActive`: Whether this tab is currently selected
    - `isHovered`: Whether this tab is being hovered
    - `theme`: Theme for styling
    - `dims`: Dimension configuration
-/
def tabHeaderVisual (name : String) (label : String) (isActive : Bool)
    (isHovered : Bool) (theme : Theme) (dims : TabView.Dimensions := {}) : WidgetBuilder := do
  -- Tab colors based on state
  let bgColor := if isActive then theme.primary.background.withAlpha 0.15
    else if isHovered then theme.secondary.backgroundHover
    else Color.transparent
  let textColor := if isActive then theme.primary.foreground else theme.text
  let borderColor := if isActive then theme.primary.background else Color.transparent

  let tabStyle : BoxStyle := {
    backgroundColor := some bgColor
    borderColor := some borderColor
    borderWidth := 0  -- We'll use bottom border via indicator
    cornerRadius := 0
    padding := Trellis.EdgeInsets.symmetric dims.tabPadding (dims.tabPadding * 0.5)
    minHeight := some dims.tabHeight
  }

  -- Text widget
  let textWidget ← text' label theme.font textColor .center

  -- Bottom indicator (colored bar when active)
  let indicatorStyle : BoxStyle := {
    backgroundColor := some (if isActive then theme.primary.background else Color.transparent)
    width := .percent 1.0
    height := .length dims.indicatorHeight
  }
  let indicatorWid ← freshId
  let indicator : Widget := .rect indicatorWid none indicatorStyle

  -- Outer container with background
  let outerWid ← freshId
  let outerProps : Trellis.FlexContainer := {
    direction := .column
    gap := 0
    alignItems := .stretch
    justifyContent := .spaceBetween
  }

  -- Content area (text centered)
  let contentStyle : BoxStyle := {
    padding := Trellis.EdgeInsets.symmetric dims.tabPadding (dims.tabPadding * 0.5)
    flexItem := some (Trellis.FlexItem.growing 1)
  }
  let contentWid ← freshId
  let contentProps : Trellis.FlexContainer := {
    direction := .row
    gap := 0
    alignItems := .center
    justifyContent := .center
  }
  let content : Widget := .flex contentWid none contentProps contentStyle #[textWidget]

  pure (.flex outerWid (some name) outerProps tabStyle #[content, indicator])

/-- Build a complete visual tab view widget.
    - `name`: Base widget name
    - `headerNameFn`: Function to generate tab header names from index
    - `tabs`: Array of (label, content) pairs
    - `activeTab`: Currently selected tab index
    - `hoveredTab`: Currently hovered tab index (if any)
    - `theme`: Theme for styling
    - `dims`: Dimension configuration
-/
def tabViewVisual (name : String) (headerNameFn : Nat → String)
    (tabs : Array (String × WidgetBuilder))
    (activeTab : Nat) (hoveredTab : Option Nat) (theme : Theme)
    (dims : TabView.Dimensions := {}) : WidgetBuilder := do
  -- Build tab headers
  let mut tabHeaders : Array Widget := #[]
  for i in [:tabs.size] do
    let (label, _) := tabs.getD i ("", pure (.spacer 0 none 0 0))
    let isActive := i == activeTab
    let isHov := hoveredTab == some i
    let headerWidget ← tabHeaderVisual (headerNameFn i) label isActive isHov theme dims
    tabHeaders := tabHeaders.push headerWidget

  -- Tab bar container
  let tabBarStyle : BoxStyle := {
    backgroundColor := some (theme.panel.background.withAlpha 0.5)
    borderColor := some theme.panel.border
    borderWidth := 0  -- No outer border, individual tabs have indicators
  }
  let tabBarWid ← freshId
  let tabBarProps : Trellis.FlexContainer := {
    direction := .row
    wrap := .wrap
    gap := dims.gap
    rowGap := dims.gap
    alignItems := .stretch
  }
  let tabBar : Widget := .flex tabBarWid none tabBarProps tabBarStyle tabHeaders

  -- Divider line below tab bar
  let dividerStyle : BoxStyle := {
    backgroundColor := some theme.panel.border
    width := .percent 1.0
    height := .length 1.0
  }
  let dividerWid ← freshId
  let divider : Widget := .rect dividerWid none dividerStyle

  -- Content panel (show active tab's content)
  let (_, contentBuilder) := tabs.getD activeTab ("", pure (.spacer 0 none 0 0))
  let contentWidget ← contentBuilder

  let contentPanelStyle : BoxStyle := {
    backgroundColor := some theme.panel.background
    padding := Trellis.EdgeInsets.uniform dims.contentPadding
    flexItem := some (Trellis.FlexItem.growing 1)
    width := .percent 1.0
    height := .percent 1.0
  }
  let contentPanelWid ← freshId
  let contentPanelProps : Trellis.FlexContainer := {
    direction := .column
    gap := 0
  }
  let contentPanel : Widget := .flex contentPanelWid none contentPanelProps contentPanelStyle #[contentWidget]

  -- Outer container (grid: tab bar + divider + content row)
  let outerWid ← freshId
  let outerProps : Trellis.GridContainer :=
    Trellis.GridContainer.withTemplate #[.auto, .fixed (.length 1.0), .fr 1] #[.fr 1]
  let outerStyle : BoxStyle := {
    borderColor := some theme.panel.border
    borderWidth := 1
    cornerRadius := theme.cornerRadius
    flexItem := some (Trellis.FlexItem.growing 1)
    width := .percent 1.0
    height := .percent 1.0
  }

  pure (.grid outerWid (some name) outerProps outerStyle #[tabBar, divider, contentPanel])

/-! ## Reactive TabView Components (FRP-based)

These use WidgetM for declarative composition with automatic tab switching.
-/

open Reactive Reactive.Host
open Afferent.Canopy.Reactive

/-- A tab definition with label and WidgetM content builder. -/
structure TabDef where
  label : String
  content : WidgetM Unit

instance : Inhabited TabDef where
  default := { label := "", content := pure () }

/-- TabView result - events and dynamics. -/
structure TabViewResult where
  onTabChange : Reactive.Event Spider Nat
  activeTab : Reactive.Dynamic Spider Nat

/-- Create a reactive tab view component using WidgetM.
    Emits the tab view widget and returns tab state.
    - `tabs`: Array of tab definitions (label and content)
    - `initialTab`: Initial active tab index
-/
def tabView (tabs : Array TabDef) (initialTab : Nat := 0) : WidgetM TabViewResult := do
  let theme ← getThemeW
  let containerName ← registerComponentW "tabview" (isInteractive := false)

  let mut headerNames : Array String := #[]
  for _ in tabs do
    let name ← registerComponentW "tab-header"
    headerNames := headerNames.push name
  let headerNameFn (i : Nat) : String := headerNames.getD i ""

  -- Pre-run all tab contents to get their renders
  let mut tabContentRenders : Array (Array ComponentRender) := #[]
  for tab in tabs do
    let (_, renders) ← runWidgetChildren tab.content
    tabContentRenders := tabContentRenders.push renders

  let allClicks ← useAllClicks

  let findClickedTab (data : ClickData) : Option Nat :=
    (List.range tabs.size).findSome? fun i =>
      if hitWidget data (headerNameFn i) then some i else none

  let tabChanges ← Event.mapMaybeM findClickedTab allClicks
  let activeTab ← Reactive.holdDyn initialTab tabChanges
  let onTabChange := tabChanges

  let hoverChanges ← StateT.lift (hoverIndexEvent headerNames)
  let hoveredTab ← Reactive.holdDyn none hoverChanges

  let tabsRef := tabs

  -- Use dynWidget for efficient change-driven rebuilds
  let renderState ← Dynamic.zipWithM (fun a h => (a, h)) activeTab hoveredTab
  let _ ← dynWidget renderState fun (active, hovered) => do
    emit do
      let mut tabDefs : Array (String × WidgetBuilder) := #[]
      for i in [:tabsRef.size] do
        let tab := tabsRef[i]!
        let renders := tabContentRenders[i]!
        let contentWidgets ← renders.mapM id
        let contentStyle : BoxStyle := {
          flexItem := some (Trellis.FlexItem.growing 1)
          width := .percent 1.0
          height := .percent 1.0
        }
        let content := column (gap := 0) (style := contentStyle) contentWidgets
        tabDefs := tabDefs.push (tab.label, content)
      pure (tabViewVisual containerName headerNameFn tabDefs active hovered theme)

  pure { onTabChange, activeTab }

end Afferent.Canopy

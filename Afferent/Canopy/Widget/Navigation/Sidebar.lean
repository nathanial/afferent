/-
  Canopy Sidebar Widget
  Collapsible navigation sidebar with main content area.
-/
import Reactive
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Widget.Display.Separator
import Afferent.Canopy.Reactive.Component

namespace Afferent.Canopy

open Afferent.Arbor hiding Event
open Reactive Reactive.Host
open Afferent.Canopy.Reactive

/-- Sidebar configuration options. -/
structure SidebarConfig where
  /-- Width when expanded. -/
  width : Float := 240.0
  /-- Width when collapsed. -/
  collapsedWidth : Float := 48.0
  /-- Start collapsed. -/
  initiallyCollapsed : Bool := false
  /-- Show toggle button. -/
  showToggle : Bool := true
deriving Repr, BEq, Inhabited

/-- Sidebar result - state and controls. -/
structure SidebarResult where
  /-- Dynamic collapsed state. -/
  isCollapsed : Reactive.Dynamic Spider Bool
  /-- Toggle collapsed state. -/
  toggle : IO Unit

/-- Build toggle button visual. -/
private def toggleButtonVisual (name : String) (theme : Theme)
    (collapsed : Bool) (hovered : Bool) : WidgetBuilder := do
  let bgColor := if hovered then theme.secondary.backgroundHover else Color.transparent
  let textColor := theme.textMuted

  let style : BoxStyle := {
    backgroundColor := some bgColor
    cornerRadius := theme.cornerRadius
    padding := Trellis.EdgeInsets.uniform 8
    minWidth := some 32
    minHeight := some 32
  }

  -- Arrow icon: < when expanded, > when collapsed
  let icon := if collapsed then "›" else "‹"

  namedCenter name (style := style) do
    text' icon theme.font textColor .center

/-- Create a reactive sidebar component.
    Returns the combined result of sidebar and main content, plus sidebar controls.
    - `config`: Sidebar configuration
    - `sidebarContent`: Content for the sidebar (receives collapsed state)
    - `mainContent`: Main content area
-/
def sidebar (config : SidebarConfig)
    (sidebarContent : Bool → WidgetM α) (mainContent : WidgetM β)
    : WidgetM ((α × β) × SidebarResult) := do
  let theme ← getThemeW
  let colors := theme.panel

  -- Toggle button registration
  let toggleName ← registerComponentW "sidebar-toggle"
  let toggleClicks ← useClick toggleName
  let toggleHover ← useHover toggleName

  -- Collapse state
  let isCollapsed ← Reactive.foldDyn (fun _ c => !c) config.initiallyCollapsed toggleClicks

  -- Create trigger for external toggle
  let (_, fireToggle) ← Reactive.newTriggerEvent (t := Spider) (a := Unit)
  let toggleAction := fireToggle ()

  -- Sidebar container style
  let sidebarStyle (collapsed : Bool) : BoxStyle := {
    backgroundColor := some colors.background
    borderColor := some (colors.border.withAlpha 0.5)
    borderWidth := 1
    cornerRadius := theme.cornerRadius
    width := .length (if collapsed then config.collapsedWidth else config.width)
    height := .percent 1.0
  }

  -- Main container style
  let mainStyle : BoxStyle := {
    flexItem := some (Trellis.FlexItem.growing 1)
    height := .percent 1.0
  }

  -- Pre-run main content (it's not dynamic)
  let (mainResult, mainRenders) ← runWidgetChildren mainContent

  -- Build the layout with dynamic sidebar
  let renderState ← Dynamic.zipWithM (fun c h => (c, h)) isCollapsed toggleHover

  let sidebarResultRef : IO.Ref (Option α) ← SpiderM.liftIO (IO.mkRef none)

  let _ ← dynWidget renderState fun (collapsed, hovered) => do
    row' (gap := 0) (style := { height := .percent 1.0 }) do
      -- Sidebar
      column' (gap := 0) (style := sidebarStyle collapsed) do
        -- Toggle button at top
        if config.showToggle then
          flexRow' { Trellis.FlexContainer.row 0 with justifyContent := .flexEnd }
              (style := { padding := Trellis.EdgeInsets.uniform 8 }) do
            emit do pure (toggleButtonVisual toggleName theme collapsed hovered)

          hseparator' 1 0

        -- Sidebar content
        let sidebarInnerStyle : BoxStyle := {
          padding := Trellis.EdgeInsets.uniform (if collapsed then 4 else 12)
          flexItem := some (Trellis.FlexItem.growing 1)
        }
        let result ← column' (gap := 8) (style := sidebarInnerStyle) do
          sidebarContent collapsed
        SpiderM.liftIO (sidebarResultRef.set (some result))

      -- Vertical separator
      vseparator' 1 0

      -- Main content area
      column' (gap := 0) (style := mainStyle) do
        for render in mainRenders do
          emit render

  -- Get the sidebar result (should have been set during initial build)
  let sidebarResult ← SpiderM.liftIO do
    match ← sidebarResultRef.get with
    | some r => pure r
    | none => panic! "Sidebar content not rendered"

  pure ((sidebarResult, mainResult), { isCollapsed, toggle := toggleAction })

/-- Create a simple sidebar with static content.
    - `config`: Sidebar configuration
    - `sidebarContent`: Static sidebar content (ignores collapsed state)
    - `mainContent`: Main content area
-/
def simpleSidebar (config : SidebarConfig)
    (sidebarContent : WidgetM α) (mainContent : WidgetM β)
    : WidgetM ((α × β) × SidebarResult) := do
  sidebar config (fun _ => sidebarContent) mainContent

end Afferent.Canopy

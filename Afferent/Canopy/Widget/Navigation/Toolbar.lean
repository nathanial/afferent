/-
  Canopy Toolbar Widget
  Horizontal container for action buttons.
-/
import Reactive
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Widget.Input.Button
import Afferent.Canopy.Reactive.Component

namespace Afferent.Canopy

open Afferent.Arbor hiding Event
open Reactive Reactive.Host
open Afferent.Canopy.Reactive

/-- Toolbar visual variants. -/
inductive ToolbarVariant where
  | filled    -- Solid background
  | outlined  -- Border with transparent background
  | floating  -- Elevated with subtle shadow
deriving Repr, BEq, Inhabited

/-- A toolbar action definition. -/
structure ToolbarAction where
  /-- Unique identifier for the action. -/
  id : String
  /-- Display label. -/
  label : String
  /-- Button variant for styling. -/
  variant : ButtonVariant := .ghost
deriving Repr, BEq, Inhabited

/-- Toolbar result - events from user interaction. -/
structure ToolbarResult where
  /-- Event that fires with the action ID when any button is clicked. -/
  onAction : Reactive.Event Spider String

/-- Build toolbar button visual. -/
private def toolbarButtonVisual (name : String) (action : ToolbarAction)
    (theme : Theme) (state : WidgetState) : WidgetBuilder := do
  let colors := Button.variantColors theme action.variant
  let bgColor := Button.backgroundColor colors state
  let fgColor := Button.foregroundColor colors state
  let bw := Button.borderWidth action.variant

  let style : BoxStyle := {
    backgroundColor := some bgColor
    borderColor := if bw > 0 then some colors.border else none
    borderWidth := bw
    cornerRadius := theme.cornerRadius
    padding := Trellis.EdgeInsets.symmetric (theme.padding * 0.8) (theme.padding * 0.5)
  }

  namedCenter name (style := style) do
    text' action.label theme.font fgColor .center

/-- Create a reactive toolbar component using WidgetM.
    Emits buttons and returns event that fires with action ID on click.
    - `actions`: Array of action definitions
    - `variant`: Toolbar visual variant
-/
def toolbar (actions : Array ToolbarAction)
    (variant : ToolbarVariant := .filled) : WidgetM ToolbarResult := do
  let theme ← getThemeW
  let colors := theme.panel

  let containerStyle : BoxStyle := match variant with
    | .filled => {
        backgroundColor := some colors.background
        borderColor := some (colors.border.withAlpha 0.5)
        borderWidth := 1
        cornerRadius := theme.cornerRadius
        padding := Trellis.EdgeInsets.symmetric 8 4
      }
    | .outlined => {
        backgroundColor := none
        borderColor := some colors.border
        borderWidth := 1
        cornerRadius := theme.cornerRadius
        padding := Trellis.EdgeInsets.symmetric 8 4
      }
    | .floating => {
        backgroundColor := some colors.background
        borderColor := some (colors.border.withAlpha 0.3)
        borderWidth := 1
        cornerRadius := theme.cornerRadius + 2
        padding := Trellis.EdgeInsets.symmetric 10 6
      }

  -- Register button names and collect click events
  let mut buttonNames : Array String := #[]
  for _ in actions do
    let name ← registerComponentW "toolbar-btn"
    buttonNames := buttonNames.push name

  let allClicks ← useAllClicks

  -- Find which button was clicked
  let findClickedAction (data : ClickData) : Option String :=
    (List.range actions.size).findSome? fun i =>
      let name := buttonNames.getD i ""
      if hitWidget data name then
        some (actions.getD i { id := "", label := "" }).id
      else
        none

  let actionClicks ← Event.mapMaybeM findClickedAction allClicks

  -- Hover events for each button
  let hoverChanges ← StateT.lift (hoverIndexEvent buttonNames)
  let hoveredButton ← Reactive.holdDyn none hoverChanges

  -- Build the toolbar with reactive hover states
  let actionsRef := actions
  let buttonNamesRef := buttonNames

  let _ ← dynWidget hoveredButton fun hoveredIdx => do
    row' (gap := 4) (style := containerStyle) do
      for i in [:actionsRef.size] do
        let action := actionsRef[i]!
        let name := buttonNamesRef[i]!
        let isHovered := hoveredIdx == some i
        let state : WidgetState := { hovered := isHovered, pressed := false, focused := false }
        emit do pure (toolbarButtonVisual name action theme state)

  pure { onAction := actionClicks }

/-- Create a simple toolbar with just string labels.
    Action IDs are the same as labels.
    - `labels`: Array of button labels
-/
def simpleToolbar (labels : Array String)
    (variant : ToolbarVariant := .filled) : WidgetM ToolbarResult := do
  let actions := labels.map fun label => { id := label, label := label }
  toolbar actions variant

end Afferent.Canopy

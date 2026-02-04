/-
  Canopy Popover Widget
  Anchored floating content panel that appears on click.
-/
import Reactive
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Reactive.Component

namespace Afferent.Canopy

open Afferent.Arbor hiding Event
open Reactive Reactive.Host
open Afferent.Canopy.Reactive

/-- Position of the popover relative to its anchor. -/
inductive PopoverPosition where
  | top
  | topStart
  | topEnd
  | bottom
  | bottomStart
  | bottomEnd
  | left
  | leftStart
  | leftEnd
  | right
  | rightStart
  | rightEnd
deriving Repr, BEq, Inhabited

/-- Configuration for popover behavior and appearance. -/
structure PopoverConfig where
  /-- Position relative to anchor widget. -/
  position : PopoverPosition := .bottom
  /-- Whether clicking outside dismisses the popover. -/
  dismissOnClickOutside : Bool := true
  /-- Whether pressing escape dismisses the popover. -/
  dismissOnEscape : Bool := true
  /-- Gap between anchor and popover content. -/
  gap : Float := 8.0
  /-- Corner radius for the popover panel. -/
  cornerRadius : Float := 8.0
  /-- Padding inside the popover panel. -/
  padding : Float := 12.0
deriving Repr, Inhabited

namespace PopoverConfig

def default : PopoverConfig := {}

/-- Popover positioned above anchor. -/
def top : PopoverConfig := { position := .top }

/-- Popover positioned below anchor. -/
def bottom : PopoverConfig := { position := .bottom }

/-- Popover positioned to the left of anchor. -/
def left : PopoverConfig := { position := .left }

/-- Popover positioned to the right of anchor. -/
def right : PopoverConfig := { position := .right }

end PopoverConfig

/-- Result from popover widget. -/
structure PopoverResult where
  /-- Event fired when popover closes. -/
  onClose : Reactive.Event Spider Unit
  /-- Whether the popover is currently open. -/
  isOpen : Reactive.Dynamic Spider Bool
  /-- Open the popover programmatically. -/
  openPopover : IO Unit
  /-- Close the popover programmatically. -/
  closePopover : IO Unit
  /-- Toggle the popover open/closed. -/
  togglePopover : IO Unit

namespace Popover

/-- Internal: Get positioning style for popover based on position setting.
    Uses absolute positioning relative to the anchor container.
-/
def positionStyle (pos : PopoverPosition) (gap : Float) : BoxStyle :=
  let base : BoxStyle := { position := .absolute, layer := .overlay }
  match pos with
  | .top => { base with bottom := some gap, left := some 0 }
  | .topStart => { base with bottom := some gap, left := some 0 }
  | .topEnd => { base with bottom := some gap, right := some 0 }
  | .bottom => { base with top := some gap, left := some 0 }
  | .bottomStart => { base with top := some gap, left := some 0 }
  | .bottomEnd => { base with top := some gap, right := some 0 }
  | .left => { base with top := some 0, right := some gap }
  | .leftStart => { base with top := some 0, right := some gap }
  | .leftEnd => { base with bottom := some 0, right := some gap }
  | .right => { base with top := some 0, left := some gap }
  | .rightStart => { base with top := some 0, left := some gap }
  | .rightEnd => { base with bottom := some 0, left := some gap }

end Popover

/-- Build the popover content panel visual.
    - `name`: Widget name for the popover panel
    - `theme`: Theme for styling
    - `config`: Popover configuration
    - `content`: Content widget builder to display
-/
def popoverPanelVisual (name : String) (theme : Theme) (config : PopoverConfig)
    (content : WidgetBuilder) : WidgetBuilder := do
  let posStyle := Popover.positionStyle config.position config.gap
  let panelStyle : BoxStyle := {
    posStyle with
    backgroundColor := some theme.panel.background
    borderColor := some theme.panel.border
    borderWidth := 1
    cornerRadius := config.cornerRadius
    padding := Trellis.EdgeInsets.uniform config.padding
  }

  let contentWidget ← content
  let wid ← freshId
  let props : Trellis.FlexContainer := {
    direction := .column
    gap := 0
  }

  pure (.flex wid (some name) props panelStyle #[contentWidget])

/-- Build a complete popover visual with positioning.
    - `containerName`: Name for the outer container
    - `panelName`: Name for the popover panel (for click detection)
    - `anchorName`: Name for the anchor widget
    - `theme`: Theme for styling
    - `config`: Popover configuration
    - `isOpen`: Whether the popover is visible
    - `anchorWidget`: The anchor widget builder that triggers the popover
    - `popoverContent`: Content builder to display in the popover
-/
def popoverVisual (containerName : String) (panelName : String) (_anchorName : String)
    (theme : Theme) (config : PopoverConfig) (isOpen : Bool)
    (anchorWidget : WidgetBuilder) (popoverContent : WidgetBuilder) : WidgetBuilder := do
  let anchor ← anchorWidget
  if !isOpen then
    -- Just return anchor when closed
    let wid ← freshId
    let props : Trellis.FlexContainer := { direction := .column, gap := 0 }
    pure (.flex wid (some containerName) props {} #[anchor])
  else
    -- Build popover panel with absolute positioning
    let panel ← popoverPanelVisual panelName theme config popoverContent

    -- Outer container - the anchor sets the reference point for absolute positioning
    let wid ← freshId
    let props : Trellis.FlexContainer := { direction := .column, gap := 0 }
    pure (.flex wid (some containerName) props {} #[anchor, panel])

/-- Create a reactive popover component using WidgetM.
    The popover wraps an anchor widget and displays content when triggered.

    - `config`: Popover configuration (position, dismissal behavior)
    - `anchor`: The anchor widget that triggers the popover on click
    - `content`: Content to display in the popover panel

    Returns the anchor's result and popover control functions.

    Example:
    ```
    let (click, popover) ← popover { position := .bottom } (do
      button "Show Menu" .secondary
    ) do
      column' (gap := 8) (style := {}) do
        caption' "Popover Content" theme
        button "Action 1" .ghost
        button "Action 2" .ghost
    ```
-/
def popover (config : PopoverConfig := {}) (anchor : WidgetM α)
    (content : WidgetM Unit) : WidgetM (α × PopoverResult) := do
  let theme ← getThemeW
  let containerName ← registerComponentW "popover" (isInteractive := false)
  let anchorName ← registerComponentW "popover-anchor"
  let panelName ← registerComponentW "popover-panel" (isInteractive := false)

  -- Run anchor and content to get their renders
  let (anchorResult, anchorRenders) ← runWidgetChildren anchor
  let (_, contentRenders) ← runWidgetChildren content

  -- Hooks for interaction
  let anchorClicks ← useClick anchorName
  let allClicks ← useAllClicks
  let keyEvents ← useKeyboard

  -- Create trigger for programmatic open/close
  let (openTrigger, fireOpen) ← Reactive.newTriggerEvent (t := Spider) (a := Bool)

  -- Check if click is outside popover and anchor
  let isClickOutside (data : ClickData) : Bool :=
    config.dismissOnClickOutside &&
    !hitWidget data containerName &&
    !hitWidget data panelName &&
    !hitWidget data anchorName

  -- Check for escape key
  let isEscapePress (keyData : KeyData) : Bool :=
    config.dismissOnEscape &&
    keyData.event.key == .escape &&
    keyData.event.isPress

  -- Build isOpen dynamic with all close triggers
  let isOpen ← SpiderM.fixDynM fun isOpenBehavior => do
    -- Toggle on anchor click
    let toggleEvents ← Event.mapM (fun _ => fun open_ => !open_) anchorClicks
    -- Close on click outside (when open)
    let outsideClicks ← Event.filterM isClickOutside allClicks
    let gatedOutside ← Event.gateM isOpenBehavior outsideClicks
    let closeFromOutside ← Event.mapM (fun _ => fun _ => false) gatedOutside
    -- Close on escape (when open)
    let escapeKeys ← Event.filterM isEscapePress keyEvents
    let gatedEscape ← Event.gateM isOpenBehavior escapeKeys
    let closeFromEscape ← Event.mapM (fun _ => fun _ => false) gatedEscape
    -- Programmatic open/close
    let triggerEvents ← Event.mapM (fun open_ => fun _ => open_) openTrigger
    -- Combine all transitions
    let allTransitions ← Event.leftmostM [closeFromOutside, closeFromEscape, toggleEvents, triggerEvents]
    Reactive.foldDyn (fun f s => f s) false allTransitions

  -- Extract close events
  let closeEvents ← Event.filterM (fun open_ => !open_) isOpen.updated
  let onClose ← Event.voidM closeEvents

  -- Use dynWidget for efficient change-driven rebuilds
  let _ ← dynWidget isOpen fun open_ => do
    emit do
      let anchorWidgets ← anchorRenders.mapM id
      let anchorBuilder := namedColumn anchorName (gap := 0) (style := {}) anchorWidgets

      let contentWidgets ← contentRenders.mapM id
      let contentBuilder := column (gap := 0) (style := {}) contentWidgets

      pure (popoverVisual containerName panelName anchorName theme config open_
        anchorBuilder contentBuilder)

  pure (anchorResult, {
    onClose
    isOpen
    openPopover := fireOpen true
    closePopover := fireOpen false
    togglePopover := do
      let current ← isOpen.sample
      fireOpen (!current)
  })

/-- Convenience: Popover positioned above anchor. -/
def popoverTop (anchor : WidgetM α) (content : WidgetM Unit) : WidgetM (α × PopoverResult) :=
  popover { position := .top } anchor content

/-- Convenience: Popover positioned below anchor. -/
def popoverBottom (anchor : WidgetM α) (content : WidgetM Unit) : WidgetM (α × PopoverResult) :=
  popover { position := .bottom } anchor content

/-- Convenience: Popover positioned to the left of anchor. -/
def popoverLeft (anchor : WidgetM α) (content : WidgetM Unit) : WidgetM (α × PopoverResult) :=
  popover { position := .left } anchor content

/-- Convenience: Popover positioned to the right of anchor. -/
def popoverRight (anchor : WidgetM α) (content : WidgetM Unit) : WidgetM (α × PopoverResult) :=
  popover { position := .right } anchor content

end Afferent.Canopy

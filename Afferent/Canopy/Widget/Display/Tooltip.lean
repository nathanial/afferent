/-
  Canopy Tooltip Widget
  Displays contextual information on hover after a configurable delay.
-/
import Reactive
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Reactive.Component

namespace Afferent.Canopy

open Afferent.Arbor hiding Event
open Reactive Reactive.Host
open Afferent.Canopy.Reactive

/-- Tooltip position relative to the target widget. -/
inductive TooltipPosition where
  | top
  | bottom
  | left
  | right
deriving Repr, BEq, Inhabited

/-- Configuration for tooltip behavior and appearance. -/
structure TooltipConfig where
  /-- Text content of the tooltip. -/
  text : String
  /-- Position relative to target widget. -/
  position : TooltipPosition := .top
  /-- Delay in seconds before showing (0 = instant). -/
  delay : Float := 0.3
deriving Repr, Inhabited

/-- Result from tooltip widget. -/
structure TooltipResult where
  /-- Whether the tooltip is currently visible. -/
  isVisible : Reactive.Dynamic Spider Bool

namespace Tooltip

/-- Default tooltip dimensions. -/
structure Dimensions where
  padding : Float := 6.0
  cornerRadius : Float := 4.0
  gap : Float := 6.0
  tooltipHeight : Float := 24.0  -- Estimated height for positioning
deriving Repr, Inhabited

def defaultDimensions : Dimensions := {}

/-- Calculate tooltip offset relative to parent container.
    Returns (top, left) values for absolute positioning within parent.
    - `tooltipWidth`: Measured width of the tooltip (for left positioning) -/
def calculateOffset (targetWidth targetHeight : Float) (position : TooltipPosition)
    (tooltipWidth : Float) (dims : Dimensions) : Float × Float :=
  let gap := dims.gap
  match position with
  | .top =>
    -- Above target: negative top to go above parent, left=0 to align left edge
    (-(dims.tooltipHeight + gap), 0)
  | .bottom =>
    -- Below target: top = targetHeight + gap
    (targetHeight + gap, 0)
  | .left =>
    -- Left of target: position so tooltip's right edge is gap pixels from target's left edge
    (0, -(tooltipWidth + gap))
  | .right =>
    -- Right of target: left = targetWidth + gap
    (0, targetWidth + gap)

end Tooltip

/-- Build the tooltip visual with absolute positioning relative to parent.
    Uses font measurement for accurate positioning.
-/
def tooltipVisual (text : String) (theme : Theme) (font : Afferent.Font)
    (position : TooltipPosition) (targetWidth targetHeight : Float)
    (dims : Tooltip.Dimensions := Tooltip.defaultDimensions) : IO WidgetBuilder := do
  -- Measure actual text width
  let (textWidth, _) ← font.measureText text
  let tooltipWidth := textWidth + dims.padding * 2

  let (top, left) := Tooltip.calculateOffset targetWidth targetHeight position tooltipWidth dims

  let style : BoxStyle := {
    backgroundColor := some (Color.gray 0.15)
    cornerRadius := dims.cornerRadius
    padding := Trellis.EdgeInsets.uniform dims.padding
    position := .absolute
    layer := .overlay
    top := some top
    left := some left
    borderColor := some (Color.gray 0.3)
    borderWidth := 0.5
  }

  pure (center (style := style) (text' text theme.smallFont Color.white .center))

/-- Internal event type for tooltip state machine. -/
inductive TooltipEvent where
  | hoverChange (hovering : Bool)
  | tick (dt : Float)

/-- Tooltip state tracking hover time and visibility. -/
structure TooltipState where
  /-- Time since hover started (None if not hovering). -/
  hoverElapsed : Option Float
  /-- Whether tooltip is visible. -/
  isVisible : Bool
deriving Repr, BEq, Inhabited

/-- Create a tooltip wrapper around a target widget.
    The tooltip appears after a delay when hovering over the target.
    Uses the default font from WidgetM context (set via createInputs).

    - `config`: Tooltip configuration (text, position, delay)
    - `target`: The widget(s) to wrap with a tooltip

    Returns the target's result and tooltip visibility state.

    Example:
    ```
    let ((), tooltipResult) ← tooltip { text := "Click to submit" } do
      let _ ← button "Submit" .primary
      pure ()
    ```
-/
def tooltip (config : TooltipConfig) (target : WidgetM α) : WidgetM (α × TooltipResult) := do
  let theme ← getThemeW
  let font ← getFontW
  let name ← registerComponentW "tooltip-target"

  -- Run target widget to get its renders
  let (result, targetRenders) ← runWidgetChildren target

  -- Hover detection
  let isHovered ← useHover name
  let allHovers ← useAllHovers
  let animFrames ← useAnimationFrame

  -- Store target dimensions from hover data
  let targetDimsRef ← SpiderM.liftIO (IO.mkRef (60.0, 30.0))  -- Default button-ish size

  -- Update target dimensions when hovering
  let _ ← performEvent_ (← Event.mapM (fun data => do
    if hitWidgetHover data name then
      match findWidgetIdByName data.widget name with
      | some widgetId =>
        match data.layouts.get widgetId with
        | some layout =>
          targetDimsRef.set (layout.contentRect.width, layout.contentRect.height)
        | none => pure ()
      | none => pure ()
  ) allHovers)

  -- Create merged event stream for state machine
  let hoverChanges ← Event.mapM (fun b => TooltipEvent.hoverChange b) isHovered.updated
  let ticks ← Event.mapM TooltipEvent.tick animFrames
  let allEvents ← Event.leftmostM [hoverChanges, ticks]

  -- Fold events into tooltip state
  let tooltipState ← Reactive.foldDyn
    (fun event state =>
      match event with
      | .hoverChange hovering =>
        if hovering then
          -- Start hover timer
          { state with hoverElapsed := some 0.0, isVisible := config.delay <= 0 }
        else
          -- Hide immediately on hover end
          { hoverElapsed := none, isVisible := false }
      | .tick dt =>
        match state.hoverElapsed with
        | some elapsed =>
          let newElapsed := elapsed + dt
          if newElapsed >= config.delay then
            { state with hoverElapsed := some newElapsed, isVisible := true }
          else
            { state with hoverElapsed := some newElapsed }
        | none => state
    )
    ({ hoverElapsed := none, isVisible := false } : TooltipState)
    allEvents

  let isVisible ← Dynamic.mapM (fun s => s.isVisible) tooltipState

  -- Use dynWidget for efficient change-driven rebuilds
  let _ ← dynWidget isVisible fun visible => do
    emit do
      let widgets ← targetRenders.mapM id
      let (targetWidth, targetHeight) ← targetDimsRef.get

      -- Build target with the registered name for hover detection
      let targetBuilder := namedColumn name (gap := 0) (style := {}) widgets

      -- Always emit the same tree structure to prevent hover flicker
      -- When not visible, use zero-sized spacer with absolute positioning
      let tooltipOrPlaceholder ← if visible then
        tooltipVisual config.text theme font config.position targetWidth targetHeight
      else
        -- Invisible placeholder - absolute positioned so doesn't affect layout
        pure (spacer 0 0)

      pure (column (gap := 0) (style := {}) #[targetBuilder, tooltipOrPlaceholder])

  pure (result, { isVisible })

/-- Convenience: tooltip above target. -/
def tooltipTop {α : Type} (text : String) (delay : Float := 0.3)
    (target : WidgetM α) : WidgetM (α × TooltipResult) :=
  tooltip { text, position := .top, delay } target

/-- Convenience: tooltip below target. -/
def tooltipBottom {α : Type} (text : String) (delay : Float := 0.3)
    (target : WidgetM α) : WidgetM (α × TooltipResult) :=
  tooltip { text, position := .bottom, delay } target

/-- Convenience: tooltip left of target. -/
def tooltipLeft {α : Type} (text : String) (delay : Float := 0.3)
    (target : WidgetM α) : WidgetM (α × TooltipResult) :=
  tooltip { text, position := .left, delay } target

/-- Convenience: tooltip right of target. -/
def tooltipRight {α : Type} (text : String) (delay : Float := 0.3)
    (target : WidgetM α) : WidgetM (α × TooltipResult) :=
  tooltip { text, position := .right, delay } target

end Afferent.Canopy

/-
  Canopy SplitPane Widget
  Resizable split container (horizontal/vertical).
-/
import Reactive
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Reactive.Component

namespace Afferent.Canopy

open Afferent.Arbor hiding Event
open Reactive Reactive.Host
open Afferent.Canopy.Reactive
open Trellis

/-- Split pane orientation. -/
inductive SplitPaneOrientation where
  | horizontal
  | vertical
deriving Repr, BEq, Inhabited

/-- Configuration for split pane. -/
structure SplitPaneConfig where
  /-- Layout direction for panes. -/
  orientation : SplitPaneOrientation := .horizontal
  /-- Initial split ratio (0.0 - 1.0). -/
  initialRatio : Float := 0.5
  /-- Minimum size for each pane (in px, along the split axis). -/
  minPaneSize : Float := 120.0
  /-- Thickness of the draggable handle (in px). -/
  handleThickness : Float := 6.0
  /-- Optional fixed width for the split pane. -/
  width : Option Float := none
  /-- Optional fixed height for the split pane. -/
  height : Option Float := none
deriving Repr, Inhabited

namespace SplitPaneConfig

/-- Default split pane configuration. -/
def default : SplitPaneConfig := {}

end SplitPaneConfig

namespace SplitPane

/-- Clamp a Float to [0, 1]. -/
def clamp01 (x : Float) : Float :=
  if x < 0.0 then 0.0 else if x > 1.0 then 1.0 else x

/-- Clamp a ratio based on minimum pane size. -/
def clampRatio (ratio : Float) (available : Float) (config : SplitPaneConfig) : Float :=
  let usable := max 1.0 (available - config.handleThickness)
  let minRatio := config.minPaneSize / usable
  let maxRatio := 1.0 - minRatio
  let clamped := clamp01 ratio
  if minRatio >= maxRatio then 0.5 else
    max minRatio (min maxRatio clamped)

/-- Get the content rect for a named widget. -/
def getWidgetRect (widget : Widget) (layouts : Trellis.LayoutResult)
    (name : String) : Option Trellis.LayoutRect :=
  match findWidgetIdByName widget name with
  | some wid =>
    match layouts.get wid with
    | some layout => some layout.contentRect
    | none => none
  | none => none

/-- Compute ratio from a pointer position within the container rect. -/
def ratioFromPosition (orientation : SplitPaneOrientation) (config : SplitPaneConfig)
    (rect : Trellis.LayoutRect) (pos : Float) : Float :=
  let length := match orientation with
    | .horizontal => rect.width
    | .vertical => rect.height
  if length <= 0.0 then
    clamp01 config.initialRatio
  else
    let start := match orientation with
      | .horizontal => rect.x
      | .vertical => rect.y
    let usable := max 0.0 (length - config.handleThickness)
    if usable <= 0.0 then
      0.5
    else
      let offset := pos - start - config.handleThickness / 2.0
      let raw := offset / usable
      clampRatio raw length config

end SplitPane

/-- Build a visual split pane widget. -/
def splitPaneVisual (containerName handleName : String)
    (config : SplitPaneConfig) (theme : Theme)
    (ratio : Float) (handleHovered : Bool) (isDragging : Bool)
    (first second : WidgetBuilder) : WidgetBuilder := do
  let availableOpt := match config.orientation with
    | .horizontal => config.width
    | .vertical => config.height
  let safeRatio := match availableOpt with
    | some available => SplitPane.clampRatio ratio available config
    | none => SplitPane.clamp01 ratio
  let firstPortion := max 0.001 safeRatio
  let secondPortion := max 0.001 (1.0 - safeRatio)
  let paneFlex (portion : Float) : FlexItem :=
    { FlexItem.default with
      grow := 1
      shrink := 1
      basis := .percent portion }

  let handleColor :=
    if isDragging then theme.primary.background
    else if handleHovered then theme.secondary.backgroundHover
    else theme.panel.border

  let pane1Style : BoxStyle := match config.orientation with
    | .horizontal =>
        { flexItem := some (paneFlex firstPortion)
          minWidth := some config.minPaneSize
          height := .percent 1.0 }
    | .vertical =>
        { flexItem := some (paneFlex firstPortion)
          minHeight := some config.minPaneSize
          width := .percent 1.0 }

  let pane2Style : BoxStyle := match config.orientation with
    | .horizontal =>
        { flexItem := some (paneFlex secondPortion)
          minWidth := some config.minPaneSize
          height := .percent 1.0 }
    | .vertical =>
        { flexItem := some (paneFlex secondPortion)
          minHeight := some config.minPaneSize
          width := .percent 1.0 }

  let handleStyle : BoxStyle := match config.orientation with
    | .horizontal =>
        { backgroundColor := some handleColor
          width := .length config.handleThickness
          height := .percent 1.0
          flexItem := some (FlexItem.fixed config.handleThickness) }
    | .vertical =>
        { backgroundColor := some handleColor
          height := .length config.handleThickness
          width := .percent 1.0
          flexItem := some (FlexItem.fixed config.handleThickness) }

  let outerStyle : BoxStyle := {
    minWidth := config.width
    maxWidth := config.width
    minHeight := config.height
    maxHeight := config.height
  }

  let mkPane : BoxStyle → WidgetBuilder → WidgetBuilder := fun style child => do
    let wid ← freshId
    let props : FlexContainer := { direction := .column, gap := 0, alignItems := .stretch }
    let content ← child
    pure (.flex wid none props style #[content])

  let pane1 ← mkPane pane1Style first
  let pane2 ← mkPane pane2Style second

  let handleWid ← freshId
  let handle : Widget := .rect handleWid (some handleName) handleStyle

  let outerProps : FlexContainer := match config.orientation with
    | .horizontal => { direction := .row, gap := 0, alignItems := .stretch }
    | .vertical => { direction := .column, gap := 0, alignItems := .stretch }

  let outerWid ← freshId
  pure (.flex outerWid (some containerName) outerProps outerStyle #[pane1, handle, pane2])

/-! ## Reactive SplitPane Components (FRP-based) -/

/-- Combined state for split pane interaction. -/
structure SplitPaneState where
  ratio : Float := 0.5
  isDragging : Bool := false
deriving Repr, BEq, Inhabited

/-- Result from splitPane widget. -/
structure SplitPaneResult where
  /-- Fires when the split ratio changes. -/
  onResize : Reactive.Event Spider Float
  /-- Current split ratio as a Dynamic. -/
  ratio : Reactive.Dynamic Spider Float

/-- Input events for split pane interactions. -/
inductive SplitPaneInputEvent where
  | click (data : ClickData)
  | hover (data : HoverData)
  | mouseUp

/-- Create a reactive split pane component using WidgetM.
    Emits the split pane widget and returns ratio state.
    - `config`: Split pane configuration
    - `first`: Left/top pane contents
    - `second`: Right/bottom pane contents
-/
def splitPane (config : SplitPaneConfig) (first : WidgetM α) (second : WidgetM β)
    : WidgetM ((α × β) × SplitPaneResult) := do
  let theme ← getThemeW
  let containerName ← registerComponentW "split-pane" (isInteractive := false)
  let handleName ← registerComponentW "split-pane-handle"

  -- Pre-run child panes to get their renders
  let (firstResult, firstRenders) ← runWidgetChildren first
  let (secondResult, secondRenders) ← runWidgetChildren second

  let handleHovered ← useHover handleName
  let allClicks ← useAllClicks
  let allHovers ← useAllHovers
  let allMouseUp ← useAllMouseUp

  let liftSpider {α : Type} : SpiderM α → WidgetM α := fun m => StateT.lift (liftM m)
  let clickEvents ← liftSpider (Event.mapM SplitPaneInputEvent.click allClicks)
  let hoverEvents ← liftSpider (Event.mapM SplitPaneInputEvent.hover allHovers)
  let mouseUpEvents ← liftSpider (Event.mapM (fun _ => SplitPaneInputEvent.mouseUp) allMouseUp)
  let allInputEvents ← liftSpider (Event.leftmostM [clickEvents, hoverEvents, mouseUpEvents])

  let initialState : SplitPaneState := { ratio := SplitPane.clamp01 config.initialRatio }

  let combinedState ← Reactive.foldDynM
    (fun event state => do
      match event with
      | .click clickData =>
        if clickData.click.button != 0 then
          pure state
        else if hitWidget clickData handleName then
          let pos := match config.orientation with
            | .horizontal => clickData.click.x
            | .vertical => clickData.click.y
          match SplitPane.getWidgetRect clickData.widget clickData.layouts containerName with
          | some rect =>
            let newRatio := SplitPane.ratioFromPosition config.orientation config rect pos
            pure { ratio := newRatio, isDragging := true }
          | none =>
            pure { state with isDragging := true }
        else
          pure state

      | .hover hoverData =>
        if state.isDragging then
          let pos := match config.orientation with
            | .horizontal => hoverData.x
            | .vertical => hoverData.y
          match SplitPane.getWidgetRect hoverData.widget hoverData.layouts containerName with
          | some rect =>
            let newRatio := SplitPane.ratioFromPosition config.orientation config rect pos
            pure { state with ratio := newRatio }
          | none => pure state
        else
          pure state

      | .mouseUp =>
        pure { state with isDragging := false }
    )
    initialState
    allInputEvents

  let ratioDyn ← Dynamic.mapM (fun s => s.ratio) combinedState
  let onResize ← Event.mapM (fun s => s.ratio) combinedState.updated

  -- Use dynWidget for efficient change-driven rebuilds
  let renderState ← Dynamic.zipWithM (fun s h => (s, h)) combinedState handleHovered
  let _ ← dynWidget renderState fun (state, hovered) => do
    emit do
      let firstWidgets ← firstRenders.mapM id
      let secondWidgets ← secondRenders.mapM id
      let firstContent := column (gap := 0) (style := {}) firstWidgets
      let secondContent := column (gap := 0) (style := {}) secondWidgets
      pure (splitPaneVisual containerName handleName config theme state.ratio hovered state.isDragging
        firstContent secondContent)

  pure ((firstResult, secondResult), { onResize, ratio := ratioDyn })

end Afferent.Canopy

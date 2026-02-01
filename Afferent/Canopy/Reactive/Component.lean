/-
  Canopy Reactive - Component Infrastructure
  React-like component helpers for self-contained widget definitions.
-/
import Std.Data.HashMap
import Reactive
import Afferent.Arbor
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Widget.Display.Label
import Afferent.Canopy.Widget.Layout.Panel
import Afferent.Canopy.Reactive.Types
import Afferent.Canopy.Reactive.Inputs

open Reactive Reactive.Host
open Afferent.Canopy

namespace Afferent.Canopy.Reactive

/-! ## The ReactiveM Monad

Like React's context, ReactiveM carries the event streams implicitly.
Components use hooks that access this context without explicit parameters.
-/

/-- ReactiveM is SpiderM with implicit access to ReactiveEvents.
    This is analogous to how React components access context through hooks. -/
abbrev ReactiveM := ReaderT ReactiveEvents SpiderM

/-- Explicit ForIn instance for ReactiveM to avoid issues with derived instances.
    This properly threads through both the ReactiveEvents context and SpiderEnv. -/
instance [ForIn SpiderM ρ α] : ForIn ReactiveM ρ α where
  forIn x init f := fun ctx => ForIn.forIn x init fun a b => f a b ctx

/-- Explicit MonadLift instance to allow SpiderM operations in ReactiveM without liftSpider.
    ReaderT should provide this automatically, but being explicit ensures it works. -/
instance : MonadLift SpiderM ReactiveM where
  monadLift m := fun _ => m

/-- MonadSample instance for ReactiveM - delegates to SpiderM. -/
instance : MonadSample Spider ReactiveM where
  sample b := fun _ => b.sample

/-- MonadHold instance for ReactiveM - delegates to SpiderM.
    This allows holdDyn, foldDyn, etc. to work directly in ReactiveM. -/
instance : MonadHold Spider ReactiveM where
  hold initial event := fun _ => MonadHold.hold initial event
  holdDyn initial event := fun _ => MonadHold.holdDyn initial event
  foldDyn f init event := fun _ => MonadHold.foldDyn f init event
  foldDynM f init event := fun ctx =>
    MonadHold.foldDynM (m := SpiderM) (fun a b => f a b ctx) init event

/-- TriggerEvent instance for ReactiveM - delegates to SpiderM.
    This allows newTriggerEvent to work directly in ReactiveM. -/
instance : TriggerEvent Spider ReactiveM where
  newTriggerEvent := fun _ => TriggerEvent.newTriggerEvent
  newEventWithTrigger callback := fun _ => TriggerEvent.newEventWithTrigger callback

/-- Run a ReactiveM computation with the given events context. -/
def ReactiveM.run (events : ReactiveEvents) (m : ReactiveM α) : SpiderM α :=
  ReaderT.run m events

/-- Get the events from the implicit context. -/
def getEvents : ReactiveM ReactiveEvents := read

/-- Get the font registry from the implicit context. -/
def getFontRegistry : ReactiveM Afferent.FontRegistry := do
  let events ← getEvents
  pure events.fontRegistry

/-- Get the theme from the implicit context. -/
def getTheme : ReactiveM Theme := do
  let events ← getEvents
  pure events.theme

/-- Get the default font from the implicit context.
    Throws an error if font was not provided to createInputs. -/
def getFont : ReactiveM Afferent.Font := do
  let events ← getEvents
  match events.font with
  | some f => pure f
  | none => SpiderM.liftIO (IO.throwServerError
      "Font not provided in ReactiveEvents. Pass a font to createInputs.")

/-- Register a component and get an auto-generated name.
    This is the preferred way to register components in ReactiveM context. -/
def registerComponent (namePrefix : String) (isInput : Bool := false)
    (isInteractive : Bool := true) : ReactiveM String := do
  let events ← getEvents
  SpiderM.liftIO <| events.registry.register namePrefix isInput isInteractive

/-! ## Optional Hover/DynWidget Metrics (bench-only instrumentation) -/

private def isSwitchName (name : String) : Bool :=
  name.startsWith "switch-"

structure HoverMetrics where
  mapNanos : IO.Ref Nat
  mapCount : IO.Ref Nat
  mapSwitchNanos : IO.Ref Nat
  mapSwitchCount : IO.Ref Nat
  holdNanos : IO.Ref Nat
  holdCount : IO.Ref Nat
  holdSwitchNanos : IO.Ref Nat
  holdSwitchCount : IO.Ref Nat

structure HoverMetricsSnapshot where
  mapNanos : Nat
  mapCount : Nat
  mapSwitchNanos : Nat
  mapSwitchCount : Nat
  holdNanos : Nat
  holdCount : Nat
  holdSwitchNanos : Nat
  holdSwitchCount : Nat
deriving Repr, Inhabited

def HoverMetrics.new : IO HoverMetrics := do
  pure {
    mapNanos := (← IO.mkRef 0)
    mapCount := (← IO.mkRef 0)
    mapSwitchNanos := (← IO.mkRef 0)
    mapSwitchCount := (← IO.mkRef 0)
    holdNanos := (← IO.mkRef 0)
    holdCount := (← IO.mkRef 0)
    holdSwitchNanos := (← IO.mkRef 0)
    holdSwitchCount := (← IO.mkRef 0)
  }

def HoverMetrics.reset (m : HoverMetrics) : IO Unit := do
  m.mapNanos.set 0
  m.mapCount.set 0
  m.mapSwitchNanos.set 0
  m.mapSwitchCount.set 0
  m.holdNanos.set 0
  m.holdCount.set 0
  m.holdSwitchNanos.set 0
  m.holdSwitchCount.set 0

def HoverMetrics.snapshot (m : HoverMetrics) : IO HoverMetricsSnapshot := do
  pure {
    mapNanos := (← m.mapNanos.get)
    mapCount := (← m.mapCount.get)
    mapSwitchNanos := (← m.mapSwitchNanos.get)
    mapSwitchCount := (← m.mapSwitchCount.get)
    holdNanos := (← m.holdNanos.get)
    holdCount := (← m.holdCount.get)
    holdSwitchNanos := (← m.holdSwitchNanos.get)
    holdSwitchCount := (← m.holdSwitchCount.get)
  }

initialize hoverMetricsRef : IO.Ref (Option HoverMetrics) ← IO.mkRef none

def enableHoverMetrics : IO HoverMetrics := do
  let metrics ← HoverMetrics.new
  hoverMetricsRef.set (some metrics)
  pure metrics

def disableHoverMetrics : IO Unit :=
  hoverMetricsRef.set none

private def recordHoverMap (metrics : HoverMetrics) (name : String) (nanos : Nat) : IO Unit := do
  metrics.mapNanos.modify (· + nanos)
  metrics.mapCount.modify (· + 1)
  if isSwitchName name then
    metrics.mapSwitchNanos.modify (· + nanos)
    metrics.mapSwitchCount.modify (· + 1)

private def recordHoverHold (metrics : HoverMetrics) (name : String) (nanos : Nat) : IO Unit := do
  metrics.holdNanos.modify (· + nanos)
  metrics.holdCount.modify (· + 1)
  if isSwitchName name then
    metrics.holdSwitchNanos.modify (· + nanos)
    metrics.holdSwitchCount.modify (· + 1)

structure DynWidgetMetrics where
  rebuildNanos : IO.Ref Nat
  rebuildCount : IO.Ref Nat

structure DynWidgetMetricsSnapshot where
  rebuildNanos : Nat
  rebuildCount : Nat
deriving Repr, Inhabited

def DynWidgetMetrics.new : IO DynWidgetMetrics := do
  pure {
    rebuildNanos := (← IO.mkRef 0)
    rebuildCount := (← IO.mkRef 0)
  }

def DynWidgetMetrics.reset (m : DynWidgetMetrics) : IO Unit := do
  m.rebuildNanos.set 0
  m.rebuildCount.set 0

def DynWidgetMetrics.snapshot (m : DynWidgetMetrics) : IO DynWidgetMetricsSnapshot := do
  pure {
    rebuildNanos := (← m.rebuildNanos.get)
    rebuildCount := (← m.rebuildCount.get)
  }

initialize dynWidgetMetricsRef : IO.Ref (Option DynWidgetMetrics) ← IO.mkRef none

def enableDynWidgetMetrics : IO DynWidgetMetrics := do
  let metrics ← DynWidgetMetrics.new
  dynWidgetMetricsRef.set (some metrics)
  pure metrics

def disableDynWidgetMetrics : IO Unit :=
  dynWidgetMetricsRef.set none

/-- Lift SpiderM into ReactiveM. Prefer using automatic lifting instead. -/
@[deprecated "Use automatic monad lifting instead of explicit liftSpider"]
def liftSpider (m : SpiderM α) : ReactiveM α := fun _ => m

/-! ## Type Aliases -/

/-- A component's render function - samples its internal dynamics and produces a widget. -/
abbrev ComponentRender := IO Afferent.Arbor.WidgetBuilder

/-! ## The WidgetM Monad

WidgetM combines FRP network construction (ReactiveM) with widget tree accumulation.
This enables Reflex-DOM style monadic widget building where components emit their
renders into the parent container automatically.
-/

/-- State for accumulating child render functions during widget building. -/
structure WidgetMState where
  /-- Array of child component render functions, accumulated in order. -/
  children : Array ComponentRender := #[]
deriving Inhabited

/-- WidgetM combines FRP setup (ReactiveM) with widget accumulation.
    Components use `emit` to add their render functions to the current container. -/
abbrev WidgetM := StateT WidgetMState ReactiveM

/-- ForIn instance for WidgetM - threads state through each iteration properly.
    This ensures that emit calls inside for loops accumulate correctly. -/
instance [ForIn ReactiveM ρ α] : ForIn WidgetM ρ α where
  forIn x init f := fun s => do
    -- Thread state through by including it in the accumulator
    let (result, finalState) ← ForIn.forIn x (init, s) fun a (b, currentState) => do
      let (step, newState) ← f a b currentState
      match step with
      | .done b' => pure (ForInStep.done (b', newState))
      | .yield b' => pure (ForInStep.yield (b', newState))
    pure (result, finalState)

/-- MonadLift from SpiderM to WidgetM. -/
instance : MonadLift SpiderM WidgetM where
  monadLift m := StateT.lift (fun _ => m)

/-- MonadSample instance for WidgetM - delegates to ReactiveM. -/
instance : MonadSample Spider WidgetM where
  sample b := StateT.lift (sample b)

/-- MonadHold instance for WidgetM - delegates to ReactiveM. -/
instance : MonadHold Spider WidgetM where
  hold initial event := StateT.lift (MonadHold.hold initial event)
  holdDyn initial event := StateT.lift (MonadHold.holdDyn initial event)
  foldDyn f init event := StateT.lift (MonadHold.foldDyn f init event)
  foldDynM f init event := fun s => do
    let result ← MonadHold.foldDynM (m := ReactiveM) (fun a b => (f a b).run' s) init event
    pure (result, s)

/-- TriggerEvent instance for WidgetM - delegates to ReactiveM. -/
instance : TriggerEvent Spider WidgetM where
  newTriggerEvent := StateT.lift TriggerEvent.newTriggerEvent
  newEventWithTrigger callback := StateT.lift (TriggerEvent.newEventWithTrigger callback)

/-! ## WidgetM Theme and Font Access -/

/-- Get the theme from WidgetM context. -/
def getThemeW : WidgetM Theme := StateT.lift getTheme

/-- Get the default font from WidgetM context.
    Throws if font was not provided to createInputs. -/
def getFontW : WidgetM Afferent.Font := StateT.lift getFont

/-- Run a WidgetM computation with a different theme (for subtree overrides). -/
def withTheme (theme : Theme) (m : WidgetM α) : WidgetM α := do
  let s ← get
  let modifyEvents := fun events => { events with theme := theme }
  let (result, newState) ← StateT.lift (withReader modifyEvents (m.run s))
  set newState
  pure result

/-! ## WidgetM Core Helpers -/

/-- Emit a widget render function into the current container's children.
    This is the primary way components contribute their visual representation. -/
def emit (render : ComponentRender) : WidgetM Unit := do
  modify fun s => { s with children := s.children.push render }

/-- Run a WidgetM computation and extract both the result and collected child renders.
    Used by container combinators to gather children's render functions. -/
def runWidgetChildren (m : WidgetM α) : WidgetM (α × Array ComponentRender) := do
  let parentState ← get
  set (WidgetMState.mk #[])
  let result ← m
  let childState ← get
  set parentState
  pure (result, childState.children)

/-- Run a WidgetM computation in ReactiveM context and extract the final render.
    This is used at the top level to get a single ComponentRender from WidgetM. -/
def runWidget (m : WidgetM α) : ReactiveM (α × ComponentRender) := do
  let (result, state) ← m.run { children := #[] }
  let render : ComponentRender := do
    if state.children.isEmpty then
      pure (Afferent.Arbor.spacer 0 0)
    else if h : state.children.size = 1 then
      state.children[0]
    else
      let widgets ← state.children.mapM id
      pure (Afferent.Arbor.column (gap := 0) (style := {}) widgets)
  pure (result, render)

/-- Get the events from WidgetM context. -/
def getEventsW : WidgetM ReactiveEvents := StateT.lift getEvents

/-- Register a component in WidgetM context. -/
def registerComponentW (namePrefix : String) (isInput : Bool := false)
    (isInteractive : Bool := true) : WidgetM String :=
  StateT.lift (registerComponent namePrefix isInput isInteractive)

/-! ## Hit Testing Helpers -/

/-- Find a widget ID by name in the widget tree.
    Uses a depth limit to ensure termination. -/
def findWidgetIdByName (widget : Afferent.Arbor.Widget)
    (target : String) (maxDepth : Nat := 100) : Option Afferent.Arbor.WidgetId :=
  go widget target maxDepth
where
  go (widget : Afferent.Arbor.Widget) (target : String) (fuel : Nat)
      : Option Afferent.Arbor.WidgetId :=
    match fuel with
    | 0 => none  -- Depth limit reached
    | fuel' + 1 =>
      let widgetName := Afferent.Arbor.Widget.name? widget
      match widgetName with
      | some name =>
          if name == target then
            some (Afferent.Arbor.Widget.id widget)
          else
            findInChildren widget target fuel'
      | none =>
          findInChildren widget target fuel'
  findInChildren (widget : Afferent.Arbor.Widget) (target : String) (fuel : Nat)
      : Option Afferent.Arbor.WidgetId :=
    let children := Afferent.Arbor.Widget.children widget
    let rec loop (idx : Nat) : Option Afferent.Arbor.WidgetId :=
      if idx >= children.size then
        none
      else
        match children[idx]? with
        | some child =>
            match go child target fuel with
            | some result => some result
            | none => loop (idx + 1)
        | none => loop (idx + 1)
    loop 0

/-- Build a name->id map for the widget tree (used to speed up hit checks). -/
partial def buildNameMap (widget : Afferent.Arbor.Widget) : Std.HashMap String Afferent.Arbor.WidgetId :=
  go widget {}
where
  go (w : Afferent.Arbor.Widget) (acc : Std.HashMap String Afferent.Arbor.WidgetId) :
      Std.HashMap String Afferent.Arbor.WidgetId :=
    let acc := match Afferent.Arbor.Widget.name? w with
      | some name => acc.insert name (Afferent.Arbor.Widget.id w)
      | none => acc
    w.children.foldl (fun m child => go child m) acc

/-- Check if a named widget is in the hit path. -/
def hitPathHasNamedWidget (widget : Afferent.Arbor.Widget)
    (hitPath : Array Afferent.Arbor.WidgetId) (name : String)
    (nameMap : Std.HashMap String Afferent.Arbor.WidgetId) : Bool :=
  match nameMap[name]? with
  | some wid => hitPath.any (· == wid)
  | none =>
      match findWidgetIdByName widget name with
      | some wid => hitPath.any (· == wid)
      | none => false

/-- Check if a widget name is in the hit path (for ClickData). -/
def hitWidget (data : ClickData) (name : String) : Bool :=
  hitPathHasNamedWidget data.widget data.hitPath name data.nameMap

/-- Check if a widget name is in the hit path (for HoverData). -/
def hitWidgetHover (data : HoverData) (name : String) : Bool :=
  hitPathHasNamedWidget data.widget data.hitPath name data.nameMap

/-- Check if a widget name is in the hit path (for ScrollData). -/
def hitWidgetScroll (data : ScrollData) (name : String) : Bool :=
  hitPathHasNamedWidget data.widget data.hitPath name data.nameMap

/-- Calculate slider value from click position given the slider's layout.
    `trackWidth` is the width of the slider track in pixels. -/
def calculateSliderValue (clickX : Float) (layouts : Trellis.LayoutResult)
    (widget : Afferent.Arbor.Widget) (sliderName : String) (trackWidth : Float) : Option Float :=
  match findWidgetIdByName widget sliderName with
  | some wid =>
      match layouts.get wid with
      | some layout =>
          let rect := layout.contentRect
          let relativeX := clickX - rect.x
          let value := relativeX / trackWidth
          let clampedValue := if value < 0.0 then 0.0 else if value > 1.0 then 1.0 else value
          some clampedValue
      | none => none
  | none => none

/-! ## Component Hooks

These are like React hooks - they access the event context implicitly
and set up subscriptions automatically.
-/

/-- Create a hover state Dynamic for a widget (like React's useState + useEffect for hover).
    Returns a Dynamic that is true when the mouse is over the widget. -/
def useHover (name : String) : ReactiveM (Dynamic Spider Bool) := do
  let events ← getEvents
  let metricsOpt ← SpiderM.liftIO hoverMetricsRef.get
  let hoverChanges ← Event.selectM events.hoverFan name

  match metricsOpt with
  | none =>
      let baseDyn ← holdDyn false hoverChanges
      Dynamic.holdUniqDynM baseDyn
  | some metrics =>
      let nodeId ← SpiderM.freshNodeId
      let (dyn, update) ← SpiderM.liftIO <|
        Reactive.Dynamic.newWithId (t := Spider) false nodeId
      let _ ← Reactive.Host.Event.subscribeM hoverChanges fun value => do
        let old ← dyn.sample
        if value != old then
          let t0 ← IO.monoNanosNow
          update value
          let t1 ← IO.monoNanosNow
          recordHoverHold metrics name (t1 - t0)
      pure dyn

/-- Build a hover event for named targets using hoverFan.
    Returns `some payload` on enter and `none` on leave, preferring enters when both occur. -/
def hoverEventForTargets (targets : Array (String × α))
    : ReactiveM (Reactive.Event Spider (Option α)) := do
  let events ← getEvents
  let mut enterEvents : Array (Reactive.Event Spider (Option α)) := #[]
  let mut leaveEvents : Array (Reactive.Event Spider (Option α)) := #[]
  for (name, payload) in targets do
    let hoverChanges ← Event.selectM events.hoverFan name
    let enter ← Event.mapMaybeM (fun hovered => if hovered then some (some payload) else none) hoverChanges
    let leave ← Event.mapMaybeM (fun hovered => if hovered then some (none : Option α) else none) hoverChanges
    enterEvents := enterEvents.push enter
    leaveEvents := leaveEvents.push leave
  let ctx ← SpiderM.getTimelineCtx
  let neverHover ← SpiderM.liftIO (Reactive.Event.never ctx)
  let combined := enterEvents ++ leaveEvents
  match combined.toList with
  | [] => pure neverHover
  | events => Event.leftmostM events

/-- Convenience: hover event that returns the hovered index (or none). -/
def hoverIndexEvent (names : Array String) : ReactiveM (Reactive.Event Spider (Option Nat)) := do
  let targets := names.mapIdx fun i name => (name, i)
  hoverEventForTargets targets

/-- Create a click event for a widget that fires Unit (like React's onClick handler).
    Returns an Event that fires when the widget is clicked. -/
def useClick (name : String) : ReactiveM (Event Spider Unit) := do
  let events ← getEvents
  let clicks ← Event.filterM (fun data => hitWidget data name) events.clickEvent
  Event.voidM clicks

/-- Get animation frame events (fires each frame with delta time).
    Use for widgets that need delta time (e.g., physics, hover delay tracking).
    For continuous animations, prefer useElapsedTime which shares a single Dynamic. -/
def useAnimationFrame : ReactiveM (Event Spider Float) := do
  let events ← getEvents
  pure events.animationFrame

/-- Get the shared elapsed time Dynamic (seconds since app start).
    All animated widgets share this single Dynamic instead of each creating their own foldDyn.
    This dramatically reduces FRP overhead when many widgets animate simultaneously. -/
def useElapsedTime : ReactiveM (Dynamic Spider Float) := do
  let events ← getEvents
  pure events.elapsedTime

/-- Subscribe to key events. Returns the raw key event stream. -/
def useKeyboard : ReactiveM (Event Spider KeyData) := do
  let events ← getEvents
  Event.filterM (fun data => data.event.isPress) events.keyEvent

/-- Subscribe to all key events (press and release). -/
def useKeyboardAll : ReactiveM (Event Spider KeyData) := do
  let events ← getEvents
  pure events.keyEvent

/-- Subscribe to mouse delta events (relative movement since last frame). -/
def useMouseDelta : ReactiveM (Event Spider MouseDeltaData) := do
  let events ← getEvents
  pure events.mouseDeltaEvent

/-- Create click event with full data (for sliders that need position). -/
def useClickData (name : String) : ReactiveM (Event Spider ClickData) := do
  let events ← getEvents
  Event.filterM (fun data => hitWidget data name) events.clickEvent

/-- Subscribe to all click events (for focus management). -/
def useAllClicks : ReactiveM (Event Spider ClickData) := do
  let events ← getEvents
  pure events.clickEvent

/-- Subscribe to all mouse up events (for drag ending). -/
def useAllMouseUp : ReactiveM (Event Spider MouseButtonData) := do
  let events ← getEvents
  pure events.mouseUpEvent

/-- Subscribe to all hover events (for position tracking). -/
def useAllHovers : ReactiveM (Event Spider HoverData) := do
  let events ← getEvents
  pure events.hoverEvent

/-- Subscribe to scroll events for a named widget.
    Returns an Event that fires when scrolling occurs over the widget. -/
def useScroll (name : String) : ReactiveM (Event Spider ScrollData) := do
  let events ← getEvents
  Event.filterM (fun data => hitWidgetScroll data name) events.scrollEvent

/-- Subscribe to all scroll events (for custom handling). -/
def useAllScrolls : ReactiveM (Event Spider ScrollData) := do
  let events ← getEvents
  pure events.scrollEvent

/-- Set up automatic focus clearing when clicking non-input interactive widgets.
    Call this after all components have been created. -/
def ComponentRegistry.setupFocusClearing (reg : ComponentRegistry) : ReactiveM Unit := do
  let inputs ← SpiderM.liftIO reg.inputNames.get
  let interactives ← SpiderM.liftIO reg.interactiveNames.get

  let isInputClick (data : ClickData) : Bool :=
    inputs.any (fun name => hitWidget data name)
  let isNonInputInteractiveClick (data : ClickData) : Bool :=
    interactives.any (fun name => hitWidget data name)

  let allClicks ← useAllClicks
  let nonInputClicks ← Event.filterM
    (fun data => !isInputClick data && isNonInputInteractiveClick data) allClicks
  let clearAction ← Event.mapM (fun _ => reg.fireFocus none) nonInputClicks
  performEvent_ clearAction

/-! ## WidgetM Container Combinators

These combinators run child WidgetM computations and wrap their renders
in container widgets. They enable declarative nesting like Reflex-DOM.
-/

/-- Create a column container that collects children's renders.
    Children are laid out vertically with the specified gap. -/
def column' (gap : Float := 0) (style : Afferent.Arbor.BoxStyle := {})
    (children : WidgetM α) : WidgetM α := do
  let (result, childRenders) ← runWidgetChildren children
  emit do
    let widgets ← childRenders.mapM id
    pure (Afferent.Arbor.column (gap := gap) (style := style) widgets)
  pure result

/-- Create a row container that collects children's renders.
    Children are laid out horizontally with the specified gap. -/
def row' (gap : Float := 0) (style : Afferent.Arbor.BoxStyle := {})
    (children : WidgetM α) : WidgetM α := do
  let (result, childRenders) ← runWidgetChildren children
  emit do
    let widgets ← childRenders.mapM id
    pure (Afferent.Arbor.row (gap := gap) (style := style) widgets)
  pure result

/-- Create a flex row with custom properties. -/
def flexRow' (props : Trellis.FlexContainer) (style : Afferent.Arbor.BoxStyle := {})
    (children : WidgetM α) : WidgetM α := do
  let (result, childRenders) ← runWidgetChildren children
  emit do
    let widgets ← childRenders.mapM id
    pure (Afferent.Arbor.flexRow props (style := style) widgets)
  pure result

/-- Create a flex column with custom properties. -/
def flexColumn' (props : Trellis.FlexContainer) (style : Afferent.Arbor.BoxStyle := {})
    (children : WidgetM α) : WidgetM α := do
  let (result, childRenders) ← runWidgetChildren children
  emit do
    let widgets ← childRenders.mapM id
    pure (Afferent.Arbor.flexColumn props (style := style) widgets)
  pure result

/-- Create a titled panel container. -/
def titledPanel' (title : String) (variant : PanelVariant)
    (children : WidgetM α) : WidgetM α := do
  let theme ← getThemeW
  let (result, childRenders) ← runWidgetChildren children
  emit do
    let widgets ← childRenders.mapM id
    let content := Afferent.Arbor.column (gap := 0) (style := {}) widgets
    pure (titledPanel title variant theme content)
  pure result

/-- Create an elevated panel container. -/
def elevatedPanel' (padding : Float := 16)
    (children : WidgetM α) : WidgetM α := do
  let theme ← getThemeW
  let (result, childRenders) ← runWidgetChildren children
  emit do
    let widgets ← childRenders.mapM id
    let content := Afferent.Arbor.column (gap := 0) (style := {}) widgets
    pure (elevatedPanel theme padding content)
  pure result

/-- Create an outlined panel container. -/
def outlinedPanel' (padding : Float := 16)
    (children : WidgetM α) : WidgetM α := do
  let theme ← getThemeW
  let (result, childRenders) ← runWidgetChildren children
  emit do
    let widgets ← childRenders.mapM id
    let content := Afferent.Arbor.column (gap := 0) (style := {}) widgets
    pure (outlinedPanel theme padding content)
  pure result

/-- Create a filled panel container. -/
def filledPanel' (padding : Float := 16)
    (children : WidgetM α) : WidgetM α := do
  let theme ← getThemeW
  let (result, childRenders) ← runWidgetChildren children
  emit do
    let widgets ← childRenders.mapM id
    let content := Afferent.Arbor.column (gap := 0) (style := {}) widgets
    pure (filledPanel theme padding content)
  pure result

/-! ## WidgetM Static Widget Emitters

These emit visual-only widgets without returning events or dynamics.
-/

/-- Emit a heading1 label. -/
def heading1' (text : String) : WidgetM Unit := do
  let theme ← getThemeW
  emit (pure (heading1 text theme))

/-- Emit a heading2 label. -/
def heading2' (text : String) : WidgetM Unit := do
  let theme ← getThemeW
  emit (pure (heading2 text theme))

/-- Emit a heading3 label. -/
def heading3' (text : String) : WidgetM Unit := do
  let theme ← getThemeW
  emit (pure (heading3 text theme))

/-- Emit body text. -/
def bodyText' (text : String) : WidgetM Unit := do
  let theme ← getThemeW
  emit (pure (bodyText text theme))

/-- Emit caption text. -/
def caption' (text : String) : WidgetM Unit := do
  let theme ← getThemeW
  emit (pure (caption text theme))

/-- Emit a spacer. -/
def spacer' (width height : Float) : WidgetM Unit := do
  emit (pure (Afferent.Arbor.spacer width height))

/-! ## WidgetM Conditional Rendering -/

/-- Emit a widget only when condition is true (sampled at render time). -/
def when' (condition : Dynamic Spider Bool) (content : WidgetM Unit) : WidgetM Unit := do
  let (_, childRenders) ← runWidgetChildren content
  emit do
    let visible ← condition.sample
    if visible then
      let widgets ← childRenders.mapM id
      pure (Afferent.Arbor.column (gap := 0) (style := {}) widgets)
    else
      pure (Afferent.Arbor.spacer 0 0)

/-! ## Dynamic Widget Subtrees

`dynWidget` enables rebuilding entire widget subtrees when a Dynamic value changes.
This is similar to Reflex's `dyn` combinator.
-/

/-- Typeclass to optimize dynWidget result tracking.
    For Unit results (common in animation widgets), we skip creating trigger events
    and firing results entirely, giving a significant performance boost.
    For other types, we track results normally. -/
class DynWidgetResult (b : Type) where
  /-- Create result tracking infrastructure. For Unit, returns a constant Dynamic and no-op fire. -/
  createTracking : b → WidgetM (Dynamic Spider b × (b → IO Unit))
  /-- Fire result update. For Unit, this is a no-op. -/
  maybeFire : (b → IO Unit) → b → IO Unit

instance : DynWidgetResult Unit where
  createTracking _ := do
    -- No trigger event, no holdDyn - just a constant Dynamic
    let dyn ← Dynamic.pureM ()
    pure (dyn, fun _ => pure ())
  maybeFire _ _ := pure ()

instance (priority := low) : DynWidgetResult b where
  createTracking initial := do
    let (trigger, fire) ← Reactive.newTriggerEvent
    let dyn ← Reactive.holdDyn initial trigger
    pure (dyn, fire)
  maybeFire fire val := fire val

/-- Run a dynamic widget computation. When the input Dynamic changes, the widget
    builder is re-run with the new value, rebuilding the subtree with fresh
    reactive subscriptions.

    Similar to Reflex's `dyn`, but takes a builder function to avoid BEq constraints.

    **Performance optimizations**:
    1. For Unit-returning builders (common in animations), result tracking is skipped
       entirely via typeclass specialization - no trigger event, no holdDyn, no fireResult.
    2. After the initial build, dynWidget checks whether the builder created any
       subscriptions. If not (pure/animation builders), it uses a fast path that
       skips scope management on rebuilds.

    Example (dependent dropdowns):
    ```
    let catResult ← dropdown categories theme 0
    let _ ← dynWidget catResult.selection fun catIdx =>
      dropdown (itemsForCategory catIdx) theme 0
    ```
-/
def dynWidget [DynWidgetResult b] (dynValue : Dynamic Spider a) (builder : a → WidgetM b)
    : WidgetM (Dynamic Spider b) := do
  let events ← getEventsW  -- Capture ReactiveEvents context

  -- All scope and initial build logic in one SpiderM block to access env.currentScope
  let (initialResult, childScopeRef, rendersRef, generationRef, needsScopeManagementRef) ← (⟨fun env => do
    -- Create child scope for builder subscriptions (enables cleanup on rebuild)
    let initialChildScope ← env.currentScope.child
    let scopeRef : IO.Ref Reactive.SubscriptionScope ← IO.mkRef initialChildScope

    -- Cache generation counter - incremented on each rebuild to invalidate cached commands
    let genRef : IO.Ref Nat ← IO.mkRef 0

    -- Initial build in child scope
    let initialValue ← dynValue.sample
    let widgetM := runWidgetChildren (builder initialValue)
    let reactiveM := widgetM.run { children := #[] }
    let spiderM := reactiveM.run events
    let ((result, renders), _) ← spiderM.run { env with currentScope := initialChildScope }

    -- Refs for current state
    let renRef : IO.Ref (Array ComponentRender) ← IO.mkRef renders

    -- Check if builder created any subscriptions - if not, we can use fast path
    let isPure ← initialChildScope.isEmpty
    let needsScopeRef ← IO.mkRef (!isPure)

    pure (result, scopeRef, renRef, genRef, needsScopeRef)
  ⟩ : SpiderM (b × IO.Ref Reactive.SubscriptionScope × IO.Ref (Array ComponentRender) × IO.Ref Nat × IO.Ref Bool))

  -- Result tracking via typeclass - Unit skips this entirely
  let (resultDyn, fireResult) ← DynWidgetResult.createTracking initialResult

  -- Subscribe to rebuilds when dynValue changes
  let subscribeAction : SpiderM Unit := ⟨fun env => do
    let unsub ← Reactive.Event.subscribe dynValue.updated fun newValue => do
      let rebuild := do
        let needsScopeManagement ← needsScopeManagementRef.get

        if needsScopeManagement then
          -- Full path: clear scope before rebuild
          let childScope ← childScopeRef.get
          childScope.clear

          generationRef.modify (· + 1)

          let widgetM := runWidgetChildren (builder newValue)
          let reactiveM := widgetM.run { children := #[] }
          let spiderM := reactiveM.run events
          let ((result, renders), _) ← spiderM.run { env with currentScope := childScope }
          rendersRef.set renders
          DynWidgetResult.maybeFire fireResult result
        else
          -- Fast path: no scope clearing needed
          generationRef.modify (· + 1)

          let childScope ← childScopeRef.get
          let widgetM := runWidgetChildren (builder newValue)
          let reactiveM := widgetM.run { children := #[] }
          let spiderM := reactiveM.run events
          let ((result, renders), _) ← spiderM.run { env with currentScope := childScope }
          rendersRef.set renders
          DynWidgetResult.maybeFire fireResult result

          -- Safety check: if subscriptions were created, switch to full mode permanently
          let stillEmpty ← childScope.isEmpty
          if !stillEmpty then
            needsScopeManagementRef.set true

      match ← dynWidgetMetricsRef.get with
      | none => rebuild
      | some metrics => do
          let t0 ← IO.monoNanosNow
          rebuild
          let t1 ← IO.monoNanosNow
          metrics.rebuildNanos.modify (· + (t1 - t0))
          metrics.rebuildCount.modify (· + 1)

    env.currentScope.register unsub⟩
  subscribeAction  -- Lift SpiderM to WidgetM via MonadLift

  -- Emit render that uses current renders with proper cache generation
  emit do
    let renders ← rendersRef.get
    let gen ← generationRef.get
    -- Helper to wrap a builder with the current cache generation
    let withGen (b : Afferent.Arbor.WidgetBuilder) : Afferent.Arbor.WidgetBuilder := do
      modify fun s => { s with cacheGeneration := gen }
      b
    if renders.isEmpty then
      pure (Afferent.Arbor.spacer 0 0)
    else if h : renders.size = 1 then
      let builder ← renders[0]
      pure (withGen builder)
    else
      let builders ← renders.mapM id
      pure (withGen (Afferent.Arbor.column (gap := 0) (style := {}) builders))

  pure resultDyn

end Afferent.Canopy.Reactive

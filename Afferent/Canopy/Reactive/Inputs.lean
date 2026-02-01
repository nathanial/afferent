/-
  Canopy Reactive - Input Infrastructure
  Creates trigger events that the demo loop fires when FFI events occur.
-/
import Reactive
import Std.Data.HashMap
import Afferent.Canopy.Reactive.Types
import Afferent.Canopy.Theme
import Afferent.Text.Measurer

open Reactive Reactive.Host

namespace Afferent.Canopy.Reactive

/-- Trigger functions to fire from the application loop when FFI events occur. -/
structure ReactiveInputs where
  /-- Fire when a click event occurs (mouse down). -/
  fireClick : ClickData → IO Unit
  /-- Fire when mouse button is released. -/
  fireMouseUp : MouseButtonData → IO Unit
  /-- Fire when mouse position changes (hover). -/
  fireHover : HoverData → IO Unit
  /-- Fire when mouse delta changes (relative movement since last frame). -/
  fireMouseDelta : MouseDeltaData → IO Unit
  /-- Fire when a key is pressed. -/
  fireKey : KeyData → IO Unit
  /-- Fire each frame with delta time (for animations). -/
  fireAnimationFrame : Float → IO Unit
  /-- Fire when scroll wheel is used. -/
  fireScroll : ScrollData → IO Unit

/-- Registry for auto-generating widget names and tracking component categories. -/
structure ComponentRegistry where
  private mk ::
  /-- Counter for generating unique IDs. -/
  idCounter : IO.Ref Nat
  /-- Names of focusable input widgets. -/
  inputNames : IO.Ref (Array String)
  /-- Names of all interactive widgets. -/
  interactiveNames : IO.Ref (Array String)
  /-- Currently focused input (by auto-generated name). -/
  focusedInput : Dynamic Spider (Option String)
  /-- Trigger to change focus. -/
  fireFocus : Option String → IO Unit

/-- Create a new component registry. -/
def ComponentRegistry.create : SpiderM ComponentRegistry := do
  let idCounter ← SpiderM.liftIO <| IO.mkRef 0
  let inputNames ← SpiderM.liftIO <| IO.mkRef #[]
  let interactiveNames ← SpiderM.liftIO <| IO.mkRef #[]
  let (focusEvent, fireFocus) ← newTriggerEvent (t := Spider) (a := Option String)
  let focusedInput ← holdDyn none focusEvent
  pure { idCounter, inputNames, interactiveNames, focusedInput, fireFocus }

/-- Reset the registry for a new frame.
    Clears the counter and name arrays to prevent unbounded growth. -/
def ComponentRegistry.reset (reg : ComponentRegistry) : IO Unit := do
  reg.idCounter.set 0
  reg.inputNames.set #[]
  reg.interactiveNames.set #[]

/-- Get diagnostic stats from the registry. -/
def ComponentRegistry.getStats (reg : ComponentRegistry) : IO (Nat × Nat × Nat) := do
  let counter ← reg.idCounter.get
  let inputCount ← reg.inputNames.get
  let interactiveCount ← reg.interactiveNames.get
  pure (counter, inputCount.size, interactiveCount.size)

/-- Register a component and get an auto-generated name.
    - `namePrefix`: Component type prefix (e.g., "button", "text-input")
    - `isInput`: Whether this is a focusable input widget
    - `isInteractive`: Whether this widget responds to clicks -/
def ComponentRegistry.register (reg : ComponentRegistry) (namePrefix : String)
    (isInput : Bool := false) (isInteractive : Bool := true) : IO String := do
  let id ← reg.idCounter.modifyGet fun n => (n, n + 1)
  let name := s!"{namePrefix}-{id}"
  if isInput then
    reg.inputNames.modify (·.push name)
  if isInteractive then
    reg.interactiveNames.modify (·.push name)
  pure name

/-- Global reactive event streams that widgets subscribe to. -/
structure ReactiveEvents where
  /-- Click events with layout context (mouse down). -/
  clickEvent : Event Spider ClickData
  /-- Mouse up events with layout context. -/
  mouseUpEvent : Event Spider MouseButtonData
  /-- Hover events with position and layout context. -/
  hoverEvent : Event Spider HoverData
  /-- Mouse delta events (relative movement since last frame). -/
  mouseDeltaEvent : Event Spider MouseDeltaData
  /-- Hover state changes by widget name (only changed keys fire). -/
  hoverFan : Event.Fan Spider String Bool
  /-- Keyboard events. -/
  keyEvent : Event Spider KeyData
  /-- Animation frame events (fires each frame with dt).
      Use for widgets that need delta time (e.g., physics, hover delay tracking). -/
  animationFrame : Event Spider Float
  /-- Shared elapsed time (seconds since app start, accumulated from animation frames).
      Use for continuous animations - all widgets share this single Dynamic. -/
  elapsedTime : Dynamic Spider Float
  /-- Scroll events with layout context. -/
  scrollEvent : Event Spider ScrollData
  /-- Component registry for auto-generating names. -/
  registry : ComponentRegistry
  /-- Font registry for text measurement. -/
  fontRegistry : Afferent.FontRegistry
  /-- Theme for widget styling. -/
  theme : Canopy.Theme
  /-- Default font for text input widgets (optional for testing). -/
  font : Option Afferent.Font := none

private def hoverChangedByName (data : HoverData) (name : String) : Bool :=
  match data.nameMap.get? name with
  | some wid => data.hitPath.any (· == wid)
  | none => false

private def buildHoverChangeEvent (hoverEvent : Event Spider HoverData) (registry : ComponentRegistry)
    : SpiderM (Event Spider (Std.HashMap String Bool)) := do
  let nodeId ← SpiderM.freshNodeId
  let derived ← SpiderM.liftIO <|
    Reactive.Event.newNodeWithId (t := Spider) nodeId (hoverEvent.height.inc)
  let stateRef ← SpiderM.liftIO <| IO.mkRef (∅ : Std.HashMap String Bool)
  let _ ← Reactive.Host.Event.subscribeM hoverEvent fun data => do
    let names ← registry.interactiveNames.get
    if names.isEmpty then
      pure ()
    else
      let prev ← stateRef.get
      let mut next := prev
      let mut delta : Std.HashMap String Bool := {}
      for name in names do
        let hovered := hoverChangedByName data name
        let prevVal := prev.getD name false
        if hovered != prevVal then
          next := next.insert name hovered
          delta := delta.insert name hovered
      if !delta.isEmpty then
        stateRef.set next
        Reactive.Event.fire derived delta
  pure derived

/-- Reset the component registry for a new frame.
    Call this at the start of each frame to prevent memory leaks from
    unbounded growth of component names and IDs. -/
def ReactiveEvents.resetRegistry (events : ReactiveEvents) : IO Unit :=
  events.registry.reset

/-- Get diagnostic stats: (idCounter, inputNames.size, interactiveNames.size). -/
def ReactiveEvents.getRegistryStats (events : ReactiveEvents) : IO (Nat × Nat × Nat) :=
  events.registry.getStats

/-- Create the reactive input infrastructure.
    Returns both the event streams (for subscriptions) and triggers (for firing).
    - `fontRegistry`: Registry for text measurement
    - `theme`: Theme for widget styling (default: dark)
    - `font`: Default font for text input widgets (optional for testing) -/
def createInputs (fontRegistry : Afferent.FontRegistry) (theme : Canopy.Theme := Canopy.Theme.dark)
    (font : Option Afferent.Font := none) : SpiderM (ReactiveEvents × ReactiveInputs) := do
  let (clickEvent, fireClick) ← newTriggerEvent (t := Spider) (a := ClickData)
  let (mouseUpEvent, fireMouseUp) ← newTriggerEvent (t := Spider) (a := MouseButtonData)
  let (hoverEvent, fireHover) ← newTriggerEvent (t := Spider) (a := HoverData)
  let (mouseDeltaEvent, fireMouseDelta) ← newTriggerEvent (t := Spider) (a := MouseDeltaData)
  let (keyEvent, fireKey) ← newTriggerEvent (t := Spider) (a := KeyData)
  let (animFrameEvent, fireAnimFrame) ← newTriggerEvent (t := Spider) (a := Float)
  let (scrollEvent, fireScroll) ← newTriggerEvent (t := Spider) (a := ScrollData)
  let registry ← ComponentRegistry.create

  -- Create a SINGLE shared Dynamic for elapsed time that all widgets use
  let elapsedTime ← foldDyn (fun dt acc => acc + dt) 0.0 animFrameEvent
  let hoverChanges ← buildHoverChangeEvent hoverEvent registry
  let hoverFan ← Event.fanM hoverChanges

  let events : ReactiveEvents := {
    clickEvent := clickEvent
    mouseUpEvent := mouseUpEvent
    hoverEvent := hoverEvent
    mouseDeltaEvent := mouseDeltaEvent
    hoverFan := hoverFan
    keyEvent := keyEvent
    animationFrame := animFrameEvent
    elapsedTime := elapsedTime
    scrollEvent := scrollEvent
    registry := registry
    fontRegistry := fontRegistry
    theme := theme
    font := font
  }
  let inputs : ReactiveInputs := {
    fireClick := fireClick
    fireMouseUp := fireMouseUp
    fireHover := fireHover
    fireMouseDelta := fireMouseDelta
    fireKey := fireKey
    fireAnimationFrame := fireAnimFrame
    fireScroll := fireScroll
  }
  pure (events, inputs)

end Afferent.Canopy.Reactive

/-
  Afferent Widget Application
  Elm-style message-passing architecture for interactive widget applications.
-/
import Afferent.Widget.Core
import Afferent.Widget.Event
import Afferent.Widget.HitTest
import Afferent.Layout.Result
import Afferent.FFI.Metal

namespace Afferent.Widget

/-- Event handler function type.
    Given an event, optionally produces a message. -/
def EventHandler (Msg : Type) := Event → Option Msg

/-- Collection of event handlers for a widget tree.
    Maps widget IDs to their handlers. -/
structure EventHandlers (Msg : Type) where
  handlers : Array (WidgetId × EventHandler Msg)
deriving Inhabited

namespace EventHandlers

def empty : EventHandlers Msg := { handlers := #[] }

/-- Add a handler for a widget. -/
def add (eh : EventHandlers Msg) (widgetId : WidgetId) (handler : EventHandler Msg) : EventHandlers Msg :=
  { handlers := eh.handlers.push (widgetId, handler) }

/-- Get handler for a widget. -/
def get (eh : EventHandlers Msg) (widgetId : WidgetId) : Option (EventHandler Msg) :=
  eh.handlers.find? (·.1 == widgetId) |>.map (·.2)

/-- Check if a widget has any handlers. -/
def hasHandler (eh : EventHandlers Msg) (widgetId : WidgetId) : Bool :=
  eh.handlers.any (·.1 == widgetId)

/-- Merge two handler collections. -/
def merge (a b : EventHandlers Msg) : EventHandlers Msg :=
  { handlers := a.handlers ++ b.handlers }

end EventHandlers

/-- Widget with attached event handlers. -/
structure InteractiveWidget (Msg : Type) where
  widget : Widget
  handlers : EventHandlers Msg
deriving Inhabited

/-- Result of processing an event through the event system. -/
structure EventProcessingResult (Msg : Type) where
  /-- Messages produced (may be multiple from bubbling). -/
  messages : Array Msg
  /-- Whether any handler stopped propagation. -/
  propagationStopped : Bool
deriving Inhabited

/-- Process an event through the widget tree with bubbling.
    Events bubble from target to root. First handler that produces a message stops propagation. -/
def processEvent (handlers : EventHandlers Msg) (event : Event)
    (targetPath : Array WidgetId) : EventProcessingResult Msg := Id.run do
  let mut messages : Array Msg := #[]
  let mut stopped := false

  -- Should this event bubble?
  if !event.shouldBubble then
    -- Non-bubbling: only deliver to target (last in path)
    if let some targetId := targetPath.back? then
      if let some handler := handlers.get targetId then
        if let some msg := handler event then
          messages := messages.push msg
    return { messages, propagationStopped := true }

  -- Bubbling: process handlers from target to root
  for i in [:targetPath.size] do
    if stopped then break
    let widgetId := targetPath[targetPath.size - 1 - i]!

    if let some handler := handlers.get widgetId then
      if let some msg := handler event then
        messages := messages.push msg
        stopped := true  -- First message stops propagation

  { messages, propagationStopped := stopped }

/-- Application state container. -/
structure AppState (Model Msg : Type) where
  /-- User model state. -/
  model : Model
  /-- Currently hovered widget (for mouseEnter/Leave). -/
  hoveredWidget : Option WidgetId := none
  /-- Previous mouse button bitmask (for edge-triggered click fallback). -/
  mouseButtons : UInt8 := 0
deriving Inhabited

/-- Application definition (like Elm's Browser.element). -/
structure App (Model Msg : Type) where
  /-- Initial model. -/
  init : Model
  /-- Update function: message → model → model. -/
  update : Msg → Model → Model
  /-- View function: model → widget tree with handlers. -/
  view : Model → InteractiveWidget Msg

/-- Input state collected each frame. -/
structure InputState where
  mousePos : Float × Float
  mouseButtons : UInt8
  modifiers : UInt16
  click : Option FFI.ClickEvent
  scrollDelta : Float × Float
  mouseInWindow : Bool
  keyCode : UInt16
deriving Repr, Inhabited

namespace InputState

/-- Collect current input state from window. -/
def collect (window : FFI.Window) : IO InputState := do
  let mousePos ← FFI.Window.getMousePos window
  let mouseButtons ← FFI.Window.getMouseButtons window
  let modifiers ← FFI.Window.getModifiers window
  let click ← FFI.Window.getClick window
  let scrollDelta ← FFI.Window.getScrollDelta window
  let mouseInWindow ← FFI.Window.mouseInWindow window
  let keyCode ← FFI.Window.getKeyCode window
  pure { mousePos, mouseButtons, modifiers, click, scrollDelta, mouseInWindow, keyCode }

/-- Clear consumed input state. -/
def clear (window : FFI.Window) : IO Unit := do
  FFI.Window.clearClick window
  FFI.Window.clearScroll window
  FFI.Window.clearKey window

end InputState

/-- Main event loop runner for applications. -/
structure AppRunner (Model Msg : Type) where
  app : App Model Msg
  stateRef : IO.Ref (AppState Model Msg)

namespace AppRunner

/-- Create a new app runner. -/
def create (app : App Model Msg) : IO (AppRunner Model Msg) := do
  let stateRef ← IO.mkRef {
    model := app.init
    hoveredWidget := none
    mouseButtons := 0
  }
  pure { app, stateRef }

/-- Get current model. -/
def getModel (runner : AppRunner Model Msg) : IO Model := do
  let state ← runner.stateRef.get
  pure state.model

/-- Update model with a message. -/
def sendMessage (runner : AppRunner Model Msg) (msg : Msg) : IO Unit := do
  let state ← runner.stateRef.get
  let newModel := runner.app.update msg state.model
  runner.stateRef.set { state with model := newModel }

/-- Get the current view (widget tree with handlers). -/
def getView (runner : AppRunner Model Msg) : IO (InteractiveWidget Msg) := do
  let state ← runner.stateRef.get
  pure (runner.app.view state.model)

/-- Process input events and update model.
    Returns the messages that were processed. -/
def processInput (runner : AppRunner Model Msg)
    (widget : Widget) (layouts : Layout.LayoutResult)
    (handlers : EventHandlers Msg) (input : InputState) : IO (Array Msg) := do
  let state ← runner.stateRef.get
  let mods := Modifiers.fromBitmask input.modifiers
  let mut allMessages : Array Msg := #[]
  let mut newHovered := state.hoveredWidget
  let prevButtons := state.mouseButtons

  -- Process click event
  let mut clickEvents : Array FFI.ClickEvent := #[]

  -- Prefer native click event if available
  if let some c := input.click then
    clickEvents := clickEvents.push c
  else
    -- Fallback: synthesize a click on rising edges of mouseButtons.
    -- Some platforms/backends may not populate `getClick`, but do provide button state.
    let mkSynthetic (buttonCode : UInt8) : FFI.ClickEvent := {
      button := buttonCode
      x := input.mousePos.1
      y := input.mousePos.2
      modifiers := input.modifiers
    }

    let leftNew := (input.mouseButtons &&& 1) != 0 && (prevButtons &&& 1) == 0
    let rightNew := (input.mouseButtons &&& 2) != 0 && (prevButtons &&& 2) == 0
    let middleNew := (input.mouseButtons &&& 4) != 0 && (prevButtons &&& 4) == 0

    if leftNew then
      clickEvents := clickEvents.push (mkSynthetic 0)
    if rightNew then
      clickEvents := clickEvents.push (mkSynthetic 1)
    if middleNew then
      clickEvents := clickEvents.push (mkSynthetic 2)

  for c in clickEvents do
    let clickEvent : MouseEvent := {
      x := c.x
      y := c.y
      button := MouseButton.fromCode c.button
      modifiers := Modifiers.fromBitmask c.modifiers
    }
    let path := hitTestPath widget layouts c.x c.y
    let result := processEvent handlers (.mouseClick clickEvent) path
    allMessages := allMessages ++ result.messages

  -- Process scroll event
  if input.scrollDelta.1 != 0 || input.scrollDelta.2 != 0 then
    let scrollEvent : ScrollEvent := {
      x := input.mousePos.1
      y := input.mousePos.2
      deltaX := input.scrollDelta.1
      deltaY := input.scrollDelta.2
      modifiers := mods
    }
    let path := hitTestPath widget layouts input.mousePos.1 input.mousePos.2
    let result := processEvent handlers (.scroll scrollEvent) path
    allMessages := allMessages ++ result.messages

  -- Process hover state changes (mouseEnter/Leave)
  let currentHover := hitTestId widget layouts input.mousePos.1 input.mousePos.2
  if currentHover != state.hoveredWidget then
    let hoverEvent : MouseEvent := {
      x := input.mousePos.1
      y := input.mousePos.2
      modifiers := mods
    }

    -- Mouse leave old widget
    if let some oldId := state.hoveredWidget then
      if let some handler := handlers.get oldId then
        if let some msg := handler (.mouseLeave hoverEvent) then
          allMessages := allMessages.push msg

    -- Mouse enter new widget
    if let some newId := currentHover then
      if let some handler := handlers.get newId then
        if let some msg := handler (.mouseEnter hoverEvent) then
          allMessages := allMessages.push msg

    newHovered := currentHover

  -- Process keyboard events
  if input.keyCode != 0 then
    let keyEvent : KeyEvent := {
      key := Key.fromKeyCode input.keyCode
      modifiers := mods
      isPress := true
    }
    -- Keyboard events go to focused widget (not implemented yet) or broadcast
    -- For now, just include in messages if any handler wants it
    let result := processEvent handlers (.keyPress keyEvent) #[]
    allMessages := allMessages ++ result.messages

  -- Apply all messages to model
  for msg in allMessages do
    let currentState ← runner.stateRef.get
    let newModel := runner.app.update msg currentState.model
    runner.stateRef.set { currentState with model := newModel }

  -- Update hover state
  runner.stateRef.modify fun s => {
    s with hoveredWidget := newHovered
           mouseButtons := input.mouseButtons
  }

  pure allMessages

end AppRunner

end Afferent.Widget

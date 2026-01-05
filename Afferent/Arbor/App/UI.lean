/-
  Arbor UI Runtime
  Widget + handler registry, event dispatch, and UIBuilder convenience helpers.
-/
import Std
import Afferent.Arbor.Event.HitTest
import Afferent.Arbor.Event.Types
import Afferent.Arbor.Widget.DSL
import Trellis

namespace Afferent.Arbor

/-- Result of handling a single event at a widget. -/
structure EventResult (Msg : Type) where
  msgs : Array Msg := #[]
  /-- Capture subsequent pointer events (dragging). -/
  capture : Option WidgetId := none
  /-- Release pointer capture. -/
  releaseCapture : Bool := false
  /-- Stop bubbling further up the tree. -/
  stopPropagation : Bool := false

namespace EventResult

def map {Msg Msg' : Type} (f : Msg → Msg') (r : EventResult Msg) : EventResult Msg' :=
  { r with msgs := r.msgs.map f }

end EventResult

/-- Event handling context for a specific widget. -/
structure EventContext where
  widgetId : WidgetId
  layout : Trellis.ComputedLayout
  layouts : Trellis.LayoutResult
  path : Array WidgetId
  globalPos : Option Point := none
  localPos : Option Point := none
  contentPos : Option Point := none
  isCaptured : Bool := false

/-- Handler for events on a widget. -/
abbrev Handler (Msg : Type) := EventContext → Event → EventResult Msg

/-- Handler registry keyed by widget id. -/
structure HandlerRegistry (Msg : Type) where
  handlers : Std.HashMap WidgetId (Handler Msg) := {}

namespace HandlerRegistry

def empty (Msg : Type) : HandlerRegistry Msg := {}

def insert {Msg : Type} (id : WidgetId) (h : Handler Msg) (reg : HandlerRegistry Msg) : HandlerRegistry Msg :=
  { reg with handlers := reg.handlers.insert id h }

def merge {Msg : Type} (a b : HandlerRegistry Msg) : HandlerRegistry Msg :=
  { handlers := b.handlers.fold (init := a.handlers) (fun acc id h => acc.insert id h) }

def map {Msg Msg' : Type} (f : Msg → Msg') (reg : HandlerRegistry Msg) : HandlerRegistry Msg' :=
  { handlers :=
      reg.handlers.fold (init := {}) (fun acc id h =>
        acc.insert id (fun ctx ev => (h ctx ev).map f)) }

end HandlerRegistry

/-- UI bundle: widget tree + handler registry. -/
structure UI (Msg : Type) where
  widget : Widget
  handlers : HandlerRegistry Msg

namespace UI

def map {Msg Msg' : Type} (f : Msg → Msg') (ui : UI Msg) : UI Msg' :=
  { widget := ui.widget, handlers := ui.handlers.map f }

def withHandlers {Msg : Type} (ui : UI Msg) (handlers : HandlerRegistry Msg) : UI Msg :=
  { ui with handlers := ui.handlers.merge handlers }

end UI

/-- Pointer capture state (for dragging). -/
structure CaptureState where
  captured : Option WidgetId := none
deriving Repr

namespace CaptureState

def empty : CaptureState := {}

end CaptureState

/-- Extract pointer position from an event when available. -/
def eventPosition? : Event → Option Point
  | .mouseClick e => some ⟨e.x, e.y⟩
  | .mouseDown e => some ⟨e.x, e.y⟩
  | .mouseUp e => some ⟨e.x, e.y⟩
  | .mouseMove e => some ⟨e.x, e.y⟩
  | .mouseEnter e => some ⟨e.x, e.y⟩
  | .mouseLeave e => some ⟨e.x, e.y⟩
  | .scroll e => some ⟨e.x, e.y⟩
  | .keyPress _ => none
  | .keyRelease _ => none

private def contextFor (id : WidgetId) (path : Array WidgetId)
    (layouts : Trellis.LayoutResult) (globalPos : Option Point) (isCaptured : Bool) : Option EventContext := do
  let layout ← layouts.get id
  let localPos := globalPos.map fun p =>
    ⟨p.x - layout.borderRect.x, p.y - layout.borderRect.y⟩
  let contentPos := globalPos.map fun p =>
    ⟨p.x - layout.contentRect.x, p.y - layout.contentRect.y⟩
  pure {
    widgetId := id
    layout
    layouts
    path
    globalPos
    localPos
    contentPos
    isCaptured
  }

/-- Dispatch a single event through the widget tree.
    Uses pointer capture when active; otherwise hit tests and bubbles. -/
def dispatchEvent {Msg : Type} (event : Event) (root : Widget) (layouts : Trellis.LayoutResult)
    (handlers : HandlerRegistry Msg) (capture : CaptureState := {})
    : CaptureState × Array Msg := Id.run do
  let mut msgs : Array Msg := #[]
  let mut captureState := capture
  let mut stop := false

  let globalPos := eventPosition? event

  match captureState.captured with
  | some capturedId =>
    if let some h := handlers.handlers[capturedId]? then
      match contextFor capturedId #[capturedId] layouts globalPos true with
      | some ctx =>
        let res := h ctx event
        msgs := msgs ++ res.msgs
        if let some cap := res.capture then
          captureState := { captureState with captured := some cap }
        if res.releaseCapture then
          captureState := { captureState with captured := none }
      | none => pure ()
  | none =>
    match globalPos with
    | none => pure ()
    | some p =>
      let path := hitTestPath root layouts p.x p.y
      if path.isEmpty then
        pure ()
      else
        let dispatchPath :=
          if Event.shouldBubble event then
            path.reverse
          else
            #[path.back!]
        for id in dispatchPath do
          if !stop then
            if let some h := handlers.handlers[id]? then
              match contextFor id path layouts globalPos false with
              | some ctx =>
                let res := h ctx event
                msgs := msgs ++ res.msgs
                if let some cap := res.capture then
                  captureState := { captureState with captured := some cap }
                if res.releaseCapture then
                  captureState := { captureState with captured := none }
                if res.stopPropagation then
                  stop := true
              | none => pure ()

  return (captureState, msgs)

/-! ## UI Builder -/

structure UIState (Msg : Type) extends BuilderState where
  handlers : HandlerRegistry Msg := {}

abbrev UIBuilder (Msg : Type) := StateM (UIState Msg)

namespace UIBuilder

def freshId {Msg : Type} : UIBuilder Msg WidgetId := do
  let s ← get
  set { s with nextId := s.nextId + 1 }
  pure s.nextId

def register {Msg : Type} (id : WidgetId) (handler : Handler Msg) : UIBuilder Msg Unit := do
  modify fun s => { s with handlers := s.handlers.insert id handler }

def lift {Msg : Type} (builder : WidgetBuilder) : UIBuilder Msg Widget := do
  let s ← get
  let (w, st) := builder.run { nextId := s.nextId }
  set { s with nextId := st.nextId }
  pure w

def build {Msg : Type} (builder : UIBuilder Msg Widget) : UI Msg :=
  let (widget, state) := builder.run {}
  { widget, handlers := state.handlers }

def buildFrom {Msg : Type} (startId : Nat) (builder : UIBuilder Msg Widget) : UI Msg :=
  let (widget, state) := builder.run { nextId := startId }
  { widget, handlers := state.handlers }

end UIBuilder

end Afferent.Arbor

/-
  Afferent Widget Interactive DSL
  Builder functions for widgets with event handlers.
-/
import Afferent.Widget.Core
import Afferent.Widget.DSL
import Afferent.Widget.Event
import Afferent.Widget.App


namespace Afferent.Widget

/-- Builder state extended with event handlers. -/
structure InteractiveBuilderState (Msg : Type) where
  widgetState : BuilderState
  handlers : EventHandlers Msg
deriving Inhabited

/-- Generate a fresh widget ID. -/
def freshIdI : StateM (InteractiveBuilderState Msg) WidgetId := do
  let s ← get
  let id := s.widgetState.nextId
  set { s with widgetState := { s.widgetState with nextId := id + 1 } }
  pure id

/-- Attach an event handler to a widget ID. -/
def attachHandlerI (widgetId : WidgetId) (handler : EventHandler Msg) :
    StateM (InteractiveBuilderState Msg) Unit := do
  let s ← get
  set { s with handlers := s.handlers.add widgetId handler }

/-- Convert a regular WidgetBuilder to interactive monad. -/
def liftBuilderI (b : WidgetBuilder) : StateM (InteractiveBuilderState Msg) Widget := do
  let s ← get
  let (widget, newWidgetState) := b.run s.widgetState
  set { s with widgetState := newWidgetState }
  pure widget

/-- Build an interactive widget tree. -/
def buildInteractive (builder : StateM (InteractiveBuilderState Msg) Widget) : InteractiveWidget Msg :=
  let (widget, state) := builder.run { widgetState := {}, handlers := EventHandlers.empty }
  { widget, handlers := state.handlers }

/-! ## Event Handler Combinators -/

/-- Attach a click handler to a widget. -/
def onClick (handler : MouseEvent → Msg)
    (child : StateM (InteractiveBuilderState Msg) Widget) : StateM (InteractiveBuilderState Msg) Widget := do
  let widget ← child
  attachHandlerI widget.id fun
    | .mouseClick e => some (handler e)
    | _ => none
  pure widget

/-- Attach hover handlers (enter and leave) to a widget. -/
def onHover (enterHandler : MouseEvent → Msg) (leaveHandler : MouseEvent → Msg)
    (child : StateM (InteractiveBuilderState Msg) Widget) : StateM (InteractiveBuilderState Msg) Widget := do
  let widget ← child
  attachHandlerI widget.id fun
    | .mouseEnter e => some (enterHandler e)
    | .mouseLeave e => some (leaveHandler e)
    | _ => none
  pure widget

/-- Attach a scroll handler to a widget. -/
def onScroll (handler : ScrollEvent → Msg)
    (child : StateM (InteractiveBuilderState Msg) Widget) : StateM (InteractiveBuilderState Msg) Widget := do
  let widget ← child
  attachHandlerI widget.id fun
    | .scroll e => some (handler e)
    | _ => none
  pure widget

/-- Attach a click handler that produces a constant message. -/
def onClickMsg (msg : Msg)
    (child : StateM (InteractiveBuilderState Msg) Widget) : StateM (InteractiveBuilderState Msg) Widget :=
  onClick (fun _ => msg) child

/-! ## Lifted DSL Functions -/

/-- Create a text widget. -/
def textI (content : String) (font : Font) (color : Color := Color.white)
    (align : TextAlign := .left) : StateM (InteractiveBuilderState Msg) Widget :=
  liftBuilderI (text' content font color align)

/-- Create a colored box. -/
def coloredBoxI (color : Color) (width height : Float) : StateM (InteractiveBuilderState Msg) Widget :=
  liftBuilderI (coloredBox color width height)

/-- Create a spacer. -/
def spacerI (width height : Float) : StateM (InteractiveBuilderState Msg) Widget :=
  liftBuilderI (spacer width height)

/-- Create a horizontal row from child widgets. -/
def rowI' (gap : Float := 0) (style : BoxStyle := {})
    (children : Array Widget) : StateM (InteractiveBuilderState Msg) Widget := do
  let wid ← freshIdI
  let props := Layout.FlexContainer.row gap
  pure (Widget.flex wid props style children)

/-- Create a vertical column from child widgets. -/
def columnI' (gap : Float := 0) (style : BoxStyle := {})
    (children : Array Widget) : StateM (InteractiveBuilderState Msg) Widget := do
  let wid ← freshIdI
  let props := Layout.FlexContainer.column gap
  pure (Widget.flex wid props style children)

/-- Create a centered container. -/
def centerI (style : BoxStyle := {})
    (child : StateM (InteractiveBuilderState Msg) Widget) : StateM (InteractiveBuilderState Msg) Widget := do
  let wid ← freshIdI
  let props := Layout.FlexContainer.centered
  let c ← child
  pure (Widget.flex wid props style #[c])

/-- Create a box with custom style. -/
def boxI (style : BoxStyle) : StateM (InteractiveBuilderState Msg) Widget := do
  let wid ← freshIdI
  pure (Widget.rect wid style)

/-! ## Convenience Widgets -/

/-- A button: colored box with click handler. -/
def buttonI (label : String) (font : Font) (bgColor : Color)
    (textColor : Color := Color.white) (msg : Msg) : StateM (InteractiveBuilderState Msg) Widget :=
  onClick (fun _ => msg) do
    let padding := Layout.EdgeInsets.symmetric 12 8
    let style : BoxStyle := { backgroundColor := some bgColor, padding, cornerRadius := 4 }
    centerI (style := style) (textI label font textColor .center)

/-- An icon button (just a colored box for now). -/
def iconButtonI (bgColor : Color) (size : Float) (msg : Msg) : StateM (InteractiveBuilderState Msg) Widget :=
  onClick (fun _ => msg) (coloredBoxI bgColor size size)

end Afferent.Widget

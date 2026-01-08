/-
  Canopy Core
  High-level widget framework built on Arbor.
-/
import Afferent.Arbor
import Std

namespace Afferent.Canopy

def version : String := "0.1.0"

/-- Interaction state for a single widget. -/
structure WidgetState where
  hovered : Bool := false
  focused : Bool := false
  pressed : Bool := false
  disabled : Bool := false
deriving Repr, BEq, Inhabited

/-- Global registry of widget states, keyed by widget name. -/
structure WidgetStates where
  states : Std.HashMap String WidgetState := {}
deriving Inhabited

namespace WidgetStates

/-- Create an empty widget states registry. -/
def empty : WidgetStates := {}

/-- Get the state for a widget by name. Returns default state if not found. -/
def get (ws : WidgetStates) (name : String) : WidgetState :=
  ws.states.getD name {}

/-- Set the state for a widget by name. -/
def set (ws : WidgetStates) (name : String) (s : WidgetState) : WidgetStates :=
  { ws with states := ws.states.insert name s }

/-- Update hovered state for a widget. -/
def setHovered (ws : WidgetStates) (name : String) (hovered : Bool) : WidgetStates :=
  let s := ws.get name
  ws.set name { s with hovered }

/-- Update focused state for a widget. -/
def setFocused (ws : WidgetStates) (name : String) (focused : Bool) : WidgetStates :=
  let s := ws.get name
  ws.set name { s with focused }

/-- Update pressed state for a widget. -/
def setPressed (ws : WidgetStates) (name : String) (pressed : Bool) : WidgetStates :=
  let s := ws.get name
  ws.set name { s with pressed }

/-- Update disabled state for a widget. -/
def setDisabled (ws : WidgetStates) (name : String) (disabled : Bool) : WidgetStates :=
  let s := ws.get name
  ws.set name { s with disabled }

/-- Clear focus from all widgets except the specified one. -/
def focusOnly (ws : WidgetStates) (name : String) : WidgetStates :=
  let clearedStates := ws.states.fold (init := (default : Std.HashMap String WidgetState)) fun acc k v =>
    acc.insert k { v with focused := k == name }
  { ws with states := clearedStates }

end WidgetStates

end Afferent.Canopy

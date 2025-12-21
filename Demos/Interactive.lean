/-
  Interactive Demo - Counter with buttons using Arbor
-/
import Afferent
import Afferent.Widget
import Arbor
import Trellis

open Afferent CanvasM
open Arbor
open Trellis (EdgeInsets)

namespace Demos

/-- Button widget IDs for hit testing. -/
structure CounterWidgetIds where
  increment : WidgetId
  decrement : WidgetId
  reset : WidgetId
deriving Repr

/-- Counter state. -/
structure CounterState where
  value : Int := 0
  widgetIds : Option CounterWidgetIds := none
deriving Repr

namespace CounterState

def initial : CounterState := {}

def increment (s : CounterState) : CounterState :=
  { s with value := s.value + 1 }

def decrement (s : CounterState) : CounterState :=
  { s with value := s.value - 1 }

def reset (s : CounterState) : CounterState :=
  { s with value := 0 }

end CounterState

/-- Build the counter UI widget tree.
    Returns (widget, widgetIds) where widgetIds contains the button IDs for hit testing. -/
def buildCounterWidget (fontId : FontId) (smallFontId : FontId) (counter : Int) (screenScale : Float)
    : Widget × CounterWidgetIds :=
  let s := fun (v : Float) => v * screenScale

  -- Build widget with explicit starting ID
  let widget := Arbor.buildFrom 0 do
    column (gap := s 30) (style := { backgroundColor := some (Color.gray 0.15), padding := EdgeInsets.uniform (s 40) }) #[
      -- Title (ID 1)
      text' "Interactive Counter Demo" fontId Color.white .center,

      -- Counter display (ID 2, inner text ID 3)
      center (style := { backgroundColor := some (Color.gray 0.25), minWidth := some (s 200), minHeight := some (s 80), cornerRadius := s 12 }) do
        text' (toString counter) fontId Color.white .center,

      -- Button row (ID 4)
      row (gap := s 20) {} #[
        -- Decrement button "-" (ID 5)
        center (style := {
          backgroundColor := some (Color.hsv 0.0 0.6 0.5)  -- Red
          minWidth := some (s 80)
          minHeight := some (s 50)
          cornerRadius := s 8
        }) do
          text' "-" fontId Color.white .center,

        -- Reset button (ID 7)
        center (style := {
          backgroundColor := some (Color.hsv 0.6 0.6 0.5)  -- Blue
          minWidth := some (s 100)
          minHeight := some (s 50)
          cornerRadius := s 8
        }) do
          text' "Reset" smallFontId Color.white .center,

        -- Increment button "+" (ID 9)
        center (style := {
          backgroundColor := some (Color.hsv 0.33 0.6 0.5)  -- Green
          minWidth := some (s 80)
          minHeight := some (s 50)
          cornerRadius := s 8
        }) do
          text' "+" fontId Color.white .center
      ],

      -- Instructions (ID 11)
      text' "Click the buttons to change the counter" smallFontId (Color.gray 0.6) .center
    ]

  -- Widget IDs based on tree structure:
  -- 0: column (root)
  -- 1: title text
  -- 2: counter display center
  -- 3: counter value text
  -- 4: button row
  -- 5: decrement center
  -- 6: decrement text
  -- 7: reset center
  -- 8: reset text
  -- 9: increment center
  -- 10: increment text
  -- 11: instructions text
  let ids : CounterWidgetIds := {
    decrement := 5
    reset := 7
    increment := 9
  }

  (widget, ids)

/-- Process a click event and update counter state. -/
def processClick (state : CounterState) (hitId : Option WidgetId) : CounterState :=
  match state.widgetIds, hitId with
  | some ids, some id =>
    -- Check if clicked on button or its text child
    if id == ids.increment || id == ids.increment + 1 then state.increment
    else if id == ids.decrement || id == ids.decrement + 1 then state.decrement
    else if id == ids.reset || id == ids.reset + 1 then state.reset
    else state
  | _, _ => state

/-- Render the interactive counter demo. -/
def renderInteractiveM (reg : FontRegistry) (fontId : FontId) (smallFontId : FontId)
    (width height : Float) (state : CounterState) (screenScale : Float := 1.0) : CanvasM Unit := do
  let (widget, _ids) := buildCounterWidget fontId smallFontId state.value screenScale
  Afferent.Widget.renderArborWidgetCentered reg widget width height

/-- Render the interactive counter demo with debug borders. -/
def renderInteractiveDebugM (reg : FontRegistry) (fontId : FontId) (smallFontId : FontId)
    (width height : Float) (state : CounterState) (screenScale : Float := 1.0) : CanvasM Unit := do
  let (widget, _ids) := buildCounterWidget fontId smallFontId state.value screenScale
  Afferent.Widget.renderArborWidgetDebug reg widget width height

/-- Measure and layout the counter widget for hit testing. -/
def prepareCounterForHitTest (reg : FontRegistry) (fontId : FontId) (smallFontId : FontId)
    (width height : Float) (state : CounterState) (screenScale : Float := 1.0)
    : IO (Widget × Trellis.LayoutResult × CounterWidgetIds × Float × Float) := do
  let (widget, ids) := buildCounterWidget fontId smallFontId state.value screenScale

  -- Measure widget
  let measureResult ← Afferent.runWithFonts reg (Arbor.measureWidget widget width height)
  let layoutNode := measureResult.node
  let measuredWidget := measureResult.widget

  -- Get intrinsic size for centering
  let (intrinsicWidth, intrinsicHeight) ← Afferent.runWithFonts reg (Arbor.intrinsicSize widget)

  -- Compute layout at intrinsic size
  let layouts := Trellis.layout layoutNode intrinsicWidth intrinsicHeight

  -- Calculate offset for centering
  let offsetX := (width - intrinsicWidth) / 2
  let offsetY := (height - intrinsicHeight) / 2

  pure (measuredWidget, layouts, ids, offsetX, offsetY)

/-- Perform hit test on the counter widget at the given screen coordinates. -/
def hitTestCounter (widget : Widget) (layouts : Trellis.LayoutResult)
    (offsetX offsetY : Float) (screenX screenY : Float) : Option WidgetId :=
  -- Adjust screen coordinates to widget-local coordinates
  let localX := screenX - offsetX
  let localY := screenY - offsetY
  Arbor.hitTestId widget layouts localX localY

end Demos

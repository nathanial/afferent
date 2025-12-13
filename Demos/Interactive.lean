/-
  Interactive Demo
  Demonstrates the event system with a simple counter application.
-/
import Afferent
import Afferent.Widget

open Afferent Afferent.Widget

/-- Messages for the counter application. -/
inductive CounterMsg where
  | increment
  | decrement
  | reset
deriving Repr

/-- Model for the counter application. -/
structure CounterModel where
  count : Int := 0
deriving Repr, Inhabited

/-- Counter application definition. -/
def counterApp (font : Font) : App CounterModel CounterMsg where
  init := {}

  update := fun msg model =>
    match msg with
    | .increment => { model with count := model.count + 1 }
    | .decrement => { model with count := model.count - 1 }
    | .reset => { model with count := 0 }

  view := fun model =>
    buildInteractive do
      let cardStyle : BoxStyle := {
        backgroundColor := some (Color.rgb 0.2 0.2 0.25)
        padding := Layout.EdgeInsets.uniform 24
        cornerRadius := 12
      }
      centerI (style := cardStyle) do
        columnI' (gap := 16) {} #[
          -- Title
          ← textI "Counter Demo" font Color.white .center,
          -- Count display
          ← textI s!"Count: {model.count}" font (Color.rgb 0.4 0.8 1.0) .center,
          -- Buttons row
          ← rowI' (gap := 12) {} #[
            ← buttonI "-" font (Color.rgb 0.8 0.3 0.3) Color.white .decrement,
            ← buttonI "Reset" font (Color.rgb 0.5 0.5 0.5) Color.white .reset,
            ← buttonI "+" font (Color.rgb 0.3 0.7 0.3) Color.white .increment
          ]
        ]

/-- Render the counter demo (for use within the main demo runner). -/
def renderInteractiveDemo (runner : AppRunner CounterModel CounterMsg)
    (width height : Float) : CanvasM Unit := do
  -- Get current view
  let interactive ← runner.getView
  -- Prepare UI with layout
  let prepared ← prepareUI interactive.widget width height
  -- Render
  renderPreparedUI prepared

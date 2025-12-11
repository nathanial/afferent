/-
  Afferent Canvas State
  Stateful drawing context with save/restore and transforms.
-/
import Afferent.Core.Types
import Afferent.Core.Path
import Afferent.Core.Transform
import Afferent.Core.Paint
import Collimator.Prelude

namespace Afferent

open Collimator

/-- Canvas drawing state that can be saved and restored. -/
structure CanvasState where
  /-- Current transformation matrix. -/
  transform : Transform
  /-- Current fill style. -/
  fillStyle : FillStyle
  /-- Current stroke style. -/
  strokeStyle : StrokeStyle
  /-- Global alpha (multiplied with style alphas). -/
  globalAlpha : Float
  /-- Clipping path (optional). -/
  clipPath : Option Path
deriving Repr

namespace CanvasState

/-- Default canvas state with identity transform and black fill/stroke. -/
def default : CanvasState :=
  { transform := Transform.identity
    fillStyle := .solid Color.black
    strokeStyle := StrokeStyle.default
    globalAlpha := 1.0
    clipPath := none }

instance : Inhabited CanvasState := ⟨default⟩

/-! ## Lenses for CanvasState fields -/

def transformLens : Lens' CanvasState Transform :=
  lens' (fun s => s.transform) (fun s t => { s with transform := t })

def fillStyleLens : Lens' CanvasState FillStyle :=
  lens' (fun s => s.fillStyle) (fun s f => { s with fillStyle := f })

def strokeStyleLens : Lens' CanvasState StrokeStyle :=
  lens' (fun s => s.strokeStyle) (fun s ss => { s with strokeStyle := ss })

def globalAlphaLens : Lens' CanvasState Float :=
  lens' (fun s => s.globalAlpha) (fun s a => { s with globalAlpha := a })

def clipPathLens : Lens' CanvasState (Option Path) :=
  lens' (fun s => s.clipPath) (fun s p => { s with clipPath := p })

/-! ## Transform operations -/

/-- Apply a translation to the current transform. -/
def translate (dx dy : Float) (state : CanvasState) : CanvasState :=
  { state with transform := state.transform.translated dx dy }

/-- Apply a rotation to the current transform (angle in radians). -/
def rotate (angle : Float) (state : CanvasState) : CanvasState :=
  { state with transform := state.transform.rotated angle }

/-- Apply a scale to the current transform. -/
def scale (sx sy : Float) (state : CanvasState) : CanvasState :=
  { state with transform := state.transform.scaled sx sy }

/-- Apply a uniform scale to the current transform. -/
def scaleUniform (s : Float) (state : CanvasState) : CanvasState :=
  scale s s state

/-- Set the transform to a specific value. -/
def setTransform (t : Transform) (state : CanvasState) : CanvasState :=
  { state with transform := t }

/-- Reset the transform to identity. -/
def resetTransform (state : CanvasState) : CanvasState :=
  { state with transform := Transform.identity }

/-! ## Style operations -/

/-- Set the fill color. -/
def setFillColor (c : Color) (state : CanvasState) : CanvasState :=
  { state with fillStyle := .solid c }

/-- Set the stroke color. -/
def setStrokeColor (c : Color) (state : CanvasState) : CanvasState :=
  { state with strokeStyle := { state.strokeStyle with color := c } }

/-- Set the stroke line width. -/
def setLineWidth (w : Float) (state : CanvasState) : CanvasState :=
  { state with strokeStyle := { state.strokeStyle with lineWidth := w } }

/-- Set the global alpha. -/
def setGlobalAlpha (a : Float) (state : CanvasState) : CanvasState :=
  { state with globalAlpha := a }

/-! ## Path transformation -/

/-- Transform a point by the current transform. -/
def transformPoint (state : CanvasState) (p : Point) : Point :=
  state.transform.apply p

/-- Transform an entire path by the current transform. -/
def transformPath (state : CanvasState) (path : Path) : Path :=
  let transformCmd : PathCommand → PathCommand
    | .moveTo p => .moveTo (state.transform.apply p)
    | .lineTo p => .lineTo (state.transform.apply p)
    | .quadraticCurveTo cp p =>
        .quadraticCurveTo (state.transform.apply cp) (state.transform.apply p)
    | .bezierCurveTo cp1 cp2 p =>
        .bezierCurveTo (state.transform.apply cp1) (state.transform.apply cp2) (state.transform.apply p)
    | .arcTo p1 p2 r =>
        .arcTo (state.transform.apply p1) (state.transform.apply p2) r  -- radius doesn't scale uniformly
    | .arc center r startAngle endAngle ccw =>
        .arc (state.transform.apply center) r startAngle endAngle ccw  -- needs proper transform handling
    | .rect rect =>
        -- Transform rectangle to path since rectangles don't transform well
        .rect { origin := state.transform.apply rect.origin, size := rect.size }
    | .closePath => .closePath
  { path with
    commands := path.commands.map transformCmd
    currentPoint := path.currentPoint.map state.transform.apply
    startPoint := path.startPoint.map state.transform.apply }

/-- Get the effective fill color with global alpha applied. -/
def effectiveFillColor (state : CanvasState) : Color :=
  let baseColor := state.fillStyle.toColor
  { baseColor with a := baseColor.a * state.globalAlpha }

/-- Get the effective stroke color with global alpha applied. -/
def effectiveStrokeColor (state : CanvasState) : Color :=
  let baseColor := state.strokeStyle.color
  { baseColor with a := baseColor.a * state.globalAlpha }

end CanvasState

/-- State stack for save/restore functionality. -/
structure StateStack where
  /-- Current active state. -/
  current : CanvasState
  /-- Stack of saved states (most recent first). -/
  saved : List CanvasState
deriving Repr, Inhabited

namespace StateStack

/-- Create a new state stack with default state. -/
def new : StateStack :=
  { current := CanvasState.default
    saved := [] }

/-- Save the current state to the stack. -/
def save (stack : StateStack) : StateStack :=
  { stack with saved := stack.current :: stack.saved }

/-- Restore the most recently saved state. -/
def restore (stack : StateStack) : StateStack :=
  match stack.saved with
  | [] => stack  -- Nothing to restore
  | s :: rest => { current := s, saved := rest }

/-- Get the current state. -/
def state (stack : StateStack) : CanvasState :=
  stack.current

/-- Modify the current state. -/
def modify (f : CanvasState → CanvasState) (stack : StateStack) : StateStack :=
  { stack with current := f stack.current }

/-- Set the current state. -/
def setState (s : CanvasState) (stack : StateStack) : StateStack :=
  { stack with current := s }

/-! ## Convenience functions that operate on the current state -/

def translate (dx dy : Float) : StateStack → StateStack :=
  modify (CanvasState.translate dx dy)

def rotate (angle : Float) : StateStack → StateStack :=
  modify (CanvasState.rotate angle)

def scale (sx sy : Float) : StateStack → StateStack :=
  modify (CanvasState.scale sx sy)

def scaleUniform (s : Float) : StateStack → StateStack :=
  modify (CanvasState.scaleUniform s)

def setFillColor (c : Color) : StateStack → StateStack :=
  modify (CanvasState.setFillColor c)

def setStrokeColor (c : Color) : StateStack → StateStack :=
  modify (CanvasState.setStrokeColor c)

def setLineWidth (w : Float) : StateStack → StateStack :=
  modify (CanvasState.setLineWidth w)

def setGlobalAlpha (a : Float) : StateStack → StateStack :=
  modify (CanvasState.setGlobalAlpha a)

def resetTransform : StateStack → StateStack :=
  modify CanvasState.resetTransform

/-! ## Lenses for StateStack -/

def currentLens : Lens' StateStack CanvasState :=
  lens' (fun s => s.current) (fun s c => { s with current := c })

def savedLens : Lens' StateStack (List CanvasState) :=
  lens' (fun s => s.saved) (fun s l => { s with saved := l })

/-- Composed lens to access transform through the stack. -/
def transformLens : Lens' StateStack Transform :=
  -- Manual composition since we need to go through two lenses
  lens'
    (fun s => s.current.transform)
    (fun s t => { s with current := { s.current with transform := t } })

/-- Composed lens to access fill style through the stack. -/
def fillStyleLens : Lens' StateStack FillStyle :=
  lens'
    (fun s => s.current.fillStyle)
    (fun s f => { s with current := { s.current with fillStyle := f } })

end StateStack

end Afferent

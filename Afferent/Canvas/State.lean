/-
  Afferent Canvas State
  Stateful drawing context with save/restore and transforms.
-/
import Afferent.Core.Types
import Afferent.Core.Path
import Afferent.Core.Transform
import Afferent.Core.Paint
import Afferent.Render.Tessellation

namespace Afferent

/-- Canvas drawing state that can be saved and restored. -/
structure CanvasState where
  /-- Current transformation matrix. -/
  transform : Transform
  /-- Base transform used by resetTransform. -/
  baseTransform : Transform
  /-- Current fill style. -/
  fillStyle : FillStyle
  /-- Current stroke style. -/
  strokeStyle : StrokeStyle
  /-- Global alpha (multiplied with style alphas). -/
  globalAlpha : Float
  /-- Stack of clip rectangles. Each rect is stored with the transform active
      when clip was called, allowing proper transform-aware clipping. -/
  clipStack : Array (Rect × Transform) := #[]
deriving Repr

namespace CanvasState

/-- Default canvas state with identity transform and black fill/stroke. -/
def default : CanvasState :=
  { transform := Transform.identity
    baseTransform := Transform.identity
    fillStyle := .solid Color.black
    strokeStyle := StrokeStyle.default
    globalAlpha := 1.0
    clipStack := #[] }

instance : Inhabited CanvasState := ⟨default⟩

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

/-- Set the base transform used by resetTransform (also updates current transform). -/
def setBaseTransform (t : Transform) (state : CanvasState) : CanvasState :=
  { state with baseTransform := t, transform := t }

/-- Reset the transform to the base transform. -/
def resetTransform (state : CanvasState) : CanvasState :=
  { state with transform := state.baseTransform }

/-! ## Style operations -/

/-- Set the fill color. -/
def setFillColor (c : Color) (state : CanvasState) : CanvasState :=
  { state with fillStyle := .solid c }

/-- Set the fill style (solid color or gradient). -/
def setFillStyle (style : FillStyle) (state : CanvasState) : CanvasState :=
  { state with fillStyle := style }

/-- Set the fill to a linear gradient. -/
def setFillLinearGradient (start finish : Point) (stops : Array GradientStop) (state : CanvasState) : CanvasState :=
  { state with fillStyle := .gradient (.linear start finish stops) }

/-- Set the fill to a radial gradient. -/
def setFillRadialGradient (center : Point) (radius : Float) (stops : Array GradientStop) (state : CanvasState) : CanvasState :=
  { state with fillStyle := .gradient (.radial center radius stops) }

/-- Set the stroke color. -/
def setStrokeColor (c : Color) (state : CanvasState) : CanvasState :=
  { state with strokeStyle := { state.strokeStyle with color := c } }

/-- Set the stroke line width. -/
def setLineWidth (w : Float) (state : CanvasState) : CanvasState :=
  { state with strokeStyle := { state.strokeStyle with lineWidth := w } }

/-- Set the global alpha. -/
def setGlobalAlpha (a : Float) (state : CanvasState) : CanvasState :=
  { state with globalAlpha := a }

/-- Set the line cap style. -/
def setLineCap (cap : LineCap) (state : CanvasState) : CanvasState :=
  { state with strokeStyle := { state.strokeStyle with lineCap := cap } }

/-- Set the line join style. -/
def setLineJoin (join : LineJoin) (state : CanvasState) : CanvasState :=
  { state with strokeStyle := { state.strokeStyle with lineJoin := join } }

/-- Set the dash pattern for stroked lines. None = solid line. -/
def setDashPattern (pattern : Option DashPattern) (state : CanvasState) : CanvasState :=
  { state with strokeStyle := { state.strokeStyle with dashPattern := pattern } }

/-- Set a simple dash pattern with dash and gap lengths. -/
def setDashed (dashLen gapLen : Float) (state : CanvasState) : CanvasState :=
  { state with strokeStyle := { state.strokeStyle with
      dashPattern := some (DashPattern.simple dashLen gapLen) } }

/-- Set a dotted line pattern. -/
def setDotted (state : CanvasState) : CanvasState :=
  { state with strokeStyle := { state.strokeStyle with
      lineCap := .round
      dashPattern := some (DashPattern.dotted (state.strokeStyle.lineWidth * 2)) } }

/-- Clear the dash pattern (solid line). -/
def setSolid (state : CanvasState) : CanvasState :=
  { state with strokeStyle := { state.strokeStyle with dashPattern := none } }

/-! ## Path transformation -/

/-- Transform a point by the current transform. -/
def transformPoint (state : CanvasState) (p : Point) : Point :=
  state.transform.apply p

/-- Transform an entire path by the current transform.
    Arcs are converted to bezier curves before transformation to handle non-uniform scaling correctly. -/
def transformPath (state : CanvasState) (path : Path) : Path := Id.run do
  let mut result : Array PathCommand := Array.mkEmpty path.commands.size
  let mut currentPoint := path.startPoint.getD Point.zero
  let mut subpathStart := currentPoint

  for cmd in path.commands do
    match cmd with
    | .moveTo p =>
      currentPoint := p
      subpathStart := p
      result := result.push (.moveTo (state.transform.apply p))

    | .lineTo p =>
      currentPoint := p
      result := result.push (.lineTo (state.transform.apply p))

    | .quadraticCurveTo cp p =>
      currentPoint := p
      result := result.push (.quadraticCurveTo (state.transform.apply cp) (state.transform.apply p))

    | .bezierCurveTo cp1 cp2 p =>
      currentPoint := p
      result := result.push (.bezierCurveTo (state.transform.apply cp1) (state.transform.apply cp2) (state.transform.apply p))

    | .arcTo p1 p2 radius =>
      -- Convert arcTo to beziers, then transform the control points
      -- This handles non-uniform scaling correctly
      match Tessellation.computeArcTo currentPoint p1 p2 radius with
      | some (t1, beziers, t2) =>
        -- Line to first tangent point
        result := result.push (.lineTo (state.transform.apply t1))
        -- Add transformed bezier segments
        for (cp1, cp2, endPt) in beziers do
          result := result.push (.bezierCurveTo (state.transform.apply cp1) (state.transform.apply cp2) (state.transform.apply endPt))
        currentPoint := t2
      | none =>
        -- Degenerate case: just line to p1
        result := result.push (.lineTo (state.transform.apply p1))
        currentPoint := p1

    | .arc center radius startAngle endAngle counterclockwise =>
      -- Convert arc to beziers, then transform the control points
      -- This handles non-uniform scaling correctly (circles become ellipses)
      let beziers := Path.arcToBeziers center radius startAngle endAngle counterclockwise
      -- First point on arc
      let startPt := Point.mk' (center.x + radius * Float.cos startAngle) (center.y + radius * Float.sin startAngle)
      if beziers.size > 0 then
        -- Move to arc start if not already there (for standalone arcs)
        if (currentPoint.x - startPt.x).abs > 0.001 || (currentPoint.y - startPt.y).abs > 0.001 then
          result := result.push (.lineTo (state.transform.apply startPt))
        -- Add transformed bezier segments
        for (cp1, cp2, endPt) in beziers do
          result := result.push (.bezierCurveTo (state.transform.apply cp1) (state.transform.apply cp2) (state.transform.apply endPt))
          currentPoint := endPt

    | .rect rect =>
      -- Convert rect to lines, then transform
      let tl := rect.topLeft
      let tr := rect.topRight
      let br := rect.bottomRight
      let bl := rect.bottomLeft
      result := result.push (.moveTo (state.transform.apply tl))
      result := result.push (.lineTo (state.transform.apply tr))
      result := result.push (.lineTo (state.transform.apply br))
      result := result.push (.lineTo (state.transform.apply bl))
      result := result.push .closePath
      currentPoint := tl
      subpathStart := tl

    | .closePath =>
      result := result.push .closePath
      currentPoint := subpathStart

  return { path with
    commands := result
    currentPoint := path.currentPoint.map state.transform.apply
    startPoint := path.startPoint.map state.transform.apply }

/-- Get the effective fill color with global alpha applied. -/
def effectiveFillColor (state : CanvasState) : Color :=
  let baseColor := state.fillStyle.toColor
  { baseColor with a := baseColor.a * state.globalAlpha }

/-- Get the effective fill style with global alpha applied. -/
def effectiveFillStyle (state : CanvasState) : FillStyle :=
  match state.fillStyle with
  | .solid c => .solid { c with a := c.a * state.globalAlpha }
  | .gradient g =>
    -- Apply globalAlpha to all gradient stops
    let applyAlpha : GradientStop → GradientStop := fun stop =>
      { stop with color := { stop.color with a := stop.color.a * state.globalAlpha } }
    match g with
    | .linear start finish stops => .gradient (.linear start finish (stops.map applyAlpha))
    | .radial center radius stops => .gradient (.radial center radius (stops.map applyAlpha))

/-- Get the effective stroke color with global alpha applied. -/
def effectiveStrokeColor (state : CanvasState) : Color :=
  let baseColor := state.strokeStyle.color
  { baseColor with a := baseColor.a * state.globalAlpha }

/-! ## Clip stack operations -/

/-- Transform a rect by applying a transform to all 4 corners and computing
    the axis-aligned bounding box. The result is in the same coordinate system
    as the transform (typically pixel coordinates). -/
def transformRectToScreenAABB (rect : Rect) (xform : Transform) : Rect :=
  -- Transform all 4 corners
  let p1 := xform.apply rect.origin
  let p2 := xform.apply ⟨rect.maxX, rect.y⟩
  let p3 := xform.apply ⟨rect.maxX, rect.maxY⟩
  let p4 := xform.apply ⟨rect.x, rect.maxY⟩
  -- Compute AABB
  let minX := min (min p1.x p2.x) (min p3.x p4.x)
  let maxX := max (max p1.x p2.x) (max p3.x p4.x)
  let minY := min (min p1.y p2.y) (min p3.y p4.y)
  let maxY := max (max p1.y p2.y) (max p3.y p4.y)
  Rect.mk' minX minY (maxX - minX) (maxY - minY)

/-- Intersect two rectangles. Returns none if they don't overlap. -/
def rectIntersect (r1 r2 : Rect) : Option Rect :=
  let x := max r1.x r2.x
  let y := max r1.y r2.y
  let right := min r1.maxX r2.maxX
  let bottom := min r1.maxY r2.maxY
  if right > x && bottom > y then
    some (Rect.mk' x y (right - x) (bottom - y))
  else
    none

/-- Compute the effective clip rect in screen pixels from the clip stack.
    Returns none if clip stack is empty (no clipping).
    Each clip rect is transformed by its stored transform and then intersected. -/
def effectiveClipRect (state : CanvasState) : Option Rect :=
  if state.clipStack.isEmpty then
    none
  else
    -- Transform each clip rect to screen space
    let screenRects := state.clipStack.map fun (rect, xform) =>
      transformRectToScreenAABB rect xform
    -- Intersect all rects
    screenRects.foldl (init := some screenRects[0]!) fun acc r =>
      acc.bind (rectIntersect · r)

/-- Push a clip rect with the current transform onto the clip stack. -/
def pushClip (rect : Rect) (state : CanvasState) : CanvasState :=
  { state with clipStack := state.clipStack.push (rect, state.transform) }

/-- Pop the most recent clip rect from the stack. -/
def popClip (state : CanvasState) : CanvasState :=
  { state with clipStack := state.clipStack.pop }

/-- Clear the entire clip stack. -/
def clearClipStack (state : CanvasState) : CanvasState :=
  { state with clipStack := #[] }

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

def setBaseTransform (t : Transform) : StateStack → StateStack :=
  modify (CanvasState.setBaseTransform t)

def setFillColor (c : Color) : StateStack → StateStack :=
  modify (CanvasState.setFillColor c)

def setFillStyle (style : FillStyle) : StateStack → StateStack :=
  modify (CanvasState.setFillStyle style)

def setFillLinearGradient (start finish : Point) (stops : Array GradientStop) : StateStack → StateStack :=
  modify (CanvasState.setFillLinearGradient start finish stops)

def setFillRadialGradient (center : Point) (radius : Float) (stops : Array GradientStop) : StateStack → StateStack :=
  modify (CanvasState.setFillRadialGradient center radius stops)

def setStrokeColor (c : Color) : StateStack → StateStack :=
  modify (CanvasState.setStrokeColor c)

def setLineWidth (w : Float) : StateStack → StateStack :=
  modify (CanvasState.setLineWidth w)

def setGlobalAlpha (a : Float) : StateStack → StateStack :=
  modify (CanvasState.setGlobalAlpha a)

def setLineCap (cap : LineCap) : StateStack → StateStack :=
  modify (CanvasState.setLineCap cap)

def setLineJoin (join : LineJoin) : StateStack → StateStack :=
  modify (CanvasState.setLineJoin join)

def setDashPattern (pattern : Option DashPattern) : StateStack → StateStack :=
  modify (CanvasState.setDashPattern pattern)

def setDashed (dashLen gapLen : Float) : StateStack → StateStack :=
  modify (CanvasState.setDashed dashLen gapLen)

def setDotted : StateStack → StateStack :=
  modify CanvasState.setDotted

def setSolid : StateStack → StateStack :=
  modify CanvasState.setSolid

def resetTransform : StateStack → StateStack :=
  modify CanvasState.resetTransform

end StateStack

end Afferent

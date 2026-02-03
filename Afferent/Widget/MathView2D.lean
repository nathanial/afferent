/-
  MathView2D - 2D math view widget with grid/axes/labels and world<->screen transforms.

  Intended for linalg demos and math-heavy visualizations.
-/
import Afferent.Arbor
import Afferent.Core.Types
import Afferent.Core.Path
import Afferent.Core.Transform
import Afferent.Canvas.Context
import Afferent.Text.Font
import Trellis
import Linalg.Vec2

namespace Afferent.Widget

open Afferent
open Afferent.Arbor
open CanvasM
open Linalg

namespace MathView2D

structure Config where
  style : BoxStyle := BoxStyle.fill
  background : Option Color := none
  scale : Float := 60.0
  origin : Option (Float × Float) := none
  originOffset : Float × Float := (0.0, 0.0)
  showGrid : Bool := true
  showAxes : Bool := true
  showLabels : Bool := true
  minorStep : Float := 0.5
  majorStep : Float := 1.0
  gridMinorColor : Color := Color.gray 0.15
  gridMajorColor : Color := Color.gray 0.25
  axisColor : Color := Color.gray 0.6
  labelColor : Color := Color.gray 0.7
  gridLineWidth : Float := 1.0
  axisLineWidth : Float := 2.0
  labelOffset : Float := 6.0
  labelPrecision : Nat := 2
  xLabel : Option String := none
  yLabel : Option String := none
  deriving Inhabited

structure View where
  origin : Vec2
  scale : Float
  width : Float
  height : Float
  worldMin : Vec2
  worldMax : Vec2
  deriving Inhabited

def worldToScreen (view : View) (p : Vec2) : Float × Float :=
  (view.origin.x + p.x * view.scale, view.origin.y - p.y * view.scale)

def screenToWorld (view : View) (p : Float × Float) : Vec2 :=
  Vec2.mk ((p.1 - view.origin.x) / view.scale) ((view.origin.y - p.2) / view.scale)

private def clamp (x lo hi : Float) : Float :=
  if x < lo then lo else if x > hi then hi else x

private def floatMod (a b : Float) : Float :=
  a - b * Float.floor (a / b)

private def isMultipleOf (a b : Float) : Bool :=
  if b == 0.0 then false
  else
    let rem := floatMod (Float.abs a) (Float.abs b)
    rem < 0.0005 || (Float.abs b - rem) < 0.0005

private def formatTick (value : Float) (precision : Nat) : String :=
  let pow := Float.pow 10.0 precision.toFloat
  let scaled := Float.floor (value * pow + 0.5) / pow
  let cleaned := if Float.abs scaled < 1.0e-6 then 0.0 else scaled
  s!"{cleaned}"

private def buildView (config : Config) (w h : Float) : View :=
  let origin := match config.origin with
    | some o => Vec2.mk o.1 o.2
    | none => Vec2.mk (w / 2 + config.originOffset.1) (h / 2 + config.originOffset.2)
  let minX := (0.0 - origin.x) / config.scale
  let maxX := (w - origin.x) / config.scale
  let maxY := (origin.y - 0.0) / config.scale
  let minY := (origin.y - h) / config.scale
  let worldMin := Vec2.mk (Float.min minX maxX) (Float.min minY maxY)
  let worldMax := Vec2.mk (Float.max minX maxX) (Float.max minY maxY)
  { origin := origin, scale := config.scale, width := w, height := h
    worldMin := worldMin, worldMax := worldMax }

def viewForSize (config : Config) (w h : Float) : View :=
  buildView config w h

/-- Pan the view by a screen-space delta (pixels). -/
def pan (config : Config) (dx dy : Float) : Config :=
  match config.origin with
  | some origin =>
      { config with origin := some (origin.1 + dx, origin.2 + dy) }
  | none =>
      { config with originOffset := (config.originOffset.1 + dx, config.originOffset.2 + dy) }

/-- Zoom the view around a screen-space point. Keeps the world point under the cursor stable. -/
def zoomAt (config : Config) (w h : Float) (cursor : Float × Float) (factor : Float)
    (minScale : Float := 5.0) (maxScale : Float := 500.0) : Config :=
  let factor := if factor <= 0.0 then 1.0 else factor
  let minS := if minScale <= 0.0 then 0.001 else minScale
  let maxS := if maxScale < minS then minS else maxScale
  let newScale := clamp (config.scale * factor) minS maxS
  if newScale == config.scale then
    config
  else
    let view := viewForSize config w h
    let world := screenToWorld view cursor
    let originX := cursor.1 - world.x * newScale
    let originY := cursor.2 + world.y * newScale
    match config.origin with
    | some _ =>
        { config with scale := newScale, origin := some (originX, originY) }
    | none =>
        let offsetX := originX - w / 2.0
        let offsetY := originY - h / 2.0
        { config with scale := newScale, originOffset := (offsetX, offsetY) }

private def drawGrid (view : View) (config : Config) : CanvasM Unit := do
  let minorStep := if config.minorStep <= 0.0 then 1.0 else config.minorStep
  let majorStep := if config.majorStep <= 0.0 then minorStep else config.majorStep

  let mut x := Float.floor (view.worldMin.x / minorStep) * minorStep
  while x <= view.worldMax.x do
    let isMajor := isMultipleOf x majorStep
    let color := if isMajor then config.gridMajorColor else config.gridMinorColor
    let width := if isMajor then config.gridLineWidth * 1.3 else config.gridLineWidth
    let sx := view.origin.x + x * view.scale
    setStrokeColor color
    setLineWidth width
    let path := Afferent.Path.empty
      |>.moveTo (Point.mk sx 0.0)
      |>.lineTo (Point.mk sx view.height)
    strokePath path
    x := x + minorStep

  let mut y := Float.floor (view.worldMin.y / minorStep) * minorStep
  while y <= view.worldMax.y do
    let isMajor := isMultipleOf y majorStep
    let color := if isMajor then config.gridMajorColor else config.gridMinorColor
    let width := if isMajor then config.gridLineWidth * 1.3 else config.gridLineWidth
    let sy := view.origin.y - y * view.scale
    setStrokeColor color
    setLineWidth width
    let path := Afferent.Path.empty
      |>.moveTo (Point.mk 0.0 sy)
      |>.lineTo (Point.mk view.width sy)
    strokePath path
    y := y + minorStep

private def drawAxes (view : View) (config : Config) : CanvasM Unit := do
  setStrokeColor config.axisColor
  setLineWidth config.axisLineWidth
  let axisX := clamp view.origin.x 0.0 view.width
  let axisY := clamp view.origin.y 0.0 view.height
  let xPath := Afferent.Path.empty
    |>.moveTo (Point.mk 0.0 axisY)
    |>.lineTo (Point.mk view.width axisY)
  strokePath xPath
  let yPath := Afferent.Path.empty
    |>.moveTo (Point.mk axisX 0.0)
    |>.lineTo (Point.mk axisX view.height)
  strokePath yPath

private def drawLabels (view : View) (config : Config) (font : Font) : CanvasM Unit := do
  let majorStep := if config.majorStep <= 0.0 then 1.0 else config.majorStep
  let axisX := clamp view.origin.x 0.0 view.width
  let axisY := clamp view.origin.y 0.0 view.height
  setFillColor config.labelColor

  let mut x := Float.floor (view.worldMin.x / majorStep) * majorStep
  while x <= view.worldMax.x do
    let label := formatTick x config.labelPrecision
    let (tw, th) ← font.measureText label
    let sx := view.origin.x + x * view.scale
    fillTextXY label (sx - tw / 2.0) (axisY + config.labelOffset + th) font
    x := x + majorStep

  let mut y := Float.floor (view.worldMin.y / majorStep) * majorStep
  while y <= view.worldMax.y do
    let label := formatTick y config.labelPrecision
    let (tw, th) ← font.measureText label
    let sy := view.origin.y - y * view.scale
    fillTextXY label (axisX - config.labelOffset - tw) (sy + th / 2.0) font
    y := y + majorStep

  match config.xLabel with
  | some label =>
      let (tw, _) ← font.measureText label
      fillTextXY label (view.width - tw - config.labelOffset) (axisY - config.labelOffset) font
  | none => pure ()

  match config.yLabel with
  | some label =>
      let (_, th) ← font.measureText label
      fillTextXY label (axisX + config.labelOffset) (th + config.labelOffset) font
  | none => pure ()

private def withContentRect (layout : Trellis.ComputedLayout)
    (draw : Float → Float → CanvasM Unit) : CanvasM Unit := do
  let rect := layout.contentRect
  save
  setBaseTransform (Transform.translate rect.x rect.y)
  resetTransform
  clip (Rect.mk' 0 0 rect.width rect.height)
  draw rect.width rect.height
  restore

def mathView2DVisual (name : Option String := none)
    (config : Config := {})
    (font : Font)
    (drawContent : View → CanvasM Unit) : WidgetBuilder := do
  let spec : CustomSpec := {
    measure := fun _ _ => (0, 0)
    collect := fun _ => #[]
    draw := some (fun layout => do
      withContentRect layout fun w h => do
        resetTransform
        let view := buildView config w h
        match config.background with
        | some color =>
            setFillColor color
            fillRect (Rect.mk' 0 0 w h)
        | none => pure ()
        if config.showGrid then
          drawGrid view config
        if config.showAxes then
          drawAxes view config
        if config.showLabels then
          drawLabels view config font
        drawContent view
    )
  }
  match name with
  | some n => namedCustom n spec (style := config.style)
  | none => custom spec (style := config.style)

def mathView2D (config : Config := {}) (font : Font)
    (drawContent : View → CanvasM Unit) : WidgetBuilder :=
  mathView2DVisual none config font drawContent

end MathView2D

end Afferent.Widget

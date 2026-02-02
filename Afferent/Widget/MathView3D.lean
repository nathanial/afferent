/-
  MathView3D - 3D math view widget with perspective projection and orbit camera.

  Provides:
  - Orbit camera (yaw/pitch), zoom
  - Perspective projection helpers
  - Simple grid/axes drawing helpers
-/
import Afferent.Arbor
import Afferent.Core.Types
import Afferent.Core.Path
import Afferent.Core.Transform
import Afferent.Canvas.Context
import Afferent.Text.Font
import Trellis
import Linalg.Vec2
import Linalg.Vec3
import Linalg.Mat4

namespace Afferent.Widget

open Afferent
open Afferent.Arbor
open CanvasM
open Linalg

namespace MathView3D

structure Camera where
  yaw : Float := 0.6
  pitch : Float := 0.3
  distance : Float := 8.0
  target : Vec3 := Vec3.zero
  deriving Inhabited

structure Config where
  style : BoxStyle := BoxStyle.fill
  background : Option Color := none
  origin : Option (Float × Float) := none
  originOffset : Float × Float := (0.0, 0.0)
  fov : Float := Float.pi / 3
  near : Float := 0.1
  far : Float := 200.0
  camera : Camera := {}
  minPitch : Float := -1.4
  maxPitch : Float := 1.4
  minDistance : Float := 1.0
  maxDistance : Float := 200.0
  orbitSpeed : Float := 0.005
  zoomSpeed : Float := 0.1
  showGrid : Bool := true
  showAxes : Bool := true
  showAxisLabels : Bool := true
  gridStep : Float := 1.0
  gridMajorStep : Float := 5.0
  gridExtent : Float := 10.0
  gridMinorColor : Color := Color.rgba 0.3 0.4 0.55 0.2
  gridMajorColor : Color := Color.rgba 0.4 0.55 0.8 0.35
  gridLineWidth : Float := 1.0
  axisLineWidth : Float := 2.0
  axisLength : Float := 3.0
  axisXColor : Color := Color.rgba 0.95 0.4 0.3 0.9
  axisYColor : Color := Color.rgba 0.3 0.9 0.4 0.9
  axisZColor : Color := Color.rgba 0.3 0.6 1.0 0.9
  axisLabelColor : Color := Color.gray 0.8
  axisLabelOffset : Float := 8.0
  deriving Inhabited

structure View where
  origin : Vec2
  width : Float
  height : Float
  aspect : Float
  camera : Camera
  cameraPos : Vec3
  right : Vec3
  up : Vec3
  forward : Vec3
  fov : Float
  near : Float
  far : Float
  viewMatrix : Mat4
  projectionMatrix : Mat4
  viewProj : Mat4
  deriving Inhabited

private def clamp (x lo hi : Float) : Float :=
  if x < lo then lo else if x > hi then hi else x

private def floatMod (a b : Float) : Float :=
  a - b * Float.floor (a / b)

private def isMultipleOf (a b : Float) : Bool :=
  if b == 0.0 then false
  else
    let rem := floatMod (Float.abs a) (Float.abs b)
    rem < 0.0005 || (Float.abs b - rem) < 0.0005

private def clampCamera (config : Config) (camera : Camera) : Camera :=
  { camera with
    pitch := clamp camera.pitch config.minPitch config.maxPitch
    distance := clamp camera.distance config.minDistance config.maxDistance
  }

def orbit (camera : Camera) (dx dy : Float) (config : Config := {}) : Camera :=
  let yaw := camera.yaw + dx * config.orbitSpeed
  let pitch := clamp (camera.pitch + dy * config.orbitSpeed) config.minPitch config.maxPitch
  { camera with yaw := yaw, pitch := pitch }

def zoom (camera : Camera) (delta : Float) (config : Config := {}) : Camera :=
  let scale := Float.max 0.05 (1.0 + delta * config.zoomSpeed)
  let distance := clamp (camera.distance * scale) config.minDistance config.maxDistance
  { camera with distance := distance }

private def cameraPosition (camera : Camera) : Vec3 :=
  let cy := Float.cos camera.yaw
  let sy := Float.sin camera.yaw
  let cp := Float.cos camera.pitch
  let sp := Float.sin camera.pitch
  let x := camera.distance * sy * cp
  let y := camera.distance * sp
  let z := camera.distance * cy * cp
  camera.target.add (Vec3.mk x y z)

private def viewMatrixFromFrame (pos right up forward : Vec3) : Mat4 :=
  { data := #[
    right.x, up.x, -forward.x, 0,
    right.y, up.y, -forward.y, 0,
    right.z, up.z, -forward.z, 0,
    -(right.dot pos), -(up.dot pos), forward.dot pos, 1
  ] }

private def buildView (config : Config) (w h : Float) : View :=
  let camera := clampCamera config config.camera
  let origin := match config.origin with
    | some o => Vec2.mk o.1 o.2
    | none => Vec2.mk (w / 2 + config.originOffset.1) (h / 2 + config.originOffset.2)
  let aspect := if h > 0.0 then w / h else 1.0
  let pos := cameraPosition camera
  let forward := (camera.target.sub pos).normalize
  let rightRaw := Vec3.cross Vec3.unitY forward
  let right := if rightRaw.lengthSquared < 0.000001 then Vec3.unitX else rightRaw.normalize
  let up := (Vec3.cross forward right).normalize
  let view := viewMatrixFromFrame pos right up forward
  let proj := Mat4.perspective config.fov aspect config.near config.far
  {
    origin := origin
    width := w
    height := h
    aspect := aspect
    camera := camera
    cameraPos := pos
    right := right
    up := up
    forward := forward
    fov := config.fov
    near := config.near
    far := config.far
    viewMatrix := view
    projectionMatrix := proj
    viewProj := proj * view
  }

def viewForSize (config : Config) (w h : Float) : View :=
  buildView config w h

private def projectCamera (view : View) (p : Vec3) : Option (Float × Float × Float) :=
  let rel := p.sub view.cameraPos
  let xCam := rel.dot view.right
  let yCam := rel.dot view.up
  let zCam := rel.dot view.forward
  if zCam <= view.near || zCam >= view.far then
    none
  else
    let f := 1.0 / Float.tan (view.fov * 0.5)
    let xNdc := (xCam / zCam) * (f / view.aspect)
    let yNdc := (yCam / zCam) * f
    some (xNdc, yNdc, zCam)

def worldToScreen (view : View) (p : Vec3) : Option (Float × Float) :=
  match projectCamera view p with
  | some (xNdc, yNdc, _) =>
      let halfW := view.width / 2.0
      let halfH := view.height / 2.0
      let sx := view.origin.x + xNdc * halfW
      let sy := view.origin.y - yNdc * halfH
      some (sx, sy)
  | none => none

def screenRay (view : View) (screen : Float × Float) : Vec3 × Vec3 :=
  let halfW := view.width / 2.0
  let halfH := view.height / 2.0
  let ndcX := if halfW == 0.0 then 0.0 else (screen.1 - view.origin.x) / halfW
  let ndcY := if halfH == 0.0 then 0.0 else -((screen.2 - view.origin.y) / halfH)
  let f := 1.0 / Float.tan (view.fov * 0.5)
  let dirCam := Vec3.mk (ndcX * view.aspect / f) (ndcY / f) 1.0
  let dirWorld :=
    (view.right.scale dirCam.x).add (view.up.scale dirCam.y)
      |>.add (view.forward.scale dirCam.z)
      |>.normalize
  (view.cameraPos, dirWorld)

def screenToWorldOnPlane (view : View) (screen : Float × Float)
    (planePoint planeNormal : Vec3) : Option Vec3 :=
  let (origin, dir) := screenRay view screen
  let denom := planeNormal.dot dir
  if Float.abs' denom < Float.epsilon then
    none
  else
    let t := (planePoint.sub origin).dot planeNormal / denom
    if t <= 0.0 then none else some (origin.add (dir.scale t))

def drawLine3D (view : View) (a b : Vec3) (color : Color)
    (lineWidth : Float := 1.2) : CanvasM Unit := do
  match worldToScreen view a, worldToScreen view b with
  | some p1, some p2 =>
      setStrokeColor color
      setLineWidth lineWidth
      let path := Afferent.Path.empty
        |>.moveTo (Point.mk p1.1 p1.2)
        |>.lineTo (Point.mk p2.1 p2.2)
      strokePath path
  | _, _ => pure ()

def drawPoint3D (view : View) (p : Vec3) (color : Color) (radius : Float := 4.0) : CanvasM Unit := do
  match worldToScreen view p with
  | some pos =>
      setFillColor color
      fillPath (Afferent.Path.circle (Point.mk pos.1 pos.2) radius)
  | none => pure ()

def drawPolyline3D (view : View) (points : Array Vec3) (color : Color)
    (lineWidth : Float := 1.2) : CanvasM Unit := do
  if points.size < 2 then return
  let projected := points.map (worldToScreen view ·)
  if projected.any (fun p => p.isNone) then
    return
  let p0 := projected.getD 0 none |>.getD (0.0, 0.0)
  let mut path := Afferent.Path.empty
    |>.moveTo (Point.mk p0.1 p0.2)
  for i in [1:projected.size] do
    let p := projected.getD i none |>.getD (0.0, 0.0)
    path := path.lineTo (Point.mk p.1 p.2)
  setStrokeColor color
  setLineWidth lineWidth
  strokePath path

def drawAxes (view : View) (config : Config) (font : Font) : CanvasM Unit := do
  let axisLength := config.axisLength
  let xEnd := Vec3.mk axisLength 0.0 0.0
  let yEnd := Vec3.mk 0.0 axisLength 0.0
  let zEnd := Vec3.mk 0.0 0.0 axisLength
  drawLine3D view Vec3.zero xEnd config.axisXColor config.axisLineWidth
  drawLine3D view Vec3.zero yEnd config.axisYColor config.axisLineWidth
  drawLine3D view Vec3.zero zEnd config.axisZColor config.axisLineWidth
  if config.showAxisLabels then
    setFillColor config.axisLabelColor
    match worldToScreen view xEnd with
    | some p => fillTextXY "X" (p.1 + config.axisLabelOffset) (p.2 - config.axisLabelOffset) font
    | none => pure ()
    match worldToScreen view yEnd with
    | some p => fillTextXY "Y" (p.1 + config.axisLabelOffset) (p.2 - config.axisLabelOffset) font
    | none => pure ()
    match worldToScreen view zEnd with
    | some p => fillTextXY "Z" (p.1 + config.axisLabelOffset) (p.2 - config.axisLabelOffset) font
    | none => pure ()

def drawGridXZ (view : View) (config : Config) : CanvasM Unit := do
  let step := if config.gridStep <= 0.0 then 1.0 else config.gridStep
  let major := if config.gridMajorStep <= 0.0 then step else config.gridMajorStep
  let extent := if config.gridExtent <= 0.0 then 0.0 else config.gridExtent
  if extent == 0.0 then return
  let count := Float.floor (extent / step) |>.toUInt64.toNat
  for i in [: (count * 2 + 1)] do
    let offset := (i.toFloat - count.toFloat) * step
    let isMajor := isMultipleOf offset major
    let color := if isMajor then config.gridMajorColor else config.gridMinorColor
    let width := if isMajor then config.gridLineWidth * 1.4 else config.gridLineWidth
    let a1 := Vec3.mk (-extent) 0.0 offset
    let b1 := Vec3.mk extent 0.0 offset
    let a2 := Vec3.mk offset 0.0 (-extent)
    let b2 := Vec3.mk offset 0.0 extent
    drawLine3D view a1 b1 color width
    drawLine3D view a2 b2 color width

private def withContentRect (layout : Trellis.ComputedLayout)
    (draw : Float → Float → CanvasM Unit) : CanvasM Unit := do
  let rect := layout.contentRect
  save
  setBaseTransform (Transform.translate rect.x rect.y)
  resetTransform
  clip (Rect.mk' 0 0 rect.width rect.height)
  draw rect.width rect.height
  restore

def mathView3DVisual (name : Option String := none)
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
          drawGridXZ view config
        if config.showAxes then
          drawAxes view config font
        drawContent view
    )
  }
  match name with
  | some n => namedCustom n spec (style := config.style)
  | none => custom spec (style := config.style)

def mathView3D (config : Config := {}) (font : Font)
    (drawContent : View → CanvasM Unit) : WidgetBuilder :=
  mathView3DVisual none config font drawContent

end MathView3D

end Afferent.Widget

/-
  VectorField widget helpers for rendering 2D/3D vector fields with arrows.

  Provides:
  - Configurable sampling grids
  - Magnitude-based color ramps
  - Arrow styling and scaling
-/
import Afferent.Arbor
import Afferent.Core.Types
import Afferent.Core.Path
import Afferent.Canvas.Context
import Afferent.Text.Font
import Afferent.Widget.MathView2D
import Afferent.Widget.MathView3D
import Linalg.Vec2
import Linalg.Vec3

namespace Afferent.Widget

open Afferent
open Afferent.Arbor
open CanvasM
open Linalg

namespace VectorField

/-- Arrow length scaling modes. -/
inductive ArrowScaleMode where
  | world  -- `scale` is in world units
  | cell   -- `scale` is a fraction of the sampling cell size
  deriving Repr, Inhabited, BEq

/-- Arrow drawing configuration. -/
structure ArrowStyle where
  lineWidth : Float := 1.5
  headLength : Float := 6.0
  headAngle : Float := 0.5
  scale : Float := 0.4
  scaleMode : ArrowScaleMode := .world
  scaleByMagnitude : Bool := false
  minMagnitude : Float := 0.001
  deriving Repr, Inhabited

/-- Color ramps for magnitude coloring. -/
inductive ColorScale where
  | blueCyanGreenYellowRed
  | viridis
  | grayscale
  | custom (low high : Color)
  deriving Repr, Inhabited, BEq

/-- Color selection mode for arrows. -/
inductive ColorMode where
  | fixed (color : Color)
  | magnitude (scale : ColorScale)
  deriving Repr, Inhabited, BEq

/-- Coloring configuration for vector fields. -/
structure Coloring where
  mode : ColorMode := .magnitude .blueCyanGreenYellowRed
  maxMagnitude : Option Float := none
  deriving Repr, Inhabited

/-- Sampling configuration for 2D vector fields. -/
structure Sampling2D where
  samplesX : Nat := 12
  samplesY : Nat := 12
  padding : Float := 0.0
  bounds : Option (Vec2 × Vec2) := none
  computeMax : Bool := false
  deriving Repr, Inhabited

/-- Sampling configuration for 3D vector fields. -/
structure Sampling3D where
  samplesX : Nat := 8
  samplesY : Nat := 8
  samplesZ : Nat := 4
  padding : Float := 0.0
  bounds : Option (Vec3 × Vec3) := none
  extent : Float := 5.0
  computeMax : Bool := false
  deriving Repr, Inhabited

/-- Vector field widget config for 2D views. -/
structure Config2D where
  view : MathView2D.Config := {}
  sampling : Sampling2D := {}
  arrows : ArrowStyle := {}
  coloring : Coloring := {}
  overlay : Option (MathView2D.View → Float → CanvasM Unit) := none
  deriving Inhabited

/-- Vector field widget config for 3D views. -/
structure Config3D where
  view : MathView3D.Config := {}
  sampling : Sampling3D := {}
  arrows : ArrowStyle := {}
  coloring : Coloring := {}
  overlay : Option (MathView3D.View → Float → CanvasM Unit) := none
  deriving Inhabited

private def clamp01 (t : Float) : Float :=
  if t < 0.0 then 0.0 else if t > 1.0 then 1.0 else t

private def lerpColor (c1 c2 : Color) (t : Float) : Color :=
  let t := clamp01 t
  Color.rgba
    (c1.r + (c2.r - c1.r) * t)
    (c1.g + (c2.g - c1.g) * t)
    (c1.b + (c2.b - c1.b) * t)
    (c1.a + (c2.a - c1.a) * t)

/-- Map a normalized value to a color on the given scale. -/
def colorForScale (scale : ColorScale) (t : Float) : Color :=
  let t := clamp01 t
  match scale with
  | .blueCyanGreenYellowRed =>
      if t < 0.25 then
        lerpColor (Color.rgba 0.2 0.3 0.8 1.0) (Color.rgba 0.0 0.8 0.8 1.0) (t * 4.0)
      else if t < 0.5 then
        lerpColor (Color.rgba 0.0 0.8 0.8 1.0) (Color.rgba 0.2 0.9 0.2 1.0) ((t - 0.25) * 4.0)
      else if t < 0.75 then
        lerpColor (Color.rgba 0.2 0.9 0.2 1.0) (Color.rgba 0.9 0.9 0.0 1.0) ((t - 0.5) * 4.0)
      else
        lerpColor (Color.rgba 0.9 0.9 0.0 1.0) (Color.rgba 0.9 0.2 0.2 1.0) ((t - 0.75) * 4.0)
  | .viridis =>
      if t < 0.25 then
        lerpColor (Color.rgba 0.27 0.0 0.33 1.0) (Color.rgba 0.28 0.36 0.51 1.0) (t * 4)
      else if t < 0.5 then
        lerpColor (Color.rgba 0.28 0.36 0.51 1.0) (Color.rgba 0.13 0.57 0.55 1.0) ((t - 0.25) * 4)
      else if t < 0.75 then
        lerpColor (Color.rgba 0.13 0.57 0.55 1.0) (Color.rgba 0.55 0.76 0.22 1.0) ((t - 0.5) * 4)
      else
        lerpColor (Color.rgba 0.55 0.76 0.22 1.0) (Color.rgba 0.99 0.91 0.15 1.0) ((t - 0.75) * 4)
  | .grayscale =>
      Color.rgba t t t 1.0
  | .custom low high =>
      lerpColor low high t

/-- Map a magnitude to a color using the provided scale. -/
def colorForMagnitude (scale : ColorScale) (magnitude maxMagnitude : Float) : Color :=
  let t := if maxMagnitude > 0.0 then magnitude / maxMagnitude else 0.0
  colorForScale scale t

private def colorNeedsMagnitude (mode : ColorMode) : Bool :=
  match mode with
  | .fixed _ => false
  | .magnitude _ => true

private def drawArrow2D (start finish : Float × Float) (color : Color)
    (style : ArrowStyle) : CanvasM Unit := do
  let (x1, y1) := start
  let (x2, y2) := finish

  setStrokeColor color
  setLineWidth style.lineWidth
  let path := Afferent.Path.empty
    |>.moveTo (Point.mk x1 y1)
    |>.lineTo (Point.mk x2 y2)
  strokePath path

  let dx := x2 - x1
  let dy := y2 - y1
  let len := Float.sqrt (dx * dx + dy * dy)
  if len > style.headLength && style.headLength > 0.0 then
    let ux := dx / len
    let uy := dy / len
    let cosA := Float.cos style.headAngle
    let sinA := Float.sin style.headAngle
    let lx := x2 - style.headLength * (ux * cosA - uy * sinA)
    let ly := y2 - style.headLength * (uy * cosA + ux * sinA)
    let rx := x2 - style.headLength * (ux * cosA + uy * sinA)
    let ry := y2 - style.headLength * (uy * cosA - ux * sinA)

    setFillColor color
    let headPath := Afferent.Path.empty
      |>.moveTo (Point.mk x2 y2)
      |>.lineTo (Point.mk lx ly)
      |>.lineTo (Point.mk rx ry)
      |>.closePath
    fillPath headPath

private def bounds2D (view : MathView2D.View) (sampling : Sampling2D) : Vec2 × Vec2 :=
  let (minRaw, maxRaw) := sampling.bounds.getD (view.worldMin, view.worldMax)
  let minX := Float.min minRaw.x maxRaw.x
  let maxX := Float.max minRaw.x maxRaw.x
  let minY := Float.min minRaw.y maxRaw.y
  let maxY := Float.max minRaw.y maxRaw.y
  let paddedMin := Vec2.mk (minX + sampling.padding) (minY + sampling.padding)
  let paddedMax := Vec2.mk (maxX - sampling.padding) (maxY - sampling.padding)
  if paddedMin.x > paddedMax.x || paddedMin.y > paddedMax.y then
    (Vec2.mk minX minY, Vec2.mk maxX maxY)
  else
    (paddedMin, paddedMax)

private def bounds3D (sampling : Sampling3D) : Vec3 × Vec3 :=
  match sampling.bounds with
  | some b => b
  | none =>
      let e := sampling.extent
      (Vec3.mk (-e) (-e) (-e), Vec3.mk e e e)

private def applyPadding3D (bounds : Vec3 × Vec3) (padding : Float) : Vec3 × Vec3 :=
  let (minRaw, maxRaw) := bounds
  let minX := Float.min minRaw.x maxRaw.x
  let maxX := Float.max minRaw.x maxRaw.x
  let minY := Float.min minRaw.y maxRaw.y
  let maxY := Float.max minRaw.y maxRaw.y
  let minZ := Float.min minRaw.z maxRaw.z
  let maxZ := Float.max minRaw.z maxRaw.z
  let paddedMin := Vec3.mk (minX + padding) (minY + padding) (minZ + padding)
  let paddedMax := Vec3.mk (maxX - padding) (maxY - padding) (maxZ - padding)
  if paddedMin.x > paddedMax.x || paddedMin.y > paddedMax.y || paddedMin.z > paddedMax.z then
    (Vec3.mk minX minY minZ, Vec3.mk maxX maxY maxZ)
  else
    (paddedMin, paddedMax)

private def computeMax2D (field : Vec2 → Vec2) (minB maxB : Vec2)
    (samplesX samplesY : Nat) (minMagnitude : Float) : Float := Id.run do
  let mut maxMag := minMagnitude
  let stepX := if samplesX == 0 then 0.0 else (maxB.x - minB.x) / samplesX.toFloat
  let stepY := if samplesY == 0 then 0.0 else (maxB.y - minB.y) / samplesY.toFloat
  for i in [:samplesX + 1] do
    for j in [:samplesY + 1] do
      let p := Vec2.mk (minB.x + i.toFloat * stepX) (minB.y + j.toFloat * stepY)
      let mag := (field p).length
      if mag > maxMag then
        maxMag := mag
  maxMag

private def computeMax3D (field : Vec3 → Vec3) (minB maxB : Vec3)
    (samplesX samplesY samplesZ : Nat) (minMagnitude : Float) : Float := Id.run do
  let mut maxMag := minMagnitude
  let stepX := if samplesX == 0 then 0.0 else (maxB.x - minB.x) / samplesX.toFloat
  let stepY := if samplesY == 0 then 0.0 else (maxB.y - minB.y) / samplesY.toFloat
  let stepZ := if samplesZ == 0 then 0.0 else (maxB.z - minB.z) / samplesZ.toFloat
  for i in [:samplesX + 1] do
    for j in [:samplesY + 1] do
      for k in [:samplesZ + 1] do
        let p := Vec3.mk
          (minB.x + i.toFloat * stepX)
          (minB.y + j.toFloat * stepY)
          (minB.z + k.toFloat * stepZ)
        let mag := (field p).length
        if mag > maxMag then
          maxMag := mag
  maxMag

/-- Draw a 2D vector field on a MathView2D view. Returns the computed max magnitude. -/
def drawField2D (view : MathView2D.View) (field : Vec2 → Vec2)
    (sampling : Sampling2D := {}) (arrows : ArrowStyle := {})
    (coloring : Coloring := {}) : CanvasM Float := do
  let (minB, maxB) := bounds2D view sampling
  let samplesX := sampling.samplesX
  let samplesY := sampling.samplesY
  let stepX := if samplesX == 0 then 0.0 else (maxB.x - minB.x) / samplesX.toFloat
  let stepY := if samplesY == 0 then 0.0 else (maxB.y - minB.y) / samplesY.toFloat
  let cellSize := if stepX == 0.0 || stepY == 0.0 then 1.0 else Float.min stepX stepY

  let needComputedMax :=
    sampling.computeMax || arrows.scaleByMagnitude ||
      (colorNeedsMagnitude coloring.mode && coloring.maxMagnitude.isNone)

  let computedMax :=
    if needComputedMax then
      computeMax2D field minB maxB samplesX samplesY arrows.minMagnitude
    else
      arrows.minMagnitude

  let colorMax := coloring.maxMagnitude.getD computedMax
  let colorMax := Float.max colorMax arrows.minMagnitude
  let resultMax := if needComputedMax then computedMax else colorMax

  let baseScale :=
    match arrows.scaleMode with
    | .world => arrows.scale
    | .cell => arrows.scale * cellSize

  for i in [:samplesX + 1] do
    for j in [:samplesY + 1] do
      let p := Vec2.mk (minB.x + i.toFloat * stepX) (minB.y + j.toFloat * stepY)
      let v := field p
      let mag := v.length
      if mag > arrows.minMagnitude then
        let dir := if mag > 0.0 then v.scale (1.0 / mag) else Vec2.zero
        let length :=
          if arrows.scaleByMagnitude && colorMax > 0.0 then
            baseScale * (mag / colorMax)
          else
            baseScale
        if length > 0.0 then
          let start := MathView2D.worldToScreen view p
          let finish := MathView2D.worldToScreen view (p.add (dir.scale length))
          let color :=
            match coloring.mode with
            | .fixed c => c
            | .magnitude scale => colorForMagnitude scale mag colorMax
          drawArrow2D start finish color arrows

  pure resultMax

/-- Draw a 3D vector field on a MathView3D view. Returns the computed max magnitude. -/
def drawField3D (view : MathView3D.View) (field : Vec3 → Vec3)
    (sampling : Sampling3D := {}) (arrows : ArrowStyle := {})
    (coloring : Coloring := {}) : CanvasM Float := do
  let baseBounds := bounds3D sampling
  let (minB, maxB) := applyPadding3D baseBounds sampling.padding
  let samplesX := sampling.samplesX
  let samplesY := sampling.samplesY
  let samplesZ := sampling.samplesZ
  let stepX := if samplesX == 0 then 0.0 else (maxB.x - minB.x) / samplesX.toFloat
  let stepY := if samplesY == 0 then 0.0 else (maxB.y - minB.y) / samplesY.toFloat
  let stepZ := if samplesZ == 0 then 0.0 else (maxB.z - minB.z) / samplesZ.toFloat
  let cellSize :=
    if stepX == 0.0 || stepY == 0.0 || stepZ == 0.0 then 1.0
    else
      Float.min stepX (Float.min stepY stepZ)

  let needComputedMax :=
    sampling.computeMax || arrows.scaleByMagnitude ||
      (colorNeedsMagnitude coloring.mode && coloring.maxMagnitude.isNone)

  let computedMax :=
    if needComputedMax then
      computeMax3D field minB maxB samplesX samplesY samplesZ arrows.minMagnitude
    else
      arrows.minMagnitude

  let colorMax := coloring.maxMagnitude.getD computedMax
  let colorMax := Float.max colorMax arrows.minMagnitude
  let resultMax := if needComputedMax then computedMax else colorMax

  let baseScale :=
    match arrows.scaleMode with
    | .world => arrows.scale
    | .cell => arrows.scale * cellSize

  for i in [:samplesX + 1] do
    for j in [:samplesY + 1] do
      for k in [:samplesZ + 1] do
        let p := Vec3.mk
          (minB.x + i.toFloat * stepX)
          (minB.y + j.toFloat * stepY)
          (minB.z + k.toFloat * stepZ)
        let v := field p
        let mag := v.length
        if mag > arrows.minMagnitude then
          let dir := if mag > 0.0 then v.scale (1.0 / mag) else Vec3.zero
          let length :=
            if arrows.scaleByMagnitude && colorMax > 0.0 then
              baseScale * (mag / colorMax)
            else
              baseScale
          if length > 0.0 then
            let start := MathView3D.worldToScreen view p
            let finish := MathView3D.worldToScreen view (p.add (dir.scale length))
            match start, finish with
            | some s, some f =>
                let color :=
                  match coloring.mode with
                  | .fixed c => c
                  | .magnitude scale => colorForMagnitude scale mag colorMax
                drawArrow2D s f color arrows
            | _, _ => pure ()

  pure resultMax

/-- Build a vector field widget using MathView2D. -/
def vectorField2DVisual (name : Option String := none)
    (config : Config2D := {}) (font : Font)
    (field : Vec2 → Vec2) : WidgetBuilder :=
  MathView2D.mathView2DVisual name config.view font (fun view => do
    let maxMag ← drawField2D view field config.sampling config.arrows config.coloring
    match config.overlay with
    | some overlay => overlay view maxMag
    | none => pure ()
    pure ())

/-- Build a vector field widget using MathView2D. -/
def vectorField2D (config : Config2D := {}) (font : Font)
    (field : Vec2 → Vec2) : WidgetBuilder :=
  vectorField2DVisual none config font field

/-- Build a vector field widget using MathView3D. -/
def vectorField3DVisual (name : Option String := none)
    (config : Config3D := {}) (font : Font)
    (field : Vec3 → Vec3) : WidgetBuilder :=
  MathView3D.mathView3DVisual name config.view font (fun view => do
    let maxMag ← drawField3D view field config.sampling config.arrows config.coloring
    match config.overlay with
    | some overlay => overlay view maxMag
    | none => pure ()
    pure ())

/-- Build a vector field widget using MathView3D. -/
def vectorField3D (config : Config3D := {}) (font : Font)
    (field : Vec3 → Vec3) : WidgetBuilder :=
  vectorField3DVisual none config font field

end VectorField

end Afferent.Widget

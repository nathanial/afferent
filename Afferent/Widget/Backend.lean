/-
  Afferent Widget Backend
  Implementation of Arbor rendering using Afferent's CanvasM.
  Converts abstract RenderCommands to Metal-backed drawing calls.
-/
import Afferent.Canvas.Context
import Afferent.Core.Path
import Afferent.Core.Transform
import Afferent.Text.Font
import Afferent.Text.Measurer
import Afferent.Arbor
import Afferent.Arbor.Core.Path
import Std.Data.HashMap

namespace Afferent.Widget

open Afferent
open Afferent.Arbor

/-- Convert Arbor Rect to Afferent Rect. -/
def toAfferentRect (r : Afferent.Arbor.Rect) : Afferent.Rect :=
  Afferent.Rect.mk' r.origin.x r.origin.y r.size.width r.size.height

/-- Convert Arbor Point to Afferent Point. -/
def toAfferentPoint (p : Afferent.Arbor.Point) : Afferent.Point :=
  Afferent.Point.mk' p.x p.y

/-- Convert a polygon to an Afferent Path. -/
def polygonToPath (points : Array Afferent.Arbor.Point) : Afferent.Path :=
  Id.run do
    if points.size > 0 then
      let first := points[0]!
      let mut path := (Afferent.Path.empty).moveTo (toAfferentPoint first)
      for i in [1:points.size] do
        let p := points[i]!
        path := path.lineTo (toAfferentPoint p)
      return path.closePath
    else
      return Afferent.Path.empty

/-- Convert Arbor Color to Afferent Color.
    Arbor uses Tincture.Color which is the same as Afferent's Color. -/
def toAfferentColor (c : Afferent.Arbor.Color) : Afferent.Color := c

/-- Convert Arbor FillRule to Afferent FillRule. -/
def toAfferentFillRule (rule : Afferent.Arbor.FillRule) : Afferent.FillRule :=
  match rule with
  | .nonZero => .nonZero
  | .evenOdd => .evenOdd

/-- Convert Arbor Path to Afferent Path. -/
def toAfferentPath (path : Afferent.Arbor.Path) : Afferent.Path :=
  let base := Afferent.Path.empty
  let built := path.commands.foldl (init := base) fun acc cmd =>
    match cmd with
    | .moveTo p =>
      acc.moveTo (toAfferentPoint p)
    | .lineTo p =>
      acc.lineTo (toAfferentPoint p)
    | .quadraticCurveTo cp p =>
      acc.quadraticCurveTo (toAfferentPoint cp) (toAfferentPoint p)
    | .bezierCurveTo cp1 cp2 p =>
      acc.bezierCurveTo (toAfferentPoint cp1) (toAfferentPoint cp2) (toAfferentPoint p)
    | .arcTo p1 p2 radius =>
      acc.arcTo (toAfferentPoint p1) (toAfferentPoint p2) radius
    | .arc center radius startAngle endAngle counterclockwise =>
      acc.arc (toAfferentPoint center) radius startAngle endAngle counterclockwise
    | .rect r =>
      acc.rect (toAfferentRect r)
    | .closePath =>
      acc.closePath
  built.withFillRule (toAfferentFillRule path.fillRule)

/-- Execute a single RenderCommand using CanvasM.
    Requires a FontRegistry to resolve FontIds to Font handles. -/
def executeCommand (reg : FontRegistry) (cmd : Afferent.Arbor.RenderCommand) : CanvasM Unit := do
  match cmd with
  | .fillRect rect color cornerRadius =>
    let afferentRect := toAfferentRect rect
    if cornerRadius > 0 then
      CanvasM.setFillColor (toAfferentColor color)
      CanvasM.fillRoundedRect afferentRect cornerRadius
    else
      CanvasM.setFillColor (toAfferentColor color)
      CanvasM.fillRect afferentRect

  | .fillRectStyle rect style cornerRadius =>
    let afferentRect := toAfferentRect rect
    CanvasM.save
    CanvasM.setFillStyle style
    if cornerRadius > 0 then
      CanvasM.fillRoundedRect afferentRect cornerRadius
    else
      CanvasM.fillRect afferentRect
    CanvasM.restore

  | .strokeRect rect color lineWidth cornerRadius =>
    let afferentRect := toAfferentRect rect
    CanvasM.setStrokeColor (toAfferentColor color)
    CanvasM.setLineWidth lineWidth
    if cornerRadius > 0 then
      CanvasM.strokeRoundedRect afferentRect cornerRadius
    else
      CanvasM.strokeRect afferentRect

  | .fillCircle center radius color =>
    -- Draw a single filled circle via the batch function
    let canvas ← CanvasM.getCanvas
    let (canvasWidth, canvasHeight) ← canvas.ctx.getCurrentSize
    let data := #[center.x, center.y, radius, 0.0, color.r, color.g, color.b, color.a, 0.0]
    canvas.ctx.renderer.drawBatch 1 data 1 0.0 0.0 canvasWidth canvasHeight

  | .strokeCircle center radius color lineWidth =>
    -- Draw a stroked circle using path (no stroked circle batch yet)
    let twoPi := 6.283185307179586  -- 2 * pi
    let path := Afferent.Path.empty.arc ⟨center.x, center.y⟩ radius 0 twoPi false
    CanvasM.setStrokeColor (toAfferentColor color)
    CanvasM.setLineWidth lineWidth
    CanvasM.strokePath path

  | .strokeLine p1 p2 color lineWidth =>
    -- Draw a single line via the batch function (batch size = 1)
    let canvas ← CanvasM.getCanvas
    let (canvasWidth, canvasHeight) ← canvas.ctx.getCurrentSize
    let data := #[p1.x, p1.y, p2.x, p2.y, color.r, color.g, color.b, color.a, 0.0]
    canvas.ctx.renderer.drawLineBatch data 1 lineWidth canvasWidth canvasHeight

  | .strokeLineBatch data count lineWidth =>
    if count == 0 then
      pure ()
    else
      let canvas ← CanvasM.getCanvas
      let (canvasWidth, canvasHeight) ← canvas.ctx.getCurrentSize
      canvas.ctx.renderer.drawLineBatch data count.toUInt32 lineWidth canvasWidth canvasHeight

  | .fillCircleBatch data count =>
    if count == 0 then
      pure ()
    else
      let canvas ← CanvasM.getCanvas
      let (canvasWidth, canvasHeight) ← canvas.ctx.getCurrentSize
      -- Convert from [cx, cy, radius, r, g, b, a] (7 floats) to [x, y, w, h, r, g, b, a, cornerRadius] (9 floats)
      let mut batchData : Array Float := Array.mkEmpty (count * 9)
      for i in [:count] do
        let base := i * 7
        let cx := data[base]!
        let cy := data[base + 1]!
        let radius := data[base + 2]!
        let r := data[base + 3]!
        let g := data[base + 4]!
        let b := data[base + 5]!
        let a := data[base + 6]!
        let diameter := radius * 2.0
        batchData := batchData.push (cx - radius) |>.push (cy - radius)
                               |>.push diameter |>.push diameter
                               |>.push r |>.push g |>.push b |>.push a |>.push 0.0
      canvas.ctx.renderer.drawBatch 1 batchData count.toUInt32 0.0 0.0 canvasWidth canvasHeight

  | .fillText text x y fontId color =>
    match reg.get fontId with
    | some font =>
      CanvasM.fillTextColor text ⟨x, y⟩ font (toAfferentColor color)
    | none =>
      -- Font not found, skip rendering
      pure ()

  | .fillTextBlock text rect fontId color align valign =>
    match reg.get fontId with
    | some font =>
      -- Measure text to calculate alignment
      let (textWidth, textHeight) ← CanvasM.measureText text font
      let x := match align with
        | .left => rect.origin.x
        | .center => rect.origin.x + (rect.size.width - textWidth) / 2
        | .right => rect.origin.x + rect.size.width - textWidth
      let y := match valign with
        | .top => rect.origin.y + font.ascender
        | .middle => rect.origin.y + (rect.size.height - textHeight) / 2 + font.ascender
        | .bottom => rect.origin.y + rect.size.height - font.descender
      CanvasM.fillTextColor text ⟨x, y⟩ font (toAfferentColor color)
    | none =>
      pure ()

  | .fillPolygon points color =>
    if points.size >= 3 then
      let path := polygonToPath points
      CanvasM.setFillColor (toAfferentColor color)
      CanvasM.fillPath path
    else
      pure ()

  | .strokePolygon points color lineWidth =>
    if points.size >= 3 then
      let path := polygonToPath points
      CanvasM.setStrokeColor (toAfferentColor color)
      CanvasM.setLineWidth lineWidth
      CanvasM.strokePath path
    else
      pure ()

  | .fillPath path color =>
    let afferentPath := toAfferentPath path
    CanvasM.setFillColor (toAfferentColor color)
    CanvasM.fillPath afferentPath

  | .fillPathStyle path style =>
    let afferentPath := toAfferentPath path
    CanvasM.save
    CanvasM.setFillStyle style
    CanvasM.fillPath afferentPath
    CanvasM.restore

  | .strokePath path color lineWidth =>
    let afferentPath := toAfferentPath path
    CanvasM.setStrokeColor (toAfferentColor color)
    CanvasM.setLineWidth lineWidth
    CanvasM.strokePath afferentPath

  | .fillPolygonInstanced pathHash vertices indices instances centerX centerY =>
    if instances.size == 0 then
      pure ()
    else
      -- Get or create cached mesh
      let canvas ← CanvasM.getCanvas
      let (mesh, canvas) ← do
        match canvas.meshCache.get? pathHash with
        | some mesh => pure (mesh, canvas)
        | none =>
          -- Create new cached mesh
          let mesh ← FFI.MeshCache.create canvas.ctx.renderer vertices indices centerX centerY
          let cache := canvas.meshCache.insert pathHash mesh
          pure (mesh, { canvas with meshCache := cache })

      -- Ensure instance buffer has enough capacity (8 floats per instance)
      let requiredFloats := instances.size * 8
      let (buf, cap, canvas) ←
        match canvas.meshInstanceBuffer with
        | some buf =>
          if canvas.meshInstanceBufferCapacity >= requiredFloats then
            pure (buf, canvas.meshInstanceBufferCapacity, canvas)
          else
            FFI.FloatBuffer.destroy buf
            let newBuf ← FFI.FloatBuffer.create requiredFloats.toUSize
            pure (newBuf, requiredFloats, { canvas with meshInstanceBuffer := some newBuf, meshInstanceBufferCapacity := requiredFloats })
        | none =>
          let newBuf ← FFI.FloatBuffer.create requiredFloats.toUSize
          pure (newBuf, requiredFloats, { canvas with meshInstanceBuffer := some newBuf, meshInstanceBufferCapacity := requiredFloats })

      -- Write instance data to buffer
      let mut idx : USize := 0
      for inst in instances do
        buf.setVec8 idx inst.x inst.y inst.rotation inst.scale inst.r inst.g inst.b inst.a
        idx := idx + 8

      CanvasM.setCanvas canvas

      -- Draw all instances
      let (canvasWidth, canvasHeight) ← canvas.ctx.getCurrentSize
      FFI.MeshCache.drawInstancedBuffer canvas.ctx.renderer mesh buf instances.size.toUInt32 canvasWidth canvasHeight

  | .strokeArcInstanced instances segments =>
    if instances.size == 0 then
      pure ()
    else
      let canvas ← CanvasM.getCanvas
      let (canvasWidth, canvasHeight) ← canvas.ctx.getCurrentSize
      -- Pack arc instances into Float array: 10 floats per instance
      let data := instances.foldl (init := #[]) fun acc inst =>
        acc.push inst.centerX |>.push inst.centerY
           |>.push inst.startAngle |>.push inst.sweepAngle
           |>.push inst.radius |>.push inst.strokeWidth
           |>.push inst.r |>.push inst.g |>.push inst.b |>.push inst.a
      canvas.ctx.renderer.drawArcInstanced data instances.size.toUInt32 segments.toUInt32 canvasWidth canvasHeight

  | .drawFragment fragmentHash _primitiveType params _instanceCount =>
    -- Get pipeline cache from canvas
    let canvas ← CanvasM.getCanvas
    let cache ← canvas.fragmentCache.get

    -- Get or compile the fragment pipeline using global registry
    let (maybePipeline, newCache) ← Shader.getOrCompileGlobal cache canvas.ctx.renderer fragmentHash

    -- Update cache if changed
    canvas.fragmentCache.set newCache

    -- Draw if we have a pipeline
    match maybePipeline with
    | some pipeline =>
      let (canvasWidth, canvasHeight) ← canvas.ctx.getCurrentSize
      FFI.Fragment.draw canvas.ctx.renderer pipeline params canvasWidth canvasHeight
    | none =>
      -- Pipeline compilation failed or fragment not registered
      pure ()

  | .pushClip rect =>
    let afferentRect := toAfferentRect rect
    CanvasM.clip afferentRect

  | .popClip =>
    CanvasM.unclip

  | .pushTranslate dx dy =>
    CanvasM.save
    CanvasM.translate dx dy

  | .pushRotate angle =>
    CanvasM.save
    CanvasM.rotate angle

  | .pushScale sx sy =>
    CanvasM.save
    CanvasM.scale sx sy

  | .popTransform =>
    CanvasM.restore

  | .save =>
    CanvasM.save

  | .restore =>
    CanvasM.restore

/-! ## Command Batching

Batching groups consecutive fillRect commands into a single GPU draw call,
with per-instance cornerRadius data to avoid splitting batches on radius.
This dramatically improves performance for charts that draw many rectangles
(heatmaps, bar charts, scatter plots, etc.).

Example: A 20x20 heatmap generates 400 fillRect commands.
- Without batching: 400 separate draw calls
- With batching: 1 batched draw call
-/

/-- Statistics from batched command execution. -/
structure BatchStats where
  /-- Number of batched draw calls (multiple rects in one call). -/
  batchedCalls : Nat := 0
  /-- Number of individual draw calls (non-batchable commands). -/
  individualCalls : Nat := 0
  /-- Total commands processed. -/
  totalCommands : Nat := 0
  /-- Number of rects batched. -/
  rectsBatched : Nat := 0
  /-- Number of circles batched. -/
  circlesBatched : Nat := 0
  /-- Number of stroke rects batched. -/
  strokeRectsBatched : Nat := 0
  /-- Number of lines batched. -/
  linesBatched : Nat := 0
  /-- Number of texts batched. -/
  textsBatched : Nat := 0
  /-- Time spent computing bounded commands (transform flattening) in ms. -/
  timeFlattenMs : Float := 0.0
  /-- Time spent coalescing/sorting commands in ms. -/
  timeCoalesceMs : Float := 0.0
  /-- Time spent in main batching loop (building batch arrays) in ms. -/
  timeBatchLoopMs : Float := 0.0
  /-- Time spent executing draw calls (FFI to native) in ms. -/
  timeDrawCallsMs : Float := 0.0
  deriving Repr, Inhabited

/-- Entry for a batched rectangle. -/
structure RectBatchEntry where
  x : Float
  y : Float
  width : Float
  height : Float
  r : Float
  g : Float
  b : Float
  a : Float
  cornerRadius : Float

/-- Entry for a batched circle.
    Format: [centerX, centerY, radius, padding, r, g, b, a, padding] (9 floats) -/
structure CircleBatchEntry where
  centerX : Float
  centerY : Float
  radius : Float
  r : Float
  g : Float
  b : Float
  a : Float

structure StrokeCircleBatchEntry where
  centerX : Float
  centerY : Float
  radius : Float
  r : Float
  g : Float
  b : Float
  a : Float

/-- Entry for a batched stroked rectangle.
    Format: [x, y, width, height, r, g, b, a, cornerRadius] (9 floats) -/
structure StrokeRectBatchEntry where
  x : Float
  y : Float
  width : Float
  height : Float
  r : Float
  g : Float
  b : Float
  a : Float
  cornerRadius : Float

/-- Entry for a batched line segment.
    Format: [x1, y1, x2, y2, r, g, b, a, padding] (9 floats) -/
structure LineBatchEntry where
  x1 : Float
  y1 : Float
  x2 : Float
  y2 : Float
  r : Float
  g : Float
  b : Float
  a : Float

/-- Entry for batched text rendering.
    Includes per-entry transform for rotated/scaled text. -/
structure TextBatchEntry where
  text : String
  x : Float
  y : Float
  r : Float
  g : Float
  b : Float
  a : Float
  /-- 2D affine transform: [a, b, c, d, tx, ty] -/
  transform : Array Float

/-! ## Command Coalescing

Coalescing reorders commands within safe boundaries to maximize batching.
Commands are grouped into "scopes" delimited by state-changing commands
(save/restore, clips, transforms). Within each scope, commands are sorted
by type to ensure all fillRects are consecutive.

This enables batching even when fillRect commands are interleaved with
other command types in the original stream (common in charts where
grid lines, data, and axis rectangles are separated by text/path commands).
-/

/-- Bins for grouping commands by type within a scope. -/
structure CommandBins where
  fillRects : Array RenderCommand := #[]
  fillRectStyles : Array RenderCommand := #[]
  strokeRects : Array RenderCommand := #[]
  fillCircles : Array RenderCommand := #[]
  strokeCircles : Array RenderCommand := #[]
  fillPolygons : Array RenderCommand := #[]
  fillPaths : Array RenderCommand := #[]
  fillPathStyles : Array RenderCommand := #[]
  strokePolygons : Array RenderCommand := #[]
  strokePaths : Array RenderCommand := #[]
  texts : Array RenderCommand := #[]

/-- Check if a command changes graphics state (scope boundary). -/
def isStateChanging (cmd : RenderCommand) : Bool :=
  match cmd with
  | .save | .restore => true
  | .pushClip _ | .popClip => true
  | .pushTranslate _ _ | .pushRotate _ | .pushScale _ _ | .popTransform => true
  | _ => false

/-- Add a command to the appropriate bin. -/
def CommandBins.add (bins : CommandBins) (cmd : RenderCommand) : CommandBins :=
  match cmd with
  | .fillRect .. => { bins with fillRects := bins.fillRects.push cmd }
  | .fillRectStyle .. => { bins with fillRectStyles := bins.fillRectStyles.push cmd }
  | .strokeRect .. => { bins with strokeRects := bins.strokeRects.push cmd }
  | .fillCircle .. => { bins with fillCircles := bins.fillCircles.push cmd }
  | .strokeCircle .. => { bins with strokeCircles := bins.strokeCircles.push cmd }
  | .fillPolygon .. => { bins with fillPolygons := bins.fillPolygons.push cmd }
  | .fillPath .. => { bins with fillPaths := bins.fillPaths.push cmd }
  | .fillPathStyle .. => { bins with fillPathStyles := bins.fillPathStyles.push cmd }
  | .strokePolygon .. => { bins with strokePolygons := bins.strokePolygons.push cmd }
  | .strokePath .. => { bins with strokePaths := bins.strokePaths.push cmd }
  | .fillText .. | .fillTextBlock .. => { bins with texts := bins.texts.push cmd }
  | _ => bins  -- State-changing commands handled separately

/-- Flatten bins into command array in optimal batching order.
    Order: fills first (backgrounds), then strokes (outlines), then text (labels on top). -/
def CommandBins.flush (bins : CommandBins) : Array RenderCommand :=
  bins.fillRects ++ bins.fillRectStyles ++ bins.strokeRects ++
  bins.fillCircles ++ bins.strokeCircles ++
  bins.fillPolygons ++ bins.fillPaths ++ bins.fillPathStyles ++
  bins.strokePolygons ++ bins.strokePaths ++ bins.texts

/-- Reorder commands within scopes to maximize batching.
    Scopes are delimited by state-changing commands (save/restore, clips, transforms).
    Within each scope, commands are grouped by type in optimal batching order:
    fillRects first (all batch together), then other fills, strokes, and text last.

    This preserves visual correctness for non-overlapping elements while enabling
    significantly better batching for charts and UI layouts. -/
def coalesceCommands (cmds : Array RenderCommand) : Array RenderCommand := Id.run do
  let mut result : Array RenderCommand := #[]
  let mut bins : CommandBins := {}

  for cmd in cmds do
    if isStateChanging cmd then
      -- Flush current scope, emit state command, start new scope
      result := result ++ bins.flush
      result := result.push cmd
      bins := {}
    else
      bins := bins.add cmd

  -- Flush final scope
  result ++ bins.flush

/-! ## Overlap-Aware Command Coalescing

Advanced coalescing that analyzes command bounds to determine which commands
can be safely reordered across scope boundaries. Non-overlapping commands
can be batched together even if they were originally in different scopes.

The key insight is that for non-overlapping UI elements (common in flex layouts),
z-order doesn't affect visual output, so we can freely reorder by type.
-/

/-- Convert Arbor.Point to Afferent.Point for transform application. -/
private def arborToAfferentPt (p : Afferent.Arbor.Point) : Afferent.Point :=
  ⟨p.x, p.y⟩

/-- Convert Afferent.Point to Arbor.Point after transform. -/
private def afferentToArborPt (p : Afferent.Point) : Afferent.Arbor.Point :=
  ⟨p.x, p.y⟩

/-- Compute screen-space bounds for a render command.
    Returns None for state-changing commands that don't have spatial extent. -/
def computeBounds (cmd : RenderCommand) (transform : Transform) : Option CommandBounds :=
  match cmd with
  | .fillRect rect _ _ =>
      let p := transform.apply (arborToAfferentPt rect.origin)
      some (CommandBounds.fromRect p.x p.y rect.size.width rect.size.height)
  | .fillRectStyle rect _ _ =>
      let p := transform.apply (arborToAfferentPt rect.origin)
      some (CommandBounds.fromRect p.x p.y rect.size.width rect.size.height)
  | .strokeRect rect _ _ _ =>
      let p := transform.apply (arborToAfferentPt rect.origin)
      some (CommandBounds.fromRect p.x p.y rect.size.width rect.size.height)
  | .fillCircle center radius _ =>
      let p := transform.apply (arborToAfferentPt center)
      some (CommandBounds.fromCircle p.x p.y radius)
  | .strokeCircle center radius _ _ =>
      let p := transform.apply (arborToAfferentPt center)
      some (CommandBounds.fromCircle p.x p.y radius)
  | .fillText _ x y _ _ =>
      let p := transform.apply ⟨x, y⟩
      -- Approximate text bounds (conservative estimate)
      some { minX := p.x, minY := p.y - 20, maxX := p.x + 200, maxY := p.y + 5 }
  | .fillTextBlock _ rect _ _ _ _ =>
      let p := transform.apply (arborToAfferentPt rect.origin)
      some (CommandBounds.fromRect p.x p.y rect.size.width rect.size.height)
  | .fillPolygon points _ =>
      if points.isEmpty then none
      else
        let transformed := points.map (fun pt => transform.apply (arborToAfferentPt pt))
        let minX := transformed.foldl (fun acc p => min acc p.x) transformed[0]!.x
        let maxX := transformed.foldl (fun acc p => max acc p.x) transformed[0]!.x
        let minY := transformed.foldl (fun acc p => min acc p.y) transformed[0]!.y
        let maxY := transformed.foldl (fun acc p => max acc p.y) transformed[0]!.y
        some { minX, minY, maxX, maxY }
  | .strokePolygon points _ _ =>
      if points.isEmpty then none
      else
        let transformed := points.map (fun pt => transform.apply (arborToAfferentPt pt))
        let minX := transformed.foldl (fun acc p => min acc p.x) transformed[0]!.x
        let maxX := transformed.foldl (fun acc p => max acc p.x) transformed[0]!.x
        let minY := transformed.foldl (fun acc p => min acc p.y) transformed[0]!.y
        let maxY := transformed.foldl (fun acc p => max acc p.y) transformed[0]!.y
        some { minX, minY, maxX, maxY }
  | .strokeLine p1 p2 _ _ =>
      let tp1 := transform.apply (arborToAfferentPt p1)
      let tp2 := transform.apply (arborToAfferentPt p2)
      let minX := min tp1.x tp2.x
      let maxX := max tp1.x tp2.x
      let minY := min tp1.y tp2.y
      let maxY := max tp1.y tp2.y
      some { minX, minY, maxX, maxY }
  | _ => none  -- State-changing commands have no bounds

/-- Check if a path is a simple line (moveTo + lineTo only).
    Returns the two endpoints if so. -/
def isSimpleLine (path : Afferent.Arbor.Path) : Option (Afferent.Arbor.Point × Afferent.Arbor.Point) :=
  if path.commands.size == 2 then
    match path.commands[0]?, path.commands[1]? with
    | some (Afferent.Arbor.PathCommand.moveTo p1), some (Afferent.Arbor.PathCommand.lineTo p2) => some (p1, p2)
    | _, _ => none
  else
    none

/-- Flatten a command to absolute screen coordinates if possible.
    For simple geometry (rects, circles), applies the transform to get absolute positions.
    Simple line paths (moveTo + lineTo) are converted to strokeLine commands.
    Returns the (possibly modified) command and its screen-space bounds. -/
def flattenCommand (cmd : RenderCommand) (transform : Transform)
    : RenderCommand × Option CommandBounds :=
  -- Handle simple line paths specially (even with identity transform)
  -- so they can be batched
  match cmd with
  | .strokeLineBatch data count lineWidth =>
      Id.run do
        if count == 0 then
          return (.strokeLineBatch data count lineWidth, none)
        if transform == Transform.identity then
          let x1 := data[0]!
          let y1 := data[1]!
          let x2 := data[2]!
          let y2 := data[3]!
          let mut minX := min x1 x2
          let mut minY := min y1 y2
          let mut maxX := max x1 x2
          let mut maxY := max y1 y2
          for i in [1:count] do
            let base := i * 9
            let lx1 := data[base]!
            let ly1 := data[base + 1]!
            let lx2 := data[base + 2]!
            let ly2 := data[base + 3]!
            minX := min minX (min lx1 lx2)
            minY := min minY (min ly1 ly2)
            maxX := max maxX (max lx1 lx2)
            maxY := max maxY (max ly1 ly2)
          let bounds := some { minX, minY, maxX, maxY : CommandBounds }
          return (.strokeLineBatch data count lineWidth, bounds)
        let x1 := data[0]!
        let y1 := data[1]!
        let x2 := data[2]!
        let y2 := data[3]!
        let tp1 := transform.apply ⟨x1, y1⟩
        let tp2 := transform.apply ⟨x2, y2⟩
        let mut minX := min tp1.x tp2.x
        let mut minY := min tp1.y tp2.y
        let mut maxX := max tp1.x tp2.x
        let mut maxY := max tp1.y tp2.y
        let mut out : Array Float := Array.mkEmpty data.size
        let r0 := data[4]!
        let g0 := data[5]!
        let b0 := data[6]!
        let a0 := data[7]!
        let p0 := data[8]!
        out := out.push tp1.x |>.push tp1.y |>.push tp2.x |>.push tp2.y
                 |>.push r0 |>.push g0 |>.push b0 |>.push a0
                 |>.push p0
        for i in [1:count] do
          let base := i * 9
          let lx1 := data[base]!
          let ly1 := data[base + 1]!
          let lx2 := data[base + 2]!
          let ly2 := data[base + 3]!
          let tp1 := transform.apply ⟨lx1, ly1⟩
          let tp2 := transform.apply ⟨lx2, ly2⟩
          minX := min minX (min tp1.x tp2.x)
          minY := min minY (min tp1.y tp2.y)
          maxX := max maxX (max tp1.x tp2.x)
          maxY := max maxY (max tp1.y tp2.y)
          let r := data[base + 4]!
          let g := data[base + 5]!
          let b := data[base + 6]!
          let a := data[base + 7]!
          let p := data[base + 8]!
          out := out.push tp1.x |>.push tp1.y |>.push tp2.x |>.push tp2.y
                   |>.push r |>.push g |>.push b |>.push a
                   |>.push p
        let bounds := some { minX, minY, maxX, maxY : CommandBounds }
        return (.strokeLineBatch out count lineWidth, bounds)
  | .fillCircleBatch data count =>
      Id.run do
        if count == 0 then
          return (.fillCircleBatch data count, none)
        if transform == Transform.identity then
          -- No transform needed, just compute bounds
          let cx0 := data[0]!
          let cy0 := data[1]!
          let r0 := data[2]!
          let mut minX := cx0 - r0
          let mut minY := cy0 - r0
          let mut maxX := cx0 + r0
          let mut maxY := cy0 + r0
          for i in [1:count] do
            let base := i * 7
            let cx := data[base]!
            let cy := data[base + 1]!
            let radius := data[base + 2]!
            minX := min minX (cx - radius)
            minY := min minY (cy - radius)
            maxX := max maxX (cx + radius)
            maxY := max maxY (cy + radius)
          let bounds := some { minX, minY, maxX, maxY : CommandBounds }
          return (.fillCircleBatch data count, bounds)
        -- Transform all circle centers
        let cx0 := data[0]!
        let cy0 := data[1]!
        let r0 := data[2]!
        let tc0 := transform.apply ⟨cx0, cy0⟩
        let mut minX := tc0.x - r0
        let mut minY := tc0.y - r0
        let mut maxX := tc0.x + r0
        let mut maxY := tc0.y + r0
        let mut out : Array Float := Array.mkEmpty data.size
        out := out.push tc0.x |>.push tc0.y |>.push r0
                 |>.push data[3]! |>.push data[4]! |>.push data[5]! |>.push data[6]!
        for i in [1:count] do
          let base := i * 7
          let cx := data[base]!
          let cy := data[base + 1]!
          let radius := data[base + 2]!
          let tc := transform.apply ⟨cx, cy⟩
          minX := min minX (tc.x - radius)
          minY := min minY (tc.y - radius)
          maxX := max maxX (tc.x + radius)
          maxY := max maxY (tc.y + radius)
          out := out.push tc.x |>.push tc.y |>.push radius
                   |>.push data[base + 3]! |>.push data[base + 4]!
                   |>.push data[base + 5]! |>.push data[base + 6]!
        let bounds := some { minX, minY, maxX, maxY : CommandBounds }
        return (.fillCircleBatch out count, bounds)
  | .strokePath path color lw =>
      match isSimpleLine path with
      | some (p1, p2) =>
          if transform == Transform.identity then
            let minX := min p1.x p2.x
            let minY := min p1.y p2.y
            let maxX := max p1.x p2.x
            let maxY := max p1.y p2.y
            let bounds := some { minX, minY, maxX, maxY : CommandBounds }
            (.strokeLine p1 p2 color lw, bounds)
          else
            let absP1 := transform.apply (arborToAfferentPt p1)
            let absP2 := transform.apply (arborToAfferentPt p2)
            let arborP1 := afferentToArborPt absP1
            let arborP2 := afferentToArborPt absP2
            let minX := min absP1.x absP2.x
            let minY := min absP1.y absP2.y
            let maxX := max absP1.x absP2.x
            let maxY := max absP1.y absP2.y
            let bounds := some { minX, minY, maxX, maxY : CommandBounds }
            (.strokeLine arborP1 arborP2 color lw, bounds)
      | none =>
          (cmd, computeBounds cmd transform)
  | _ =>
  if transform == Transform.identity then
    -- No transform needed, just compute bounds
    (cmd, computeBounds cmd transform)
  else
    match cmd with
    | .fillRect rect color cr =>
        let topLeft := transform.apply (arborToAfferentPt rect.origin)
        let arborTopLeft := afferentToArborPt topLeft
        let size := rect.size
        -- For non-rotated transforms, we can flatten to absolute coords
        let absRect : Afferent.Arbor.Rect := ⟨arborTopLeft, size⟩
        let bounds := CommandBounds.fromRect topLeft.x topLeft.y size.width size.height
        (.fillRect absRect color cr, some bounds)
    | .fillRectStyle rect style cr =>
        let topLeft := transform.apply (arborToAfferentPt rect.origin)
        let arborTopLeft := afferentToArborPt topLeft
        let size := rect.size
        let absRect : Afferent.Arbor.Rect := ⟨arborTopLeft, size⟩
        let bounds := CommandBounds.fromRect topLeft.x topLeft.y size.width size.height
        (.fillRectStyle absRect style cr, some bounds)
    | .strokeRect rect color lw cr =>
        let topLeft := transform.apply (arborToAfferentPt rect.origin)
        let arborTopLeft := afferentToArborPt topLeft
        let size := rect.size
        let absRect : Afferent.Arbor.Rect := ⟨arborTopLeft, size⟩
        let bounds := CommandBounds.fromRect topLeft.x topLeft.y size.width size.height
        (.strokeRect absRect color lw cr, some bounds)
    | .fillCircle center radius color =>
        let absCenter := transform.apply (arborToAfferentPt center)
        let arborCenter := afferentToArborPt absCenter
        let bounds := CommandBounds.fromCircle absCenter.x absCenter.y radius
        (.fillCircle arborCenter radius color, some bounds)
    | .strokeCircle center radius color lw =>
        let absCenter := transform.apply (arborToAfferentPt center)
        let arborCenter := afferentToArborPt absCenter
        let bounds := CommandBounds.fromCircle absCenter.x absCenter.y radius
        (.strokeCircle arborCenter radius color lw, some bounds)
    | _ =>
        -- For other commands (text, paths), keep as-is with computed bounds
        -- Text captures its transform during batch creation, so it works correctly
        (cmd, computeBounds cmd transform)

/-- Compute bounded commands by replaying transform state through command stream.
    Also flattens simple geometry (rects, circles) to absolute coordinates. -/
def computeBoundedCommands (cmds : Array RenderCommand) : Array BoundedCommand := Id.run do
  let mut result : Array BoundedCommand := #[]
  let mut transformStack : Array Transform := #[Transform.identity]
  let mut idx := 0

  for cmd in cmds do
    let transform := transformStack.back?.getD Transform.identity

    -- Update transform state for state-changing commands
    match cmd with
    | .pushTranslate dx dy =>
        let current := transformStack.back?.getD Transform.identity
        transformStack := transformStack.push (current.translated dx dy)
    | .pushScale sx sy =>
        let current := transformStack.back?.getD Transform.identity
        transformStack := transformStack.push (current.scaled sx sy)
    | .pushRotate angle =>
        let current := transformStack.back?.getD Transform.identity
        transformStack := transformStack.push (current.rotated angle)
    | .popTransform =>
        if transformStack.size > 1 then
          transformStack := transformStack.pop
    | .save =>
        -- save duplicates current transform (so restore pops back to it)
        transformStack := transformStack.push (transformStack.back?.getD Transform.identity)
    | .restore =>
        if transformStack.size > 1 then
          transformStack := transformStack.pop
    | _ => pure ()

    -- Flatten simple geometry to absolute coordinates
    let (flatCmd, bounds) := flattenCommand cmd transform
    result := result.push { cmd := flatCmd, bounds := bounds, originalIndex := idx }
    idx := idx + 1

  result

/-- Coalesce commands by grouping same-category commands together using bucket sort.
    O(N) bucket sort by category priority, preserving original order within each bucket.

    After transform flattening, simple geometry (rects, circles) is in
    absolute coordinates and doesn't depend on transform state.
    Text captures its transform during batching (TextBatchEntry.transform). -/
def coalesceByCategory (bounded : Array BoundedCommand) : Array RenderCommand := Id.run do
  if bounded.isEmpty then return #[]

  -- 7 buckets for priorities 0-6: fillRect, fillCircle, strokeRect, strokeCircle, strokeLine, fillText, other
  let mut bucket0 : Array RenderCommand := #[]  -- fillRect
  let mut bucket1 : Array RenderCommand := #[]  -- fillCircle
  let mut bucket2 : Array RenderCommand := #[]  -- strokeRect
  let mut bucket3 : Array RenderCommand := #[]  -- strokeCircle
  let mut bucket4 : Array RenderCommand := #[]  -- strokeLine
  let mut bucket5 : Array RenderCommand := #[]  -- fillText
  let mut bucket6 : Array RenderCommand := #[]  -- other

  -- Distribute into buckets (O(N))
  for bc in bounded do
    match bc.cmd.category.sortPriority with
    | 0 => bucket0 := bucket0.push bc.cmd
    | 1 => bucket1 := bucket1.push bc.cmd
    | 2 => bucket2 := bucket2.push bc.cmd
    | 3 => bucket3 := bucket3.push bc.cmd
    | 4 => bucket4 := bucket4.push bc.cmd
    | 5 => bucket5 := bucket5.push bc.cmd
    | _ => bucket6 := bucket6.push bc.cmd

  -- Concatenate buckets in priority order
  -- Pre-allocate output array for efficiency
  let totalSize := bucket0.size + bucket1.size + bucket2.size + bucket3.size +
                   bucket4.size + bucket5.size + bucket6.size
  let mut out : Array RenderCommand := Array.mkEmpty totalSize
  for cmd in bucket0 do out := out.push cmd
  for cmd in bucket1 do out := out.push cmd
  for cmd in bucket2 do out := out.push cmd
  for cmd in bucket3 do out := out.push cmd
  for cmd in bucket4 do out := out.push cmd
  for cmd in bucket5 do out := out.push cmd
  for cmd in bucket6 do out := out.push cmd
  out

/-- Coalesce commands by category while preserving stateful command order.
    Splits at any non-batchable ("other") command so transforms/clips apply.
    Uses bucket sort within each segment for O(N) performance. -/
def coalesceByCategoryWithClip (bounded : Array BoundedCommand) : Array RenderCommand := Id.run do
  if bounded.isEmpty then return #[]

  -- Pre-allocate output array (at most bounded.size commands)
  let mut out : Array RenderCommand := Array.mkEmpty bounded.size

  -- Temporary buckets for current segment (reused across segments)
  -- Priority: fillRect(0), fillCircle(1), strokeRect(2), strokeCircle(3), strokeLine(4),
  --           strokeArcInstanced(5), fillText(6), fillPolygonInstanced(7)
  let mut bucket0 : Array RenderCommand := #[]
  let mut bucket1 : Array RenderCommand := #[]
  let mut bucket2 : Array RenderCommand := #[]
  let mut bucket3 : Array RenderCommand := #[]
  let mut bucket4 : Array RenderCommand := #[]
  let mut bucket5 : Array RenderCommand := #[]
  let mut bucket6 : Array RenderCommand := #[]
  let mut bucket7 : Array RenderCommand := #[]

  for bc in bounded do
    if bc.cmd.category == .other then
      -- Flush accumulated buckets before the "other" command
      for cmd in bucket0 do out := out.push cmd
      for cmd in bucket1 do out := out.push cmd
      for cmd in bucket2 do out := out.push cmd
      for cmd in bucket3 do out := out.push cmd
      for cmd in bucket4 do out := out.push cmd
      for cmd in bucket5 do out := out.push cmd
      for cmd in bucket6 do out := out.push cmd
      for cmd in bucket7 do out := out.push cmd
      bucket0 := #[]; bucket1 := #[]; bucket2 := #[]
      bucket3 := #[]; bucket4 := #[]; bucket5 := #[]
      bucket6 := #[]; bucket7 := #[]
      -- Add the "other" command directly
      out := out.push bc.cmd
    else
      -- Distribute into buckets
      match bc.cmd.category.sortPriority with
      | 0 => bucket0 := bucket0.push bc.cmd
      | 1 => bucket1 := bucket1.push bc.cmd
      | 2 => bucket2 := bucket2.push bc.cmd
      | 3 => bucket3 := bucket3.push bc.cmd
      | 4 => bucket4 := bucket4.push bc.cmd
      | 5 => bucket5 := bucket5.push bc.cmd  -- strokeArcInstanced
      | 6 => bucket6 := bucket6.push bc.cmd  -- fillText
      | _ => bucket7 := bucket7.push bc.cmd  -- fillPolygonInstanced

  -- Flush any remaining commands
  for cmd in bucket0 do out := out.push cmd
  for cmd in bucket1 do out := out.push cmd
  for cmd in bucket2 do out := out.push cmd
  for cmd in bucket3 do out := out.push cmd
  for cmd in bucket4 do out := out.push cmd
  for cmd in bucket5 do out := out.push cmd
  for cmd in bucket6 do out := out.push cmd
  for cmd in bucket7 do out := out.push cmd
  out

/-- Ensure a FloatBuffer has at least the required capacity, reusing or growing as needed. -/
private def ensureBufferCapacity (bufOpt : Option FFI.FloatBuffer) (currentCap : Nat) (required : Nat)
    : IO (FFI.FloatBuffer × Nat) := do
  match bufOpt with
  | some buf =>
    if currentCap >= required then
      pure (buf, currentCap)
    else
      FFI.FloatBuffer.destroy buf
      let newBuf ← FFI.FloatBuffer.create required.toUSize
      pure (newBuf, required)
  | none =>
    let newBuf ← FFI.FloatBuffer.create required.toUSize
    pure (newBuf, required)

/-- Execute a batch of fillRect commands in a single draw call using FloatBuffer. -/
def executeFillRectBatch (rects : Array RectBatchEntry) : CanvasM Unit := do
  if rects.isEmpty then return
  let canvas ← CanvasM.getCanvas
  let (canvasWidth, canvasHeight) ← canvas.ctx.getCurrentSize

  -- Ensure rectBuffer has enough capacity (9 floats per rect)
  let requiredFloats := rects.size * 9
  let (buf, newCap) ← ensureBufferCapacity canvas.rectBuffer canvas.rectBufferCapacity requiredFloats
  CanvasM.setCanvas { canvas with rectBuffer := some buf, rectBufferCapacity := newCap }

  -- Write directly to FloatBuffer (O(1) per write, no array allocation)
  let mut idx : USize := 0
  for entry in rects do
    buf.setVec9 idx entry.x entry.y entry.width entry.height
                    entry.r entry.g entry.b entry.a entry.cornerRadius
    idx := idx + 9

  canvas.ctx.renderer.drawBatchBuffer 0 buf rects.size.toUInt32 0.0 0.0
    canvasWidth canvasHeight

/-- Execute a batch of fillCircle commands in a single draw call using FloatBuffer. -/
def executeFillCircleBatch (circles : Array CircleBatchEntry) : CanvasM Unit := do
  if circles.isEmpty then return
  let canvas ← CanvasM.getCanvas
  let (canvasWidth, canvasHeight) ← canvas.ctx.getCurrentSize

  -- Ensure circleBuffer has enough capacity (9 floats per circle)
  let requiredFloats := circles.size * 9
  let (buf, newCap) ← ensureBufferCapacity canvas.circleBuffer canvas.circleBufferCapacity requiredFloats
  CanvasM.setCanvas { canvas with circleBuffer := some buf, circleBufferCapacity := newCap }

  -- Write directly to FloatBuffer
  let mut idx : USize := 0
  for entry in circles do
    let size := entry.radius * 2.0
    let x := entry.centerX - entry.radius
    let y := entry.centerY - entry.radius
    buf.setVec9 idx x y size size entry.r entry.g entry.b entry.a 0.0
    idx := idx + 9

  canvas.ctx.renderer.drawBatchBuffer 1 buf circles.size.toUInt32 0.0 0.0
    canvasWidth canvasHeight

/-- Execute a batch of strokeRect commands in a single draw call using FloatBuffer. -/
def executeStrokeRectBatch (rects : Array StrokeRectBatchEntry) (lineWidth : Float) : CanvasM Unit := do
  if rects.isEmpty then return
  let canvas ← CanvasM.getCanvas
  let (canvasWidth, canvasHeight) ← canvas.ctx.getCurrentSize

  -- Ensure strokeRectBuffer has enough capacity (9 floats per rect)
  let requiredFloats := rects.size * 9
  let (buf, newCap) ← ensureBufferCapacity canvas.strokeRectBuffer canvas.strokeRectBufferCapacity requiredFloats
  CanvasM.setCanvas { canvas with strokeRectBuffer := some buf, strokeRectBufferCapacity := newCap }

  -- Write directly to FloatBuffer
  let mut idx : USize := 0
  for entry in rects do
    buf.setVec9 idx entry.x entry.y entry.width entry.height
                    entry.r entry.g entry.b entry.a entry.cornerRadius
    idx := idx + 9

  canvas.ctx.renderer.drawBatchBuffer 2 buf rects.size.toUInt32 lineWidth 0.0
    canvasWidth canvasHeight

/-- Execute a batch of strokeCircle commands in a single draw call using FloatBuffer. -/
def executeStrokeCircleBatch (circles : Array StrokeCircleBatchEntry) (lineWidth : Float) : CanvasM Unit := do
  if circles.isEmpty then return
  let canvas ← CanvasM.getCanvas
  let (canvasWidth, canvasHeight) ← canvas.ctx.getCurrentSize

  -- Ensure strokeCircleBuffer has enough capacity (9 floats per circle)
  let requiredFloats := circles.size * 9
  let (buf, newCap) ← ensureBufferCapacity canvas.strokeCircleBuffer canvas.strokeCircleBufferCapacity requiredFloats
  CanvasM.setCanvas { canvas with strokeCircleBuffer := some buf, strokeCircleBufferCapacity := newCap }

  -- Write directly to FloatBuffer
  -- Format: [x, y, width, height, r, g, b, a, 0] where width=height=diameter
  let mut idx : USize := 0
  for entry in circles do
    let diameter := entry.radius * 2.0
    let x := entry.centerX - entry.radius
    let y := entry.centerY - entry.radius
    buf.setVec9 idx x y diameter diameter entry.r entry.g entry.b entry.a 0.0
    idx := idx + 9

  -- kind 4 = strokeCircle, param0 = lineWidth
  canvas.ctx.renderer.drawBatchBuffer 4 buf circles.size.toUInt32 lineWidth 0.0
    canvasWidth canvasHeight

/-- Execute a batch of strokeLine commands in a single draw call. -/
def executeLineBatch (lines : Array LineBatchEntry) (lineWidth : Float) : CanvasM Unit := do
  if lines.isEmpty then return
  let canvas ← CanvasM.getCanvas
  let (canvasWidth, canvasHeight) ← canvas.ctx.getCurrentSize
  -- Pack into Float array: [x1, y1, x2, y2, r, g, b, a, padding] per line
  let data := lines.foldl (init := #[]) fun acc entry =>
    acc.push entry.x1 |>.push entry.y1 |>.push entry.x2 |>.push entry.y2
       |>.push entry.r |>.push entry.g |>.push entry.b |>.push entry.a
       |>.push 0.0
  canvas.ctx.renderer.drawLineBatch data lines.size.toUInt32 lineWidth
    canvasWidth canvasHeight

/-- Execute line batch from FloatBuffer (high-performance path). -/
def executeLineBatchFromBuffer (buf : FFI.FloatBuffer) (count : Nat) (lineWidth : Float) : CanvasM Unit := do
  if count == 0 then return
  let canvas ← CanvasM.getCanvas
  let (canvasWidth, canvasHeight) ← canvas.ctx.getCurrentSize
  canvas.ctx.renderer.drawLineBatchBuffer buf count.toUInt32 lineWidth canvasWidth canvasHeight

/-- Execute strokeLine commands directly from RenderCommand array, avoiding intermediate structures. -/
def executeLineCommandsDirect (cmds : Array RenderCommand) (startIdx endIdx : Nat) : CanvasM Nat := do
  if startIdx >= endIdx then return 0
  let canvas ← CanvasM.getCanvas
  let (canvasWidth, canvasHeight) ← canvas.ctx.getCurrentSize
  -- First pass: find all lines with same lineWidth and count them
  let mut i := startIdx
  let mut lineWidth : Float := 0.0
  let mut count : Nat := 0
  -- Get lineWidth from first command
  if let some (.strokeLine _ _ _ lw) := cmds[startIdx]? then
    lineWidth := lw
  -- Count consecutive lines with same lineWidth
  while h : i < endIdx do
    if let some (.strokeLine _ _ _ lw) := cmds[i]? then
      if count == 0 || lw == lineWidth then
        count := count + 1
        i := i + 1
      else
        break
    else
      break
  -- Build Float array directly (9 floats per line)
  let mut data : Array Float := Array.mkEmpty (count * 9)
  i := startIdx
  let endI := startIdx + count
  while h : i < endI do
    if let some (.strokeLine p1 p2 color _) := cmds[i]? then
      data := data.push p1.x |>.push p1.y |>.push p2.x |>.push p2.y
              |>.push color.r |>.push color.g |>.push color.b |>.push color.a
              |>.push 0.0
    i := i + 1
  if count > 0 then
    canvas.ctx.renderer.drawLineBatch data count.toUInt32 lineWidth canvasWidth canvasHeight
  return count

/-- Execute a batch of fillText commands with the same font in a single draw call. -/
def executeTextBatch (font : Font) (entries : Array TextBatchEntry) : CanvasM Unit := do
  if entries.isEmpty then return
  let canvas ← CanvasM.getCanvas
  let (canvasWidth, canvasHeight) ← canvas.ctx.getCurrentSize
  -- Pack into parallel arrays for FFI
  let texts := entries.map (·.text)
  let positions := entries.foldl (init := #[]) fun acc e => acc.push e.x |>.push e.y
  let colors := entries.foldl (init := #[]) fun acc e =>
    acc.push e.r |>.push e.g |>.push e.b |>.push e.a
  let transforms := entries.foldl (init := #[]) fun acc e => acc ++ e.transform
  FFI.Text.renderBatch canvas.ctx.renderer font.handle texts positions colors transforms
    canvasWidth canvasHeight

/-- Merge fillPolygonInstanced commands with the same pathHash into single commands.
    This converts N separate draw calls into M draw calls where M = number of unique pathHashes. -/
def mergeInstancedPolygons (cmds : Array RenderCommand) : Array RenderCommand := Id.run do
  if cmds.isEmpty then return #[]

  -- First pass: collect all fillPolygonInstanced by pathHash
  -- Use an Array of (pathHash, vertices, indices, instances, centerX, centerY) tuples
  let mut polygonGroups : Array (UInt64 × Array Float × Array UInt32 × Array MeshInstance × Float × Float) := #[]
  let mut otherCmds : Array RenderCommand := #[]

  for cmd in cmds do
    match cmd with
    | .fillPolygonInstanced pathHash vertices indices instances centerX centerY =>
      -- Find existing group with same pathHash
      let mut found := false
      let mut newGroups : Array (UInt64 × Array Float × Array UInt32 × Array MeshInstance × Float × Float) := #[]
      for group in polygonGroups do
        let (hash, verts, inds, insts, cx, cy) := group
        if hash == pathHash then
          -- Merge instances into existing group
          newGroups := newGroups.push (hash, verts, inds, insts ++ instances, cx, cy)
          found := true
        else
          newGroups := newGroups.push group
      if found then
        polygonGroups := newGroups
      else
        -- Add new group
        polygonGroups := polygonGroups.push (pathHash, vertices, indices, instances, centerX, centerY)
    | _ =>
      otherCmds := otherCmds.push cmd

  -- Build output: other commands first (they have lower sort priorities), then merged polygons
  let mut result := otherCmds
  for group in polygonGroups do
    let (pathHash, vertices, indices, instances, centerX, centerY) := group
    result := result.push (.fillPolygonInstanced pathHash vertices indices instances centerX centerY)

  result

/-- Merge strokeArcInstanced commands with the same segment count into single commands.
    This converts N separate draw calls into M draw calls where M = number of unique segment counts.
    Since most arcs use the same segment count (16), this typically results in 1 draw call. -/
def mergeInstancedArcs (cmds : Array RenderCommand) : Array RenderCommand := Id.run do
  if cmds.isEmpty then return #[]

  -- Group arcs by segment count (instances, segments)
  let mut arcGroups : Array (Array ArcInstance × Nat) := #[]
  let mut otherCmds : Array RenderCommand := #[]

  for cmd in cmds do
    match cmd with
    | .strokeArcInstanced instances segments =>
      -- Find existing group with same segment count
      let mut found := false
      let mut newGroups : Array (Array ArcInstance × Nat) := #[]
      for group in arcGroups do
        let (insts, segs) := group
        if segs == segments then
          -- Merge instances into existing group
          newGroups := newGroups.push (insts ++ instances, segs)
          found := true
        else
          newGroups := newGroups.push group
      if found then
        arcGroups := newGroups
      else
        -- Add new group
        arcGroups := arcGroups.push (instances, segments)
    | _ =>
      otherCmds := otherCmds.push cmd

  -- Build output: other commands first, then merged arcs
  let mut result := otherCmds
  for group in arcGroups do
    let (instances, segments) := group
    result := result.push (.strokeArcInstanced instances segments)

  result

/-- Execute an array of RenderCommands using CanvasM with batching optimization.
    First coalesces commands within scopes to maximize batching opportunities, then
    groups consecutive fillRect commands (per-instance cornerRadius),
    consecutive strokeRect commands with the same lineWidth (per-instance cornerRadius),
    and consecutive fillCircle commands into batched draw calls.
    Returns batch statistics for performance monitoring. -/
def executeCommandsBatchedWithStats (reg : FontRegistry) (cmds : Array Afferent.Arbor.RenderCommand) : CanvasM BatchStats := do
  -- Time: Flatten commands (transform tracking, simple geometry to absolute coords)
  -- Use `pure` to force evaluation at this point in the monadic sequence
  let tFlatten0 ← IO.monoNanosNow
  let bounded ← pure (computeBoundedCommands cmds)
  let tFlatten1 ← IO.monoNanosNow

  -- Time: Coalesce/sort commands by category (skip lines to reduce work)
  let tCoalesce0 ← IO.monoNanosNow
  let mut lineCmds : Array RenderCommand := #[]
  let mut nonLine : Array BoundedCommand := #[]
  for bc in bounded do
    if bc.cmd.category == .strokeLine then
      lineCmds := lineCmds.push bc.cmd
    else
      nonLine := nonLine.push bc
  let cmds ← pure (coalesceByCategoryWithClip nonLine)

  -- Merge fillPolygonInstanced commands with the same pathHash
  let cmds ← pure (mergeInstancedPolygons cmds)

  -- Merge strokeArcInstanced commands with the same segment count
  let cmds ← pure (mergeInstancedArcs cmds)

  let tCoalesce1 ← IO.monoNanosNow

  -- Time: Main batch loop (batch building + draw calls)
  let tLoop0 ← IO.monoNanosNow

  -- Get canvas info for line drawing later
  let canvas ← CanvasM.getCanvas
  let (canvasWidth, canvasHeight) ← canvas.ctx.getCurrentSize

  -- Line metadata for buffer pre-allocation (lines already separated)
  let mut lineCount : Nat := 0
  let mut uniformLineWidth : Float := 1.0
  for cmd in lineCmds do
    match cmd with
    | .strokeLine _ _ _ lw =>
      if lineCount == 0 then uniformLineWidth := lw
      lineCount := lineCount + 1
    | .strokeLineBatch _ count lw =>
      if lineCount == 0 then uniformLineWidth := lw
      lineCount := lineCount + count
    | _ => pure ()

  -- Pre-allocate/reuse line buffer if needed (actual drawing happens AFTER main loop)
  let lineBuffer ← if lineCount > 0 then
    let requiredFloats := lineCount * 9
    let canvas ← CanvasM.getCanvas
    let canvas ←
      match canvas.floatBuffer with
      | some buf =>
        if canvas.floatBufferCapacity >= requiredFloats then
          pure canvas
        else
          FFI.FloatBuffer.destroy buf
          let newBuf ← FFI.FloatBuffer.create requiredFloats.toUSize
          pure { canvas with floatBuffer := some newBuf, floatBufferCapacity := requiredFloats }
      | none =>
        let newBuf ← FFI.FloatBuffer.create requiredFloats.toUSize
        pure { canvas with floatBuffer := some newBuf, floatBufferCapacity := requiredFloats }
    CanvasM.setCanvas canvas
    pure canvas.floatBuffer
  else
    pure none

  let mut i := 0
  let mut rectBatch : Array RectBatchEntry := #[]
  let mut strokeRectBatch : Array StrokeRectBatchEntry := #[]
  let mut currentStrokeLineWidth : Float := 0.0
  let mut circleBatch : Array CircleBatchEntry := #[]
  let mut strokeCircleBatch : Array StrokeCircleBatchEntry := #[]
  let mut currentStrokeCircleLineWidth : Float := 0.0
  let mut textBatch : Array TextBatchEntry := #[]
  let mut currentTextFontId : Option FontId := none
  -- Fragment batching: accumulate params for consecutive drawFragment commands with same hash
  let mut fragmentParamsBatch : Array Float := #[]
  let mut currentFragmentHash : Option UInt64 := none
  let mut stats : BatchStats := { totalCommands := cmds.size + lineCmds.size }
  let mut drawCallTimeNs : Nat := 0

  -- Helper to flush rect batch (returns updated stats and draw call time delta)
  let flushRects := fun (batch : Array RectBatchEntry) (s : BatchStats) => do
    if !batch.isEmpty then
      let t0 ← IO.monoNanosNow
      executeFillRectBatch batch
      let t1 ← IO.monoNanosNow
      pure ({ s with batchedCalls := s.batchedCalls + 1, rectsBatched := s.rectsBatched + batch.size }, t1 - t0)
    else
      pure (s, 0)

  -- Helper to flush stroke rect batch
  let flushStrokeRects := fun (batch : Array StrokeRectBatchEntry) (lw : Float) (s : BatchStats) => do
    if !batch.isEmpty then
      let t0 ← IO.monoNanosNow
      executeStrokeRectBatch batch lw
      let t1 ← IO.monoNanosNow
      pure ({ s with batchedCalls := s.batchedCalls + 1, strokeRectsBatched := s.strokeRectsBatched + batch.size }, t1 - t0)
    else
      pure (s, 0)

  -- Helper to flush circle batch
  let flushCircles := fun (batch : Array CircleBatchEntry) (s : BatchStats) => do
    if !batch.isEmpty then
      let t0 ← IO.monoNanosNow
      executeFillCircleBatch batch
      let t1 ← IO.monoNanosNow
      pure ({ s with batchedCalls := s.batchedCalls + 1, circlesBatched := s.circlesBatched + batch.size }, t1 - t0)
    else
      pure (s, 0)

  -- Helper to flush stroke circle batch
  let flushStrokeCircles := fun (batch : Array StrokeCircleBatchEntry) (lw : Float) (s : BatchStats) => do
    if !batch.isEmpty then
      let t0 ← IO.monoNanosNow
      executeStrokeCircleBatch batch lw
      let t1 ← IO.monoNanosNow
      -- Reuse circlesBatched counter for stroke circles too
      pure ({ s with batchedCalls := s.batchedCalls + 1, circlesBatched := s.circlesBatched + batch.size }, t1 - t0)
    else
      pure (s, 0)

  -- Lines are processed in a separate tight loop above, no flush helper needed

  -- Helper to flush text batch
  let flushTexts := fun (batch : Array TextBatchEntry) (fontIdOpt : Option FontId) (s : BatchStats) => do
    if !batch.isEmpty then
      match fontIdOpt with
      | some fontId =>
        match reg.get fontId with
        | some font =>
          let t0 ← IO.monoNanosNow
          executeTextBatch font batch
          let t1 ← IO.monoNanosNow
          pure ({ s with batchedCalls := s.batchedCalls + 1, textsBatched := s.textsBatched + batch.size }, t1 - t0)
        | none => pure (s, 0)
      | none => pure (s, 0)
    else
      pure (s, 0)

  -- Helper to flush fragment batch (batched params for same fragment hash)
  let flushFragments := fun (params : Array Float) (hashOpt : Option UInt64) (s : BatchStats) => do
    if params.isEmpty then
      pure (s, 0)
    else
      match hashOpt with
      | some fragmentHash =>
        let canvas ← CanvasM.getCanvas
        let cache ← canvas.fragmentCache.get
        let (maybePipeline, newCache) ← Shader.getOrCompileGlobal cache canvas.ctx.renderer fragmentHash
        canvas.fragmentCache.set newCache
        match maybePipeline with
        | some pipeline =>
          let t0 ← IO.monoNanosNow
          let (canvasWidth, canvasHeight) ← canvas.ctx.getCurrentSize
          FFI.Fragment.draw canvas.ctx.renderer pipeline params canvasWidth canvasHeight
          let t1 ← IO.monoNanosNow
          pure ({ s with batchedCalls := s.batchedCalls + 1 }, t1 - t0)
        | none => pure (s, 0)
      | none => pure (s, 0)

  -- Helper to flush all batches (lines handled separately above)
  let flushAll := fun (rB : Array RectBatchEntry)
                      (sRB : Array StrokeRectBatchEntry) (slw : Float)
                      (cB : Array CircleBatchEntry)
                      (sCB : Array StrokeCircleBatchEntry) (sclw : Float)
                      (tB : Array TextBatchEntry) (tFontId : Option FontId)
                      (fB : Array Float) (fHash : Option UInt64)
                      (s : BatchStats) (accTime : Nat) => do
    let (s, dt1) ← flushRects rB s
    let (s, dt2) ← flushStrokeRects sRB slw s
    let (s, dt3) ← flushCircles cB s
    let (s, dt4) ← flushStrokeCircles sCB sclw s
    let (s, dt5) ← flushTexts tB tFontId s
    let (s, dt6) ← flushFragments fB fHash s
    pure (s, accTime + dt1 + dt2 + dt3 + dt4 + dt5 + dt6)

  while h : i < cmds.size do
    let cmd := cmds[i]
    match cmd with
    | .fillRect rect color cornerRadius =>
      -- Only flush other batches if non-empty (commands sorted by category)
      -- Lines handled separately in tight loop above
      if !strokeRectBatch.isEmpty then
        let (s, dt) ← flushStrokeRects strokeRectBatch currentStrokeLineWidth stats
        stats := s; drawCallTimeNs := drawCallTimeNs + dt
        strokeRectBatch := #[]
      if !circleBatch.isEmpty then
        let (s, dt) ← flushCircles circleBatch stats
        stats := s; drawCallTimeNs := drawCallTimeNs + dt
        circleBatch := #[]
      if !textBatch.isEmpty then
        let (s, dt) ← flushTexts textBatch currentTextFontId stats
        stats := s; drawCallTimeNs := drawCallTimeNs + dt
        textBatch := #[]
        currentTextFontId := none
      let entry : RectBatchEntry := {
        x := rect.origin.x, y := rect.origin.y
        width := rect.size.width, height := rect.size.height
        r := color.r, g := color.g, b := color.b, a := color.a
        cornerRadius := cornerRadius
      }
      rectBatch := rectBatch.push entry

    | .strokeRect rect color lineWidth cornerRadius =>
      -- Only flush other batches if non-empty (lines handled separately)
      if !rectBatch.isEmpty then
        let (s, dt) ← flushRects rectBatch stats
        stats := s; drawCallTimeNs := drawCallTimeNs + dt
        rectBatch := #[]
      if !circleBatch.isEmpty then
        let (s, dt) ← flushCircles circleBatch stats
        stats := s; drawCallTimeNs := drawCallTimeNs + dt
        circleBatch := #[]
      if !textBatch.isEmpty then
        let (s, dt) ← flushTexts textBatch currentTextFontId stats
        stats := s; drawCallTimeNs := drawCallTimeNs + dt
        textBatch := #[]
        currentTextFontId := none
      -- Check if we can add to current stroke rect batch
      if strokeRectBatch.isEmpty || currentStrokeLineWidth == lineWidth then
        let entry : StrokeRectBatchEntry := {
          x := rect.origin.x, y := rect.origin.y
          width := rect.size.width, height := rect.size.height
          r := color.r, g := color.g, b := color.b, a := color.a
          cornerRadius := cornerRadius
        }
        strokeRectBatch := strokeRectBatch.push entry
        currentStrokeLineWidth := lineWidth
      else
        -- Different lineWidth - flush and start new batch
        let (s, dt) ← flushStrokeRects strokeRectBatch currentStrokeLineWidth stats
        stats := s; drawCallTimeNs := drawCallTimeNs + dt
        let entry : StrokeRectBatchEntry := {
          x := rect.origin.x, y := rect.origin.y
          width := rect.size.width, height := rect.size.height
          r := color.r, g := color.g, b := color.b, a := color.a
          cornerRadius := cornerRadius
        }
        strokeRectBatch := #[entry]
        currentStrokeLineWidth := lineWidth

    | .fillCircle center radius color =>
      -- Only flush other batches if non-empty (lines handled separately)
      if !rectBatch.isEmpty then
        let (s, dt) ← flushRects rectBatch stats
        stats := s; drawCallTimeNs := drawCallTimeNs + dt
        rectBatch := #[]
      if !strokeRectBatch.isEmpty then
        let (s, dt) ← flushStrokeRects strokeRectBatch currentStrokeLineWidth stats
        stats := s; drawCallTimeNs := drawCallTimeNs + dt
        strokeRectBatch := #[]
      if !strokeCircleBatch.isEmpty then
        let (s, dt) ← flushStrokeCircles strokeCircleBatch currentStrokeCircleLineWidth stats
        stats := s; drawCallTimeNs := drawCallTimeNs + dt
        strokeCircleBatch := #[]
      if !textBatch.isEmpty then
        let (s, dt) ← flushTexts textBatch currentTextFontId stats
        stats := s; drawCallTimeNs := drawCallTimeNs + dt
        textBatch := #[]
        currentTextFontId := none
      -- Add to circle batch (all circles batch together, no radius grouping needed)
      let entry : CircleBatchEntry := {
        centerX := center.x, centerY := center.y, radius := radius
        r := color.r, g := color.g, b := color.b, a := color.a
      }
      circleBatch := circleBatch.push entry

    | .fillCircleBatch data count =>
      -- Flush any pending circle batch first (can't mix individual + batched)
      if !circleBatch.isEmpty then
        let (s, dt) ← flushCircles circleBatch stats
        stats := s; drawCallTimeNs := drawCallTimeNs + dt
        circleBatch := #[]
      -- Execute the batch directly
      if count > 0 then
        let t0 ← IO.monoNanosNow
        executeCommand reg cmd
        let t1 ← IO.monoNanosNow
        drawCallTimeNs := drawCallTimeNs + (t1 - t0)
        stats := { stats with batchedCalls := stats.batchedCalls + 1, circlesBatched := stats.circlesBatched + count }

    | .strokeCircle center radius color lineWidth =>
      -- Only flush other batches if non-empty
      if !rectBatch.isEmpty then
        let (s, dt) ← flushRects rectBatch stats
        stats := s; drawCallTimeNs := drawCallTimeNs + dt
        rectBatch := #[]
      if !strokeRectBatch.isEmpty then
        let (s, dt) ← flushStrokeRects strokeRectBatch currentStrokeLineWidth stats
        stats := s; drawCallTimeNs := drawCallTimeNs + dt
        strokeRectBatch := #[]
      if !circleBatch.isEmpty then
        let (s, dt) ← flushCircles circleBatch stats
        stats := s; drawCallTimeNs := drawCallTimeNs + dt
        circleBatch := #[]
      if !textBatch.isEmpty then
        let (s, dt) ← flushTexts textBatch currentTextFontId stats
        stats := s; drawCallTimeNs := drawCallTimeNs + dt
        textBatch := #[]
        currentTextFontId := none
      -- Check if we can add to current stroke circle batch (same lineWidth)
      if strokeCircleBatch.isEmpty || currentStrokeCircleLineWidth == lineWidth then
        let entry : StrokeCircleBatchEntry := {
          centerX := center.x, centerY := center.y, radius := radius
          r := color.r, g := color.g, b := color.b, a := color.a
        }
        strokeCircleBatch := strokeCircleBatch.push entry
        currentStrokeCircleLineWidth := lineWidth
      else
        -- Different lineWidth - flush and start new batch
        let (s, dt) ← flushStrokeCircles strokeCircleBatch currentStrokeCircleLineWidth stats
        stats := s; drawCallTimeNs := drawCallTimeNs + dt
        let entry : StrokeCircleBatchEntry := {
          centerX := center.x, centerY := center.y, radius := radius
          r := color.r, g := color.g, b := color.b, a := color.a
        }
        strokeCircleBatch := #[entry]
        currentStrokeCircleLineWidth := lineWidth

    | .fillText text x y fontId color =>
      -- Only flush other batches if non-empty (lines handled separately)
      if !rectBatch.isEmpty then
        let (s, dt) ← flushRects rectBatch stats
        stats := s; drawCallTimeNs := drawCallTimeNs + dt
        rectBatch := #[]
      if !strokeRectBatch.isEmpty then
        let (s, dt) ← flushStrokeRects strokeRectBatch currentStrokeLineWidth stats
        stats := s; drawCallTimeNs := drawCallTimeNs + dt
        strokeRectBatch := #[]
      if !circleBatch.isEmpty then
        let (s, dt) ← flushCircles circleBatch stats
        stats := s; drawCallTimeNs := drawCallTimeNs + dt
        circleBatch := #[]
      if !strokeCircleBatch.isEmpty then
        let (s, dt) ← flushStrokeCircles strokeCircleBatch currentStrokeCircleLineWidth stats
        stats := s; drawCallTimeNs := drawCallTimeNs + dt
        strokeCircleBatch := #[]
      -- Get current canvas transform for this text entry
      let canvas ← CanvasM.getCanvas
      let transform := canvas.state.transform.toArray
      -- Check if we can add to current text batch (same font)
      if textBatch.isEmpty || currentTextFontId == some fontId then
        let entry : TextBatchEntry := {
          text, x, y
          r := color.r, g := color.g, b := color.b, a := color.a
          transform
        }
        textBatch := textBatch.push entry
        currentTextFontId := some fontId
      else
        -- Different font - flush and start new batch
        let (s, dt) ← flushTexts textBatch currentTextFontId stats
        stats := s; drawCallTimeNs := drawCallTimeNs + dt
        let entry : TextBatchEntry := {
          text, x, y
          r := color.r, g := color.g, b := color.b, a := color.a
          transform
        }
        textBatch := #[entry]
        currentTextFontId := some fontId

    | .strokeLine _ _ _ _ =>
      -- Lines already processed in tight loop above - skip
      pure ()

    | .drawFragment fragmentHash _primitiveType params _instanceCount =>
      -- Fragment batching: accumulate params for consecutive commands with same hash
      -- Flush other batches first
      if !rectBatch.isEmpty then
        let (s, dt) ← flushRects rectBatch stats
        stats := s; drawCallTimeNs := drawCallTimeNs + dt
        rectBatch := #[]
      if !strokeRectBatch.isEmpty then
        let (s, dt) ← flushStrokeRects strokeRectBatch currentStrokeLineWidth stats
        stats := s; drawCallTimeNs := drawCallTimeNs + dt
        strokeRectBatch := #[]
      if !circleBatch.isEmpty then
        let (s, dt) ← flushCircles circleBatch stats
        stats := s; drawCallTimeNs := drawCallTimeNs + dt
        circleBatch := #[]
      if !strokeCircleBatch.isEmpty then
        let (s, dt) ← flushStrokeCircles strokeCircleBatch currentStrokeCircleLineWidth stats
        stats := s; drawCallTimeNs := drawCallTimeNs + dt
        strokeCircleBatch := #[]
      if !textBatch.isEmpty then
        let (s, dt) ← flushTexts textBatch currentTextFontId stats
        stats := s; drawCallTimeNs := drawCallTimeNs + dt
        textBatch := #[]
        currentTextFontId := none
      -- Check if we can batch with current fragment
      if currentFragmentHash == some fragmentHash then
        -- Same fragment - accumulate params
        fragmentParamsBatch := fragmentParamsBatch ++ params
      else
        -- Different fragment - flush previous batch and start new
        if !fragmentParamsBatch.isEmpty then
          let (s, dt) ← flushFragments fragmentParamsBatch currentFragmentHash stats
          stats := s; drawCallTimeNs := drawCallTimeNs + dt
        fragmentParamsBatch := params
        currentFragmentHash := some fragmentHash

    | _ =>
      -- Non-batchable command - flush all pending batches first (lines handled separately)
      let (s, dt) ← flushAll rectBatch strokeRectBatch currentStrokeLineWidth circleBatch strokeCircleBatch currentStrokeCircleLineWidth textBatch currentTextFontId fragmentParamsBatch currentFragmentHash stats drawCallTimeNs
      stats := s; drawCallTimeNs := dt
      rectBatch := #[]
      strokeRectBatch := #[]
      circleBatch := #[]
      strokeCircleBatch := #[]
      textBatch := #[]
      currentTextFontId := none
      fragmentParamsBatch := #[]
      currentFragmentHash := none
      -- Execute the command individually
      executeCommand reg cmd
      stats := { stats with individualCalls := stats.individualCalls + 1 }

    i := i + 1

  -- Flush any remaining batches (lines handled separately below)
  let (s, dt) ← flushAll rectBatch strokeRectBatch currentStrokeLineWidth circleBatch strokeCircleBatch currentStrokeCircleLineWidth textBatch currentTextFontId fragmentParamsBatch currentFragmentHash stats drawCallTimeNs
  stats := s; drawCallTimeNs := dt

  -- Process all lines in a tight loop AFTER other batches (lines sorted last)
  -- This avoids per-command overhead from the main batching loop
  if lineCount > 0 then
    match lineBuffer with
    | some buf =>
      let mut bufIdx : USize := 0
      -- Tight loop - minimal per-iteration overhead (like OrbitalInstanced)
      for cmd in lineCmds do
        match cmd with
        | .strokeLine p1 p2 color _ =>
          buf.setVec9 bufIdx p1.x p1.y p2.x p2.y color.r color.g color.b color.a 0.0
          bufIdx := bufIdx + 9
        | .strokeLineBatch data count _ =>
          if count > 0 then
            for j in [:count] do
              let base := j * 9
              let x1 := data[base]!
              let y1 := data[base + 1]!
              let x2 := data[base + 2]!
              let y2 := data[base + 3]!
              let r := data[base + 4]!
              let g := data[base + 5]!
              let b := data[base + 6]!
              let a := data[base + 7]!
              let pad := data[base + 8]!
              buf.setVec9 bufIdx x1 y1 x2 y2 r g b a pad
              bufIdx := bufIdx + 9
        | _ => pure ()
      -- Single draw call for all lines
      let t0 ← IO.monoNanosNow
      canvas.ctx.renderer.drawLineBatchBuffer buf lineCount.toUInt32 uniformLineWidth canvasWidth canvasHeight
      let t1 ← IO.monoNanosNow
      drawCallTimeNs := drawCallTimeNs + (t1 - t0)
      stats := { stats with linesBatched := lineCount, batchedCalls := stats.batchedCalls + 1 }
    | none => pure ()

  let tLoop1 ← IO.monoNanosNow

  -- Calculate timing in milliseconds
  let timeFlattenMs := (tFlatten1 - tFlatten0).toFloat / 1000000.0
  let timeCoalesceMs := (tCoalesce1 - tCoalesce0).toFloat / 1000000.0
  let timeBatchLoopMs := (tLoop1 - tLoop0).toFloat / 1000000.0
  let timeDrawCallsMs := drawCallTimeNs.toFloat / 1000000.0

  return { stats with
    timeFlattenMs := timeFlattenMs
    timeCoalesceMs := timeCoalesceMs
    timeBatchLoopMs := timeBatchLoopMs
    timeDrawCallsMs := timeDrawCallsMs
  }

/-- Execute an array of RenderCommands using CanvasM with batching optimization.
    Coalesces commands within scopes to maximize batching, then batches fillRect
    commands with per-instance cornerRadius into a single draw call. -/
def executeCommandsBatched (reg : FontRegistry) (cmds : Array Afferent.Arbor.RenderCommand) : CanvasM Unit := do
  let _ ← executeCommandsBatchedWithStats reg cmds

/-- Execute an array of RenderCommands using CanvasM (unbatched, for compatibility). -/
def executeCommands (reg : FontRegistry) (cmds : Array Afferent.Arbor.RenderCommand) : CanvasM Unit := do
  for cmd in cmds do
    executeCommand reg cmd

partial def renderCustomWidgets (w : Afferent.Arbor.Widget) (layouts : Trellis.LayoutResult) : CanvasM Unit := do
  match layouts.get w.id with
  | none => pure ()
  | some layout =>
      match w with
      | .custom _ _ _ spec =>
          match spec.draw with
          | some draw => draw layout
          | none => pure ()
      | .flex _ _ _ _ children
      | .grid _ _ _ _ children =>
          for child in children do
            renderCustomWidgets child layouts
      | .scroll _ _ _ _ _ _ _ child =>
          renderCustomWidgets child layouts
      | _ => pure ()

/-- Render an Arbor widget tree using CanvasM with automatic render command caching.
    This is the main entry point for rendering Arbor widgets with Afferent's Metal backend.

    Steps:
    1. Measure the widget tree (computes text layouts)
    2. Compute layout using Trellis
    3. Collect render commands (with caching for CustomSpec widgets)
    4. Execute commands using CanvasM

    Caching: CustomSpec widgets with names (from registerComponentW) are automatically
    cached. Cache is keyed by widget name + layout hash. When data changes, dynWidget
    rebuilds and generates new widget names, causing natural cache invalidation. -/
def renderArborWidget (reg : FontRegistry) (widget : Afferent.Arbor.Widget)
    (availWidth availHeight : Float) : CanvasM Unit := do
  -- Measure widget and get layout nodes
  let measureResult ← runWithFonts reg (Afferent.Arbor.measureWidget widget availWidth availHeight)
  let layoutNode := measureResult.node
  let measuredWidget := measureResult.widget

  -- Compute layout
  let layouts := Trellis.layout layoutNode availWidth availHeight

  -- Collect render commands with caching
  let canvas ← CanvasM.getCanvas
  let commands ← Afferent.Arbor.collectCommandsCached canvas.renderCache measuredWidget layouts

  -- Execute commands with batching optimization
  executeCommandsBatched reg commands

/-- Render an Arbor widget tree and run any custom CanvasM draw hooks. -/
def renderArborWidgetWithCustom (reg : FontRegistry) (widget : Afferent.Arbor.Widget)
    (availWidth availHeight : Float) : CanvasM Unit := do
  let measureResult ← runWithFonts reg (Afferent.Arbor.measureWidget widget availWidth availHeight)
  let layoutNode := measureResult.node
  let measuredWidget := measureResult.widget
  let layouts := Trellis.layout layoutNode availWidth availHeight
  let canvas ← CanvasM.getCanvas
  let commands ← Afferent.Arbor.collectCommandsCached canvas.renderCache measuredWidget layouts
  executeCommandsBatched reg commands
  renderCustomWidgets measuredWidget layouts

/-- Render an Arbor widget tree and return cache statistics.
    Returns (cacheHits, cacheMisses) for debugging/verification purposes. -/
def renderArborWidgetWithStats (reg : FontRegistry) (widget : Afferent.Arbor.Widget)
    (availWidth availHeight : Float) : CanvasM (Nat × Nat) := do
  let measureResult ← runWithFonts reg (Afferent.Arbor.measureWidget widget availWidth availHeight)
  let layoutNode := measureResult.node
  let measuredWidget := measureResult.widget
  let layouts := Trellis.layout layoutNode availWidth availHeight
  let canvas ← CanvasM.getCanvas
  let (commands, hits, misses) ← Afferent.Arbor.collectCommandsCachedWithStats canvas.renderCache measuredWidget layouts
  executeCommandsBatched reg commands
  pure (hits, misses)

/-- Convenience function to render a widget built with Arbor's DSL.
    Takes a WidgetBuilder and executes the full render pipeline. -/
def renderArborBuilder (reg : FontRegistry) (builder : Afferent.Arbor.WidgetBuilder)
    (availWidth availHeight : Float) : CanvasM Unit := do
  let widget := Afferent.Arbor.build builder
  renderArborWidget reg widget availWidth availHeight

/-- Render an Arbor widget tree centered on screen.
    Computes intrinsic size and offsets rendering to center the widget. -/
def renderArborWidgetCentered (reg : FontRegistry) (widget : Afferent.Arbor.Widget)
    (screenWidth screenHeight : Float) : CanvasM Unit := do
  -- Measure widget to get intrinsic size
  let (intrinsicWidth, intrinsicHeight) ← runWithFonts reg (Afferent.Arbor.intrinsicSize widget)

  -- Measure widget for layout
  let measureResult ← runWithFonts reg (Afferent.Arbor.measureWidget widget intrinsicWidth intrinsicHeight)
  let layoutNode := measureResult.node
  let measuredWidget := measureResult.widget

  -- Compute layout at intrinsic size
  let layouts := Trellis.layout layoutNode intrinsicWidth intrinsicHeight

  -- Calculate offset to center
  let offsetX := (screenWidth - intrinsicWidth) / 2
  let offsetY := (screenHeight - intrinsicHeight) / 2

  -- Collect render commands with caching
  let canvas ← CanvasM.getCanvas
  let commands ← Afferent.Arbor.collectCommandsCached canvas.renderCache measuredWidget layouts

  -- Save state, translate, render, restore
  CanvasM.save
  CanvasM.translate offsetX offsetY
  executeCommandsBatched reg commands
  CanvasM.restore

/-- Render an Arbor widget tree centered with debug borders.
    Shows colored borders around each layout cell for debugging. -/
def renderArborWidgetDebug (reg : FontRegistry) (widget : Afferent.Arbor.Widget)
    (screenWidth screenHeight : Float)
    (borderColor : Afferent.Arbor.Color := ⟨0.5, 1.0, 0.5, 0.5⟩) : CanvasM Unit := do
  -- Measure widget to get intrinsic size
  let (intrinsicWidth, intrinsicHeight) ← runWithFonts reg (Afferent.Arbor.intrinsicSize widget)

  -- Measure widget for layout
  let measureResult ← runWithFonts reg (Afferent.Arbor.measureWidget widget intrinsicWidth intrinsicHeight)
  let layoutNode := measureResult.node
  let measuredWidget := measureResult.widget

  -- Compute layout at intrinsic size
  let layouts := Trellis.layout layoutNode intrinsicWidth intrinsicHeight

  -- Calculate offset to center
  let offsetX := (screenWidth - intrinsicWidth) / 2
  let offsetY := (screenHeight - intrinsicHeight) / 2

  -- Collect render commands with debug borders
  let commands := Afferent.Arbor.collectCommandsWithDebug measuredWidget layouts borderColor

  -- Save state, translate, render, restore
  CanvasM.save
  CanvasM.translate offsetX offsetY
  executeCommandsBatched reg commands
  CanvasM.restore

end Afferent.Widget

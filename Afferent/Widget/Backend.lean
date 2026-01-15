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
    let data := #[center.x, center.y, radius, 0.0, color.r, color.g, color.b, color.a]
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
    let data := #[p1.x, p1.y, p2.x, p2.y, color.r, color.g, color.b, color.a]
    canvas.ctx.renderer.drawLineBatch data 1 lineWidth canvasWidth canvasHeight

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

Batching groups consecutive fillRect commands with the same corner radius into
a single GPU draw call. This dramatically improves performance for charts that
draw many rectangles (heatmaps, bar charts, scatter plots, etc.).

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

/-- Entry for a batched circle.
    Format: [centerX, centerY, radius, padding, r, g, b, a] (8 floats for GPU alignment) -/
structure CircleBatchEntry where
  centerX : Float
  centerY : Float
  radius : Float
  r : Float
  g : Float
  b : Float
  a : Float

/-- Entry for a batched stroked rectangle.
    Format: [x, y, width, height, r, g, b, a] (8 floats) -/
structure StrokeRectBatchEntry where
  x : Float
  y : Float
  width : Float
  height : Float
  r : Float
  g : Float
  b : Float
  a : Float

/-- Entry for a batched line segment.
    Format: [x1, y1, x2, y2, r, g, b, a] (8 floats) -/
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

/-- Coalesce commands by grouping same-category commands together.
    Uses a stable sort so relative order within each category is preserved.

    After transform flattening, simple geometry (rects, circles) is in
    absolute coordinates and doesn't depend on transform state.
    Text captures its transform during batching (TextBatchEntry.transform).

    This is O(N log N) and avoids the O(N²) memory of full overlap analysis. -/
def coalesceByCategory (bounded : Array BoundedCommand) : Array RenderCommand := Id.run do
  if bounded.isEmpty then return #[]

  -- Stable sort by category priority, preserving original order within category
  -- We use (priority, originalIndex) as sort key for stability
  let sorted := bounded.qsort fun a b =>
    let pa := a.cmd.category.sortPriority
    let pb := b.cmd.category.sortPriority
    if pa != pb then decide (pa < pb)
    else decide (a.originalIndex < b.originalIndex)

  sorted.map (·.cmd)

/-- Execute a batch of fillRect commands in a single draw call. -/
def executeFillRectBatch (rects : Array RectBatchEntry) (cornerRadius : Float) : CanvasM Unit := do
  if rects.isEmpty then return
  let canvas ← CanvasM.getCanvas
  -- Use current drawable size for NDC conversion (dynamic resize support)
  let (canvasWidth, canvasHeight) ← canvas.ctx.getCurrentSize
  -- Pack into Float array: [x, y, w, h, r, g, b, a] per rect
  let data := rects.foldl (init := #[]) fun acc entry =>
    acc.push entry.x |>.push entry.y |>.push entry.width |>.push entry.height
       |>.push entry.r |>.push entry.g |>.push entry.b |>.push entry.a
  canvas.ctx.renderer.drawBatch 0 data rects.size.toUInt32 cornerRadius 0.0
    canvasWidth canvasHeight

/-- Execute a batch of fillCircle commands in a single draw call. -/
def executeFillCircleBatch (circles : Array CircleBatchEntry) : CanvasM Unit := do
  if circles.isEmpty then return
  let canvas ← CanvasM.getCanvas
  let (canvasWidth, canvasHeight) ← canvas.ctx.getCurrentSize
  -- Pack into Float array: [x, y, w, h, r, g, b, a] per circle (bounding box)
  let data := circles.foldl (init := #[]) fun acc entry =>
    let size := entry.radius * 2.0
    let x := entry.centerX - entry.radius
    let y := entry.centerY - entry.radius
    acc.push x |>.push y |>.push size |>.push size
       |>.push entry.r |>.push entry.g |>.push entry.b |>.push entry.a
  canvas.ctx.renderer.drawBatch 1 data circles.size.toUInt32 0.0 0.0
    canvasWidth canvasHeight

/-- Execute a batch of strokeRect commands in a single draw call. -/
def executeStrokeRectBatch (rects : Array StrokeRectBatchEntry) (lineWidth cornerRadius : Float) : CanvasM Unit := do
  if rects.isEmpty then return
  let canvas ← CanvasM.getCanvas
  let (canvasWidth, canvasHeight) ← canvas.ctx.getCurrentSize
  -- Pack into Float array: [x, y, w, h, r, g, b, a] per rect
  let data := rects.foldl (init := #[]) fun acc entry =>
    acc.push entry.x |>.push entry.y |>.push entry.width |>.push entry.height
       |>.push entry.r |>.push entry.g |>.push entry.b |>.push entry.a
  canvas.ctx.renderer.drawBatch 2 data rects.size.toUInt32 lineWidth cornerRadius
    canvasWidth canvasHeight

/-- Execute a batch of strokeLine commands in a single draw call. -/
def executeLineBatch (lines : Array LineBatchEntry) (lineWidth : Float) : CanvasM Unit := do
  if lines.isEmpty then return
  let canvas ← CanvasM.getCanvas
  let (canvasWidth, canvasHeight) ← canvas.ctx.getCurrentSize
  -- Pack into Float array: [x1, y1, x2, y2, r, g, b, a] per line
  let data := lines.foldl (init := #[]) fun acc entry =>
    acc.push entry.x1 |>.push entry.y1 |>.push entry.x2 |>.push entry.y2
       |>.push entry.r |>.push entry.g |>.push entry.b |>.push entry.a
  canvas.ctx.renderer.drawLineBatch data lines.size.toUInt32 lineWidth
    canvasWidth canvasHeight

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

/-- Execute an array of RenderCommands using CanvasM with batching optimization.
    First coalesces commands within scopes to maximize batching opportunities, then
    groups consecutive fillRect commands with the same corner radius,
    consecutive strokeRect commands with the same lineWidth and cornerRadius,
    and consecutive fillCircle commands into batched draw calls.
    Returns batch statistics for performance monitoring. -/
def executeCommandsBatchedWithStats (reg : FontRegistry) (cmds : Array Afferent.Arbor.RenderCommand) : CanvasM BatchStats := do
  -- Time: Flatten commands (transform tracking, simple geometry to absolute coords)
  let tFlatten0 ← IO.monoNanosNow
  let bounded := computeBoundedCommands cmds
  let tFlatten1 ← IO.monoNanosNow

  -- Time: Coalesce/sort commands by category
  let tCoalesce0 ← IO.monoNanosNow
  let cmds := coalesceByCategory bounded
  let tCoalesce1 ← IO.monoNanosNow

  -- Time: Main batch loop (batch building + draw calls)
  let tLoop0 ← IO.monoNanosNow
  let mut i := 0
  let mut rectBatch : Array RectBatchEntry := #[]
  let mut currentCornerRadius : Float := 0.0
  let mut strokeRectBatch : Array StrokeRectBatchEntry := #[]
  let mut currentStrokeLineWidth : Float := 0.0
  let mut currentStrokeCornerRadius : Float := 0.0
  let mut circleBatch : Array CircleBatchEntry := #[]
  let mut lineBatch : Array LineBatchEntry := #[]
  let mut currentLineWidth : Float := 0.0
  let mut textBatch : Array TextBatchEntry := #[]
  let mut currentTextFontId : Option FontId := none
  let mut stats : BatchStats := { totalCommands := cmds.size }

  -- Helper to flush rect batch
  let flushRects := fun (batch : Array RectBatchEntry) (radius : Float) (s : BatchStats) => do
    if !batch.isEmpty then
      executeFillRectBatch batch radius
      pure { s with batchedCalls := s.batchedCalls + 1, rectsBatched := s.rectsBatched + batch.size }
    else
      pure s

  -- Helper to flush stroke rect batch
  let flushStrokeRects := fun (batch : Array StrokeRectBatchEntry) (lw cr : Float) (s : BatchStats) => do
    if !batch.isEmpty then
      executeStrokeRectBatch batch lw cr
      pure { s with batchedCalls := s.batchedCalls + 1, strokeRectsBatched := s.strokeRectsBatched + batch.size }
    else
      pure s

  -- Helper to flush circle batch
  let flushCircles := fun (batch : Array CircleBatchEntry) (s : BatchStats) => do
    if !batch.isEmpty then
      executeFillCircleBatch batch
      pure { s with batchedCalls := s.batchedCalls + 1, circlesBatched := s.circlesBatched + batch.size }
    else
      pure s

  -- Helper to flush line batch
  let flushLines := fun (batch : Array LineBatchEntry) (lw : Float) (s : BatchStats) => do
    if !batch.isEmpty then
      executeLineBatch batch lw
      pure { s with batchedCalls := s.batchedCalls + 1, linesBatched := s.linesBatched + batch.size }
    else
      pure s

  -- Helper to flush text batch
  let flushTexts := fun (batch : Array TextBatchEntry) (fontIdOpt : Option FontId) (s : BatchStats) => do
    if !batch.isEmpty then
      match fontIdOpt with
      | some fontId =>
        match reg.get fontId with
        | some font =>
          executeTextBatch font batch
          pure { s with batchedCalls := s.batchedCalls + 1, textsBatched := s.textsBatched + batch.size }
        | none => pure s
      | none => pure s
    else
      pure s

  -- Helper to flush all batches
  let flushAll := fun (rB : Array RectBatchEntry) (cr : Float)
                      (sRB : Array StrokeRectBatchEntry) (slw scr : Float)
                      (cB : Array CircleBatchEntry)
                      (lB : Array LineBatchEntry) (llw : Float)
                      (tB : Array TextBatchEntry) (tFontId : Option FontId)
                      (s : BatchStats) => do
    let s ← flushRects rB cr s
    let s ← flushStrokeRects sRB slw scr s
    let s ← flushCircles cB s
    let s ← flushLines lB llw s
    let s ← flushTexts tB tFontId s
    pure s

  while h : i < cmds.size do
    let cmd := cmds[i]
    match cmd with
    | .fillRect rect color cornerRadius =>
      -- Flush other batches first
      stats ← flushStrokeRects strokeRectBatch currentStrokeLineWidth currentStrokeCornerRadius stats
      strokeRectBatch := #[]
      stats ← flushCircles circleBatch stats
      circleBatch := #[]
      stats ← flushLines lineBatch currentLineWidth stats
      lineBatch := #[]
      stats ← flushTexts textBatch currentTextFontId stats
      textBatch := #[]
      currentTextFontId := none
      -- Check if we can add to current rect batch
      if rectBatch.isEmpty || currentCornerRadius == cornerRadius then
        let entry : RectBatchEntry := {
          x := rect.origin.x, y := rect.origin.y
          width := rect.size.width, height := rect.size.height
          r := color.r, g := color.g, b := color.b, a := color.a
        }
        rectBatch := rectBatch.push entry
        currentCornerRadius := cornerRadius
      else
        -- Different corner radius - flush and start new batch
        stats ← flushRects rectBatch currentCornerRadius stats
        let entry : RectBatchEntry := {
          x := rect.origin.x, y := rect.origin.y
          width := rect.size.width, height := rect.size.height
          r := color.r, g := color.g, b := color.b, a := color.a
        }
        rectBatch := #[entry]
        currentCornerRadius := cornerRadius

    | .strokeRect rect color lineWidth cornerRadius =>
      -- Flush other batches first
      stats ← flushRects rectBatch currentCornerRadius stats
      rectBatch := #[]
      stats ← flushCircles circleBatch stats
      circleBatch := #[]
      stats ← flushLines lineBatch currentLineWidth stats
      lineBatch := #[]
      stats ← flushTexts textBatch currentTextFontId stats
      textBatch := #[]
      currentTextFontId := none
      -- Check if we can add to current stroke rect batch
      if strokeRectBatch.isEmpty || (currentStrokeLineWidth == lineWidth && currentStrokeCornerRadius == cornerRadius) then
        let entry : StrokeRectBatchEntry := {
          x := rect.origin.x, y := rect.origin.y
          width := rect.size.width, height := rect.size.height
          r := color.r, g := color.g, b := color.b, a := color.a
        }
        strokeRectBatch := strokeRectBatch.push entry
        currentStrokeLineWidth := lineWidth
        currentStrokeCornerRadius := cornerRadius
      else
        -- Different lineWidth or cornerRadius - flush and start new batch
        stats ← flushStrokeRects strokeRectBatch currentStrokeLineWidth currentStrokeCornerRadius stats
        let entry : StrokeRectBatchEntry := {
          x := rect.origin.x, y := rect.origin.y
          width := rect.size.width, height := rect.size.height
          r := color.r, g := color.g, b := color.b, a := color.a
        }
        strokeRectBatch := #[entry]
        currentStrokeLineWidth := lineWidth
        currentStrokeCornerRadius := cornerRadius

    | .fillCircle center radius color =>
      -- Flush other batches first
      stats ← flushRects rectBatch currentCornerRadius stats
      rectBatch := #[]
      stats ← flushStrokeRects strokeRectBatch currentStrokeLineWidth currentStrokeCornerRadius stats
      strokeRectBatch := #[]
      stats ← flushLines lineBatch currentLineWidth stats
      lineBatch := #[]
      stats ← flushTexts textBatch currentTextFontId stats
      textBatch := #[]
      currentTextFontId := none
      -- Add to circle batch (all circles batch together, no radius grouping needed)
      let entry : CircleBatchEntry := {
        centerX := center.x, centerY := center.y, radius := radius
        r := color.r, g := color.g, b := color.b, a := color.a
      }
      circleBatch := circleBatch.push entry

    | .fillText text x y fontId color =>
      -- Flush other batches first
      stats ← flushRects rectBatch currentCornerRadius stats
      rectBatch := #[]
      stats ← flushStrokeRects strokeRectBatch currentStrokeLineWidth currentStrokeCornerRadius stats
      strokeRectBatch := #[]
      stats ← flushCircles circleBatch stats
      circleBatch := #[]
      stats ← flushLines lineBatch currentLineWidth stats
      lineBatch := #[]
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
        stats ← flushTexts textBatch currentTextFontId stats
        let entry : TextBatchEntry := {
          text, x, y
          r := color.r, g := color.g, b := color.b, a := color.a
          transform
        }
        textBatch := #[entry]
        currentTextFontId := some fontId

    | .strokeLine p1 p2 color lineWidth =>
      -- Flush other batches first
      stats ← flushRects rectBatch currentCornerRadius stats
      rectBatch := #[]
      stats ← flushStrokeRects strokeRectBatch currentStrokeLineWidth currentStrokeCornerRadius stats
      strokeRectBatch := #[]
      stats ← flushCircles circleBatch stats
      circleBatch := #[]
      stats ← flushTexts textBatch currentTextFontId stats
      textBatch := #[]
      currentTextFontId := none
      -- Check if we can add to current line batch (same lineWidth)
      if lineBatch.isEmpty || currentLineWidth == lineWidth then
        let entry : LineBatchEntry := {
          x1 := p1.x, y1 := p1.y, x2 := p2.x, y2 := p2.y
          r := color.r, g := color.g, b := color.b, a := color.a
        }
        lineBatch := lineBatch.push entry
        currentLineWidth := lineWidth
      else
        -- Different lineWidth - flush and start new batch
        stats ← flushLines lineBatch currentLineWidth stats
        let entry : LineBatchEntry := {
          x1 := p1.x, y1 := p1.y, x2 := p2.x, y2 := p2.y
          r := color.r, g := color.g, b := color.b, a := color.a
        }
        lineBatch := #[entry]
        currentLineWidth := lineWidth

    | _ =>
      -- Non-batchable command - flush all pending batches first
      stats ← flushAll rectBatch currentCornerRadius strokeRectBatch currentStrokeLineWidth currentStrokeCornerRadius circleBatch lineBatch currentLineWidth textBatch currentTextFontId stats
      rectBatch := #[]
      strokeRectBatch := #[]
      circleBatch := #[]
      lineBatch := #[]
      textBatch := #[]
      currentTextFontId := none
      -- Execute the command individually
      executeCommand reg cmd
      stats := { stats with individualCalls := stats.individualCalls + 1 }

    i := i + 1

  -- Flush any remaining batches
  stats ← flushAll rectBatch currentCornerRadius strokeRectBatch currentStrokeLineWidth currentStrokeCornerRadius circleBatch lineBatch currentLineWidth textBatch currentTextFontId stats

  let tLoop1 ← IO.monoNanosNow

  -- Calculate timing in milliseconds
  let timeFlattenMs := (tFlatten1 - tFlatten0).toFloat / 1000000.0
  let timeCoalesceMs := (tCoalesce1 - tCoalesce0).toFloat / 1000000.0
  let timeBatchLoopMs := (tLoop1 - tLoop0).toFloat / 1000000.0

  return { stats with
    timeFlattenMs := timeFlattenMs
    timeCoalesceMs := timeCoalesceMs
    timeBatchLoopMs := timeBatchLoopMs
  }

/-- Execute an array of RenderCommands using CanvasM with batching optimization.
    Coalesces commands within scopes to maximize batching, then groups
    consecutive fillRect commands with the same corner radius into batched draw calls. -/
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

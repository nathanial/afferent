/-
  Afferent Widget Backend Batching Helpers
-/
import Afferent.Canvas.Context
import Afferent.Core.Transform
import Afferent.Text.Font
import Afferent.Text.Measurer
import Afferent.Arbor

namespace Afferent.Widget

open Afferent
open Afferent.Arbor

/-! ## Command Batching -/

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

end Afferent.Widget

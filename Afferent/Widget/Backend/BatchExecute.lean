/-
  Afferent Widget Backend Batched Execution
-/
import Afferent.Canvas.Context
import Afferent.Core.Transform
import Afferent.Text.Font
import Afferent.Text.Measurer
import Afferent.Arbor
import Afferent.Widget.Backend.Execute
import Afferent.Widget.Backend.Batches
import Afferent.Widget.Backend.Coalesce

namespace Afferent.Widget

open Afferent
open Afferent.Arbor

/-- Snap text positions to device pixels for axis-aligned transforms (scale + translate only). -/
private def snapTextPosition (x y : Float) (transform : Transform) : (Float × Float) :=
  let eps : Float := 1.0e-4
  let axisAligned := Float.abs transform.b <= eps && Float.abs transform.c <= eps
  if axisAligned && transform.a != 0.0 && transform.d != 0.0 then
    let snappedX := (Float.round (transform.a * x + transform.tx) - transform.tx) / transform.a
    let snappedY := (Float.round (transform.d * y + transform.ty) - transform.ty) / transform.d
    (snappedX, snappedY)
  else
    (x, y)

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
          match (← Shader.lookupFragment fragmentHash) with
          | some fragment =>
            if fragment.paramsPackedFloatCount == fragment.paramsFloatCount then
              FFI.Fragment.draw canvas.ctx.renderer pipeline params canvasWidth canvasHeight
            else if fragment.paramsPackedFloatCount == 0 || params.size % fragment.paramsPackedFloatCount != 0 then
              -- Fallback to direct draw if packing info doesn't match params.
              FFI.Fragment.draw canvas.ctx.renderer pipeline params canvasWidth canvasHeight
            else
              let batchCount := params.size / fragment.paramsPackedFloatCount
              let requiredFloats := batchCount * fragment.paramsFloatCount
              let canvas ← CanvasM.getCanvas
              let canvas ←
                match canvas.fragmentBuffer with
                | some buf =>
                  if canvas.fragmentBufferCapacity >= requiredFloats then
                    pure canvas
                  else
                    FFI.FloatBuffer.destroy buf
                    let newBuf ← FFI.FloatBuffer.create requiredFloats.toUSize
                    pure { canvas with fragmentBuffer := some newBuf, fragmentBufferCapacity := requiredFloats }
                | none =>
                  let newBuf ← FFI.FloatBuffer.create requiredFloats.toUSize
                  pure { canvas with fragmentBuffer := some newBuf, fragmentBufferCapacity := requiredFloats }
              CanvasM.setCanvas canvas
              match canvas.fragmentBuffer with
              | some buf =>
                FFI.FloatBuffer.writePadded buf params
                  fragment.paramsPackedFloatCount.toUInt32
                  fragment.paramsFloatCount.toUInt32
                  fragment.paramsPackOffsets
                FFI.Fragment.drawBuffer canvas.ctx.renderer pipeline buf canvasWidth canvasHeight
              | none =>
                FFI.Fragment.draw canvas.ctx.renderer pipeline params canvasWidth canvasHeight
          | none =>
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
      let transform := canvas.state.transform
      let transformArr := transform.toArray
      let (sx, sy) := snapTextPosition x y transform
      -- Check if we can add to current text batch (same font)
      if textBatch.isEmpty || currentTextFontId == some fontId then
        let entry : TextBatchEntry := {
          text, x := sx, y := sy
          r := color.r, g := color.g, b := color.b, a := color.a
          transform := transformArr
        }
        textBatch := textBatch.push entry
        currentTextFontId := some fontId
      else
        -- Different font - flush and start new batch
        let (s, dt) ← flushTexts textBatch currentTextFontId stats
        stats := s; drawCallTimeNs := drawCallTimeNs + dt
        let entry : TextBatchEntry := {
          text, x := sx, y := sy
          r := color.r, g := color.g, b := color.b, a := color.a
          transform := transformArr
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

end Afferent.Widget

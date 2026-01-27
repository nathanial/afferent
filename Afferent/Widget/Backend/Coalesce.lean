/-
  Afferent Widget Backend Command Coalescing
-/
import Afferent.Core.Transform
import Afferent.Arbor

namespace Afferent.Widget

open Afferent
open Afferent.Arbor

/-! ## Command Coalescing -/

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

/-! ## Overlap-Aware Command Coalescing -/

/-- Compute screen-space bounds for a render command.
    Returns None for state-changing commands that don't have spatial extent. -/
def computeBounds (cmd : RenderCommand) (transform : Transform) : Option CommandBounds :=
  match cmd with
  | .fillRect rect _ _ =>
      let p := transform.apply rect.origin
      some (CommandBounds.fromRect p.x p.y rect.size.width rect.size.height)
  | .fillRectStyle rect _ _ =>
      let p := transform.apply rect.origin
      some (CommandBounds.fromRect p.x p.y rect.size.width rect.size.height)
  | .strokeRect rect _ _ _ =>
      let p := transform.apply rect.origin
      some (CommandBounds.fromRect p.x p.y rect.size.width rect.size.height)
  | .fillCircle center radius _ =>
      let p := transform.apply center
      some (CommandBounds.fromCircle p.x p.y radius)
  | .strokeCircle center radius _ _ =>
      let p := transform.apply center
      some (CommandBounds.fromCircle p.x p.y radius)
  | .fillText _ x y _ _ =>
      let p := transform.apply ⟨x, y⟩
      -- Approximate text bounds (conservative estimate)
      some { minX := p.x, minY := p.y - 20, maxX := p.x + 200, maxY := p.y + 5 }
  | .fillTextBlock _ rect _ _ _ _ =>
      let p := transform.apply rect.origin
      some (CommandBounds.fromRect p.x p.y rect.size.width rect.size.height)
  | .fillPolygon points _ =>
      if points.isEmpty then none
      else
        let transformed := points.map (fun pt => transform.apply pt)
        let minX := transformed.foldl (fun acc p => min acc p.x) transformed[0]!.x
        let maxX := transformed.foldl (fun acc p => max acc p.x) transformed[0]!.x
        let minY := transformed.foldl (fun acc p => min acc p.y) transformed[0]!.y
        let maxY := transformed.foldl (fun acc p => max acc p.y) transformed[0]!.y
        some { minX, minY, maxX, maxY }
  | .strokePolygon points _ _ =>
      if points.isEmpty then none
      else
        let transformed := points.map (fun pt => transform.apply pt)
        let minX := transformed.foldl (fun acc p => min acc p.x) transformed[0]!.x
        let maxX := transformed.foldl (fun acc p => max acc p.x) transformed[0]!.x
        let minY := transformed.foldl (fun acc p => min acc p.y) transformed[0]!.y
        let maxY := transformed.foldl (fun acc p => max acc p.y) transformed[0]!.y
        some { minX, minY, maxX, maxY }
  | .strokeLine p1 p2 _ _ =>
      let tp1 := transform.apply p1
      let tp2 := transform.apply p2
      let minX := min tp1.x tp2.x
      let maxX := max tp1.x tp2.x
      let minY := min tp1.y tp2.y
      let maxY := max tp1.y tp2.y
      some { minX, minY, maxX, maxY }
  | _ => none  -- State-changing commands have no bounds

/-- Check if a path is a simple line (moveTo + lineTo only).
    Returns the two endpoints if so. -/
def isSimpleLine (path : Path) : Option (Point × Point) :=
  if path.commands.size == 2 then
    match path.commands[0]?, path.commands[1]? with
    | some (PathCommand.moveTo p1), some (PathCommand.lineTo p2) => some (p1, p2)
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
            let absP1 := transform.apply p1
            let absP2 := transform.apply p2
            let minX := min absP1.x absP2.x
            let minY := min absP1.y absP2.y
            let maxX := max absP1.x absP2.x
            let maxY := max absP1.y absP2.y
            let bounds := some { minX, minY, maxX, maxY : CommandBounds }
            (.strokeLine absP1 absP2 color lw, bounds)
      | none =>
          (cmd, computeBounds cmd transform)
  | _ =>
  if transform == Transform.identity then
    -- No transform needed, just compute bounds
    (cmd, computeBounds cmd transform)
  else
    match cmd with
    | .fillRect rect color cr =>
        let topLeft := transform.apply rect.origin
        let size := rect.size
        -- For non-rotated transforms, we can flatten to absolute coords
        let absRect : Rect := ⟨topLeft, size⟩
        let bounds := CommandBounds.fromRect topLeft.x topLeft.y size.width size.height
        (.fillRect absRect color cr, some bounds)
    | .fillRectStyle rect style cr =>
        let topLeft := transform.apply rect.origin
        let size := rect.size
        let absRect : Rect := ⟨topLeft, size⟩
        let bounds := CommandBounds.fromRect topLeft.x topLeft.y size.width size.height
        (.fillRectStyle absRect style cr, some bounds)
    | .strokeRect rect color lw cr =>
        let topLeft := transform.apply rect.origin
        let size := rect.size
        let absRect : Rect := ⟨topLeft, size⟩
        let bounds := CommandBounds.fromRect topLeft.x topLeft.y size.width size.height
        (.strokeRect absRect color lw cr, some bounds)
    | .fillCircle center radius color =>
        let absCenter := transform.apply center
        let bounds := CommandBounds.fromCircle absCenter.x absCenter.y radius
        (.fillCircle absCenter radius color, some bounds)
    | .strokeCircle center radius color lw =>
        let absCenter := transform.apply center
        let bounds := CommandBounds.fromCircle absCenter.x absCenter.y radius
        (.strokeCircle absCenter radius color lw, some bounds)
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

end Afferent.Widget

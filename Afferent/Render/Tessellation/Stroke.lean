/-
  Afferent Tessellation Stroke Segments
-/
import Afferent.Core.Types
import Afferent.Core.Path
import Afferent.Core.Paint
import Afferent.Core.Transform
import Afferent.Render.Tessellation.Types
import Afferent.Render.Tessellation.Path

namespace Afferent

namespace Tessellation

/-! ## Stroke Segment Builder (GPU extrusion) -/

/-- Normalize a 2D vector. Returns zero vector if input is zero length. -/
private def normalize (dx dy : Float) : Point :=
  let len := Float.sqrt (dx * dx + dy * dy)
  if len < 0.0001 then ⟨0, 0⟩
  else ⟨dx / len, dy / len⟩

/-- Compute the normal (perpendicular) at a point given the direction. -/
private def computeNormal (dir : Point) : Point :=
  ⟨-dir.y, dir.x⟩

/-- Convert a quadratic Bezier curve to cubic control points. -/
private def quadraticToCubic (p0 cp p1 : Point) : Point × Point :=
  let c1 := Point.lerp p0 cp (2.0 / 3.0)
  let c2 := Point.lerp p1 cp (2.0 / 3.0)
  (c1, c2)

/-- Evaluate a cubic Bezier at parameter t. -/
private def evalCubic (p0 c1 c2 p1 : Point) (t : Float) : Point :=
  let u := 1.0 - t
  let tt := t * t
  let uu := u * u
  let uuu := uu * u
  let ttt := tt * t
  p0 * uuu +
    c1 * (3.0 * uu * t) +
    c2 * (3.0 * u * tt) +
    p1 * ttt

/-- Evaluate the cubic Bezier tangent at parameter t. -/
private def evalCubicTangent (p0 c1 c2 p1 : Point) (t : Float) : Point :=
  let u := 1.0 - t
  let tt := t * t
  let uu := u * u
  let term1 := (c1 - p0) * (3.0 * uu)
  let term2 := (c2 - c1) * (6.0 * u * t)
  let term3 := (p1 - c2) * (3.0 * tt)
  term1 + term2 + term3

/-- Approximate cubic Bezier length by sampling after applying a transform. -/
private def approximateCubicLength (p0 c1 c2 p1 : Point) (transform : Transform)
    (steps : Nat := 12) : Float := Id.run do
  let steps := if steps < 2 then 2 else steps
  let mut length := 0.0
  let mut prev := transform.apply p0
  for i in [1:steps + 1] do
    let t := i.toFloat / steps.toFloat
    let pt := transform.apply (evalCubic p0 c1 c2 p1 t)
    length := length + Point.distance prev pt
    prev := pt
  return length

/-- Internal raw stroke segment before adjacency is computed. -/
private structure RawStrokeSegment where
  p0 : Point
  p1 : Point
  c1 : Point
  c2 : Point
  kind : StrokeSegmentKind
deriving Inhabited

private def mkLineSegment (p0 p1 : Point) : RawStrokeSegment :=
  { p0, p1, c1 := p0, c2 := p1, kind := .line }

private def mkCubicSegment (p0 c1 c2 p1 : Point) : RawStrokeSegment :=
  { p0, p1, c1, c2, kind := .cubic }

private def segmentStartDir (seg : RawStrokeSegment) : Point :=
  match seg.kind with
  | .line =>
      let dx := seg.p1.x - seg.p0.x
      let dy := seg.p1.y - seg.p0.y
      normalize dx dy
  | .cubic =>
      let d := evalCubicTangent seg.p0 seg.c1 seg.c2 seg.p1 0.0
      normalize d.x d.y

private def segmentEndDir (seg : RawStrokeSegment) : Point :=
  match seg.kind with
  | .line =>
      let dx := seg.p1.x - seg.p0.x
      let dy := seg.p1.y - seg.p0.y
      normalize dx dy
  | .cubic =>
      let d := evalCubicTangent seg.p0 seg.c1 seg.c2 seg.p1 1.0
      normalize d.x d.y

private def segmentLength (transform : Transform) (seg : RawStrokeSegment) : Float :=
  match seg.kind with
  | .line => Point.distance (transform.apply seg.p0) (transform.apply seg.p1)
  | .cubic => approximateCubicLength seg.p0 seg.c1 seg.c2 seg.p1 transform

/-- Remove degenerate segments (zero-length). -/
private def filterDegenerateSegments (segments : Array RawStrokeSegment)
    (transform : Transform) : Array RawStrokeSegment := Id.run do
  let mut result : Array RawStrokeSegment := #[]
  for seg in segments do
    if segmentLength transform seg > 0.0001 then
      result := result.push seg
  return result

/-- Finalize raw segments into GPU-ready stroke segments (with adjacency + distances). -/
private def finalizeStrokeSegments (segments : Array RawStrokeSegment) (closed : Bool)
    (transform : Transform) : Array StrokeSegment := Id.run do
  let segments := filterDegenerateSegments segments transform
  if segments.size == 0 then
    return #[]

  let mut result : Array StrokeSegment := Array.mkEmpty segments.size
  let mut dist : Float := 0.0

  for i in [:segments.size] do
    let seg := segments[i]!
    let len := segmentLength transform seg
    let hasPrev := closed || i > 0
    let hasNext := closed || i + 1 < segments.size
    let prevIdx := if hasPrev then (if i == 0 then segments.size - 1 else i - 1) else 0
    let nextIdx := if hasNext then (if i + 1 == segments.size then 0 else i + 1) else 0
    let prevDir := if hasPrev then segmentEndDir (segments[prevIdx]!) else segmentStartDir seg
    let nextDir := if hasNext then segmentStartDir (segments[nextIdx]!) else segmentEndDir seg

    result := result.push {
      p0 := seg.p0
      p1 := seg.p1
      c1 := seg.c1
      c2 := seg.c2
      prevDir := prevDir
      nextDir := nextDir
      startDist := dist
      length := len
      hasPrev := hasPrev
      hasNext := hasNext
      kind := seg.kind
    }

    dist := dist + len

  return result

private def segmentKindFloat (kind : StrokeSegmentKind) : Float :=
  match kind with
  | .line => 0.0
  | .cubic => 1.0

private def pushStrokeSegment (arr : Array Float) (seg : StrokeSegment) : Array Float :=
  arr
    |>.push seg.p0.x |>.push seg.p0.y
    |>.push seg.p1.x |>.push seg.p1.y
    |>.push seg.c1.x |>.push seg.c1.y
    |>.push seg.c2.x |>.push seg.c2.y
    |>.push seg.prevDir.x |>.push seg.prevDir.y
    |>.push seg.nextDir.x |>.push seg.nextDir.y
    |>.push seg.startDist |>.push seg.length
    |>.push (if seg.hasPrev then 1.0 else 0.0)
    |>.push (if seg.hasNext then 1.0 else 0.0)
    |>.push (segmentKindFloat seg.kind)
    |>.push 0.0

/-- Append finalized stroke segments into line/curve buffers. -/
private def appendStrokeSegments (rawSegments : Array RawStrokeSegment) (closed : Bool)
    (transform : Transform)
    (lineSegments : Array Float) (curveSegments : Array Float)
    (lineCount curveCount : Nat) : Array Float × Array Float × Nat × Nat := Id.run do
  if rawSegments.size == 0 then
    return (lineSegments, curveSegments, lineCount, curveCount)
  let finalized := finalizeStrokeSegments rawSegments closed transform
  let mut lineSegs := lineSegments
  let mut curveSegs := curveSegments
  let mut lineCnt := lineCount
  let mut curveCnt := curveCount
  for seg in finalized do
    match seg.kind with
    | .line =>
        lineSegs := pushStrokeSegment lineSegs seg
        lineCnt := lineCnt + 1
    | .cubic =>
        curveSegs := pushStrokeSegment curveSegs seg
        curveCnt := curveCnt + 1
  return (lineSegs, curveSegs, lineCnt, curveCnt)

/-- Recommended subdivision count for cubic segments in the GPU shader. -/
def strokeCurveSubdivisions : Nat := 16

/-- Tessellate a path into GPU stroke segments (no CPU extrusion). -/
def tessellateStrokeSegments (path : Path) (_style : StrokeStyle)
    (transform : Transform := Transform.identity) : StrokePathSegments := Id.run do
  let mut lineSegments : Array Float := #[]
  let mut curveSegments : Array Float := #[]
  let mut lineCount : Nat := 0
  let mut curveCount : Nat := 0

  let mut rawSegments : Array RawStrokeSegment := #[]
  let mut hasCurrent := false
  let mut current := Point.zero
  let mut subpathStart := Point.zero
  let mut closed := false

  for cmd in path.commands do
    match cmd with
    | .moveTo p =>
      if rawSegments.size > 0 then
        let (ls, cs, lc, cc) := appendStrokeSegments rawSegments closed transform
          lineSegments curveSegments lineCount curveCount
        lineSegments := ls
        curveSegments := cs
        lineCount := lc
        curveCount := cc
        rawSegments := #[]
        closed := false
      hasCurrent := true
      current := p
      subpathStart := p
    | .lineTo p =>
      if !hasCurrent then
        hasCurrent := true
        current := p
        subpathStart := p
      else
        rawSegments := rawSegments.push (mkLineSegment current p)
        current := p
    | .quadraticCurveTo cp p =>
      if !hasCurrent then
        hasCurrent := true
        current := p
        subpathStart := p
      else
        let (c1, c2) := quadraticToCubic current cp p
        rawSegments := rawSegments.push (mkCubicSegment current c1 c2 p)
        current := p
    | .bezierCurveTo cp1 cp2 p =>
      if !hasCurrent then
        hasCurrent := true
        current := p
        subpathStart := p
      else
        rawSegments := rawSegments.push (mkCubicSegment current cp1 cp2 p)
        current := p
    | .arc center radius startAngle endAngle counterclockwise =>
      if !hasCurrent then
        -- Treat arc as moveTo end point when no current
        let endPt := Point.mk'
          (center.x + radius * Float.cos endAngle)
          (center.y + radius * Float.sin endAngle)
        hasCurrent := true
        current := endPt
        subpathStart := endPt
      else
        let beziers := Path.arcToBeziers center radius startAngle endAngle counterclockwise
        for (cp1, cp2, endPt) in beziers do
          rawSegments := rawSegments.push (mkCubicSegment current cp1 cp2 endPt)
          current := endPt
    | .arcTo p1 p2 radius =>
      if !hasCurrent then
        hasCurrent := true
        current := p1
        subpathStart := p1
      else
        match computeArcTo current p1 p2 radius with
        | some (t1, beziers, t2) =>
          if Point.distance current t1 > 0.0001 then
            rawSegments := rawSegments.push (mkLineSegment current t1)
          let mut arcCurrent := t1
          for (cp1, cp2, endPt) in beziers do
            rawSegments := rawSegments.push (mkCubicSegment arcCurrent cp1 cp2 endPt)
            arcCurrent := endPt
          current := t2
        | none =>
          rawSegments := rawSegments.push (mkLineSegment current p1)
          current := p1
    | .rect r =>
      if rawSegments.size > 0 then
        let (ls, cs, lc, cc) := appendStrokeSegments rawSegments closed transform
          lineSegments curveSegments lineCount curveCount
        lineSegments := ls
        curveSegments := cs
        lineCount := lc
        curveCount := cc
        rawSegments := #[]
        closed := false
      let p0 := r.topLeft
      let p1 := r.topRight
      let p2 := r.bottomRight
      let p3 := r.bottomLeft
      rawSegments := #[
        mkLineSegment p0 p1,
        mkLineSegment p1 p2,
        mkLineSegment p2 p3,
        mkLineSegment p3 p0
      ]
      closed := true
      current := p0
      subpathStart := p0
      if rawSegments.size > 0 then
        let (ls, cs, lc, cc) := appendStrokeSegments rawSegments closed transform
          lineSegments curveSegments lineCount curveCount
        lineSegments := ls
        curveSegments := cs
        lineCount := lc
        curveCount := cc
        rawSegments := #[]
        closed := false
      hasCurrent := true
    | .closePath =>
      if hasCurrent then
        if Point.distance current subpathStart > 0.0001 then
          rawSegments := rawSegments.push (mkLineSegment current subpathStart)
        closed := true
        current := subpathStart
        if rawSegments.size > 0 then
          let (ls, cs, lc, cc) := appendStrokeSegments rawSegments closed transform
            lineSegments curveSegments lineCount curveCount
          lineSegments := ls
          curveSegments := cs
          lineCount := lc
          curveCount := cc
          rawSegments := #[]
          closed := false
        hasCurrent := true

  if rawSegments.size > 0 then
    let (ls, cs, lc, cc) := appendStrokeSegments rawSegments closed transform
      lineSegments curveSegments lineCount curveCount
    lineSegments := ls
    curveSegments := cs
    lineCount := lc
    curveCount := cc

  return { lineSegments, curveSegments, lineCount, curveCount }

end Tessellation

end Afferent

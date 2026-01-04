/-
  Earcut triangulation (ported from mapbox/earcut)
  Handles polygons with holes and degeneracies.
-/
import Afferent.Core.Types

namespace Afferent

namespace Earcut

structure Node where
  i : Nat
  x : Float
  y : Float
  prev : Nat
  next : Nat
  z : UInt32 := 0
  prevZ : Option Nat := none
  nextZ : Option Nat := none
  steiner : Bool := false
deriving Inhabited

private def getNode (nodes : Array Node) (idx : Nat) : Node :=
  nodes[idx]!

private def setNode (nodes : Array Node) (idx : Nat) (n : Node) : Array Node :=
  nodes.set! idx n

private def updateNode (nodes : Array Node) (idx : Nat) (f : Node → Node) : Array Node :=
  nodes.set! idx (f (nodes[idx]!))

private def setPrev (nodes : Array Node) (idx prev : Nat) : Array Node :=
  updateNode nodes idx (fun n => { n with prev := prev })

private def setNext (nodes : Array Node) (idx next : Nat) : Array Node :=
  updateNode nodes idx (fun n => { n with next := next })

private def setPrevZ (nodes : Array Node) (idx : Nat) (prev : Option Nat) : Array Node :=
  updateNode nodes idx (fun n => { n with prevZ := prev })

private def setNextZ (nodes : Array Node) (idx : Nat) (next : Option Nat) : Array Node :=
  updateNode nodes idx (fun n => { n with nextZ := next })

private def setZ (nodes : Array Node) (idx : Nat) (z : UInt32) : Array Node :=
  updateNode nodes idx (fun n => { n with z := z })

private def createNode (i : Nat) (x y : Float) : Node :=
  { i, x, y, prev := 0, next := 0, z := 0, prevZ := none, nextZ := none, steiner := false }

private def equalsIdx (nodes : Array Node) (a b : Nat) : Bool :=
  let na := getNode nodes a
  let nb := getNode nodes b
  na.x == nb.x && na.y == nb.y

private def areaIdx (nodes : Array Node) (a b c : Nat) : Float :=
  let pa := getNode nodes a
  let pb := getNode nodes b
  let pc := getNode nodes c
  (pb.y - pa.y) * (pc.x - pb.x) - (pb.x - pa.x) * (pc.y - pb.y)

private def signedArea (data : Array Float) (start end' dim : Nat) : Float := Id.run do
  if end' <= start + dim then
    return 0.0
  let mut sum := 0.0
  let mut i := start
  let mut j := end' - dim
  while i < end' do
    let xi := data[i]!
    let yi := data[i + 1]!
    let xj := data[j]!
    let yj := data[j + 1]!
    sum := sum + (xj - xi) * (yi + yj)
    j := i
    i := i + dim
  return sum

private def insertNode (nodes : Array Node) (i : Nat) (x y : Float) (last : Option Nat)
    : Array Node × Nat := Id.run do
  let idx := nodes.size
  let mut n := createNode i x y
  -- temporarily self-link; will be updated if last exists
  n := { n with prev := idx, next := idx }
  let mut nodes := nodes.push n
  match last with
  | none =>
      return (nodes, idx)
  | some lastIdx =>
      let lastNode := getNode nodes lastIdx
      let nextIdx := lastNode.next
      nodes := setPrev nodes idx lastIdx
      nodes := setNext nodes idx nextIdx
      nodes := setNext nodes lastIdx idx
      nodes := setPrev nodes nextIdx idx
      return (nodes, idx)

private def removeNode (nodes : Array Node) (idx : Nat) : Array Node := Id.run do
  let n := getNode nodes idx
  let prevIdx := n.prev
  let nextIdx := n.next
  let mut nodes := setNext nodes prevIdx nextIdx
  nodes := setPrev nodes nextIdx prevIdx
  match n.prevZ with
  | some pz => nodes := setNextZ nodes pz n.nextZ
  | none => pure ()
  match n.nextZ with
  | some nz => nodes := setPrevZ nodes nz n.prevZ
  | none => pure ()
  return nodes

private def linkedList (nodes0 : Array Node) (data : Array Float) (start end' dim : Nat) (clockwise : Bool)
    : Array Node × Option Nat := Id.run do
  let area := signedArea data start end' dim
  let forward := clockwise == (area > 0)
  let mut nodes := nodes0
  let mut last : Option Nat := none

  if forward then
    let mut i := start
    while i < end' do
      let (nodes', idx) := insertNode nodes (i / dim) data[i]! data[i + 1]! last
      nodes := nodes'
      last := some idx
      i := i + dim
  else
    let count := if end' > start then (end' - start) / dim else 0
    for k in [:count] do
      let i := end' - dim - k * dim
      let (nodes', idx) := insertNode nodes (i / dim) data[i]! data[i + 1]! last
      nodes := nodes'
      last := some idx

  match last with
  | none => return (nodes, none)
  | some lastIdx =>
      let nextIdx := (getNode nodes lastIdx).next
      if equalsIdx nodes lastIdx nextIdx then
        nodes := removeNode nodes lastIdx
        return (nodes, some nextIdx)
      return (nodes, some lastIdx)

private def filterPoints (nodes : Array Node) (start : Option Nat) (end' : Option Nat := none)
    : Array Node × Option Nat := Id.run do
  match start with
  | none => return (nodes, none)
  | some s =>
      let mut nodes := nodes
      let mut p := s
      let mut stop := end'.getD s
      let mut again := true
      while again || p != stop do
        again := false
        let n := getNode nodes p
        let prevIdx := n.prev
        let nextIdx := n.next
        if !n.steiner && (equalsIdx nodes p nextIdx || areaIdx nodes prevIdx p nextIdx == 0) then
          nodes := removeNode nodes p
          p := prevIdx
          stop := prevIdx
          if p == (getNode nodes p).next then
            break
          again := true
        else
          p := nextIdx
      return (nodes, some stop)

private def pointInTriangle (ax ay bx by_ cx cy px py : Float) : Bool :=
  (cx - px) * (ay - py) >= (ax - px) * (cy - py) &&
  (ax - px) * (by_ - py) >= (bx - px) * (ay - py) &&
  (bx - px) * (cy - py) >= (cx - px) * (by_ - py)

private def pointInTriangleExceptFirst (ax ay bx by_ cx cy px py : Float) : Bool :=
  !(ax == px && ay == py) && pointInTriangle ax ay bx by_ cx cy px py

private def zOrder (x y minX minY invSize : Float) : UInt32 :=
  let x := ((x - minX) * invSize).toUInt32
  let y := ((y - minY) * invSize).toUInt32
  let x := (x ||| (x <<< 8)) &&& 0x00FF00FF
  let x := (x ||| (x <<< 4)) &&& 0x0F0F0F0F
  let x := (x ||| (x <<< 2)) &&& 0x33333333
  let x := (x ||| (x <<< 1)) &&& 0x55555555
  let y := (y ||| (y <<< 8)) &&& 0x00FF00FF
  let y := (y ||| (y <<< 4)) &&& 0x0F0F0F0F
  let y := (y ||| (y <<< 2)) &&& 0x33333333
  let y := (y ||| (y <<< 1)) &&& 0x55555555
  x ||| (y <<< 1)

private def sortLinked (nodes : Array Node) (list : Option Nat) : Array Node × Option Nat := Id.run do
  match list with
  | none => return (nodes, none)
  | some l =>
      let mut nodes := nodes
      let mut inSize := 1
      let mut list := some l
      let mut numMerges := 0
      while true do
        let mut p := list
        list := none
        let mut tail : Option Nat := none
        numMerges := 0

        while p.isSome do
          numMerges := numMerges + 1
          let mut q := p
          let mut pSize := 0
          for _ in [:inSize] do
            if q.isNone then
              break
            pSize := pSize + 1
            q := (getNode nodes q.get!).nextZ
          let mut qSize := inSize

          while pSize > 0 || (qSize > 0 && q.isSome) do
            let mut e : Nat := 0
            if pSize > 0 && (qSize == 0 || q.isNone) then
              let pIdx := p.get!
              e := pIdx
              p := (getNode nodes pIdx).nextZ
              pSize := pSize - 1
            else if pSize > 0 && q.isSome then
              let pIdx := p.get!
              let qIdx := q.get!
              let pz := (getNode nodes pIdx).z
              let qz := (getNode nodes qIdx).z
              if pz <= qz then
                e := pIdx
                p := (getNode nodes pIdx).nextZ
                pSize := pSize - 1
              else
                e := qIdx
                q := (getNode nodes qIdx).nextZ
                qSize := qSize - 1
            else
              let qIdx := q.get!
              e := qIdx
              q := (getNode nodes qIdx).nextZ
              qSize := qSize - 1

            match tail with
            | some t => nodes := setNextZ nodes t (some e)
            | none => list := some e
            nodes := setPrevZ nodes e tail
            tail := some e
          p := q

        match tail with
        | some t => nodes := setNextZ nodes t none
        | none => pure ()

        if numMerges <= 1 then
          return (nodes, list)
        inSize := inSize * 2

      return (nodes, list)

private def indexCurve (nodes : Array Node) (start : Nat) (minX minY invSize : Float)
    : Array Node × Nat := Id.run do
  let mut nodes := nodes
  let mut p := start
  while true do
    let n := getNode nodes p
    if n.z == 0 then
      nodes := setZ nodes p (zOrder n.x n.y minX minY invSize)
    nodes := setPrevZ nodes p (some n.prev)
    nodes := setNextZ nodes p (some n.next)
    p := n.next
    if p == start then
      break

  -- break circular list
  let last := (getNode nodes start).prev
  nodes := setNextZ nodes last none
  nodes := setPrevZ nodes start none
  let (nodes', head) := sortLinked nodes (some start)
  nodes := nodes'
  match head with
  | none => return (nodes, start)
  | some h => return (nodes, h)

private def isEar (nodes : Array Node) (ear : Nat) : Bool := Id.run do
  let a := (getNode nodes ear).prev
  let c := (getNode nodes ear).next
  let b := ear
  if areaIdx nodes a b c >= 0 then
    return false

  let na := getNode nodes a
  let nb := getNode nodes b
  let nc := getNode nodes c
  let ax := na.x
  let ay := na.y
  let bx := nb.x
  let by_ := nb.y
  let cx := nc.x
  let cy := nc.y

  let x0 := min ax (min bx cx)
  let y0 := min ay (min by_ cy)
  let x1 := max ax (max bx cx)
  let y1 := max ay (max by_ cy)

  let mut p := (getNode nodes c).next
  while p != a do
    let np := getNode nodes p
    if np.x >= x0 && np.x <= x1 && np.y >= y0 && np.y <= y1 &&
        pointInTriangleExceptFirst ax ay bx by_ cx cy np.x np.y &&
        areaIdx nodes np.prev p np.next >= 0 then
      return false
    p := np.next
  return true

private def isEarHashed (nodes : Array Node) (ear : Nat) (minX minY invSize : Float) : Bool := Id.run do
  let a := (getNode nodes ear).prev
  let c := (getNode nodes ear).next
  let b := ear
  if areaIdx nodes a b c >= 0 then
    return false

  let na := getNode nodes a
  let nb := getNode nodes b
  let nc := getNode nodes c
  let ax := na.x
  let ay := na.y
  let bx := nb.x
  let by_ := nb.y
  let cx := nc.x
  let cy := nc.y

  let x0 := min ax (min bx cx)
  let y0 := min ay (min by_ cy)
  let x1 := max ax (max bx cx)
  let y1 := max ay (max by_ cy)

  let minZ := zOrder x0 y0 minX minY invSize
  let maxZ := zOrder x1 y1 minX minY invSize

  let mut p := (getNode nodes ear).prevZ
  let mut n := (getNode nodes ear).nextZ

  while p.isSome && n.isSome do
    let pIdx := p.get!
    let nIdx := n.get!
    let pNode := getNode nodes pIdx
    let nNode := getNode nodes nIdx
    if pNode.z < minZ || nNode.z > maxZ then
      break
    if pNode.x >= x0 && pNode.x <= x1 && pNode.y >= y0 && pNode.y <= y1 && pIdx != a && pIdx != c &&
        pointInTriangleExceptFirst ax ay bx by_ cx cy pNode.x pNode.y &&
        areaIdx nodes pNode.prev pIdx pNode.next >= 0 then
      return false
    if nNode.x >= x0 && nNode.x <= x1 && nNode.y >= y0 && nNode.y <= y1 && nIdx != a && nIdx != c &&
        pointInTriangleExceptFirst ax ay bx by_ cx cy nNode.x nNode.y &&
        areaIdx nodes nNode.prev nIdx nNode.next >= 0 then
      return false
    p := pNode.prevZ
    n := nNode.nextZ

  while p.isSome do
    let pIdx := p.get!
    let pNode := getNode nodes pIdx
    if pNode.z < minZ then
      break
    if pNode.x >= x0 && pNode.x <= x1 && pNode.y >= y0 && pNode.y <= y1 && pIdx != a && pIdx != c &&
        pointInTriangleExceptFirst ax ay bx by_ cx cy pNode.x pNode.y &&
        areaIdx nodes pNode.prev pIdx pNode.next >= 0 then
      return false
    p := pNode.prevZ

  while n.isSome do
    let nIdx := n.get!
    let nNode := getNode nodes nIdx
    if nNode.z > maxZ then
      break
    if nNode.x >= x0 && nNode.x <= x1 && nNode.y >= y0 && nNode.y <= y1 && nIdx != a && nIdx != c &&
        pointInTriangleExceptFirst ax ay bx by_ cx cy nNode.x nNode.y &&
        areaIdx nodes nNode.prev nIdx nNode.next >= 0 then
      return false
    n := nNode.nextZ

  return true

private def sign (num : Float) : Int :=
  if num > 0 then 1 else if num < 0 then -1 else 0

private def onSegment (p q r : Node) : Bool :=
  q.x <= max p.x r.x && q.x >= min p.x r.x &&
  q.y <= max p.y r.y && q.y >= min p.y r.y

private def intersects (nodes : Array Node) (p1 q1 p2 q2 : Nat) : Bool :=
  let a := getNode nodes p1
  let b := getNode nodes q1
  let c := getNode nodes p2
  let d := getNode nodes q2
  let o1 := sign (areaIdx nodes p1 q1 p2)
  let o2 := sign (areaIdx nodes p1 q1 q2)
  let o3 := sign (areaIdx nodes p2 q2 p1)
  let o4 := sign (areaIdx nodes p2 q2 q1)
  if o1 != o2 && o3 != o4 then
    true
  else if o1 == 0 && onSegment a c b then
    true
  else if o2 == 0 && onSegment a d b then
    true
  else if o3 == 0 && onSegment c a d then
    true
  else if o4 == 0 && onSegment c b d then
    true
  else
    false

private def intersectsPolygon (nodes : Array Node) (a b : Nat) : Bool := Id.run do
  let aI := (getNode nodes a).i
  let bI := (getNode nodes b).i
  let mut p := a
  while true do
    let n := getNode nodes p
    let pI := n.i
    let nI := (getNode nodes n.next).i
    if pI != aI && nI != aI && pI != bI && nI != bI &&
        intersects nodes p n.next a b then
      return true
    p := n.next
    if p == a then
      break
  return false

private def locallyInside (nodes : Array Node) (a b : Nat) : Bool :=
  if areaIdx nodes (getNode nodes a).prev a (getNode nodes a).next < 0 then
    areaIdx nodes a b (getNode nodes a).next >= 0 &&
    areaIdx nodes a (getNode nodes a).prev b >= 0
  else
    areaIdx nodes a b (getNode nodes a).prev < 0 ||
    areaIdx nodes a (getNode nodes a).next b < 0

private def middleInside (nodes : Array Node) (a b : Nat) : Bool := Id.run do
  let mut p := a
  let mut inside := false
  let pa := getNode nodes a
  let pb := getNode nodes b
  let px := (pa.x + pb.x) / 2.0
  let py := (pa.y + pb.y) / 2.0
  while true do
    let pn := getNode nodes p
    let pnNext := getNode nodes pn.next
    if ((pn.y > py) != (pnNext.y > py)) && pnNext.y != pn.y &&
        (px < (pnNext.x - pn.x) * (py - pn.y) / (pnNext.y - pn.y) + pn.x) then
      inside := !inside
    p := pn.next
    if p == a then
      break
  return inside

private def isValidDiagonal (nodes : Array Node) (a b : Nat) : Bool :=
  (getNode nodes a).next != b && (getNode nodes a).prev != b &&
  !intersectsPolygon nodes a b &&
  (locallyInside nodes a b && locallyInside nodes b a && middleInside nodes a b &&
      (areaIdx nodes (getNode nodes a).prev a (getNode nodes b).prev != 0 ||
       areaIdx nodes a (getNode nodes b).prev b != 0) ||
   equalsIdx nodes a b && areaIdx nodes (getNode nodes a).prev a (getNode nodes a).next > 0 &&
     areaIdx nodes (getNode nodes b).prev b (getNode nodes b).next > 0)

private def splitPolygon (nodes : Array Node) (a b : Nat) : Array Node × Nat := Id.run do
  let na := getNode nodes a
  let nb := getNode nodes b
  let (nodes, a2) := insertNode nodes na.i na.x na.y none
  let (nodes, b2) := insertNode nodes nb.i nb.x nb.y none
  let an := na.next
  let bp := nb.prev

  let mut nodes := nodes
  nodes := setNext nodes a b
  nodes := setPrev nodes b a

  nodes := setNext nodes a2 an
  nodes := setPrev nodes an a2

  nodes := setNext nodes b2 a2
  nodes := setPrev nodes a2 b2

  nodes := setNext nodes bp b2
  nodes := setPrev nodes b2 bp

  return (nodes, b2)

private def cureLocalIntersections (nodes : Array Node) (start : Option Nat)
    (triangles : Array UInt32) : Array Node × Array UInt32 × Option Nat := Id.run do
  match start with
  | none => return (nodes, triangles, none)
  | some s =>
      let mut nodes := nodes
      let mut triangles := triangles
      let mut p := s
      let mut start := s
      while true do
        let a := (getNode nodes p).prev
        let pNext := (getNode nodes p).next
        let b := (getNode nodes pNext).next
        if !equalsIdx nodes a b && intersects nodes a p pNext b &&
            locallyInside nodes a b && locallyInside nodes b a then
          triangles := triangles.push (getNode nodes a).i.toUInt32
          triangles := triangles.push (getNode nodes p).i.toUInt32
          triangles := triangles.push (getNode nodes b).i.toUInt32
          nodes := removeNode nodes p
          nodes := removeNode nodes pNext
          p := b
          start := b
        p := (getNode nodes p).next
        if p == start then
          break
      let (nodes', stopIdx) := filterPoints nodes (some p)
      return (nodes', triangles, stopIdx)

mutual
  private partial def earcutLinked (nodes : Array Node) (ear : Option Nat) (triangles : Array UInt32)
      (dim : Nat) (minX minY invSize : Float) (pass : Nat) : Array Node × Array UInt32 := Id.run do
    match ear with
    | none => return (nodes, triangles)
    | some e =>
      let mut nodes := nodes
      let mut triangles := triangles
      let mut ear := e
      if pass == 0 && invSize != 0.0 then
        let (nodes', head) := indexCurve nodes ear minX minY invSize
        nodes := nodes'
        ear := head
      let mut stop := ear
      while (getNode nodes ear).prev != (getNode nodes ear).next do
        let prev := (getNode nodes ear).prev
        let next := (getNode nodes ear).next
        let isEarOk :=
          if invSize != 0.0 then
            isEarHashed nodes ear minX minY invSize
          else
            isEar nodes ear
        if isEarOk then
          triangles := triangles.push (getNode nodes prev).i.toUInt32
          triangles := triangles.push (getNode nodes ear).i.toUInt32
          triangles := triangles.push (getNode nodes next).i.toUInt32
          nodes := removeNode nodes ear
          ear := (getNode nodes next).next
          stop := ear
        else
          ear := next
          if ear == stop then
            if pass == 0 then
              let (nodes', filtered) := filterPoints nodes (some ear)
              let (nodes', triangles') := earcutLinked nodes' filtered triangles dim minX minY invSize 1
              return (nodes', triangles')
            else if pass == 1 then
              let (nodes', triangles', cured) := cureLocalIntersections nodes (some ear) triangles
              let (nodes', triangles'') := earcutLinked nodes' cured triangles' dim minX minY invSize 2
              return (nodes', triangles'')
            else if pass == 2 then
              let (nodes', triangles') := splitEarcut nodes (some ear) triangles dim minX minY invSize
              return (nodes', triangles')
      return (nodes, triangles)

  private partial def splitEarcut (nodes : Array Node) (start : Option Nat) (triangles : Array UInt32)
      (dim : Nat) (minX minY invSize : Float) : Array Node × Array UInt32 := Id.run do
    match start with
    | none => return (nodes, triangles)
    | some s =>
        let mut nodes := nodes
        let mut triangles := triangles
        let mut a := s
        while true do
          let mut b := (getNode nodes a).next
          b := (getNode nodes b).next
          while b != (getNode nodes a).prev do
            if (getNode nodes a).i != (getNode nodes b).i && isValidDiagonal nodes a b then
              let (nodes', c) := splitPolygon nodes a b
              nodes := nodes'
              let (nodes', aFiltered) := filterPoints nodes (some a) (some (getNode nodes a).next)
              nodes := nodes'
              let (nodes', cFiltered) := filterPoints nodes (some c) (some (getNode nodes c).next)
              nodes := nodes'
              let (nodes1, triangles1) := earcutLinked nodes aFiltered triangles dim minX minY invSize 0
              let (nodes2, triangles2) := earcutLinked nodes1 cFiltered triangles1 dim minX minY invSize 0
              return (nodes2, triangles2)
            b := (getNode nodes b).next
          a := (getNode nodes a).next
          if a == s then
            break
        return (nodes, triangles)
end

private def sectorContainsSector (nodes : Array Node) (m p : Nat) : Bool :=
  areaIdx nodes (getNode nodes m).prev m (getNode nodes p).prev < 0 &&
  areaIdx nodes (getNode nodes p).next m (getNode nodes m).next < 0

private def findHoleBridge (nodes : Array Node) (hole outerNode : Nat) : Option Nat := Id.run do
  let mut p := outerNode
  let hx := (getNode nodes hole).x
  let hy := (getNode nodes hole).y
  let mut qx := (-1.0e30)
  let mut m : Option Nat := none

  while true do
    let pn := getNode nodes p
    let pnNext := getNode nodes pn.next
    if hy <= pn.y && hy >= pnNext.y && pnNext.y != pn.y then
      let x := pn.x + (hy - pn.y) * (pnNext.x - pn.x) / (pnNext.y - pn.y)
      if x <= hx && x > qx then
        qx := x
        if x == hx then
          if hy == pn.y then return some p
          if hy == pnNext.y then return some pn.next
        m := some (if pn.x < pnNext.x then p else pn.next)
    p := pn.next
    if p == outerNode then
      break

  if m.isNone then return none
  if hx == qx then return m

  let stop := m.get!
  let mx := (getNode nodes stop).x
  let my := (getNode nodes stop).y
  let mut tanMin := 1.0e30

  p := stop
  while true do
    let pn := getNode nodes p
    if hx >= pn.x && pn.x >= mx && hx != pn.x &&
        pointInTriangle (if hy < my then hx else qx) hy mx my (if hy < my then qx else hx) hy pn.x pn.y then
      let tan := Float.abs (hy - pn.y) / (hx - pn.x)
      if locallyInside nodes p hole &&
          (tan < tanMin || (tan == tanMin && (pn.x > (getNode nodes stop).x ||
            (pn.x == (getNode nodes stop).x && sectorContainsSector nodes stop p)))) then
        m := some p
        tanMin := tan
    p := pn.next
    if p == stop then
      break
  return m

private def eliminateHole (nodes : Array Node) (hole outerNode : Nat) : Array Node × Nat := Id.run do
  match findHoleBridge nodes hole outerNode with
  | none => return (nodes, outerNode)
  | some bridge =>
      let (nodes, bridgeReverse) := splitPolygon nodes bridge hole
      let (nodes, _) := filterPoints nodes (some bridgeReverse) (some (getNode nodes bridgeReverse).next)
      let (nodes, filtered) := filterPoints nodes (some bridge) (some (getNode nodes bridge).next)
      return (nodes, filtered.getD bridge)

private def compareXYSlope (nodes : Array Node) (a b : Nat) : Bool :=
  let na := getNode nodes a
  let nb := getNode nodes b
  if na.x < nb.x then true
  else if na.x > nb.x then false
  else if na.y < nb.y then true
  else if na.y > nb.y then false
  else
    let naNext := getNode nodes na.next
    let nbNext := getNode nodes nb.next
    let aSlope := (naNext.y - na.y) / (naNext.x - na.x)
    let bSlope := (nbNext.y - nb.y) / (nbNext.x - nb.x)
    aSlope < bSlope

private def sortQueue (nodes : Array Node) (queue : Array Nat) : Array Nat := Id.run do
  let mut result : Array Nat := #[]
  for item in queue do
    let mut inserted := false
    let mut newResult : Array Nat := Array.mkEmpty (result.size + 1)
    for i in [:result.size] do
      if !inserted && compareXYSlope nodes item result[i]! then
        newResult := newResult.push item
        inserted := true
      newResult := newResult.push result[i]!
    if !inserted then
      newResult := newResult.push item
    result := newResult
  return result

private def getLeftmost (nodes : Array Node) (start : Nat) : Nat := Id.run do
  let mut p := start
  let mut leftmost := start
  while true do
    let pn := getNode nodes p
    let ln := getNode nodes leftmost
    if pn.x < ln.x || (pn.x == ln.x && pn.y < ln.y) then
      leftmost := p
    p := pn.next
    if p == start then
      break
  return leftmost

private def eliminateHoles (nodes : Array Node) (data : Array Float) (holeIndices : Array Nat)
    (outerNode : Nat) (dim : Nat) : Array Node × Nat := Id.run do
  let mut nodes := nodes
  let mut queue : Array Nat := #[]
  for i in [:holeIndices.size] do
    let start := holeIndices[i]! * dim
    let end' :=
      if i + 1 < holeIndices.size then holeIndices[i + 1]! * dim
      else data.size
    let (nodes', list) := linkedList nodes data start end' dim false
    nodes := nodes'
    match list with
    | none => pure ()
    | some l =>
        let nodes' := updateNode nodes l (fun n => if n.next == l then { n with steiner := true } else n)
        queue := queue.push (getLeftmost nodes' l)
        nodes := nodes'
  queue := sortQueue nodes queue
  let mut outer := outerNode
  for h in queue do
    let (nodes', outer') := eliminateHole nodes h outer
    nodes := nodes'
    outer := outer'
  return (nodes, outer)

def earcut (data : Array Float) (holeIndices : Array Nat := #[]) (dim : Nat := 2) : Array UInt32 := Id.run do
  let hasHoles := holeIndices.size > 0
  let outerLen := if hasHoles then holeIndices[0]! * dim else data.size
  let (nodes, outerNodeOpt) := linkedList #[] data 0 outerLen dim true
  let mut nodes := nodes
  let mut triangles : Array UInt32 := #[]
  match outerNodeOpt with
  | none => return triangles
  | some outerNode =>
      if (getNode nodes outerNode).next == (getNode nodes outerNode).prev then
        return triangles

      let mut outerNode := outerNode

      if hasHoles then
        let (nodes', outer') := eliminateHoles nodes data holeIndices outerNode dim
        nodes := nodes'
        outerNode := outer'

      let mut minX := 0.0
      let mut minY := 0.0
      let mut invSize := 0.0
      if data.size > 80 * dim then
        minX := data[0]!
        minY := data[1]!
        let mut maxX := minX
        let mut maxY := minY
        let mut i := dim
        while i < outerLen do
          let x := data[i]!
          let y := data[i + 1]!
          if x < minX then minX := x
          if y < minY then minY := y
          if x > maxX then maxX := x
          if y > maxY then maxY := y
          i := i + dim
        let size := max (maxX - minX) (maxY - minY)
        invSize := if size != 0 then 32767.0 / size else 0.0

      let (nodes', triangles') := earcutLinked nodes (some outerNode) triangles dim minX minY invSize 0
      nodes := nodes'
      triangles := triangles'
      return triangles

def deviation (data : Array Float) (holeIndices : Array Nat) (dim : Nat)
    (triangles : Array UInt32) : Float := Id.run do
  let hasHoles := holeIndices.size > 0
  let outerLen := if hasHoles then holeIndices[0]! * dim else data.size
  let mut polygonArea := Float.abs (signedArea data 0 outerLen dim)
  if hasHoles then
    for i in [:holeIndices.size] do
      let start := holeIndices[i]! * dim
      let end' :=
        if i + 1 < holeIndices.size then holeIndices[i + 1]! * dim
        else data.size
      polygonArea := polygonArea - Float.abs (signedArea data start end' dim)
  let mut trianglesArea := 0.0
  let mut i := 0
  while i + 2 < triangles.size do
    let a := triangles[i]!.toNat * dim
    let b := triangles[i + 1]!.toNat * dim
    let c := triangles[i + 2]!.toNat * dim
    trianglesArea := trianglesArea + Float.abs (
      (data[a]! - data[c]!) * (data[b + 1]! - data[a + 1]!) -
      (data[a]! - data[b]!) * (data[c + 1]! - data[a + 1]!)
    )
    i := i + 3
  if polygonArea == 0 && trianglesArea == 0 then
    return 0.0
  return Float.abs ((trianglesArea - polygonArea) / polygonArea)

end Earcut

end Afferent

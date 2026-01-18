/-
  Heartbeat Spinner - Pulsing shape with ECG-like rhythm
  Uses pre-tessellated heart geometry for instanced rendering.
-/
import Afferent.Canopy.Core
import Afferent.Canopy.Widget.Display.Spinner.Core

namespace Afferent.Canopy.Spinner

open Afferent.Arbor hiding Event
open Linalg

/-! ## Precomputed Heart Tessellation for Instanced Rendering -/

/-- Flatten a cubic Bézier curve using de Casteljau subdivision.
    Returns array of points (excluding start point). -/
private partial def flattenCubic (p0x p0y p1x p1y p2x p2y p3x p3y : Float)
    (tolerance : Float := 0.02) : Array (Float × Float) :=
  let rec go (p0x p0y p1x p1y p2x p2y p3x p3y : Float)
      (acc : Array (Float × Float)) : Array (Float × Float) :=
    -- Check flatness: distance from control points to line p0-p3
    let dx := p3x - p0x
    let dy := p3y - p0y
    let len := Float.sqrt (dx * dx + dy * dy)
    let d1 := if len < 0.0001 then 0.0
              else Float.abs ((p1x - p0x) * dy - (p1y - p0y) * dx) / len
    let d2 := if len < 0.0001 then 0.0
              else Float.abs ((p2x - p0x) * dy - (p2y - p0y) * dx) / len
    if max d1 d2 < tolerance then
      acc.push (p3x, p3y)
    else
      -- Subdivide at t=0.5 using de Casteljau
      let m01x := (p0x + p1x) * 0.5; let m01y := (p0y + p1y) * 0.5
      let m12x := (p1x + p2x) * 0.5; let m12y := (p1y + p2y) * 0.5
      let m23x := (p2x + p3x) * 0.5; let m23y := (p2y + p3y) * 0.5
      let m012x := (m01x + m12x) * 0.5; let m012y := (m01y + m12y) * 0.5
      let m123x := (m12x + m23x) * 0.5; let m123y := (m12y + m23y) * 0.5
      let midx := (m012x + m123x) * 0.5; let midy := (m012y + m123y) * 0.5
      let acc' := go p0x p0y m01x m01y m012x m012y midx midy acc
      go midx midy m123x m123y m23x m23y p3x p3y acc'
  go p0x p0y p1x p1y p2x p2y p3x p3y #[]

/-- Tessellate a canonical heart (centered at origin, size = 1.0) as triangles.
    Returns (vertices, indices, centerX, centerY) for instanced rendering.
    Heart shape from Path.heart with size=1:
    - moveTo (0, 0.3)
    - bezierCurveTo (-0.5, -0.2) (-0.5, -0.5) (0, -0.2)
    - bezierCurveTo (0.5, -0.5) (0.5, -0.2) (0, 0.3)
    - closePath -/
private def tessellateCanonicalHeart : Array Float × Array UInt32 × Float × Float := Id.run do
  -- Flatten the two Bézier curves
  -- First curve: (0, 0.3) -> (-0.5, -0.2), (-0.5, -0.5) -> (0, -0.2)
  let curve1 := flattenCubic 0.0 0.3 (-0.5) (-0.2) (-0.5) (-0.5) 0.0 (-0.2)
  -- Second curve: (0, -0.2) -> (0.5, -0.5), (0.5, -0.2) -> (0, 0.3)
  let curve2 := flattenCubic 0.0 (-0.2) 0.5 (-0.5) 0.5 (-0.2) 0.0 0.3

  -- Build perimeter points: start point + curve1 + curve2 (excluding duplicates)
  let mut perimeterPoints : Array (Float × Float) := #[(0.0, 0.3)]
  for pt in curve1 do
    perimeterPoints := perimeterPoints.push pt
  -- Skip first point of curve2 (it's the same as last of curve1)
  for pt in curve2 do
    perimeterPoints := perimeterPoints.push pt
  -- Remove last point if it duplicates the first (closed path)
  if perimeterPoints.size > 1 then
    let (lastX, lastY) := perimeterPoints[perimeterPoints.size - 1]!
    let (firstX, firstY) := perimeterPoints[0]!
    if Float.abs (lastX - firstX) < 0.001 && Float.abs (lastY - firstY) < 0.001 then
      perimeterPoints := perimeterPoints.pop

  let numPoints := perimeterPoints.size

  -- Compute centroid for center vertex (better triangulation than origin)
  let mut sumX := 0.0
  let mut sumY := 0.0
  for (x, y) in perimeterPoints do
    sumX := sumX + x
    sumY := sumY + y
  let centerX := sumX / numPoints.toFloat
  let centerY := sumY / numPoints.toFloat

  -- Vertices: center + perimeter points
  -- Layout: [cx, cy, x0, y0, x1, y1, ...]
  let mut vertices : Array Float := Array.mkEmpty ((numPoints + 1) * 2)
  vertices := vertices.push centerX |>.push centerY  -- Center vertex
  for (x, y) in perimeterPoints do
    vertices := vertices.push x |>.push y

  -- Indices: triangle fan from center
  let mut indices : Array UInt32 := Array.mkEmpty (numPoints * 3)
  for i in [:numPoints] do
    indices := indices.push 0  -- Center
    indices := indices.push (i + 1).toUInt32
    let nextIdx := if i + 1 < numPoints then i + 2 else 1
    indices := indices.push nextIdx.toUInt32

  return (vertices, indices, centerX, centerY)

/-- Pre-computed heart tessellation. -/
private def heartTessellation : Array Float × Array UInt32 × Float × Float :=
  tessellateCanonicalHeart

/-- Hash for heart mesh (FNV-1a style). -/
private def heartHash : UInt64 := 0x68656172745F5F5F  -- "heart___"

/-- Heartbeat: Pulsing shape with ECG-like rhythm.
    Uses pre-tessellated heart geometry with instanced rendering for better performance. -/
def heartbeatSpec (t : Float) (color : Color) (dims : Dimensions) : CustomSpec := {
  measure := fun _ _ => (dims.size, dims.size)
  collect := fun layout =>
    let rect := layout.contentRect
    let cx := rect.x + dims.size / 2
    let cy := rect.y + dims.size / 2
    let baseSize := dims.size * 0.25

    -- ECG-like timing: quick pulse, pause, repeat
    let cyclePos := t
    let scale := if cyclePos < 0.15 then
        1.0 + 0.3 * Float.sin (cyclePos / 0.15 * Float.pi)  -- First beat
      else if cyclePos < 0.3 then
        1.0 - 0.1 * Float.sin ((cyclePos - 0.15) / 0.15 * Float.pi)  -- Slight dip
      else if cyclePos < 0.45 then
        1.0 + 0.2 * Float.sin ((cyclePos - 0.3) / 0.15 * Float.pi)  -- Second beat
      else
        1.0  -- Rest

    let (heartVerts, heartIndices, heartCX, heartCY) := heartTessellation

    RenderM.build do
      let heartInstance : MeshInstance := {
        x := cx, y := cy
        rotation := 0.0
        scale := baseSize * scale
        r := color.r, g := color.g, b := color.b, a := color.a
      }
      RenderM.fillPolygonInstanced heartHash heartVerts heartIndices #[heartInstance] heartCX heartCY
  draw := none
}

end Afferent.Canopy.Spinner

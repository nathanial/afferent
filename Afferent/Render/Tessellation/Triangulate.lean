/-
  Afferent Tessellation Triangulation
-/
import Afferent.Core.Types
import Afferent.Render.Earcut

namespace Afferent

namespace Tessellation

private def pointsToData (points : Array Point) : Array Float := Id.run do
  let mut data : Array Float := Array.mkEmpty (points.size * 2)
  for p in points do
    data := data.push p.x
    data := data.push p.y
  return data

/-- Simple fan triangulation for convex polygons.
    Triangulates from first vertex to all other vertices. -/
def triangulateConvexFan (numVertices : Nat) : Array UInt32 := Id.run do
  if numVertices < 3 then return #[]
  let numTriangles := numVertices - 2
  let mut indices : Array UInt32 := Array.mkEmpty (numTriangles * 3)
  for i in [1:numVertices - 1] do
    indices := indices.push 0
    indices := indices.push i.toUInt32
    indices := indices.push (i + 1).toUInt32
  return indices

/-- Triangulate a polygon using earcut (handles concave shapes). -/
def triangulatePolygon (vertices : Array Point) : Array UInt32 := Id.run do
  if vertices.size < 3 then return #[]
  let data := pointsToData vertices
  return Earcut.earcut data #[]

end Tessellation

end Afferent

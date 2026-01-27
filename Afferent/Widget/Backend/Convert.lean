/-
  Afferent Widget Backend Conversions

  Note: Most conversion functions have been removed since Arbor now
  re-exports types from Afferent.Core (Point, Size, Rect, etc.
  are the same type in both namespaces).
-/
import Afferent.Core.Path
import Afferent.Arbor

namespace Afferent.Widget

open Afferent

/-- Convert a polygon (array of points) to an Afferent Path. -/
def polygonToPath (points : Array Point) : Path :=
  Id.run do
    if points.size > 0 then
      let first := points[0]!
      let mut path := Path.empty.moveTo first
      for i in [1:points.size] do
        let p := points[i]!
        path := path.lineTo p
      return path.closePath
    else
      return Path.empty

end Afferent.Widget

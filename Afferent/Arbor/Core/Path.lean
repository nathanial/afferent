/-
  Arbor Path
  Re-exports path types from Afferent.Core for use in the widget system.
-/
import Afferent.Core.Path

namespace Afferent.Arbor

-- Re-export path types from Afferent
export Afferent (PathCommand FillRule Path)

-- Re-export Path namespace members so Path.empty, Path.pie, etc. work
namespace Path
  export Afferent.Path (
    bezierCircleK empty isEmpty hash
    moveTo lineTo quadraticCurveTo bezierCurveTo arcTo arc rect closePath withFillRule
    rectangle rectangleXYWH circle ellipse roundedRect
    arcToBeziers pie arcPath semicircle
    quadraticCurve cubicCurve heart star polygon triangle equilateralTriangle hexagon octagon
  )

  -- Float constants for backwards compatibility
  -- (old Arbor.Path defined these locally)
  def pi : Float := 3.14159265358979323846
  def twoPi : Float := 6.28318530717958647692
  def halfPi : Float := 1.57079632679489661923
end Path

end Afferent.Arbor

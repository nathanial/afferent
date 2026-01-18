/-
  Orbit Spinner - Dots orbiting center at different speeds and radii
-/
import Afferent.Canopy.Core
import Afferent.Canopy.Widget.Display.Spinner.Core

namespace Afferent.Canopy.Spinner

open Afferent.Arbor hiding Event
open Linalg

/-- Orbit: Dots orbiting center at different speeds and radii. -/
def orbitSpec (t : Float) (color : Color) (dims : Dimensions) : CustomSpec := {
  measure := fun _ _ => (dims.size, dims.size)
  collect := fun layout =>
    let rect := layout.contentRect
    let cx := rect.x + dims.size / 2
    let cy := rect.y + dims.size / 2

    -- Define orbits: (radius factor, speed multiplier, size factor, alpha)
    let orbits : Array (Float × Float × Float × Float) := #[
      (0.35, 1.0, 0.08, 1.0),
      (0.28, 1.7, 0.06, 0.8),
      (0.20, 2.5, 0.05, 0.6),
      (0.12, 4.0, 0.04, 0.4)
    ]

    RenderM.build do
      for (radiusFactor, speedMult, sizeFactor, alpha) in orbits do
        let angle := t * Float.twoPi * speedMult
        let radius := dims.size * radiusFactor
        let dx := cx + radius * Float.cos angle
        let dy := cy + radius * Float.sin angle
        let dotRadius := dims.size * sizeFactor
        RenderM.fillCircle (Arbor.Point.mk' dx dy) dotRadius (color.withAlpha alpha)
  draw := none
}

end Afferent.Canopy.Spinner

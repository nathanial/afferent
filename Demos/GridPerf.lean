/-
  Grid Performance Test - Spinning squares in a grid
-/
import Afferent

open Afferent

namespace Demos

/-- Render grid spinning squares using unified Dynamic module.
    Static grid positions, GPU does color + NDC conversion. -/
def renderGridTest (c : Canvas) (t : Float) (font : Font) (particles : Render.Dynamic.ParticleState)
    (halfSize : Float) : IO Canvas := do
  let c := c.setFillColor Color.white
  let c ← c.fillTextXY s!"Grid: {particles.count} dynamic squares (Space to advance)" 20 30 font
  Render.Dynamic.drawRectsAnimated c.ctx.renderer particles halfSize t 3.0
  pure c

end Demos

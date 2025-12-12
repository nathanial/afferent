/-
  Circles Performance Test - Bouncing circles
-/
import Afferent

open Afferent

namespace Demos

/-- Render bouncing circles using unified Dynamic module.
    CPU updates positions (physics), GPU does color + NDC conversion. -/
def renderCircleTest (c : Canvas) (t : Float) (font : Font) (particles : Render.Dynamic.ParticleState)
    (radius : Float) : IO Canvas := do
  let c := c.setFillColor Color.white
  let c ← c.fillTextXY s!"Circles: {particles.count} dynamic circles (Space to advance)" 20 30 font
  Render.Dynamic.drawCircles c.ctx.renderer particles radius t
  pure c

end Demos

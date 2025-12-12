/-
  Triangles Performance Test - Spinning triangles in a grid
-/
import Afferent

open Afferent

namespace Demos

/-- Render grid of spinning triangles using unified Dynamic module. -/
def renderTriangleTest (c : Canvas) (t : Float) (font : Font) (particles : Render.Dynamic.ParticleState)
    (halfSize : Float) : IO Canvas := do
  let c := c.setFillColor Color.white
  let c ← c.fillTextXY s!"Triangles: {particles.count} dynamic triangles (Space to advance)" 20 30 font
  Render.Dynamic.drawTrianglesAnimated c.ctx.renderer particles halfSize t 2.0
  pure c

end Demos

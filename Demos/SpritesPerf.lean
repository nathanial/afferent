/-
  Sprites Performance Test - Bunnymark-style textured sprites
-/
import Afferent

open Afferent

namespace Demos

/-- Render textured sprites using FloatBuffer (high-performance Bunnymark).
    Lean physics, FloatBuffer for zero-copy GPU rendering. -/
def renderSpriteTestFast (c : Canvas) (font : Font) (particles : Render.Dynamic.ParticleState)
    (spriteBuffer : FFI.FloatBuffer) (texture : FFI.Texture) (halfSize : Float) : IO Canvas := do
  let c := c.setFillColor Color.white
  let c ← c.fillTextXY s!"Sprites: {particles.count} textured sprites [FloatBuffer] (Space to advance)" 20 30 font
  -- Write particle positions to FloatBuffer (1 FFI call per sprite)
  Render.Dynamic.writeSpritesToBuffer particles spriteBuffer halfSize
  -- Render from FloatBuffer (zero-copy to GPU)
  Render.Dynamic.drawSpritesFromBuffer c.ctx.renderer texture spriteBuffer particles.count.toUInt32 halfSize particles.screenWidth particles.screenHeight
  pure c

end Demos

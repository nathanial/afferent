/-
  Afferent FFI FloatBuffer
  High-performance mutable float array for instance data.
  Lives in C memory to avoid Lean's copy-on-write array semantics.
-/
import Afferent.FFI.Types
import Init.Data.FloatArray

namespace Afferent.FFI

-- FloatBuffer management
@[extern "lean_afferent_float_buffer_create"]
opaque FloatBuffer.create (capacity : USize) : IO FloatBuffer

@[extern "lean_afferent_float_buffer_destroy"]
opaque FloatBuffer.destroy (buf : @& FloatBuffer) : IO Unit

@[extern "lean_afferent_float_buffer_set"]
opaque FloatBuffer.set (buf : @& FloatBuffer) (index : USize) (value : Float) : IO Unit

@[extern "lean_afferent_float_buffer_get"]
opaque FloatBuffer.get (buf : @& FloatBuffer) (index : USize) : IO Float

-- Set 8 consecutive floats at once (8x less FFI overhead for instance data)
@[extern "lean_afferent_float_buffer_set_vec8"]
opaque FloatBuffer.setVec8 (buf : @& FloatBuffer) (index : USize)
  (v0 v1 v2 v3 v4 v5 v6 v7 : Float) : IO Unit

-- Set 5 consecutive floats at once (for sprite data: x, y, rotation, halfSize, alpha)
@[extern "lean_afferent_float_buffer_set_vec5"]
opaque FloatBuffer.setVec5 (buf : @& FloatBuffer) (index : USize)
  (v0 v1 v2 v3 v4 : Float) : IO Unit

-- Bulk-write sprite instance data from a ParticleState data array.
-- particleData layout: [x, y, vx, vy, hue] per particle (5 floats).
-- Writes SpriteInstanceData layout into FloatBuffer: [x, y, rotation, halfSize, alpha].
@[extern "lean_afferent_float_buffer_write_sprites_from_particles"]
opaque FloatBuffer.writeSpritesFromParticles
  (buffer : @& FloatBuffer)
  (particleData : @& FloatArray)
  (count : UInt32)
  (halfSize : Float)
  (rotation : Float)
  (alpha : Float) : IO Unit

namespace Particles

-- Update bouncing physics and write sprite instance data in the same pass.
-- particleData layout: [x, y, vx, vy, hue] per particle (5 floats).
-- spriteBuffer layout: [x, y, rotation(=0), halfSize, alpha(=1)] per particle (5 floats).
@[extern "lean_afferent_particles_update_bouncing_and_write_sprites"]
opaque updateBouncingAndWriteSprites
  (particleData : FloatArray)
  (count : UInt32)
  (dt : Float)
  (halfSize : Float)
  (screenWidth : Float)
  (screenHeight : Float)
  (spriteBuffer : @& FloatBuffer) : IO FloatArray

-- Update bouncing physics and write dynamic-circle instance data in the same pass.
-- circleBuffer layout: [x, y, hueBase, radius] per particle (4 floats).
@[extern "lean_afferent_particles_update_bouncing_and_write_circles"]
opaque updateBouncingAndWriteCircles
  (particleData : FloatArray)
  (count : UInt32)
  (dt : Float)
  (radius : Float)
  (screenWidth : Float)
  (screenHeight : Float)
  (circleBuffer : @& FloatBuffer) : IO FloatArray

end Particles

end Afferent.FFI

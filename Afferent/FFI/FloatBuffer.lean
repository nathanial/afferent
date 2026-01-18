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

@[extern "lean_afferent_float_buffer_set_count"]
opaque FloatBuffer.setCount (buf : @& FloatBuffer) (count : USize) : IO Unit

-- Set 8 consecutive floats at once (8x less FFI overhead for instance data)
@[extern "lean_afferent_float_buffer_set_vec8"]
opaque FloatBuffer.setVec8 (buf : @& FloatBuffer) (index : USize)
  (v0 v1 v2 v3 v4 v5 v6 v7 : Float) : IO Unit

-- Set 9 consecutive floats at once (for 9-float instance data)
@[extern "lean_afferent_float_buffer_set_vec9"]
opaque FloatBuffer.setVec9 (buf : @& FloatBuffer) (index : USize)
  (v0 v1 v2 v3 v4 v5 v6 v7 v8 : Float) : IO Unit

-- Set 5 consecutive floats at once (for sprite data: x, y, rotation, halfSize, alpha)
@[extern "lean_afferent_float_buffer_set_vec5"]
opaque FloatBuffer.setVec5 (buf : @& FloatBuffer) (index : USize)
  (v0 v1 v2 v3 v4 : Float) : IO Unit

-- Bulk-write packed params into a padded layout in FloatBuffer.
@[extern "lean_afferent_float_buffer_write_padded"]
opaque FloatBuffer.writePadded
  (buffer : @& FloatBuffer)
  (params : @& Array Float)
  (packedCount : UInt32)
  (paddedCount : UInt32)
  (offsets : @& Array Nat) : IO Unit

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

-- Bulk-write instanced shape data from particles into a FloatBuffer.
-- particleData layout: [x, y, vx, vy, hue] per particle (5 floats).
-- Writes InstanceData layout: [x, y, rotation, halfSize, hue, 0, 0, 1] (8 floats).
-- rotationMode: 0 = uniform rotation, 1 = animated (time * spinSpeed + hue * 2π).
@[extern "lean_afferent_float_buffer_write_instanced_from_particles"]
opaque FloatBuffer.writeInstancedFromParticles
  (buffer : @& FloatBuffer)
  (particleData : @& FloatArray)
  (count : UInt32)
  (halfSize : Float)
  (rotation : Float)
  (time : Float)
  (spinSpeed : Float)
  (rotationMode : UInt32) : IO Unit

end Afferent.FFI

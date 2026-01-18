/-
  Afferent FFI MeshCache
  Cached mesh for instanced polygon rendering. Tessellate a polygon once,
  store in GPU memory, draw all instances in a single draw call.
-/

import Afferent.FFI.Types

namespace Afferent.FFI

/-- Create a cached mesh from tessellated polygon data.
    - vertices: Flat array of [x, y, x, y, ...] positions
    - indices: Triangle indices
    - centerX, centerY: Mesh centroid (rotation pivot) -/
@[extern "lean_afferent_mesh_cache_create"]
opaque MeshCache.create (renderer : @& Renderer) (vertices : @& Array Float)
  (indices : @& Array UInt32) (centerX centerY : Float) : IO CachedMesh

/-- Destroy a cached mesh and free GPU resources. -/
@[extern "lean_afferent_mesh_cache_destroy"]
opaque MeshCache.destroy (mesh : @& CachedMesh) : IO Unit

/-- Draw all instances of a cached mesh in a single draw call.
    instance_data: 8 floats per instance [x, y, rotation, scale, r, g, b, a] -/
@[extern "lean_afferent_mesh_draw_instanced_buffer"]
opaque MeshCache.drawInstancedBuffer (renderer : @& Renderer) (mesh : @& CachedMesh)
  (instances : @& FloatBuffer) (instanceCount : UInt32)
  (canvasWidth canvasHeight : Float) : IO Unit

end Afferent.FFI

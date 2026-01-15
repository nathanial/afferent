/-
  Afferent 3D Rendering FFI Bindings
  Provides 3D mesh rendering with perspective projection and lighting.
-/
import Afferent.FFI.Types

namespace Afferent.FFI

/-- Draw a 3D mesh with perspective projection, lighting, and optional fog.
    vertices: Array of floats, 10 per vertex (position[3], normal[3], color[4])
    indices: Triangle indices (UInt32)
    mvpMatrix: 4x4 Model-View-Projection matrix (16 floats, column-major)
    modelMatrix: 4x4 Model matrix for normal transformation (16 floats)
    lightDir: Normalized light direction (3 floats)
    ambient: Ambient light factor (0.0-1.0)
    cameraPos: Camera position for fog distance calculation (3 floats)
    fogColor: Fog color RGB (3 floats)
    fogStart: Distance where fog begins
    fogEnd: Distance where fog is fully opaque (0 to disable) -/
@[extern "lean_afferent_renderer_draw_mesh_3d"]
opaque Renderer.drawMesh3D
  (renderer : @& Renderer)
  (vertices : @& Array Float)
  (indices : @& Array UInt32)
  (mvpMatrix : @& Array Float)
  (modelMatrix : @& Array Float)
  (lightDir : @& Array Float)
  (ambient : Float)
  (cameraPos : @& Array Float)
  (fogColor : @& Array Float)
  (fogStart fogEnd : Float) : IO Unit

/-- Draw an infinite-feeling ocean using a projected grid + Gerstner waves on the GPU.
    This avoids per-frame large vertex array marshaling from Lean.
    `waveParams` layout (Float array, length ≥ 32):
    - first 16 floats: 4x `waveA` = (dirX, dirZ, k, omegaSpeed)
    - next 16 floats: 4x `waveB` = (amplitude, ak, 0, 0) -/
@[extern "lean_afferent_renderer_draw_ocean_projected_grid_with_fog"]
opaque Renderer.drawOceanProjectedGridWithFog
  (renderer : @& Renderer)
  (gridSize : UInt32)
  (mvpMatrix : @& Array Float)
  (modelMatrix : @& Array Float)
  (lightDir : @& Array Float)
  (ambient : Float)
  (cameraPos : @& Array Float)
  (fogColor : @& Array Float)
  (fogStart fogEnd : Float)
  (time : Float)
  (fovY aspect : Float)
  (maxDistance snapSize overscanNdc horizonMargin : Float)
  (yaw pitch : Float)
  (waveParams : @& Array Float) : IO Unit

/-- Draw a textured 3D mesh with perspective projection, lighting, and fog.
    vertices: Array of floats, 12 per vertex (position[3], normal[3], uv[2], color[4])
    indices: Triangle indices (UInt32)
    indexOffset: Starting index in the index buffer
    indexCount: Number of indices to draw
    mvpMatrix: 4x4 Model-View-Projection matrix (16 floats, column-major)
    modelMatrix: 4x4 Model matrix for normal transformation (16 floats)
    lightDir: Normalized light direction (3 floats)
    ambient: Ambient light factor (0.0-1.0)
    cameraPos: Camera position for fog distance calculation (3 floats)
    fogColor: Fog color RGB (3 floats)
    fogStart: Distance where fog begins
    fogEnd: Distance where fog is fully opaque
    texture: Diffuse texture to sample -/
@[extern "lean_afferent_renderer_draw_mesh_3d_textured"]
opaque Renderer.drawMesh3DTextured
  (renderer : @& Renderer)
  (vertices : @& Array Float)
  (indices : @& Array UInt32)
  (indexOffset indexCount : UInt32)
  (mvpMatrix : @& Array Float)
  (modelMatrix : @& Array Float)
  (lightDir : @& Array Float)
  (ambient : Float)
  (cameraPos : @& Array Float)
  (fogColor : @& Array Float)
  (fogStart fogEnd : Float)
  (texture : @& Texture) : IO Unit

end Afferent.FFI

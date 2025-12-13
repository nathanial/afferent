/-
  Afferent 3D Rendering FFI Bindings
  Provides 3D mesh rendering with perspective projection and lighting.
-/
import Afferent.FFI.Types

namespace Afferent.FFI

/-- Draw a 3D mesh with perspective projection and basic lighting.
    vertices: Array of floats, 10 per vertex (position[3], normal[3], color[4])
    indices: Triangle indices (UInt32)
    mvpMatrix: 4x4 Model-View-Projection matrix (16 floats, column-major)
    modelMatrix: 4x4 Model matrix for normal transformation (16 floats)
    lightDir: Normalized light direction (3 floats)
    ambient: Ambient light factor (0.0-1.0) -/
@[extern "lean_afferent_renderer_draw_mesh_3d"]
opaque Renderer.drawMesh3D
  (renderer : @& Renderer)
  (vertices : @& Array Float)
  (indices : @& Array UInt32)
  (mvpMatrix : @& Array Float)
  (modelMatrix : @& Array Float)
  (lightDir : @& Array Float)
  (ambient : Float) : IO Unit

end Afferent.FFI

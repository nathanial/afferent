/-
  Afferent FFI Asset Loading
  Loads 3D models via Assimp with vertices, indices, and texture paths.
  Uses an "ungranular" API - single loadAsset call returns all data ready for rendering.
-/
import Afferent.FFI.Types

namespace Afferent.FFI

/-- A single sub-mesh within a loaded asset.
    Multi-material models are split into sub-meshes, each with its own texture. -/
structure SubMesh where
  indexOffset : UInt32    -- Offset into combined index buffer
  indexCount : UInt32     -- Number of indices for this submesh
  textureIndex : UInt32   -- Index into texturePaths array (`UInt32.size - 1` if no texture)
  deriving Repr, Inhabited

/-- A loaded 3D asset containing geometry and material references.
    Vertices use a 12-float layout: position(3) + normal(3) + uv(2) + color(4) -/
structure LoadedAsset where
  -- Vertex data: 12 floats per vertex (position[3], normal[3], uv[2], color[4])
  vertices : Array Float
  -- Triangle indices
  indices : Array UInt32
  -- Sub-meshes for multi-material models
  subMeshes : Array SubMesh
  -- Relative paths to diffuse textures
  texturePaths : Array String
  deriving Repr, Inhabited

/-- Load a 3D asset file (FBX, OBJ supported).
    Returns mesh data ready for rendering with textured 3D pipeline.
    - filePath: Path to the 3D model file
    - basePath: Directory containing textures (for resolving relative paths)

    Vertex format: 12 floats per vertex
    - position: x, y, z (3 floats)
    - normal: nx, ny, nz (3 floats)
    - uv: u, v (2 floats)
    - color: r, g, b, a (4 floats, defaults to white if no vertex colors)

    PBR Extension Notes:
    Currently only loads diffuse textures. To add full PBR support:
    1. Extend SubMesh to include normalMapIndex, metallicIndex, roughnessIndex
    2. Extract from assimp: aiTextureType_NORMALS, aiTextureType_METALNESS,
       aiTextureType_DIFFUSE_ROUGHNESS
    3. Update shader to sample all PBR textures -/
@[extern "lean_afferent_asset_load"]
opaque loadAsset (filePath : @& String) (basePath : @& String) : IO LoadedAsset

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

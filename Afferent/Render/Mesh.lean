/-
  Afferent Mesh Data
  Pre-defined mesh data for common 3D shapes.
-/

namespace Afferent.Render.Mesh

/-- Cube mesh vertices: 24 vertices (4 per face for distinct normals)
    Each vertex: x, y, z, nx, ny, nz, r, g, b, a (10 floats)
    Different colors per face for visual clarity. -/
def cubeVertices : Array Float := #[
  -- Front face (z = +0.5, normal = 0,0,1) - Red
  -0.5, -0.5,  0.5,   0, 0, 1,   0.9, 0.2, 0.2, 1,
   0.5, -0.5,  0.5,   0, 0, 1,   0.9, 0.2, 0.2, 1,
   0.5,  0.5,  0.5,   0, 0, 1,   0.9, 0.2, 0.2, 1,
  -0.5,  0.5,  0.5,   0, 0, 1,   0.9, 0.2, 0.2, 1,

  -- Back face (z = -0.5, normal = 0,0,-1) - Green
   0.5, -0.5, -0.5,   0, 0, -1,  0.2, 0.8, 0.2, 1,
  -0.5, -0.5, -0.5,   0, 0, -1,  0.2, 0.8, 0.2, 1,
  -0.5,  0.5, -0.5,   0, 0, -1,  0.2, 0.8, 0.2, 1,
   0.5,  0.5, -0.5,   0, 0, -1,  0.2, 0.8, 0.2, 1,

  -- Right face (x = +0.5, normal = 1,0,0) - Blue
   0.5, -0.5,  0.5,   1, 0, 0,   0.2, 0.4, 0.9, 1,
   0.5, -0.5, -0.5,   1, 0, 0,   0.2, 0.4, 0.9, 1,
   0.5,  0.5, -0.5,   1, 0, 0,   0.2, 0.4, 0.9, 1,
   0.5,  0.5,  0.5,   1, 0, 0,   0.2, 0.4, 0.9, 1,

  -- Left face (x = -0.5, normal = -1,0,0) - Yellow
  -0.5, -0.5, -0.5,  -1, 0, 0,   0.9, 0.9, 0.2, 1,
  -0.5, -0.5,  0.5,  -1, 0, 0,   0.9, 0.9, 0.2, 1,
  -0.5,  0.5,  0.5,  -1, 0, 0,   0.9, 0.9, 0.2, 1,
  -0.5,  0.5, -0.5,  -1, 0, 0,   0.9, 0.9, 0.2, 1,

  -- Top face (y = +0.5, normal = 0,1,0) - Cyan
  -0.5,  0.5,  0.5,   0, 1, 0,   0.2, 0.9, 0.9, 1,
   0.5,  0.5,  0.5,   0, 1, 0,   0.2, 0.9, 0.9, 1,
   0.5,  0.5, -0.5,   0, 1, 0,   0.2, 0.9, 0.9, 1,
  -0.5,  0.5, -0.5,   0, 1, 0,   0.2, 0.9, 0.9, 1,

  -- Bottom face (y = -0.5, normal = 0,-1,0) - Magenta
  -0.5, -0.5, -0.5,   0, -1, 0,  0.9, 0.2, 0.9, 1,
   0.5, -0.5, -0.5,   0, -1, 0,  0.9, 0.2, 0.9, 1,
   0.5, -0.5,  0.5,   0, -1, 0,  0.9, 0.2, 0.9, 1,
  -0.5, -0.5,  0.5,   0, -1, 0,  0.9, 0.2, 0.9, 1
]

/-- Cube mesh indices: 36 indices (6 faces x 2 triangles x 3 vertices) -/
def cubeIndices : Array UInt32 := #[
  -- Front face
  0, 1, 2,  0, 2, 3,
  -- Back face
  4, 5, 6,  4, 6, 7,
  -- Right face
  8, 9, 10,  8, 10, 11,
  -- Left face
  12, 13, 14,  12, 14, 15,
  -- Top face
  16, 17, 18,  16, 18, 19,
  -- Bottom face
  20, 21, 22,  20, 22, 23
]

end Afferent.Render.Mesh

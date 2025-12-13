/-
  Afferent 4x4 Matrix Operations
  Column-major 4x4 matrices for 3D transformations.
-/

namespace Afferent.Render

/-- 4x4 matrix stored in column-major order (like OpenGL/Metal). -/
structure Matrix4 where
  data : Array Float
  deriving Inhabited

namespace Matrix4

/-- Identity matrix. -/
def identity : Matrix4 := { data := #[
  1, 0, 0, 0,
  0, 1, 0, 0,
  0, 0, 1, 0,
  0, 0, 0, 1
]}

/-- Create perspective projection matrix.
    fovY: vertical field of view in radians
    aspect: width/height ratio
    near: near clipping plane
    far: far clipping plane -/
def perspective (fovY aspect near far : Float) : Matrix4 :=
  let tanHalfFov := Float.tan (fovY / 2.0)
  let f := 1.0 / tanHalfFov
  let nf := 1.0 / (near - far)
  { data := #[
    f / aspect, 0, 0, 0,
    0, f, 0, 0,
    0, 0, (far + near) * nf, -1,
    0, 0, 2.0 * far * near * nf, 0
  ]}

/-- Create look-at view matrix.
    eye: camera position
    center: point to look at
    up: up direction -/
def lookAt (eye center up : Float × Float × Float) : Matrix4 :=
  let (ex, ey, ez) := eye
  let (cx, cy, cz) := center
  let (ux, uy, uz) := up

  -- Forward direction (from camera to target)
  let fx := cx - ex
  let fy := cy - ey
  let fz := cz - ez
  let fLen := Float.sqrt (fx*fx + fy*fy + fz*fz)
  let fx := fx / fLen
  let fy := fy / fLen
  let fz := fz / fLen

  -- Right direction (cross product of forward and up)
  let rx := fy * uz - fz * uy
  let ry := fz * ux - fx * uz
  let rz := fx * uy - fy * ux
  let rLen := Float.sqrt (rx*rx + ry*ry + rz*rz)
  let rx := rx / rLen
  let ry := ry / rLen
  let rz := rz / rLen

  -- True up (cross product of right and forward)
  let ux := ry * fz - rz * fy
  let uy := rz * fx - rx * fz
  let uz := rx * fy - ry * fx

  -- Translation
  let tx := -(rx * ex + ry * ey + rz * ez)
  let ty := -(ux * ex + uy * ey + uz * ez)
  let tz := fx * ex + fy * ey + fz * ez

  { data := #[
    rx, ux, -fx, 0,
    ry, uy, -fy, 0,
    rz, uz, -fz, 0,
    tx, ty, tz, 1
  ]}

/-- Create translation matrix. -/
def translate (x y z : Float) : Matrix4 := { data := #[
  1, 0, 0, 0,
  0, 1, 0, 0,
  0, 0, 1, 0,
  x, y, z, 1
]}

/-- Create rotation matrix around X axis (angle in radians). -/
def rotateX (angle : Float) : Matrix4 :=
  let c := Float.cos angle
  let s := Float.sin angle
  { data := #[
    1, 0, 0, 0,
    0, c, s, 0,
    0, -s, c, 0,
    0, 0, 0, 1
  ]}

/-- Create rotation matrix around Y axis (angle in radians). -/
def rotateY (angle : Float) : Matrix4 :=
  let c := Float.cos angle
  let s := Float.sin angle
  { data := #[
    c, 0, -s, 0,
    0, 1, 0, 0,
    s, 0, c, 0,
    0, 0, 0, 1
  ]}

/-- Create rotation matrix around Z axis (angle in radians). -/
def rotateZ (angle : Float) : Matrix4 :=
  let c := Float.cos angle
  let s := Float.sin angle
  { data := #[
    c, s, 0, 0,
    -s, c, 0, 0,
    0, 0, 1, 0,
    0, 0, 0, 1
  ]}

/-- Create scale matrix. -/
def scale (x y z : Float) : Matrix4 := { data := #[
  x, 0, 0, 0,
  0, y, 0, 0,
  0, 0, z, 0,
  0, 0, 0, 1
]}

/-- Multiply two matrices (a * b). -/
def multiply (a b : Matrix4) : Matrix4 :=
  let get (m : Matrix4) (row col : Nat) : Float :=
    m.data.getD (col * 4 + row) 0.0
  let result := Id.run do
    let mut result := Array.replicate 16 0.0
    for row in [:4] do
      for col in [:4] do
        let mut sum := 0.0
        for k in [:4] do
          sum := sum + get a row k * get b k col
        result := result.set! (col * 4 + row) sum
    return result
  { data := result }

/-- Get matrix data as Array Float for FFI. -/
def toArray (m : Matrix4) : Array Float := m.data

end Matrix4

end Afferent.Render

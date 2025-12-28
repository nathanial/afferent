/-
  Afferent Transform
  2D affine transformation matrix.
-/
import Afferent.Core.Types
import Linalg.Affine2D

namespace Afferent

/-- 2D affine transform as a 3x2 matrix (column-major for Metal compatibility).
    | a  c  tx |
    | b  d  ty |
    The bottom row is implicitly [0, 0, 1].
-/
structure Transform where
  a : Float   -- scale X / rotation component
  b : Float   -- skew Y / rotation component
  c : Float   -- skew X / rotation component
  d : Float   -- scale Y / rotation component
  tx : Float  -- translate X
  ty : Float  -- translate Y
deriving Repr, BEq, Inhabited

namespace Transform

/-- Identity transform (no transformation). -/
def identity : Transform :=
  { a := 1.0, b := 0.0, c := 0.0, d := 1.0, tx := 0.0, ty := 0.0 }

/-- Create a translation transform. -/
def translate (dx dy : Float) : Transform :=
  { identity with tx := dx, ty := dy }

/-- Create a scaling transform. -/
def scale (sx sy : Float) : Transform :=
  { identity with a := sx, d := sy }

/-- Create a uniform scaling transform. -/
def scaleUniform (s : Float) : Transform :=
  scale s s

/-- Create a rotation transform (angle in radians). -/
def rotate (angle : Float) : Transform :=
  let cos := Float.cos angle
  let sin := Float.sin angle
  { identity with a := cos, b := sin, c := -sin, d := cos }

/-- Create a skew transform along X axis (angle in radians). -/
def skewX (angle : Float) : Transform :=
  { identity with c := Float.tan angle }

/-- Create a skew transform along Y axis (angle in radians). -/
def skewY (angle : Float) : Transform :=
  { identity with b := Float.tan angle }

/-- Concatenate two transforms: t1 then t2 (t2 * t1 in matrix terms). -/
def concat (t1 t2 : Transform) : Transform :=
  { a := t2.a * t1.a + t2.c * t1.b
    b := t2.b * t1.a + t2.d * t1.b
    c := t2.a * t1.c + t2.c * t1.d
    d := t2.b * t1.c + t2.d * t1.d
    tx := t2.a * t1.tx + t2.c * t1.ty + t2.tx
    ty := t2.b * t1.tx + t2.d * t1.ty + t2.ty }

/-- Apply a transform to a point. -/
def apply (t : Transform) (p : Point) : Point :=
  { x := t.a * p.x + t.c * p.y + t.tx
    y := t.b * p.x + t.d * p.y + t.ty }

/-- Compute the determinant of the transform matrix. -/
def determinant (t : Transform) : Float :=
  t.a * t.d - t.b * t.c

/-- Check if the transform is invertible. -/
def isInvertible (t : Transform) : Bool :=
  t.determinant != 0.0

/-- Compute the inverse transform (returns identity if not invertible). -/
def inverse (t : Transform) : Transform :=
  let det := t.determinant
  if det == 0.0 then identity
  else
    let invDet := 1.0 / det
    { a := t.d * invDet
      b := -t.b * invDet
      c := -t.c * invDet
      d := t.a * invDet
      tx := (t.c * t.ty - t.d * t.tx) * invDet
      ty := (t.b * t.tx - t.a * t.ty) * invDet }

/-- Apply a translation to the current transform (Canvas API semantics).
    The translation is applied in local coordinates, before existing transform. -/
def translated (t : Transform) (dx dy : Float) : Transform :=
  concat (translate dx dy) t

/-- Apply a scale to the current transform (Canvas API semantics).
    The scale is applied in local coordinates, before existing transform. -/
def scaled (t : Transform) (sx sy : Float) : Transform :=
  concat (scale sx sy) t

/-- Apply a rotation to the current transform (Canvas API semantics).
    The rotation is applied in local coordinates, before existing transform. -/
def rotated (t : Transform) (angle : Float) : Transform :=
  concat (rotate angle) t

instance : Mul Transform := ⟨concat⟩

/-- Pack transform into a flat array for FFI (6 floats). -/
def toArray (t : Transform) : Array Float :=
  #[t.a, t.b, t.c, t.d, t.tx, t.ty]

/-- Create transform from flat array. -/
def fromArray (arr : Array Float) : Transform :=
  if arr.size >= 6 then
    { a := arr[0]!, b := arr[1]!, c := arr[2]!,
      d := arr[3]!, tx := arr[4]!, ty := arr[5]! }
  else identity

/-- Convert to Linalg Affine2D. Both use the same column-major layout. -/
def toAffine2D (t : Transform) : Linalg.Affine2D :=
  { data := #[t.a, t.b, t.c, t.d, t.tx, t.ty] }

/-- Create from Linalg Affine2D. Both use the same column-major layout. -/
def fromAffine2D (a : Linalg.Affine2D) : Transform :=
  { a := a.data[0]!, b := a.data[1]!, c := a.data[2]!,
    d := a.data[3]!, tx := a.data[4]!, ty := a.data[5]! }

end Transform

end Afferent

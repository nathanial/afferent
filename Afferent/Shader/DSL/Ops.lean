/-
  Afferent Shader DSL - Operators and Helpers
  Operator instances and convenience functions for ergonomic DSL usage.
-/
import Afferent.Shader.DSL.Expr

namespace Afferent.Shader.DSL

/-! ## Numeric Literal Instances -/

instance : OfNat (ShaderExpr .float) n where
  ofNat := .litFloat n.toFloat

instance : OfNat (ShaderExpr .uint) n where
  ofNat := .litUInt n.toUInt32

instance : OfScientific (ShaderExpr .float) where
  ofScientific mantissa exponentSign decimalExponent :=
    .litFloat (OfScientific.ofScientific mantissa exponentSign decimalExponent)

/-! ## Arithmetic Operator Instances -/

instance : Add (ShaderExpr t) where
  add := ShaderExpr.add

instance : Sub (ShaderExpr t) where
  sub := ShaderExpr.sub

instance : Mul (ShaderExpr t) where
  mul := ShaderExpr.mul

instance : Div (ShaderExpr t) where
  div := ShaderExpr.div

instance : Neg (ShaderExpr t) where
  neg := ShaderExpr.neg

/-! ## Vector Construction -/

/-- Create a float2 vector from two floats. -/
def vec2 (x y : ShaderExpr .float) : ShaderExpr .float2 := .vec2 x y

/-- Create a float3 vector from three floats. -/
def vec3 (x y z : ShaderExpr .float) : ShaderExpr .float3 := .vec3 x y z

/-- Create a float4 vector from four floats. -/
def vec4 (x y z w : ShaderExpr .float) : ShaderExpr .float4 := .vec4 x y z w

/-- Create a float3 from a single float (all components same). -/
def vec3from1 (x : ShaderExpr .float) : ShaderExpr .float3 := .vec3from1 x

/-- Create a float4 from a single float (all components same). -/
def vec4from1 (x : ShaderExpr .float) : ShaderExpr .float4 := .vec4from1 x

/-- Create a float4 from float3 and alpha. -/
def vec4from31 (rgb : ShaderExpr .float3) (a : ShaderExpr .float) : ShaderExpr .float4 :=
  .vec4from31 rgb a

/-! ## Component Access (Swizzle) -/

namespace ShaderExpr

/-- Access .x component. -/
def x (e : ShaderExpr t) : ShaderExpr .float := .swizzleX e

/-- Access .y component. -/
def y (e : ShaderExpr t) : ShaderExpr .float := .swizzleY e

/-- Access .z component. -/
def z (e : ShaderExpr t) : ShaderExpr .float := .swizzleZ e

/-- Access .w component (or .a for colors). -/
def w (e : ShaderExpr t) : ShaderExpr .float := .swizzleW e

/-- Alias for .x (for colors). -/
def r (e : ShaderExpr t) : ShaderExpr .float := .swizzleX e

/-- Alias for .y (for colors). -/
def g (e : ShaderExpr t) : ShaderExpr .float := .swizzleY e

/-- Alias for .z (for colors). -/
def b (e : ShaderExpr t) : ShaderExpr .float := .swizzleZ e

/-- Alias for .w (for colors). -/
def a (e : ShaderExpr t) : ShaderExpr .float := .swizzleW e

/-- Access .xy components. -/
def xy (e : ShaderExpr t) : ShaderExpr .float2 := .swizzleXY e

/-- Access .xyz components. -/
def xyz (e : ShaderExpr t) : ShaderExpr .float3 := .swizzleXYZ e

/-- Access .rgb components of a color. -/
def rgb (e : ShaderExpr .float4) : ShaderExpr .float3 := .swizzleRGB e

end ShaderExpr

/-! ## Math Functions (Float) -/

/-- Sine function. -/
def sin (e : ShaderExpr .float) : ShaderExpr .float := .sin e

/-- Cosine function. -/
def cos (e : ShaderExpr .float) : ShaderExpr .float := .cos e

/-- Tangent function. -/
def tan (e : ShaderExpr .float) : ShaderExpr .float := .tan e

/-- Arc sine function. -/
def asin (e : ShaderExpr .float) : ShaderExpr .float := .asin e

/-- Arc cosine function. -/
def acos (e : ShaderExpr .float) : ShaderExpr .float := .acos e

/-- Arc tangent function. -/
def atan (e : ShaderExpr .float) : ShaderExpr .float := .atan e

/-- Arc tangent of y/x. -/
def atan2 (y x : ShaderExpr .float) : ShaderExpr .float := .atan2 y x

/-- Square root. -/
def sqrt (e : ShaderExpr .float) : ShaderExpr .float := .sqrt e

/-- Power function. -/
def pow (base exp : ShaderExpr .float) : ShaderExpr .float := .pow base exp

/-- Exponential (e^x). -/
def exp (e : ShaderExpr .float) : ShaderExpr .float := .exp e

/-- Natural logarithm. -/
def log (e : ShaderExpr .float) : ShaderExpr .float := .log e

/-- Floor function. -/
def floor (e : ShaderExpr .float) : ShaderExpr .float := .floor e

/-- Ceiling function. -/
def ceil (e : ShaderExpr .float) : ShaderExpr .float := .ceil e

/-- Round to nearest integer. -/
def round (e : ShaderExpr .float) : ShaderExpr .float := .round e

/-- Fractional part. -/
def fract (e : ShaderExpr .float) : ShaderExpr .float := .fract e

/-- Absolute value (float). -/
def absF (e : ShaderExpr .float) : ShaderExpr .float := .absF e

/-- Absolute value (float). Alias for absF. -/
def abs (e : ShaderExpr .float) : ShaderExpr .float := .absF e

/-- Sign function. -/
def signF (e : ShaderExpr .float) : ShaderExpr .float := .signF e

/-- Minimum of two floats. -/
def minF (a b : ShaderExpr .float) : ShaderExpr .float := .minF a b

/-- Maximum of two floats. -/
def maxF (a b : ShaderExpr .float) : ShaderExpr .float := .maxF a b

/-- Clamp float to range. -/
def clampF (x lo hi : ShaderExpr .float) : ShaderExpr .float := .clampF x lo hi

/-- Linear interpolation (mix). -/
def mixF (a b t : ShaderExpr .float) : ShaderExpr .float := .mixF a b t

/-- Smoothstep interpolation. -/
def smoothstep (lo hi x : ShaderExpr .float) : ShaderExpr .float := .smoothstep lo hi x

/-- Step function (0 if x < edge, else 1). -/
def step (edge x : ShaderExpr .float) : ShaderExpr .float := .step edge x

/-! ## Vector Math Functions -/

/-- Absolute value (vector, componentwise). -/
def absV (e : ShaderExpr t) : ShaderExpr t := .absV e

/-- Clamp vector to range (componentwise). -/
def clampV (x lo hi : ShaderExpr t) : ShaderExpr t := .clampV x lo hi

/-- Linear interpolation for vectors. -/
def mixV (a b : ShaderExpr t) (alpha : ShaderExpr .float) : ShaderExpr t := .mixV a b alpha

/-- Minimum (vector, componentwise). -/
def minV (a b : ShaderExpr t) : ShaderExpr t := .minV a b

/-- Maximum (vector, componentwise). -/
def maxV (a b : ShaderExpr t) : ShaderExpr t := .maxV a b

/-- Fractional part (vector, componentwise). -/
def fractV (e : ShaderExpr t) : ShaderExpr t := .fractV e

/-- Length of a vector. -/
def length (e : ShaderExpr t) : ShaderExpr .float := .length e

/-- Normalize a vector. -/
def normalize (e : ShaderExpr t) : ShaderExpr t := .normalize e

/-- Dot product. -/
def dot (a b : ShaderExpr t) : ShaderExpr .float := .dot a b

/-- Cross product (float3 only). -/
def cross (a b : ShaderExpr .float3) : ShaderExpr .float3 := .cross a b

/-- Distance between two points. -/
def distance (a b : ShaderExpr t) : ShaderExpr .float := .distance a b

/-! ## Type Conversion -/

/-- Convert uint to float. -/
def toFloat (e : ShaderExpr .uint) : ShaderExpr .float := .toFloat e

/-- Convert float to uint. -/
def toUInt (e : ShaderExpr .float) : ShaderExpr .uint := .toUInt e

/-! ## Integer Operations -/

/-- Integer division. -/
def idiv (a b : ShaderExpr .uint) : ShaderExpr .uint := .idiv a b

/-- Integer modulo. -/
def imod (a b : ShaderExpr .uint) : ShaderExpr .uint := .imod a b

/-! ## Comparisons -/

/-- Less than (float). -/
def lt (a b : ShaderExpr .float) : ShaderExpr .bool := .lt a b

/-- Less than or equal (float). -/
def le (a b : ShaderExpr .float) : ShaderExpr .bool := .le a b

/-- Greater than (float). -/
def gt (a b : ShaderExpr .float) : ShaderExpr .bool := .gt a b

/-- Greater than or equal (float). -/
def ge (a b : ShaderExpr .float) : ShaderExpr .bool := .ge a b

/-- Equality. -/
def eq (a b : ShaderExpr t) : ShaderExpr .bool := .eq a b

/-- Inequality. -/
def ne (a b : ShaderExpr t) : ShaderExpr .bool := .ne a b

/-- Less than (uint). -/
def ltU (a b : ShaderExpr .uint) : ShaderExpr .uint → ShaderExpr .uint → ShaderExpr .bool :=
  fun _ _ => .ltU a b

/-- Equality (uint). -/
def eqU (a b : ShaderExpr .uint) : ShaderExpr .bool := .eqU a b

/-! ## Boolean Operations -/

/-- Boolean and. -/
def andB (a b : ShaderExpr .bool) : ShaderExpr .bool := .andB a b

/-- Boolean or. -/
def orB (a b : ShaderExpr .bool) : ShaderExpr .bool := .orB a b

/-- Boolean not. -/
def notB (a : ShaderExpr .bool) : ShaderExpr .bool := .notB a

/-! ## Conditional -/

/-- Ternary conditional (cond ? thenExpr : elseExpr). -/
def cond (c : ShaderExpr .bool) (t f : ShaderExpr ty) : ShaderExpr ty := .cond c t f

/-- Alias for cond with more descriptive name. -/
def ifThenElse (c : ShaderExpr .bool) (t f : ShaderExpr ty) : ShaderExpr ty := .cond c t f

/-! ## Let Binding -/

/-- Introduce a let binding for an intermediate value.
    Usage: letIn "x" myExpr (var "x" .float + 1) -/
def letIn (name : String) (ty : ShaderType) (val : ShaderExpr ty) (body : ShaderExpr t) :
    ShaderExpr t :=
  .letIn name ty val body

/-- Reference a variable introduced by letIn. -/
def var (name : String) (ty : ShaderType) : ShaderExpr ty := .var name ty

/-! ## Built-in Values -/

/-- The instance index (idx in Metal). -/
def idx : ShaderExpr .uint := .idx

/-- Access a parameter field (p.fieldName in Metal). -/
def param (field : String) (ty : ShaderType) : ShaderExpr ty := .param field ty

/-! ## Scalar Multiplication -/

/-- Scalar multiplication (scalar * vector). -/
def scale (s : ShaderExpr .float) (v : ShaderExpr t) : ShaderExpr t := .scale s v

end Afferent.Shader.DSL

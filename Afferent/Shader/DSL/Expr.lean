/-
  Afferent Shader DSL - Expressions
  Typed shader expression AST (GADT-style).
-/
import Afferent.Shader.DSL.Types

namespace Afferent.Shader.DSL

/-- Typed shader expression. The type parameter tracks the Metal type. -/
inductive ShaderExpr : ShaderType → Type where
  -- Literals
  | litFloat   : Float → ShaderExpr .float
  | litFloat2  : Float → Float → ShaderExpr .float2
  | litFloat3  : Float → Float → Float → ShaderExpr .float3
  | litFloat4  : Float → Float → Float → Float → ShaderExpr .float4
  | litUInt    : UInt32 → ShaderExpr .uint
  | litInt     : Int32 → ShaderExpr .int
  | litBool    : Bool → ShaderExpr .bool

  -- Instance index (uint idx parameter in fragment function)
  | idx        : ShaderExpr .uint

  -- Pixel coordinates (for QuadShader fragment functions)
  | pixelUV    : ShaderExpr .float2   -- Normalized 0-1 coords within quad
  | pixelPos   : ShaderExpr .float2   -- Screen-space pixel position

  -- Parameter field access (p.fieldName)
  | param      : (field : String) → (ty : ShaderType) → ShaderExpr ty

  -- Variable reference (for let bindings)
  | var        : (name : String) → (ty : ShaderType) → ShaderExpr ty

  -- Arithmetic (same-type operands)
  | add        : ShaderExpr t → ShaderExpr t → ShaderExpr t
  | sub        : ShaderExpr t → ShaderExpr t → ShaderExpr t
  | mul        : ShaderExpr t → ShaderExpr t → ShaderExpr t
  | div        : ShaderExpr t → ShaderExpr t → ShaderExpr t
  | neg        : ShaderExpr t → ShaderExpr t

  -- Scalar multiplication (scalar * vector)
  | scale      : ShaderExpr .float → ShaderExpr t → ShaderExpr t

  -- Integer operations
  | idiv       : ShaderExpr .uint → ShaderExpr .uint → ShaderExpr .uint
  | imod       : ShaderExpr .uint → ShaderExpr .uint → ShaderExpr .uint

  -- Type conversion
  | toFloat    : ShaderExpr .uint → ShaderExpr .float
  | toUInt     : ShaderExpr .float → ShaderExpr .uint

  -- Math functions (float → float)
  | sin        : ShaderExpr .float → ShaderExpr .float
  | cos        : ShaderExpr .float → ShaderExpr .float
  | tan        : ShaderExpr .float → ShaderExpr .float
  | asin       : ShaderExpr .float → ShaderExpr .float
  | acos       : ShaderExpr .float → ShaderExpr .float
  | atan       : ShaderExpr .float → ShaderExpr .float
  | atan2      : ShaderExpr .float → ShaderExpr .float → ShaderExpr .float
  | sqrt       : ShaderExpr .float → ShaderExpr .float
  | pow        : ShaderExpr .float → ShaderExpr .float → ShaderExpr .float
  | exp        : ShaderExpr .float → ShaderExpr .float
  | log        : ShaderExpr .float → ShaderExpr .float
  | floor      : ShaderExpr .float → ShaderExpr .float
  | ceil       : ShaderExpr .float → ShaderExpr .float
  | round      : ShaderExpr .float → ShaderExpr .float
  | fract      : ShaderExpr .float → ShaderExpr .float
  | absF       : ShaderExpr .float → ShaderExpr .float
  | signF      : ShaderExpr .float → ShaderExpr .float
  | minF       : ShaderExpr .float → ShaderExpr .float → ShaderExpr .float
  | maxF       : ShaderExpr .float → ShaderExpr .float → ShaderExpr .float
  | clampF     : ShaderExpr .float → ShaderExpr .float → ShaderExpr .float → ShaderExpr .float
  | mixF       : ShaderExpr .float → ShaderExpr .float → ShaderExpr .float → ShaderExpr .float
  | smoothstep : ShaderExpr .float → ShaderExpr .float → ShaderExpr .float → ShaderExpr .float
  | step       : ShaderExpr .float → ShaderExpr .float → ShaderExpr .float

  -- Vector-wise functions (operate componentwise)
  | absV       : ShaderExpr t → ShaderExpr t
  | clampV     : ShaderExpr t → ShaderExpr t → ShaderExpr t → ShaderExpr t
  | mixV       : ShaderExpr t → ShaderExpr t → ShaderExpr .float → ShaderExpr t
  | minV       : ShaderExpr t → ShaderExpr t → ShaderExpr t
  | maxV       : ShaderExpr t → ShaderExpr t → ShaderExpr t
  | fractV     : ShaderExpr t → ShaderExpr t

  -- Vector construction
  | vec2       : ShaderExpr .float → ShaderExpr .float → ShaderExpr .float2
  | vec3       : ShaderExpr .float → ShaderExpr .float → ShaderExpr .float → ShaderExpr .float3
  | vec4       : ShaderExpr .float → ShaderExpr .float → ShaderExpr .float → ShaderExpr .float → ShaderExpr .float4
  | vec3from1  : ShaderExpr .float → ShaderExpr .float3  -- float3(x) = float3(x, x, x)
  | vec4from1  : ShaderExpr .float → ShaderExpr .float4  -- float4(x) = float4(x, x, x, x)
  | vec4from31 : ShaderExpr .float3 → ShaderExpr .float → ShaderExpr .float4  -- float4(rgb, a)

  -- Vector geometry
  | length     : ShaderExpr t → ShaderExpr .float
  | normalize  : ShaderExpr t → ShaderExpr t
  | dot        : ShaderExpr t → ShaderExpr t → ShaderExpr .float
  | cross      : ShaderExpr .float3 → ShaderExpr .float3 → ShaderExpr .float3
  | distance   : ShaderExpr t → ShaderExpr t → ShaderExpr .float

  -- Swizzle components (extract single component)
  | swizzleX   : ShaderExpr t → ShaderExpr .float
  | swizzleY   : ShaderExpr t → ShaderExpr .float
  | swizzleZ   : ShaderExpr t → ShaderExpr .float
  | swizzleW   : ShaderExpr t → ShaderExpr .float

  -- Multi-component swizzle (common patterns)
  | swizzleXY  : ShaderExpr t → ShaderExpr .float2
  | swizzleXYZ : ShaderExpr t → ShaderExpr .float3
  | swizzleXXX : ShaderExpr t → ShaderExpr .float3
  | swizzleWWW : ShaderExpr t → ShaderExpr .float3
  | swizzleRGB : ShaderExpr .float4 → ShaderExpr .float3

  -- Comparisons (return bool)
  | lt         : ShaderExpr .float → ShaderExpr .float → ShaderExpr .bool
  | le         : ShaderExpr .float → ShaderExpr .float → ShaderExpr .bool
  | gt         : ShaderExpr .float → ShaderExpr .float → ShaderExpr .bool
  | ge         : ShaderExpr .float → ShaderExpr .float → ShaderExpr .bool
  | eq         : ShaderExpr t → ShaderExpr t → ShaderExpr .bool
  | ne         : ShaderExpr t → ShaderExpr t → ShaderExpr .bool

  -- Integer comparisons
  | ltU        : ShaderExpr .uint → ShaderExpr .uint → ShaderExpr .bool
  | eqU        : ShaderExpr .uint → ShaderExpr .uint → ShaderExpr .bool

  -- Boolean operations
  | andB       : ShaderExpr .bool → ShaderExpr .bool → ShaderExpr .bool
  | orB        : ShaderExpr .bool → ShaderExpr .bool → ShaderExpr .bool
  | notB       : ShaderExpr .bool → ShaderExpr .bool

  -- Conditional (ternary operator)
  | cond       : ShaderExpr .bool → ShaderExpr t → ShaderExpr t → ShaderExpr t

  -- Let binding (for intermediate values)
  | letIn      : (name : String) → (ty : ShaderType) → ShaderExpr ty →
                 ShaderExpr t → ShaderExpr t

namespace ShaderExpr

/-- Get the type of an expression. -/
def getType : ShaderExpr t → ShaderType := fun _ => t

end ShaderExpr

end Afferent.Shader.DSL

/-
  Afferent Shader DSL - Types
  Core type definitions for the shader DSL.
-/

namespace Afferent.Shader.DSL

/-- Shader value types (mirrors Metal types). -/
inductive ShaderType where
  | float
  | float2
  | float3
  | float4
  | uint
  | int
  | bool
deriving Repr, BEq, Inhabited

namespace ShaderType

/-- Convert to Metal type name. -/
def toMetal : ShaderType → String
  | .float  => "float"
  | .float2 => "float2"
  | .float3 => "float3"
  | .float4 => "float4"
  | .uint   => "uint"
  | .int    => "int"
  | .bool   => "bool"

/-- Number of floats this type uses when serialized. -/
def floatCount : ShaderType → Nat
  | .float  => 1
  | .float2 => 2
  | .float3 => 3
  | .float4 => 4
  | .uint   => 1
  | .int    => 1
  | .bool   => 1

end ShaderType

/-- A field in a parameter struct. -/
structure ParamField where
  name : String
  type : ShaderType
deriving Repr, BEq, Inhabited

/-- Parameter struct definition (list of fields). -/
def ParamStruct := List ParamField

namespace ParamStruct

/-- Render a parameter struct to Metal code. -/
def toMetal (structName : String) (params : ParamStruct) : String :=
  let fields := params.map fun f => s!"  {f.type.toMetal} {f.name};"
  s!"struct {structName} \{\n{String.intercalate "\n" fields}\n};"

/-- Count total floats in the parameter struct. -/
def floatCount (params : ParamStruct) : Nat :=
  params.foldl (fun acc f => acc + f.type.floatCount) 0

end ParamStruct

end Afferent.Shader.DSL

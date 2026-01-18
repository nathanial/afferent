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

/-- Alignment in floats (4-byte units) for Metal struct packing. -/
def alignFloats : ShaderType → Nat
  | .float  => 1
  | .float2 => 2
  | .float3 => 4
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

/-- Parameter packing layout for Metal structs. -/
structure ParamLayout where
  packedFloatCount : Nat
  paddedFloatCount : Nat
  packOffsets : Array Nat
deriving Repr, BEq

private def alignUp (n a : Nat) : Nat :=
  ((n + a - 1) / a) * a

private def pushOffsets (acc : Array Nat) (start count : Nat) : Array Nat :=
  Nat.rec (motive := fun _ => Array Nat) acc (fun n acc => acc.push (start + n)) count

/-- Compute packing offsets and padded size for Metal struct layout. -/
def layout (params : ParamStruct) : ParamLayout :=
  let init : Nat × Nat × Nat × Array Nat := (0, 0, 1, #[])
  let (offset, packed, maxAlign, offsets) :=
    params.foldl (init := init) fun (offset, packed, maxAlign, offsets) field =>
      let align := field.type.alignFloats
      let size := field.type.floatCount
      let maxAlign := if align > maxAlign then align else maxAlign
      let offset := alignUp offset align
      let offsets := pushOffsets offsets offset size
      (offset + size, packed + size, maxAlign, offsets)
  let padded := alignUp offset maxAlign
  { packedFloatCount := packed, paddedFloatCount := padded, packOffsets := offsets }

end ParamStruct

end Afferent.Shader.DSL

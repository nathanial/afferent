/-
  Afferent Shader DSL - Rectangle Fragment
  Defines rectangle-generating fragment shaders using the DSL.
-/
import Afferent.Shader.DSL.Render
import Afferent.Shader.Fragment

namespace Afferent.Shader.DSL

open Afferent.Shader

/-- The output of a rectangle fragment shader.
    Expresses the computed position, size, cornerRadius, and color for a rect instance. -/
structure RectResultExpr where
  position     : ShaderExpr .float2  -- Top-left corner
  size         : ShaderExpr .float2  -- Width, height
  cornerRadius : ShaderExpr .float := .litFloat 0.0  -- 0 = sharp corners
  color        : ShaderExpr .float4

namespace RectResultExpr

/-- Render the rect result assignment and return statement. -/
def toMetal (result : RectResultExpr) : String :=
  let lines := [
    "RectResult result;",
    s!"result.position = {result.position.toMetal};",
    s!"result.size = {result.size.toMetal};",
    s!"result.cornerRadius = {result.cornerRadius.toMetal};",
    s!"result.color = {result.color.toMetal};",
    "return result;"
  ]
  String.intercalate "\n" lines

end RectResultExpr

/-- A complete rectangle fragment shader definition.
    Contains the name, instance count, parameters, and body computation. -/
structure RectShader where
  /-- Unique name for this shader. -/
  name : String
  /-- Number of rectangles generated per draw call. -/
  instanceCount : Nat
  /-- Parameter struct fields (passed from Lean to GPU). -/
  params : ParamStruct
  /-- The shader body that computes the rect result. -/
  body : RectResultExpr

namespace RectShader

/-- Generate the Metal struct name for parameters. -/
def paramsTypeName (shader : RectShader) : String :=
  let name := shader.name
  let first := name.get 0
  let rest := name.drop 1
  s!"{Char.toUpper first}{rest}Params"

/-- Compile a RectShader to a ShaderFragment. -/
def compile (shader : RectShader) : ShaderFragment :=
  let paramsStruct := shader.params.toMetal shader.paramsTypeName
  let bodyCode := shader.body.toMetal
  let layout := shader.params.layout
  fragmentRectPacked shader.name shader.instanceCount layout.paddedFloatCount
    layout.packedFloatCount layout.packOffsets paramsStruct bodyCode

end RectShader

end Afferent.Shader.DSL

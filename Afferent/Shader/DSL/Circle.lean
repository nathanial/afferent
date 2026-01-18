/-
  Afferent Shader DSL - Circle Fragment
  Defines circle-generating fragment shaders using the DSL.
-/
import Afferent.Shader.DSL.Render
import Afferent.Shader.Fragment

namespace Afferent.Shader.DSL

open Afferent.Shader

/-- The output of a circle fragment shader.
    Expresses the computed center, radius, strokeWidth, and color for a circle instance.
    strokeWidth = 0 renders a filled circle, > 0 renders a ring/stroke. -/
structure CircleResultExpr where
  center      : ShaderExpr .float2
  radius      : ShaderExpr .float
  strokeWidth : ShaderExpr .float := .litFloat 0.0  -- Default to filled circle
  color       : ShaderExpr .float4

namespace CircleResultExpr

/-- Render the circle result assignment and return statement. -/
def toMetal (result : CircleResultExpr) : String :=
  let lines := [
    "CircleResult result;",
    s!"result.center = {result.center.toMetal};",
    s!"result.radius = {result.radius.toMetal};",
    s!"result.strokeWidth = {result.strokeWidth.toMetal};",
    s!"result.color = {result.color.toMetal};",
    "return result;"
  ]
  String.intercalate "\n" lines

end CircleResultExpr

/-- A complete circle fragment shader definition.
    Contains the name, instance count, parameters, and body computation. -/
structure CircleShader where
  /-- Unique name for this shader. -/
  name : String
  /-- Number of circles generated per draw call. -/
  instanceCount : Nat
  /-- Parameter struct fields (passed from Lean to GPU). -/
  params : ParamStruct
  /-- The shader body that computes the circle result. -/
  body : CircleResultExpr

namespace CircleShader

/-- Generate the Metal struct name for parameters. -/
def paramsTypeName (shader : CircleShader) : String :=
  let name := shader.name
  let first := name.get 0
  let rest := name.drop 1
  s!"{Char.toUpper first}{rest}Params"

/-- Compile a CircleShader to a ShaderFragment. -/
def compile (shader : CircleShader) : ShaderFragment :=
  let paramsStruct := shader.params.toMetal shader.paramsTypeName
  let bodyCode := shader.body.toMetal
  let layout := shader.params.layout
  fragmentCirclePacked shader.name shader.instanceCount layout.paddedFloatCount
    layout.packedFloatCount layout.packOffsets paramsStruct bodyCode

end CircleShader

end Afferent.Shader.DSL

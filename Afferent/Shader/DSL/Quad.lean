/-
  Afferent Shader DSL - Quad Fragment (Per-Pixel Shading)
  Defines quad-generating fragment shaders where the pixel color DSL runs in the fragment shader.

  Unlike CircleShader/RectShader which run DSL code in the vertex shader (once per instance),
  QuadShader runs DSL code in the fragment shader (once per pixel), enabling smooth gradients
  and custom per-pixel effects.
-/
import Afferent.Shader.DSL.Render
import Afferent.Shader.Fragment

namespace Afferent.Shader.DSL

open Afferent.Shader

/-- The vertex shader output for a quad: computes position and size.
    This runs once per instance to determine the quad bounds. -/
structure QuadInstanceExpr where
  position : ShaderExpr .float2  -- Top-left corner
  size     : ShaderExpr .float2  -- Width, height

namespace QuadInstanceExpr

/-- Render the quad instance result assignment and return statement. -/
def toMetal (result : QuadInstanceExpr) : String :=
  let lines := [
    "QuadVertexResult result;",
    s!"result.position = {result.position.toMetal};",
    s!"result.size = {result.size.toMetal};",
    "return result;"
  ]
  String.intercalate "\n" lines

end QuadInstanceExpr

/-- The fragment shader output for a quad: computes per-pixel color.
    This runs once per pixel with access to `pixelUV` (0-1 normalized coords). -/
structure QuadPixelExpr where
  color : ShaderExpr .float4  -- Per-pixel color (can use pixelUV)

namespace QuadPixelExpr

/-- Render the pixel result as a return statement. -/
def toMetal (result : QuadPixelExpr) : String :=
  s!"return {result.color.toMetal};"

end QuadPixelExpr

/-- A complete quad fragment shader definition with per-pixel shading.
    The `vertex` computation runs in the vertex shader (once per quad),
    while the `pixel` computation runs in the fragment shader (once per pixel). -/
structure QuadShader where
  /-- Unique name for this shader. -/
  name : String
  /-- Number of quads generated per draw call. -/
  instanceCount : Nat
  /-- Parameter struct fields (passed from Lean to GPU). -/
  params : ParamStruct
  /-- Vertex shader: computes quad bounds (position, size). -/
  vertex : QuadInstanceExpr
  /-- Fragment shader: computes per-pixel color (has access to pixelUV). -/
  pixel : QuadPixelExpr

namespace QuadShader

/-- Generate the Metal struct name for parameters. -/
def paramsTypeName (shader : QuadShader) : String :=
  let name := shader.name
  let first := name.get 0
  let rest := name.drop 1
  s!"{Char.toUpper first}{rest}Params"

/-- Compile a QuadShader to a ShaderFragment. -/
def compile (shader : QuadShader) : ShaderFragment :=
  let paramsStruct := shader.params.toMetal shader.paramsTypeName
  let vertexCode := shader.vertex.toMetal
  let pixelCode := shader.pixel.toMetal
  let layout := shader.params.layout
  fragmentQuadPacked shader.name shader.instanceCount layout.paddedFloatCount
    layout.packedFloatCount layout.packOffsets paramsStruct vertexCode pixelCode

end QuadShader

end Afferent.Shader.DSL

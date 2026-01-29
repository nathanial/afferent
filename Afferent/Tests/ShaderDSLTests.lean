/-
  Afferent Shader DSL Tests
  Unit tests for the shader DSL Metal code generation.
-/
import Afferent.Tests.Framework
import Afferent.Shader.DSL

namespace Afferent.Tests.ShaderDSLTests

open Crucible
open Shader

testSuite "Shader DSL Tests"

/-! ## ShaderType.toMetal Tests -/

test "ShaderType.float renders to Metal" := do
  shouldBe ShaderType.float.toMetal "float"

test "ShaderType.float2 renders to Metal" := do
  shouldBe ShaderType.float2.toMetal "float2"

test "ShaderType.float3 renders to Metal" := do
  shouldBe ShaderType.float3.toMetal "float3"

test "ShaderType.float4 renders to Metal" := do
  shouldBe ShaderType.float4.toMetal "float4"

test "ShaderType.uint renders to Metal" := do
  shouldBe ShaderType.uint.toMetal "uint"

test "ShaderType.bool renders to Metal" := do
  shouldBe ShaderType.bool.toMetal "bool"

/-! ## ShaderType.floatCount Tests -/

test "ShaderType.float has floatCount 1" := do
  shouldBe ShaderType.float.floatCount 1

test "ShaderType.float4 has floatCount 4" := do
  shouldBe ShaderType.float4.floatCount 4

/-! ## ParamStruct.toMetal Tests -/

test "empty ParamStruct renders to empty struct" := do
  let params : ParamStruct := []
  let result := params.toMetal "EmptyParams"
  -- Empty list produces empty intercalate, but we still get the braces on separate lines
  shouldBe result "struct EmptyParams {\n\n};"

test "single field ParamStruct renders correctly" := do
  let params : ParamStruct := [⟨"size", .float⟩]
  let result := params.toMetal "SingleParams"
  shouldBe result "struct SingleParams {\n  float size;\n};"

test "multiple field ParamStruct renders correctly" := do
  let params : ParamStruct := [
    ⟨"center", .float2⟩,
    ⟨"size", .float⟩,
    ⟨"color", .float4⟩
  ]
  let result := params.toMetal "MultiParams"
  shouldBe result "struct MultiParams {\n  float2 center;\n  float size;\n  float4 color;\n};"

test "ParamStruct.floatCount sums field sizes" := do
  let params : ParamStruct := [
    ⟨"center", .float2⟩,  -- 2
    ⟨"size", .float⟩,     -- 1
    ⟨"color", .float4⟩    -- 4
  ]
  shouldBe (params.floatCount) 7

/-! ## ShaderExpr Literal Tests -/

test "litFloat renders to Metal" := do
  let e : ShaderExpr .float := .litFloat 3.14
  shouldBe e.toMetal "3.140000"

test "litFloat integer renders with decimal" := do
  let e : ShaderExpr .float := .litFloat 5.0
  shouldBe e.toMetal "5.000000"

test "litFloat2 renders to Metal" := do
  let e : ShaderExpr .float2 := .litFloat2 1.0 2.0
  shouldBe e.toMetal "float2(1.000000, 2.000000)"

test "litFloat3 renders to Metal" := do
  let e : ShaderExpr .float3 := .litFloat3 1.0 2.0 3.0
  shouldBe e.toMetal "float3(1.000000, 2.000000, 3.000000)"

test "litFloat4 renders to Metal" := do
  let e : ShaderExpr .float4 := .litFloat4 1.0 2.0 3.0 4.0
  shouldBe e.toMetal "float4(1.000000, 2.000000, 3.000000, 4.000000)"

test "litUInt renders with u suffix" := do
  let e : ShaderExpr .uint := .litUInt 42
  shouldBe e.toMetal "42u"

test "litBool true renders to Metal" := do
  let e : ShaderExpr .bool := .litBool true
  shouldBe e.toMetal "true"

test "litBool false renders to Metal" := do
  let e : ShaderExpr .bool := .litBool false
  shouldBe e.toMetal "false"

/-! ## ShaderExpr Variable Tests -/

test "idx renders to Metal" := do
  let e : ShaderExpr .uint := .idx
  shouldBe e.toMetal "idx"

test "param renders with p. prefix" := do
  let e : ShaderExpr .float := .param "size" .float
  shouldBe e.toMetal "p.size"

test "var renders as variable name" := do
  let e : ShaderExpr .float := .var "myVar" .float
  shouldBe e.toMetal "myVar"

/-! ## ShaderExpr Arithmetic Tests -/

test "add renders with parentheses" := do
  let a : ShaderExpr .float := .litFloat 1.0
  let b : ShaderExpr .float := .litFloat 2.0
  let e := ShaderExpr.add a b
  shouldBe e.toMetal "(1.000000 + 2.000000)"

test "sub renders with parentheses" := do
  let a : ShaderExpr .float := .litFloat 5.0
  let b : ShaderExpr .float := .litFloat 3.0
  let e := ShaderExpr.sub a b
  shouldBe e.toMetal "(5.000000 - 3.000000)"

test "mul renders with parentheses" := do
  let a : ShaderExpr .float := .litFloat 2.0
  let b : ShaderExpr .float := .litFloat 3.0
  let e := ShaderExpr.mul a b
  shouldBe e.toMetal "(2.000000 * 3.000000)"

test "div renders with parentheses" := do
  let a : ShaderExpr .float := .litFloat 10.0
  let b : ShaderExpr .float := .litFloat 2.0
  let e := ShaderExpr.div a b
  shouldBe e.toMetal "(10.000000 / 2.000000)"

test "neg renders with parentheses" := do
  let a : ShaderExpr .float := .litFloat 5.0
  let e := ShaderExpr.neg a
  shouldBe e.toMetal "(-5.000000)"

/-! ## ShaderExpr Math Function Tests -/

test "sin renders to Metal" := do
  let e := ShaderExpr.sin (.litFloat 0.5)
  shouldBe e.toMetal "sin(0.500000)"

test "cos renders to Metal" := do
  let e := ShaderExpr.cos (.litFloat 0.5)
  shouldBe e.toMetal "cos(0.500000)"

test "fract renders to Metal" := do
  let e := ShaderExpr.fract (.litFloat 1.5)
  shouldBe e.toMetal "fract(1.500000)"

test "absF renders to Metal" := do
  let e := ShaderExpr.absF (.litFloat (-3.0))
  shouldBe e.toMetal "abs(-3.000000)"

test "sqrt renders to Metal" := do
  let e := ShaderExpr.sqrt (.litFloat 4.0)
  shouldBe e.toMetal "sqrt(4.000000)"

test "clampF renders to Metal" := do
  let e := ShaderExpr.clampF (.litFloat 1.5) (.litFloat 0.0) (.litFloat 1.0)
  shouldBe e.toMetal "clamp(1.500000, 0.000000, 1.000000)"

test "mixF renders to Metal" := do
  let e := ShaderExpr.mixF (.litFloat 0.0) (.litFloat 1.0) (.litFloat 0.5)
  shouldBe e.toMetal "mix(0.000000, 1.000000, 0.500000)"

test "smoothstep renders to Metal" := do
  let e := ShaderExpr.smoothstep (.litFloat 0.0) (.litFloat 1.0) (.litFloat 0.5)
  shouldBe e.toMetal "smoothstep(0.000000, 1.000000, 0.500000)"

/-! ## ShaderExpr Vector Construction Tests -/

test "vec2 renders to Metal" := do
  let e := ShaderExpr.vec2 (.litFloat 1.0) (.litFloat 2.0)
  shouldBe e.toMetal "float2(1.000000, 2.000000)"

test "vec3 renders to Metal" := do
  let e := ShaderExpr.vec3 (.litFloat 1.0) (.litFloat 2.0) (.litFloat 3.0)
  shouldBe e.toMetal "float3(1.000000, 2.000000, 3.000000)"

test "vec4 renders to Metal" := do
  let e := ShaderExpr.vec4 (.litFloat 1.0) (.litFloat 2.0) (.litFloat 3.0) (.litFloat 4.0)
  shouldBe e.toMetal "float4(1.000000, 2.000000, 3.000000, 4.000000)"

test "vec3from1 renders to Metal" := do
  let e := ShaderExpr.vec3from1 (.litFloat 1.0)
  shouldBe e.toMetal "float3(1.000000)"

test "vec4from31 renders to Metal" := do
  let rgb : ShaderExpr .float3 := .litFloat3 1.0 0.5 0.0
  let e := ShaderExpr.vec4from31 rgb (.litFloat 1.0)
  shouldBe e.toMetal "float4(float3(1.000000, 0.500000, 0.000000), 1.000000)"

/-! ## ShaderExpr Swizzle Tests -/

test "swizzleX renders .x suffix" := do
  let v : ShaderExpr .float2 := .litFloat2 1.0 2.0
  let e := ShaderExpr.swizzleX v
  shouldBe e.toMetal "float2(1.000000, 2.000000).x"

test "swizzleY renders .y suffix" := do
  let v : ShaderExpr .float2 := .param "center" .float2
  let e := ShaderExpr.swizzleY v
  shouldBe e.toMetal "p.center.y"

test "swizzleXYZ renders .xyz suffix" := do
  let v : ShaderExpr .float4 := .param "color" .float4
  let e := ShaderExpr.swizzleXYZ v
  shouldBe e.toMetal "p.color.xyz"

test "swizzleRGB renders .rgb suffix" := do
  let v : ShaderExpr .float4 := .param "color" .float4
  let e := ShaderExpr.swizzleRGB v
  shouldBe e.toMetal "p.color.rgb"

/-! ## ShaderExpr Comparison Tests -/

test "lt renders to Metal" := do
  let e := ShaderExpr.lt (.litFloat 1.0) (.litFloat 2.0)
  shouldBe e.toMetal "(1.000000 < 2.000000)"

test "eq renders to Metal" := do
  let a : ShaderExpr .float := .litFloat 1.0
  let b : ShaderExpr .float := .litFloat 1.0
  let e := ShaderExpr.eq a b
  shouldBe e.toMetal "(1.000000 == 1.000000)"

test "eqU renders for uint" := do
  let e := ShaderExpr.eqU (.litUInt 1) (.litUInt 1)
  shouldBe e.toMetal "(1u == 1u)"

/-! ## ShaderExpr Boolean Tests -/

test "andB renders to Metal" := do
  let e := ShaderExpr.andB (.litBool true) (.litBool false)
  shouldBe e.toMetal "(true && false)"

test "orB renders to Metal" := do
  let e := ShaderExpr.orB (.litBool true) (.litBool false)
  shouldBe e.toMetal "(true || false)"

test "notB renders to Metal" := do
  let e := ShaderExpr.notB (.litBool true)
  shouldBe e.toMetal "(!true)"

/-! ## ShaderExpr Conditional Tests -/

test "cond renders ternary operator" := do
  let c : ShaderExpr .bool := .litBool true
  let t : ShaderExpr .float := .litFloat 1.0
  let f : ShaderExpr .float := .litFloat 0.0
  let e := ShaderExpr.cond c t f
  shouldBe e.toMetal "(true ? 1.000000 : 0.000000)"

/-! ## ShaderExpr Type Conversion Tests -/

test "toFloat renders uint to float conversion" := do
  let e := ShaderExpr.toFloat (.litUInt 5)
  shouldBe e.toMetal "float(5u)"

test "toUInt renders float to uint conversion" := do
  let e := ShaderExpr.toUInt (.litFloat 5.0)
  shouldBe e.toMetal "uint(5.000000)"

/-! ## ShaderExpr Integer Operation Tests -/

test "idiv renders integer division" := do
  let e := ShaderExpr.idiv (.litUInt 10) (.litUInt 3)
  shouldBe e.toMetal "(10u / 3u)"

test "imod renders integer modulo" := do
  let e := ShaderExpr.imod (.litUInt 10) (.litUInt 3)
  shouldBe e.toMetal "(10u % 3u)"

/-! ## ShaderExpr Let Binding Tests -/

test "letIn renders compound expression" := do
  let val : ShaderExpr .float := .litFloat 5.0
  let body : ShaderExpr .float := .var "x" .float
  let e := ShaderExpr.letIn "x" .float val body
  shouldBe e.toMetal "({ float x = 5.000000; x; })"

test "letIn with expression body" := do
  let val : ShaderExpr .float := .litFloat 5.0
  let body : ShaderExpr .float := ShaderExpr.add (.var "x" .float) (.litFloat 1.0)
  let e := ShaderExpr.letIn "x" .float val body
  shouldBe e.toMetal "({ float x = 5.000000; (x + 1.000000); })"

/-! ## DSL Operator Instance Tests -/

test "OfNat instance for float" := do
  let e : ShaderExpr .float := 42
  shouldBe e.toMetal "42.000000"

test "OfScientific instance for float" := do
  let e : ShaderExpr .float := 3.14
  shouldBe e.toMetal "3.140000"

test "Add instance works" := do
  let a : ShaderExpr .float := 1
  let b : ShaderExpr .float := 2
  let e := a + b
  shouldBe e.toMetal "(1.000000 + 2.000000)"

test "Sub instance works" := do
  let a : ShaderExpr .float := 5
  let b : ShaderExpr .float := 3
  let e := a - b
  shouldBe e.toMetal "(5.000000 - 3.000000)"

test "Mul instance works" := do
  let a : ShaderExpr .float := 2
  let b : ShaderExpr .float := 3
  let e := a * b
  shouldBe e.toMetal "(2.000000 * 3.000000)"

test "Div instance works" := do
  let a : ShaderExpr .float := 10
  let b : ShaderExpr .float := 2
  let e := a / b
  shouldBe e.toMetal "(10.000000 / 2.000000)"

test "Neg instance works" := do
  let a : ShaderExpr .float := 5
  let e := -a
  shouldBe e.toMetal "(-5.000000)"

/-! ## DSL Helper Function Tests -/

test "sin helper function works" := do
  let e := sin (1.0 : ShaderExpr .float)
  shouldBe e.toMetal "sin(1.000000)"

test "cos helper function works" := do
  let e := cos (1.0 : ShaderExpr .float)
  shouldBe e.toMetal "cos(1.000000)"

test "fract helper function works" := do
  let e := fract (1.5 : ShaderExpr .float)
  shouldBe e.toMetal "fract(1.500000)"

test "vec2 helper function works" := do
  let e := vec2 (1.0 : ShaderExpr .float) 2.0
  shouldBe e.toMetal "float2(1.000000, 2.000000)"

test "idx helper returns instance index" := do
  let e := idx
  shouldBe e.toMetal "idx"

test "param helper returns parameter access" := do
  let e := param "size" .float
  shouldBe e.toMetal "p.size"

/-! ## DSL Prelude Tests -/

test "pi constant has correct value" := do
  shouldBe pi.toMetal "3.141593"

test "twoPi constant has correct value" := do
  shouldBe twoPi.toMetal "6.283185"

test "quarterPi constant has correct value" := do
  shouldBe quarterPi.toMetal "0.785398"

/-! ## CircleResultExpr Tests -/

test "CircleResultExpr.toMetal generates correct code" := do
  let result : CircleResultExpr := {
    center := .litFloat2 100.0 200.0
    radius := .litFloat 50.0
    color := .litFloat4 1.0 0.0 0.0 1.0
  }
  let code := result.toMetal
  shouldContainSubstr code "CircleResult result;"
  shouldContainSubstr code "result.center = float2(100.000000, 200.000000);"
  shouldContainSubstr code "result.radius = 50.000000;"
  shouldContainSubstr code "result.color = float4(1.000000, 0.000000, 0.000000, 1.000000);"
  shouldContainSubstr code "return result;"

/-! ## CircleShader Tests -/

test "CircleShader.paramsTypeName capitalizes first letter" := do
  let shader : CircleShader := {
    name := "helix"
    instanceCount := 16
    params := []
    body := {
      center := .litFloat2 0.0 0.0
      radius := .litFloat 1.0
      color := .litFloat4 1.0 1.0 1.0 1.0
    }
  }
  shouldBe shader.paramsTypeName "HelixParams"

test "CircleShader.compile produces ShaderFragment" := do
  let shader : CircleShader := {
    name := "test"
    instanceCount := 4
    params := [
      ⟨"center", .float2⟩,
      ⟨"size", .float⟩
    ]
    body := {
      center := param "center" .float2
      radius := param "size" .float
      color := .litFloat4 1.0 1.0 1.0 1.0
    }
  }
  let fragment := shader.compile
  shouldBe fragment.name "test"
  shouldBe fragment.instanceCount 4
  shouldBe fragment.paramsFloatCount 4
  shouldBe fragment.paramsPackedFloatCount 3
  shouldBe fragment.paramsPackOffsets.size 3
  shouldContainSubstr fragment.paramsStructCode "struct TestParams"
  shouldContainSubstr fragment.paramsStructCode "float2 center;"
  shouldContainSubstr fragment.paramsStructCode "float size;"
  shouldContainSubstr fragment.functionCode "result.center = p.center;"
  shouldContainSubstr fragment.functionCode "result.radius = p.size;"

/-! ## Complex Expression Tests -/

test "nested arithmetic renders correctly" := do
  -- (1 + 2) * (3 - 4)
  let a : ShaderExpr .float := 1
  let b : ShaderExpr .float := 2
  let c : ShaderExpr .float := 3
  let d : ShaderExpr .float := 4
  let e := (a + b) * (c - d)
  shouldBe e.toMetal "((1.000000 + 2.000000) * (3.000000 - 4.000000))"

test "chained function calls render correctly" := do
  -- sin(cos(0.5))
  let e := sin (cos (0.5 : ShaderExpr .float))
  shouldBe e.toMetal "sin(cos(0.500000))"

test "vector component access in expression" := do
  -- p.center.x + p.center.y
  let cx := (param "center" .float2).x
  let cy := (param "center" .float2).y
  let e := cx + cy
  shouldBe e.toMetal "(p.center.x + p.center.y)"

/-! ## hsvToRgb Helper Tests -/

test "hsvToRgb generates correct structure" := do
  let rgb := hsvToRgb (0.5 : ShaderExpr .float) 0.8 1.0
  let code := rgb.toMetal
  -- Should contain the K constant pattern
  shouldContainSubstr code "float4(1.000000, 0.666667, 0.333333, 3.000000)"
  -- Should use abs, fract, clamp, mix
  shouldContainSubstr code "abs("
  shouldContainSubstr code "fract("
  shouldContainSubstr code "clamp("
  shouldContainSubstr code "mix("

/-! ## Pixel Coordinate Tests (QuadShader) -/

test "pixelUV renders to in.uv" := do
  let e : ShaderExpr .float2 := .pixelUV
  shouldBe e.toMetal "in.uv"

test "pixelPos renders to in.position.xy" := do
  let e : ShaderExpr .float2 := .pixelPos
  shouldBe e.toMetal "in.position.xy"

test "pixelUV prelude helper works" := do
  shouldBe pixelUV.toMetal "in.uv"

test "pixelPos prelude helper works" := do
  shouldBe pixelPos.toMetal "in.position.xy"

test "radialDistance uses pixelUV" := do
  let code := radialDistance.toMetal
  shouldContainSubstr code "in.uv"
  shouldContainSubstr code "length("

test "radialFalloff uses radialDistance" := do
  let code := radialFalloff.toMetal
  shouldContainSubstr code "smoothstep("
  shouldContainSubstr code "in.uv"

/-! ## QuadInstanceExpr Tests -/

test "QuadInstanceExpr.toMetal generates correct code" := do
  let result : QuadInstanceExpr := {
    position := .litFloat2 100.0 200.0
    size := .litFloat2 50.0 60.0
  }
  let code := result.toMetal
  shouldContainSubstr code "QuadVertexResult result;"
  shouldContainSubstr code "result.position = float2(100.000000, 200.000000);"
  shouldContainSubstr code "result.size = float2(50.000000, 60.000000);"
  shouldContainSubstr code "return result;"

/-! ## QuadPixelExpr Tests -/

test "QuadPixelExpr.toMetal generates return statement" := do
  let result : QuadPixelExpr := {
    color := .litFloat4 1.0 0.0 0.0 1.0
  }
  let code := result.toMetal
  shouldBe code "return float4(1.000000, 0.000000, 0.000000, 1.000000);"

test "QuadPixelExpr with pixelUV expression" := do
  -- Color based on UV coordinates
  let result : QuadPixelExpr := {
    color := vec4 pixelUV.x pixelUV.y 0.0 1.0
  }
  let code := result.toMetal
  shouldContainSubstr code "in.uv.x"
  shouldContainSubstr code "in.uv.y"

/-! ## QuadShader Tests -/

test "QuadShader.paramsTypeName capitalizes first letter" := do
  let shader : QuadShader := {
    name := "radialStar"
    instanceCount := 1
    params := []
    vertex := {
      position := .litFloat2 0.0 0.0
      size := .litFloat2 100.0 100.0
    }
    pixel := {
      color := .litFloat4 1.0 1.0 1.0 1.0
    }
  }
  shouldBe shader.paramsTypeName "RadialStarParams"

test "QuadShader.compile produces ShaderFragment with quad primitive" := do
  let shader : QuadShader := {
    name := "gradient"
    instanceCount := 1
    params := [
      ⟨"center", .float2⟩,
      ⟨"size", .float⟩,
      ⟨"color", .float4⟩
    ]
    vertex := {
      position := param "center" .float2 - vec2 (param "size" .float) (param "size" .float)
      size := vec2 (param "size" .float * 2.0) (param "size" .float * 2.0)
    }
    pixel := {
      color := param "color" .float4 * vec4 1.0 1.0 1.0 radialFalloff
    }
  }
  let fragment := shader.compile
  shouldBe fragment.name "gradient"
  shouldBe fragment.instanceCount 1
  shouldBe fragment.primitive .quad
  shouldContainSubstr fragment.paramsStructCode "struct GradientParams"
  shouldContainSubstr fragment.paramsStructCode "float2 center;"
  shouldContainSubstr fragment.paramsStructCode "float size;"
  shouldContainSubstr fragment.paramsStructCode "float4 color;"
  -- functionCode contains vertex|||pixel separator
  shouldContainSubstr fragment.functionCode "|||"

test "QuadShader functionCode contains vertex and pixel code" := do
  let shader : QuadShader := {
    name := "test"
    instanceCount := 1
    params := []
    vertex := {
      position := .litFloat2 10.0 20.0
      size := .litFloat2 100.0 100.0
    }
    pixel := {
      color := vec4 pixelUV.x pixelUV.y 0.5 1.0
    }
  }
  let fragment := shader.compile
  let parts := fragment.functionCode.splitOn "|||"
  shouldBe parts.length 2
  -- Vertex part contains position and size
  shouldContainSubstr parts[0]! "result.position"
  shouldContainSubstr parts[0]! "result.size"
  -- Pixel part contains UV access
  shouldContainSubstr parts[1]! "in.uv"



end Afferent.Tests.ShaderDSLTests

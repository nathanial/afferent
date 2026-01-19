/-
  Afferent Shader DSL - Metal Code Generation
  Renders ShaderExpr AST to Metal shader code.
-/
import Afferent.Shader.DSL.Expr

namespace Afferent.Shader.DSL

/-- Format a Float for Metal (avoid trailing zeros, handle precision). -/
private def formatFloat (f : Float) : String :=
  let s := toString f
  -- Ensure we have a decimal point for float literals in Metal
  if s.contains '.' then s else s ++ ".0"

/-- Render a ShaderExpr to Metal code string. -/
partial def ShaderExpr.toMetal : ShaderExpr t → String
  -- Literals
  | .litFloat f => formatFloat f
  | .litFloat2 x y => s!"float2({formatFloat x}, {formatFloat y})"
  | .litFloat3 x y z => s!"float3({formatFloat x}, {formatFloat y}, {formatFloat z})"
  | .litFloat4 x y z w => s!"float4({formatFloat x}, {formatFloat y}, {formatFloat z}, {formatFloat w})"
  | .litUInt n => s!"{n}u"
  | .litInt n => s!"{n}"
  | .litBool b => if b then "true" else "false"

  -- Instance index
  | .idx => "idx"

  -- Pixel coordinates (for QuadShader)
  | .pixelUV  => "in.uv"
  | .pixelPos => "in.position.xy"

  -- Variable access
  | .param field _ => s!"p.{field}"
  | .var name _ => name

  -- Arithmetic
  | .add a b => s!"({a.toMetal} + {b.toMetal})"
  | .sub a b => s!"({a.toMetal} - {b.toMetal})"
  | .mul a b => s!"({a.toMetal} * {b.toMetal})"
  | .div a b => s!"({a.toMetal} / {b.toMetal})"
  | .neg a => s!"(-{a.toMetal})"

  -- Scalar multiplication
  | .scale s v => s!"({s.toMetal} * {v.toMetal})"

  -- Integer operations
  | .idiv a b => s!"({a.toMetal} / {b.toMetal})"
  | .imod a b => s!"({a.toMetal} % {b.toMetal})"

  -- Type conversion
  | .toFloat e => s!"float({e.toMetal})"
  | .toUInt e => s!"uint({e.toMetal})"

  -- Math functions
  | .sin e => s!"sin({e.toMetal})"
  | .cos e => s!"cos({e.toMetal})"
  | .tan e => s!"tan({e.toMetal})"
  | .asin e => s!"asin({e.toMetal})"
  | .acos e => s!"acos({e.toMetal})"
  | .atan e => s!"atan({e.toMetal})"
  | .atan2 y x => s!"atan2({y.toMetal}, {x.toMetal})"
  | .sqrt e => s!"sqrt({e.toMetal})"
  | .pow b e => s!"pow({b.toMetal}, {e.toMetal})"
  | .exp e => s!"exp({e.toMetal})"
  | .log e => s!"log({e.toMetal})"
  | .floor e => s!"floor({e.toMetal})"
  | .ceil e => s!"ceil({e.toMetal})"
  | .round e => s!"round({e.toMetal})"
  | .fract e => s!"fract({e.toMetal})"
  | .absF e => s!"abs({e.toMetal})"
  | .signF e => s!"sign({e.toMetal})"
  | .minF a b => s!"min({a.toMetal}, {b.toMetal})"
  | .maxF a b => s!"max({a.toMetal}, {b.toMetal})"
  | .clampF x lo hi => s!"clamp({x.toMetal}, {lo.toMetal}, {hi.toMetal})"
  | .mixF a b t => s!"mix({a.toMetal}, {b.toMetal}, {t.toMetal})"
  | .smoothstep lo hi x => s!"smoothstep({lo.toMetal}, {hi.toMetal}, {x.toMetal})"
  | .step edge x => s!"step({edge.toMetal}, {x.toMetal})"

  -- Vector-wise functions
  | .absV e => s!"abs({e.toMetal})"
  | .clampV x lo hi => s!"clamp({x.toMetal}, {lo.toMetal}, {hi.toMetal})"
  | .mixV a b t => s!"mix({a.toMetal}, {b.toMetal}, {t.toMetal})"
  | .minV a b => s!"min({a.toMetal}, {b.toMetal})"
  | .maxV a b => s!"max({a.toMetal}, {b.toMetal})"
  | .fractV e => s!"fract({e.toMetal})"

  -- Vector construction
  | .vec2 x y => s!"float2({x.toMetal}, {y.toMetal})"
  | .vec3 x y z => s!"float3({x.toMetal}, {y.toMetal}, {z.toMetal})"
  | .vec4 x y z w => s!"float4({x.toMetal}, {y.toMetal}, {z.toMetal}, {w.toMetal})"
  | .vec3from1 x => s!"float3({x.toMetal})"
  | .vec4from1 x => s!"float4({x.toMetal})"
  | .vec4from31 v w => s!"float4({v.toMetal}, {w.toMetal})"

  -- Vector geometry
  | .length e => s!"length({e.toMetal})"
  | .normalize e => s!"normalize({e.toMetal})"
  | .dot a b => s!"dot({a.toMetal}, {b.toMetal})"
  | .cross a b => s!"cross({a.toMetal}, {b.toMetal})"
  | .distance a b => s!"distance({a.toMetal}, {b.toMetal})"

  -- Swizzle single component
  | .swizzleX e => s!"{e.toMetal}.x"
  | .swizzleY e => s!"{e.toMetal}.y"
  | .swizzleZ e => s!"{e.toMetal}.z"
  | .swizzleW e => s!"{e.toMetal}.w"

  -- Multi-component swizzle
  | .swizzleXY e => s!"{e.toMetal}.xy"
  | .swizzleXYZ e => s!"{e.toMetal}.xyz"
  | .swizzleXXX e => s!"{e.toMetal}.xxx"
  | .swizzleWWW e => s!"{e.toMetal}.www"
  | .swizzleRGB e => s!"{e.toMetal}.rgb"

  -- Comparisons
  | .lt a b => s!"({a.toMetal} < {b.toMetal})"
  | .le a b => s!"({a.toMetal} <= {b.toMetal})"
  | .gt a b => s!"({a.toMetal} > {b.toMetal})"
  | .ge a b => s!"({a.toMetal} >= {b.toMetal})"
  | .eq a b => s!"({a.toMetal} == {b.toMetal})"
  | .ne a b => s!"({a.toMetal} != {b.toMetal})"
  | .ltU a b => s!"({a.toMetal} < {b.toMetal})"
  | .eqU a b => s!"({a.toMetal} == {b.toMetal})"

  -- Boolean operations
  | .andB a b => s!"({a.toMetal} && {b.toMetal})"
  | .orB a b => s!"({a.toMetal} || {b.toMetal})"
  | .notB a => s!"(!{a.toMetal})"

  -- Conditional
  | .cond c t f => s!"({c.toMetal} ? {t.toMetal} : {f.toMetal})"

  -- Let binding (uses compound expression syntax)
  | .letIn name ty val body =>
      s!"(\{ {ty.toMetal} {name} = {val.toMetal}; {body.toMetal}; })"

end Afferent.Shader.DSL

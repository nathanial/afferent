/-
  Afferent Shader DSL - Prelude
  Constants and common shader operations.
-/
import Afferent.Shader.DSL.Ops

namespace Afferent.Shader.DSL

/-! ## Mathematical Constants -/

/-- Pi (π ≈ 3.14159). -/
def pi : ShaderExpr .float := .litFloat 3.14159265359

/-- Two pi (2π ≈ 6.28318). -/
def twoPi : ShaderExpr .float := .litFloat 6.28318530718

/-- Half pi (π/2 ≈ 1.5708). -/
def halfPi : ShaderExpr .float := .litFloat 1.57079632679

/-- Quarter pi (π/4 ≈ 0.7854). -/
def quarterPi : ShaderExpr .float := .litFloat 0.78539816339

/-- Euler's number (e ≈ 2.71828). -/
def euler : ShaderExpr .float := .litFloat 2.71828182845

/-! ## Common Float Literals -/

/-- Zero as a float. -/
def zero : ShaderExpr .float := .litFloat 0.0

/-- One as a float. -/
def one : ShaderExpr .float := .litFloat 1.0

/-- Negative one as a float. -/
def negOne : ShaderExpr .float := .litFloat (-1.0)

/-- One half as a float. -/
def half : ShaderExpr .float := .litFloat 0.5

/-! ## Color Operations -/

/-- Convert HSV to RGB color.
    h: hue (0-1), s: saturation (0-1), v: value (0-1)
    Returns float3 RGB values (0-1).

    This implements the standard HSV to RGB conversion using the
    formula from GPU shader programming. -/
def hsvToRgb (h s v : ShaderExpr .float) : ShaderExpr .float3 :=
  -- float4 K = float4(1.0, 2.0/3.0, 1.0/3.0, 3.0);
  let k : ShaderExpr .float4 := .litFloat4 1.0 (2.0/3.0) (1.0/3.0) 3.0
  -- float3 hp = abs(fract(float3(h, h, h) + K.xyz) * 6.0 - K.www);
  let hVec : ShaderExpr .float3 := .vec3 h h h
  let kXyz : ShaderExpr .float3 := .swizzleXYZ k
  let kWww : ShaderExpr .float3 := .swizzleWWW k
  let hp := absV (scale 6.0 (fractV (hVec + kXyz)) - kWww)
  -- float3 rgb = v * mix(K.xxx, clamp(hp - K.xxx, 0.0, 1.0), s);
  let kXxx : ShaderExpr .float3 := .swizzleXXX k
  let zeroVec : ShaderExpr .float3 := .vec3from1 zero
  let oneVec : ShaderExpr .float3 := .vec3from1 one
  let clamped := clampV (hp - kXxx) zeroVec oneVec
  let mixed := mixV kXxx clamped s
  scale v mixed

/-- Premultiply alpha on an RGB color.
    Returns float4 with RGB premultiplied by alpha. -/
def premultiplyAlpha (rgb : ShaderExpr .float3) (a : ShaderExpr .float) : ShaderExpr .float4 :=
  vec4from31 (scale a rgb) a

/-! ## Animation Helpers -/

/-- Smooth oscillation between 0 and 1 based on time.
    period: time for one complete cycle. -/
def oscillate (time period : ShaderExpr .float) : ShaderExpr .float :=
  (sin (time * twoPi / period) + one) * half

/-- Smooth pulse effect (0 to 1 to 0) over a period. -/
def pulse (time period : ShaderExpr .float) : ShaderExpr .float :=
  let phase := fract (time / period)
  sin (phase * pi)

/-- Linear ramp from 0 to 1 over a period, then reset. -/
def sawtooth (time period : ShaderExpr .float) : ShaderExpr .float :=
  fract (time / period)

/-- Ping-pong between 0 and 1. -/
def pingPong (time period : ShaderExpr .float) : ShaderExpr .float :=
  let t := fract (time / period)
  cond (lt t half) (t * 2.0) (2.0 - t * 2.0)

/-! ## Easing Functions -/

/-- Ease in (quadratic). -/
def easeInQuad (t : ShaderExpr .float) : ShaderExpr .float := t * t

/-- Ease out (quadratic). -/
def easeOutQuad (t : ShaderExpr .float) : ShaderExpr .float := t * (2.0 - t)

/-- Ease in-out (quadratic). -/
def easeInOutQuad (t : ShaderExpr .float) : ShaderExpr .float :=
  cond (lt t half) (2.0 * t * t) (negOne + (4.0 - 2.0 * t) * t)

/-- Ease in (cubic). -/
def easeInCubic (t : ShaderExpr .float) : ShaderExpr .float := t * t * t

/-- Ease out (cubic). -/
def easeOutCubic (t : ShaderExpr .float) : ShaderExpr .float :=
  let tm1 := t - one
  tm1 * tm1 * tm1 + one

/-! ## Geometry Helpers -/

/-- Rotate a 2D point around the origin by angle (radians). -/
def rotate2D (p : ShaderExpr .float2) (angle : ShaderExpr .float) : ShaderExpr .float2 :=
  let c := cos angle
  let s := sin angle
  let px := p.x
  let py := p.y
  vec2 (px * c - py * s) (px * s + py * c)

/-- Compute the signed distance from a point to a circle. -/
def sdfCircle (p : ShaderExpr .float2) (center : ShaderExpr .float2)
    (radius : ShaderExpr .float) : ShaderExpr .float :=
  length (p - center) - radius

/-! ## Parameter Accessors -/

/-- Common parameter: center position (float2). -/
def center : ShaderExpr .float2 := param "center" .float2

/-- Common parameter: size (float). -/
def size : ShaderExpr .float := param "size" .float

/-- Common parameter: time in seconds (float). -/
def time : ShaderExpr .float := param "time" .float

/-- Common parameter: base color (float4). -/
def color : ShaderExpr .float4 := param "color" .float4

end Afferent.Shader.DSL

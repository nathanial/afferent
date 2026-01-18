/-
  Afferent Shader DSL
  A domain-specific language for writing GPU shaders in pure Lean.

  This module provides:
  - Typed expression AST (`ShaderExpr`) that mirrors Metal types
  - Metal code generation via `toMetal`
  - Circle fragment shader compilation via `CircleShader.compile`
  - Operator instances for ergonomic DSL usage
  - Common shader operations (hsvToRgb, easing functions, etc.)

  ## Example Usage

  ```lean
  import Afferent.Shader.DSL

  open Afferent.Shader.DSL in
  def myShader : CircleShader := {
    name := "myShader"
    instanceCount := 8
    params := [
      ⟨"center", .float2⟩,
      ⟨"size", .float⟩,
      ⟨"time", .float⟩,
      ⟨"color", .float4⟩
    ]
    body := {
      center := center + vec2 (sin (time * twoPi)) 0.0
      radius := size * 0.1
      color := color
    }
  }

  def myFragment : ShaderFragment := myShader.compile
  ```
-/

import Afferent.Shader.DSL.Types
import Afferent.Shader.DSL.Expr
import Afferent.Shader.DSL.Render
import Afferent.Shader.DSL.Ops
import Afferent.Shader.DSL.Prelude
import Afferent.Shader.DSL.Circle
import Afferent.Shader.DSL.Rect

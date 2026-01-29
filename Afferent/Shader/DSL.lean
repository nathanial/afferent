/-
  Afferent Shader DSL
  A domain-specific language for writing GPU shaders in pure Lean.

  This module re-exports the standalone Shader library for use within Afferent.
  Import this module and use `open Shader` to access the DSL.

  This module provides:
  - Typed expression AST (`ShaderExpr`) that mirrors Metal types
  - Metal code generation via `toMetal`
  - Circle fragment shader compilation via `CircleShader.compile`
  - Operator instances for ergonomic DSL usage
  - Common shader operations (hsvToRgb, easing functions, etc.)

  ## Example Usage

  ```lean
  import Afferent.Shader.DSL

  open Shader in
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

-- Import the standalone Shader library
-- Consumers should use `open Shader` to access the DSL
import Shader

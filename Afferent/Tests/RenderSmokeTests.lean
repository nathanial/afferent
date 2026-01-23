/-
  Afferent Render Smoke Tests
  Lightweight checks for shader registry and instanced buffer packing.
-/
import Afferent.Tests.Framework
import Afferent.Shaders
import Afferent.Render.Dynamic
import Afferent.FFI.FloatBuffer
import Init.Data.FloatArray

namespace Afferent.Tests.RenderSmokeTests

open Crucible
open Afferent.Tests
open Afferent

testSuite "Render Smoke Tests"

private def shaderSource? (name : String) : Option String :=
  Afferent.Shaders.all.find? (fun entry => entry.fst == name) |>.map (·.snd)

private def hasDuplicate (names : Array String) : Bool := Id.run do
  let mut seen : Array String := #[]
  for n in names do
    if seen.contains n then
      return true
    seen := seen.push n
  return false

test "shader registry includes expected names and non-empty sources" := do
  let names := Afferent.Shaders.all.map (·.fst)
  ensure (!hasDuplicate names) "Shader registry contains duplicate names"
  let expected := #[
    "basic",
    "text",
    "instanced",
    "sprite",
    "stroke",
    "stroke_path",
    "mesh3d"
  ]
  for name in expected do
    match shaderSource? name with
    | some src =>
        ensure (src.length > 0) s!"Shader {name} source is empty"
    | none =>
        ensure false s!"Shader {name} missing from registry"

test "core shader entry points exist" := do
  let instanced := (shaderSource? "instanced").getD ""
  shouldContainSubstr instanced "instanced_vertex_main"
  shouldContainSubstr instanced "instanced_fragment_main"
  let sprite := (shaderSource? "sprite").getD ""
  shouldContainSubstr sprite "sprite_vertex_layout0"
  shouldContainSubstr sprite "sprite_fragment"
  let mesh3d := (shaderSource? "mesh3d").getD ""
  shouldContainSubstr mesh3d "vertex_main_3d"
  shouldContainSubstr mesh3d "vertex_main_3d_textured"
  shouldContainSubstr mesh3d "fragment_main_3d"

private def mkParticles : Render.Dynamic.ParticleState :=
  let data := Id.run do
    let mut arr := FloatArray.emptyWithCapacity 10
    -- Particle 0: x=10, y=20, vx=0, vy=0, hue=0.1
    arr := arr.push 10.0
    arr := arr.push 20.0
    arr := arr.push 0.0
    arr := arr.push 0.0
    arr := arr.push 0.1
    -- Particle 1: x=30, y=40, vx=0, vy=0, hue=0.3
    arr := arr.push 30.0
    arr := arr.push 40.0
    arr := arr.push 0.0
    arr := arr.push 0.0
    arr := arr.push 0.3
    arr
  { data, count := 2, screenWidth := 100.0, screenHeight := 100.0 }

test "instanced buffer writer packs uniform rotation layout" := do
  let particles := mkParticles
  let buf ← particles.createInstanceBuffer
  Render.Dynamic.writeInstancedUniformToBuffer particles buf 5.0 1.25
  let x0 ← FFI.FloatBuffer.get buf 0
  let y0 ← FFI.FloatBuffer.get buf 1
  let rot0 ← FFI.FloatBuffer.get buf 2
  let size0 ← FFI.FloatBuffer.get buf 3
  let hue0 ← FFI.FloatBuffer.get buf 4
  let alpha0 ← FFI.FloatBuffer.get buf 7
  shouldBeNear x0 10.0
  shouldBeNear y0 20.0
  shouldBeNear rot0 1.25
  shouldBeNear size0 5.0
  shouldBeNear hue0 0.1
  shouldBeNear alpha0 1.0
  let x1 ← FFI.FloatBuffer.get buf 8
  let y1 ← FFI.FloatBuffer.get buf 9
  shouldBeNear x1 30.0
  shouldBeNear y1 40.0
  FFI.FloatBuffer.destroy buf

test "instanced buffer writer packs animated rotation layout" := do
  let particles := mkParticles
  let buf ← particles.createInstanceBuffer
  Render.Dynamic.writeInstancedAnimatedToBuffer particles buf 5.0 2.0 3.0
  let angle0 ← FFI.FloatBuffer.get buf 2
  let angle1 ← FFI.FloatBuffer.get buf 10
  let twoPi : Float := 6.283185307
  let expected0 := 2.0 * 3.0 + 0.1 * twoPi
  let expected1 := 2.0 * 3.0 + 0.3 * twoPi
  shouldBeNear angle0 expected0
  shouldBeNear angle1 expected1
  FFI.FloatBuffer.destroy buf

test "sprite buffer writer packs layout" := do
  let particles := mkParticles
  let buf ← particles.createSpriteBuffer
  Render.Dynamic.writeSpritesToBuffer particles buf 7.0 0.5 0.25
  let x0 ← FFI.FloatBuffer.get buf 0
  let y0 ← FFI.FloatBuffer.get buf 1
  let rot0 ← FFI.FloatBuffer.get buf 2
  let size0 ← FFI.FloatBuffer.get buf 3
  let alpha0 ← FFI.FloatBuffer.get buf 4
  shouldBeNear x0 10.0
  shouldBeNear y0 20.0
  shouldBeNear rot0 0.5
  shouldBeNear size0 7.0
  shouldBeNear alpha0 0.25
  FFI.FloatBuffer.destroy buf



end Afferent.Tests.RenderSmokeTests

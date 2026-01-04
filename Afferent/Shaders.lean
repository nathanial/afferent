/-
  Afferent.Shaders - Embedded Metal shader sources

  Shaders are embedded at compile time using include_str% from Staple.
  This eliminates the need to locate shader files at runtime.
-/
import Staple

namespace Afferent.Shaders

/-- Basic colored vertices shader -/
def basic : String := include_str% "../native/src/metal/shaders/basic.metal"

/-- Text rendering shader -/
def text : String := include_str% "../native/src/metal/shaders/text.metal"

/-- Instanced shapes shader (rects, triangles, circles) -/
def instanced : String := include_str% "../native/src/metal/shaders/instanced.metal"

/-- GPU-side animated shapes shader -/
def animated : String := include_str% "../native/src/metal/shaders/animated.metal"

/-- Orbital particles shader -/
def orbital : String := include_str% "../native/src/metal/shaders/orbital.metal"

/-- Sprite/texture shader (layout0 + layout1 entry points). -/
def sprite : String := include_str% "../native/src/metal/shaders/sprite.metal"

/-- Screen-space stroke shader -/
def stroke : String := include_str% "../native/src/metal/shaders/stroke.metal"

/-- Screen-space stroke path shader (segment-based) -/
def strokePath : String := include_str% "../native/src/metal/shaders/stroke_path.metal"

/-- 3D mesh shader with lighting and fog -/
def mesh3d : String := include_str% "../native/src/metal/shaders/mesh3d.metal"

/-- 3D textured mesh shader -/
def mesh3dTextured : String := include_str% "../native/src/metal/shaders/mesh3d_textured.metal"

/-- All shader sources as (name, source) pairs for FFI initialization -/
def all : Array (String × String) := #[
  ("basic", basic),
  ("text", text),
  ("instanced", instanced),
  ("animated", animated),
  ("orbital", orbital),
  ("sprite", sprite),
  ("stroke", stroke),
  ("stroke_path", strokePath),
  ("mesh3d", mesh3d),
  ("mesh3d_textured", mesh3dTextured)
]

end Afferent.Shaders

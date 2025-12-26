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

/-- Dynamic circle shader -/
def dynamicCircle : String := include_str% "../native/src/metal/shaders/dynamic_circle.metal"

/-- Dynamic rect shader -/
def dynamicRect : String := include_str% "../native/src/metal/shaders/dynamic_rect.metal"

/-- Dynamic triangle shader -/
def dynamicTriangle : String := include_str% "../native/src/metal/shaders/dynamic_triangle.metal"

/-- Sprite/texture shader -/
def sprite : String := include_str% "../native/src/metal/shaders/sprite.metal"

/-- 3D mesh shader with lighting and fog -/
def mesh3d : String := include_str% "../native/src/metal/shaders/mesh3d.metal"

/-- 3D textured mesh shader -/
def mesh3dTextured : String := include_str% "../native/src/metal/shaders/mesh3d_textured.metal"

/-- Textured rectangle shader (for map tiles) -/
def texturedRect : String := include_str% "../native/src/metal/shaders/textured_rect.metal"

/-- All shader sources as (name, source) pairs for FFI initialization -/
def all : Array (String × String) := #[
  ("basic", basic),
  ("text", text),
  ("instanced", instanced),
  ("animated", animated),
  ("orbital", orbital),
  ("dynamic_circle", dynamicCircle),
  ("dynamic_rect", dynamicRect),
  ("dynamic_triangle", dynamicTriangle),
  ("sprite", sprite),
  ("mesh3d", mesh3d),
  ("mesh3d_textured", mesh3dTextured),
  ("textured_rect", texturedRect)
]

end Afferent.Shaders

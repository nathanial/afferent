/-
  Afferent FFI Initialization
  Module initialization that registers external classes and shader sources.
-/
import Afferent.Shaders

namespace Afferent.FFI

-- Low-level module initialization (registers external classes)
@[extern "afferent_initialize"]
opaque initClasses : IO Unit

-- Set a shader source by name (called during initialization)
@[extern "lean_afferent_set_shader_source"]
opaque setShaderSource (name : @& String) (source : @& String) : IO Unit

/-- Initialize all embedded shader sources.
    Called automatically by `init`. -/
def initShaders : IO Unit := do
  for (name, source) in Afferent.Shaders.all do
    setShaderSource name source

/-- Initialize Afferent FFI (registers external classes and embeds shaders).
    Must be called before creating a Renderer. -/
def init : IO Unit := do
  initClasses
  initShaders

end Afferent.FFI

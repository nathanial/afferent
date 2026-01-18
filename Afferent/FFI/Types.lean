/-
  Afferent FFI Types
  Opaque handle types for native resources using the NonemptyType pattern.
-/

namespace Afferent.FFI

-- Window handle (platform-specific: NSWindow on macOS)
opaque WindowPointed : NonemptyType
def Window : Type := WindowPointed.type
instance : Nonempty Window := WindowPointed.property

-- Renderer handle (GPU API-specific: Metal on macOS)
opaque RendererPointed : NonemptyType
def Renderer : Type := RendererPointed.type
instance : Nonempty Renderer := RendererPointed.property

-- GPU buffer handle
opaque BufferPointed : NonemptyType
def Buffer : Type := BufferPointed.type
instance : Nonempty Buffer := BufferPointed.property

-- Font handle (FreeType-based, cross-platform)
opaque FontPointed : NonemptyType
def Font : Type := FontPointed.type
instance : Nonempty Font := FontPointed.property

-- FloatBuffer: High-performance mutable float array for instance data
-- Lives in C memory, avoids Lean's copy-on-write array semantics
opaque FloatBufferPointed : NonemptyType
def FloatBuffer : Type := FloatBufferPointed.type
instance : Nonempty FloatBuffer := FloatBufferPointed.property

-- Texture handle (GPU texture)
opaque TexturePointed : NonemptyType
def Texture : Type := TexturePointed.type
instance : Nonempty Texture := TexturePointed.property

-- CachedMesh handle (GPU-resident tessellated polygon for instanced rendering)
opaque CachedMeshPointed : NonemptyType
def CachedMesh : Type := CachedMeshPointed.type
instance : Nonempty CachedMesh := CachedMeshPointed.property

end Afferent.FFI

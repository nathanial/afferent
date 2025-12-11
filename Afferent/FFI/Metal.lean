/-
  Afferent FFI bindings to native Metal rendering code.
  These are low-level bindings - higher level APIs will be built on top.
-/

namespace Afferent.FFI

-- Opaque handle types using NonemptyType pattern
opaque WindowPointed : NonemptyType
def Window : Type := WindowPointed.type
instance : Nonempty Window := WindowPointed.property

opaque RendererPointed : NonemptyType
def Renderer : Type := RendererPointed.type
instance : Nonempty Renderer := RendererPointed.property

opaque BufferPointed : NonemptyType
def Buffer : Type := BufferPointed.type
instance : Nonempty Buffer := BufferPointed.property

opaque FontPointed : NonemptyType
def Font : Type := FontPointed.type
instance : Nonempty Font := FontPointed.property

-- Module initialization (registers external classes)
@[extern "afferent_initialize"]
opaque init : IO Unit

-- Window management
@[extern "lean_afferent_window_create"]
opaque Window.create (width height : UInt32) (title : @& String) : IO Window

@[extern "lean_afferent_window_destroy"]
opaque Window.destroy (window : @& Window) : IO Unit

@[extern "lean_afferent_window_should_close"]
opaque Window.shouldClose (window : @& Window) : IO Bool

@[extern "lean_afferent_window_poll_events"]
opaque Window.pollEvents (window : @& Window) : IO Unit

-- Input handling
@[extern "lean_afferent_window_new_frame"]
opaque Window.newFrame (window : @& Window) : IO Unit

@[extern "lean_afferent_window_get_mouse_pos"]
opaque Window.getMousePos (window : @& Window) : IO (Float × Float)

@[extern "lean_afferent_window_mouse_down"]
opaque Window.mouseDown (window : @& Window) (button : UInt8) : IO Bool

@[extern "lean_afferent_window_mouse_pressed"]
opaque Window.mousePressed (window : @& Window) (button : UInt8) : IO Bool

@[extern "lean_afferent_window_mouse_released"]
opaque Window.mouseReleased (window : @& Window) (button : UInt8) : IO Bool

@[extern "lean_afferent_window_get_scroll"]
opaque Window.getScroll (window : @& Window) : IO (Float × Float)

@[extern "lean_afferent_window_get_text_input"]
opaque Window.getTextInput (window : @& Window) : IO String

-- Renderer management
@[extern "lean_afferent_renderer_create"]
opaque Renderer.create (window : @& Window) : IO Renderer

@[extern "lean_afferent_renderer_destroy"]
opaque Renderer.destroy (renderer : @& Renderer) : IO Unit

@[extern "lean_afferent_renderer_begin_frame"]
opaque Renderer.beginFrame (renderer : @& Renderer) (r g b a : Float) : IO Bool

@[extern "lean_afferent_renderer_end_frame"]
opaque Renderer.endFrame (renderer : @& Renderer) : IO Unit

-- Buffer management
-- Vertices: Array of Float, 6 per vertex (pos.x, pos.y, color.r, color.g, color.b, color.a)
@[extern "lean_afferent_buffer_create_vertex"]
opaque Buffer.createVertex (renderer : @& Renderer) (vertices : @& Array Float) : IO Buffer

-- Indices: Array of UInt32
@[extern "lean_afferent_buffer_create_index"]
opaque Buffer.createIndex (renderer : @& Renderer) (indices : @& Array UInt32) : IO Buffer

@[extern "lean_afferent_buffer_destroy"]
opaque Buffer.destroy (buffer : @& Buffer) : IO Unit

-- Drawing
@[extern "lean_afferent_renderer_draw_triangles"]
opaque Renderer.drawTriangles
  (renderer : @& Renderer)
  (vertexBuffer indexBuffer : @& Buffer)
  (indexCount : UInt32) : IO Unit

-- Font management
@[extern "lean_afferent_font_load"]
opaque Font.load (path : @& String) (size : UInt32) : IO Font

@[extern "lean_afferent_font_destroy"]
opaque Font.destroy (font : @& Font) : IO Unit

@[extern "lean_afferent_font_get_metrics"]
opaque Font.getMetrics (font : @& Font) : IO (Float × Float × Float)

-- Text rendering
@[extern "lean_afferent_text_measure"]
opaque Text.measure (font : @& Font) (text : @& String) : IO (Float × Float)

@[extern "lean_afferent_text_render"]
opaque Text.render
  (renderer : @& Renderer)
  (font : @& Font)
  (text : @& String)
  (x y : Float)
  (r g b a : Float) : IO Unit

end Afferent.FFI

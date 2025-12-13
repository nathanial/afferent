/-
  Afferent FFI Window
  Window management and input handling bindings.
-/
import Afferent.FFI.Types

namespace Afferent.FFI

-- Window management
@[extern "lean_afferent_window_create"]
opaque Window.create (width height : UInt32) (title : @& String) : IO Window

@[extern "lean_afferent_window_destroy"]
opaque Window.destroy (window : @& Window) : IO Unit

@[extern "lean_afferent_window_should_close"]
opaque Window.shouldClose (window : @& Window) : IO Bool

@[extern "lean_afferent_window_poll_events"]
opaque Window.pollEvents (window : @& Window) : IO Unit

@[extern "lean_afferent_window_get_size"]
opaque Window.getSize (window : @& Window) : IO (UInt32 × UInt32)

-- Keyboard input
@[extern "lean_afferent_window_get_key_code"]
opaque Window.getKeyCode (window : @& Window) : IO UInt16

@[extern "lean_afferent_window_clear_key"]
opaque Window.clearKey (window : @& Window) : IO Unit

-- Mouse input
@[extern "lean_afferent_window_get_mouse_pos"]
opaque Window.getMousePos (window : @& Window) : IO (Float × Float)

@[extern "lean_afferent_window_get_mouse_buttons"]
opaque Window.getMouseButtons (window : @& Window) : IO UInt8

@[extern "lean_afferent_window_get_modifiers"]
opaque Window.getModifiers (window : @& Window) : IO UInt16

@[extern "lean_afferent_window_get_scroll_delta"]
opaque Window.getScrollDelta (window : @& Window) : IO (Float × Float)

@[extern "lean_afferent_window_clear_scroll"]
opaque Window.clearScroll (window : @& Window) : IO Unit

@[extern "lean_afferent_window_mouse_in_window"]
opaque Window.mouseInWindow (window : @& Window) : IO Bool

/-- Click event data from native layer. -/
structure ClickEvent where
  button : UInt8      -- 0=left, 1=right, 2=middle
  x : Float
  y : Float
  modifiers : UInt16  -- shift=1, ctrl=2, alt=4, cmd=8
deriving Repr, Inhabited

@[extern "lean_afferent_window_get_click"]
opaque Window.getClick (window : @& Window) : IO (Option ClickEvent)

@[extern "lean_afferent_window_clear_click"]
opaque Window.clearClick (window : @& Window) : IO Unit

-- Get the main screen's backing scale factor (e.g., 2.0 for Retina, 1.5 for 150% scaling)
@[extern "lean_afferent_get_screen_scale"]
opaque getScreenScale : IO Float

end Afferent.FFI

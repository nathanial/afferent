/-
  Afferent FFI Window
  Window management and input handling bindings.
-/
import Afferent.FFI.Types

namespace Afferent.FFI

/-! ## Window Management -/

/-- Create a new window with the given dimensions and title.
    - `width`: Initial window width in pixels
    - `height`: Initial window height in pixels
    - `title`: Window title shown in title bar -/
@[extern "lean_afferent_window_create"]
opaque Window.create (width height : UInt32) (title : @& String) : IO Window

/-- Destroy a window and release its resources. -/
@[extern "lean_afferent_window_destroy"]
opaque Window.destroy (window : @& Window) : IO Unit

/-- Check if the window should close (user clicked close button). -/
@[extern "lean_afferent_window_should_close"]
opaque Window.shouldClose (window : @& Window) : IO Bool

/-- Process pending window events. Call once per frame. -/
@[extern "lean_afferent_window_poll_events"]
opaque Window.pollEvents (window : @& Window) : IO Unit

/-- Get the current window size as (width, height) in pixels. -/
@[extern "lean_afferent_window_get_size"]
opaque Window.getSize (window : @& Window) : IO (UInt32 × UInt32)

/-! ## Keyboard Input -/

/-- Get the key code of the most recent key press (0 if none). -/
@[extern "lean_afferent_window_get_key_code"]
opaque Window.getKeyCode (window : @& Window) : IO UInt16

/-- Check if a key press event is pending. -/
@[extern "lean_afferent_window_has_key_pressed"]
opaque Window.hasKeyPressed (window : @& Window) : IO Bool

/-- Clear the pending key press event. -/
@[extern "lean_afferent_window_clear_key"]
opaque Window.clearKey (window : @& Window) : IO Unit

/-! ## Mouse Input -/

/-- Get the current mouse position as (x, y) in window coordinates. -/
@[extern "lean_afferent_window_get_mouse_pos"]
opaque Window.getMousePos (window : @& Window) : IO (Float × Float)

/-- Get mouse button state as a bitmask (bit 0=left, 1=right, 2=middle). -/
@[extern "lean_afferent_window_get_mouse_buttons"]
opaque Window.getMouseButtons (window : @& Window) : IO UInt8

/-- Get keyboard modifier state (shift=1, ctrl=2, alt=4, cmd=8). -/
@[extern "lean_afferent_window_get_modifiers"]
opaque Window.getModifiers (window : @& Window) : IO UInt16

/-- Get scroll wheel delta as (deltaX, deltaY) since last clear. -/
@[extern "lean_afferent_window_get_scroll_delta"]
opaque Window.getScrollDelta (window : @& Window) : IO (Float × Float)

/-- Clear accumulated scroll delta. -/
@[extern "lean_afferent_window_clear_scroll"]
opaque Window.clearScroll (window : @& Window) : IO Unit

/-- Check if the mouse cursor is inside the window. -/
@[extern "lean_afferent_window_mouse_in_window"]
opaque Window.mouseInWindow (window : @& Window) : IO Bool

/-- Click event data from native layer. -/
structure ClickEvent where
  button : UInt8      -- 0=left, 1=right, 2=middle
  x : Float
  y : Float
  modifiers : UInt16  -- shift=1, ctrl=2, alt=4, cmd=8
deriving Repr, Inhabited

/-- Get the pending click event, if any. -/
@[extern "lean_afferent_window_get_click"]
opaque Window.getClick (window : @& Window) : IO (Option ClickEvent)

/-- Clear the pending click event. -/
@[extern "lean_afferent_window_clear_click"]
opaque Window.clearClick (window : @& Window) : IO Unit

/-! ## Pointer Lock (FPS Camera Controls) -/

/-- Enable or disable pointer lock for FPS-style mouse look.
    When locked, cursor is hidden and mouse reports relative movement. -/
@[extern "lean_afferent_window_set_pointer_lock"]
opaque Window.setPointerLock (window : @& Window) (locked : Bool) : IO Unit

/-- Check if pointer lock is currently enabled. -/
@[extern "lean_afferent_window_get_pointer_lock"]
opaque Window.getPointerLock (window : @& Window) : IO Bool

/-- Get mouse movement delta (useful for FPS camera).
    Returns (deltaX, deltaY) in pixels since last frame. -/
@[extern "lean_afferent_window_get_mouse_delta"]
opaque Window.getMouseDelta (window : @& Window) : IO (Float × Float)

/-! ## Continuous Key State -/

/-- Check if a specific key is currently held down.
    - `keyCode`: The key code to check (use macOS virtual key codes) -/
@[extern "lean_afferent_window_is_key_down"]
opaque Window.isKeyDown (window : @& Window) (keyCode : UInt16) : IO Bool

/-! ## Display -/

/-- Get the main screen's backing scale factor.
    Returns 2.0 for Retina displays, 1.0 for standard displays. -/
@[extern "lean_afferent_get_screen_scale"]
opaque getScreenScale : IO Float

end Afferent.FFI

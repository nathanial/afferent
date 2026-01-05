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

/-- Run the native event loop (blocks until stopped). -/
@[extern "lean_afferent_window_run_event_loop"]
opaque Window.runEventLoop (window : @& Window) : IO Unit

/-- Get the current window size as (width, height) in pixels. -/
@[extern "lean_afferent_window_get_size"]
opaque Window.getSize (window : @& Window) : IO (UInt32 × UInt32)

/-! ## Keyboard Input -/

/-! ### Key Constants

macOS virtual key codes for common keys.
Use with `Window.getKeyCode` and `Window.isKeyDown`. -/

namespace Key
  -- Letters (QWERTY layout)
  def a : UInt16 := 0
  def s : UInt16 := 1
  def d : UInt16 := 2
  def f : UInt16 := 3
  def h : UInt16 := 4
  def g : UInt16 := 5
  def z : UInt16 := 6
  def x : UInt16 := 7
  def c : UInt16 := 8
  def v : UInt16 := 9
  def b : UInt16 := 11
  def q : UInt16 := 12
  def w : UInt16 := 13
  def e : UInt16 := 14
  def r : UInt16 := 15
  def y : UInt16 := 16
  def t : UInt16 := 17
  def o : UInt16 := 31
  def u : UInt16 := 32
  def i : UInt16 := 34
  def p : UInt16 := 35
  def l : UInt16 := 37
  def j : UInt16 := 38
  def k : UInt16 := 40
  def n : UInt16 := 45
  def m : UInt16 := 46

  -- Numbers (top row)
  def num1 : UInt16 := 18
  def num2 : UInt16 := 19
  def num3 : UInt16 := 20
  def num4 : UInt16 := 21
  def num5 : UInt16 := 23
  def num6 : UInt16 := 22
  def num7 : UInt16 := 26
  def num8 : UInt16 := 28
  def num9 : UInt16 := 25
  def num0 : UInt16 := 29

  -- Special keys
  def «return» : UInt16 := 36
  def tab : UInt16 := 48
  def space : UInt16 := 49
  def delete : UInt16 := 51      -- Backspace
  def escape : UInt16 := 53
  def forwardDelete : UInt16 := 117

  -- Arrow keys
  def left : UInt16 := 123
  def right : UInt16 := 124
  def down : UInt16 := 125
  def up : UInt16 := 126

  -- Modifiers (left variants)
  def shift : UInt16 := 56
  def control : UInt16 := 59
  def option : UInt16 := 58
  def command : UInt16 := 55
  def capsLock : UInt16 := 57

  -- Function keys
  def f1 : UInt16 := 122
  def f2 : UInt16 := 120
  def f3 : UInt16 := 99
  def f4 : UInt16 := 118
  def f5 : UInt16 := 96
  def f6 : UInt16 := 97
  def f7 : UInt16 := 98
  def f8 : UInt16 := 100
  def f9 : UInt16 := 101
  def f10 : UInt16 := 109
  def f11 : UInt16 := 103
  def f12 : UInt16 := 111
end Key

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

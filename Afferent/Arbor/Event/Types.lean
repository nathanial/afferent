/-
  Arbor Widget Events
  Event types and handling for widget interactions.
-/
import Afferent.Arbor.Widget.Core

namespace Afferent.Arbor

/-- Modifier keys state. -/
structure Modifiers where
  shift : Bool := false
  ctrl : Bool := false
  alt : Bool := false    -- Option key on macOS
  cmd : Bool := false    -- Command key on macOS
deriving Repr, BEq, Inhabited

namespace Modifiers

def none : Modifiers := {}

def fromBitmask (bits : UInt16) : Modifiers :=
  { shift := bits &&& 1 != 0
    ctrl := bits &&& 2 != 0
    alt := bits &&& 4 != 0
    cmd := bits &&& 8 != 0 }

def toBitmask (m : Modifiers) : UInt16 :=
  (if m.shift then 1 else 0) |||
  (if m.ctrl then 2 else 0) |||
  (if m.alt then 4 else 0) |||
  (if m.cmd then 8 else 0)

def hasAny (m : Modifiers) : Bool :=
  m.shift || m.ctrl || m.alt || m.cmd

end Modifiers

/-- Mouse button identifier. -/
inductive MouseButton where
  | left
  | right
  | middle
deriving Repr, BEq, Inhabited

namespace MouseButton

def fromCode (code : UInt8) : MouseButton :=
  match code.toNat with
  | 0 => .left
  | 1 => .right
  | _ => .middle

def toCode (b : MouseButton) : UInt8 :=
  match b with
  | .left => 0
  | .right => 1
  | .middle => 2

end MouseButton

/-- Mouse event data. -/
structure MouseEvent where
  /-- Position in canvas coordinates (pixels, Y-down). -/
  x : Float
  y : Float
  /-- Mouse button (for click events). -/
  button : MouseButton := .left
  /-- Modifier keys held during event. -/
  modifiers : Modifiers := {}
  /-- Target widget ID (set during hit testing). -/
  targetId : Option WidgetId := none
deriving Repr, Inhabited

namespace MouseEvent

def mk' (x y : Float) (button : MouseButton := .left) (mods : Modifiers := {}) : MouseEvent :=
  { x, y, button, modifiers := mods }

def withTarget (e : MouseEvent) (id : WidgetId) : MouseEvent :=
  { e with targetId := some id }

def position (e : MouseEvent) : Point := ⟨e.x, e.y⟩

end MouseEvent

/-- Scroll event data. -/
structure ScrollEvent where
  /-- Mouse position during scroll. -/
  x : Float
  y : Float
  /-- Scroll delta (positive = scroll down/right on most systems). -/
  deltaX : Float
  deltaY : Float
  /-- Modifier keys held during scroll. -/
  modifiers : Modifiers := {}
  /-- Target widget ID. -/
  targetId : Option WidgetId := none
deriving Repr, Inhabited

namespace ScrollEvent

def withTarget (e : ScrollEvent) (id : WidgetId) : ScrollEvent :=
  { e with targetId := some id }

end ScrollEvent

/-- Keyboard key codes (subset of common keys). -/
inductive Key where
  | char (c : Char)        -- Regular character
  | space
  | enter
  | tab
  | escape
  | backspace
  | delete
  | up | down | left | right
  | home | «end» | pageUp | pageDown
  | f1 | f2 | f3 | f4 | f5 | f6 | f7 | f8 | f9 | f10 | f11 | f12
  | unknown (keyCode : UInt16)
deriving Repr, BEq, Inhabited

namespace Key

/-- Convert macOS virtual key code to Key. -/
def fromKeyCode (code : UInt16) : Key :=
  match code.toNat with
  | 49 => .space
  | 36 => .enter
  | 48 => .tab
  | 53 => .escape
  | 51 => .backspace
  | 117 => .delete
  | 126 => .up
  | 125 => .down
  | 123 => .left
  | 124 => .right
  | 115 => .home
  | 119 => .«end»
  | 116 => .pageUp
  | 121 => .pageDown
  | 122 => .f1
  | 120 => .f2
  | 99  => .f3
  | 118 => .f4
  | 96  => .f5
  | 97  => .f6
  | 98  => .f7
  | 100 => .f8
  | 101 => .f9
  | 109 => .f10
  | 103 => .f11
  | 111 => .f12
  -- Letter keys (macOS virtual key codes)
  | 0   => .char 'a'
  | 11  => .char 'b'
  | 8   => .char 'c'
  | 2   => .char 'd'
  | 14  => .char 'e'
  | 3   => .char 'f'
  | 5   => .char 'g'
  | 4   => .char 'h'
  | 34  => .char 'i'
  | 38  => .char 'j'
  | 40  => .char 'k'
  | 37  => .char 'l'
  | 46  => .char 'm'
  | 45  => .char 'n'
  | 31  => .char 'o'
  | 35  => .char 'p'
  | 12  => .char 'q'
  | 15  => .char 'r'
  | 1   => .char 's'
  | 17  => .char 't'
  | 32  => .char 'u'
  | 9   => .char 'v'
  | 13  => .char 'w'
  | 7   => .char 'x'
  | 16  => .char 'y'
  | 6   => .char 'z'
  -- Number keys
  | 29  => .char '0'
  | 18  => .char '1'
  | 19  => .char '2'
  | 20  => .char '3'
  | 21  => .char '4'
  | 23  => .char '5'
  | 22  => .char '6'
  | 26  => .char '7'
  | 28  => .char '8'
  | 25  => .char '9'
  -- Punctuation keys (US keyboard layout)
  | 27  => .char '-'   -- minus/underscore
  | 24  => .char '='   -- equals/plus
  | 33  => .char '['   -- left bracket/brace
  | 30  => .char ']'   -- right bracket/brace
  | 42  => .char '\\'  -- backslash/pipe
  | 41  => .char ';'   -- semicolon/colon
  | 39  => .char '\''  -- quote/double-quote
  | 43  => .char ','   -- comma/less-than
  | 47  => .char '.'   -- period/greater-than
  | 44  => .char '/'   -- slash/question
  | 50  => .char '`'   -- backtick/tilde
  | _ => .unknown code

/-- Map a character to its shifted variant (US keyboard layout). -/
def shiftChar (c : Char) : Char :=
  match c with
  -- Numbers to symbols
  | '1' => '!' | '2' => '@' | '3' => '#' | '4' => '$' | '5' => '%'
  | '6' => '^' | '7' => '&' | '8' => '*' | '9' => '(' | '0' => ')'
  -- Punctuation to shifted symbols
  | '-' => '_' | '=' => '+' | '[' => '{' | ']' => '}'
  | '\\' => '|' | ';' => ':' | '\'' => '"'
  | ',' => '<' | '.' => '>' | '/' => '?' | '`' => '~'
  -- Letters use toUpper
  | c => c.toUpper

end Key

/-- Keyboard event data. -/
structure KeyEvent where
  key : Key
  modifiers : Modifiers := {}
  /-- Whether this is a key press (true) or release (false). -/
  isPress : Bool := true
deriving Repr, Inhabited

/-- Unified event type for all widget events. -/
inductive Event where
  | mouseClick (e : MouseEvent)
  | mouseDown (e : MouseEvent)
  | mouseUp (e : MouseEvent)
  | mouseMove (e : MouseEvent)
  | mouseEnter (e : MouseEvent)
  | mouseLeave (e : MouseEvent)
  | scroll (e : ScrollEvent)
  | keyPress (e : KeyEvent)
  | keyRelease (e : KeyEvent)
deriving Repr

namespace Event

def targetId : Event → Option WidgetId
  | .mouseClick e => e.targetId
  | .mouseDown e => e.targetId
  | .mouseUp e => e.targetId
  | .mouseMove e => e.targetId
  | .mouseEnter e => e.targetId
  | .mouseLeave e => e.targetId
  | .scroll e => e.targetId
  | .keyPress _ => none
  | .keyRelease _ => none

/-- Events that should bubble up through the widget tree. -/
def shouldBubble : Event → Bool
  | .mouseClick _ => true
  | .mouseDown _ => true
  | .mouseUp _ => true
  | .scroll _ => true
  | .mouseMove _ => false  -- Too noisy to bubble
  | .mouseEnter _ => false -- Widget-specific
  | .mouseLeave _ => false -- Widget-specific
  | .keyPress _ => false   -- Goes to focused widget
  | .keyRelease _ => false

end Event

end Afferent.Arbor

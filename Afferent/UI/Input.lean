/-
  Afferent UI Input
  Input state types and querying for immediate-mode UI.
-/
import Afferent.Core.Types
import Afferent.FFI.Metal

namespace Afferent.UI

/-- Mouse button identifiers. -/
inductive MouseButton where
  | left
  | right
  | middle
deriving Repr, BEq, Inhabited

namespace MouseButton

def toUInt8 : MouseButton → UInt8
  | .left => 0
  | .right => 1
  | .middle => 2

end MouseButton

/-- Per-frame input state snapshot. -/
structure InputState where
  /-- Current mouse position in window coordinates (top-left origin). -/
  mouseX : Float
  mouseY : Float
  /-- Mouse buttons currently held down. -/
  mouseDownLeft : Bool
  mouseDownRight : Bool
  mouseDownMiddle : Bool
  /-- Mouse buttons pressed this frame (single-frame signal). -/
  mousePressedLeft : Bool
  mousePressedRight : Bool
  mousePressedMiddle : Bool
  /-- Mouse buttons released this frame (single-frame signal). -/
  mouseReleasedLeft : Bool
  mouseReleasedRight : Bool
  mouseReleasedMiddle : Bool
  /-- Scroll wheel delta this frame. -/
  scrollX : Float
  scrollY : Float
  /-- Text input this frame (typed characters). -/
  textInput : String
deriving Repr, Inhabited

namespace InputState

/-- Create an empty input state. -/
def empty : InputState := {
  mouseX := 0.0, mouseY := 0.0
  mouseDownLeft := false, mouseDownRight := false, mouseDownMiddle := false
  mousePressedLeft := false, mousePressedRight := false, mousePressedMiddle := false
  mouseReleasedLeft := false, mouseReleasedRight := false, mouseReleasedMiddle := false
  scrollX := 0.0, scrollY := 0.0
  textInput := ""
}

/-- Get mouse position as a Point. -/
def mousePos (s : InputState) : Point := ⟨s.mouseX, s.mouseY⟩

/-- Check if a mouse button is currently held down. -/
def mouseDown (s : InputState) (button : MouseButton) : Bool :=
  match button with
  | .left => s.mouseDownLeft
  | .right => s.mouseDownRight
  | .middle => s.mouseDownMiddle

/-- Check if a mouse button was pressed this frame. -/
def mousePressed (s : InputState) (button : MouseButton) : Bool :=
  match button with
  | .left => s.mousePressedLeft
  | .right => s.mousePressedRight
  | .middle => s.mousePressedMiddle

/-- Check if a mouse button was released this frame. -/
def mouseReleased (s : InputState) (button : MouseButton) : Bool :=
  match button with
  | .left => s.mouseReleasedLeft
  | .right => s.mouseReleasedRight
  | .middle => s.mouseReleasedMiddle

/-- Check if mouse is over a rectangle. -/
def isMouseOver (s : InputState) (rect : Rect) : Bool :=
  rect.contains s.mousePos

/-- Query input state from the window. Call once per frame after pollEvents. -/
def query (window : FFI.Window) : IO InputState := do
  let (mx, my) ← window.getMousePos
  let leftDown ← window.mouseDown 0
  let rightDown ← window.mouseDown 1
  let middleDown ← window.mouseDown 2
  let leftPressed ← window.mousePressed 0
  let rightPressed ← window.mousePressed 1
  let middlePressed ← window.mousePressed 2
  let leftReleased ← window.mouseReleased 0
  let rightReleased ← window.mouseReleased 1
  let middleReleased ← window.mouseReleased 2
  let (sx, sy) ← window.getScroll
  let text ← window.getTextInput
  pure {
    mouseX := mx, mouseY := my
    mouseDownLeft := leftDown, mouseDownRight := rightDown, mouseDownMiddle := middleDown
    mousePressedLeft := leftPressed, mousePressedRight := rightPressed, mousePressedMiddle := middlePressed
    mouseReleasedLeft := leftReleased, mouseReleasedRight := rightReleased, mouseReleasedMiddle := middleReleased
    scrollX := sx, scrollY := sy
    textInput := text
  }

end InputState

end Afferent.UI

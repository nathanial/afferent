/-
  Afferent UI Context
  Core state management for immediate-mode UI.
-/
import Afferent.Core.Types
import Afferent.Canvas.Context
import Afferent.Text.Font
import Afferent.UI.Input

namespace Afferent.UI

/-- Hardcoded default style values. -/
def Style.buttonBg : Color := Color.rgba 0.3 0.3 0.35 1.0
def Style.buttonHover : Color := Color.rgba 0.4 0.4 0.45 1.0
def Style.buttonActive : Color := Color.rgba 0.2 0.2 0.25 1.0
def Style.buttonText : Color := Color.white

def Style.checkboxBg : Color := Color.rgba 0.2 0.2 0.25 1.0
def Style.checkboxBorder : Color := Color.rgba 0.5 0.5 0.55 1.0
def Style.checkboxCheck : Color := Color.rgba 0.3 0.7 0.3 1.0

def Style.textBoxBg : Color := Color.rgba 0.15 0.15 0.18 1.0
def Style.textBoxBorder : Color := Color.rgba 0.4 0.4 0.45 1.0
def Style.textBoxFocused : Color := Color.rgba 0.3 0.5 0.7 1.0
def Style.textBoxText : Color := Color.white
def Style.textBoxCursor : Color := Color.white

def Style.sliderTrack : Color := Color.rgba 0.2 0.2 0.25 1.0
def Style.sliderFill : Color := Color.rgba 0.3 0.5 0.7 1.0
def Style.sliderHandle : Color := Color.rgba 0.5 0.5 0.55 1.0

def Style.labelText : Color := Color.white

def Style.padding : Float := 8.0
def Style.cornerRadius : Float := 4.0
def Style.borderWidth : Float := 1.0

/-- Widget ID for tracking interaction state across frames. -/
abbrev WidgetId := String

/-- The UI context holds all state needed for immediate-mode widgets. -/
structure UIContext where
  /-- The drawing context for rendering. -/
  drawCtx : DrawContext
  /-- Current input state for this frame. -/
  input : InputState
  /-- Font for widget text. -/
  font : Font
  /-- Currently "hot" widget (mouse is over, will respond to click). -/
  hotWidget : Option WidgetId
  /-- Currently "active" widget (being clicked/focused). -/
  activeWidget : Option WidgetId
  /-- Text being edited in the active text box. -/
  textEditBuffer : String
  /-- Cursor position in text edit buffer. -/
  textEditCursor : Nat
  /-- Frame counter (for cursor blink, animations). -/
  frameCount : Nat

namespace UIContext

/-- Create a new UI context for a frame. -/
def create (drawCtx : DrawContext) (input : InputState) (font : Font) : UIContext :=
  { drawCtx, input, font
    hotWidget := none
    activeWidget := none
    textEditBuffer := ""
    textEditCursor := 0
    frameCount := 0 }

/-- Advance to a new frame, preserving persistent state (activeWidget, textEditBuffer). -/
def beginFrame (ctx : UIContext) (newInput : InputState) : UIContext :=
  { ctx with
    input := newInput
    hotWidget := none  -- Reset hot each frame; widgets will re-claim
    frameCount := ctx.frameCount + 1 }

/-- Set a widget as hot (hovered). -/
def setHot (ctx : UIContext) (id : WidgetId) : UIContext :=
  { ctx with hotWidget := some id }

/-- Set a widget as active (clicked/focused). -/
def setActive (ctx : UIContext) (id : WidgetId) : UIContext :=
  { ctx with activeWidget := some id }

/-- Clear the active widget. -/
def clearActive (ctx : UIContext) : UIContext :=
  { ctx with activeWidget := none }

/-- Check if a widget is the active one. -/
def isActive (ctx : UIContext) (id : WidgetId) : Bool :=
  ctx.activeWidget == some id

/-- Check if a widget is hot (hovered). -/
def isHot (ctx : UIContext) (id : WidgetId) : Bool :=
  ctx.hotWidget == some id

/-- Update text edit buffer (for active text box). -/
def setTextEdit (ctx : UIContext) (text : String) (cursor : Nat) : UIContext :=
  { ctx with textEditBuffer := text, textEditCursor := cursor }

end UIContext

end Afferent.UI

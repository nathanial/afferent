/-
  Canopy Reactive - Event Data Types
  Structures for passing FFI events into the reactive network with layout context.
-/
import Std.Data.HashMap
import Afferent.FFI
import Afferent.Arbor
import Trellis

namespace Afferent.Canopy.Reactive

/-- Click event with layout context for hit-testing in reactive handlers. -/
structure ClickData where
  /-- The raw click event from FFI. -/
  click : Afferent.FFI.ClickEvent
  /-- Path from root to clicked widget (for bubbling/filtering). -/
  hitPath : Array Afferent.Arbor.WidgetId
  /-- The root widget tree (for name-based lookups). -/
  widget : Afferent.Arbor.Widget
  /-- Computed layouts for all widgets (for position-based calculations). -/
  layouts : Trellis.LayoutResult
  /-- Optional name->id map for fast lookups (defaults to empty). -/
  nameMap : Std.HashMap String Afferent.Arbor.WidgetId := {}

/-- Hover event with position and layout context. -/
structure HoverData where
  /-- Mouse X position. -/
  x : Float
  /-- Mouse Y position. -/
  y : Float
  /-- Path from root to hovered widget. -/
  hitPath : Array Afferent.Arbor.WidgetId
  /-- The root widget tree. -/
  widget : Afferent.Arbor.Widget
  /-- Computed layouts. -/
  layouts : Trellis.LayoutResult
  /-- Optional name->id map for fast lookups (defaults to empty). -/
  nameMap : Std.HashMap String Afferent.Arbor.WidgetId := {}

/-- Mouse delta event (relative movement since last frame). -/
structure MouseDeltaData where
  dx : Float
  dy : Float
deriving Repr, Inhabited

/-- Key event wrapper with focus context. -/
structure KeyData where
  /-- The keyboard event. -/
  event : Afferent.Arbor.KeyEvent
  /-- Currently focused widget name (for routing). -/
  focusedWidget : Option String

/-- Scroll event with layout context for reactive handlers. -/
structure ScrollData where
  /-- The raw scroll event from Arbor. -/
  scroll : Afferent.Arbor.ScrollEvent
  /-- Path from root to widget under mouse during scroll. -/
  hitPath : Array Afferent.Arbor.WidgetId
  /-- The root widget tree. -/
  widget : Afferent.Arbor.Widget
  /-- Computed layouts. -/
  layouts : Trellis.LayoutResult
  /-- Optional name->id map for fast lookups (defaults to empty). -/
  nameMap : Std.HashMap String Afferent.Arbor.WidgetId := {}

/-- Mouse button event with layout context. -/
structure MouseButtonData where
  /-- Mouse X position. -/
  x : Float
  /-- Mouse Y position. -/
  y : Float
  /-- Mouse button (0=left, 1=right, 2=middle). -/
  button : UInt8
  /-- Path from root to widget under mouse. -/
  hitPath : Array Afferent.Arbor.WidgetId
  /-- The root widget tree. -/
  widget : Afferent.Arbor.Widget
  /-- Computed layouts. -/
  layouts : Trellis.LayoutResult
  /-- Optional name->id map for fast lookups (defaults to empty). -/
  nameMap : Std.HashMap String Afferent.Arbor.WidgetId := {}

end Afferent.Canopy.Reactive

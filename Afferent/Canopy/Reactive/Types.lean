/-
  Canopy Reactive - Event Data Types
  Structures for passing FFI events into the reactive network with layout context.
-/
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

/-- Key event wrapper with focus context. -/
structure KeyData where
  /-- The keyboard event. -/
  event : Afferent.Arbor.KeyEvent
  /-- Currently focused widget name (for routing). -/
  focusedWidget : Option String

end Afferent.Canopy.Reactive

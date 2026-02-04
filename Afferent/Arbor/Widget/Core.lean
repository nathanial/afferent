/-
  Arbor Widget Core Types
  Declarative widget system foundation.
-/
import Afferent.Arbor.Core.Types
import Afferent.Arbor.Render.Command
import Trellis
import Afferent.Canvas.Context

namespace Afferent.Arbor

/-- A single line of wrapped text with metrics. -/
structure TextLine where
  text : String
  width : Float
deriving Repr, BEq, Inhabited

/-- Result of text wrapping computation. -/
structure TextLayout where
  lines : Array TextLine
  totalHeight : Float
  maxWidth : Float
  lineHeight : Float := 16  -- Line height for advancing between lines
  ascender : Float := 12    -- Distance from baseline to top of text (for vertical positioning)
deriving Repr, BEq, Inhabited

namespace TextLayout

def empty : TextLayout := ⟨#[], 0, 0, 16, 12⟩

def singleLine (text : String) (width height : Float) : TextLayout :=
  ⟨#[⟨text, width⟩], height, width, height, height * 0.8⟩

end TextLayout

/-- Scroll position state. -/
structure ScrollState where
  offsetX : Float := 0
  offsetY : Float := 0
deriving Repr, BEq, Inhabited

namespace ScrollState

def zero : ScrollState := {}

end ScrollState

/-- Scrollbar rendering configuration (passed to render layer). -/
structure ScrollbarRenderConfig where
  /-- Show vertical scrollbar. -/
  showVertical : Bool := true
  /-- Show horizontal scrollbar. -/
  showHorizontal : Bool := false
  /-- Scrollbar track thickness in pixels. -/
  thickness : Float := 8.0
  /-- Minimum thumb length in pixels. -/
  minThumbLength : Float := 30.0
  /-- Corner radius for scrollbar elements. -/
  cornerRadius : Float := 4.0
  /-- Track background color. -/
  trackColor : Color := ⟨0.15, 0.15, 0.15, 1.0⟩
  /-- Thumb color. -/
  thumbColor : Color := ⟨0.35, 0.35, 0.35, 1.0⟩
deriving Repr, BEq, Inhabited

namespace ScrollbarRenderConfig

def hidden : ScrollbarRenderConfig :=
  { showVertical := false, showHorizontal := false }

end ScrollbarRenderConfig

/-- Widget identifier for layout-to-widget mapping. -/
abbrev WidgetId := Nat

inductive RenderLayer where
  | normal
  | overlay
deriving Repr, BEq, Inhabited

/-- Visual styling for widget boxes. -/
structure BoxStyle where
  backgroundColor : Option Color := none
  borderColor : Option Color := none
  borderWidth : Float := 0
  cornerRadius : Float := 0
  padding : Trellis.EdgeInsets := {}
  margin : Trellis.EdgeInsets := {}
  width : Trellis.Dimension := .auto    -- explicit width (auto, length, or percent)
  height : Trellis.Dimension := .auto   -- explicit height (auto, length, or percent)
  minWidth : Option Float := none
  maxWidth : Option Float := none
  minHeight : Option Float := none
  maxHeight : Option Float := none
  position : Trellis.Position := .static
  top : Option Float := none
  right : Option Float := none
  bottom : Option Float := none
  left : Option Float := none
  /-- Render layer: normal content or deferred overlay. -/
  layer : RenderLayer := .normal
  /-- Flex item properties (grow, shrink, basis, alignSelf) when this widget is a flex child -/
  flexItem : Option Trellis.FlexItem := none
  /-- Grid item properties when this widget is a grid child. -/
  gridItem : Option Trellis.GridItem := none
deriving Repr, BEq, Inhabited

namespace BoxStyle

def default : BoxStyle := {}

/-- Create a box style with background color. -/
def withBackground (color : Color) : BoxStyle :=
  { backgroundColor := some color }

/-- Create a box style with uniform padding. -/
def withPadding (p : Float) : BoxStyle :=
  { padding := Trellis.EdgeInsets.uniform p }

/-- Create a box style with background and padding. -/
def card (bg : Color) (p : Float) : BoxStyle :=
  { backgroundColor := some bg, padding := Trellis.EdgeInsets.uniform p }

/-- Style that grows to fill available space. -/
def grow (factor : Float := 1) : BoxStyle :=
  { flexItem := some (Trellis.FlexItem.growing factor) }

/-- Full width (100%). -/
def fullWidth : BoxStyle := { width := .percent 1.0 }

/-- Full height (100%). -/
def fullHeight : BoxStyle := { height := .percent 1.0 }

/-- Full size (100% x 100%). -/
def fill : BoxStyle := { width := .percent 1.0, height := .percent 1.0 }

/-- Growing and full height (common pattern). -/
def growFill : BoxStyle :=
  { flexItem := some (Trellis.FlexItem.growing 1), height := .percent 1.0 }

end BoxStyle

/-- Custom widget specification.
    Provides measurement and render command collection. -/
structure CustomSpec where
  /-- Measure intrinsic content size given available width/height. -/
  measure : Float → Float → (Float × Float)
  /-- Collect render commands given computed layout. -/
  collect : Trellis.ComputedLayout → RenderCommands
  /-- Optional CanvasM draw hook for backend-specific rendering. -/
  draw : Option (Trellis.ComputedLayout → Afferent.CanvasM Unit) := none
  /-- Optional custom hit test (true if point is inside widget). -/
  hitTest : Option (Trellis.ComputedLayout → Point → Bool) := none
  /-- Cache generation number. Widgets with different generations are not cached together.
      This is automatically set by dynWidget to invalidate cache on rebuild. -/
  generation : Nat := 0
  /-- Skip render cache entirely. Use for widgets that change every frame (e.g., spinners)
      where caching adds overhead without benefit. -/
  skipCache : Bool := false

namespace CustomSpec

def default : CustomSpec :=
  { measure := fun _ _ => (0, 0)
    collect := fun _ => #[]
    draw := none
    hitTest := none }

end CustomSpec

instance : Inhabited CustomSpec := ⟨CustomSpec.default⟩

/-- Core widget type - declarative, display-only.
    Uses FontId instead of concrete Font type for renderer independence.
    Each widget has an optional `name` for debug/semantic identification. -/
inductive Widget where
  /-- Flexbox container -/
  | flex (id : WidgetId)
         (name : Option String := none)
         (props : Trellis.FlexContainer)
         (style : BoxStyle)
         (children : Array Widget)

  /-- CSS Grid container -/
  | grid (id : WidgetId)
         (name : Option String := none)
         (props : Trellis.GridContainer)
         (style : BoxStyle)
         (children : Array Widget)

  /-- Text with optional wrapping -/
  | text (id : WidgetId)
         (name : Option String := none)
         (content : String)
         (font : FontId)
         (color : Color)
         (align : TextAlign)
         (maxWidth : Option Float)
         (textLayout : Option TextLayout)

  /-- A colored rectangle box -/
  | rect (id : WidgetId)
         (name : Option String := none)
         (style : BoxStyle)

  /-- Scroll container with clipping -/
  | scroll (id : WidgetId)
           (name : Option String := none)
           (style : BoxStyle)
           (scrollState : ScrollState)
           (contentWidth : Float)
           (contentHeight : Float)
           (scrollbarConfig : ScrollbarRenderConfig)
           (child : Widget)

  /-- Fixed-size spacer -/
  | spacer (id : WidgetId)
           (name : Option String := none)
           (width : Float)
           (height : Float)

  /-- Custom widget with user-provided measurement and rendering. -/
  | custom (id : WidgetId)
           (name : Option String := none)
           (style : BoxStyle)
           (spec : CustomSpec)

deriving Inhabited

namespace Widget

/-- Get the widget's unique identifier. -/
def id : Widget → WidgetId
  | .flex id .. => id
  | .grid id .. => id
  | .text id .. => id
  | .rect id .. => id
  | .scroll id .. => id
  | .spacer id .. => id
  | .custom id .. => id

/-- Get the widget's optional name for debug identification. -/
def name? : Widget → Option String
  | .flex _ name .. => name
  | .grid _ name .. => name
  | .text _ name .. => name
  | .rect _ name .. => name
  | .scroll _ name .. => name
  | .spacer _ name .. => name
  | .custom _ name .. => name

/-- Get the widget's children (empty for leaf widgets). -/
def children : Widget → Array Widget
  | .flex _ _ _ _ children => children
  | .grid _ _ _ _ children => children
  | .scroll _ _ _ _ _ _ _ child => #[child]
  | _ => #[]

/-- Get the widget's style if it has one. -/
def style? : Widget → Option BoxStyle
  | .flex _ _ _ style _ => some style
  | .grid _ _ _ style _ => some style
  | .rect _ _ style => some style
  | .scroll _ _ style .. => some style
  | .custom _ _ style _ => some style
  | _ => none

/-- Check if this widget is a container. -/
def isContainer : Widget → Bool
  | .flex .. | .grid .. | .scroll .. => true
  | _ => false

/-- Check if this widget is a leaf (no children). -/
def isLeaf (w : Widget) : Bool := !w.isContainer

/-- Count total widgets in tree. -/
partial def widgetCount (w : Widget) : Nat :=
  1 + w.children.foldl (fun acc child => acc + child.widgetCount) 0

/-- Get all widget IDs in tree. -/
partial def allIds (w : Widget) : Array WidgetId :=
  #[w.id] ++ w.children.flatMap allIds

end Widget

end Afferent.Arbor

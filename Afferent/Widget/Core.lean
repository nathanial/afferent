/-
  Afferent Widget Core Types
  Declarative widget system foundation.
-/
import Afferent.Core.Types
import Afferent.Layout
import Afferent.Text.Font
import Trellis

namespace Afferent.Widget

/-- Text horizontal alignment within its bounds. -/
inductive TextAlign where
  | left
  | center
  | right
deriving Repr, BEq, Inhabited

/-- Text vertical alignment within its bounds. -/
inductive TextVAlign where
  | top
  | center
  | bottom
deriving Repr, BEq, Inhabited

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
deriving Repr, BEq, Inhabited

namespace TextLayout

def empty : TextLayout := ⟨#[], 0, 0⟩

def singleLine (text : String) (width height : Float) : TextLayout :=
  ⟨#[⟨text, width⟩], height, width⟩

end TextLayout

/-- Scroll position state. -/
structure ScrollState where
  offsetX : Float := 0
  offsetY : Float := 0
deriving Repr, BEq, Inhabited

namespace ScrollState

def zero : ScrollState := {}

end ScrollState

/-- Widget identifier for layout-to-widget mapping. -/
abbrev WidgetId := Nat

/-- Visual styling for widget boxes. -/
structure BoxStyle where
  backgroundColor : Option Color := none
  borderColor : Option Color := none
  borderWidth : Float := 0
  cornerRadius : Float := 0
  padding : Trellis.EdgeInsets := {}
  margin : Trellis.EdgeInsets := {}
  minWidth : Option Float := none
  maxWidth : Option Float := none
  minHeight : Option Float := none
  maxHeight : Option Float := none
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

end BoxStyle

/-- Core widget type - declarative, display-only. -/
inductive Widget where
  /-- Flexbox container -/
  | flex (id : WidgetId)
         (props : Trellis.FlexContainer)
         (style : BoxStyle)
         (children : Array Widget)

  /-- CSS Grid container -/
  | grid (id : WidgetId)
         (props : Trellis.GridContainer)
         (style : BoxStyle)
         (children : Array Widget)

  /-- Text with optional wrapping -/
  | text (id : WidgetId)
         (content : String)
         (font : Font)
         (color : Color)
         (align : TextAlign)
         (maxWidth : Option Float)
         (textLayout : Option TextLayout)

  /-- A colored rectangle box -/
  | rect (id : WidgetId)
         (style : BoxStyle)

  /-- Scroll container with clipping -/
  | scroll (id : WidgetId)
           (style : BoxStyle)
           (scrollState : ScrollState)
           (contentWidth : Float)
           (contentHeight : Float)
           (child : Widget)

  /-- Fixed-size spacer -/
  | spacer (id : WidgetId)
           (width : Float)
           (height : Float)

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

/-- Get the widget's children (empty for leaf widgets). -/
def children : Widget → Array Widget
  | .flex _ _ _ children => children
  | .grid _ _ _ children => children
  | .scroll _ _ _ _ _ child => #[child]
  | _ => #[]

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

end Afferent.Widget

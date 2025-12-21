/-
  Afferent Widget DSL
  Declarative builder functions for creating widget trees.
-/
import Afferent.Widget.Core
import Afferent.Widget.Scroll
import Trellis


namespace Afferent.Widget

/-- Builder state for generating unique widget IDs. -/
structure BuilderState where
  nextId : Nat := 0
deriving Repr, Inhabited

/-- Widget builder monad for automatic ID generation. -/
abbrev WidgetBuilder := StateM BuilderState Widget

/-- Generate a fresh widget ID. -/
def freshId : StateM BuilderState WidgetId := do
  let s ← get
  set { s with nextId := s.nextId + 1 }
  pure s.nextId

/-! ## Text Widgets -/

/-- Create a text widget with optional wrapping. -/
def text' (content : String) (font : Font) (color : Color := Color.white)
    (align : TextAlign := .left) (maxWidth : Option Float := none) : WidgetBuilder := do
  let wid ← freshId
  pure (.text wid content font color align maxWidth none)

/-- Create a text widget that wraps at the given width. -/
def wrappedText (content : String) (font : Font) (maxWidth : Float)
    (color : Color := Color.white) (align : TextAlign := .left) : WidgetBuilder := do
  let wid ← freshId
  pure (.text wid content font color align (some maxWidth) none)

/-- Create a centered text widget. -/
def centeredText (content : String) (font : Font) (color : Color := Color.white) : WidgetBuilder :=
  text' content font color .center

/-! ## Box Widgets -/

/-- Create a colored rectangle box. -/
def box (style : BoxStyle) : WidgetBuilder := do
  let wid ← freshId
  pure (.rect wid style)

/-- Create a simple colored box with dimensions. -/
def coloredBox (color : Color) (width height : Float) : WidgetBuilder := do
  let wid ← freshId
  pure (.rect wid { backgroundColor := some color, minWidth := some width, minHeight := some height })

/-- Create a spacer with fixed dimensions. -/
def spacer (width height : Float) : WidgetBuilder := do
  let wid ← freshId
  pure (.spacer wid width height)

/-- Create a horizontal spacer (for row layouts). -/
def hspacer (width : Float) : WidgetBuilder := spacer width 0

/-- Create a vertical spacer (for column layouts). -/
def vspacer (height : Float) : WidgetBuilder := spacer 0 height

/-! ## Flex Container Widgets -/

/-- Create a horizontal flex row. -/
def row (gap : Float := 0) (style : BoxStyle := {}) (children : Array WidgetBuilder) : WidgetBuilder := do
  let wid ← freshId
  let props := Trellis.FlexContainer.row gap
  let cs ← children.mapM fun b => b
  pure (.flex wid props style cs)

/-- Create a vertical flex column. -/
def column (gap : Float := 0) (style : BoxStyle := {}) (children : Array WidgetBuilder) : WidgetBuilder := do
  let wid ← freshId
  let props := Trellis.FlexContainer.column gap
  let cs ← children.mapM fun b => b
  pure (.flex wid props style cs)

/-- Create a row with custom flex properties. -/
def flexRow (props : Trellis.FlexContainer) (style : BoxStyle := {}) (children : Array WidgetBuilder) : WidgetBuilder := do
  let wid ← freshId
  let cs ← children.mapM fun b => b
  pure (.flex wid { props with direction := .row } style cs)

/-- Create a column with custom flex properties. -/
def flexColumn (props : Trellis.FlexContainer) (style : BoxStyle := {}) (children : Array WidgetBuilder) : WidgetBuilder := do
  let wid ← freshId
  let cs ← children.mapM fun b => b
  pure (.flex wid { props with direction := .column } style cs)

/-- Create a centered container (centers single child). -/
def center (style : BoxStyle := {}) (child : WidgetBuilder) : WidgetBuilder := do
  let wid ← freshId
  let props := Trellis.FlexContainer.centered
  let c ← child
  pure (.flex wid props style #[c])

/-- Create a container with space-between alignment. -/
def spaceBetween (direction : Trellis.FlexDirection := .row) (style : BoxStyle := {})
    (children : Array WidgetBuilder) : WidgetBuilder := do
  let wid ← freshId
  let props := { Trellis.FlexContainer.default with direction, justifyContent := .spaceBetween }
  let cs ← children.mapM fun b => b
  pure (.flex wid props style cs)

/-- Create a container with space-around alignment. -/
def spaceAround (direction : Trellis.FlexDirection := .row) (style : BoxStyle := {})
    (children : Array WidgetBuilder) : WidgetBuilder := do
  let wid ← freshId
  let props := { Trellis.FlexContainer.default with direction, justifyContent := .spaceAround }
  let cs ← children.mapM fun b => b
  pure (.flex wid props style cs)

/-! ## Grid Container Widgets -/

/-- Create a grid with n equal columns. -/
def grid (columns : Nat) (gap : Float := 0) (style : BoxStyle := {})
    (children : Array WidgetBuilder) : WidgetBuilder := do
  let wid ← freshId
  let props := Trellis.GridContainer.columns columns gap
  let cs ← children.mapM fun b => b
  pure (.grid wid props style cs)

/-- Create a grid with custom properties. -/
def gridCustom (props : Trellis.GridContainer) (style : BoxStyle := {})
    (children : Array WidgetBuilder) : WidgetBuilder := do
  let wid ← freshId
  let cs ← children.mapM fun b => b
  pure (.grid wid props style cs)

/-! ## Scroll Widgets -/

/-- Create a scroll container. -/
def scroll (style : BoxStyle := {}) (contentWidth contentHeight : Float)
    (scrollState : ScrollState := {}) (child : WidgetBuilder) : WidgetBuilder := do
  let wid ← freshId
  let c ← child
  pure (.scroll wid style scrollState contentWidth contentHeight c)

/-- Create a vertical scroll container (scrolls only vertically). -/
def vscroll (style : BoxStyle := {}) (contentHeight : Float)
    (scrollState : ScrollState := {}) (child : WidgetBuilder) : WidgetBuilder := do
  -- Content width = viewport width (no horizontal scrolling)
  let contentWidth := style.minWidth.getD 0
  scroll style contentWidth contentHeight scrollState child

/-! ## Convenience Combinators -/

/-- Create a padded container around a child. -/
def padded (padding : Float) (child : WidgetBuilder) : WidgetBuilder := do
  let wid ← freshId
  let props := Trellis.FlexContainer.default
  let style := { padding := Trellis.EdgeInsets.uniform padding }
  let c ← child
  pure (.flex wid props style #[c])

/-- Create a container with margin around a child. -/
def marginBox (margin : Float) (child : WidgetBuilder) : WidgetBuilder := do
  let wid ← freshId
  let props := Trellis.FlexContainer.default
  let style := { margin := Trellis.EdgeInsets.uniform margin }
  let c ← child
  pure (.flex wid props style #[c])

/-- Create a card (box with background and padding). -/
def card (bg : Color) (padding : Float := 16) (child : WidgetBuilder) : WidgetBuilder := do
  let wid ← freshId
  let props := Trellis.FlexContainer.default
  let style := BoxStyle.card bg padding
  let c ← child
  pure (.flex wid props style #[c])

/-- Create a container with border. -/
def bordered (borderColor : Color) (borderWidth : Float := 1) (child : WidgetBuilder) : WidgetBuilder := do
  let wid ← freshId
  let props := Trellis.FlexContainer.default
  let style := { borderColor := some borderColor, borderWidth }
  let c ← child
  pure (.flex wid props style #[c])

/-! ## Building -/

/-- Build a widget tree from a builder, starting IDs from 0. -/
def build (builder : WidgetBuilder) : Widget :=
  (builder.run {}).1

/-- Build a widget tree starting from a specific ID. -/
def buildFrom (startId : Nat) (builder : WidgetBuilder) : Widget :=
  (builder.run { nextId := startId }).1

/-- Run a builder and get both the widget and final state. -/
def buildWithState (builder : WidgetBuilder) : Widget × BuilderState :=
  builder.run {}

end Afferent.Widget

/-
  Arbor Widget DSL
  Declarative builder functions for creating widget trees.
-/
import Afferent.Arbor.Widget.Core
import Trellis

namespace Afferent.Arbor

/-- Builder state for generating unique widget IDs. -/
structure BuilderState where
  nextId : Nat := 0
  /-- Cache generation counter. CustomSpec widgets built with different generations
      will not share cache entries. Incremented by dynWidget on rebuild. -/
  cacheGeneration : Nat := 0
deriving Repr, Inhabited

/-- Widget builder monad for automatic ID generation. -/
abbrev WidgetBuilder := StateM BuilderState Widget

/-- Generate a fresh widget ID. -/
def freshId : StateM BuilderState WidgetId := do
  let s ← get
  set { s with nextId := s.nextId + 1 }
  pure s.nextId

/-- Get the current cache generation. -/
def getCacheGeneration : StateM BuilderState Nat := do
  let s ← get
  pure s.cacheGeneration

/-- Increment the cache generation counter. Call this when rebuilding a dynamic widget subtree
    to invalidate cached render commands for widgets in that subtree. -/
def incrementCacheGeneration : StateM BuilderState Unit := do
  modify fun s => { s with cacheGeneration := s.cacheGeneration + 1 }

/-! ## Text Widgets -/

/-- Create a text widget with optional wrapping. -/
def text' (content : String) (font : FontId) (color : Color := Tincture.Color.white)
    (align : TextAlign := .left) (maxWidth : Option Float := none) : WidgetBuilder := do
  let wid ← freshId
  pure (.text wid none content font color align maxWidth none)

/-- Create a named text widget with optional wrapping. -/
def namedText (name : String) (content : String) (font : FontId) (color : Color := Tincture.Color.white)
    (align : TextAlign := .left) (maxWidth : Option Float := none) : WidgetBuilder := do
  let wid ← freshId
  pure (.text wid (some name) content font color align maxWidth none)

/-- Create a text widget that wraps at the given width. -/
def wrappedText (content : String) (font : FontId) (maxWidth : Float)
    (color : Color := Tincture.Color.white) (align : TextAlign := .left) : WidgetBuilder := do
  let wid ← freshId
  pure (.text wid none content font color align (some maxWidth) none)

/-- Create a centered text widget. -/
def centeredText (content : String) (font : FontId) (color : Color := Tincture.Color.white) : WidgetBuilder :=
  text' content font color .center

/-! ## Box Widgets -/

/-- Create a colored rectangle box. -/
def box (style : BoxStyle) : WidgetBuilder := do
  let wid ← freshId
  pure (.rect wid none style)

/-- Create a named colored rectangle box. -/
def namedBox (name : String) (style : BoxStyle) : WidgetBuilder := do
  let wid ← freshId
  pure (.rect wid (some name) style)

/-- Create a simple colored box with dimensions. -/
def coloredBox (color : Color) (width height : Float) : WidgetBuilder := do
  let wid ← freshId
  pure (.rect wid none { backgroundColor := some color, minWidth := some width, minHeight := some height })

/-- Create a named colored box with dimensions. -/
def namedColoredBox (name : String) (color : Color) (width height : Float) : WidgetBuilder := do
  let wid ← freshId
  pure (.rect wid (some name) { backgroundColor := some color, minWidth := some width, minHeight := some height })

/-- Create a custom widget with a rendering spec.
    The widget is stamped with the current cacheGeneration from BuilderState.
    When dynWidget rebuilds, it increments the generation so cache is invalidated. -/
def custom (spec : CustomSpec) (style : BoxStyle := {}) : WidgetBuilder := do
  let s ← get
  let wid ← freshId
  let stampedSpec := { spec with generation := s.cacheGeneration }
  pure (.custom wid none style stampedSpec)

/-- Create a named custom widget with a rendering spec. -/
def namedCustom (name : String) (spec : CustomSpec) (style : BoxStyle := {}) : WidgetBuilder := do
  let wid ← freshId
  pure (.custom wid (some name) style spec)

/-- Create a spacer with fixed dimensions. -/
def spacer (width height : Float) : WidgetBuilder := do
  let wid ← freshId
  pure (.spacer wid none width height)

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
  pure (.flex wid none props style cs)

/-- Create a named horizontal flex row. -/
def namedRow (name : String) (gap : Float := 0) (style : BoxStyle := {}) (children : Array WidgetBuilder) : WidgetBuilder := do
  let wid ← freshId
  let props := Trellis.FlexContainer.row gap
  let cs ← children.mapM fun b => b
  pure (.flex wid (some name) props style cs)

/-- Create a vertical flex column. -/
def column (gap : Float := 0) (style : BoxStyle := {}) (children : Array WidgetBuilder) : WidgetBuilder := do
  let wid ← freshId
  let props := Trellis.FlexContainer.column gap
  let cs ← children.mapM fun b => b
  pure (.flex wid none props style cs)

/-- Create a named vertical flex column. -/
def namedColumn (name : String) (gap : Float := 0) (style : BoxStyle := {}) (children : Array WidgetBuilder) : WidgetBuilder := do
  let wid ← freshId
  let props := Trellis.FlexContainer.column gap
  let cs ← children.mapM fun b => b
  pure (.flex wid (some name) props style cs)

/-- Create a row with custom flex properties. -/
def flexRow (props : Trellis.FlexContainer) (style : BoxStyle := {}) (children : Array WidgetBuilder) : WidgetBuilder := do
  let wid ← freshId
  let cs ← children.mapM fun b => b
  pure (.flex wid none { props with direction := .row } style cs)

/-- Create a column with custom flex properties. -/
def flexColumn (props : Trellis.FlexContainer) (style : BoxStyle := {}) (children : Array WidgetBuilder) : WidgetBuilder := do
  let wid ← freshId
  let cs ← children.mapM fun b => b
  pure (.flex wid none { props with direction := .column } style cs)

/-- Create a centered container (centers single child). -/
def center (style : BoxStyle := {}) (child : WidgetBuilder) : WidgetBuilder := do
  let wid ← freshId
  let props := Trellis.FlexContainer.centered
  let c ← child
  pure (.flex wid none props style #[c])

/-- Create a named centered container (centers single child). -/
def namedCenter (name : String) (style : BoxStyle := {}) (child : WidgetBuilder) : WidgetBuilder := do
  let wid ← freshId
  let props := Trellis.FlexContainer.centered
  let c ← child
  pure (.flex wid (some name) props style #[c])

/-- Create a container with space-between alignment. -/
def spaceBetween (direction : Trellis.FlexDirection := .row) (style : BoxStyle := {})
    (children : Array WidgetBuilder) : WidgetBuilder := do
  let wid ← freshId
  let props := { Trellis.FlexContainer.default with direction, justifyContent := .spaceBetween }
  let cs ← children.mapM fun b => b
  pure (.flex wid none props style cs)

/-- Create a container with space-around alignment. -/
def spaceAround (direction : Trellis.FlexDirection := .row) (style : BoxStyle := {})
    (children : Array WidgetBuilder) : WidgetBuilder := do
  let wid ← freshId
  let props := { Trellis.FlexContainer.default with direction, justifyContent := .spaceAround }
  let cs ← children.mapM fun b => b
  pure (.flex wid none props style cs)

/-- Create a row with both axes centered. -/
def rowCenter (gap : Float := 0) (style : BoxStyle := {})
    (children : Array WidgetBuilder) : WidgetBuilder := do
  let wid ← freshId
  let props := { Trellis.FlexContainer.default with
    direction := .row, justifyContent := .center, alignItems := .center, gap }
  let cs ← children.mapM fun b => b
  pure (.flex wid none props style cs)

/-- Create a column with both axes centered. -/
def colCenter (gap : Float := 0) (style : BoxStyle := {})
    (children : Array WidgetBuilder) : WidgetBuilder := do
  let wid ← freshId
  let props := { Trellis.FlexContainer.default with
    direction := .column, justifyContent := .center, alignItems := .center, gap }
  let cs ← children.mapM fun b => b
  pure (.flex wid none props style cs)

/-- Create a row with space-between alignment. -/
def rowSpaceBetween (gap : Float := 0) (style : BoxStyle := {})
    (children : Array WidgetBuilder) : WidgetBuilder := do
  let wid ← freshId
  let props := { Trellis.FlexContainer.default with
    direction := .row, justifyContent := .spaceBetween, gap }
  let cs ← children.mapM fun b => b
  pure (.flex wid none props style cs)

/-- Create a column with space-between alignment. -/
def colSpaceBetween (gap : Float := 0) (style : BoxStyle := {})
    (children : Array WidgetBuilder) : WidgetBuilder := do
  let wid ← freshId
  let props := { Trellis.FlexContainer.default with
    direction := .column, justifyContent := .spaceBetween, gap }
  let cs ← children.mapM fun b => b
  pure (.flex wid none props style cs)

/-! ## Grid Container Widgets -/

/-- Create a grid with n equal columns. -/
def grid (columns : Nat) (gap : Float := 0) (style : BoxStyle := {})
    (children : Array WidgetBuilder) : WidgetBuilder := do
  let wid ← freshId
  let props := Trellis.GridContainer.columns columns gap
  let cs ← children.mapM fun b => b
  pure (.grid wid none props style cs)

/-- Create a named grid with n equal columns. -/
def namedGrid (name : String) (columns : Nat) (gap : Float := 0) (style : BoxStyle := {})
    (children : Array WidgetBuilder) : WidgetBuilder := do
  let wid ← freshId
  let props := Trellis.GridContainer.columns columns gap
  let cs ← children.mapM fun b => b
  pure (.grid wid (some name) props style cs)

/-- Create a grid with custom properties. -/
def gridCustom (props : Trellis.GridContainer) (style : BoxStyle := {})
    (children : Array WidgetBuilder) : WidgetBuilder := do
  let wid ← freshId
  let cs ← children.mapM fun b => b
  pure (.grid wid none props style cs)

/-! ## Scroll Widgets -/

/-- Create a scroll container. -/
def scroll (style : BoxStyle := {}) (contentWidth contentHeight : Float)
    (scrollState : ScrollState := {}) (scrollbarConfig : ScrollbarRenderConfig := {})
    (child : WidgetBuilder) : WidgetBuilder := do
  let wid ← freshId
  let c ← child
  pure (.scroll wid none style scrollState contentWidth contentHeight scrollbarConfig c)

/-- Create a named scroll container. -/
def namedScroll (name : String) (style : BoxStyle := {}) (contentWidth contentHeight : Float)
    (scrollState : ScrollState := {}) (scrollbarConfig : ScrollbarRenderConfig := {})
    (child : WidgetBuilder) : WidgetBuilder := do
  let wid ← freshId
  let c ← child
  pure (.scroll wid (some name) style scrollState contentWidth contentHeight scrollbarConfig c)

/-- Create a vertical scroll container (scrolls only vertically). -/
def vscroll (style : BoxStyle := {}) (contentHeight : Float)
    (scrollState : ScrollState := {}) (scrollbarConfig : ScrollbarRenderConfig := {})
    (child : WidgetBuilder) : WidgetBuilder := do
  -- Content width = viewport width (no horizontal scrolling)
  let contentWidth := style.minWidth.getD 0
  scroll style contentWidth contentHeight scrollState scrollbarConfig child

/-! ## Convenience Combinators -/

/-- Create a padded container around a child. -/
def padded (padding : Float) (child : WidgetBuilder) : WidgetBuilder := do
  let wid ← freshId
  let props := Trellis.FlexContainer.default
  let style := { padding := Trellis.EdgeInsets.uniform padding }
  let c ← child
  pure (.flex wid none props style #[c])

/-- Create a container with margin around a child. -/
def marginBox (margin : Float) (child : WidgetBuilder) : WidgetBuilder := do
  let wid ← freshId
  let props := Trellis.FlexContainer.default
  let style := { margin := Trellis.EdgeInsets.uniform margin }
  let c ← child
  pure (.flex wid none props style #[c])

/-- Create a card (box with background and padding). -/
def card (bg : Color) (padding : Float := 16) (child : WidgetBuilder) : WidgetBuilder := do
  let wid ← freshId
  let props := Trellis.FlexContainer.default
  let style := BoxStyle.card bg padding
  let c ← child
  pure (.flex wid none props style #[c])

/-- Create a named card (box with background and padding). -/
def namedCard (name : String) (bg : Color) (padding : Float := 16) (child : WidgetBuilder) : WidgetBuilder := do
  let wid ← freshId
  let props := Trellis.FlexContainer.default
  let style := BoxStyle.card bg padding
  let c ← child
  pure (.flex wid (some name) props style #[c])

/-- Create a container with border. -/
def bordered (borderColor : Color) (borderWidth : Float := 1) (child : WidgetBuilder) : WidgetBuilder := do
  let wid ← freshId
  let props := Trellis.FlexContainer.default
  let style := { borderColor := some borderColor, borderWidth }
  let c ← child
  pure (.flex wid none props style #[c])

/-! ## Building -/

/-- Build a widget tree from a builder, starting IDs from 0. -/
def build (builder : WidgetBuilder) : Widget :=
  (builder.run {}).1

/-- Build a widget tree starting from a specific ID. -/
def buildFrom (startId : Nat) (builder : WidgetBuilder) : Widget :=
  (builder.run { nextId := startId }).1

/-- Build a widget tree with a specific cache generation.
    Widgets built with different generations will not share cache entries.
    Used by dynWidget to invalidate cache on rebuild. -/
def buildWithGeneration (generation : Nat) (builder : WidgetBuilder) : Widget :=
  (builder.run { cacheGeneration := generation }).1

/-- Build a widget tree with both a starting ID and cache generation. -/
def buildFromWithGeneration (startId : Nat) (generation : Nat) (builder : WidgetBuilder) : Widget :=
  (builder.run { nextId := startId, cacheGeneration := generation }).1

/-- Run a builder and get both the widget and final state. -/
def buildWithState (builder : WidgetBuilder) : Widget × BuilderState :=
  builder.run {}

/-! ## Monadic Child Builder

The `ChildBuilder` monad allows building widget children using do-notation:

```
row (gap := 10) {} do
  text' "Hello" font
  text' "World" font
  column (gap := 5) {} do
    text' "Nested" font
```

Instead of the array style:

```
row (gap := 10) {} #[
  text' "Hello" font,
  text' "World" font,
  column (gap := 5) {} #[
    text' "Nested" font
  ]
]
```
-/

/-- Child builder monad that accumulates widgets using StateT. -/
abbrev ChildBuilder := StateT (Array Widget) (StateM BuilderState)

/-- Coerce a WidgetBuilder to a ChildBuilder that emits the widget as a child.
    This enables using widget builders directly in do-notation. -/
instance : Coe WidgetBuilder (ChildBuilder Unit) where
  coe builder := do
    let widget ← StateT.lift builder
    modify fun arr => arr.push widget

/-- Run a ChildBuilder and extract the children array. -/
def runChildren (cb : ChildBuilder Unit) : StateM BuilderState (Array Widget) := do
  let ((), children) ← cb.run #[]
  pure children


/-! ## Monadic Container Builders

These builders use `ChildBuilder Unit` for do-notation children.
Use `row`/`column` with `#[...]` arrays for the traditional style.
-/

/-- Create a horizontal flex row with monadic children. -/
def hbox (gap : Float := 0) (style : BoxStyle := {}) (children : ChildBuilder Unit) : WidgetBuilder := do
  let wid ← freshId
  let props := Trellis.FlexContainer.row gap
  let cs ← runChildren children
  pure (.flex wid none props style cs)

/-- Create a vertical flex column with monadic children. -/
def vbox (gap : Float := 0) (style : BoxStyle := {}) (children : ChildBuilder Unit) : WidgetBuilder := do
  let wid ← freshId
  let props := Trellis.FlexContainer.column gap
  let cs ← runChildren children
  pure (.flex wid none props style cs)

/-- Create a flex row with custom properties and monadic children. -/
def hboxWith (props : Trellis.FlexContainer) (style : BoxStyle := {}) (children : ChildBuilder Unit) : WidgetBuilder := do
  let wid ← freshId
  let cs ← runChildren children
  pure (.flex wid none { props with direction := .row } style cs)

/-- Create a flex column with custom properties and monadic children. -/
def vboxWith (props : Trellis.FlexContainer) (style : BoxStyle := {}) (children : ChildBuilder Unit) : WidgetBuilder := do
  let wid ← freshId
  let cs ← runChildren children
  pure (.flex wid none { props with direction := .column } style cs)

/-- Create a row with horizontal centering (main axis only). -/
def hcenter (gap : Float := 0) (style : BoxStyle := {}) (children : ChildBuilder Unit) : WidgetBuilder := do
  let wid ← freshId
  let props := { Trellis.FlexContainer.row gap with justifyContent := .center }
  let cs ← runChildren children
  pure (.flex wid none props style cs)

/-- Create a column with vertical centering (main axis only). -/
def vcenter (gap : Float := 0) (style : BoxStyle := {}) (children : ChildBuilder Unit) : WidgetBuilder := do
  let wid ← freshId
  let props := { Trellis.FlexContainer.column gap with justifyContent := .center }
  let cs ← runChildren children
  pure (.flex wid none props style cs)

/-- Create a row with space-between alignment and monadic children. -/
def hspaced (style : BoxStyle := {}) (children : ChildBuilder Unit) : WidgetBuilder := do
  let wid ← freshId
  let props := { Trellis.FlexContainer.default with direction := .row, justifyContent := .spaceBetween }
  let cs ← runChildren children
  pure (.flex wid none props style cs)

/-- Create a column with space-between alignment and monadic children. -/
def vspaced (style : BoxStyle := {}) (children : ChildBuilder Unit) : WidgetBuilder := do
  let wid ← freshId
  let props := { Trellis.FlexContainer.default with direction := .column, justifyContent := .spaceBetween }
  let cs ← runChildren children
  pure (.flex wid none props style cs)

/-- Create a grid with n equal columns and monadic children. -/
def gridbox (columns : Nat) (gap : Float := 0) (style : BoxStyle := {}) (children : ChildBuilder Unit) : WidgetBuilder := do
  let wid ← freshId
  let props := Trellis.GridContainer.columns columns gap
  let cs ← runChildren children
  pure (.grid wid none props style cs)

end Afferent.Arbor

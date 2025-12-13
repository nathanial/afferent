/-
  Afferent Widget Measure
  Convert widget trees to LayoutNode trees with measured content sizes.
-/
import Afferent.Widget.Core
import Afferent.Widget.TextLayout

namespace Afferent.Widget

/-- Convert BoxStyle to Layout.BoxConstraints. -/
def styleToBoxConstraints (style : BoxStyle) : Layout.BoxConstraints :=
  { width := .auto
    height := .auto
    minWidth := style.minWidth.getD 0
    maxWidth := style.maxWidth
    minHeight := style.minHeight.getD 0
    maxHeight := style.maxHeight
    margin := style.margin
    padding := style.padding }

/-- Result of measuring a widget: the LayoutNode and the updated widget (with computed TextLayout). -/
structure MeasureResult where
  node : Layout.LayoutNode
  widget : Widget
deriving Inhabited

/-- Measure a widget tree and convert to LayoutNode tree.
    Also computes and stores TextLayout for text widgets.
    Returns both the LayoutNode tree and the updated Widget tree with computed layouts. -/
partial def measureWidget (w : Widget) (availWidth availHeight : Float) : IO MeasureResult := do
  match w with
  | .text id content font color align maxWidthOpt textLayoutOpt =>
    -- Compute text layout if not already computed
    let effectiveMaxWidth := maxWidthOpt.getD availWidth
    let textLayout ← match textLayoutOpt with
      | some tl => pure tl
      | none =>
        if maxWidthOpt.isSome then
          wrapText font content effectiveMaxWidth
        else
          measureSingleLine font content

    let contentSize := Layout.ContentSize.mk' textLayout.maxWidth textLayout.totalHeight
    let node := Layout.LayoutNode.leaf id contentSize
    let updatedWidget := Widget.text id content font color align maxWidthOpt (some textLayout)
    pure ⟨node, updatedWidget⟩

  | .rect id style =>
    let box := styleToBoxConstraints style
    let contentW := style.minWidth.getD 0
    let contentH := style.minHeight.getD 0
    let node := Layout.LayoutNode.leaf id ⟨contentW, contentH⟩ box
    pure ⟨node, w⟩

  | .spacer id width height =>
    let node := Layout.LayoutNode.leaf id ⟨width, height⟩
    pure ⟨node, w⟩

  | .flex id props style children =>
    let box := styleToBoxConstraints style
    -- Recursively measure children
    let mut childNodes : Array Layout.LayoutNode := #[]
    let mut updatedChildren : Array Widget := #[]
    for child in children do
      let result ← measureWidget child availWidth availHeight
      childNodes := childNodes.push result.node
      updatedChildren := updatedChildren.push result.widget
    let node := Layout.LayoutNode.flexBox id props childNodes box
    let updatedWidget := Widget.flex id props style updatedChildren
    pure ⟨node, updatedWidget⟩

  | .grid id props style children =>
    let box := styleToBoxConstraints style
    -- Recursively measure children
    let mut childNodes : Array Layout.LayoutNode := #[]
    let mut updatedChildren : Array Widget := #[]
    for child in children do
      let result ← measureWidget child availWidth availHeight
      childNodes := childNodes.push result.node
      updatedChildren := updatedChildren.push result.widget
    let node := Layout.LayoutNode.gridBox id props childNodes box
    let updatedWidget := Widget.grid id props style updatedChildren
    pure ⟨node, updatedWidget⟩

  | .scroll id style scrollState contentW contentH child =>
    let box := styleToBoxConstraints style
    -- Measure child with content size as available space
    let childResult ← measureWidget child contentW contentH
    -- The scroll container's LayoutNode is a flex container that will be sized by parent
    -- The child will be laid out at full content size
    let childNode := childResult.node
    let node := Layout.LayoutNode.flexBox id Layout.FlexContainer.default #[childNode] box
    let updatedWidget := Widget.scroll id style scrollState contentW contentH childResult.widget
    pure ⟨node, updatedWidget⟩

/-- Convenience function that just returns the LayoutNode. -/
def toLayoutNode (w : Widget) (availWidth availHeight : Float) : IO Layout.LayoutNode := do
  let result ← measureWidget w availWidth availHeight
  pure result.node

end Afferent.Widget

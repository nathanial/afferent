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

/-- Compute the intrinsic (content-based) size of a widget tree.
    This is the minimum size needed to fit all content without overflow.
    Used for centering and auto-sizing. -/
partial def intrinsicSize (w : Widget) : IO (Float × Float) := do
  match w with
  | .text _ content font _ _ maxWidthOpt textLayoutOpt =>
    -- Use existing TextLayout if available, otherwise compute
    match textLayoutOpt with
    | some tl => pure (tl.maxWidth, tl.totalHeight)
    | none =>
      let effectiveMaxWidth := maxWidthOpt.getD 10000  -- Large default
      let textLayout ← if maxWidthOpt.isSome then
        wrapText font content effectiveMaxWidth
      else
        measureSingleLine font content
      pure (textLayout.maxWidth, textLayout.totalHeight)

  | .rect _ style =>
    let w := style.minWidth.getD 0
    let h := style.minHeight.getD 0
    pure (w, h)

  | .spacer _ w h =>
    pure (w, h)

  | .flex _ props style children =>
    let padding := style.padding
    let gap := props.gap
    let isColumn := !props.direction.isHorizontal

    -- Compute intrinsic sizes of all children
    let childSizes ← children.mapM intrinsicSize

    if isColumn then
      -- Column: width = max of children, height = sum of children + gaps
      let maxWidth := childSizes.foldl (fun acc (w, _) => max acc w) 0
      let totalHeight := childSizes.foldl (fun acc (_, h) => acc + h) 0
      let gaps := if children.size > 1 then gap * (children.size - 1).toFloat else 0
      pure (maxWidth + padding.horizontal, totalHeight + gaps + padding.vertical)
    else
      -- Row: width = sum of children + gaps, height = max of children
      let totalWidth := childSizes.foldl (fun acc (w, _) => acc + w) 0
      let maxHeight := childSizes.foldl (fun acc (_, h) => max acc h) 0
      let gaps := if children.size > 1 then gap * (children.size - 1).toFloat else 0
      pure (totalWidth + gaps + padding.horizontal, maxHeight + padding.vertical)

  | .grid _ props style children =>
    let padding := style.padding
    let numCols := props.templateColumns.tracks.size
    let numCols := if numCols == 0 then 1 else numCols  -- Default to 1 column
    let colGap := props.columnGap
    let rowGap := props.rowGap

    -- Compute intrinsic sizes of all children
    let childSizes ← children.mapM intrinsicSize

    -- For grid, compute column widths and row heights
    let numRows := (children.size + numCols - 1) / numCols
    let mut maxColWidth : Float := 0
    let mut maxRowHeight : Float := 0

    for (w, h) in childSizes do
      maxColWidth := max maxColWidth w
      maxRowHeight := max maxRowHeight h

    let totalWidth := maxColWidth * numCols.toFloat + colGap * (numCols - 1).toFloat
    let totalHeight := maxRowHeight * numRows.toFloat + rowGap * (numRows - 1).toFloat
    pure (totalWidth + padding.horizontal, totalHeight + padding.vertical)

  | .scroll _ style _ contentW contentH _ =>
    -- Scroll containers use their viewport size (from style) or content size
    let w := style.minWidth.getD contentW
    let h := style.minHeight.getD contentH
    pure (w + style.padding.horizontal, h + style.padding.vertical)

end Afferent.Widget

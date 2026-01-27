/-
  Arbor Widget Measure
  Convert widget trees to LayoutNode trees with measured content sizes.
  Uses the TextMeasurer typeclass for backend independence.
-/
import Afferent.Arbor.Widget.Core
import Afferent.Arbor.Widget.TextLayout
import Afferent.Arbor.Core.TextMeasurer
import Trellis

namespace Afferent.Arbor

/-- Convert BoxStyle to Trellis.BoxConstraints. -/
def styleToBoxConstraints (style : BoxStyle) : Trellis.BoxConstraints :=
  { width := style.width
    height := style.height
    minWidth := style.minWidth.getD 0
    maxWidth := style.maxWidth
    minHeight := style.minHeight.getD 0
    maxHeight := style.maxHeight
    position := style.position
    top := style.top
    right := style.right
    bottom := style.bottom
    left := style.left
    margin := style.margin
    padding := style.padding }

/-- Extract BoxStyle from a Widget (if it has one). -/
def widgetBoxStyle : Widget → Option BoxStyle
  | .flex _ _ _ style _ => some style
  | .grid _ _ _ style _ => some style
  | .rect _ _ style => some style
  | .custom _ _ style _ => some style
  | .scroll _ _ style _ _ _ _ _ => some style
  | .text .. => none
  | .spacer .. => none

/-- Set the ItemKind on a LayoutNode. -/
def setItemKind (node : Trellis.LayoutNode) (item : Trellis.ItemKind) : Trellis.LayoutNode :=
  node.withItem item

/-- Apply flexItem from BoxStyle to a LayoutNode if present. -/
def applyFlexItem (node : Trellis.LayoutNode) (style : Option BoxStyle) : Trellis.LayoutNode :=
  match style with
  | some s =>
    match s.flexItem with
    | some fi => setItemKind node (.flexChild fi)
    | none => node
  | none => node

/-- Apply gridItem from BoxStyle to a LayoutNode if present. -/
def applyGridItem (node : Trellis.LayoutNode) (style : Option BoxStyle) : Trellis.LayoutNode :=
  match style with
  | some s =>
    match s.gridItem with
    | some gi => setItemKind node (.gridChild gi)
    | none => node
  | none => node

/-- Result of measuring a widget: the LayoutNode and the updated widget (with computed TextLayout). -/
structure MeasureResult where
  node : Trellis.LayoutNode
  widget : Widget
deriving Inhabited

/-- Get the intrinsic content size stored on a layout node (0 if missing). -/
def nodeContentSize (n : Trellis.LayoutNode) : Float × Float :=
  match n.content with
  | some cs => (cs.width, cs.height)
  | none => (0, 0)

/-- Measure a widget tree and convert to LayoutNode tree.
    Also computes and stores TextLayout for text widgets.
    Returns both the LayoutNode tree and the updated Widget tree with computed layouts.
    Uses the TextMeasurer typeclass for backend independence. -/
partial def measureWidget {M : Type → Type} [Monad M] [TextMeasurer M] (w : Widget) (availWidth availHeight : Float)
    : M MeasureResult := do
  match w with
  | .text id name content font color align maxWidthOpt textLayoutOpt =>
    -- Compute text layout if not already computed
    let effectiveMaxWidth := maxWidthOpt.getD availWidth
    let textLayout ← match textLayoutOpt with
      | some tl => pure tl
      | none =>
        if maxWidthOpt.isSome then
          wrapText font content effectiveMaxWidth
        else
          measureSingleLine font content

    let contentSize := Trellis.ContentSize.mk' textLayout.maxWidth textLayout.totalHeight
    let node := Trellis.LayoutNode.leaf id contentSize
    let updatedWidget := Widget.text id name content font color align maxWidthOpt (some textLayout)
    pure ⟨node, updatedWidget⟩

  | .rect id _ style =>
    let box := styleToBoxConstraints style
    let contentW := style.minWidth.getD 0
    let contentH := style.minHeight.getD 0
    let node := Trellis.LayoutNode.leaf id (Trellis.ContentSize.mk' contentW contentH) box
    pure ⟨node, w⟩

  | .spacer id _ width height =>
    let node := Trellis.LayoutNode.leaf id (Trellis.ContentSize.mk' width height)
    pure ⟨node, w⟩

  | .custom id _ style spec =>
    let box := styleToBoxConstraints style
    let (measuredW, measuredH) := spec.measure availWidth availHeight
    let contentW := max measuredW box.minWidth
    let contentH := max measuredH box.minHeight
    let node := Trellis.LayoutNode.leaf id (Trellis.ContentSize.mk' contentW contentH) box
    pure ⟨node, w⟩

  | .flex id name props style children =>
    let box := styleToBoxConstraints style
    -- Recursively measure children, applying flexItem properties
    let mut childNodes : Array Trellis.LayoutNode := #[]
    let mut updatedChildren : Array Widget := #[]
    for child in children do
      let result ← measureWidget child availWidth availHeight
      -- Apply flexItem from child's BoxStyle if present
      let nodeWithItem := applyFlexItem result.node (widgetBoxStyle child)
      childNodes := childNodes.push nodeWithItem
      updatedChildren := updatedChildren.push result.widget
    -- Store an intrinsic content size on the container so parent flex/grid layout
    -- can size this node based on its children (avoids collapsing to 0).
    let padding := style.padding
    let gap := props.gap
    let isColumn := !props.direction.isHorizontal
    let flowNodes := childNodes.filter (fun n => n.box.position != .absolute)
    let childSizes := flowNodes.map nodeContentSize
    let (rawContentW, rawContentH) :=
      if isColumn then
        let maxWidth := childSizes.foldl (fun acc (cw, _) => max acc cw) 0
        let totalHeight := childSizes.foldl (fun acc (_, ch) => acc + ch) 0
        let gaps := if flowNodes.size > 1 then gap * (flowNodes.size - 1).toFloat else 0
        (maxWidth + padding.horizontal, totalHeight + gaps + padding.vertical)
      else
        let totalWidth := childSizes.foldl (fun acc (cw, _) => acc + cw) 0
        let maxHeight := childSizes.foldl (fun acc (_, ch) => max acc ch) 0
        let gaps := if flowNodes.size > 1 then gap * (flowNodes.size - 1).toFloat else 0
        (totalWidth + gaps + padding.horizontal, maxHeight + padding.vertical)

    -- Apply min constraints to content size so parent containers respect our minimum size
    let contentW := max rawContentW box.minWidth
    let contentH := max rawContentH box.minHeight

    let node :=
      Trellis.LayoutNode.mk id box (.flex props) .none (some (Trellis.ContentSize.mk' contentW contentH)) childNodes
    let updatedWidget := Widget.flex id name props style updatedChildren
    pure ⟨node, updatedWidget⟩

  | .grid id name props style children =>
    let box := styleToBoxConstraints style
    -- Recursively measure children
    let mut childNodes : Array Trellis.LayoutNode := #[]
    let mut updatedChildren : Array Widget := #[]
    for child in children do
      let result ← measureWidget child availWidth availHeight
      -- Apply gridItem from child's BoxStyle if present
      let nodeWithItem := applyGridItem result.node (widgetBoxStyle child)
      childNodes := childNodes.push nodeWithItem
      updatedChildren := updatedChildren.push result.widget
    -- Store an intrinsic content size on the container so parent flex/grid layout
    -- can size this node based on its children (avoids collapsing to 0).
    let padding := style.padding
    let numCols := props.templateColumns.tracks.size
    let numCols := if numCols == 0 then 1 else numCols
    let colGap := props.columnGap
    let rowGap := props.rowGap

    let flowNodes := childNodes.filter (fun n => n.box.position != .absolute)
    let childSizes := flowNodes.map nodeContentSize
    let numRows := (flowNodes.size + numCols - 1) / numCols
    let mut maxColWidth : Float := 0
    let mut maxRowHeight : Float := 0
    for (cw, ch) in childSizes do
      maxColWidth := max maxColWidth cw
      maxRowHeight := max maxRowHeight ch

    let totalWidth := maxColWidth * numCols.toFloat + colGap * (numCols - 1).toFloat
    let totalHeight := maxRowHeight * numRows.toFloat + rowGap * (numRows - 1).toFloat
    let rawContentW := totalWidth + padding.horizontal
    let rawContentH := totalHeight + padding.vertical

    -- Apply min constraints to content size so parent containers respect our minimum size
    let contentW := max rawContentW box.minWidth
    let contentH := max rawContentH box.minHeight

    let node :=
      Trellis.LayoutNode.mk id box (.grid props) .none (some (Trellis.ContentSize.mk' contentW contentH)) childNodes
    let updatedWidget := Widget.grid id name props style updatedChildren
    pure ⟨node, updatedWidget⟩

  | .scroll id name style scrollState contentW contentH scrollbarConfig child =>
    let box := styleToBoxConstraints style
    -- Measure child with content size as available space
    let childResult ← measureWidget child contentW contentH
    -- The scroll container's LayoutNode is a flex container that will be sized by parent
    -- The child must be laid out at full content size (not shrunk to viewport)
    -- Set shrink=0 and alignSelf=flexStart to prevent cross-axis stretch
    let childItem : Trellis.FlexItem := { grow := 1, shrink := 0, alignSelf := some .flexStart }
    let origBox := childResult.node.box
    let newMinWidth := max origBox.minWidth contentW
    let newMinHeight := max origBox.minHeight contentH
    let childBox := { origBox with
      -- Keep width flexible so the child can stretch to the viewport,
      -- but ensure it never shrinks below the scrollable content width.
      width := origBox.width
      height := if origBox.height.isAuto then .length contentH else origBox.height
      minHeight := newMinHeight
      minWidth := newMinWidth
    }
    let origNode := childResult.node
    let childNode := Trellis.LayoutNode.mk origNode.id childBox origNode.container
      (.flexChild childItem) origNode.content origNode.children
    let viewportW := style.minWidth.getD contentW
    let viewportH := style.minHeight.getD contentH
    let viewportBorderW := viewportW + style.padding.horizontal
    let viewportBorderH := viewportH + style.padding.vertical
    let node :=
      Trellis.LayoutNode.mk id box (.flex Trellis.FlexContainer.default) .none
        (some (Trellis.ContentSize.mk' viewportBorderW viewportBorderH)) #[childNode]
    let updatedWidget := Widget.scroll id name style scrollState contentW contentH scrollbarConfig childResult.widget
    pure ⟨node, updatedWidget⟩

/-- Convenience function that just returns the LayoutNode. -/
def toLayoutNode {M : Type → Type} [Monad M] [TextMeasurer M] (w : Widget) (availWidth availHeight : Float)
    : M Trellis.LayoutNode := do
  let result ← measureWidget w availWidth availHeight
  pure result.node

/-- Compute the intrinsic (content-based) size of a widget tree.
    This is the minimum size needed to fit all content without overflow.
    Used for centering and auto-sizing. -/
partial def intrinsicSize {M : Type → Type} [Monad M] [TextMeasurer M] (w : Widget) : M (Float × Float) := do
  match w with
  | .text _ _ content font _ _ maxWidthOpt textLayoutOpt =>
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

  | .rect _ _ style =>
    let w := style.minWidth.getD 0
    let h := style.minHeight.getD 0
    pure (w, h)

  | .spacer _ _ w h =>
    pure (w, h)

  | .custom _ _ style spec =>
    let (measuredW, measuredH) := spec.measure 1000000000.0 1000000000.0
    let contentW := max measuredW (style.minWidth.getD 0)
    let contentH := max measuredH (style.minHeight.getD 0)
    pure (contentW, contentH)

  | .flex _ _ props style children =>
    let padding := style.padding
    let gap := props.gap
    let isColumn := !props.direction.isHorizontal

    -- Compute intrinsic sizes of all children
    let childSizes ← children.mapM intrinsicSize

    let (rawW, rawH) := if isColumn then
      -- Column: width = max of children, height = sum of children + gaps
      let maxWidth := childSizes.foldl (fun acc (w, _) => max acc w) 0
      let totalHeight := childSizes.foldl (fun acc (_, h) => acc + h) 0
      let gaps := if children.size > 1 then gap * (children.size - 1).toFloat else 0
      (maxWidth + padding.horizontal, totalHeight + gaps + padding.vertical)
    else
      -- Row: width = sum of children + gaps, height = max of children
      let totalWidth := childSizes.foldl (fun acc (w, _) => acc + w) 0
      let maxHeight := childSizes.foldl (fun acc (_, h) => max acc h) 0
      let gaps := if children.size > 1 then gap * (children.size - 1).toFloat else 0
      (totalWidth + gaps + padding.horizontal, maxHeight + padding.vertical)

    -- Apply min constraints
    pure (max rawW (style.minWidth.getD 0), max rawH (style.minHeight.getD 0))

  | .grid _ _ props style children =>
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
    let rawW := totalWidth + padding.horizontal
    let rawH := totalHeight + padding.vertical

    -- Apply min constraints
    pure (max rawW (style.minWidth.getD 0), max rawH (style.minHeight.getD 0))

  | .scroll _ _ style _ contentW contentH _ _ =>
    -- Scroll containers use their viewport size (from style) or content size
    let w := style.minWidth.getD contentW
    let h := style.minHeight.getD contentH
    pure (w + style.padding.horizontal, h + style.padding.vertical)

/-- Compute the intrinsic (content-based) size of a widget tree AND return
    the updated widget with computed TextLayouts.
    This avoids double traversal: compute size AND cache text layouts in one pass.
    Used for caching in centered layout mode. -/
partial def intrinsicSizeWithWidget {M : Type → Type} [Monad M] [TextMeasurer M] (w : Widget)
    : M (Float × Float × Widget) := do
  match w with
  | .text id name content font color align maxWidthOpt textLayoutOpt =>
    -- Use existing TextLayout if available, otherwise compute
    match textLayoutOpt with
    | some tl => pure (tl.maxWidth, tl.totalHeight, w)
    | none =>
      let effectiveMaxWidth := maxWidthOpt.getD 10000  -- Large default
      let textLayout ← if maxWidthOpt.isSome then
        wrapText font content effectiveMaxWidth
      else
        measureSingleLine font content
      let updatedWidget := Widget.text id name content font color align maxWidthOpt (some textLayout)
      pure (textLayout.maxWidth, textLayout.totalHeight, updatedWidget)

  | .rect _ _ style =>
    let width := style.minWidth.getD 0
    let height := style.minHeight.getD 0
    pure (width, height, w)

  | .spacer _ _ width height =>
    pure (width, height, w)

  | .custom _ _ style spec =>
    let (measuredW, measuredH) := spec.measure 1000000000.0 1000000000.0
    let contentW := max measuredW (style.minWidth.getD 0)
    let contentH := max measuredH (style.minHeight.getD 0)
    pure (contentW, contentH, w)

  | .flex id name props style children =>
    let padding := style.padding
    let gap := props.gap
    let isColumn := !props.direction.isHorizontal

    -- Compute intrinsic sizes of all children AND get updated children
    let mut childSizes : Array (Float × Float) := #[]
    let mut updatedChildren : Array Widget := #[]
    for child in children do
      let (cw, ch, updatedChild) ← intrinsicSizeWithWidget child
      childSizes := childSizes.push (cw, ch)
      updatedChildren := updatedChildren.push updatedChild

    let (rawW, rawH) := if isColumn then
      -- Column: width = max of children, height = sum of children + gaps
      let maxWidth := childSizes.foldl (fun acc (cw, _) => max acc cw) 0
      let totalHeight := childSizes.foldl (fun acc (_, ch) => acc + ch) 0
      let gaps := if children.size > 1 then gap * (children.size - 1).toFloat else 0
      (maxWidth + padding.horizontal, totalHeight + gaps + padding.vertical)
    else
      -- Row: width = sum of children + gaps, height = max of children
      let totalWidth := childSizes.foldl (fun acc (cw, _) => acc + cw) 0
      let maxHeight := childSizes.foldl (fun acc (_, ch) => max acc ch) 0
      let gaps := if children.size > 1 then gap * (children.size - 1).toFloat else 0
      (totalWidth + gaps + padding.horizontal, maxHeight + padding.vertical)

    -- Apply min constraints
    let finalW := max rawW (style.minWidth.getD 0)
    let finalH := max rawH (style.minHeight.getD 0)
    let updatedWidget := Widget.flex id name props style updatedChildren
    pure (finalW, finalH, updatedWidget)

  | .grid id name props style children =>
    let padding := style.padding
    let numCols := props.templateColumns.tracks.size
    let numCols := if numCols == 0 then 1 else numCols  -- Default to 1 column
    let colGap := props.columnGap
    let rowGap := props.rowGap

    -- Compute intrinsic sizes of all children AND get updated children
    let mut childSizes : Array (Float × Float) := #[]
    let mut updatedChildren : Array Widget := #[]
    for child in children do
      let (cw, ch, updatedChild) ← intrinsicSizeWithWidget child
      childSizes := childSizes.push (cw, ch)
      updatedChildren := updatedChildren.push updatedChild

    -- For grid, compute column widths and row heights
    let numRows := (children.size + numCols - 1) / numCols
    let mut maxColWidth : Float := 0
    let mut maxRowHeight : Float := 0

    for (cw, ch) in childSizes do
      maxColWidth := max maxColWidth cw
      maxRowHeight := max maxRowHeight ch

    let totalWidth := maxColWidth * numCols.toFloat + colGap * (numCols - 1).toFloat
    let totalHeight := maxRowHeight * numRows.toFloat + rowGap * (numRows - 1).toFloat
    let rawW := totalWidth + padding.horizontal
    let rawH := totalHeight + padding.vertical

    -- Apply min constraints
    let finalW := max rawW (style.minWidth.getD 0)
    let finalH := max rawH (style.minHeight.getD 0)
    let updatedWidget := Widget.grid id name props style updatedChildren
    pure (finalW, finalH, updatedWidget)

  | .scroll id name style scrollState contentW contentH scrollbarConfig child =>
    -- Measure child to get updated child with TextLayouts
    let (_, _, updatedChild) ← intrinsicSizeWithWidget child
    -- Scroll containers use their viewport size (from style) or content size
    let w := style.minWidth.getD contentW
    let h := style.minHeight.getD contentH
    let updatedWidget := Widget.scroll id name style scrollState contentW contentH scrollbarConfig updatedChild
    pure (w + style.padding.horizontal, h + style.padding.vertical, updatedWidget)

end Afferent.Arbor

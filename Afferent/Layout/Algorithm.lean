/-
  Afferent Layout Algorithm
  CSS Flexbox and Grid layout computation.
-/
import Afferent.Layout.Types
import Afferent.Layout.Flex
import Afferent.Layout.Grid
import Afferent.Layout.Node
import Afferent.Layout.Axis
import Afferent.Layout.Result

namespace Afferent.Layout

/-! ## Flex Item Intermediate State -/

/-- Intermediate state for a flex item during layout computation. -/
structure FlexItemState where
  node : LayoutNode
  /-- Outer margin. -/
  margin : EdgeInsets
  /-- Hypothetical main size (before flex grow/shrink). -/
  hypotheticalMainSize : Length
  /-- Base flex size (flex-basis resolved). -/
  flexBaseSize : Length
  /-- Min main size constraint. -/
  minMainSize : Length
  /-- Max main size constraint. -/
  maxMainSize : Length
  /-- Hypothetical cross size. -/
  hypotheticalCrossSize : Length
  /-- Flex grow factor. -/
  flexGrow : Float
  /-- Flex shrink factor. -/
  flexShrink : Float
  /-- Is this item frozen (cannot flex further)? -/
  frozen : Bool := false
  /-- Resolved main size after flex. -/
  resolvedMainSize : Length := 0
  /-- Resolved cross size. -/
  resolvedCrossSize : Length := 0
deriving Repr, Inhabited

/-- A flex line (group of items on one line). -/
structure FlexLine where
  items : Array FlexItemState
  /-- Total main space used by items and gaps. -/
  usedMainSpace : Length
  /-- Cross size of this line. -/
  crossSize : Length
  /-- Position along cross axis. -/
  crossPosition : Length := 0
deriving Repr, Inhabited

namespace FlexLine

def itemCount (line : FlexLine) : Nat := line.items.size

def totalFlexGrow (line : FlexLine) : Float :=
  line.items.foldl (fun acc item => acc + item.flexGrow) 0

def totalFlexShrinkScaled (line : FlexLine) : Float :=
  line.items.foldl (fun acc item => acc + item.flexShrink * item.flexBaseSize) 0

def unfrozenCount (line : FlexLine) : Nat :=
  line.items.foldl (fun acc item => if item.frozen then acc else acc + 1) 0

end FlexLine

/-! ## Phase 1: Available Space -/

/-- Compute available space inside a container after padding. -/
def computeAvailableSpace (containerWidth containerHeight : Length)
    (padding : EdgeInsets) : Length × Length :=
  (max 0 (containerWidth - padding.horizontal),
   max 0 (containerHeight - padding.vertical))

/-! ## Phase 2: Collect Flex Items -/

/-- Get the content size of a node. -/
def getContentSize (node : LayoutNode) : Length × Length :=
  match node.content with
  | some cs => (cs.width, cs.height)
  | none => (0, 0)  -- Containers measure children recursively

/-- Resolve flex-basis for an item. -/
def resolveFlexBasis (basis : Dimension) (contentMain : Length)
    (availableMain : Length) : Length :=
  match basis with
  | .auto => contentMain
  | .length l => l
  | .percent p => availableMain * p
  | .minContent | .maxContent => contentMain

/-- Collect flex items from children with initial measurements. -/
def collectFlexItems (axis : AxisInfo) (children : Array LayoutNode)
    (availableMain : Length) : Array FlexItemState := Id.run do
  let mut items : Array FlexItemState := #[]
  for child in children do
    let flexProps := child.flexItem?.getD FlexItem.default
    let contentSize := getContentSize child
    let contentMain := axis.mainFromPair contentSize
    let contentCross := axis.crossFromPair contentSize

    -- Resolve flex-basis
    let flexBasis := resolveFlexBasis flexProps.basis contentMain availableMain

    -- Get constraints
    let box := child.box
    let minMain := axis.mainMin box
    let maxMain := (axis.mainMax box).getD 1000000.0  -- Large value for "unbounded"
    let minCross := axis.crossMin box
    let maxCross := (axis.crossMax box).getD 1000000.0

    -- Compute hypothetical sizes (clamped to constraints)
    let hypotheticalMain := min maxMain (max minMain flexBasis)
    let hypotheticalCross := min maxCross (max minCross contentCross)

    items := items.push {
      node := child
      margin := box.margin
      hypotheticalMainSize := hypotheticalMain
      flexBaseSize := flexBasis
      minMainSize := minMain
      maxMainSize := maxMain
      hypotheticalCrossSize := hypotheticalCross
      flexGrow := flexProps.grow
      flexShrink := flexProps.shrink
    }
  return items

/-! ## Phase 3: Partition Into Lines -/

/-- Compute total main space used by items in a line. -/
def computeLineMainSpace (items : Array FlexItemState) (gap : Length) : Length :=
  if items.isEmpty then 0
  else
    let itemSizes := items.foldl (fun acc item =>
      acc + item.hypotheticalMainSize + item.margin.horizontal) 0
    let gaps := gap * (items.size - 1).toFloat
    itemSizes + gaps

/-- Compute cross size of a line (max of item cross sizes). -/
def computeLineCrossSize (items : Array FlexItemState) : Length :=
  items.foldl (fun acc item =>
    max acc (item.hypotheticalCrossSize + item.margin.vertical)) 0

/-- Partition items into flex lines based on wrapping. -/
def partitionIntoLines (items : Array FlexItemState) (wrap : FlexWrap)
    (availableMain gap : Length) : Array FlexLine := Id.run do
  if items.isEmpty then return #[]

  match wrap with
  | .nowrap =>
    -- Single line with all items
    let usedSpace := computeLineMainSpace items gap
    #[{ items, usedMainSpace := usedSpace, crossSize := computeLineCrossSize items }]
  | .wrap | .wrapReverse =>
    let mut lines : Array FlexLine := #[]
    let mut currentItems : Array FlexItemState := #[]
    let mut currentUsed : Length := 0

    for item in items do
      let itemSize := item.hypotheticalMainSize + item.margin.horizontal
      let wouldUse := currentUsed + itemSize +
        (if currentItems.isEmpty then 0 else gap)

      if !currentItems.isEmpty && wouldUse > availableMain then
        -- Start new line
        lines := lines.push {
          items := currentItems
          usedMainSpace := currentUsed
          crossSize := computeLineCrossSize currentItems
        }
        currentItems := #[item]
        currentUsed := itemSize
      else
        currentItems := currentItems.push item
        currentUsed := wouldUse

    -- Add last line
    if !currentItems.isEmpty then
      lines := lines.push {
        items := currentItems
        usedMainSpace := currentUsed
        crossSize := computeLineCrossSize currentItems
      }

    -- Reverse lines if wrap-reverse
    if wrap == .wrapReverse then lines.reverse else lines

/-! ## Phase 4: Resolve Flexible Lengths -/

/-- Check if all items in a line are frozen. -/
def allFrozen (items : Array FlexItemState) : Bool :=
  items.all (·.frozen)

/-- Distribute positive free space (grow). -/
def distributeGrowth (line : FlexLine) (freeSpace : Length)
    (gap : Length) : FlexLine :=
  let totalGrow := line.totalFlexGrow
  if totalGrow <= 0 then
    -- No items can grow, use hypothetical sizes
    let items := line.items.map fun item =>
      { item with resolvedMainSize := item.hypotheticalMainSize }
    { line with items }
  else
    -- Simple one-pass distribution (without iterative constraint handling for now)
    let spacePerGrow := freeSpace / totalGrow
    let items := line.items.map fun item =>
      if item.flexGrow <= 0 then
        { item with resolvedMainSize := item.hypotheticalMainSize }
      else
        let growth := spacePerGrow * item.flexGrow
        let newSize := item.hypotheticalMainSize + growth
        let clamped := min item.maxMainSize (max item.minMainSize newSize)
        { item with resolvedMainSize := clamped }

    let newUsed := items.foldl (fun acc item =>
      acc + item.resolvedMainSize + item.margin.horizontal) 0
    let gaps := gap * (items.size - 1).toFloat
    { line with items, usedMainSpace := newUsed + gaps }

/-- Distribute negative free space (shrink). -/
def distributeShrinkage (line : FlexLine) (overflow : Length)
    (gap : Length) : FlexLine :=
  let totalShrinkScaled := line.totalFlexShrinkScaled
  if totalShrinkScaled <= 0 then
    -- No items can shrink, use hypothetical sizes
    let items := line.items.map fun item =>
      { item with resolvedMainSize := item.hypotheticalMainSize }
    { line with items }
  else
    -- Simple one-pass distribution
    let items := line.items.map fun item =>
      if item.flexShrink <= 0 || item.flexBaseSize <= 0 then
        { item with resolvedMainSize := item.hypotheticalMainSize }
      else
        let shrinkRatio := (item.flexShrink * item.flexBaseSize) / totalShrinkScaled
        let shrinkage := overflow * shrinkRatio
        let newSize := item.hypotheticalMainSize - shrinkage
        let clamped := max item.minMainSize newSize
        { item with resolvedMainSize := clamped }

    let newUsed := items.foldl (fun acc item =>
      acc + item.resolvedMainSize + item.margin.horizontal) 0
    let gaps := gap * (items.size - 1).toFloat
    { line with items, usedMainSpace := newUsed + gaps }

/-- Resolve flexible lengths for a line. -/
def resolveFlexibleLengths (line : FlexLine) (availableMain gap : Length) : FlexLine :=
  let freeSpace := availableMain - line.usedMainSpace
  if freeSpace >= 0 then
    distributeGrowth line freeSpace gap
  else
    distributeShrinkage line (-freeSpace) gap

/-! ## Phase 5: Cross Axis Sizing -/

/-- Resolve cross sizes based on align-items. -/
def resolveCrossSizes (line : FlexLine) (alignItems : AlignItems) : FlexLine :=
  let items := line.items.map fun item =>
    let alignSelf := match item.node.flexItem? with
      | some fi => fi.alignSelf.getD alignItems
      | none => alignItems
    let crossSize := match alignSelf with
      | .stretch => line.crossSize - item.margin.vertical
      | _ => item.hypotheticalCrossSize
    { item with resolvedCrossSize := crossSize }
  { line with items }

/-! ## Phase 6: Main Axis Alignment (justify-content) -/

/-- Compute main axis positions for items in a line. -/
def computeMainPositions (items : Array FlexItemState)
    (justify : JustifyContent) (availableMain gap : Length)
    (isReversed : Bool) : Array Length := Id.run do
  let n := items.size
  if n == 0 then return #[]

  let totalItemSize := items.foldl (fun acc i => acc + i.resolvedMainSize) 0
  let totalMargins := items.foldl (fun acc i => acc + i.margin.horizontal) 0
  let totalGaps := gap * (n - 1).toFloat
  let usedSpace := totalItemSize + totalMargins + totalGaps
  let freeSpace := availableMain - usedSpace

  let (startOffset, itemGap) := match justify with
    | .flexStart => (0.0, gap)
    | .flexEnd => (freeSpace, gap)
    | .center => (freeSpace / 2.0, gap)
    | .spaceBetween =>
        if n == 1 then (0.0, 0.0)
        else (0.0, freeSpace / (n - 1).toFloat + gap)
    | .spaceAround =>
        let space := freeSpace / n.toFloat
        (space / 2.0, space + gap)
    | .spaceEvenly =>
        let space := freeSpace / (n + 1).toFloat
        (space, space + gap)

  let mut positions : Array Length := #[]
  let mut currentPos := startOffset

  for item in items do
    positions := positions.push (currentPos + item.margin.left)
    currentPos := currentPos + item.margin.horizontal + item.resolvedMainSize + itemGap

  if isReversed then
    -- Reverse positions relative to available space
    positions.mapIdx fun i pos =>
      availableMain - pos - items[i]!.resolvedMainSize
  else
    positions

/-! ## Phase 7: Cross Axis Alignment (align-items) -/

/-- Compute cross axis positions for items in a line. -/
def computeCrossPositions (items : Array FlexItemState)
    (alignItems : AlignItems) (lineCrossSize : Length) : Array Length :=
  items.map fun item =>
    let alignSelf := match item.node.flexItem? with
      | some fi => fi.alignSelf.getD alignItems
      | none => alignItems
    let itemCrossSize := item.resolvedCrossSize + item.margin.vertical
    match alignSelf with
    | .flexStart | .baseline => item.margin.top
    | .flexEnd => lineCrossSize - item.margin.bottom - item.resolvedCrossSize
    | .center => (lineCrossSize - itemCrossSize) / 2.0 + item.margin.top
    | .stretch => item.margin.top

/-! ## Phase 8: Align Content (multi-line) -/

/-- Position flex lines along the cross axis. -/
def alignFlexLines (lines : Array FlexLine) (alignContent : AlignContent)
    (availableCross rowGap : Length) : Array FlexLine := Id.run do
  if lines.isEmpty then return #[]

  let totalLineCross := lines.foldl (fun acc l => acc + l.crossSize) 0
  let totalGaps := rowGap * (lines.size - 1).toFloat
  let freeSpace := availableCross - totalLineCross - totalGaps

  let (startOffset, lineGap) := match alignContent with
    | .flexStart | .stretch => (0.0, rowGap)
    | .flexEnd => (freeSpace, rowGap)
    | .center => (freeSpace / 2.0, rowGap)
    | .spaceBetween =>
        if lines.size == 1 then (0.0, 0.0)
        else (0.0, freeSpace / (lines.size - 1).toFloat + rowGap)
    | .spaceAround =>
        let space := freeSpace / lines.size.toFloat
        (space / 2.0, space + rowGap)
    | .spaceEvenly =>
        let space := freeSpace / (lines.size + 1).toFloat
        (space, space + rowGap)

  let mut positioned : Array FlexLine := #[]
  let mut currentCross := startOffset

  for line in lines do
    positioned := positioned.push { line with crossPosition := currentCross }
    currentCross := currentCross + line.crossSize + lineGap

  positioned

/-! ## Main Layout Function -/

/-- Layout a flex container. -/
def layoutFlexContainer (container : FlexContainer) (children : Array LayoutNode)
    (containerWidth containerHeight : Length)
    (padding : EdgeInsets) : LayoutResult := Id.run do
  let axis := AxisInfo.fromDirection container.direction

  -- Phase 1: Available space
  let (availableMain, availableCross) :=
    let (w, h) := computeAvailableSpace containerWidth containerHeight padding
    (axis.mainSize w h, axis.crossSize w h)

  -- Phase 2: Collect items
  let items := collectFlexItems axis children availableMain

  -- Phase 3: Partition into lines
  let lines := partitionIntoLines items container.wrap availableMain container.gap

  -- Phase 4: Resolve flexible lengths
  let lines := lines.map fun line =>
    resolveFlexibleLengths line availableMain container.gap

  -- Phase 8: Align content (position lines)
  -- For single-line containers with stretch, use full cross space
  let lines := if lines.size == 1 && container.alignContent == .stretch then
    lines.map fun line => { line with crossSize := availableCross }
  else
    alignFlexLines lines container.alignContent availableCross container.rowGap

  -- Phase 5: Cross axis sizing (after line sizes are finalized)
  let lines := lines.map fun line =>
    resolveCrossSizes line container.alignItems

  -- Phases 6-7: Position items within lines
  let mut result := LayoutResult.empty

  for line in lines do
    -- Phase 6: Main axis positions
    let mainPositions := computeMainPositions line.items
                         container.justifyContent availableMain
                         container.gap axis.isReversed

    -- Phase 7: Cross axis positions
    let crossPositions := computeCrossPositions line.items
                          container.alignItems line.crossSize

    -- Build computed layouts
    for i in [:line.items.size] do
      if h : i < line.items.size then
        let item := line.items[i]
        let mainPos := mainPositions[i]! + axis.mainStart padding
        let crossPos := crossPositions[i]! + line.crossPosition + axis.crossStart padding

        let (x, y) := axis.toXY mainPos crossPos
        let (width, height) := axis.toWidthHeight item.resolvedMainSize item.resolvedCrossSize

        let rect := LayoutRect.mk' x y width height
        result := result.add (ComputedLayout.simple item.node.id rect)

  result

/-- Layout a single node and its children recursively. -/
partial def layoutNode (node : LayoutNode) (availableWidth availableHeight : Length)
    (offsetX offsetY : Length := 0) : LayoutResult := Id.run do
  let box := node.box

  -- Resolve node dimensions
  -- For containers with auto dimensions, use available space
  -- For leaf nodes, use content size
  let contentSize := getContentSize node
  let isContainer := !node.isLeaf
  let resolvedWidth := match box.width with
    | .auto => if isContainer then availableWidth else contentSize.1
    | dim => dim.resolve availableWidth contentSize.1
  let resolvedHeight := match box.height with
    | .auto => if isContainer then availableHeight else contentSize.2
    | dim => dim.resolve availableHeight contentSize.2
  let width := box.clampWidth resolvedWidth
  let height := box.clampHeight resolvedHeight

  -- Create layout for this node
  let nodeRect := LayoutRect.mk' offsetX offsetY width height
  let mut result := LayoutResult.empty.add (ComputedLayout.withPadding node.id nodeRect box.padding)

  -- Layout children based on container type
  match node.container with
  | .flex props =>
    let childResult := layoutFlexContainer props node.children width height box.padding
    -- Translate child results by node position
    let childResult := childResult.translate offsetX offsetY
    result := result.merge childResult

    -- Recursively layout any container children
    for child in node.children do
      if !child.isLeaf then
        if let some cl := childResult.get child.id then
          let grandchildResult := layoutNode child cl.borderRect.width cl.borderRect.height
                                  cl.borderRect.x cl.borderRect.y
          -- Only add grandchildren (child is already in result)
          for layout in grandchildResult.layouts do
            if layout.nodeId != child.id then
              result := result.add layout

  | .grid _props =>
    -- TODO: Implement grid layout
    -- For now, just place children at origin
    for child in node.children do
      let childResult := layoutNode child width height (offsetX + box.padding.left) (offsetY + box.padding.top)
      result := result.merge childResult

  | .none =>
    -- Leaf node, no children to layout
    pure ()

  result

/-- Main entry point: Layout a tree starting from the root. -/
def layout (root : LayoutNode) (availableWidth availableHeight : Length) : LayoutResult :=
  layoutNode root availableWidth availableHeight

end Afferent.Layout

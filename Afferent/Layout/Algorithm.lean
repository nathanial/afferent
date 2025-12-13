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

/-! ## Grid Item Intermediate State -/

/-- A resolved track (row or column) with computed size and position. -/
structure ResolvedTrack where
  size : TrackSize
  /-- Base size (from content or fixed). -/
  baseSize : Length := 0
  /-- Fr factor if flexible. -/
  frValue : Float := 0
  /-- Final resolved size. -/
  resolvedSize : Length := 0
  /-- Position (cumulative offset from start). -/
  position : Length := 0
deriving Repr, Inhabited

/-- Intermediate state for a grid item during layout. -/
structure GridItemState where
  node : LayoutNode
  /-- Resolved row start (0-indexed). -/
  rowStart : Nat
  /-- Resolved row end (exclusive, 0-indexed). -/
  rowEnd : Nat
  /-- Resolved column start (0-indexed). -/
  colStart : Nat
  /-- Resolved column end (exclusive, 0-indexed). -/
  colEnd : Nat
  /-- Content width of the item. -/
  contentWidth : Length
  /-- Content height of the item. -/
  contentHeight : Length
  /-- Final X position. -/
  resolvedX : Length := 0
  /-- Final Y position. -/
  resolvedY : Length := 0
  /-- Final width. -/
  resolvedWidth : Length := 0
  /-- Final height. -/
  resolvedHeight : Length := 0
deriving Repr, Inhabited

/-- Occupancy grid for auto-placement. -/
structure OccupancyGrid where
  /-- 2D array of occupied cells (row-major). -/
  cells : Array (Array Bool)
  rows : Nat
  cols : Nat
deriving Repr, Inhabited

namespace OccupancyGrid

/-- Create an empty occupancy grid. -/
def create (rows cols : Nat) : OccupancyGrid :=
  { cells := Array.mkArray rows (Array.mkArray cols false), rows, cols }

/-- Check if a cell is occupied. -/
def isOccupied (grid : OccupancyGrid) (row col : Nat) : Bool :=
  if h : row < grid.cells.size then
    let rowArr := grid.cells[row]
    if col < rowArr.size then rowArr[col]! else false
  else false

/-- Check if a range of cells is available. -/
def isAreaAvailable (grid : OccupancyGrid) (rowStart rowEnd colStart colEnd : Nat) : Bool :=
  (List.range (rowEnd - rowStart)).all fun dr =>
    (List.range (colEnd - colStart)).all fun dc =>
      !grid.isOccupied (rowStart + dr) (colStart + dc)

/-- Mark a range of cells as occupied. -/
def markOccupied (grid : OccupancyGrid) (rowStart rowEnd colStart colEnd : Nat) : OccupancyGrid := Id.run do
  let mut cells := grid.cells
  for r in [rowStart:rowEnd] do
    if h : r < cells.size then
      let mut row := cells[r]
      for c in [colStart:colEnd] do
        if c < row.size then
          row := row.set! c true
      cells := cells.set! r row
  { grid with cells }

/-- Extend grid to accommodate more rows. -/
def extendRows (grid : OccupancyGrid) (newRows : Nat) : OccupancyGrid :=
  if newRows <= grid.rows then grid
  else
    let additionalRows := newRows - grid.rows
    let newCells := grid.cells ++ Array.mkArray additionalRows (Array.mkArray grid.cols false)
    { grid with cells := newCells, rows := newRows }

end OccupancyGrid

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

/-! ## Grid Layout Functions -/

/-- Resolve a GridLine to a 0-indexed track index. -/
def resolveGridLine (line : GridLine) (trackCount : Nat) (isEnd : Bool) (spanCount : Nat := 1) : Nat :=
  match line with
  | .auto => if isEnd then spanCount else 0  -- Will be determined by auto-placement
  | .line n =>
    if n >= 0 then
      min (n.toNat - 1) trackCount  -- CSS lines are 1-indexed
    else
      -- Negative lines count from end (e.g., -1 = last line)
      let fromEnd := (-n).toNat
      if fromEnd <= trackCount + 1 then trackCount + 1 - fromEnd else 0
  | .span n => spanCount + n - 1  -- Span is relative to start
  | .named _ => 0  -- Named lines not yet supported

/-- Check if a GridSpan has explicit placement. -/
def hasExplicitPlacement (span : GridSpan) : Bool :=
  match span.start with
  | .auto => false
  | .line _ => true
  | .span _ => false  -- span alone is not explicit
  | .named _ => true

/-- Get the span count from a GridSpan. -/
def getSpanCount (span : GridSpan) : Nat :=
  match span.finish with
  | .span n => n
  | _ => 1

/-- Resolve a GridSpan to start and end indices. -/
def resolveGridSpan (span : GridSpan) (trackCount : Nat) (defaultStart : Nat := 0) : Nat × Nat :=
  let spanCount := getSpanCount span
  let startIdx := match span.start with
    | .auto => defaultStart
    | .line n =>
      if n >= 0 then min (n.toNat - 1) trackCount
      else
        let fromEnd := (-n).toNat
        if fromEnd <= trackCount + 1 then trackCount + 1 - fromEnd else 0
    | .span _ => defaultStart
    | .named _ => defaultStart
  let endIdx := match span.finish with
    | .auto => startIdx + 1
    | .line n =>
      if n >= 0 then min n.toNat (trackCount + 1)
      else
        let fromEnd := (-n).toNat
        if fromEnd <= trackCount then trackCount + 1 - fromEnd else startIdx + 1
    | .span n => startIdx + n
    | .named _ => startIdx + 1
  (startIdx, max (startIdx + 1) endIdx)

/-- Initialize ResolvedTrack array from a GridTemplate. -/
def initTracks (template : GridTemplate) (minCount : Nat) : Array ResolvedTrack := Id.run do
  let explicitCount := template.tracks.size
  let count := max explicitCount minCount
  let mut tracks : Array ResolvedTrack := #[]
  for i in [:count] do
    let size := if i < explicitCount then template.tracks[i]!.size else template.autoSize
    let frVal := match size with
      | .fr n => n
      | _ => 0
    tracks := tracks.push { size, frValue := frVal }
  tracks

/-- Resolve the base size of a track (for fixed/auto sizes). -/
def resolveTrackBaseSize (size : TrackSize) (available : Length) : Length :=
  match size with
  | .fixed dim => dim.resolve available 0
  | .fr _ => 0  -- Fr tracks start at 0, sized in fr resolution phase
  | .minmax min _ => resolveTrackBaseSize min available
  | .fitContent max => min max available

/-- Calculate the maximum content size for items in a given track. -/
def maxContentInTrack (items : Array GridItemState) (trackIdx : Nat) (isColumn : Bool) : Length :=
  items.foldl (fun acc item =>
    let inTrack := if isColumn then
      item.colStart <= trackIdx && trackIdx < item.colEnd
    else
      item.rowStart <= trackIdx && trackIdx < item.rowEnd
    if inTrack then
      -- For spanning items, divide content size by span count
      let span := if isColumn then item.colEnd - item.colStart else item.rowEnd - item.rowStart
      let size := if isColumn then item.contentWidth else item.contentHeight
      max acc (size / span.toFloat)
    else acc
  ) 0

/-- Size tracks based on content and fixed sizes. -/
def sizeTracksToContent (tracks : Array ResolvedTrack) (items : Array GridItemState)
    (isColumn : Bool) (available : Length) : Array ResolvedTrack := Id.run do
  let mut result := tracks
  for i in [:tracks.size] do
    let track := tracks[i]!
    let baseSize := match track.size with
      | .fixed .auto => maxContentInTrack items i isColumn
      | .fixed dim => dim.resolve available 0
      | .fr _ => 0
      | .minmax min _ => resolveTrackBaseSize min available
      | .fitContent max => min (maxContentInTrack items i isColumn) max
    result := result.set! i { track with baseSize, resolvedSize := baseSize }
  result

/-- Calculate total fr units in track array. -/
def totalFrUnits (tracks : Array ResolvedTrack) : Float :=
  tracks.foldl (fun acc t => acc + t.frValue) 0

/-- Distribute remaining space to fr tracks. -/
def resolveFrTracks (tracks : Array ResolvedTrack) (available : Length) (gap : Length) : Array ResolvedTrack := Id.run do
  -- Calculate space used by non-fr tracks
  let usedSpace := tracks.foldl (fun acc t =>
    if t.frValue > 0 then acc else acc + t.resolvedSize) 0
  let gaps := if tracks.size > 0 then gap * (tracks.size - 1).toFloat else 0
  let remaining := max 0 (available - usedSpace - gaps)

  let totalFr := totalFrUnits tracks
  if totalFr <= 0 then return tracks

  let spacePerFr := remaining / totalFr

  let mut result := tracks
  for i in [:tracks.size] do
    let track := tracks[i]!
    if track.frValue > 0 then
      let frSize := spacePerFr * track.frValue
      result := result.set! i { track with resolvedSize := frSize }
  result

/-- Calculate cumulative positions for tracks. -/
def calculateTrackPositions (tracks : Array ResolvedTrack) (gap : Length) (startOffset : Length := 0) : Array ResolvedTrack := Id.run do
  let mut result := tracks
  let mut pos := startOffset
  for i in [:tracks.size] do
    let track := tracks[i]!
    result := result.set! i { track with position := pos }
    pos := pos + track.resolvedSize + gap
  result

/-- Get the bounds of a grid area from track positions. -/
def getGridArea (rowTracks colTracks : Array ResolvedTrack)
    (rowStart rowEnd colStart colEnd : Nat) (rowGap colGap : Length) : Length × Length × Length × Length :=
  let x := if colStart < colTracks.size then colTracks[colStart]!.position else 0
  let y := if rowStart < rowTracks.size then rowTracks[rowStart]!.position else 0

  -- Width spans from colStart to colEnd (exclusive)
  let width := Id.run do
    let mut w : Length := 0
    for c in [colStart:colEnd] do
      if c < colTracks.size then
        w := w + colTracks[c]!.resolvedSize
        if c + 1 < colEnd then w := w + colGap
    w

  -- Height spans from rowStart to rowEnd (exclusive)
  let height := Id.run do
    let mut h : Length := 0
    for r in [rowStart:rowEnd] do
      if r < rowTracks.size then
        h := h + rowTracks[r]!.resolvedSize
        if r + 1 < rowEnd then h := h + rowGap
    h

  (x, y, width, height)

/-- Apply alignment within a cell. -/
def alignInCell (itemWidth itemHeight : Length)
    (cellX cellY cellWidth cellHeight : Length)
    (justifySelf alignSelf : AlignItems) : Length × Length × Length × Length :=
  let (x, w) := match justifySelf with
    | .stretch => (cellX, cellWidth)
    | .flexStart => (cellX, itemWidth)
    | .flexEnd => (cellX + cellWidth - itemWidth, itemWidth)
    | .center => (cellX + (cellWidth - itemWidth) / 2, itemWidth)
    | .baseline => (cellX, itemWidth)  -- Baseline = flexStart for now

  let (y, h) := match alignSelf with
    | .stretch => (cellY, cellHeight)
    | .flexStart => (cellY, itemHeight)
    | .flexEnd => (cellY + cellHeight - itemHeight, itemHeight)
    | .center => (cellY + (cellHeight - itemHeight) / 2, itemHeight)
    | .baseline => (cellY, itemHeight)

  (x, y, w, h)

/-- Auto-place an item in the grid. Returns updated item and occupancy grid. -/
def autoPlaceItem (item : GridItemState) (occupancy : OccupancyGrid)
    (flow : GridAutoFlow) (cols : Nat) : GridItemState × OccupancyGrid := Id.run do
  let rowSpan := item.rowEnd - item.rowStart
  let colSpan := item.colEnd - item.colStart

  -- Search for available position
  let mut occ := occupancy
  let mut foundRow := 0
  let mut foundCol := 0
  let mut found := false

  let maxRow := occ.rows + 10  -- Allow some implicit rows

  for r in [:maxRow] do
    if found then break
    for c in [:cols] do
      if found then break
      if c + colSpan <= cols then
        -- Extend grid if needed
        if r + rowSpan > occ.rows then
          occ := occ.extendRows (r + rowSpan)
        if occ.isAreaAvailable r (r + rowSpan) c (c + colSpan) then
          foundRow := r
          foundCol := c
          found := true

  -- Mark area as occupied
  if foundRow + rowSpan > occ.rows then
    occ := occ.extendRows (foundRow + rowSpan)
  occ := occ.markOccupied foundRow (foundRow + rowSpan) foundCol (foundCol + colSpan)

  let newItem := { item with
    rowStart := foundRow
    rowEnd := foundRow + rowSpan
    colStart := foundCol
    colEnd := foundCol + colSpan
  }
  (newItem, occ)

/-- Place all grid items, resolving explicit positions and auto-placing others. -/
def placeAllItems (items : Array GridItemState) (container : GridContainer)
    (explicitRows explicitCols : Nat) : Array GridItemState × Nat × Nat := Id.run do
  let cols := max 1 explicitCols
  let rows := max 1 explicitRows
  let mut placedItems : Array GridItemState := #[]
  let mut occupancy := OccupancyGrid.create rows cols
  let mut maxRow := rows
  let mut maxCol := cols

  -- First pass: place items with explicit positions
  for item in items do
    let placement := item.node.gridItem?.map (·.placement) |>.getD GridPlacement.auto
    let hasExplicitRow := hasExplicitPlacement placement.row
    let hasExplicitCol := hasExplicitPlacement placement.column

    if hasExplicitRow && hasExplicitCol then
      -- Both explicit - place directly
      let (rowStart, rowEnd) := resolveGridSpan placement.row maxRow
      let (colStart, colEnd) := resolveGridSpan placement.column maxCol
      maxRow := max maxRow rowEnd
      maxCol := max maxCol colEnd

      -- Extend occupancy if needed
      if maxRow > occupancy.rows then
        occupancy := occupancy.extendRows maxRow

      occupancy := occupancy.markOccupied rowStart rowEnd colStart colEnd
      placedItems := placedItems.push { item with rowStart, rowEnd, colStart, colEnd }
    else
      -- Will be auto-placed in second pass
      placedItems := placedItems.push item

  -- Second pass: auto-place remaining items
  let mut finalItems : Array GridItemState := #[]
  for item in placedItems do
    let placement := item.node.gridItem?.map (·.placement) |>.getD GridPlacement.auto
    let hasExplicitRow := hasExplicitPlacement placement.row
    let hasExplicitCol := hasExplicitPlacement placement.column

    if hasExplicitRow && hasExplicitCol then
      -- Already placed
      finalItems := finalItems.push item
    else
      -- Auto-place
      let rowSpan := getSpanCount placement.row
      let colSpan := getSpanCount placement.column
      let itemWithSpan := { item with
        rowStart := 0, rowEnd := rowSpan,
        colStart := 0, colEnd := colSpan
      }
      let (placed, newOcc) := autoPlaceItem itemWithSpan occupancy container.autoFlow maxCol
      occupancy := newOcc
      maxRow := max maxRow placed.rowEnd
      finalItems := finalItems.push placed

  (finalItems, maxRow, maxCol)

/-- Position all items in their grid cells. -/
def positionGridItems (items : Array GridItemState) (rowTracks colTracks : Array ResolvedTrack)
    (container : GridContainer) : Array GridItemState := Id.run do
  let mut result : Array GridItemState := #[]

  for item in items do
    -- Get cell bounds
    let (cellX, cellY, cellWidth, cellHeight) := getGridArea rowTracks colTracks
      item.rowStart item.rowEnd item.colStart item.colEnd
      container.rowGap container.columnGap

    -- Get alignment
    let gridItem := item.node.gridItem?.getD GridItem.default
    let justifySelf := gridItem.justifySelf.getD container.justifyItems
    let alignSelf := gridItem.alignSelf.getD container.alignItems

    -- Apply alignment
    let (x, y, w, h) := alignInCell item.contentWidth item.contentHeight
      cellX cellY cellWidth cellHeight justifySelf alignSelf

    result := result.push { item with resolvedX := x, resolvedY := y, resolvedWidth := w, resolvedHeight := h }

  result

/-- Layout a grid container. -/
def layoutGridContainer (container : GridContainer) (children : Array LayoutNode)
    (containerWidth containerHeight : Length)
    (padding : EdgeInsets) : LayoutResult := Id.run do
  -- Phase 1: Available space
  let (availableWidth, availableHeight) := computeAvailableSpace containerWidth containerHeight padding

  -- Get explicit track counts
  let explicitCols := container.templateColumns.tracks.size
  let explicitRows := container.templateRows.tracks.size

  -- Initialize items with content sizes
  let mut items : Array GridItemState := #[]
  for child in children do
    let contentSize := getContentSize child
    let placement := child.gridItem?.map (·.placement) |>.getD GridPlacement.auto
    let (rowStart, rowEnd) := resolveGridSpan placement.row explicitRows
    let (colStart, colEnd) := resolveGridSpan placement.column explicitCols
    items := items.push {
      node := child
      rowStart, rowEnd, colStart, colEnd
      contentWidth := contentSize.1
      contentHeight := contentSize.2
    }

  -- Phase 2: Place items (explicit + auto-placement)
  let minCols := max 1 explicitCols
  let (placedItems, actualRows, actualCols) := placeAllItems items container explicitRows minCols

  -- Phase 3: Initialize and size tracks
  let mut colTracks := initTracks container.templateColumns actualCols
  let mut rowTracks := initTracks container.templateRows actualRows

  -- Size tracks to content
  colTracks := sizeTracksToContent colTracks placedItems true availableWidth
  rowTracks := sizeTracksToContent rowTracks placedItems false availableHeight

  -- Phase 4: Resolve fr units
  colTracks := resolveFrTracks colTracks availableWidth container.columnGap
  rowTracks := resolveFrTracks rowTracks availableHeight container.rowGap

  -- Phase 5: Calculate positions
  colTracks := calculateTrackPositions colTracks container.columnGap padding.left
  rowTracks := calculateTrackPositions rowTracks container.rowGap padding.top

  -- Phase 6: Position items
  let positionedItems := positionGridItems placedItems rowTracks colTracks container

  -- Build result
  let mut result := LayoutResult.empty
  for item in positionedItems do
    let rect := LayoutRect.mk' item.resolvedX item.resolvedY item.resolvedWidth item.resolvedHeight
    result := result.add (ComputedLayout.simple item.node.id rect)

  result

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

  | .grid props =>
    let childResult := layoutGridContainer props node.children width height box.padding
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

  | .none =>
    -- Leaf node, no children to layout
    pure ()

  result

/-- Main entry point: Layout a tree starting from the root. -/
def layout (root : LayoutNode) (availableWidth availableHeight : Length) : LayoutResult :=
  layoutNode root availableWidth availableHeight

end Afferent.Layout

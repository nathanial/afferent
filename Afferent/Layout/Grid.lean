/-
  Afferent Grid Types
  CSS Grid container and item properties.
-/
import Afferent.Layout.Types
import Afferent.Layout.Flex  -- for AlignItems reuse

namespace Afferent.Layout

/-- Size of a grid track (row or column). -/
inductive TrackSize where
  | fixed (d : Dimension)           -- Fixed dimension (length, percent, auto)
  | fr (n : Float)                  -- Fractional unit (flexible space)
  | minmax (min max : TrackSize)    -- minmax(min, max) function
  | fitContent (max : Length)       -- fit-content(max) function
deriving Repr, BEq, Inhabited

namespace TrackSize

def auto : TrackSize := .fixed .auto
def px (n : Length) : TrackSize := .fixed (.length n)
def percent (p : Float) : TrackSize := .fixed (.percent p)

/-- Check if this track size uses fr units. -/
def isFr : TrackSize → Bool
  | .fr _ => true
  | .minmax _ max => max.isFr
  | _ => false

/-- Get the fr value if this is an fr track. -/
def frValue : TrackSize → Float
  | .fr n => n
  | _ => 0

end TrackSize

/-- A single grid track definition. -/
structure GridTrack where
  size : TrackSize := .fixed .auto
  name : Option String := none  -- Optional line name
deriving Repr, BEq, Inhabited

/-- Grid template for rows or columns. -/
structure GridTemplate where
  tracks : Array GridTrack := #[]
  autoSize : TrackSize := .fixed .auto  -- Size for auto-generated tracks
deriving Repr, BEq, Inhabited

namespace GridTemplate

def empty : GridTemplate := {}

/-- Create a template from track sizes. -/
def fromSizes (sizes : Array TrackSize) : GridTemplate :=
  { tracks := sizes.map (GridTrack.mk · none) }

/-- Create a template with n equal fr columns/rows. -/
def repeated (n : Nat) (size : TrackSize := .fr 1) : GridTemplate :=
  { tracks := (List.replicate n { size := size }).toArray }

/-- Create a template with specific pixel sizes. -/
def pixels (sizes : Array Length) : GridTemplate :=
  { tracks := sizes.map fun s => { size := .fixed (.length s) } }

end GridTemplate

/-- Reference to a grid line for item placement. -/
inductive GridLine where
  | auto               -- Automatic placement
  | line (n : Int)     -- Numeric line (1-indexed, negative from end)
  | span (n : Nat)     -- Span n tracks from start position
  | named (s : String) -- Named line reference
deriving Repr, BEq, Inhabited

/-- Placement of a grid item in one dimension. -/
structure GridSpan where
  start : GridLine := .auto
  finish : GridLine := .auto  -- Using 'finish' to avoid 'end' keyword
deriving Repr, BEq, Inhabited

namespace GridSpan

def auto : GridSpan := {}

/-- Place at specific line numbers. -/
def lines (startLine endLine : Int) : GridSpan :=
  { start := .line startLine, finish := .line endLine }

/-- Span a number of tracks from auto placement. -/
def spanTracks (n : Nat) : GridSpan :=
  { start := .auto, finish := .span n }

/-- Start at a line and span tracks. -/
def lineSpan (startLine : Int) (spanCount : Nat) : GridSpan :=
  { start := .line startLine, finish := .span spanCount }

end GridSpan

/-- Placement of a grid item. -/
structure GridPlacement where
  row : GridSpan := {}
  column : GridSpan := {}
deriving Repr, BEq, Inhabited

namespace GridPlacement

def auto : GridPlacement := {}

/-- Place at specific row and column lines. -/
def atPosition (row col : Int) : GridPlacement :=
  { row := { start := .line row }, column := { start := .line col } }

/-- Place spanning multiple rows and columns. -/
def span (rowSpan colSpan : Nat) : GridPlacement :=
  { row := GridSpan.spanTracks rowSpan, column := GridSpan.spanTracks colSpan }

end GridPlacement

/-- Direction for auto-placement of grid items. -/
inductive GridAutoFlow where
  | row         -- Fill rows first (default)
  | column      -- Fill columns first
  | rowDense    -- Fill rows, backfill gaps
  | columnDense -- Fill columns, backfill gaps
deriving Repr, BEq, Inhabited

/-- Properties for a grid container. -/
structure GridContainer where
  templateRows : GridTemplate := {}
  templateColumns : GridTemplate := {}
  rowGap : Length := 0
  columnGap : Length := 0
  justifyItems : AlignItems := .stretch   -- Alignment within cells (inline axis)
  alignItems : AlignItems := .stretch     -- Alignment within cells (block axis)
  justifyContent : JustifyContent := .flexStart  -- Alignment of grid in container
  alignContent : AlignContent := .flexStart
  autoFlow : GridAutoFlow := .row
  autoRows : TrackSize := .fixed .auto    -- Size for implicit rows
  autoColumns : TrackSize := .fixed .auto -- Size for implicit columns
deriving Repr, BEq, Inhabited

namespace GridContainer

def default : GridContainer := {}

/-- Create a simple grid with fixed column count. -/
def columns (n : Nat) (gap : Length := 0) : GridContainer :=
  { templateColumns := GridTemplate.repeated n
    columnGap := gap
    rowGap := gap }

/-- Create a grid with specific column sizes. -/
def withColumns (sizes : Array TrackSize) (gap : Length := 0) : GridContainer :=
  { templateColumns := GridTemplate.fromSizes sizes
    columnGap := gap
    rowGap := gap }

/-- Create a grid with specific row and column sizes. -/
def withTemplate (rows cols : Array TrackSize) (gap : Length := 0) : GridContainer :=
  { templateRows := GridTemplate.fromSizes rows
    templateColumns := GridTemplate.fromSizes cols
    rowGap := gap
    columnGap := gap }

end GridContainer

/-- Properties for a grid item (child of grid container). -/
structure GridItem where
  placement : GridPlacement := {}
  justifySelf : Option AlignItems := none  -- Override container's justifyItems
  alignSelf : Option AlignItems := none    -- Override container's alignItems
deriving Repr, BEq, Inhabited

namespace GridItem

def default : GridItem := {}

/-- Place at specific grid position. -/
def atPosition (row col : Int) : GridItem :=
  { placement := GridPlacement.atPosition row col }

/-- Place spanning multiple cells. -/
def span (rowSpan colSpan : Nat) : GridItem :=
  { placement := GridPlacement.span rowSpan colSpan }

end GridItem

end Afferent.Layout

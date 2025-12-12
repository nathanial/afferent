/-
  Afferent Flexbox Types
  CSS Flexbox container and item properties.
-/
import Afferent.Layout.Types

namespace Afferent.Layout

/-- Direction of the main axis for flex layout. -/
inductive FlexDirection where
  | row           -- Main axis horizontal, left to right (default)
  | rowReverse    -- Main axis horizontal, right to left
  | column        -- Main axis vertical, top to bottom
  | columnReverse -- Main axis vertical, bottom to top
deriving Repr, BEq, Inhabited

namespace FlexDirection

def isHorizontal : FlexDirection → Bool
  | .row | .rowReverse => true
  | .column | .columnReverse => false

def isReversed : FlexDirection → Bool
  | .rowReverse | .columnReverse => true
  | .row | .column => false

end FlexDirection

/-- Whether flex items wrap to new lines. -/
inductive FlexWrap where
  | nowrap      -- Single line, may overflow (default)
  | wrap        -- Wrap to new lines in normal direction
  | wrapReverse -- Wrap to new lines in reverse direction
deriving Repr, BEq, Inhabited

/-- Distribution of space along the main axis. -/
inductive JustifyContent where
  | flexStart    -- Pack items toward start (default)
  | flexEnd      -- Pack items toward end
  | center       -- Center items
  | spaceBetween -- Even space between items, no space at edges
  | spaceAround  -- Even space around items (half space at edges)
  | spaceEvenly  -- Equal space between items and edges
deriving Repr, BEq, Inhabited

/-- Alignment of items along the cross axis. -/
inductive AlignItems where
  | flexStart -- Align to cross axis start
  | flexEnd   -- Align to cross axis end
  | center    -- Center on cross axis
  | stretch   -- Stretch to fill cross axis (default)
  | baseline  -- Align baselines (simplified as flexStart)
deriving Repr, BEq, Inhabited

/-- Alignment of wrapped lines along the cross axis. -/
inductive AlignContent where
  | flexStart    -- Pack lines toward start
  | flexEnd      -- Pack lines toward end
  | center       -- Center lines
  | stretch      -- Stretch lines to fill (default)
  | spaceBetween -- Even space between lines
  | spaceAround  -- Even space around lines
  | spaceEvenly  -- Equal space everywhere
deriving Repr, BEq, Inhabited

/-- Properties for a flex container. -/
structure FlexContainer where
  direction : FlexDirection := .row
  wrap : FlexWrap := .nowrap
  justifyContent : JustifyContent := .flexStart
  alignItems : AlignItems := .stretch
  alignContent : AlignContent := .stretch
  gap : Length := 0          -- Gap between items on main axis
  rowGap : Length := 0       -- Gap between lines on cross axis (when wrapped)
deriving Repr, BEq, Inhabited

namespace FlexContainer

def default : FlexContainer := {}

/-- Create a row flex container with optional gap. -/
def row (gap : Length := 0) : FlexContainer :=
  { direction := .row, gap := gap }

/-- Create a column flex container with optional gap. -/
def column (gap : Length := 0) : FlexContainer :=
  { direction := .column, gap := gap }

/-- Create a wrapping row container. -/
def rowWrap (gap : Length := 0) (rowGap : Length := 0) : FlexContainer :=
  { direction := .row, wrap := .wrap, gap := gap, rowGap := rowGap }

/-- Create a centered container. -/
def centered : FlexContainer :=
  { justifyContent := .center, alignItems := .center }

end FlexContainer

/-- Properties for a flex item (child of flex container). -/
structure FlexItem where
  grow : Float := 0          -- How much to grow relative to siblings (0 = don't grow)
  shrink : Float := 1        -- How much to shrink relative to siblings (1 = normal)
  basis : Dimension := .auto -- Initial main size before grow/shrink
  alignSelf : Option AlignItems := none  -- Override container's alignItems
deriving Repr, BEq, Inhabited

namespace FlexItem

def default : FlexItem := {}

/-- Create a flex item that grows to fill available space. -/
def growing (factor : Float := 1) : FlexItem :=
  { grow := factor }

/-- Create a flex item with specific grow and shrink factors. -/
def flexing (growFactor shrinkFactor : Float) : FlexItem :=
  { grow := growFactor, shrink := shrinkFactor }

/-- Create a flex item with fixed basis size. -/
def fixed (size : Length) : FlexItem :=
  { grow := 0, shrink := 0, basis := .length size }

end FlexItem

end Afferent.Layout

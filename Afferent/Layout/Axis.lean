/-
  Afferent Layout Axis
  Abstraction for main/cross axis operations in flexbox layout.
-/
import Afferent.Layout.Types
import Afferent.Layout.Flex

namespace Afferent.Layout

/-- Information about the main and cross axes derived from flex direction. -/
structure AxisInfo where
  isHorizontal : Bool  -- Is the main axis horizontal?
  isReversed : Bool    -- Is the main axis reversed?
deriving Repr, BEq, Inhabited

namespace AxisInfo

/-- Default axis info (horizontal, not reversed). -/
def default : AxisInfo := ⟨true, false⟩

/-- Create axis info from flex direction. -/
def fromDirection (dir : FlexDirection) : AxisInfo :=
  ⟨dir.isHorizontal, dir.isReversed⟩

/-- Row layout axis (horizontal, left to right). -/
def row : AxisInfo := ⟨true, false⟩

/-- Row-reverse layout axis (horizontal, right to left). -/
def rowReverse : AxisInfo := ⟨true, true⟩

/-- Column layout axis (vertical, top to bottom). -/
def column : AxisInfo := ⟨false, false⟩

/-- Column-reverse layout axis (vertical, bottom to top). -/
def columnReverse : AxisInfo := ⟨false, true⟩

/-! ## Size Operations -/

/-- Get main axis size from width and height. -/
def mainSize (a : AxisInfo) (width height : Length) : Length :=
  if a.isHorizontal then width else height

/-- Get cross axis size from width and height. -/
def crossSize (a : AxisInfo) (width height : Length) : Length :=
  if a.isHorizontal then height else width

/-- Create width and height from main and cross sizes. -/
def toWidthHeight (a : AxisInfo) (main cross : Length) : Length × Length :=
  if a.isHorizontal then (main, cross) else (cross, main)

/-- Get main axis size from a size pair. -/
def mainFromPair (a : AxisInfo) (size : Length × Length) : Length :=
  if a.isHorizontal then size.1 else size.2

/-- Get cross axis size from a size pair. -/
def crossFromPair (a : AxisInfo) (size : Length × Length) : Length :=
  if a.isHorizontal then size.2 else size.1

/-! ## Position Operations -/

/-- Get main axis position from x and y. -/
def mainPos (a : AxisInfo) (x y : Length) : Length :=
  if a.isHorizontal then x else y

/-- Get cross axis position from x and y. -/
def crossPos (a : AxisInfo) (x y : Length) : Length :=
  if a.isHorizontal then y else x

/-- Create x and y from main and cross positions. -/
def toXY (a : AxisInfo) (main cross : Length) : Length × Length :=
  if a.isHorizontal then (main, cross) else (cross, main)

/-! ## Edge Inset Operations -/

/-- Get main axis start inset (left or top). -/
def mainStart (a : AxisInfo) (insets : EdgeInsets) : Length :=
  if a.isHorizontal then insets.left else insets.top

/-- Get main axis end inset (right or bottom). -/
def mainEnd (a : AxisInfo) (insets : EdgeInsets) : Length :=
  if a.isHorizontal then insets.right else insets.bottom

/-- Get cross axis start inset (top or left). -/
def crossStart (a : AxisInfo) (insets : EdgeInsets) : Length :=
  if a.isHorizontal then insets.top else insets.left

/-- Get cross axis end inset (bottom or right). -/
def crossEnd (a : AxisInfo) (insets : EdgeInsets) : Length :=
  if a.isHorizontal then insets.bottom else insets.right

/-- Get total main axis insets. -/
def mainInsets (a : AxisInfo) (insets : EdgeInsets) : Length :=
  a.mainStart insets + a.mainEnd insets

/-- Get total cross axis insets. -/
def crossInsets (a : AxisInfo) (insets : EdgeInsets) : Length :=
  a.crossStart insets + a.crossEnd insets

/-! ## Dimension Operations -/

/-- Get main axis dimension from box constraints. -/
def mainDimension (a : AxisInfo) (box : BoxConstraints) : Dimension :=
  if a.isHorizontal then box.width else box.height

/-- Get cross axis dimension from box constraints. -/
def crossDimension (a : AxisInfo) (box : BoxConstraints) : Dimension :=
  if a.isHorizontal then box.height else box.width

/-- Get main axis min constraint. -/
def mainMin (a : AxisInfo) (box : BoxConstraints) : Length :=
  if a.isHorizontal then box.minWidth else box.minHeight

/-- Get main axis max constraint. -/
def mainMax (a : AxisInfo) (box : BoxConstraints) : Option Length :=
  if a.isHorizontal then box.maxWidth else box.maxHeight

/-- Get cross axis min constraint. -/
def crossMin (a : AxisInfo) (box : BoxConstraints) : Length :=
  if a.isHorizontal then box.minHeight else box.minWidth

/-- Get cross axis max constraint. -/
def crossMax (a : AxisInfo) (box : BoxConstraints) : Option Length :=
  if a.isHorizontal then box.maxHeight else box.maxWidth

/-! ## Reversal Handling -/

/-- Apply reversal to a position within a container. -/
def reversePos (a : AxisInfo) (containerSize itemSize pos : Length) : Length :=
  if a.isReversed then containerSize - itemSize - pos else pos

/-- Reverse an array if the axis is reversed. -/
def reverseArray (a : AxisInfo) (arr : Array α) : Array α :=
  if a.isReversed then arr.reverse else arr

end AxisInfo

end Afferent.Layout

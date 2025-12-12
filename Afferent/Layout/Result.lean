/-
  Afferent Layout Result
  Output types from layout computation.
-/
import Afferent.Layout.Types
import Afferent.Core.Types

namespace Afferent.Layout

/-- A computed rectangle with position and size. -/
structure LayoutRect where
  x : Length
  y : Length
  width : Length
  height : Length
deriving Repr, BEq, Inhabited

namespace LayoutRect

def zero : LayoutRect := ⟨0, 0, 0, 0⟩

def mk' (x y width height : Length) : LayoutRect := ⟨x, y, width, height⟩

/-- Create from position and size. -/
def fromPosSize (x y : Length) (width height : Length) : LayoutRect :=
  ⟨x, y, width, height⟩

/-- Get the right edge x coordinate. -/
def right (r : LayoutRect) : Length := r.x + r.width

/-- Get the bottom edge y coordinate. -/
def bottom (r : LayoutRect) : Length := r.y + r.height

/-- Get the center point. -/
def center (r : LayoutRect) : Length × Length :=
  (r.x + r.width / 2, r.y + r.height / 2)

/-- Check if a point is inside this rect. -/
def contains (r : LayoutRect) (px py : Length) : Bool :=
  px >= r.x && px <= r.right && py >= r.y && py <= r.bottom

/-- Translate by an offset. -/
def translate (r : LayoutRect) (dx dy : Length) : LayoutRect :=
  ⟨r.x + dx, r.y + dy, r.width, r.height⟩

/-- Inset by edge amounts. -/
def inset (r : LayoutRect) (insets : EdgeInsets) : LayoutRect :=
  ⟨r.x + insets.left,
   r.y + insets.top,
   r.width - insets.horizontal,
   r.height - insets.vertical⟩

/-- Expand by edge amounts (opposite of inset). -/
def expand (r : LayoutRect) (insets : EdgeInsets) : LayoutRect :=
  ⟨r.x - insets.left,
   r.y - insets.top,
   r.width + insets.horizontal,
   r.height + insets.vertical⟩

/-- Convert to Afferent.Rect for rendering. -/
def toAfferentRect (r : LayoutRect) : Afferent.Rect :=
  Afferent.Rect.mk' r.x r.y r.width r.height

/-- Create from Afferent.Rect. -/
def fromAfferentRect (r : Afferent.Rect) : LayoutRect :=
  ⟨r.x, r.y, r.width, r.height⟩

end LayoutRect

/-- Computed layout for a single node. -/
structure ComputedLayout where
  nodeId : Nat
  /-- Border box (includes margin area for positioning). -/
  borderRect : LayoutRect
  /-- Content box (actual drawable area after padding). -/
  contentRect : LayoutRect
deriving Repr, BEq, Inhabited

namespace ComputedLayout

/-- Create a computed layout with same rect for border and content. -/
def simple (nodeId : Nat) (rect : LayoutRect) : ComputedLayout :=
  ⟨nodeId, rect, rect⟩

/-- Create with insets applied. -/
def withPadding (nodeId : Nat) (rect : LayoutRect) (padding : EdgeInsets) : ComputedLayout :=
  ⟨nodeId, rect, rect.inset padding⟩

/-- Get the main rect for drawing. -/
def rect (cl : ComputedLayout) : LayoutRect := cl.borderRect

/-- Get x position. -/
def x (cl : ComputedLayout) : Length := cl.borderRect.x

/-- Get y position. -/
def y (cl : ComputedLayout) : Length := cl.borderRect.y

/-- Get width. -/
def width (cl : ComputedLayout) : Length := cl.borderRect.width

/-- Get height. -/
def height (cl : ComputedLayout) : Length := cl.borderRect.height

end ComputedLayout

/-- Complete layout result for a tree. -/
structure LayoutResult where
  layouts : Array ComputedLayout
deriving Repr, Inhabited

namespace LayoutResult

def empty : LayoutResult := ⟨#[]⟩

/-- Find layout by node ID. -/
def get (r : LayoutResult) (nodeId : Nat) : Option ComputedLayout :=
  r.layouts.find? (·.nodeId == nodeId)

/-- Get layout, panicking if not found. -/
def get! (r : LayoutResult) (nodeId : Nat) : ComputedLayout :=
  match r.get nodeId with
  | some cl => cl
  | none => panic! s!"Layout not found for node {nodeId}"

/-- Add a computed layout. -/
def add (r : LayoutResult) (cl : ComputedLayout) : LayoutResult :=
  ⟨r.layouts.push cl⟩

/-- Merge with another result. -/
def merge (r1 r2 : LayoutResult) : LayoutResult :=
  ⟨r1.layouts ++ r2.layouts⟩

/-- Get all rects for rendering. -/
def allRects (r : LayoutResult) : Array LayoutRect :=
  r.layouts.map (·.borderRect)

/-- Map over all layouts. -/
def map (r : LayoutResult) (f : ComputedLayout → ComputedLayout) : LayoutResult :=
  ⟨r.layouts.map f⟩

/-- Translate all layouts by an offset. -/
def translate (r : LayoutResult) (dx dy : Length) : LayoutResult :=
  r.map fun cl =>
    { cl with
      borderRect := cl.borderRect.translate dx dy
      contentRect := cl.contentRect.translate dx dy }

/-- Number of layouts. -/
def size (r : LayoutResult) : Nat := r.layouts.size

end LayoutResult

end Afferent.Layout

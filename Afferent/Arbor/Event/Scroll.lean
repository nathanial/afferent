/-
  Arbor Widget Scroll
  Scroll state management helpers.
-/
import Afferent.Arbor.Widget.Core

namespace Afferent.Arbor

/-- Scroll bounds (min and max scroll positions). -/
structure ScrollBounds where
  minX : Float := 0
  minY : Float := 0
  maxX : Float := 0
  maxY : Float := 0
deriving Repr, BEq, Inhabited

namespace ScrollBounds

/-- Calculate scroll bounds from viewport and content sizes.
    Returns bounds where (0, 0) is minimum and max is how far content extends beyond viewport. -/
def calculate (viewportWidth viewportHeight : Float) (contentWidth contentHeight : Float) : ScrollBounds :=
  { minX := 0
    minY := 0
    maxX := max 0 (contentWidth - viewportWidth)
    maxY := max 0 (contentHeight - viewportHeight) }

/-- Check if scrolling is needed (content exceeds viewport). -/
def needsScrollX (b : ScrollBounds) : Bool := b.maxX > 0
def needsScrollY (b : ScrollBounds) : Bool := b.maxY > 0
def needsScroll (b : ScrollBounds) : Bool := b.needsScrollX || b.needsScrollY

end ScrollBounds

namespace ScrollState

/-- Calculate scroll bounds for this state given viewport and content sizes. -/
def bounds (viewportWidth viewportHeight : Float) (contentWidth contentHeight : Float) : ScrollBounds :=
  ScrollBounds.calculate viewportWidth viewportHeight contentWidth contentHeight

/-- Clamp scroll position to valid range. -/
def clamp (state : ScrollState) (viewportWidth viewportHeight : Float)
    (contentWidth contentHeight : Float) : ScrollState :=
  let b := bounds viewportWidth viewportHeight contentWidth contentHeight
  { offsetX := min b.maxX (max b.minX state.offsetX)
    offsetY := min b.maxY (max b.minY state.offsetY) }

/-- Update scroll position by a delta, clamping to valid range. -/
def scrollBy (dx dy : Float) (state : ScrollState)
    (viewportWidth viewportHeight : Float)
    (contentWidth contentHeight : Float) : ScrollState :=
  let newState : ScrollState := { offsetX := state.offsetX + dx, offsetY := state.offsetY + dy }
  newState.clamp viewportWidth viewportHeight contentWidth contentHeight

/-- Scroll to make a specific position visible.
    Returns updated scroll state that ensures the point (x, y) is within the viewport. -/
def scrollToVisible (x y : Float) (state : ScrollState)
    (viewportWidth viewportHeight : Float)
    (contentWidth contentHeight : Float) : ScrollState :=
  -- Horizontal: ensure x is in [offsetX, offsetX + viewportWidth]
  let newX :=
    if x < state.offsetX then x
    else if x > state.offsetX + viewportWidth then x - viewportWidth
    else state.offsetX

  -- Vertical: ensure y is in [offsetY, offsetY + viewportHeight]
  let newY :=
    if y < state.offsetY then y
    else if y > state.offsetY + viewportHeight then y - viewportHeight
    else state.offsetY

  let newState : ScrollState := { offsetX := newX, offsetY := newY }
  newState.clamp viewportWidth viewportHeight contentWidth contentHeight

/-- Scroll to make a rectangle visible.
    Returns updated scroll state that ensures the rect is fully within the viewport
    (or as much as possible if rect is larger than viewport). -/
def scrollRectToVisible (rectX rectY rectWidth rectHeight : Float) (state : ScrollState)
    (viewportWidth viewportHeight : Float)
    (contentWidth contentHeight : Float) : ScrollState :=
  -- Horizontal adjustment
  let newX :=
    if rectX < state.offsetX then
      -- Rect starts before viewport, scroll left
      rectX
    else if rectX + rectWidth > state.offsetX + viewportWidth then
      -- Rect ends after viewport, scroll right (but not past rect start)
      max rectX (rectX + rectWidth - viewportWidth)
    else
      state.offsetX

  -- Vertical adjustment
  let newY :=
    if rectY < state.offsetY then
      -- Rect starts before viewport, scroll up
      rectY
    else if rectY + rectHeight > state.offsetY + viewportHeight then
      -- Rect ends after viewport, scroll down (but not past rect start)
      max rectY (rectY + rectHeight - viewportHeight)
    else
      state.offsetY

  let newState : ScrollState := { offsetX := newX, offsetY := newY }
  newState.clamp viewportWidth viewportHeight contentWidth contentHeight

/-- Scroll to top-left (reset scroll position). -/
def scrollToTop (_state : ScrollState) : ScrollState :=
  { offsetX := 0, offsetY := 0 }

/-- Scroll to bottom. -/
def scrollToBottom (_state : ScrollState)
    (viewportWidth viewportHeight : Float)
    (contentWidth contentHeight : Float) : ScrollState :=
  let b := bounds viewportWidth viewportHeight contentWidth contentHeight
  { offsetX := 0, offsetY := b.maxY }

/-- Get scroll percentage (0.0 to 1.0) for vertical scrollbar. -/
def scrollPercentY (state : ScrollState)
    (viewportWidth viewportHeight : Float)
    (contentWidth contentHeight : Float) : Float :=
  let b := bounds viewportWidth viewportHeight contentWidth contentHeight
  if b.maxY <= 0 then 0 else state.offsetY / b.maxY

/-- Get scroll percentage (0.0 to 1.0) for horizontal scrollbar. -/
def scrollPercentX (state : ScrollState)
    (viewportWidth viewportHeight : Float)
    (contentWidth contentHeight : Float) : Float :=
  let b := bounds viewportWidth viewportHeight contentWidth contentHeight
  if b.maxX <= 0 then 0 else state.offsetX / b.maxX

end ScrollState

end Afferent.Arbor

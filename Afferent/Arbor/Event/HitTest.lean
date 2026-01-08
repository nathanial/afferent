/-
  Arbor Widget Hit Testing
  Map screen coordinates to widget IDs with proper Z-order.
-/
import Afferent.Arbor.Widget.Core
import Trellis

namespace Afferent.Arbor

/-- Result of hit testing. -/
structure HitTestResult where
  /-- The hit widget ID (topmost widget at coordinates). -/
  widgetId : WidgetId
  /-- Path from root to hit widget (for bubbling). -/
  path : Array WidgetId
  /-- The widget's computed layout. -/
  layout : Trellis.ComputedLayout
deriving Repr, Inhabited

/-- Scroll offset for coordinate adjustment in nested scroll containers. -/
structure ScrollOffset where
  x : Float := 0
  y : Float := 0
deriving Repr, Inhabited

namespace ScrollOffset

def zero : ScrollOffset := {}

def add (a b : ScrollOffset) : ScrollOffset :=
  { x := a.x + b.x, y := a.y + b.y }

end ScrollOffset

/-- Transform for hit testing that includes scroll offset and scale.
    Used to track cumulative transforms when descending into containers. -/
structure HitTransform where
  scrollX : Float := 0
  scrollY : Float := 0
  /-- Scale factors (1.0 = no scale). Applied after scroll adjustment. -/
  scaleX : Float := 1.0
  scaleY : Float := 1.0
  /-- Origin for scale transform (childBounds position of scaled children). -/
  scaleOriginX : Float := 0
  scaleOriginY : Float := 0
  /-- Offset within scaled container (contentRect.x + offsetX - scaleOriginX). -/
  scaleOffsetX : Float := 0
  scaleOffsetY : Float := 0
deriving Repr, Inhabited

namespace HitTransform

def zero : HitTransform := {}

/-- Add scroll offset. -/
def addScroll (t : HitTransform) (dx dy : Float) : HitTransform :=
  { t with scrollX := t.scrollX + dx, scrollY := t.scrollY + dy }

/-- Apply scale transform from a scaled container.
    childBoundsOrigin is the top-left of the children's bounding box,
    which may differ from contentRect if children overflow. -/
def withScale (t : HitTransform) (m : Trellis.ScaleMetadata)
    (contentRect : Trellis.LayoutRect) (childBoundsOriginX childBoundsOriginY : Float)
    : HitTransform :=
  { t with
    scaleX := m.scaleX
    scaleY := m.scaleY
    scaleOriginX := childBoundsOriginX
    scaleOriginY := childBoundsOriginY
    scaleOffsetX := contentRect.x + m.offsetX - childBoundsOriginX
    scaleOffsetY := contentRect.y + m.offsetY - childBoundsOriginY }

/-- Transform screen coordinates to child coordinates.
    Applies scroll adjustment, then inverts the scale transform. -/
def transformPoint (t : HitTransform) (x y : Float) : Float × Float :=
  -- First apply scroll offset
  let scrolledX := x + t.scrollX
  let scrolledY := y + t.scrollY
  -- Then invert scale transform:
  -- Rendering does: translate(cx + ox, cy + oy) * scale(sx, sy) * translate(-childBoundsX, -childBoundsY) * p
  -- Inverse: ((screen - cx - ox) / sx) + childBoundsX, ((screen - cy - oy) / sy) + childBoundsY
  if t.scaleX == 1.0 && t.scaleY == 1.0 &&
      t.scaleOffsetX == 0.0 && t.scaleOffsetY == 0.0 then
    (scrolledX, scrolledY)
  else
    let localX := (scrolledX - t.scaleOriginX - t.scaleOffsetX) / t.scaleX + t.scaleOriginX
    let localY := (scrolledY - t.scaleOriginY - t.scaleOffsetY) / t.scaleY + t.scaleOriginY
    (localX, localY)

end HitTransform

def isAbsoluteWidgetForHit (w : Widget) : Bool :=
  match w.style? with
  | some style => style.position == .absolute
  | none => false

def orderChildrenForHit (children : Array Widget) : Array Widget := Id.run do
  let mut flow : Array Widget := #[]
  let mut abs : Array Widget := #[]
  for child in children do
    if isAbsoluteWidgetForHit child then
      abs := abs.push child
    else
      flow := flow.push child
  flow ++ abs

/-- Compute the bounding box origin of children from their layouts. -/
def computeChildBoundsOrigin (children : Array Widget) (layouts : Trellis.LayoutResult)
    : Float × Float :=
  let (minX, minY) := children.foldl (init := (1000000.0, 1000000.0))
    fun (minX, minY) child =>
      match layouts.get child.id with
      | some computed =>
        let r := computed.borderRect
        (min minX r.x, min minY r.y)
      | none => (minX, minY)
  if minX == 1000000.0 then (0.0, 0.0) else (minX, minY)

/-- Check if a point is inside the hit area of a scaled container.
    Handles both hitArea modes (scaled vs container). -/
def isInsideScaledHitArea (layout : Trellis.ComputedLayout) (m : Trellis.ScaleMetadata)
    (adjX adjY : Float) : Bool :=
  match m.hitArea with
  | .scaled =>
    -- Check if point is inside the scaled content bounds
    let scaledX := layout.contentRect.x + m.offsetX
    let scaledY := layout.contentRect.y + m.offsetY
    let scaledW := m.intrinsicWidth * m.scaleX
    let scaledH := m.intrinsicHeight * m.scaleY
    adjX >= scaledX && adjX <= scaledX + scaledW &&
    adjY >= scaledY && adjY <= scaledY + scaledH
  | .container =>
    -- Use the full container bounds
    layout.borderRect.contains adjX adjY

/-- Information about an absolute positioned widget for priority hit testing. -/
structure AbsoluteWidgetInfo where
  widget : Widget
  path : Array WidgetId
  transform : HitTransform
deriving Inhabited

/-- Collect all absolute positioned widgets from the tree with their paths.
    Returns them in document order (later = rendered on top). -/
partial def collectAbsoluteWidgets (widget : Widget) (layouts : Trellis.LayoutResult)
    : Array AbsoluteWidgetInfo :=
  collectHelper widget #[] HitTransform.zero
where
  collectHelper (w : Widget) (path : Array WidgetId) (transform : HitTransform)
      : Array AbsoluteWidgetInfo :=
    let currentPath := path.push w.id

    -- Compute child transform
    let childTransform := match layouts.get w.id with
      | some layout =>
        match w with
        | .scroll _ _ _ scrollState _ _ _ =>
          transform.addScroll scrollState.offsetX scrollState.offsetY
        | .flex _ _ _ _ children | .grid _ _ _ _ children =>
          match layout.scaleMetadata with
          | some m =>
            let (boundsX, boundsY) := computeChildBoundsOrigin children layouts
            transform.withScale m layout.contentRect boundsX boundsY
          | none => transform
        | _ => transform
      | none => transform

    -- Collect from children, keeping absolute widgets separate
    w.children.foldl (init := #[]) fun acc child =>
      -- Recursively collect from child
      let childAbsolutes := collectHelper child currentPath childTransform
      let acc := acc ++ childAbsolutes
      -- If this child is absolute, add it to the result (after its children for z-order)
      if isAbsoluteWidgetForHit child then
        acc.push { widget := child, path := currentPath, transform := childTransform }
      else
        acc

/-- Perform hit testing on a widget tree.
    Returns the topmost widget at (x, y) in canvas coordinates.

    Z-order is determined by render order: children are rendered after parents,
    and later children are rendered after earlier children (thus appear on top).

    Absolute positioned elements are rendered on top of flow siblings, so we
    check all absolute elements first (in reverse document order for z-priority),
    then fall back to normal tree traversal. -/
partial def hitTest (widget : Widget) (layouts : Trellis.LayoutResult)
    (x y : Float) : Option HitTestResult :=
  -- First pass: check all absolute positioned widgets (they render on top)
  let absolutes := collectAbsoluteWidgets widget layouts
  -- Check in reverse order (last in document = topmost)
  let rec checkAbsolutes (i : Nat) : Option HitTestResult :=
    if i >= absolutes.size then
      none
    else
      let idx := absolutes.size - 1 - i
      match absolutes[idx]? with
      | some info =>
        match hitTestAbsolute info.widget layouts x y info.path info.transform with
        | some result => some result
        | none => checkAbsolutes (i + 1)
      | none => checkAbsolutes (i + 1)

  match checkAbsolutes 0 with
  | some result => some result
  | none =>
    -- Second pass: normal tree traversal (excluding absolute widgets we already checked)
    hitTestHelper widget layouts x y #[] HitTransform.zero false
where
  /-- Hit test an absolute widget and its children. -/
  hitTestAbsolute (w : Widget) (layouts : Trellis.LayoutResult)
      (x y : Float) (parentPath : Array WidgetId) (transform : HitTransform)
      : Option HitTestResult := do
    let layout ← layouts.get w.id
    let (adjX, adjY) := transform.transformPoint x y

    -- Check if point is inside this absolute widget's bounds
    let inside := match layout.scaleMetadata with
      | some m => isInsideScaledHitArea layout m adjX adjY
      | none =>
        match w with
        | .custom _ _ _ spec =>
            match spec.hitTest with
            | some hit => hit layout ⟨adjX, adjY⟩
            | none => layout.borderRect.contains adjX adjY
        | _ => layout.borderRect.contains adjX adjY

    if !inside then
      none

    let currentPath := parentPath.push w.id

    -- Compute child transform
    let childTransform := match w with
      | .scroll _ _ _ scrollState _ _ _ =>
        transform.addScroll scrollState.offsetX scrollState.offsetY
      | .flex _ _ _ _ children | .grid _ _ _ _ children =>
        match layout.scaleMetadata with
        | some m =>
          let (boundsX, boundsY) := computeChildBoundsOrigin children layouts
          transform.withScale m layout.contentRect boundsX boundsY
        | none => transform
      | _ => transform

    -- Check children (use normal hit test helper for children)
    let children := orderChildrenForHit w.children
    let rec checkChildren (i : Nat) : Option HitTestResult :=
      if i >= children.size then
        none
      else
        let childIdx := children.size - 1 - i
        match children[childIdx]? with
        | some child =>
          match hitTestHelper child layouts x y currentPath childTransform true with
          | some result => some result
          | none => checkChildren (i + 1)
        | none => checkChildren (i + 1)

    match checkChildren 0 with
    | some result => some result
    | none => some { widgetId := w.id, path := currentPath, layout }

  /-- Normal tree traversal hit test.
      skipAbsolute: if true, skip absolute widgets (they were already checked in first pass) -/
  hitTestHelper (w : Widget) (layouts : Trellis.LayoutResult)
      (x y : Float) (path : Array WidgetId) (transform : HitTransform)
      (skipAbsolute : Bool) : Option HitTestResult := do
    -- Get this widget's layout
    let layout ← layouts.get w.id

    -- Transform coordinates using current transform
    let (adjX, adjY) := transform.transformPoint x y

    -- Check if point is within this widget's bounds
    let inside := match layout.scaleMetadata with
      | some m => isInsideScaledHitArea layout m adjX adjY
      | none =>
        match w with
        | .custom _ _ _ spec =>
            match spec.hitTest with
            | some hit => hit layout ⟨adjX, adjY⟩
            | none => layout.borderRect.contains adjX adjY
        | _ => layout.borderRect.contains adjX adjY

    -- Clip to bounds (restore original behavior for normal traversal)
    if !inside then
      none

    let currentPath := path.push w.id

    -- Compute child transform based on widget type and scale metadata
    let childTransform := match w with
      | .scroll _ _ _ scrollState _ _ _ =>
        transform.addScroll scrollState.offsetX scrollState.offsetY
      | .flex _ _ _ _ children | .grid _ _ _ _ children =>
        match layout.scaleMetadata with
        | some m =>
          let (boundsX, boundsY) := computeChildBoundsOrigin children layouts
          transform.withScale m layout.contentRect boundsX boundsY
        | none => transform
      | _ => transform

    -- Check children in reverse order (last rendered = topmost)
    -- Skip absolute children if we already checked them in the first pass
    let children := if skipAbsolute then
      w.children.filter (fun c => !isAbsoluteWidgetForHit c)
    else
      orderChildrenForHit w.children

    let rec checkChildren (i : Nat) : Option HitTestResult :=
      if i >= children.size then
        none
      else
        let childIdx := children.size - 1 - i
        match children[childIdx]? with
        | some child =>
          match hitTestHelper child layouts x y currentPath childTransform skipAbsolute with
          | some result => some result
          | none => checkChildren (i + 1)
        | none => checkChildren (i + 1)

    match checkChildren 0 with
    | some result => some result
    | none => some { widgetId := w.id, path := currentPath, layout }

/-- Hit test and return just the path for bubbling (root to target). -/
def hitTestPath (widget : Widget) (layouts : Trellis.LayoutResult)
    (x y : Float) : Array WidgetId :=
  match hitTest widget layouts x y with
  | some result => result.path
  | none => #[]

/-- Hit test and return just the widget ID. -/
def hitTestId (widget : Widget) (layouts : Trellis.LayoutResult)
    (x y : Float) : Option WidgetId :=
  (hitTest widget layouts x y).map (·.widgetId)

/-- Find all widgets at a point (all overlapping widgets, topmost first).
    This can be useful for debugging or for events that affect multiple layers. -/
partial def hitTestAll (widget : Widget) (layouts : Trellis.LayoutResult)
    (x y : Float) : Array HitTestResult :=
  collectHits widget layouts x y #[] HitTransform.zero
where
  collectHits (w : Widget) (layouts : Trellis.LayoutResult)
      (x y : Float) (path : Array WidgetId) (transform : HitTransform)
      : Array HitTestResult :=
    match layouts.get w.id with
    | none => #[]
    | some layout =>
      let (adjX, adjY) := transform.transformPoint x y

      let inside := match layout.scaleMetadata with
        | some m => isInsideScaledHitArea layout m adjX adjY
        | none =>
          match w with
          | .custom _ _ _ spec =>
              match spec.hitTest with
              | some hit => hit layout ⟨adjX, adjY⟩
              | none => layout.borderRect.contains adjX adjY
          | _ => layout.borderRect.contains adjX adjY
      if !inside then
        #[]
      else
        let currentPath := path.push w.id

        -- Compute child transform based on widget type and scale metadata
        let childTransform := match w with
          | .scroll _ _ _ scrollState _ _ _ =>
            transform.addScroll scrollState.offsetX scrollState.offsetY
          | .flex _ _ _ _ children | .grid _ _ _ _ children =>
            match layout.scaleMetadata with
            | some m =>
              let (boundsX, boundsY) := computeChildBoundsOrigin children layouts
              transform.withScale m layout.contentRect boundsX boundsY
            | none => transform
          | _ => transform

        -- Collect hits from children (in reverse order, topmost first)
        let children := orderChildrenForHit w.children
        let rec collectFromChildren (i : Nat) (acc : Array HitTestResult) : Array HitTestResult :=
          if i >= children.size then
            acc
          else
            let childIdx := children.size - 1 - i
            match children[childIdx]? with
            | some child =>
              let childHits := collectHits child layouts x y currentPath childTransform
              collectFromChildren (i + 1) (childHits ++ acc)
            | none => collectFromChildren (i + 1) acc

        -- Start with this widget, then add child hits in front
        let thisHit : HitTestResult := { widgetId := w.id, path := currentPath, layout }
        collectFromChildren 0 #[thisHit]

/-- Check if a point is within a specific widget's bounds. -/
def isPointInWidget (layouts : Trellis.LayoutResult)
    (widgetId : WidgetId) (x y : Float) : Bool :=
  match layouts.get widgetId with
  | some layout => layout.borderRect.contains x y
  | none => false

/-- Get the path from root to a specific widget ID. -/
partial def pathToWidget (widget : Widget) (targetId : WidgetId) : Option (Array WidgetId) :=
  findPath widget targetId #[]
where
  findPath (w : Widget) (targetId : WidgetId) (path : Array WidgetId) : Option (Array WidgetId) :=
    let currentPath := path.push w.id
    if w.id == targetId then
      some currentPath
    else
      let rec searchChildren (children : Array Widget) (i : Nat) : Option (Array WidgetId) :=
        if i >= children.size then
          none
        else
          match children[i]? with
          | some child =>
            match findPath child targetId currentPath with
            | some result => some result
            | none => searchChildren children (i + 1)
          | none => searchChildren children (i + 1)
      searchChildren w.children 0

end Afferent.Arbor

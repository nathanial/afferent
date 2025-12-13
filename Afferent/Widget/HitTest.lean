/-
  Afferent Widget Hit Testing
  Map screen coordinates to widget IDs with proper Z-order.
-/
import Afferent.Widget.Core
import Afferent.Layout.Result

namespace Afferent.Widget

/-- Result of hit testing. -/
structure HitTestResult where
  /-- The hit widget ID (topmost widget at coordinates). -/
  widgetId : WidgetId
  /-- Path from root to hit widget (for bubbling). -/
  path : Array WidgetId
  /-- The widget's computed layout. -/
  layout : Layout.ComputedLayout
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

/-- Perform hit testing on a widget tree.
    Returns the topmost widget at (x, y) in canvas coordinates.

    Z-order is determined by render order: children are rendered after parents,
    and later children are rendered after earlier children (thus appear on top).
    To find the topmost hit, we traverse children in reverse order. -/
partial def hitTest (widget : Widget) (layouts : Layout.LayoutResult)
    (x y : Float) : Option HitTestResult :=
  hitTestHelper widget layouts x y #[] ScrollOffset.zero
where
  hitTestHelper (w : Widget) (layouts : Layout.LayoutResult)
      (x y : Float) (path : Array WidgetId) (scrollOffset : ScrollOffset)
      : Option HitTestResult := do
    -- Get this widget's layout
    let layout ← layouts.get w.id

    -- Adjust coordinates for scroll offset
    let adjX := x + scrollOffset.x
    let adjY := y + scrollOffset.y

    -- Check if point is within this widget's bounds
    if !layout.borderRect.contains adjX adjY then
      none

    let currentPath := path.push w.id

    -- For scroll containers, adjust offset for children
    let childOffset := match w with
      | .scroll _ _ scrollState _ _ _ =>
        scrollOffset.add { x := scrollState.offsetX, y := scrollState.offsetY }
      | _ => scrollOffset

    -- Check children in reverse order (last rendered = topmost)
    let children := w.children
    let rec checkChildren (i : Nat) : Option HitTestResult :=
      if i >= children.size then
        none
      else
        let childIdx := children.size - 1 - i
        match children[childIdx]? with
        | some child =>
          match hitTestHelper child layouts x y currentPath childOffset with
          | some result => some result
          | none => checkChildren (i + 1)
        | none => checkChildren (i + 1)

    match checkChildren 0 with
    | some result => some result
    | none =>
      -- No child was hit, so this widget is the hit
      some { widgetId := w.id, path := currentPath, layout }

/-- Hit test and return just the path for bubbling (root to target). -/
def hitTestPath (widget : Widget) (layouts : Layout.LayoutResult)
    (x y : Float) : Array WidgetId :=
  match hitTest widget layouts x y with
  | some result => result.path
  | none => #[]

/-- Hit test and return just the widget ID. -/
def hitTestId (widget : Widget) (layouts : Layout.LayoutResult)
    (x y : Float) : Option WidgetId :=
  (hitTest widget layouts x y).map (·.widgetId)

/-- Find all widgets at a point (all overlapping widgets, topmost first).
    This can be useful for debugging or for events that affect multiple layers. -/
partial def hitTestAll (widget : Widget) (layouts : Layout.LayoutResult)
    (x y : Float) : Array HitTestResult :=
  collectHits widget layouts x y #[] ScrollOffset.zero
where
  collectHits (w : Widget) (layouts : Layout.LayoutResult)
      (x y : Float) (path : Array WidgetId) (scrollOffset : ScrollOffset)
      : Array HitTestResult :=
    match layouts.get w.id with
    | none => #[]
    | some layout =>
      let adjX := x + scrollOffset.x
      let adjY := y + scrollOffset.y

      if !layout.borderRect.contains adjX adjY then
        #[]
      else
        let currentPath := path.push w.id

        -- Calculate child offset for scroll containers
        let childOffset := match w with
          | .scroll _ _ scrollState _ _ _ =>
            scrollOffset.add { x := scrollState.offsetX, y := scrollState.offsetY }
          | _ => scrollOffset

        -- Collect hits from children (in reverse order, topmost first)
        let children := w.children
        let rec collectFromChildren (i : Nat) (acc : Array HitTestResult) : Array HitTestResult :=
          if i >= children.size then
            acc
          else
            let childIdx := children.size - 1 - i
            match children[childIdx]? with
            | some child =>
              let childHits := collectHits child layouts x y currentPath childOffset
              collectFromChildren (i + 1) (childHits ++ acc)
            | none => collectFromChildren (i + 1) acc

        -- Start with this widget, then add child hits in front
        let thisHit : HitTestResult := { widgetId := w.id, path := currentPath, layout }
        collectFromChildren 0 #[thisHit]

/-- Check if a point is within a specific widget's bounds. -/
def isPointInWidget (layouts : Layout.LayoutResult)
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

end Afferent.Widget

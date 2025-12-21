/-
  Afferent Widget Tests
  Unit tests for the declarative widget system.
-/
import Afferent.Tests.Framework
import Afferent.Widget
import Trellis


namespace Afferent.Tests.WidgetTests

open Crucible
open Afferent.Tests
open Afferent (Color)
open Trellis (FlexContainer GridContainer EdgeInsets)

-- Explicitly alias the Widget type to avoid confusion with DSL functions
abbrev W := Afferent.Widget.Widget
abbrev WB := Afferent.Widget.WidgetBuilder

testSuite "Widget Tests"

/-! ## Text Layout Tests -/

test "tokenize splits words and spaces" := do
  let tokens := Afferent.Widget.tokenize "hello world"
  tokens.size ≡ 3  -- "hello", space, "world"
  match tokens[0]? with
  | some (Afferent.Widget.Token.word w) => w ≡ "hello"
  | _ => throw <| IO.userError "Expected word token"
  match tokens[1]? with
  | some Afferent.Widget.Token.space => pure ()
  | _ => throw <| IO.userError "Expected space token"
  match tokens[2]? with
  | some (Afferent.Widget.Token.word w) => w ≡ "world"
  | _ => throw <| IO.userError "Expected word token"

test "tokenize handles newlines" := do
  let tokens := Afferent.Widget.tokenize "line1\nline2"
  tokens.size ≡ 3  -- "line1", newline, "line2"
  match tokens[1]? with
  | some Afferent.Widget.Token.newline => pure ()
  | _ => throw <| IO.userError "Expected newline token"

test "tokenize handles empty string" := do
  let tokens := Afferent.Widget.tokenize ""
  tokens.size ≡ 0

/-! ## Intrinsic Size Tests -/

-- Note: These tests use direct Widget constructors since we can't load fonts in unit tests

test "intrinsic size of spacer matches dimensions" := do
  let widget : W := .spacer 0 100 50
  let (w, h) ← Afferent.Widget.intrinsicSize widget
  shouldBeNear w 100 0.01
  shouldBeNear h 50 0.01

test "intrinsic size of rect uses min dimensions" := do
  let widget : W := .rect 0 { minWidth := some 80, minHeight := some 60 }
  let (w, h) ← Afferent.Widget.intrinsicSize widget
  shouldBeNear w 80 0.01
  shouldBeNear h 60 0.01

test "intrinsic size of rect without min is zero" := do
  let widget : W := .rect 0 {}
  let (w, h) ← Afferent.Widget.intrinsicSize widget
  shouldBeNear w 0 0.01
  shouldBeNear h 0 0.01

test "intrinsic size of row sums widths" := do
  let child1 : W := .spacer 1 50 30
  let child2 : W := .spacer 2 60 40
  let widget : W := .flex 0 (FlexContainer.row 10) {} #[child1, child2]
  let (w, h) ← Afferent.Widget.intrinsicSize widget
  -- Width = 50 + 60 + 10 (gap) = 120
  -- Height = max(30, 40) = 40
  shouldBeNear w 120 0.01
  shouldBeNear h 40 0.01

test "intrinsic size of column sums heights" := do
  let child1 : W := .spacer 1 50 30
  let child2 : W := .spacer 2 60 40
  let widget : W := .flex 0 (FlexContainer.column 10) {} #[child1, child2]
  let (w, h) ← Afferent.Widget.intrinsicSize widget
  -- Width = max(50, 60) = 60
  -- Height = 30 + 40 + 10 (gap) = 80
  shouldBeNear w 60 0.01
  shouldBeNear h 80 0.01

test "intrinsic size includes padding" := do
  let child : W := .spacer 1 100 50
  let style : Afferent.Widget.BoxStyle := { padding := EdgeInsets.uniform 20 }
  let widget : W := .flex 0 FlexContainer.default style #[child]
  let (w, h) ← Afferent.Widget.intrinsicSize widget
  -- Width = 100 + 20*2 = 140
  -- Height = 50 + 20*2 = 90
  shouldBeNear w 140 0.01
  shouldBeNear h 90 0.01

test "intrinsic size works for nested containers" := do
  -- Inner row: two 30x30 boxes with gap 10 = 70x30
  let innerChild1 : W := .rect 3 { minWidth := some 30, minHeight := some 30 }
  let innerChild2 : W := .rect 4 { minWidth := some 30, minHeight := some 30 }
  let innerRow : W := .flex 2 (FlexContainer.row 10) {} #[innerChild1, innerChild2]

  -- Outer column: spacer 50x20, inner row 70x30, gap 5 = 70x55
  let outerChild1 : W := .spacer 1 50 20
  let widget : W := .flex 0 (FlexContainer.column 5) {} #[outerChild1, innerRow]

  let (w, h) ← Afferent.Widget.intrinsicSize widget
  -- Width = max(50, 70) = 70
  -- Height = 20 + 30 + 5 = 55
  shouldBeNear w 70 0.01
  shouldBeNear h 55 0.01

/-! ## Widget Builder Tests -/

test "widget builder generates unique IDs" := do
  let widget := Afferent.Widget.build do
    Afferent.Widget.column (gap := 0) {} #[
      Afferent.Widget.spacer 10 10,
      Afferent.Widget.spacer 20 20,
      Afferent.Widget.row (gap := 0) {} #[
        Afferent.Widget.spacer 5 5,
        Afferent.Widget.spacer 5 5
      ]
    ]
  -- Collect all IDs
  let ids := widget.allIds
  -- Check uniqueness
  let uniqueIds := ids.toList.eraseDups
  ids.size ≡ uniqueIds.length

test "widget count includes all nodes" := do
  let widget := Afferent.Widget.build do
    Afferent.Widget.column (gap := 0) {} #[
      Afferent.Widget.spacer 10 10,          -- 1
      Afferent.Widget.spacer 20 20,          -- 2
      Afferent.Widget.row (gap := 0) {} #[   -- 3
        Afferent.Widget.spacer 5 5,          -- 4
        Afferent.Widget.spacer 5 5           -- 5
      ]
    ]                        -- +1 for outer column = 6
  widget.widgetCount ≡ 6

test "column builder creates vertical flex" := do
  let widget := Afferent.Widget.build do
    Afferent.Widget.column (gap := 16) {} #[
      Afferent.Widget.spacer 10 10
    ]
  match widget with
  | .flex _ props _ _ =>
    shouldSatisfy (!props.direction.isHorizontal) "should be vertical"
    shouldBeNear props.gap 16 0.01
  | _ => throw <| IO.userError "Expected flex widget"

test "row builder creates horizontal flex" := do
  let widget := Afferent.Widget.build do
    Afferent.Widget.row (gap := 8) {} #[
      Afferent.Widget.spacer 10 10
    ]
  match widget with
  | .flex _ props _ _ =>
    shouldSatisfy props.direction.isHorizontal "should be horizontal"
    shouldBeNear props.gap 8 0.01
  | _ => throw <| IO.userError "Expected flex widget"

test "coloredBox creates rect with dimensions" := do
  let widget := Afferent.Widget.build do
    Afferent.Widget.coloredBox Color.red 80 60
  match widget with
  | .rect _ style =>
    style.backgroundColor ≡ some Color.red
    style.minWidth ≡ some 80
    style.minHeight ≡ some 60
  | _ => throw <| IO.userError "Expected rect widget"

test "card creates styled flex container" := do
  let widget := Afferent.Widget.build do
    Afferent.Widget.card (Color.gray 0.3) 24 do
      Afferent.Widget.spacer 10 10
  match widget with
  | .flex _ _ style children =>
    style.backgroundColor ≡ some (Color.gray 0.3)
    shouldBeNear style.padding.top 24 0.01
    children.size ≡ 1
  | _ => throw <| IO.userError "Expected flex widget"

/-! ## Widget Measurement Tests -/

test "measureWidget preserves spacer dimensions" := do
  let widget : W := .spacer 0 100 50
  let result ← Afferent.Widget.measureWidget widget 1000 1000
  -- Check the LayoutNode has correct content size
  match result.node.content with
  | some cs =>
    shouldBeNear cs.width 100 0.01
    shouldBeNear cs.height 50 0.01
  | none => throw <| IO.userError "Expected content size"

test "measureWidget uses rect min dimensions" := do
  let widget : W := .rect 0 { minWidth := some 80, minHeight := some 60 }
  let result ← Afferent.Widget.measureWidget widget 1000 1000
  match result.node.content with
  | some cs =>
    shouldBeNear cs.width 80 0.01
    shouldBeNear cs.height 60 0.01
  | none => throw <| IO.userError "Expected content size"

test "measureWidget recursively measures children" := do
  let child1 : W := .spacer 1 50 30
  let child2 : W := .spacer 2 60 40
  let widget : W := .flex 0 FlexContainer.default {} #[child1, child2]
  let result ← Afferent.Widget.measureWidget widget 1000 1000
  -- Check updated widget has children
  match result.widget with
  | .flex _ _ _ children => children.size ≡ 2
  | _ => throw <| IO.userError "Expected flex widget"
  -- Check LayoutNode has children
  result.node.children.size ≡ 2

/-! ## Grid Tests -/

test "intrinsic size of grid computes rows and columns" := do
  -- 3-column grid with 6 items (2 rows)
  let children : Array W := #[
    .rect 1 { minWidth := some 50, minHeight := some 40 },
    .rect 2 { minWidth := some 50, minHeight := some 40 },
    .rect 3 { minWidth := some 50, minHeight := some 40 },
    .rect 4 { minWidth := some 50, minHeight := some 40 },
    .rect 5 { minWidth := some 50, minHeight := some 40 },
    .rect 6 { minWidth := some 50, minHeight := some 40 }
  ]
  let gridProps := GridContainer.columns 3 8  -- 3 cols, gap 8
  let widget : W := .grid 0 gridProps {} children
  let (w, h) ← Afferent.Widget.intrinsicSize widget
  -- Width = 3 * 50 + 2 * 8 = 166
  -- Height = 2 * 40 + 1 * 8 = 88 (2 rows, 1 row gap)
  shouldBeNear w 166 0.01
  shouldBeNear h 88 0.01

#generate_tests

end Afferent.Tests.WidgetTests

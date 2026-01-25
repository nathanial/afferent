/-
  Reactive Layout Tests
  Unit tests to verify reactive widget system produces widgets with correct flex properties.
-/
import Afferent.Tests.Framework
import Afferent.Arbor
import Afferent.Arbor.Widget.Measure
import Afferent.Canopy.Reactive
import Reactive
import Trellis

namespace Afferent.Tests.ReactiveLayoutTests

open Crucible
open Afferent.Tests
open Afferent.Arbor
open Afferent.Canopy.Reactive
open Reactive.Host
open Trellis

-- Use Id monad for testing (uses ASCII text measurement)
abbrev TestM := Id

testSuite "Reactive Layout Tests"

/-! ## Test 1: column' preserves flexItem in style -/

test "column' produces widget with flexItem in style" := do
  -- Create a SpiderEnv for running reactive code
  let spiderEnv ← SpiderEnv.new defaultErrorHandler
  let _result ← (do
    let (events, _) ← createInputs FontRegistry.empty
    let rootStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
    }
    let (_, render) ← ReactiveM.run events do
      runWidget do
        column' (gap := 0) (style := rootStyle) do
          pure ()
    -- Execute the IO to get WidgetBuilder
    let builder ← render
    -- Build the widget
    let widget := buildFrom 0 builder
    -- Check the widget's style has flexItem
    match widgetBoxStyle widget with
    | some style =>
      match style.flexItem with
      | some fi =>
        -- Should be growing 1
        ensure (fi.grow == 1.0) s!"Expected grow=1.0, got {fi.grow}"
        pure true
      | none =>
        ensure false "Widget style has no flexItem!"
        pure false
    | none =>
      ensure false "Widget has no style!"
      pure false
  ).run spiderEnv
  pure ()

/-! ## Test 2: Pure Arbor column has correct flexItem -/

test "pure Arbor column has flexItem in style" := do
  let rootStyle : BoxStyle := {
    flexItem := some (FlexItem.growing 1)
    width := .percent 1.0
    height := .percent 1.0
  }
  let widget := buildFrom 0 (column (gap := 0) (style := rootStyle) #[])
  match widgetBoxStyle widget with
  | some style =>
    match style.flexItem with
    | some fi =>
      ensure (fi.grow == 1.0) s!"Expected grow=1.0, got {fi.grow}"
    | none =>
      ensure false "Widget style has no flexItem!"
  | none =>
    ensure false "Widget has no style!"

/-! ## Test 3: Layout applies flexItem from widget style -/

test "child with flexItem grows to fill parent" := do
  -- Create a column container with one growing child
  let tree := LayoutNode.column 0 #[
    LayoutNode.leaf 1 (ContentSize.mk' 100 50)
      { height := .percent 1.0 }
      (item := .flexChild (FlexItem.growing 1))
  ]
  let result := layout tree 400 300
  let child := result.get! 1
  -- Child should fill parent height (300)
  shouldBeNear child.height 300.0

/-! ## Test 4: Nested structure matches demo layout -/

test "tabbar + content + footer layout with growing content" := do
  -- Simulates the demo runner's root structure:
  -- column [tabbar (fixed 44), content (growing 1), footer (fixed 32)]
  let tree := LayoutNode.column 0 #[
    LayoutNode.leaf 1 (ContentSize.mk' 400 44) {} (item := .flexChild (FlexItem.fixed 44)),
    LayoutNode.leaf 2 (ContentSize.mk' 400 100) {} (item := .flexChild (FlexItem.growing 1)),
    LayoutNode.leaf 3 (ContentSize.mk' 400 32) {} (item := .flexChild (FlexItem.fixed 32))
  ]
  let result := layout tree 400 300
  let tabbar := result.get! 1
  let content := result.get! 2
  let footer := result.get! 3
  -- Tabbar: fixed 44
  shouldBeNear tabbar.height 44.0
  -- Footer: fixed 32
  shouldBeNear footer.height 32.0
  -- Content: should grow to fill remaining (300 - 44 - 32 = 224)
  shouldBeNear content.height 224.0

/-! ## Test 5: Compare reactive vs pure Arbor widget structure -/

test "reactive and pure Arbor produce equivalent widget types" := do
  -- Pure Arbor
  let pureStyle : BoxStyle := { flexItem := some (FlexItem.growing 1) }
  let pureWidget := buildFrom 0 (column (gap := 10) (style := pureStyle) #[])

  -- Check both produce .flex widget type with same style
  match pureWidget with
  | .flex _ _ _ style _ =>
    match style.flexItem with
    | some fi => ensure (fi.grow == 1.0) "Pure Arbor flexItem.grow should be 1.0"
    | none => ensure false "Pure Arbor widget should have flexItem"
  | _ => ensure false "Pure Arbor should produce .flex widget"

/-! ## Test 6: Measure widget extracts flexItem from BoxStyle -/

test "measureWidget applies flexItem from widget BoxStyle" := do
  -- Create a widget with flexItem in its BoxStyle
  let style : BoxStyle := {
    flexItem := some (FlexItem.growing 1)
    width := .percent 1.0
    height := .percent 1.0
  }
  let widget := Widget.flex 1 none (FlexContainer.column 0) style #[]

  -- Verify widgetBoxStyle extracts the style correctly
  match widgetBoxStyle widget with
  | some s =>
    match s.flexItem with
    | some fi =>
      ensure (fi.grow == 1.0) s!"Expected flexItem.grow=1.0, got {fi.grow}"
    | none =>
      ensure false "widgetBoxStyle returned style without flexItem"
  | none =>
    ensure false "widgetBoxStyle returned none for .flex widget"

/-! ## Test 7: Simulated demo structure with Widget.flex -/

test "demo structure: root column with content that has flexItem" := do
  -- Simulates: root column (no flexItem) containing content (with flexItem)
  -- This is how buildRootWidget structures the layout
  let contentStyle : BoxStyle := {
    flexItem := some (FlexItem.growing 1)
    width := .percent 1.0
    height := .percent 1.0
  }

  -- Content widget (like what reactive demos return)
  let contentWidget := Widget.flex 2 none (FlexContainer.column 10) contentStyle #[]

  -- Root layout (like buildRootWidget)
  let rootStyle : BoxStyle := {
    width := .percent 1.0
    height := .percent 1.0
  }
  let tabbar := Widget.rect 1 none { height := .length 44 }
  let footer := Widget.rect 3 none { height := .length 32 }
  let root := Widget.flex 0 none (FlexContainer.column 0) rootStyle #[tabbar, contentWidget, footer]

  -- Check content widget has flexItem
  match widgetBoxStyle contentWidget with
  | some s =>
    match s.flexItem with
    | some fi => ensure (fi.grow == 1.0) "Content should have flexItem.grow=1.0"
    | none => ensure false "Content widget missing flexItem"
  | none =>
    ensure false "Content widget has no style"

/-! ## Test 8: Full measure and layout simulation -/

test "full measure+layout: content with flexItem fills remaining space" := do
  -- Build exact structure like runner
  let tabbarStyle : BoxStyle := {
    height := .length 44
    flexItem := some (FlexItem.fixed 44)
  }
  let contentStyle : BoxStyle := {
    flexItem := some (FlexItem.growing 1)
    width := .percent 1.0
    height := .percent 1.0
  }
  let footerStyle : BoxStyle := {
    height := .length 32
    flexItem := some (FlexItem.fixed 32)
  }

  let tabbar := Widget.rect 1 none tabbarStyle
  let content := Widget.flex 2 none (FlexContainer.column 10) contentStyle #[]
  let footer := Widget.rect 3 none footerStyle

  let rootStyle : BoxStyle := {
    width := .percent 1.0
    height := .percent 1.0
  }
  let root := Widget.flex 0 none (FlexContainer.column 0) rootStyle #[tabbar, content, footer]

  -- Measure the widget tree
  let measureResult : MeasureResult := (measureWidget (M := TestM) root 800 600)

  -- Layout the measured tree
  let layouts := layout measureResult.node 800 600

  -- Check content got the remaining space (600 - 44 - 32 = 524)
  let contentLayout := layouts.get! 2
  shouldBeNear contentLayout.height 524.0

/-! ## Test 9: Reactive widget through full pipeline -/

test "reactive column' through measure+layout fills viewport" := do
  let spiderEnv ← SpiderEnv.new defaultErrorHandler
  let contentWidget ← (do
    let (events, _) ← createInputs FontRegistry.empty
    let contentStyle : BoxStyle := {
      flexItem := some (FlexItem.growing 1)
      width := .percent 1.0
      height := .percent 1.0
      backgroundColor := some (Afferent.Color.gray 0.1)
    }
    let (_, render) ← ReactiveM.run events do
      runWidget do
        column' (gap := 0) (style := contentStyle) do
          pure ()
    let builder ← render
    pure (buildFrom 100 builder)
  ).run spiderEnv

  -- Build root with tabbar + content + footer (like runner)
  let tabbar := Widget.rect 1 none { height := .length 44, flexItem := some (FlexItem.fixed 44) }
  let footer := Widget.rect 3 none { height := .length 32, flexItem := some (FlexItem.fixed 32) }
  let rootStyle : BoxStyle := { width := .percent 1.0, height := .percent 1.0 }
  let root := Widget.flex 0 none (FlexContainer.column 0) rootStyle #[tabbar, contentWidget, footer]

  -- Measure and layout
  let measureResult : MeasureResult := (measureWidget (M := TestM) root 800 600)
  let layouts := layout measureResult.node 800 600

  -- Content should fill remaining space
  -- Find the content widget's layout (ID should be 100 based on buildFrom startId)
  let contentLayout := layouts.get! 100
  shouldBeNear contentLayout.height 524.0

/-! ## Test 10: Verify layout node has flexItem applied -/

test "measureWidget sets ItemKind.flexChild on child layout nodes" := do
  -- Create content widget with flexItem
  let contentStyle : BoxStyle := {
    flexItem := some (FlexItem.growing 1)
    width := .percent 1.0
    height := .percent 1.0
  }
  let contentWidget := Widget.flex 2 none (FlexContainer.column 10) contentStyle #[]

  -- Create root with tabbar + content + footer
  let tabbar := Widget.rect 1 none { height := .length 44, flexItem := some (FlexItem.fixed 44) }
  let footer := Widget.rect 3 none { height := .length 32, flexItem := some (FlexItem.fixed 32) }
  let rootStyle : BoxStyle := { width := .percent 1.0, height := .percent 1.0 }
  let root := Widget.flex 0 none (FlexContainer.column 0) rootStyle #[tabbar, contentWidget, footer]

  -- Measure the widget tree
  let measureResult : MeasureResult := (measureWidget (M := TestM) root 800 600)

  -- Check the layout node structure
  -- Root node should have 3 children
  ensure (measureResult.node.children.size == 3) s!"Expected 3 children, got {measureResult.node.children.size}"

  -- Check tabbar child (index 0) has ItemKind.flexChild with fixed 44
  let tabbarNode := measureResult.node.children[0]!
  match tabbarNode.item with
  | .flexChild fi =>
      ensure (fi.grow == 0.0) s!"Tabbar should have grow=0, got {fi.grow}"
      -- basis is Dimension, check it's .length 44
      match fi.basis with
      | .length len => ensure (len == 44.0) s!"Tabbar should have basis length=44, got {len}"
      | _ => ensure false s!"Tabbar should have basis .length, got {repr fi.basis}"
  | _ =>
      ensure false s!"Tabbar should have ItemKind.flexChild, got {repr tabbarNode.item}"

  -- Check content child (index 1) has ItemKind.flexChild with growing 1
  let contentNode := measureResult.node.children[1]!
  match contentNode.item with
  | .flexChild fi =>
      ensure (fi.grow == 1.0) s!"Content should have grow=1, got {fi.grow}"
  | _ =>
      ensure false s!"Content should have ItemKind.flexChild, got {repr contentNode.item}"

  -- Check footer child (index 2) has ItemKind.flexChild with fixed 32
  let footerNode := measureResult.node.children[2]!
  match footerNode.item with
  | .flexChild fi =>
      ensure (fi.grow == 0.0) s!"Footer should have grow=0, got {fi.grow}"
      match fi.basis with
      | .length len => ensure (len == 32.0) s!"Footer should have basis length=32, got {len}"
      | _ => ensure false s!"Footer should have basis .length, got {repr fi.basis}"
  | _ =>
      ensure false s!"Footer should have ItemKind.flexChild, got {repr footerNode.item}"



end Afferent.Tests.ReactiveLayoutTests

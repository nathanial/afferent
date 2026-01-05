/-
  Content Scale Integration Tests
  Verify the full contentScale pipeline: measure -> layout -> applyContentScale -> render
-/
import Afferent.Tests.Framework
import Afferent.Arbor.Widget.Core
import Afferent.Arbor.Widget.Measure
import Afferent.Arbor.Widget.DSL
import Afferent.Arbor.Text.Renderer
import Afferent.Arbor.Render.Collect
import Trellis

namespace Afferent.Tests.ContentScaleTests

open Crucible
open Afferent.Tests
open Afferent.Arbor
open Trellis

-- Use Id monad for testing (uses ASCII text measurement)
abbrev TestM := Id

testSuite "Content Scale Tests"

/-! ## Basic Scale Metadata Tests -/

test "applyContentScale sets scaleMetadata on container with contentScale" := do
  -- Create a simple widget with contentScale
  let widget := Widget.flex 1 none FlexContainer.column
    { contentScale := some { mode := .contain, allowUpscale := false } }
    #[Widget.rect 2 none { minWidth := some 200, minHeight := some 150 }]

  -- Measure the widget (using Id monad)
  let result : MeasureResult := (measureWidget (M := TestM) widget 10000 10000 : TestM _)

  -- Layout with Trellis at a smaller size than intrinsic
  let layouts := Trellis.layout result.node 100 100

  -- Apply content scale
  let layoutsWithScale : Trellis.LayoutResult := (applyContentScale (M := TestM) result.widget layouts : TestM _)

  -- Check that scaleMetadata was set on the container
  let containerLayout := layoutsWithScale.get 1
  match containerLayout with
  | some cl =>
    match cl.scaleMetadata with
    | some m =>
      -- Container is 100x100, child intrinsic is 200x150
      -- contain mode: min(100/200, 100/150) = min(0.5, 0.67) = 0.5
      shouldBeNear m.scaleX 0.5
      shouldBeNear m.scaleY 0.5
      shouldBeNear m.intrinsicWidth 200.0
      shouldBeNear m.intrinsicHeight 150.0
    | none => ensure false "Expected scaleMetadata to be set"
  | none => ensure false "Expected container layout to exist"

test "applyContentScale computes correct scale for contain mode" := do
  -- Child 400x300 in container 200x200
  -- contain: min(200/400, 200/300) = min(0.5, 0.67) = 0.5
  let widget := Widget.flex 1 none FlexContainer.column
    { contentScale := some { mode := .contain } }
    #[Widget.rect 2 none { minWidth := some 400, minHeight := some 300 }]

  let result : MeasureResult := (measureWidget (M := TestM) widget 10000 10000 : TestM _)
  let layouts := Trellis.layout result.node 200 200
  let layoutsWithScale : Trellis.LayoutResult := (applyContentScale (M := TestM) result.widget layouts : TestM _)

  let containerLayout := layoutsWithScale.get! 1
  let m := containerLayout.scaleMetadata.get!
  shouldBeNear m.scaleX 0.5
  shouldBeNear m.scaleY 0.5

test "applyContentScale does not upscale when allowUpscale=false" := do
  -- Child 100x100 in container 400x400
  -- Would be 4x scale, but allowUpscale=false so should be 1.0
  let widget := Widget.flex 1 none FlexContainer.column
    { contentScale := some { mode := .contain, allowUpscale := false } }
    #[Widget.rect 2 none { minWidth := some 100, minHeight := some 100 }]

  let result : MeasureResult := (measureWidget (M := TestM) widget 10000 10000 : TestM _)
  let layouts := Trellis.layout result.node 400 400
  let layoutsWithScale : Trellis.LayoutResult := (applyContentScale (M := TestM) result.widget layouts : TestM _)

  let containerLayout := layoutsWithScale.get! 1
  let m := containerLayout.scaleMetadata.get!
  shouldBeNear m.scaleX 1.0
  shouldBeNear m.scaleY 1.0

test "applyContentScale upscales when allowUpscale=true" := do
  -- Child 100x100 in container 400x400
  -- allowUpscale=true so should scale to 4.0
  let widget := Widget.flex 1 none FlexContainer.column
    { contentScale := some { mode := .contain, allowUpscale := true } }
    #[Widget.rect 2 none { minWidth := some 100, minHeight := some 100 }]

  let result : MeasureResult := (measureWidget (M := TestM) widget 10000 10000 : TestM _)
  let layouts := Trellis.layout result.node 400 400
  let layoutsWithScale : Trellis.LayoutResult := (applyContentScale (M := TestM) result.widget layouts : TestM _)

  let containerLayout := layoutsWithScale.get! 1
  let m := containerLayout.scaleMetadata.get!
  shouldBeNear m.scaleX 4.0
  shouldBeNear m.scaleY 4.0

test "container without contentScale has no scaleMetadata" := do
  -- Widget without contentScale
  let widget := Widget.flex 1 none FlexContainer.column {}
    #[Widget.rect 2 none { minWidth := some 200, minHeight := some 150 }]

  let result : MeasureResult := (measureWidget (M := TestM) widget 10000 10000 : TestM _)
  let layouts := Trellis.layout result.node 100 100
  let layoutsWithScale : Trellis.LayoutResult := (applyContentScale (M := TestM) result.widget layouts : TestM _)

  let containerLayout := layoutsWithScale.get! 1
  match containerLayout.scaleMetadata with
  | some _ => ensure false "Expected no scaleMetadata on container without contentScale"
  | none => pure ()

/-! ## Intrinsic Size Computation Tests -/

test "intrinsicSize of rect with minWidth/minHeight" := do
  let widget := Widget.rect 1 none { minWidth := some 200, minHeight := some 150 }
  let (w, h) : Float × Float := (intrinsicSize (M := TestM) widget : TestM _)
  shouldBeNear w 200.0
  shouldBeNear h 150.0

test "intrinsicSize of column sums heights" := do
  -- Column with 3 children of 50 height each, gap 10
  let widget := Widget.flex 1 none (FlexContainer.column 10) {}
    #[Widget.rect 2 none { minWidth := some 100, minHeight := some 50 },
      Widget.rect 3 none { minWidth := some 100, minHeight := some 50 },
      Widget.rect 4 none { minWidth := some 100, minHeight := some 50 }]
  let (w, h) : Float × Float := (intrinsicSize (M := TestM) widget : TestM _)
  -- Width = max of children = 100
  -- Height = sum of children + gaps = 50*3 + 10*2 = 170
  shouldBeNear w 100.0
  shouldBeNear h 170.0

test "intrinsicSize of column with padding" := do
  let widget := Widget.flex 1 none FlexContainer.column
    { padding := EdgeInsets.uniform 20 }
    #[Widget.rect 2 none { minWidth := some 100, minHeight := some 50 }]
  let (w, h) : Float × Float := (intrinsicSize (M := TestM) widget : TestM _)
  -- Width = 100 + 40 padding = 140
  -- Height = 50 + 40 padding = 90
  shouldBeNear w 140.0
  shouldBeNear h 90.0

test "intrinsicSize of row sums widths" := do
  -- Row with 3 children of 50 width each, gap 10
  let widget := Widget.flex 1 none (FlexContainer.row 10) {}
    #[Widget.rect 2 none { minWidth := some 50, minHeight := some 100 },
      Widget.rect 3 none { minWidth := some 50, minHeight := some 100 },
      Widget.rect 4 none { minWidth := some 50, minHeight := some 100 }]
  let (w, h) : Float × Float := (intrinsicSize (M := TestM) widget : TestM _)
  -- Width = sum of children + gaps = 50*3 + 10*2 = 170
  -- Height = max of children = 100
  shouldBeNear w 170.0
  shouldBeNear h 100.0

/-! ## Children Layout Tests -/

test "children measured with relaxed constraints for contentScale" := do
  -- Create container with contentScale
  let widget := Widget.flex 1 none FlexContainer.column
    { contentScale := some { mode := .contain } }
    #[Widget.rect 2 none { minWidth := some 500, minHeight := some 400 }]

  -- Measure with SMALL available space - children should still get intrinsic size
  -- because contentScale triggers relaxed constraints
  let result : MeasureResult := (measureWidget (M := TestM) widget 100 100 : TestM _)

  -- Check the child's content size in the LayoutNode
  let childNode := result.node.children[0]!
  let childContent := childNode.content.get!
  shouldBeNear childContent.width 500.0
  shouldBeNear childContent.height 400.0

test "child BoxConstraints have explicit size for contentScale" := do
  -- Verify that fixToContentSize sets explicit width/height on child nodes
  let widget := Widget.flex 1 none FlexContainer.column
    { contentScale := some { mode := .contain } }
    #[Widget.rect 2 none { minWidth := some 400, minHeight := some 300 }]

  let result : MeasureResult := (measureWidget (M := TestM) widget 10000 10000 : TestM _)
  let childNode := result.node.children[0]!

  -- After fixToContentSize, child should have explicit sizes in BoxConstraints
  match childNode.box.width with
  | .length w => shouldBeNear w 400.0
  | _ => ensure false "Expected explicit width on child BoxConstraints"
  match childNode.box.height with
  | .length h => shouldBeNear h 300.0
  | _ => ensure false "Expected explicit height on child BoxConstraints"

test "child has shrink=0 and minWidth/minHeight for contentScale" := do
  let widget := Widget.flex 1 none FlexContainer.column
    { contentScale := some { mode := .contain } }
    #[Widget.rect 2 none { minWidth := some 400, minHeight := some 300 }]

  let result : MeasureResult := (measureWidget (M := TestM) widget 10000 10000 : TestM _)
  let childNode := result.node.children[0]!

  -- Check minWidth/minHeight are set
  shouldBeNear childNode.box.minWidth 400.0
  shouldBeNear childNode.box.minHeight 300.0

  -- Check that flexItem has shrink=0
  match childNode.item with
  | .flexChild fi => shouldBeNear fi.shrink 0.0
  | _ => ensure false "Expected flexChild item kind"

test "child layout positions with contentScale" := do
  -- Container 200x200, child 400x300
  -- Child should be positioned centered (may overflow)
  let widget := Widget.flex 1 none FlexContainer.column
    { contentScale := some { mode := .contain } }
    #[Widget.rect 2 none { minWidth := some 400, minHeight := some 300 }]

  let result : MeasureResult := (measureWidget (M := TestM) widget 10000 10000 : TestM _)
  let layouts := Trellis.layout result.node 200 200

  -- Child should be laid out at intrinsic size (400x300)
  let childLayout := layouts.get! 2
  shouldBeNear childLayout.width 400.0
  shouldBeNear childLayout.height 300.0

/-! ## Scale Metadata Offset Tests -/

test "scale metadata offset centers content" := do
  -- Container 400x400, child 200x200, contain mode, center anchor
  -- Scale = 1.0 (no upscale by default)
  -- Scaled size = 200x200
  -- Offset = ((400-200)/2, (400-200)/2) = (100, 100)
  let widget := Widget.flex 1 none FlexContainer.column
    { contentScale := some { mode := .contain, anchor := .center } }
    #[Widget.rect 2 none { minWidth := some 200, minHeight := some 200 }]

  let result : MeasureResult := (measureWidget (M := TestM) widget 10000 10000 : TestM _)
  let layouts := Trellis.layout result.node 400 400
  let layoutsWithScale : Trellis.LayoutResult := (applyContentScale (M := TestM) result.widget layouts : TestM _)

  let m := (layoutsWithScale.get! 1).scaleMetadata.get!
  -- Scale should be 1.0 (child fits, no upscale)
  shouldBeNear m.scaleX 1.0
  -- Offset should center the 200x200 content in 400x400 container
  shouldBeNear m.offsetX 100.0
  shouldBeNear m.offsetY 100.0

test "scale metadata offset for topLeft anchor" := do
  let widget := Widget.flex 1 none FlexContainer.column
    { contentScale := some { mode := .contain, anchor := .topLeft } }
    #[Widget.rect 2 none { minWidth := some 200, minHeight := some 200 }]

  let result : MeasureResult := (measureWidget (M := TestM) widget 10000 10000 : TestM _)
  let layouts := Trellis.layout result.node 400 400
  let layoutsWithScale : Trellis.LayoutResult := (applyContentScale (M := TestM) result.widget layouts : TestM _)

  let m := (layoutsWithScale.get! 1).scaleMetadata.get!
  -- TopLeft anchor means offset = (0, 0)
  shouldBeNear m.offsetX 0.0
  shouldBeNear m.offsetY 0.0

/-! ## Render Command Tests -/

test "collectCommands emits scale transform for contentScale container" := do
  -- Container 200x200, child 400x300, should emit scale transform
  let widget := Widget.flex 1 none FlexContainer.column
    { contentScale := some { mode := .contain } }
    #[Widget.rect 2 none { minWidth := some 400, minHeight := some 300 }]

  let result : MeasureResult := (measureWidget (M := TestM) widget 10000 10000 : TestM _)
  let layouts := Trellis.layout result.node 200 200
  -- CRITICAL: Must call applyContentScale before collectCommands!
  let layoutsWithScale : Trellis.LayoutResult := (applyContentScale (M := TestM) result.widget layouts : TestM _)

  let commands := collectCommands result.widget layoutsWithScale

  -- Check that pushScale command is emitted
  let hasScale := commands.any fun cmd =>
    match cmd with
    | .pushScale _ _ => true
    | _ => false
  ensure hasScale "Expected pushScale command for contentScale container"

test "collectCommands WITHOUT applyContentScale has NO scale transform" := do
  -- This test documents the bug: if you forget to call applyContentScale,
  -- the scale transform is not emitted
  let widget := Widget.flex 1 none FlexContainer.column
    { contentScale := some { mode := .contain } }
    #[Widget.rect 2 none { minWidth := some 400, minHeight := some 300 }]

  let result : MeasureResult := (measureWidget (M := TestM) widget 10000 10000 : TestM _)
  let layouts := Trellis.layout result.node 200 200
  -- Note: NOT calling applyContentScale here (the bug!)

  let commands := collectCommands result.widget layouts

  -- Without applyContentScale, there should be NO pushScale command
  let hasScale := commands.any fun cmd =>
    match cmd with
    | .pushScale _ _ => true
    | _ => false
  ensure (!hasScale) "Expected NO pushScale when applyContentScale is not called"

#generate_tests

end Afferent.Tests.ContentScaleTests

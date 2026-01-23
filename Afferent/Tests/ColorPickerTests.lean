/-
  ColorPicker Widget Tests
  Unit tests for the color picker widget functionality.
-/
import Afferent.Tests.Framework
import Afferent.Canopy.Widget.Input.ColorPicker
import Afferent.Canopy.Reactive.Component
import Afferent.Arbor
import Tincture
import Trellis

namespace Afferent.Tests.ColorPickerTests

open Crucible
open Afferent.Tests
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Afferent.Arbor
open Reactive Reactive.Host
open Tincture (HSV Color)

testSuite "ColorPicker Tests"

/-- Test font ID for widget building tests. -/
def testFont : FontId := { id := 0, name := "test", size := 14.0 }

/-- Test theme for widget tests. -/
def testTheme : Theme := { Theme.dark with font := testFont, smallFont := testFont }

/-! ## Configuration Tests -/

test "ColorPickerConfig default values" := do
  let config := ColorPickerConfig.default
  shouldBeNear config.squareSize 180.0
  shouldBeNear config.hueBarWidth 24.0
  shouldBeNear config.alphaBarWidth 24.0
  shouldBeNear config.gap 8.0
  shouldBeNear config.previewHeight 30.0
  shouldBeNear config.svIndicatorRadius 6.0
  shouldBeNear config.barIndicatorHeight 4.0
  shouldBeNear config.cornerRadius 4.0

test "ColorPickerConfig noAlpha" := do
  let config := ColorPickerConfig.noAlpha
  shouldBeNear config.alphaBarWidth 0.0

/-! ## State Tests -/

test "ColorPickerState default values" := do
  let state : ColorPickerState := {}
  shouldBeNear state.hsv.h 0.0
  shouldBeNear state.hsv.s 1.0
  shouldBeNear state.hsv.v 1.0
  shouldBeNear state.alpha 1.0
  ensure (state.dragTarget == .none) "Default drag target should be none"

test "ColorPickerDragTarget equality" := do
  ensure (ColorPickerDragTarget.none == ColorPickerDragTarget.none) "none == none"
  ensure (ColorPickerDragTarget.svSquare == ColorPickerDragTarget.svSquare) "svSquare == svSquare"
  ensure (ColorPickerDragTarget.hueBar == ColorPickerDragTarget.hueBar) "hueBar == hueBar"
  ensure (ColorPickerDragTarget.alphaBar == ColorPickerDragTarget.alphaBar) "alphaBar == alphaBar"
  ensure (ColorPickerDragTarget.none != ColorPickerDragTarget.svSquare) "none != svSquare"

/-! ## Position Conversion Tests -/

test "svFromPosition center" := do
  let rect : Trellis.LayoutRect := { x := 0, y := 0, width := 180, height := 180 }
  let (s, v) := ColorPicker.svFromPosition rect 90 90
  shouldBeNear s 0.5
  shouldBeNear v 0.5

test "svFromPosition corners" := do
  let rect : Trellis.LayoutRect := { x := 0, y := 0, width := 180, height := 180 }
  -- Top-left: s=0, v=1
  let (s1, v1) := ColorPicker.svFromPosition rect 0 0
  shouldBeNear s1 0.0
  shouldBeNear v1 1.0
  -- Top-right: s=1, v=1
  let (s2, v2) := ColorPicker.svFromPosition rect 180 0
  shouldBeNear s2 1.0
  shouldBeNear v2 1.0
  -- Bottom-left: s=0, v=0
  let (s3, v3) := ColorPicker.svFromPosition rect 0 180
  shouldBeNear s3 0.0
  shouldBeNear v3 0.0
  -- Bottom-right: s=1, v=0
  let (s4, v4) := ColorPicker.svFromPosition rect 180 180
  shouldBeNear s4 1.0
  shouldBeNear v4 0.0

test "svFromPosition clamps out of bounds" := do
  let rect : Trellis.LayoutRect := { x := 0, y := 0, width := 180, height := 180 }
  -- Beyond left
  let (s1, _v1) := ColorPicker.svFromPosition rect (-10) 90
  shouldBeNear s1 0.0
  -- Beyond right
  let (s2, _v2) := ColorPicker.svFromPosition rect 200 90
  shouldBeNear s2 1.0
  -- Beyond top
  let (_s3, v3) := ColorPicker.svFromPosition rect 90 (-10)
  shouldBeNear v3 1.0
  -- Beyond bottom
  let (_s4, v4) := ColorPicker.svFromPosition rect 90 200
  shouldBeNear v4 0.0

test "svFromPosition with offset rect" := do
  let rect : Trellis.LayoutRect := { x := 100, y := 50, width := 180, height := 180 }
  -- Center of the offset rect
  let (s, v) := ColorPicker.svFromPosition rect 190 140
  shouldBeNear s 0.5
  shouldBeNear v 0.5

test "hueFromPosition center" := do
  let rect : Trellis.LayoutRect := { x := 0, y := 0, width := 24, height := 180 }
  let h := ColorPicker.hueFromPosition rect 90
  shouldBeNear h 0.5

test "hueFromPosition edges" := do
  let rect : Trellis.LayoutRect := { x := 0, y := 0, width := 24, height := 180 }
  -- Top: h=0
  let h1 := ColorPicker.hueFromPosition rect 0
  shouldBeNear h1 0.0
  -- Bottom: h=1
  let h2 := ColorPicker.hueFromPosition rect 180
  shouldBeNear h2 1.0

test "hueFromPosition clamps" := do
  let rect : Trellis.LayoutRect := { x := 0, y := 0, width := 24, height := 180 }
  let h1 := ColorPicker.hueFromPosition rect (-10)
  shouldBeNear h1 0.0
  let h2 := ColorPicker.hueFromPosition rect 200
  shouldBeNear h2 1.0

test "alphaFromPosition center" := do
  let rect : Trellis.LayoutRect := { x := 0, y := 0, width := 24, height := 180 }
  let a := ColorPicker.alphaFromPosition rect 90
  shouldBeNear a 0.5

test "alphaFromPosition edges" := do
  let rect : Trellis.LayoutRect := { x := 0, y := 0, width := 24, height := 180 }
  -- Top: alpha=1 (opaque)
  let a1 := ColorPicker.alphaFromPosition rect 0
  shouldBeNear a1 1.0
  -- Bottom: alpha=0 (transparent)
  let a2 := ColorPicker.alphaFromPosition rect 180
  shouldBeNear a2 0.0

test "alphaFromPosition clamps" := do
  let rect : Trellis.LayoutRect := { x := 0, y := 0, width := 24, height := 180 }
  let a1 := ColorPicker.alphaFromPosition rect (-10)
  shouldBeNear a1 1.0
  let a2 := ColorPicker.alphaFromPosition rect 200
  shouldBeNear a2 0.0

/-! ## HSV Conversion Tests -/

test "HSV to Color round trip" := do
  -- Red
  let red := Color.red
  let redHSV := HSV.fromColor red
  shouldBeNear redHSV.h 0.0
  shouldBeNear redHSV.s 1.0
  shouldBeNear redHSV.v 1.0
  let redBack := HSV.toColor redHSV 1.0
  shouldBeNear redBack.r 1.0
  shouldBeNear redBack.g 0.0
  shouldBeNear redBack.b 0.0

test "HSV from different colors" := do
  -- Green (hue ~0.33)
  let green := Color.green
  let greenHSV := HSV.fromColor green
  -- Hue is approximately 0.33 for green (tolerance handled by shouldBeNear default)
  ensure (greenHSV.h > 0.3 && greenHSV.h < 0.4) s!"Green hue should be ~0.33, got {greenHSV.h}"
  shouldBeNear greenHSV.s 1.0
  shouldBeNear greenHSV.v 1.0

  -- Blue (hue ~0.67)
  let blue := Color.blue
  let blueHSV := HSV.fromColor blue
  -- Hue is approximately 0.67 for blue (tolerance handled by shouldBeNear default)
  ensure (blueHSV.h > 0.6 && blueHSV.h < 0.7) s!"Blue hue should be ~0.67, got {blueHSV.h}"
  shouldBeNear blueHSV.s 1.0
  shouldBeNear blueHSV.v 1.0

test "HSV gray has zero saturation" := do
  let gray := Color.gray 0.5
  let grayHSV := HSV.fromColor gray
  shouldBeNear grayHSV.s 0.0
  shouldBeNear grayHSV.v 0.5

/-! ## Visual Structure Tests -/

test "colorPickerVisual creates row container" := do
  let config := ColorPickerConfig.default
  let state : ColorPickerState := {}
  let builder := colorPickerVisual "picker" "sv" "hue" "alpha" config state testTheme
  let (widget, _) ← builder.run {}
  match widget with
  | .flex _ _ props _ children =>
    ensure (props.direction == .row) "Top level should be row"
    ensure (children.size == 2) s!"Expected 2 children (sv square + right column), got {children.size}"
  | _ => ensure false "Expected flex widget"

test "colorPickerVisual noAlpha creates row with 2 children" := do
  let config := ColorPickerConfig.noAlpha
  let state : ColorPickerState := {}
  let builder := colorPickerVisual "picker" "sv" "hue" "alpha" config state testTheme
  let (widget, _) ← builder.run {}
  match widget with
  | .flex _ _ props _ children =>
    ensure (props.direction == .row) "Top level should be row"
    ensure (children.size == 2) s!"Expected 2 children, got {children.size}"
  | _ => ensure false "Expected flex widget"

/-! ## CustomSpec Tests -/

test "svSquareSpec measure returns correct size" := do
  let spec := ColorPicker.svSquareSpec 0.0 0.5 0.5 180.0 6.0
  let (w, h) := spec.measure 0 0
  shouldBeNear w 180.0
  shouldBeNear h 180.0

test "hueBarSpec measure returns correct size" := do
  let spec := ColorPicker.hueBarSpec 0.5 24.0 180.0 4.0 4.0
  let (w, h) := spec.measure 0 0
  shouldBeNear w 24.0
  shouldBeNear h 180.0

test "alphaBarSpec measure returns correct size" := do
  let spec := ColorPicker.alphaBarSpec 1.0 { h := 0.0, s := 1.0, v := 1.0 } 24.0 180.0 4.0 4.0
  let (w, h) := spec.measure 0 0
  shouldBeNear w 24.0
  shouldBeNear h 180.0

test "colorPreviewSpec measure returns correct size" := do
  let spec := ColorPicker.colorPreviewSpec Color.red 56.0 30.0 4.0
  let (w, h) := spec.measure 0 0
  shouldBeNear w 56.0
  shouldBeNear h 30.0

/-! ## Render Command Generation Tests -/

test "svSquareSpec generates render commands" := do
  let spec := ColorPicker.svSquareSpec 0.0 0.5 0.5 180.0 6.0
  let layout : Trellis.ComputedLayout := {
    nodeId := 0
    borderRect := { x := 0, y := 0, width := 180, height := 180 }
    contentRect := { x := 0, y := 0, width := 180, height := 180 }
  }
  let cmds := spec.collect layout
  -- 2 gradient fills (saturation + value) + 2 stroke commands for indicator
  ensure (cmds.size == 4) s!"Expected 4 commands (2 gradients + 2 indicator), got {cmds.size}"

test "hueBarSpec generates render commands" := do
  let spec := ColorPicker.hueBarSpec 0.5 24.0 180.0 4.0 4.0
  let layout : Trellis.ComputedLayout := {
    nodeId := 0
    borderRect := { x := 0, y := 0, width := 24, height := 180 }
    contentRect := { x := 0, y := 0, width := 24, height := 180 }
  }
  let cmds := spec.collect layout
  -- 1 gradient fill + 2 indicator commands (fill + stroke)
  ensure (cmds.size == 3) s!"Expected 3 commands (1 gradient + 2 indicator), got {cmds.size}"

/-! ## Clamp Function Tests -/

test "clamp01 within range" := do
  shouldBeNear (ColorPicker.clamp01 0.5) 0.5
  shouldBeNear (ColorPicker.clamp01 0.0) 0.0
  shouldBeNear (ColorPicker.clamp01 1.0) 1.0

test "clamp01 below range" := do
  shouldBeNear (ColorPicker.clamp01 (-0.5)) 0.0
  shouldBeNear (ColorPicker.clamp01 (-100.0)) 0.0

test "clamp01 above range" := do
  shouldBeNear (ColorPicker.clamp01 1.5) 1.0
  shouldBeNear (ColorPicker.clamp01 100.0) 1.0



end Afferent.Tests.ColorPickerTests

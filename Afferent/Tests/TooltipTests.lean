/-
  Tooltip Widget Tests
  Unit tests for the tooltip widget functionality.
-/
import Afferent.Tests.Framework
import Afferent.Arbor
import Afferent.Arbor.Widget.DSL
import Afferent.Canopy.Widget.Display.Tooltip
import Trellis

namespace Afferent.Tests.TooltipTests

open Crucible
open Afferent.Tests
open Afferent.Arbor
open Afferent.Canopy
open Trellis

testSuite "Tooltip Tests"

/-- Test font ID for widget building tests. -/
def testFont : FontId := { id := 0, name := "test", size := 14.0 }

/-- Test theme for widget tests. -/
def testTheme : Theme := { Theme.dark with font := testFont, smallFont := testFont }

/-! ## Configuration Tests -/

test "TooltipConfig default values" := do
  let config : TooltipConfig := { text := "Hello" }
  ensure (config.text == "Hello") "Text should be 'Hello'"
  ensure (config.position == .top) "Default position should be .top"
  ensure (config.delay == 0.3) s!"Default delay should be 0.3, got {config.delay}"

test "TooltipConfig custom values" := do
  let config : TooltipConfig := {
    text := "Custom tooltip"
    position := .bottom
    delay := 0.5
  }
  ensure (config.text == "Custom tooltip") "Text should match"
  ensure (config.position == .bottom) "Position should be .bottom"
  ensure (config.delay == 0.5) s!"Delay should be 0.5, got {config.delay}"

/-! ## Offset Calculation Tests -/

test "calculateOffset for top position" := do
  let dims := Tooltip.defaultDimensions
  let tooltipWidth := 100.0  -- measured tooltip width (not used for top)
  let (top, left) := Tooltip.calculateOffset 80 30 .top tooltipWidth dims
  -- For top: negative offset to go above parent
  let expectedTop := -(dims.tooltipHeight + dims.gap)
  ensure (top == expectedTop) s!"Expected top={expectedTop}, got {top}"
  ensure (left == 0) s!"Expected left=0, got {left}"

test "calculateOffset for bottom position" := do
  let dims := Tooltip.defaultDimensions
  let tooltipWidth := 100.0  -- measured tooltip width (not used for bottom)
  let (top, left) := Tooltip.calculateOffset 80 30 .bottom tooltipWidth dims
  -- For bottom: top = targetHeight + gap
  let expectedTop := 30 + dims.gap
  ensure (top == expectedTop) s!"Expected top={expectedTop}, got {top}"
  ensure (left == 0) s!"Expected left=0, got {left}"

test "calculateOffset for left position" := do
  let dims := Tooltip.defaultDimensions
  let tooltipWidth := 100.0  -- measured tooltip width
  let (top, left) := Tooltip.calculateOffset 80 30 .left tooltipWidth dims
  -- For left: negative left offset based on measured width
  let expectedLeft := -(tooltipWidth + dims.gap)
  ensure (top == 0) s!"Expected top=0, got {top}"
  ensure (left == expectedLeft) s!"Expected left={expectedLeft}, got {left}"

test "calculateOffset for right position" := do
  let dims := Tooltip.defaultDimensions
  let tooltipWidth := 100.0  -- measured tooltip width (not used for right)
  let (top, left) := Tooltip.calculateOffset 80 30 .right tooltipWidth dims
  -- For right: left = targetWidth + gap
  let expectedLeft := 80 + dims.gap
  ensure (top == 0) s!"Expected top=0, got {top}"
  ensure (left == expectedLeft) s!"Expected left={expectedLeft}, got {left}"

/-! ## TooltipState Tests -/

test "TooltipState default is not visible" := do
  let state : TooltipState := { hoverElapsed := none, isVisible := false }
  ensure (!state.isVisible) "Default state should not be visible"
  ensure (state.hoverElapsed.isNone) "Default hover elapsed should be none"

test "TooltipState tracks hover elapsed time" := do
  let state : TooltipState := { hoverElapsed := some 0.2, isVisible := false }
  match state.hoverElapsed with
  | some elapsed => ensure (elapsed == 0.2) s!"Expected elapsed=0.2, got {elapsed}"
  | none => ensure false "Expected some elapsed time"

/-! ## Tooltip Dimensions Tests -/

test "Tooltip.Dimensions default values" := do
  let dims := Tooltip.defaultDimensions
  ensure (dims.padding == 6.0) s!"Default padding should be 6, got {dims.padding}"
  ensure (dims.cornerRadius == 4.0) s!"Default cornerRadius should be 4, got {dims.cornerRadius}"
  ensure (dims.gap == 6.0) s!"Default gap should be 6, got {dims.gap}"
  ensure (dims.tooltipHeight == 24.0) s!"Default tooltipHeight should be 24, got {dims.tooltipHeight}"

/-! ## TooltipPosition Tests -/

test "TooltipPosition enumeration" := do
  let positions : Array TooltipPosition := #[.top, .bottom, .left, .right]
  ensure (positions.size == 4) "Should have 4 positions"
  ensure (positions[0]! == .top) "First should be .top"
  ensure (positions[1]! == .bottom) "Second should be .bottom"
  ensure (positions[2]! == .left) "Third should be .left"
  ensure (positions[3]! == .right) "Fourth should be .right"



end Afferent.Tests.TooltipTests

/-
  Afferent CSS Macro Tests
  Unit tests for the css! macro.
-/
import Afferent.Tests.Framework
import Afferent.Arbor.Style.CSS
import Afferent.Arbor.Widget.Core
import Trellis
import Tincture

namespace Afferent.Tests.CSSTests

open Crucible
open Afferent.Tests
open Afferent.Arbor
open Afferent.Arbor.CSS
open Trellis
open Tincture

testSuite "CSS Macro Tests"

/-! ## Basic Properties -/

test "css! with background-color" := do
  let style := css! {
    background-color: red;
  }
  ensure style.backgroundColor.isSome "backgroundColor should be set"

test "css! with padding" := do
  let style := css! {
    padding: 10;
  }
  ensure (style.padding.top == 10.0) s!"Expected padding.top = 10, got {style.padding.top}"
  ensure (style.padding.right == 10.0) s!"Expected padding.right = 10, got {style.padding.right}"
  ensure (style.padding.bottom == 10.0) s!"Expected padding.bottom = 10, got {style.padding.bottom}"
  ensure (style.padding.left == 10.0) s!"Expected padding.left = 10, got {style.padding.left}"

test "css! with padding px unit" := do
  let style := css! {
    padding: 20px;
  }
  ensure (style.padding.top == 20.0) s!"Expected padding.top = 20, got {style.padding.top}"

test "css! with min-width" := do
  let style := css! {
    min-width: 100;
  }
  ensure style.minWidth.isSome "minWidth should be set"
  ensure (style.minWidth.getD 0 == 100.0) s!"Expected minWidth = 100, got {style.minWidth.getD 0}"

test "css! with border-width" := do
  let style := css! {
    border-width: 2;
  }
  ensure (style.borderWidth == 2.0) s!"Expected borderWidth = 2, got {style.borderWidth}"

test "css! with corner-radius" := do
  let style := css! {
    corner-radius: 8;
  }
  ensure (style.cornerRadius == 8.0) s!"Expected cornerRadius = 8, got {style.cornerRadius}"

/-! ## Dimension Properties -/

test "css! with width auto" := do
  let style := css! {
    width: auto;
  }
  ensure style.width.isAuto "width should be auto"

test "css! with width in px" := do
  let style := css! {
    width: 200px;
  }
  match style.width with
  | .length l => ensure (l == 200.0) s!"Expected width = 200, got {l}"
  | _ => ensure false "width should be length"

test "css! with height in percent" := do
  let style := css! {
    height: 100pct;
  }
  match style.height with
  | .percent p => ensure (p == 1.0) s!"Expected height = 100%, got {p}"
  | _ => ensure false "height should be percent"

/-! ## Flex Properties -/

test "css! with flex-grow" := do
  let style := css! {
    flex-grow: 1;
  }
  ensure style.flexItem.isSome "flexItem should be set"
  match style.flexItem with
  | some item => ensure (item.grow == 1.0) s!"Expected grow = 1, got {item.grow}"
  | none => ensure false "flexItem should be some"

test "css! with flex shorthand" := do
  let style := css! {
    flex: 2;
  }
  ensure style.flexItem.isSome "flexItem should be set"
  match style.flexItem with
  | some item => ensure (item.grow == 2.0) s!"Expected grow = 2, got {item.grow}"
  | none => ensure false "flexItem should be some"

/-! ## Border Properties -/

test "css! with border-width and border-color" := do
  let style := css! {
    border-width: 1;
    border-color: white;
  }
  ensure (style.borderWidth == 1.0) s!"Expected borderWidth = 1, got {style.borderWidth}"
  ensure style.borderColor.isSome "borderColor should be set"

/-! ## Multiple Properties -/

test "css! with multiple properties" := do
  let style := css! {
    background-color: blue;
    padding: 16;
    border-width: 2;
    min-width: 100;
    min-height: 50;
  }
  ensure style.backgroundColor.isSome "backgroundColor should be set"
  ensure (style.padding.top == 16.0) s!"Expected padding = 16, got {style.padding.top}"
  ensure (style.borderWidth == 2.0) s!"Expected borderWidth = 2, got {style.borderWidth}"
  ensure (style.minWidth.getD 0 == 100.0) s!"Expected minWidth = 100, got {style.minWidth.getD 0}"
  ensure (style.minHeight.getD 0 == 50.0) s!"Expected minHeight = 50, got {style.minHeight.getD 0}"

/-! ## Hex Colors -/

test "css! with hex color" := do
  let style := css! {
    background-color: #ff0000;
  }
  ensure style.backgroundColor.isSome "backgroundColor should be set"

test "css! with short hex color" := do
  let style := css! {
    background-color: #f00;
  }
  ensure style.backgroundColor.isSome "backgroundColor should be set"

/-! ## Empty Style -/

test "css! with empty block" := do
  let style := css! {}
  -- Should produce default BoxStyle
  ensure style.backgroundColor.isNone "backgroundColor should be none"
  ensure (style.borderWidth == 0.0) s!"borderWidth should be 0, got {style.borderWidth}"

/-! ## Color Functions -/

test "css! with rgb() color" := do
  let style := css! {
    background-color: rgb(255, 128, 0);
  }
  ensure style.backgroundColor.isSome "backgroundColor should be set"

test "css! with rgba() color" := do
  -- rgba uses 0-255 for all values including alpha
  let style := css! {
    background-color: rgba(255, 0, 0, 128);
  }
  ensure style.backgroundColor.isSome "backgroundColor should be set"

test "css! with rgba() float alpha" := do
  -- rgba with float alpha (0.0-1.0)
  let style := css! {
    background-color: rgba(255, 0, 0, 0.5);
  }
  ensure style.backgroundColor.isSome "backgroundColor should be set"

test "css! with gray() color" := do
  -- gray uses 0-100 percentage
  let style := css! {
    background-color: gray(50);
  }
  ensure style.backgroundColor.isSome "backgroundColor should be set"

test "css! with hsv() color" := do
  -- hsv uses 0.0-1.0 for all values
  let style := css! {
    background-color: hsv(0.0, 1.0, 1.0);
  }
  ensure style.backgroundColor.isSome "backgroundColor should be set"

test "css! with hsva() color" := do
  -- hsva uses 0.0-1.0 for all values
  let style := css! {
    background-color: hsva(0.33, 1.0, 1.0, 0.5);
  }
  ensure style.backgroundColor.isSome "backgroundColor should be set"

test "css! with hsl() color" := do
  -- hsl uses 0.0-1.0 for all values
  let style := css! {
    background-color: hsl(0.0, 1.0, 0.5);
  }
  ensure style.backgroundColor.isSome "backgroundColor should be set"

test "css! with hsla() color" := do
  -- hsla uses 0.0-1.0 for all values
  let style := css! {
    background-color: hsla(0.66, 1.0, 0.5, 0.8);
  }
  ensure style.backgroundColor.isSome "backgroundColor should be set"

test "css! with border using rgba" := do
  -- rgba(r, g, b, a) - all values 0-255
  let style := css! {
    border-color: rgba(128, 128, 128, 90);
    border-width: 1;
  }
  ensure style.borderColor.isSome "borderColor should be set"
  ensure (style.borderWidth == 1.0) s!"borderWidth should be 1, got {style.borderWidth}"

/-! ## Float Values -/

test "css! with float flex-grow" := do
  let style := css! {
    flex-grow: 0.5;
  }
  match style.flexItem with
  | some item => ensure (item.grow == 0.5) s!"Expected grow = 0.5, got {item.grow}"
  | none => ensure false "flexItem should be some"

test "css! with float padding" := do
  let style := css! {
    padding: 1.5;
  }
  ensure (style.padding.top == 1.5) s!"Expected padding = 1.5, got {style.padding.top}"

test "css! with float px unit" := do
  let style := css! {
    min-width: 10.5px;
  }
  ensure (style.minWidth.getD 0 == 10.5) s!"Expected minWidth = 10.5, got {style.minWidth.getD 0}"

test "css! with float border-width" := do
  let style := css! {
    border-width: 0.5;
  }
  ensure (style.borderWidth == 0.5) s!"Expected borderWidth = 0.5, got {style.borderWidth}"



end Afferent.Tests.CSSTests

/-
  ListBox Widget Tests
  Unit tests for the list box widget functionality.
-/
import Afferent.Tests.Framework
import Afferent.Canopy.Widget.Data.ListBox
import Afferent.Canopy.Reactive.Component
import Afferent.Arbor

namespace Afferent.Tests.ListBoxTests

open Crucible
open Afferent.Tests
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Afferent.Arbor
open Reactive Reactive.Host

testSuite "ListBox Tests"

/-- Test font ID for widget building tests. -/
def testFont : FontId := { id := 0, name := "test", size := 14.0 }

/-- Test theme for widget tests. -/
def testTheme : Theme := { Theme.dark with font := testFont, smallFont := testFont }

/-! ## Widget Tree Helpers -/

/-- Find the first scroll widget in a tree (depth-first). -/
partial def findScrollWidget (w : Widget) : Option (WidgetId × ScrollbarRenderConfig) :=
  match w with
  | .scroll id _ _ _ _ _ scrollbarConfig _ => some (id, scrollbarConfig)
  | _ =>
      w.children.foldl (init := none) fun acc child =>
        match acc with
        | some _ => acc
        | none => findScrollWidget child

/-- Collect widget IDs for list box items (by name prefix). -/
partial def collectListBoxItemIds (w : Widget) : Array WidgetId :=
  let ids :=
    match w.name? with
    | some name =>
        if name.startsWith "listbox-item-" then #[w.id] else #[]
    | none => #[]
  w.children.foldl (init := ids) fun acc child =>
    acc ++ collectListBoxItemIds child

/-! ## ListBoxSelectionMode Tests -/

test "ListBoxSelectionMode.single" := do
  let mode := ListBoxSelectionMode.single
  ensure (mode == .single) "Should be single"

test "ListBoxSelectionMode.multiple" := do
  let mode := ListBoxSelectionMode.multiple
  ensure (mode == .multiple) "Should be multiple"

/-! ## ListBoxConfig Tests -/

test "ListBoxConfig default values" := do
  let config := ListBox.defaultConfig
  ensure (config.itemHeight == 32.0) s!"Default item height should be 32, got {config.itemHeight}"
  ensure (config.itemPadding == 12.0) s!"Default item padding should be 12, got {config.itemPadding}"
  ensure (config.maxVisibleItems == 6) s!"Default max visible items should be 6, got {config.maxVisibleItems}"
  ensure (config.selectionMode == .single) "Default selection mode should be single"
  ensure (config.borderWidth == 1.0) s!"Default border width should be 1, got {config.borderWidth}"

test "ListBoxConfig custom values" := do
  let config : ListBoxConfig := {
    itemHeight := 40.0
    itemPadding := 16.0
    maxVisibleItems := 10
    selectionMode := .multiple
    borderWidth := 2.0
  }
  ensure (config.itemHeight == 40.0) "Item height should be 40"
  ensure (config.itemPadding == 16.0) "Item padding should be 16"
  ensure (config.maxVisibleItems == 10) "Max visible items should be 10"
  ensure (config.selectionMode == .multiple) "Selection mode should be multiple"
  ensure (config.borderWidth == 2.0) "Border width should be 2"

/-! ## Selection Logic Tests -/

test "updateSelection single mode replaces selection" := do
  let result := ListBox.updateSelection .single 0 #[]
  ensure (result == #[0]) "Single mode should select clicked item"
  let result2 := ListBox.updateSelection .single 2 #[0]
  ensure (result2 == #[2]) "Single mode should replace selection"
  let result3 := ListBox.updateSelection .single 1 #[1]
  ensure (result3 == #[1]) "Clicking same item should keep it selected"

test "updateSelection multiple mode toggles selection" := do
  let result := ListBox.updateSelection .multiple 0 #[]
  ensure (result == #[0]) "Multiple mode should add to selection"
  let result2 := ListBox.updateSelection .multiple 2 #[0]
  ensure (result2 == #[0, 2]) "Multiple mode should add second item"
  let result3 := ListBox.updateSelection .multiple 0 #[0, 2]
  ensure (result3 == #[2]) "Multiple mode should remove clicked item"

test "updateSelection multiple mode with many items" := do
  let result := ListBox.updateSelection .multiple 1 #[0, 2, 4]
  ensure (result == #[0, 2, 4, 1]) "Should add item to existing selection"
  let result2 := ListBox.updateSelection .multiple 2 #[0, 2, 4]
  ensure (result2 == #[0, 4]) "Should remove item from selection"

/-! ## Typical ListBox Configuration Tests -/

test "typical fruits list" := do
  let items : Array String := #["Apple", "Banana", "Cherry", "Date", "Elderberry"]
  ensure (items.size == 5) "Should have 5 items"
  ensure (items[0]! == "Apple") "First item should be 'Apple'"
  ensure (items[4]! == "Elderberry") "Last item should be 'Elderberry'"

test "empty list" := do
  let items : Array String := #[]
  ensure (items.size == 0) "Should have 0 items"

test "single item list" := do
  let items : Array String := #["Only One"]
  ensure (items.size == 1) "Should have 1 item"
  ensure (items[0]! == "Only One") "Item should be 'Only One'"

test "selection on empty array" := do
  let result := ListBox.updateSelection .single 0 #[]
  ensure (result == #[0]) "Should select first item"
  let result2 := ListBox.updateSelection .multiple 5 #[]
  ensure (result2 == #[5]) "Should select item at index 5"

/-! ## Visual Structure Tests -/

test "listBoxItemVisual creates widget with correct name" := do
  let itemName := "listbox-item-0"
  let builder := listBoxItemVisual itemName "Apple" false false testTheme
  let (widget, _) ← builder.run {}
  -- Check the widget has the correct name
  let widgetName := Widget.name? widget
  ensure (widgetName == some itemName) s!"Expected name '{itemName}', got {widgetName}"

test "listBoxItemVisual creates widget with name in structure" := do
  let itemName := "test-item-5"
  let builder := listBoxItemVisual itemName "Banana" true false testTheme
  let (widget, _) ← builder.run {}
  -- Use findWidgetIdByName to verify the name is findable
  let found := findWidgetIdByName widget itemName
  ensure found.isSome s!"Widget with name '{itemName}' should be findable"

test "listBoxItemsVisual creates column with items" := do
  let itemNameFn (i : Nat) : String := s!"item-{i}"
  let items := #["Apple", "Banana", "Cherry"]
  let builder := listBoxItemsVisual itemNameFn items #[] none testTheme
  let (widget, _) ← builder.run {}
  -- The outer widget should be a flex with 3 children
  match widget with
  | .flex _ _ props _ children =>
    ensure (props.direction == .column) "Should be a column"
    ensure (children.size == 3) s!"Should have 3 children, got {children.size}"
  | _ => ensure false "Expected flex widget"

test "listBoxItemsVisual items have correct names" := do
  let itemNameFn (i : Nat) : String := s!"test-item-{i}"
  let items := #["Apple", "Banana", "Cherry"]
  let builder := listBoxItemsVisual itemNameFn items #[] none testTheme
  let (widget, _) ← builder.run {}
  -- Verify each item name is findable
  for i in [:items.size] do
    let found := findWidgetIdByName widget (itemNameFn i)
    ensure found.isSome s!"Item '{itemNameFn i}' should be findable in widget tree"

test "listBoxItemsVisual container has NO name" := do
  -- This test documents the current behavior - the container is unnamed
  let itemNameFn (i : Nat) : String := s!"item-{i}"
  let items := #["Apple", "Banana"]
  let builder := listBoxItemsVisual itemNameFn items #[] none testTheme
  let (widget, _) ← builder.run {}
  -- The container itself has no name
  let containerName := Widget.name? widget
  ensure (containerName.isNone) s!"Container should have no name, but got {containerName}"

/-! ## Item Index Calculation Tests -/

/-- Helper to compute item index from click position - mirrors internal logic. -/
def computeItemIndexTest (containerX containerY containerW containerH : Float)
    (posX posY scrollOffset itemHeight : Float) (itemCount : Nat) : Option Nat :=
  if posX >= containerX && posX <= containerX + containerW &&
     posY >= containerY && posY <= containerY + containerH then
    let relativeY := posY - containerY + scrollOffset
    let itemIndex := (relativeY / itemHeight).floor.toUInt64.toNat
    if itemIndex < itemCount then some itemIndex else none
  else none

test "computeItemIndex returns item 0 for click at top" := do
  -- Container at (100, 100), size 200x192, item height 32, 6 items
  let result := computeItemIndexTest 100 100 200 192 150 110 0 32 6
  ensure (result == some 0) s!"Click near top should hit item 0, got {result}"

test "computeItemIndex returns correct item for middle click" := do
  -- Click at y=180 relative to container at y=100 means relativeY=80
  -- 80 / 32 = 2.5 -> floor = 2
  let result := computeItemIndexTest 100 100 200 192 150 180 0 32 6
  ensure (result == some 2) s!"Click at y=180 should hit item 2, got {result}"

test "computeItemIndex accounts for scroll offset" := do
  -- With scroll offset of 64 (2 items worth), click at y=110 (relativeY=10)
  -- becomes relativeY=10+64=74, 74/32=2.3 -> floor = 2
  let result := computeItemIndexTest 100 100 200 192 150 110 64 32 10
  ensure (result == some 2) s!"With scroll=64, click at y=110 should hit item 2, got {result}"

test "computeItemIndex returns none for click outside container" := do
  let result := computeItemIndexTest 100 100 200 192 50 50 0 32 6
  ensure result.isNone "Click outside container should return none"

test "computeItemIndex returns none for click below items" := do
  -- 6 items at height 32 = 192px total content
  -- Click at y=350 with container at y=100 and height=192
  let result := computeItemIndexTest 100 100 200 192 150 350 0 32 6
  ensure result.isNone "Click below visible items should return none"

test "computeItemIndex returns none for item index beyond count" := do
  -- Container shows all items but we click in empty space at bottom
  -- relativeY = 250-100 + 0 = 150, 150/32 = 4.6 -> 4
  -- If only 4 items, index 4 is beyond count
  let result := computeItemIndexTest 100 100 200 200 150 250 0 32 4
  ensure result.isNone s!"Index beyond item count should return none, got {result}"

/-! ## Hit Path Tests -/

test "hitWidget finds named item in widget tree" := do
  -- Create a simple list box visual
  let itemNameFn (i : Nat) : String := s!"listbox-item-{i}"
  let items := #["Apple", "Banana", "Cherry"]
  let builder := listBoxItemsVisual itemNameFn items #[1] none testTheme
  let (widget, _) ← builder.run {}

  -- Verify findWidgetIdByName works for each item
  for i in [:items.size] do
    let name := itemNameFn i
    let found := findWidgetIdByName widget name
    ensure found.isSome s!"findWidgetIdByName should find '{name}'"

test "findWidgetIdByName returns none for non-existent name" := do
  let itemNameFn (i : Nat) : String := s!"item-{i}"
  let items := #["Apple", "Banana"]
  let builder := listBoxItemsVisual itemNameFn items #[] none testTheme
  let (widget, _) ← builder.run {}

  let found := findWidgetIdByName widget "non-existent-container"
  ensure found.isNone "Should not find non-existent widget name"

test "searching for 'listbox-container' in items visual returns none" := do
  -- This test documents why the current hit testing fails
  let itemNameFn (i : Nat) : String := s!"listbox-item-{i}"
  let items := #["Apple", "Banana", "Cherry"]
  let builder := listBoxItemsVisual itemNameFn items #[] none testTheme
  let (widget, _) ← builder.run {}

  -- The current implementation searches for "listbox-container" but
  -- listBoxItemsVisual does NOT name the container
  let found := findWidgetIdByName widget "listbox-container"
  ensure found.isNone "listbox-container should NOT be found (this is the bug)"

/-! ## Widget Tree Layout Tests -/

test "listBoxItemsVisual with 12 items creates all 12 in tree" := do
  let itemNameFn (i : Nat) : String := s!"item-{i}"
  let items := #["0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11"]
  let builder := listBoxItemsVisual itemNameFn items #[] none testTheme
  let (widget, _) ← builder.run {}
  -- Verify ALL items are in the tree (including items 6-11)
  for i in [:12] do
    let found := findWidgetIdByName widget (itemNameFn i)
    ensure found.isSome s!"Item '{itemNameFn i}' (index {i}) should exist in widget tree"

test "all items have unique widget IDs" := do
  let itemNameFn (i : Nat) : String := s!"item-{i}"
  let items := #["0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11"]
  let builder := listBoxItemsVisual itemNameFn items #[] none testTheme
  let (widget, _) ← builder.run {}
  -- Collect all widget IDs
  let mut ids : Array WidgetId := #[]
  for i in [:12] do
    match findWidgetIdByName widget (itemNameFn i) with
    | some wid => ids := ids.push wid
    | none => ensure false s!"Item {i} not found"
  -- Verify uniqueness
  let uniqueIds := ids.toList.eraseDups
  ensure (uniqueIds.length == 12) s!"Should have 12 unique IDs, got {uniqueIds.length}"

test "listBox items reach scrollbar track in scroll container" := do
  let items := (List.range 12).map (fun i => s!"Item {i}") |>.toArray
  let config := { ListBox.defaultConfig with maxVisibleItems := 6 }
  let itemNameFn (i : Nat) : String := s!"listbox-item-{i}"

  -- Build a wide scroll container with list box items inside.
  let contentW := 200.0
  let contentH := items.size.toFloat * config.itemHeight
  let scrollStyle : BoxStyle := {
    width := .percent 1.0
    minWidth := some 400.0
    minHeight := some 200.0
    maxHeight := some 200.0
  }
  let scrollbarConfig : ScrollbarRenderConfig := {}
  let itemsBuilder := listBoxItemsVisual itemNameFn items #[] none testTheme config
  let scrollBuilder := namedScroll "listbox-scroll-test" scrollStyle contentW contentH {} scrollbarConfig itemsBuilder
  let (widget, _) ← scrollBuilder.run {}

  let viewportW := 600.0
  let viewportH := 400.0
  let measureResult : MeasureResult := (measureWidget (M := Id) widget viewportW viewportH)
  let layouts := Trellis.layout measureResult.node viewportW viewportH

  let itemIds := collectListBoxItemIds measureResult.widget
  ensure (!itemIds.isEmpty) "Expected listBox items to be present in widget tree"

  match findScrollWidget measureResult.widget with
  | some (scrollId, scrollbarConfig) =>
    let scrollLayout := layouts.get! scrollId
    ensure (contentH > scrollLayout.contentRect.height)
      s!"Expected content height {contentH} to exceed viewport {scrollLayout.contentRect.height}"
    ensure (scrollLayout.contentRect.width > contentW)
      s!"Expected viewport width {scrollLayout.contentRect.width} to exceed content width {contentW}"
    let itemLayout := layouts.get! itemIds[0]!
    let trackX := scrollLayout.contentRect.x + scrollLayout.contentRect.width - scrollbarConfig.thickness
    let itemRight := itemLayout.borderRect.x + itemLayout.borderRect.width
    ensure (itemLayout.borderRect.width > contentW)
      s!"Expected item width {itemLayout.borderRect.width} to exceed content width {contentW}"
    ensure (itemRight >= trackX)
      s!"Item right edge {itemRight} should reach scrollbar track x {trackX}"
  | none =>
    ensure false "Expected to find a scroll container in listBox widget"



end Afferent.Tests.ListBoxTests

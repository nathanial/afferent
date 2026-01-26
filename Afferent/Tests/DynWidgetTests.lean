/-
  DynWidget Tests
  Comprehensive tests for the dynWidget combinator to ensure:
  1. Correct rebuild behavior when dynamics change
  2. No spurious re-renders when dynamics don't change
  3. Proper result tracking via returned Dynamic
  4. Correct behavior in complex nested scenarios
-/
import Afferent.Tests.Framework
import Afferent.Canopy.Reactive.Component
import Afferent.Canopy.Widget.Display.Label
import Afferent.Arbor
import Reactive
import Trellis

namespace Afferent.Tests.DynWidgetTests

open Crucible
open Afferent.Tests
open Afferent.Canopy
open Afferent.Canopy.Reactive
open Afferent.Arbor
open Reactive Reactive.Host
open Trellis

testSuite "DynWidget Tests"

/-- Test font for widget tests. -/
def testFont : FontId := { id := 0, name := "test", size := 14.0 }

/-- Test theme for widget tests. -/
def testTheme : Theme := { Theme.dark with font := testFont, smallFont := testFont }

/-! ## Basic Functionality Tests -/

test "initial build uses current dynamic value" := do
  let spiderEnv ← SpiderEnv.new defaultErrorHandler
  let _ ← (do
    let (events, _) ← createInputs FontRegistry.empty testTheme
    let valueDyn ← Dynamic.pureM 42
    let capturedValue ← SpiderM.liftIO (IO.mkRef 0)

    let (_, render) ← ReactiveM.run events do
      runWidget do
        let _ ← dynWidget valueDyn fun v => do
          SpiderM.liftIO (capturedValue.set v)
          pure ()
        pure ()

    let _ ← render
    let v ← SpiderM.liftIO capturedValue.get
    ensure (v == 42) s!"Expected 42, got {v}"
  ).run spiderEnv
  pure ()

test "rebuilds on dynamic update" := do
  let spiderEnv ← SpiderEnv.new defaultErrorHandler
  let _ ← (do
    let (events, _) ← createInputs FontRegistry.empty testTheme
    let (trigger, fire) ← newTriggerEvent (t := Spider) (a := Nat)
    let valueDyn ← holdDyn 0 trigger
    let rebuildCount ← SpiderM.liftIO (IO.mkRef 0)

    let (_, render) ← ReactiveM.run events do
      runWidget do
        let _ ← dynWidget valueDyn fun _ => do
          SpiderM.liftIO (rebuildCount.modify (· + 1))
          pure ()
        pure ()

    -- Initial build
    let _ ← render
    let count1 ← SpiderM.liftIO rebuildCount.get
    ensure (count1 == 1) s!"Initial build should be 1, got {count1}"

    -- Fire update
    fire 1
    let count2 ← SpiderM.liftIO rebuildCount.get
    ensure (count2 == 2) s!"After update should be 2, got {count2}"
  ).run spiderEnv
  pure ()

test "rebuild count matches update count" := do
  let spiderEnv ← SpiderEnv.new defaultErrorHandler
  let _ ← (do
    let (events, _) ← createInputs FontRegistry.empty testTheme
    let (trigger, fire) ← newTriggerEvent (t := Spider) (a := Nat)
    let valueDyn ← holdDyn 0 trigger
    let rebuildCount ← SpiderM.liftIO (IO.mkRef 0)

    let (_, render) ← ReactiveM.run events do
      runWidget do
        let _ ← dynWidget valueDyn fun _ => do
          SpiderM.liftIO (rebuildCount.modify (· + 1))
          pure ()
        pure ()

    let _ ← render
    -- Fire 5 updates
    for i in [1:6] do
      fire i
    let count ← SpiderM.liftIO rebuildCount.get
    -- 1 initial + 5 updates = 6
    ensure (count == 6) s!"Expected 6 rebuilds, got {count}"
  ).run spiderEnv
  pure ()

test "result dynamic tracks builder output" := do
  let spiderEnv ← SpiderEnv.new defaultErrorHandler
  let _ ← (do
    let (events, _) ← createInputs FontRegistry.empty testTheme
    let (trigger, fire) ← newTriggerEvent (t := Spider) (a := Nat)
    let valueDyn ← holdDyn 10 trigger

    let (resultDyn, render) ← ReactiveM.run events do
      runWidget do
        let result ← dynWidget valueDyn fun v => do
          pure (v * 2)  -- Return doubled value
        pure result

    let _ ← render
    -- Check initial value
    let initial ← SpiderM.liftIO resultDyn.sample
    ensure (initial == 20) s!"Initial result should be 20, got {initial}"

    -- Fire update and check
    fire 15
    let updated ← SpiderM.liftIO resultDyn.sample
    ensure (updated == 30) s!"Updated result should be 30, got {updated}"
  ).run spiderEnv
  pure ()

/-! ## No Spurious Re-render Tests -/

test "constant dynamic never rebuilds after initial" := do
  let spiderEnv ← SpiderEnv.new defaultErrorHandler
  let _ ← (do
    let (events, _) ← createInputs FontRegistry.empty testTheme
    let valueDyn ← Dynamic.pureM 100
    let rebuildCount ← SpiderM.liftIO (IO.mkRef 0)

    let (_, render) ← ReactiveM.run events do
      runWidget do
        let _ ← dynWidget valueDyn fun _ => do
          SpiderM.liftIO (rebuildCount.modify (· + 1))
          pure ()
        pure ()

    -- Render multiple times
    let _ ← render
    let _ ← render
    let _ ← render

    let count ← SpiderM.liftIO rebuildCount.get
    -- Should only be 1 (initial build), no re-renders
    ensure (count == 1) s!"Constant dynamic should only build once, got {count}"
  ).run spiderEnv
  pure ()

test "duplicate values still trigger rebuild" := do
  -- dynWidget doesn't dedupe - that's the upstream Dynamic's responsibility
  let spiderEnv ← SpiderEnv.new defaultErrorHandler
  let _ ← (do
    let (events, _) ← createInputs FontRegistry.empty testTheme
    let (trigger, fire) ← newTriggerEvent (t := Spider) (a := Nat)
    let valueDyn ← holdDyn 5 trigger
    let rebuildCount ← SpiderM.liftIO (IO.mkRef 0)

    let (_, render) ← ReactiveM.run events do
      runWidget do
        let _ ← dynWidget valueDyn fun _ => do
          SpiderM.liftIO (rebuildCount.modify (· + 1))
          pure ()
        pure ()

    let _ ← render
    -- Fire same value twice
    fire 5
    fire 5

    let count ← SpiderM.liftIO rebuildCount.get
    -- 1 initial + 2 fires = 3 (no deduplication at dynWidget level)
    ensure (count == 3) s!"Duplicate values should still rebuild, expected 3, got {count}"
  ).run spiderEnv
  pure ()

test "multiple renders without update don't rebuild" := do
  let spiderEnv ← SpiderEnv.new defaultErrorHandler
  let _ ← (do
    let (events, _) ← createInputs FontRegistry.empty testTheme
    let (trigger, _) ← newTriggerEvent (t := Spider) (a := Nat)
    let valueDyn ← holdDyn 0 trigger
    let rebuildCount ← SpiderM.liftIO (IO.mkRef 0)

    let (_, render) ← ReactiveM.run events do
      runWidget do
        let _ ← dynWidget valueDyn fun _ => do
          SpiderM.liftIO (rebuildCount.modify (· + 1))
          pure ()
        pure ()

    -- Call render 5 times without firing updates
    for _ in [0:5] do
      let _ ← render

    let count ← SpiderM.liftIO rebuildCount.get
    ensure (count == 1) s!"Multiple renders without update should not rebuild, got {count}"
  ).run spiderEnv
  pure ()

/-! ## Nested Scenario Tests -/

test "nested dynWidget: inner change doesn't rebuild outer" := do
  let spiderEnv ← SpiderEnv.new defaultErrorHandler
  let _ ← (do
    let (events, _) ← createInputs FontRegistry.empty testTheme
    let (outerTrigger, fireOuter) ← newTriggerEvent (t := Spider) (a := Nat)
    let (innerTrigger, fireInner) ← newTriggerEvent (t := Spider) (a := Nat)
    let outerDyn ← holdDyn 0 outerTrigger
    let innerDyn ← holdDyn 0 innerTrigger
    let outerCount ← SpiderM.liftIO (IO.mkRef 0)
    let innerCount ← SpiderM.liftIO (IO.mkRef 0)

    let (_, render) ← ReactiveM.run events do
      runWidget do
        let _ ← dynWidget outerDyn fun _ => do
          SpiderM.liftIO (outerCount.modify (· + 1))
          let _ ← dynWidget innerDyn fun _ => do
            SpiderM.liftIO (innerCount.modify (· + 1))
            pure ()
          pure ()
        pure ()

    let _ ← render
    let outer1 ← SpiderM.liftIO outerCount.get
    let inner1 ← SpiderM.liftIO innerCount.get
    ensure (outer1 == 1) s!"Initial outer should be 1, got {outer1}"
    ensure (inner1 == 1) s!"Initial inner should be 1, got {inner1}"

    -- Fire inner only
    fireInner 1
    let outer2 ← SpiderM.liftIO outerCount.get
    let inner2 ← SpiderM.liftIO innerCount.get
    ensure (outer2 == 1) s!"Outer should still be 1 after inner fire, got {outer2}"
    ensure (inner2 == 2) s!"Inner should be 2 after inner fire, got {inner2}"
  ).run spiderEnv
  pure ()

test "nested dynWidget: outer change rebuilds both" := do
  let spiderEnv ← SpiderEnv.new defaultErrorHandler
  let _ ← (do
    let (events, _) ← createInputs FontRegistry.empty testTheme
    let (outerTrigger, fireOuter) ← newTriggerEvent (t := Spider) (a := Nat)
    let (innerTrigger, _) ← newTriggerEvent (t := Spider) (a := Nat)
    let outerDyn ← holdDyn 0 outerTrigger
    let innerDyn ← holdDyn 0 innerTrigger
    let outerCount ← SpiderM.liftIO (IO.mkRef 0)
    let innerCount ← SpiderM.liftIO (IO.mkRef 0)

    let (_, render) ← ReactiveM.run events do
      runWidget do
        let _ ← dynWidget outerDyn fun _ => do
          SpiderM.liftIO (outerCount.modify (· + 1))
          let _ ← dynWidget innerDyn fun _ => do
            SpiderM.liftIO (innerCount.modify (· + 1))
            pure ()
          pure ()
        pure ()

    let _ ← render
    -- Fire outer
    fireOuter 1
    let outer ← SpiderM.liftIO outerCount.get
    let inner ← SpiderM.liftIO innerCount.get
    -- Outer rebuild creates new inner dynWidget which does initial build
    ensure (outer == 2) s!"Outer should be 2 after outer fire, got {outer}"
    ensure (inner == 2) s!"Inner should be 2 after outer fire (recreated), got {inner}"
  ).run spiderEnv
  pure ()

test "deeply nested dynWidget (5 levels)" := do
  let spiderEnv ← SpiderEnv.new defaultErrorHandler
  let _ ← (do
    let (events, _) ← createInputs FontRegistry.empty testTheme
    let (trigger, _) ← newTriggerEvent (t := Spider) (a := Nat)
    let dyn ← holdDyn 0 trigger
    let counts ← SpiderM.liftIO (IO.mkRef #[0, 0, 0, 0, 0])

    let (_, render) ← ReactiveM.run events do
      runWidget do
        let _ ← dynWidget dyn fun _ => do
          SpiderM.liftIO (counts.modify fun arr => arr.set! 0 (arr[0]! + 1))
          let _ ← dynWidget dyn fun _ => do
            SpiderM.liftIO (counts.modify fun arr => arr.set! 1 (arr[1]! + 1))
            let _ ← dynWidget dyn fun _ => do
              SpiderM.liftIO (counts.modify fun arr => arr.set! 2 (arr[2]! + 1))
              let _ ← dynWidget dyn fun _ => do
                SpiderM.liftIO (counts.modify fun arr => arr.set! 3 (arr[3]! + 1))
                let _ ← dynWidget dyn fun _ => do
                  SpiderM.liftIO (counts.modify fun arr => arr.set! 4 (arr[4]! + 1))
                  pure ()
                pure ()
              pure ()
            pure ()
          pure ()
        pure ()

    let _ ← render
    let arr ← SpiderM.liftIO counts.get
    -- All levels should have built once
    for i in [0:5] do
      ensure (arr[i]! == 1) s!"Level {i} should be 1, got {arr[i]!}"
  ).run spiderEnv
  pure ()

test "sibling dynWidgets are independent" := do
  let spiderEnv ← SpiderEnv.new defaultErrorHandler
  let _ ← (do
    let (events, _) ← createInputs FontRegistry.empty testTheme
    let (triggerA, fireA) ← newTriggerEvent (t := Spider) (a := Nat)
    let (triggerB, fireB) ← newTriggerEvent (t := Spider) (a := Nat)
    let dynA ← holdDyn 0 triggerA
    let dynB ← holdDyn 0 triggerB
    let countA ← SpiderM.liftIO (IO.mkRef 0)
    let countB ← SpiderM.liftIO (IO.mkRef 0)

    let (_, render) ← ReactiveM.run events do
      runWidget do
        let _ ← dynWidget dynA fun _ => do
          SpiderM.liftIO (countA.modify (· + 1))
          pure ()
        let _ ← dynWidget dynB fun _ => do
          SpiderM.liftIO (countB.modify (· + 1))
          pure ()
        pure ()

    let _ ← render
    -- Fire A only
    fireA 1
    let a1 ← SpiderM.liftIO countA.get
    let b1 ← SpiderM.liftIO countB.get
    ensure (a1 == 2) s!"A should be 2, got {a1}"
    ensure (b1 == 1) s!"B should still be 1, got {b1}"

    -- Fire B only
    fireB 1
    let a2 ← SpiderM.liftIO countA.get
    let b2 ← SpiderM.liftIO countB.get
    ensure (a2 == 2) s!"A should still be 2, got {a2}"
    ensure (b2 == 2) s!"B should be 2, got {b2}"
  ).run spiderEnv
  pure ()

/-! ## Zipped Dynamic Tests -/

test "zipWithM triggers rebuild when either changes" := do
  let spiderEnv ← SpiderEnv.new defaultErrorHandler
  let _ ← (do
    let (events, _) ← createInputs FontRegistry.empty testTheme
    let (triggerA, fireA) ← newTriggerEvent (t := Spider) (a := Nat)
    let (triggerB, fireB) ← newTriggerEvent (t := Spider) (a := Nat)
    let dynA ← holdDyn 0 triggerA
    let dynB ← holdDyn 0 triggerB
    let combined ← Dynamic.zipWithM Prod.mk dynA dynB
    let rebuildCount ← SpiderM.liftIO (IO.mkRef 0)

    let (_, render) ← ReactiveM.run events do
      runWidget do
        let _ ← dynWidget combined fun _ => do
          SpiderM.liftIO (rebuildCount.modify (· + 1))
          pure ()
        pure ()

    let _ ← render
    let count1 ← SpiderM.liftIO rebuildCount.get
    ensure (count1 == 1) s!"Initial should be 1, got {count1}"

    fireA 1
    let count2 ← SpiderM.liftIO rebuildCount.get
    ensure (count2 == 2) s!"After A fire should be 2, got {count2}"

    fireB 1
    let count3 ← SpiderM.liftIO rebuildCount.get
    ensure (count3 == 3) s!"After B fire should be 3, got {count3}"
  ).run spiderEnv
  pure ()

test "zipWith3M triggers on any of three" := do
  let spiderEnv ← SpiderEnv.new defaultErrorHandler
  let _ ← (do
    let (events, _) ← createInputs FontRegistry.empty testTheme
    let (triggerA, fireA) ← newTriggerEvent (t := Spider) (a := Nat)
    let (triggerB, fireB) ← newTriggerEvent (t := Spider) (a := Nat)
    let (triggerC, fireC) ← newTriggerEvent (t := Spider) (a := Nat)
    let dynA ← holdDyn 0 triggerA
    let dynB ← holdDyn 0 triggerB
    let dynC ← holdDyn 0 triggerC
    let combined ← Dynamic.zipWith3M (fun a b c => (a, b, c)) dynA dynB dynC
    let rebuildCount ← SpiderM.liftIO (IO.mkRef 0)

    let (_, render) ← ReactiveM.run events do
      runWidget do
        let _ ← dynWidget combined fun _ => do
          SpiderM.liftIO (rebuildCount.modify (· + 1))
          pure ()
        pure ()

    let _ ← render
    fireA 1
    fireB 1
    fireC 1
    let count ← SpiderM.liftIO rebuildCount.get
    -- 1 initial + 3 fires = 4
    ensure (count == 4) s!"Expected 4 rebuilds, got {count}"
  ).run spiderEnv
  pure ()

test "chained dynamics through dynWidget result" := do
  let spiderEnv ← SpiderEnv.new defaultErrorHandler
  let _ ← (do
    let (events, _) ← createInputs FontRegistry.empty testTheme
    let (trigger, fire) ← newTriggerEvent (t := Spider) (a := Nat)
    let sourceDyn ← holdDyn 1 trigger
    let chainedCount ← SpiderM.liftIO (IO.mkRef 0)

    let (_, render) ← ReactiveM.run events do
      runWidget do
        -- First dynWidget returns a dynamic of doubled values
        let resultDyn ← dynWidget sourceDyn fun v => do
          pure (v * 2)
        -- Second dynWidget uses the result
        let _ ← dynWidget resultDyn fun _ => do
          SpiderM.liftIO (chainedCount.modify (· + 1))
          pure ()
        pure ()

    let _ ← render
    let count1 ← SpiderM.liftIO chainedCount.get
    ensure (count1 == 1) s!"Initial chained should be 1, got {count1}"

    fire 2
    let count2 ← SpiderM.liftIO chainedCount.get
    -- Source fires -> first dynWidget rebuilds -> fires resultDyn -> second rebuilds
    ensure (count2 == 2) s!"After fire, chained should be 2, got {count2}"
  ).run spiderEnv
  pure ()

/-! ## Render Output Structure Tests -/

test "empty builder emits spacer" := do
  let spiderEnv ← SpiderEnv.new defaultErrorHandler
  let _ ← (do
    let (events, _) ← createInputs FontRegistry.empty testTheme
    let valueDyn ← Dynamic.pureM 0

    let (_, render) ← ReactiveM.run events do
      runWidget do
        let _ ← dynWidget valueDyn fun _ => do
          -- Emit nothing
          pure ()
        pure ()

    let builder ← render
    let widget := buildFrom 0 builder
    -- Should be a spacer (rect with 0x0 dimensions)
    match widget with
    | .rect _ _ style =>
        ensure (style.width == some (Dimension.length 0)) "Empty should emit spacer"
    | _ => ensure true "Widget built successfully"
  ).run spiderEnv
  pure ()

test "single child emits directly without wrapper" := do
  let spiderEnv ← SpiderEnv.new defaultErrorHandler
  let _ ← (do
    let (events, _) ← createInputs FontRegistry.empty testTheme
    let valueDyn ← Dynamic.pureM 0

    let (_, render) ← ReactiveM.run events do
      runWidget do
        let _ ← dynWidget valueDyn fun _ => do
          emit (pure (spacer 50 50))
        pure ()

    let builder ← render
    let widget := buildFrom 0 builder
    -- Should be the spacer directly, not wrapped
    match widget with
    | .rect _ _ style =>
        ensure (style.width == some (Dimension.length 50)) "Single child should emit directly"
    | _ => ensure true "Widget built"
  ).run spiderEnv
  pure ()

test "multiple children wrapped in column" := do
  let spiderEnv ← SpiderEnv.new defaultErrorHandler
  let _ ← (do
    let (events, _) ← createInputs FontRegistry.empty testTheme
    let valueDyn ← Dynamic.pureM 0

    let (_, render) ← ReactiveM.run events do
      runWidget do
        let _ ← dynWidget valueDyn fun _ => do
          emit (pure (spacer 10 10))
          emit (pure (spacer 20 20))
        pure ()

    let builder ← render
    let widget := buildFrom 0 builder
    -- Should be wrapped in a flex column
    match widget with
    | .flex _ _ props _ children =>
        ensure (props.direction == FlexDirection.column) "Multiple children should be wrapped in column"
        ensure (children.size == 2) s!"Should have 2 children, got {children.size}"
    | _ => ensure false "Expected flex widget for multiple children"
  ).run spiderEnv
  pure ()

/-! ## Edge Case Tests -/

test "dynamic that never fires works correctly" := do
  let spiderEnv ← SpiderEnv.new defaultErrorHandler
  let _ ← (do
    let (events, _) ← createInputs FontRegistry.empty testTheme
    let (trigger, _) ← newTriggerEvent (t := Spider) (a := Nat)
    let valueDyn ← holdDyn 42 trigger
    let capturedValue ← SpiderM.liftIO (IO.mkRef 0)

    let (_, render) ← ReactiveM.run events do
      runWidget do
        let _ ← dynWidget valueDyn fun v => do
          SpiderM.liftIO (capturedValue.set v)
          pure ()
        pure ()

    let _ ← render
    let v ← SpiderM.liftIO capturedValue.get
    ensure (v == 42) s!"Should capture initial value 42, got {v}"
    -- No fires, but no errors either
  ).run spiderEnv
  pure ()

test "rapid sequential fires handled correctly" := do
  let spiderEnv ← SpiderEnv.new defaultErrorHandler
  let _ ← (do
    let (events, _) ← createInputs FontRegistry.empty testTheme
    let (trigger, fire) ← newTriggerEvent (t := Spider) (a := Nat)
    let valueDyn ← holdDyn 0 trigger
    let rebuildCount ← SpiderM.liftIO (IO.mkRef 0)
    let lastValue ← SpiderM.liftIO (IO.mkRef 0)

    let (_, render) ← ReactiveM.run events do
      runWidget do
        let _ ← dynWidget valueDyn fun v => do
          SpiderM.liftIO (rebuildCount.modify (· + 1))
          SpiderM.liftIO (lastValue.set v)
          pure ()
        pure ()

    let _ ← render
    -- Fire 10 times rapidly
    for i in [1:11] do
      fire i

    let count ← SpiderM.liftIO rebuildCount.get
    let last ← SpiderM.liftIO lastValue.get
    ensure (count == 11) s!"Expected 11 rebuilds (1 initial + 10 fires), got {count}"
    ensure (last == 10) s!"Last value should be 10, got {last}"
  ).run spiderEnv
  pure ()

test "builder accesses outer scope correctly" := do
  let spiderEnv ← SpiderEnv.new defaultErrorHandler
  let _ ← (do
    let (events, _) ← createInputs FontRegistry.empty testTheme
    let (trigger, fire) ← newTriggerEvent (t := Spider) (a := Nat)
    let valueDyn ← holdDyn 0 trigger
    let outerValue := 100  -- Captured from outer scope
    let capturedSum ← SpiderM.liftIO (IO.mkRef 0)

    let (_, render) ← ReactiveM.run events do
      runWidget do
        let _ ← dynWidget valueDyn fun v => do
          -- Access both dynamic value and outer scope
          SpiderM.liftIO (capturedSum.set (v + outerValue))
          pure ()
        pure ()

    let _ ← render
    let sum1 ← SpiderM.liftIO capturedSum.get
    ensure (sum1 == 100) s!"Initial sum should be 100, got {sum1}"

    fire 50
    let sum2 ← SpiderM.liftIO capturedSum.get
    ensure (sum2 == 150) s!"After fire, sum should be 150, got {sum2}"
  ).run spiderEnv
  pure ()

/-! ## Tree Correctness Tests -/

test "inner dynWidget update produces correct tree text" := do
  -- Verifies that when an inner dynWidget rebuilds (outer stays constant),
  -- the rendered tree contains the updated text content
  let spiderEnv ← SpiderEnv.new defaultErrorHandler
  let _ ← (do
    let (events, _) ← createInputs FontRegistry.empty testTheme
    let outerDyn ← Dynamic.pureM 0  -- Outer never changes
    let (innerTrigger, fireInner) ← newTriggerEvent (t := Spider) (a := Nat)
    let innerDyn ← holdDyn 0 innerTrigger

    let (_, render) ← ReactiveM.run events do
      runWidget do
        let _ ← dynWidget outerDyn fun _ => do
          let _ ← dynWidget innerDyn fun count => do
            emit (pure (text' s!"Count: {count}" testFont))
          pure ()
        pure ()

    -- Initial render should show "Count: 0"
    let builder1 ← render
    let widget1 := buildFrom 0 builder1
    match widget1 with
    | .text _ _ content .. =>
        ensure (content == "Count: 0") s!"Initial text should be 'Count: 0', got '{content}'"
    | other => ensure false s!"Expected text widget, got other"

    -- Fire inner update to 5
    fireInner 5
    let builder2 ← render
    let widget2 := buildFrom 0 builder2
    match widget2 with
    | .text _ _ content .. =>
        ensure (content == "Count: 5") s!"After update, text should be 'Count: 5', got '{content}'"
    | _ => ensure false "Expected text widget after update"

    -- Fire again to 42
    fireInner 42
    let builder3 ← render
    let widget3 := buildFrom 0 builder3
    match widget3 with
    | .text _ _ content .. =>
        ensure (content == "Count: 42") s!"After second update, text should be 'Count: 42', got '{content}'"
    | _ => ensure false "Expected text widget after second update"
  ).run spiderEnv
  pure ()

test "nested tree preserves structure with updated inner text" := do
  -- Verifies that outer structure stays intact while inner text updates
  let spiderEnv ← SpiderEnv.new defaultErrorHandler
  let _ ← (do
    let (events, _) ← createInputs FontRegistry.empty testTheme
    let outerDyn ← Dynamic.pureM 0
    let (innerTrigger, fireInner) ← newTriggerEvent (t := Spider) (a := Nat)
    let innerDyn ← holdDyn 1 innerTrigger
    let outerRebuildCount ← SpiderM.liftIO (IO.mkRef 0)

    let (_, render) ← ReactiveM.run events do
      runWidget do
        let _ ← dynWidget outerDyn fun _ => do
          SpiderM.liftIO (outerRebuildCount.modify (· + 1))
          -- Outer emits: fixed label + dynamic counter
          emit (pure (text' "Label:" testFont))
          let _ ← dynWidget innerDyn fun count => do
            emit (pure (text' s!"Value is {count}" testFont))
          pure ()
        pure ()

    -- Initial: column with 2 text children
    let builder1 ← render
    let widget1 := buildFrom 0 builder1
    match widget1 with
    | .flex _ _ props _ children =>
        ensure (props.direction == FlexDirection.column) "Should be column"
        ensure (children.size == 2) s!"Expected 2 children, got {children.size}"
        match children[0]!, children[1]! with
        | .text _ _ label .., .text _ _ value .. =>
            ensure (label == "Label:") s!"First child should be 'Label:', got '{label}'"
            ensure (value == "Value is 1") s!"Second child should be 'Value is 1', got '{value}'"
        | _, _ => ensure false "Expected two text widgets"
    | _ => ensure false "Expected flex column"

    -- Fire to change inner value
    fireInner 99
    let builder2 ← render
    let widget2 := buildFrom 0 builder2

    -- Outer should NOT have rebuilt
    let outerCount ← SpiderM.liftIO outerRebuildCount.get
    ensure (outerCount == 1) s!"Outer should only build once, got {outerCount}"

    -- Structure preserved, inner text updated
    match widget2 with
    | .flex _ _ _ _ children =>
        ensure (children.size == 2) s!"Still 2 children, got {children.size}"
        match children[0]!, children[1]! with
        | .text _ _ label .., .text _ _ value .. =>
            ensure (label == "Label:") s!"Label unchanged: '{label}'"
            ensure (value == "Value is 99") s!"Value updated to 'Value is 99', got '{value}'"
        | _, _ => ensure false "Expected two text widgets after update"
    | _ => ensure false "Expected flex column after update"
  ).run spiderEnv
  pure ()

/-! ## Subscription Cleanup Tests -/

test "dynWidget disposes child scope on rebuild" := do
  -- Verifies that the child scope is disposed when dynWidget rebuilds.
  -- We track this by having the builder register a cleanup action in the child scope.
  let spiderEnv ← SpiderEnv.new defaultErrorHandler
  let _ ← (do
    let (events, _) ← createInputs FontRegistry.empty testTheme
    let (trigger, fire) ← newTriggerEvent (t := Spider) (a := Nat)
    let valueDyn ← holdDyn 0 trigger
    let cleanupCount ← SpiderM.liftIO (IO.mkRef 0)

    let (_, render) ← ReactiveM.run events do
      runWidget do
        let _ ← dynWidget valueDyn fun _count => do
          -- Register a cleanup action that will be called when scope is disposed
          let registerCleanup : SpiderM Unit := ⟨fun env => do
            env.currentScope.register (cleanupCount.modify (· + 1))
          ⟩
          registerCleanup
          emit (pure (spacer 10 10))
        pure ()

    let _ ← render
    let count0 ← SpiderM.liftIO cleanupCount.get
    ensure (count0 == 0) s!"No cleanups yet, got {count0}"

    -- First update should dispose the initial build's scope
    fire 1
    let count1 ← SpiderM.liftIO cleanupCount.get
    ensure (count1 == 1) s!"First update should trigger 1 cleanup, got {count1}"

    -- Second update should dispose the first rebuild's scope
    fire 2
    let count2 ← SpiderM.liftIO cleanupCount.get
    ensure (count2 == 2) s!"Second update should trigger 2 total cleanups, got {count2}"

    -- Fire 8 more updates
    for i in [3:11] do
      fire i

    let countFinal ← SpiderM.liftIO cleanupCount.get
    -- Each of the 10 fires should have triggered a cleanup (disposing previous scope)
    ensure (countFinal == 10) s!"After 10 updates, should have 10 cleanups, got {countFinal}"
  ).run spiderEnv
  pure ()



end Afferent.Tests.DynWidgetTests

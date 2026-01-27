/-
  Afferent Command Coalescing Tests
  Unit tests for render command reordering and batching optimization.
-/
import Afferent.Tests.Framework
import Afferent.Widget.Backend
import Afferent.Arbor.Render.Command

namespace Afferent.Tests.CoalescingTests

open Crucible
open Afferent
open Afferent.Tests
open Afferent.Widget
open Afferent.Arbor

-- Helper to create test commands
def mkFillRect (x y w h : Float) : RenderCommand :=
  .fillRect ⟨⟨x, y⟩, ⟨w, h⟩⟩ ⟨1, 0, 0, 1⟩ 0

def mkFillRectColor (x y w h r g b : Float) : RenderCommand :=
  .fillRect ⟨⟨x, y⟩, ⟨w, h⟩⟩ ⟨r, g, b, 1⟩ 0

def mkStrokeRect (x y w h : Float) : RenderCommand :=
  .strokeRect ⟨⟨x, y⟩, ⟨w, h⟩⟩ ⟨0, 1, 0, 1⟩ 1.0 0

def testFont : FontId := { id := 0, name := "test", size := 14.0 }

def mkFillText (text : String) : RenderCommand :=
  .fillText text 0 0 testFont ⟨1, 1, 1, 1⟩

def mkFillPath : RenderCommand :=
  .fillPath Afferent.Path.empty ⟨0, 0, 1, 1⟩

def mkStrokePath : RenderCommand :=
  .strokePath Afferent.Path.empty ⟨1, 1, 0, 1⟩ 1.0

def mkFillPolygon : RenderCommand :=
  .fillPolygon #[⟨0, 0⟩, ⟨10, 0⟩, ⟨5, 10⟩] ⟨1, 0, 1, 1⟩

def mkStrokePolygon : RenderCommand :=
  .strokePolygon #[⟨0, 0⟩, ⟨10, 0⟩, ⟨5, 10⟩] ⟨0, 1, 1, 1⟩ 1.0

-- Helper to check command types
def isFillRect : RenderCommand → Bool
  | .fillRect .. => true
  | _ => false

def isStrokeRect : RenderCommand → Bool
  | .strokeRect .. => true
  | _ => false

def isFillText : RenderCommand → Bool
  | .fillText .. => true
  | _ => false

def isFillPath : RenderCommand → Bool
  | .fillPath .. => true
  | _ => false

def isStrokePath : RenderCommand → Bool
  | .strokePath .. => true
  | _ => false

def isSave : RenderCommand → Bool
  | .save => true
  | _ => false

def isRestore : RenderCommand → Bool
  | .restore => true
  | _ => false

def isPushClip : RenderCommand → Bool
  | .pushClip .. => true
  | _ => false

def isPopClip : RenderCommand → Bool
  | .popClip => true
  | _ => false

-- Count commands of each type
def countFillRects (cmds : Array RenderCommand) : Nat :=
  cmds.foldl (fun acc cmd => if isFillRect cmd then acc + 1 else acc) 0

def countStrokeRects (cmds : Array RenderCommand) : Nat :=
  cmds.foldl (fun acc cmd => if isStrokeRect cmd then acc + 1 else acc) 0

def countFillTexts (cmds : Array RenderCommand) : Nat :=
  cmds.foldl (fun acc cmd => if isFillText cmd then acc + 1 else acc) 0

def countFillPaths (cmds : Array RenderCommand) : Nat :=
  cmds.foldl (fun acc cmd => if isFillPath cmd then acc + 1 else acc) 0

def countStrokePaths (cmds : Array RenderCommand) : Nat :=
  cmds.foldl (fun acc cmd => if isStrokePath cmd then acc + 1 else acc) 0

-- Check if all fillRects come before all texts (within a command array)
def fillRectsBeforeTexts (cmds : Array RenderCommand) : Bool := Id.run do
  let mut seenText := false
  for cmd in cmds do
    if isFillText cmd then
      seenText := true
    else if isFillRect cmd && seenText then
      return false  -- Found a fillRect after a text
  return true

-- Check if all fillRects are consecutive starting at given index
def fillRectsConsecutiveFrom (cmds : Array RenderCommand) (startIdx : Nat) (count : Nat) : Bool := Id.run do
  for i in [startIdx : startIdx + count] do
    match cmds[i]? with
    | some cmd => if !isFillRect cmd then return false
    | none => return false
  return true

testSuite "Command Coalescing Tests"

/-! ## Basic Coalescing -/

test "empty array returns empty" := do
  let result := coalesceCommands #[]
  ensure (result.size == 0) "Expected empty array"

test "single fillRect unchanged" := do
  let cmds := #[mkFillRect 0 0 10 10]
  let result := coalesceCommands cmds
  ensure (result.size == 1) "Expected 1 command"
  ensure (countFillRects result == 1) "Expected 1 fillRect"

test "consecutive fillRects stay together" := do
  let cmds := #[mkFillRect 0 0 10 10, mkFillRect 20 20 10 10, mkFillRect 40 40 10 10]
  let result := coalesceCommands cmds
  ensure (result.size == 3) "Expected 3 commands"
  ensure (countFillRects result == 3) "Expected 3 fillRects"

/-! ## Command Reordering -/

test "fillRects coalesced when interleaved with text" := do
  -- Input: fillRect, fillText, fillRect
  -- Expected: fillRect, fillRect, fillText
  let cmds := #[mkFillRect 0 0 10 10, mkFillText "hello", mkFillRect 20 20 10 10]
  let result := coalesceCommands cmds
  ensure (result.size == 3) "Expected 3 commands"
  ensure (countFillRects result == 2) "Expected 2 fillRects"
  ensure (countFillTexts result == 1) "Expected 1 text"
  ensure (fillRectsBeforeTexts result) "fillRects should come before texts"

test "fillRects coalesced when interleaved with paths" := do
  -- Input: fillRect, fillPath, fillRect, strokePath, fillRect
  -- Expected: fillRect×3, fillPath, strokePath
  let cmds := #[mkFillRect 0 0 10 10, mkFillPath, mkFillRect 20 20 10 10, mkStrokePath, mkFillRect 40 40 10 10]
  let result := coalesceCommands cmds
  ensure (result.size == 5) "Expected 5 commands"
  ensure (countFillRects result == 3) "Expected 3 fillRects"
  ensure (fillRectsConsecutiveFrom result 0 3) "First 3 commands should be fillRects"

test "stroke rects grouped after fill rects" := do
  -- Input: strokeRect, fillRect, strokeRect, fillRect
  -- Expected: fillRect×2, strokeRect×2
  let cmds := #[mkStrokeRect 0 0 10 10, mkFillRect 20 20 10 10, mkStrokeRect 40 40 10 10, mkFillRect 60 60 10 10]
  let result := coalesceCommands cmds
  ensure (result.size == 4) "Expected 4 commands"
  ensure (countFillRects result == 2) "Expected 2 fillRects"
  ensure (countStrokeRects result == 2) "Expected 2 strokeRects"
  -- First two should be fillRects
  ensure (fillRectsConsecutiveFrom result 0 2) "First 2 commands should be fillRects"

test "text always comes last in scope" := do
  -- Input: fillText, fillRect, strokeRect, fillPath
  -- Expected: fillRect, strokeRect, fillPath, fillText
  let cmds := #[mkFillText "first", mkFillRect 0 0 10 10, mkStrokeRect 20 20 10 10, mkFillPath]
  let result := coalesceCommands cmds
  ensure (result.size == 4) "Expected 4 commands"
  ensure (fillRectsBeforeTexts result) "fillRects should come before texts"

/-! ## Scope Boundaries -/

test "save command creates scope boundary" := do
  -- fillRects in different scopes should NOT be coalesced together
  let cmds := #[mkFillRect 0 0 10 10, .save, mkFillRect 20 20 10 10]
  let result := coalesceCommands cmds
  ensure (result.size == 3) "Expected 3 commands"
  -- save should be in the middle
  match result[0]?, result[1]?, result[2]? with
  | some c0, some c1, some c2 =>
    ensure (isFillRect c0) "First should be fillRect"
    ensure (isSave c1) "Second should be save"
    ensure (isFillRect c2) "Third should be fillRect"
  | _, _, _ => ensure false "Expected 3 commands"

test "restore command creates scope boundary" := do
  let cmds := #[mkFillRect 0 0 10 10, .restore, mkFillRect 20 20 10 10]
  let result := coalesceCommands cmds
  ensure (result.size == 3) "Expected 3 commands"
  match result[1]? with
  | some c1 => ensure (isRestore c1) "Second should be restore"
  | none => ensure false "Missing command"

test "pushClip command creates scope boundary" := do
  let cmds := #[mkFillRect 0 0 10 10, .pushClip ⟨⟨0, 0⟩, ⟨100, 100⟩⟩, mkFillRect 20 20 10 10]
  let result := coalesceCommands cmds
  ensure (result.size == 3) "Expected 3 commands"
  match result[1]? with
  | some c1 => ensure (isPushClip c1) "Second should be pushClip"
  | none => ensure false "Missing command"

test "popClip command creates scope boundary" := do
  let cmds := #[mkFillRect 0 0 10 10, .popClip, mkFillRect 20 20 10 10]
  let result := coalesceCommands cmds
  ensure (result.size == 3) "Expected 3 commands"
  match result[1]? with
  | some c1 => ensure (isPopClip c1) "Second should be popClip"
  | none => ensure false "Missing command"

test "pushTranslate command creates scope boundary" := do
  let cmds := #[mkFillRect 0 0 10 10, .pushTranslate 10 20, mkFillRect 20 20 10 10]
  let result := coalesceCommands cmds
  ensure (result.size == 3) "Expected 3 commands"

test "popTransform command creates scope boundary" := do
  let cmds := #[mkFillRect 0 0 10 10, .popTransform, mkFillRect 20 20 10 10]
  let result := coalesceCommands cmds
  ensure (result.size == 3) "Expected 3 commands"

/-! ## Complex Scenarios -/

test "chart-like pattern: grid, shapes, text, axes" := do
  -- Simulates a typical chart: background, grid lines, data shapes, labels, axis lines
  let cmds := #[
    mkFillRect 0 0 400 300,          -- background
    mkFillRect 50 0 1 300,           -- grid line 1
    mkFillRect 100 0 1 300,          -- grid line 2
    mkFillPath,                       -- data area
    mkStrokePath,                     -- data line
    mkFillText "Label 1",            -- y-axis label
    mkFillText "Label 2",            -- y-axis label
    mkFillRect 0 299 400 1,          -- x-axis
    mkFillRect 0 0 1 300             -- y-axis
  ]
  let result := coalesceCommands cmds
  ensure (result.size == 9) "Expected 9 commands"
  -- All 5 fillRects should be first
  ensure (countFillRects result == 5) "Expected 5 fillRects"
  ensure (fillRectsConsecutiveFrom result 0 5) "First 5 commands should be fillRects"
  -- Then path operations (2)
  ensure (countFillPaths result == 1) "Expected 1 fillPath"
  ensure (countStrokePaths result == 1) "Expected 1 strokePath"
  -- Then text (2)
  ensure (countFillTexts result == 2) "Expected 2 texts"
  ensure (fillRectsBeforeTexts result) "fillRects should come before texts"

test "heatmap-like pattern: interleaved rects and text" := do
  -- Worst case: cell rect, cell value, cell rect, cell value...
  let cmds := #[
    mkFillRect 0 0 50 50,   mkFillText "1",
    mkFillRect 50 0 50 50,  mkFillText "2",
    mkFillRect 100 0 50 50, mkFillText "3",
    mkFillRect 0 50 50 50,  mkFillText "4"
  ]
  let result := coalesceCommands cmds
  ensure (result.size == 8) "Expected 8 commands"
  -- All 4 fillRects should be first
  ensure (countFillRects result == 4) "Expected 4 fillRects"
  ensure (fillRectsConsecutiveFrom result 0 4) "First 4 commands should be fillRects"
  -- Then all 4 texts
  ensure (countFillTexts result == 4) "Expected 4 texts"
  ensure (fillRectsBeforeTexts result) "fillRects should come before texts"

test "nested scopes preserve structure" := do
  -- save, fillRect, fillText, restore - text should come after rect within scope
  let cmds := #[.save, mkFillRect 0 0 10 10, mkFillText "inner", .restore, mkFillRect 20 20 10 10]
  let result := coalesceCommands cmds
  ensure (result.size == 5) "Expected 5 commands"
  match result[0]?, result[1]?, result[2]?, result[3]?, result[4]? with
  | some c0, some c1, some c2, some c3, some c4 =>
    ensure (isSave c0) "First should be save"
    ensure (isFillRect c1) "Second should be fillRect"
    ensure (isFillText c2) "Third should be fillText"
    ensure (isRestore c3) "Fourth should be restore"
    ensure (isFillRect c4) "Fifth should be fillRect"
  | _, _, _, _, _ => ensure false "Expected 5 commands"

/-! ## Order Preservation Within Type -/

test "fillRect order preserved within type" := do
  -- fillRects should maintain their relative order (by color)
  let cmds := #[
    mkFillRectColor 0 0 10 10 1 0 0,   -- red
    mkFillText "break",
    mkFillRectColor 20 20 10 10 0 1 0, -- green
    mkFillText "break2",
    mkFillRectColor 40 40 10 10 0 0 1  -- blue
  ]
  let result := coalesceCommands cmds
  ensure (result.size == 5) "Expected 5 commands"
  -- Check that the three fillRects come first and are in original order
  match result[0]?, result[1]?, result[2]? with
  | some cmd0, some cmd1, some cmd2 =>
    match cmd0, cmd1, cmd2 with
    | .fillRect _ c1 _, .fillRect _ c2 _, .fillRect _ c3 _ =>
      -- red first, then green, then blue
      shouldBeNear c1.r 1.0
      shouldBeNear c2.g 1.0
      shouldBeNear c3.b 1.0
    | _, _, _ => ensure false "Expected first 3 to be fillRects"
  | _, _, _ => ensure false "Expected at least 3 commands"

/-! ## isStateChanging Tests -/

test "isStateChanging identifies save" := do
  ensure (isStateChanging .save) "save should be state-changing"

test "isStateChanging identifies restore" := do
  ensure (isStateChanging .restore) "restore should be state-changing"

test "isStateChanging identifies pushClip" := do
  ensure (isStateChanging (.pushClip ⟨⟨0, 0⟩, ⟨10, 10⟩⟩)) "pushClip should be state-changing"

test "isStateChanging identifies popClip" := do
  ensure (isStateChanging .popClip) "popClip should be state-changing"

test "isStateChanging identifies pushTranslate" := do
  ensure (isStateChanging (.pushTranslate 10 20)) "pushTranslate should be state-changing"

test "isStateChanging identifies pushRotate" := do
  ensure (isStateChanging (.pushRotate 0.5)) "pushRotate should be state-changing"

test "isStateChanging identifies pushScale" := do
  ensure (isStateChanging (.pushScale 2 2)) "pushScale should be state-changing"

test "isStateChanging identifies popTransform" := do
  ensure (isStateChanging .popTransform) "popTransform should be state-changing"

test "isStateChanging returns false for fillRect" := do
  ensure (!isStateChanging (mkFillRect 0 0 10 10)) "fillRect should not be state-changing"

test "isStateChanging returns false for fillText" := do
  ensure (!isStateChanging (mkFillText "hello")) "fillText should not be state-changing"

test "isStateChanging returns false for fillPath" := do
  ensure (!isStateChanging mkFillPath) "fillPath should not be state-changing"

/-! ## Circle Coalescing Tests -/

def mkFillCircle (cx cy radius : Float) : RenderCommand :=
  .fillCircle ⟨cx, cy⟩ radius ⟨1, 0, 0, 1⟩

def mkStrokeCircle (cx cy radius : Float) : RenderCommand :=
  .strokeCircle ⟨cx, cy⟩ radius ⟨0, 1, 0, 1⟩ 1.0

def isFillCircle : RenderCommand → Bool
  | .fillCircle .. => true
  | _ => false

def isStrokeCircle : RenderCommand → Bool
  | .strokeCircle .. => true
  | _ => false

def countFillCircles (cmds : Array RenderCommand) : Nat :=
  cmds.foldl (fun acc cmd => if isFillCircle cmd then acc + 1 else acc) 0

def countStrokeCircles (cmds : Array RenderCommand) : Nat :=
  cmds.foldl (fun acc cmd => if isStrokeCircle cmd then acc + 1 else acc) 0

def fillCirclesConsecutiveFrom (cmds : Array RenderCommand) (startIdx : Nat) (count : Nat) : Bool := Id.run do
  for i in [startIdx : startIdx + count] do
    match cmds[i]? with
    | some cmd => if !isFillCircle cmd then return false
    | none => return false
  return true

test "fillCircles coalesced when interleaved with text" := do
  -- Input: fillCircle, fillText, fillCircle
  -- Expected: fillCircle, fillCircle, fillText (circles grouped first)
  let cmds := #[mkFillCircle 50 50 10, mkFillText "label", mkFillCircle 100 100 15]
  let result := coalesceCommands cmds
  ensure (result.size == 3) "Expected 3 commands"
  ensure (countFillCircles result == 2) "Expected 2 fillCircles"
  ensure (fillCirclesConsecutiveFrom result 0 2) "First 2 commands should be fillCircles"

test "fillCircles grouped with fillRects" := do
  -- Input: fillRect, fillCircle, fillRect, fillCircle
  -- Expected: fillRect×2, fillCircle×2 (rects before circles in coalescing order)
  let cmds := #[mkFillRect 0 0 10 10, mkFillCircle 50 50 10, mkFillRect 20 20 10 10, mkFillCircle 100 100 10]
  let result := coalesceCommands cmds
  ensure (result.size == 4) "Expected 4 commands"
  ensure (countFillRects result == 2) "Expected 2 fillRects"
  ensure (countFillCircles result == 2) "Expected 2 fillCircles"
  -- fillRects come first
  ensure (fillRectsConsecutiveFrom result 0 2) "First 2 commands should be fillRects"
  -- then circles
  ensure (fillCirclesConsecutiveFrom result 2 2) "Commands 2-3 should be fillCircles"

test "strokeCircles grouped after fillCircles" := do
  -- Input: strokeCircle, fillCircle, strokeCircle, fillCircle
  -- Expected: fillCircle×2, strokeCircle×2
  let cmds := #[mkStrokeCircle 0 0 10, mkFillCircle 50 50 10, mkStrokeCircle 100 100 10, mkFillCircle 150 150 10]
  let result := coalesceCommands cmds
  ensure (result.size == 4) "Expected 4 commands"
  ensure (countFillCircles result == 2) "Expected 2 fillCircles"
  ensure (countStrokeCircles result == 2) "Expected 2 strokeCircles"
  -- fillCircles first
  ensure (fillCirclesConsecutiveFrom result 0 2) "First 2 should be fillCircles"

/-! ## StrokeRect with Different Parameters Tests -/

def mkStrokeRectParams (x y w h lineWidth cornerRadius : Float) : RenderCommand :=
  .strokeRect ⟨⟨x, y⟩, ⟨w, h⟩⟩ ⟨0, 1, 0, 1⟩ lineWidth cornerRadius

def strokeRectsConsecutiveFrom (cmds : Array RenderCommand) (startIdx : Nat) (count : Nat) : Bool := Id.run do
  for i in [startIdx : startIdx + count] do
    match cmds[i]? with
    | some cmd => if !isStrokeRect cmd then return false
    | none => return false
  return true

test "strokeRects coalesced when interleaved with text" := do
  -- Input: strokeRect, fillText, strokeRect
  -- Expected: strokeRect×2, fillText
  let cmds := #[mkStrokeRect 0 0 10 10, mkFillText "label", mkStrokeRect 20 20 10 10]
  let result := coalesceCommands cmds
  ensure (result.size == 3) "Expected 3 commands"
  ensure (countStrokeRects result == 2) "Expected 2 strokeRects"
  ensure (strokeRectsConsecutiveFrom result 0 2) "First 2 should be strokeRects"

test "strokeRects with same params stay consecutive" := do
  -- All have same lineWidth=2.0 and cornerRadius=4.0
  let cmds := #[
    mkStrokeRectParams 0 0 10 10 2.0 4.0,
    mkFillText "break",
    mkStrokeRectParams 20 20 10 10 2.0 4.0,
    mkStrokeRectParams 40 40 10 10 2.0 4.0
  ]
  let result := coalesceCommands cmds
  ensure (result.size == 4) "Expected 4 commands"
  ensure (countStrokeRects result == 3) "Expected 3 strokeRects"
  -- All 3 strokeRects should be consecutive at start
  ensure (strokeRectsConsecutiveFrom result 0 3) "First 3 should be strokeRects"

/-! ## isStateChanging for New Commands -/

test "isStateChanging returns false for fillCircle" := do
  ensure (!isStateChanging (mkFillCircle 50 50 10)) "fillCircle should not be state-changing"

test "isStateChanging returns false for strokeCircle" := do
  ensure (!isStateChanging (mkStrokeCircle 50 50 10)) "strokeCircle should not be state-changing"

end Afferent.Tests.CoalescingTests

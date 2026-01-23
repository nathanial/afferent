/-
  Afferent CanvasState Tests
  Unit tests for transform composition and canvas state management.
-/
import Afferent.Tests.Framework
import Afferent.Core.Types
import Afferent.Core.Transform
import Afferent.Canvas.State
import Linalg

namespace Afferent.Tests.CanvasStateTests

open Crucible
open Afferent
open Afferent.Tests
open Linalg

testSuite "Canvas State Tests"

/-! ## Transform Identity Tests -/

test "identity transform leaves point unchanged" := do
  let t := Transform.identity
  let p := Point.mk' 42.0 17.5
  let result := t.apply p
  shouldBeNear result.x 42.0
  shouldBeNear result.y 17.5

test "identity composed with identity is identity" := do
  let t := Transform.identity * Transform.identity
  shouldBeNear t.a 1.0
  shouldBeNear t.b 0.0
  shouldBeNear t.c 0.0
  shouldBeNear t.d 1.0
  shouldBeNear t.tx 0.0
  shouldBeNear t.ty 0.0

/-! ## Transform Composition Tests -/

test "translate then translate composes correctly" := do
  let t1 := Transform.translate 10 20
  let t2 := Transform.translate 5 15
  let composed := t1 * t2
  let p := Point.mk' 0 0
  let result := composed.apply p
  shouldBeNear result.x 15.0
  shouldBeNear result.y 35.0

test "scale then translate applies in correct order" := do
  -- Scale first, then translate
  let t := Transform.identity.scaled 2.0 2.0 |>.translated 10.0 0.0
  let p := Point.mk' 5 5
  let result := t.apply p
  -- translated applies first (in local coords): (5,5) -> (15,5)
  -- then scale: (15,5) -> (30,10)
  shouldBeNear result.x 30.0
  shouldBeNear result.y 10.0

test "translate then scale applies in correct order" := do
  -- Translate first, then scale
  let t := Transform.identity.translated 10.0 0.0 |>.scaled 2.0 2.0
  let p := Point.mk' 5 5
  let result := t.apply p
  -- scale applies first: (5,5) -> (10,10)
  -- then translate: (10,10) -> (20,10)
  shouldBeNear result.x 20.0
  shouldBeNear result.y 10.0

test "rotate 90 degrees rotates point correctly" := do
  let t := Transform.rotate (Float.pi / 2.0)
  let p := Point.mk' 1 0
  let result := t.apply p
  shouldBeNear result.x 0.0
  shouldBeNear result.y 1.0

test "rotate 180 degrees flips point" := do
  let t := Transform.rotate Float.pi
  let p := Point.mk' 1 0
  let result := t.apply p
  shouldBeNear result.x (-1.0)
  shouldBeNear result.y 0.0

test "scale by 2 doubles coordinates" := do
  let t := Transform.scale 2.0 2.0
  let p := Point.mk' 5 10
  let result := t.apply p
  shouldBeNear result.x 10.0
  shouldBeNear result.y 20.0

test "non-uniform scale works correctly" := do
  let t := Transform.scale 3.0 0.5
  let p := Point.mk' 10 10
  let result := t.apply p
  shouldBeNear result.x 30.0
  shouldBeNear result.y 5.0

/-! ## Transform Inverse Tests -/

test "inverse of identity is identity" := do
  let t := Transform.identity
  let inv := t.inverse
  shouldBeNear inv.a 1.0
  shouldBeNear inv.d 1.0
  shouldBeNear inv.tx 0.0
  shouldBeNear inv.ty 0.0

test "inverse of translation is negative translation" := do
  let t := Transform.translate 10 20
  let inv := t.inverse
  let p := Point.mk' 0 0
  let result := inv.apply p
  shouldBeNear result.x (-10.0)
  shouldBeNear result.y (-20.0)

test "inverse of scale is reciprocal scale" := do
  let t := Transform.scale 2.0 4.0
  let inv := t.inverse
  let p := Point.mk' 10 20
  let result := inv.apply p
  shouldBeNear result.x 5.0
  shouldBeNear result.y 5.0

test "transform composed with inverse gives identity" := do
  let t := Transform.translate 10 20 * Transform.scale 2.0 3.0
  let inv := t.inverse
  let composed := t * inv
  let p := Point.mk' 42 17
  let result := composed.apply p
  shouldBeNear result.x 42.0
  shouldBeNear result.y 17.0

/-! ## CanvasState Tests -/

test "CanvasState default has identity transform" := do
  let state := CanvasState.default
  shouldBeNear state.transform.a 1.0
  shouldBeNear state.transform.d 1.0
  shouldBeNear state.transform.tx 0.0
  shouldBeNear state.transform.ty 0.0

test "CanvasState translate modifies transform" := do
  let state := CanvasState.default |> CanvasState.translate 50 100
  let p := Point.mk' 0 0
  let result := state.transformPoint p
  shouldBeNear result.x 50.0
  shouldBeNear result.y 100.0

test "CanvasState rotate modifies transform" := do
  let state := CanvasState.default |> CanvasState.rotate (Float.pi / 2.0)
  let p := Point.mk' 1 0
  let result := state.transformPoint p
  shouldBeNear result.x 0.0
  shouldBeNear result.y 1.0

test "CanvasState scale modifies transform" := do
  let state := CanvasState.default |> CanvasState.scale 2.0 3.0
  let p := Point.mk' 10 10
  let result := state.transformPoint p
  shouldBeNear result.x 20.0
  shouldBeNear result.y 30.0

test "CanvasState resetTransform returns to identity" := do
  let state := CanvasState.default
    |> CanvasState.translate 100 200
    |> CanvasState.scale 5 5
    |> CanvasState.resetTransform
  let p := Point.mk' 10 10
  let result := state.transformPoint p
  shouldBeNear result.x 10.0
  shouldBeNear result.y 10.0

/-! ## StateStack Tests -/

test "StateStack save and restore preserves state" := do
  let stack := StateStack.new
    |> StateStack.translate 100 200
    |> StateStack.save
    |> StateStack.translate 50 50
    |> StateStack.restore
  let p := Point.mk' 0 0
  let result := stack.state.transformPoint p
  shouldBeNear result.x 100.0
  shouldBeNear result.y 200.0

test "StateStack restore with empty stack is no-op" := do
  let stack := StateStack.new
    |> StateStack.translate 100 200
    |> StateStack.restore  -- Should be no-op since nothing was saved
  let p := Point.mk' 0 0
  let result := stack.state.transformPoint p
  shouldBeNear result.x 100.0
  shouldBeNear result.y 200.0

end Afferent.Tests.CanvasStateTests

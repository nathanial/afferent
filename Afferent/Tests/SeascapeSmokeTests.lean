/-
  Seascape Smoke Tests
  Optional integration tests that exercise the full Seascape render path.

  These are skipped by default because they require a working window/Metal device.
  Enable with: `AFFERENT_RUN_GPU_TESTS=1 ./test.sh`
-/
import Afferent.Tests.Framework
import Afferent
import Demos.Seascape

namespace Afferent.Tests

namespace SeascapeSmokeTests

private def gpuTestsEnabled : IO Bool := do
  match (← IO.getEnv "AFFERENT_RUN_GPU_TESTS") with
  | some "1" => pure true
  | some "true" => pure true
  | some "TRUE" => pure true
  | _ => pure false

private def gpuTestFrames : IO Nat := do
  match (← IO.getEnv "AFFERENT_GPU_TEST_FRAMES") with
  | some s =>
      match s.toNat? with
      | some n => pure (max 1 n)
      | none => pure 1
  | none => pure 1

private def cases : List TestCase :=
  [
    { name := "renderSeascape (smoke)"
      run := do
        if !(← gpuTestsEnabled) then
          return ()

        let w : UInt32 := 640
        let h : UInt32 := 360

        -- Skip (rather than fail) on headless/CI machines without a Metal device.
        let ctx? ← try
          pure (some (← Afferent.DrawContext.create w h "Afferent Seascape Smoke Test"))
        catch _ =>
          pure none

        match ctx? with
        | none => return ()
        | some ctx =>
            let frames ← gpuTestFrames
            for i in [:frames] do
              let ok ← ctx.renderer.beginFrame 0.0 0.0 0.0 1.0
              if ok then
                let t := (i.toFloat / 60.0)
                Demos.renderSeascape ctx.renderer t w.toFloat h.toFloat Demos.seascapeCamera
                ctx.renderer.endFrame

            ctx.destroy

    }
  ]

def runAllTests : IO UInt32 :=
  runTests "Seascape Smoke Tests" cases

end SeascapeSmokeTests

end Afferent.Tests

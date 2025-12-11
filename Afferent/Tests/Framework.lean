/-
  Afferent Test Framework
  Simple test harness for unit testing.
-/

namespace Afferent.Tests

/-- A test case with a name and a monadic test action. -/
structure TestCase where
  name : String
  run : IO Unit

/-- Check if two floats are approximately equal within epsilon. -/
def floatNear (a b : Float) (eps : Float := 0.0001) : Bool :=
  (a - b).abs < eps

/-- Assert that a condition is true. -/
def ensure (cond : Bool) (msg : String) : IO Unit := do
  if !cond then
    throw <| IO.userError s!"Assertion failed: {msg}"

/-- Assert that two values are equal. -/
def shouldBe [BEq α] [Repr α] (actual expected : α) : IO Unit := do
  if actual != expected then
    throw <| IO.userError s!"Expected {repr expected}, got {repr actual}"

/-- Assert that two floats are approximately equal. -/
def shouldBeNear (actual expected : Float) (eps : Float := 0.0001) : IO Unit := do
  if !floatNear actual expected eps then
    throw <| IO.userError s!"Expected {expected} (±{eps}), got {actual}"

/-- Assert that a condition is true with a message. -/
def shouldSatisfy (cond : Bool) (msg : String := "condition") : IO Unit := do
  if !cond then
    throw <| IO.userError s!"Expected {msg} to be true"

/-- Infix notation for shouldBe -/
scoped infix:50 " ≡ " => shouldBe

/-- Run a single test case. -/
def runTest (tc : TestCase) : IO Bool := do
  IO.print s!"  {tc.name}... "
  try
    tc.run
    IO.println "✓"
    return true
  catch e =>
    IO.println s!"✗\n    {e}"
    return false

/-- Run a list of test cases and report results. -/
def runTests (name : String) (cases : List TestCase) : IO UInt32 := do
  IO.println s!"\n{name}"
  IO.println ("─".intercalate (List.replicate name.length ""))
  let mut passed := 0
  let mut failed := 0
  for tc in cases do
    if ← runTest tc then
      passed := passed + 1
    else
      failed := failed + 1
  IO.println s!"\nResults: {passed} passed, {failed} failed"
  return if failed > 0 then 1 else 0

end Afferent.Tests

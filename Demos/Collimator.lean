/-
  Collimator Demo - Demonstrates collimator optics for data access
-/
import Collimator.Prelude

open Collimator
open scoped Collimator.Operators

namespace Demos

structure Person where
  name : String
  age : Nat
deriving Repr

def nameLens : Lens' Person String :=
  lens' (fun p => p.name) (fun p n => { p with name := n })

def ageLens : Lens' Person Nat :=
  lens' (fun p => p.age) (fun p a => { p with age := a })

def collimatorDemo : IO Unit := do
  IO.println "Collimator Optics Demo"
  IO.println "----------------------"

  let alice : Person := { name := "Alice", age := 30 }

  -- View through a lens
  IO.println s!"Name: {alice ^. nameLens}"
  IO.println s!"Age: {alice ^. ageLens}"

  -- Modify through a lens
  let older := alice & ageLens %~ (· + 1)
  IO.println s!"After birthday: {older ^. ageLens}"

  -- Set through a lens
  let renamed := alice & nameLens .~ "Alicia"
  IO.println s!"Renamed: {renamed ^. nameLens}"

  IO.println ""

end Demos

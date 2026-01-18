/-
  Afferent Shader Params
  Typeclass for types that can be passed as fragment parameters.
-/
import Afferent.Arbor.Core.Types

namespace Afferent.Shader

open Afferent.Arbor

/-- Typeclass for types that can be serialized to floats for GPU shader parameters. -/
class FragmentParams (α : Type) where
  /-- Convert the parameter struct to a flat array of floats. -/
  toFloats : α → Array Float
  /-- Number of floats in the serialized representation. -/
  floatCount : Nat

/-! ## Common Parameter Types -/

/-- Parameters for a helix spinner fragment. -/
structure HelixParams where
  center : Point
  size : Float
  time : Float
  color : Color
deriving Repr, Inhabited

instance : FragmentParams HelixParams where
  toFloats p := #[p.center.x, p.center.y, p.size, p.time, p.color.r, p.color.g, p.color.b, p.color.a]
  floatCount := 8

/-- Parameters for a simple circle fragment (position + color). -/
structure CircleParams where
  center : Point
  radius : Float
  color : Color
deriving Repr, Inhabited

instance : FragmentParams CircleParams where
  toFloats p := #[p.center.x, p.center.y, p.radius, p.color.r, p.color.g, p.color.b, p.color.a]
  floatCount := 7

/-- Parameters for an orbit spinner fragment. -/
structure OrbitParams where
  center : Point
  size : Float
  time : Float
  color : Color
deriving Repr, Inhabited

instance : FragmentParams OrbitParams where
  toFloats p := #[p.center.x, p.center.y, p.size, p.time, p.color.r, p.color.g, p.color.b, p.color.a]
  floatCount := 8

/-- Parameters for a pulse spinner fragment. -/
structure PulseParams where
  center : Point
  size : Float
  time : Float
  color : Color
  strokeWidth : Float
deriving Repr, Inhabited

instance : FragmentParams PulseParams where
  toFloats p := #[p.center.x, p.center.y, p.size, p.time, p.color.r, p.color.g, p.color.b, p.color.a, p.strokeWidth]
  floatCount := 9

/-- Parameters for bouncing dots spinner. -/
structure BouncingDotsParams where
  center : Point
  size : Float
  time : Float
  color : Color
deriving Repr, Inhabited

instance : FragmentParams BouncingDotsParams where
  toFloats p := #[p.center.x, p.center.y, p.size, p.time, p.color.r, p.color.g, p.color.b, p.color.a]
  floatCount := 8

/-- Parameters for wave dots spinner. -/
structure WaveParams where
  center : Point
  size : Float
  time : Float
  color : Color
deriving Repr, Inhabited

instance : FragmentParams WaveParams where
  toFloats p := #[p.center.x, p.center.y, p.size, p.time, p.color.r, p.color.g, p.color.b, p.color.a]
  floatCount := 8

/-- Parameters for circle dots spinner. -/
structure CircleDotsParams where
  center : Point
  size : Float
  time : Float
  color : Color
deriving Repr, Inhabited

instance : FragmentParams CircleDotsParams where
  toFloats p := #[p.center.x, p.center.y, p.size, p.time, p.color.r, p.color.g, p.color.b, p.color.a]
  floatCount := 8

/-- Parameters for ripple spinner. -/
structure RippleParams where
  center : Point
  size : Float
  time : Float
  color : Color
  strokeWidth : Float
deriving Repr, Inhabited

instance : FragmentParams RippleParams where
  toFloats p := #[p.center.x, p.center.y, p.size, p.time, p.color.r, p.color.g, p.color.b, p.color.a, p.strokeWidth]
  floatCount := 9

end Afferent.Shader

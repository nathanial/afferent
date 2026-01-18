/-
  Gears Spinner - Two interlocking gears rotating in opposite directions
-/
import Afferent.Canopy.Core
import Afferent.Canopy.Widget.Display.Spinner.Core

namespace Afferent.Canopy.Spinner

open Afferent.Arbor hiding Event
open Linalg

/-! ## Precomputed Gear Tessellation for Instanced Rendering -/

/-- Tessellate a canonical gear (centered at origin, outer radius = 1.0) as triangles.
    Returns (vertices, indices, centerX, centerY) for instanced rendering. -/
private def tessellateCanonicalGear (teeth : Nat) : Array Float × Array UInt32 × Float × Float := Id.run do
  let innerRadius : Float := 0.7   -- Inner radius as fraction of outer
  let toothDepth : Float := 0.3    -- Tooth extends outward by this fraction
  let outerRadius : Float := 1.0 + toothDepth  -- Total outer extent
  let numPoints := teeth * 4  -- 4 points per tooth
  let angleStep := Float.twoPi / numPoints.toFloat

  -- Generate perimeter points (at rotation=0)
  let mut perimeterPoints : Array (Float × Float) := Array.mkEmpty numPoints
  for i in [:numPoints] do
    let angle := i.toFloat * angleStep
    let posInTooth := i % 4
    let r := match posInTooth with
      | 0 => innerRadius           -- Valley
      | 1 => outerRadius           -- Outer
      | 2 => outerRadius           -- Outer
      | _ => innerRadius           -- Valley
    perimeterPoints := perimeterPoints.push (r * Float.cos angle, r * Float.sin angle)

  -- Vertices: center (0,0) + perimeter points
  -- Layout: [cx, cy, x0, y0, x1, y1, ...]
  let mut vertices : Array Float := Array.mkEmpty ((numPoints + 1) * 2)
  vertices := vertices.push 0.0 |>.push 0.0  -- Center vertex
  for (x, y) in perimeterPoints do
    vertices := vertices.push x |>.push y

  -- Indices: triangle fan from center
  -- Each triangle: center (0), point i, point (i+1) % numPoints
  let mut indices : Array UInt32 := Array.mkEmpty (numPoints * 3)
  for i in [:numPoints] do
    indices := indices.push 0  -- Center
    indices := indices.push (i + 1).toUInt32
    indices := indices.push ((i % numPoints) + 1 + 1).toUInt32
  -- Fix last triangle to wrap around
  indices := indices.set! (indices.size - 1) 1

  return (vertices, indices, 0.0, 0.0)

/-- Pre-computed 8-tooth gear tessellation. -/
private def gear8Tessellation : Array Float × Array UInt32 × Float × Float :=
  tessellateCanonicalGear 8

/-- Pre-computed 6-tooth gear tessellation. -/
private def gear6Tessellation : Array Float × Array UInt32 × Float × Float :=
  tessellateCanonicalGear 6

/-- Hash for 8-tooth gear (FNV-1a of "gear8"). -/
private def gear8Hash : UInt64 := 0x67656172385F5F5F  -- "gear8___"

/-- Hash for 6-tooth gear (FNV-1a of "gear6"). -/
private def gear6Hash : UInt64 := 0x67656172365F5F5F  -- "gear6___"

/-- Gears: Two interlocking gears rotating in opposite directions.
    Uses instanced rendering for high performance with many gear copies. -/
def gearsSpec (t : Float) (color : Color) (dims : Dimensions) : CustomSpec := {
  measure := fun _ _ => (dims.size, dims.size)
  collect := fun layout =>
    let rect := layout.contentRect
    let cx := rect.x + dims.size / 2
    let cy := rect.y + dims.size / 2

    -- Two gears offset horizontally
    let gear1X := cx - dims.size * 0.18
    let gear1Y := cy
    let gear2X := cx + dims.size * 0.18
    let gear2Y := cy
    let gear1Scale := dims.size * 0.22  -- Scale to desired size
    let gear2Scale := dims.size * 0.18
    let gear1Teeth : Nat := 8
    let gear2Teeth : Nat := 6

    -- Gears rotate opposite directions, synced by teeth ratio
    let gear1Angle := t * Float.twoPi
    let gear2Angle := -t * Float.twoPi * (gear1Teeth.toFloat / gear2Teeth.toFloat)

    -- Get pre-computed tessellations
    let (gear8Verts, gear8Indices, gear8CX, gear8CY) := gear8Tessellation
    let (gear6Verts, gear6Indices, gear6CX, gear6CY) := gear6Tessellation

    RenderM.build do
      -- Draw gear 1 (8-tooth) using instanced rendering
      let gear1Instance : MeshInstance := {
        x := gear1X, y := gear1Y
        rotation := gear1Angle
        scale := gear1Scale
        r := color.r, g := color.g, b := color.b, a := color.a
      }
      RenderM.fillPolygonInstanced gear8Hash gear8Verts gear8Indices #[gear1Instance] gear8CX gear8CY

      -- Draw gear 2 (6-tooth) using instanced rendering
      let gear2Color := color.withAlpha 0.85
      let gear2Instance : MeshInstance := {
        x := gear2X, y := gear2Y
        rotation := gear2Angle
        scale := gear2Scale
        r := gear2Color.r, g := gear2Color.g, b := gear2Color.b, a := gear2Color.a
      }
      RenderM.fillPolygonInstanced gear6Hash gear6Verts gear6Indices #[gear2Instance] gear6CX gear6CY

      -- Center holes (using GPU-batched circles)
      RenderM.fillCircle (Arbor.Point.mk' gear1X gear1Y) (gear1Scale * 0.25) (color.withAlpha 0.3)
      RenderM.fillCircle (Arbor.Point.mk' gear2X gear2Y) (gear2Scale * 0.25) (color.withAlpha 0.25)
  draw := none
}

end Afferent.Canopy.Spinner

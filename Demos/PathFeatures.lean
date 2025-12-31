/-
  PathFeatures Demo - Non-convex polygons, arcTo, and transformed arcs
  Showcases the high-priority code improvements from ROADMAP.md:
  1. Ear-clipping triangulation for non-convex polygons
  2. Proper arcTo implementation for rounded corners
  3. Arc transform handling with non-uniform scaling
-/
import Afferent

open Afferent CanvasM Linalg

namespace Demos

/-- Render path features demo content to canvas using CanvasM -/
def renderPathFeaturesM (screenScale : Float) (font : Font) : CanvasM Unit := do
  -- Section headers
  setFillColor Color.white
  fillTextXY "Non-Convex" (80 * screenScale) (80 * screenScale) font
  fillTextXY "arcTo Corners" (380 * screenScale) (80 * screenScale) font
  fillTextXY "Transformed Arcs" (680 * screenScale) (80 * screenScale) font

  -- ═══════════════════════════════════════════════════════════════════════════
  -- Section 1: Non-Convex Polygons (Ear-Clipping Triangulation)
  -- ═══════════════════════════════════════════════════════════════════════════

  -- Concave arrow shape
  let arrowX := 100 * screenScale
  let arrowY := 200 * screenScale
  let arrow := Path.empty
    |>.moveTo ⟨arrowX, arrowY - 50 * screenScale⟩
    |>.lineTo ⟨arrowX + 50 * screenScale, arrowY⟩
    |>.lineTo ⟨arrowX + 25 * screenScale, arrowY⟩
    |>.lineTo ⟨arrowX + 25 * screenScale, arrowY + 50 * screenScale⟩
    |>.lineTo ⟨arrowX - 25 * screenScale, arrowY + 50 * screenScale⟩
    |>.lineTo ⟨arrowX - 25 * screenScale, arrowY⟩
    |>.lineTo ⟨arrowX - 50 * screenScale, arrowY⟩
    |>.closePath
  setFillColor Color.blue
  fillPath arrow

  -- L-shaped polygon
  let lX := 220 * screenScale
  let lY := 140 * screenScale
  let lShape := Path.empty
    |>.moveTo ⟨lX, lY⟩
    |>.lineTo ⟨lX + 80 * screenScale, lY⟩
    |>.lineTo ⟨lX + 80 * screenScale, lY + 30 * screenScale⟩
    |>.lineTo ⟨lX + 30 * screenScale, lY + 30 * screenScale⟩
    |>.lineTo ⟨lX + 30 * screenScale, lY + 100 * screenScale⟩
    |>.lineTo ⟨lX, lY + 100 * screenScale⟩
    |>.closePath
  setFillColor Color.green
  fillPath lShape

  -- Concave star-like shape (5-pointed but concave)
  let starX := 100 * screenScale
  let starY := 380 * screenScale
  let innerR := 25 * screenScale
  let outerR := 55 * screenScale
  let mut starPath := Path.empty.moveTo ⟨starX, starY - outerR⟩
  for i in [1:10] do
    let angle := Float.pi / 5.0 * i.toFloat - Float.halfPi
    let r := if i % 2 == 0 then outerR else innerR
    starPath := starPath.lineTo ⟨starX + r * Float.cos angle, starY + r * Float.sin angle⟩
  starPath := starPath.closePath
  setFillColor Color.yellow
  fillPath starPath

  -- Chevron/V shape
  let chevX := 200 * screenScale
  let chevY := 320 * screenScale
  let chevron := Path.empty
    |>.moveTo ⟨chevX, chevY⟩
    |>.lineTo ⟨chevX + 40 * screenScale, chevY + 60 * screenScale⟩
    |>.lineTo ⟨chevX + 80 * screenScale, chevY⟩
    |>.lineTo ⟨chevX + 80 * screenScale, chevY + 30 * screenScale⟩
    |>.lineTo ⟨chevX + 40 * screenScale, chevY + 90 * screenScale⟩
    |>.lineTo ⟨chevX, chevY + 30 * screenScale⟩
    |>.closePath
  setFillColor Color.magenta
  fillPath chevron

  -- ═══════════════════════════════════════════════════════════════════════════
  -- Section 2: arcTo Rounded Corners
  -- ═══════════════════════════════════════════════════════════════════════════

  -- Rounded rectangle using arcTo
  let rrX := 380 * screenScale
  let rrY := 120 * screenScale
  let rrW := 140 * screenScale
  let rrH := 90 * screenScale
  let rrR := 18 * screenScale
  let roundedRect := Path.empty
    |>.moveTo ⟨rrX + rrR, rrY⟩
    |>.lineTo ⟨rrX + rrW - rrR, rrY⟩
    |>.arcTo ⟨rrX + rrW, rrY⟩ ⟨rrX + rrW, rrY + rrR⟩ rrR
    |>.lineTo ⟨rrX + rrW, rrY + rrH - rrR⟩
    |>.arcTo ⟨rrX + rrW, rrY + rrH⟩ ⟨rrX + rrW - rrR, rrY + rrH⟩ rrR
    |>.lineTo ⟨rrX + rrR, rrY + rrH⟩
    |>.arcTo ⟨rrX, rrY + rrH⟩ ⟨rrX, rrY + rrH - rrR⟩ rrR
    |>.lineTo ⟨rrX, rrY + rrR⟩
    |>.arcTo ⟨rrX, rrY⟩ ⟨rrX + rrR, rrY⟩ rrR
    |>.closePath
  setFillColor Color.cyan
  fillPath roundedRect

  -- Rounded triangle using arcTo
  let triX := 450 * screenScale
  let triY := 320 * screenScale
  let triR := 12 * screenScale
  let triSize := 70 * screenScale
  let p1 := Point.mk' triX (triY - triSize * 0.5)
  let p2 := Point.mk' (triX - triSize * 0.43) (triY + triSize * 0.25)
  let p3 := Point.mk' (triX + triSize * 0.43) (triY + triSize * 0.25)
  let roundedTri := Path.empty
    |>.moveTo ⟨(p1.x + p2.x) / 2, (p1.y + p2.y) / 2⟩
    |>.arcTo p1 p3 triR
    |>.arcTo p3 p2 triR
    |>.arcTo p2 p1 triR
    |>.closePath
  setFillColor Color.orange
  fillPath roundedTri

  -- Tab/pill shape with rounded ends
  let tabX := 380 * screenScale
  let tabY := 410 * screenScale
  let tabW := 100 * screenScale
  let tabH := 35 * screenScale
  let tabR := tabH / 2
  let tabShape := Path.empty
    |>.moveTo ⟨tabX + tabR, tabY⟩
    |>.lineTo ⟨tabX + tabW - tabR, tabY⟩
    |>.arcTo ⟨tabX + tabW, tabY⟩ ⟨tabX + tabW, tabY + tabH/2⟩ tabR
    |>.arcTo ⟨tabX + tabW, tabY + tabH⟩ ⟨tabX + tabW - tabR, tabY + tabH⟩ tabR
    |>.lineTo ⟨tabX + tabR, tabY + tabH⟩
    |>.arcTo ⟨tabX, tabY + tabH⟩ ⟨tabX, tabY + tabH/2⟩ tabR
    |>.arcTo ⟨tabX, tabY⟩ ⟨tabX + tabR, tabY⟩ tabR
    |>.closePath
  setFillColor Color.purple
  fillPath tabShape

  -- ═══════════════════════════════════════════════════════════════════════════
  -- Section 3: Transformed Arcs (Non-Uniform Scaling)
  -- ═══════════════════════════════════════════════════════════════════════════

  -- Circle under identity transform (reference)
  setFillColor (Color.gray 0.5)
  fillCircle ⟨700 * screenScale, 180 * screenScale⟩ (35 * screenScale)
  setFillColor Color.white
  fillTextXY "1:1" (685 * screenScale) (235 * screenScale) font

  -- Circle under 2:1 horizontal scale → becomes ellipse
  save
  translate (850 * screenScale) (180 * screenScale)
  scale 2.0 1.0
  setFillColor Color.red
  fillCircle ⟨0, 0⟩ (25 * screenScale)
  restore
  setFillColor Color.white
  fillTextXY "2:1" (825 * screenScale) (235 * screenScale) font

  -- Circle under 1:2 vertical scale → becomes tall ellipse
  save
  translate (700 * screenScale) (330 * screenScale)
  scale 1.0 2.0
  setFillColor Color.green
  fillCircle ⟨0, 0⟩ (25 * screenScale)
  restore
  setFillColor Color.white
  fillTextXY "1:2" (685 * screenScale) (410 * screenScale) font

  -- Pie slice under rotation
  save
  translate (830 * screenScale) (320 * screenScale)
  rotate (Float.pi / 6)
  setFillColor Color.orange
  fillPath (Path.pie ⟨0, 0⟩ (35 * screenScale) 0 Float.halfPi)
  restore
  setFillColor Color.white
  fillTextXY "30deg" (800 * screenScale) (380 * screenScale) font

  -- Arc under non-uniform scale + rotation
  save
  translate (770 * screenScale) (480 * screenScale)
  rotate (Float.pi / 4)
  scale 1.5 0.75
  setFillColor Color.cyan
  fillPath (Path.arcPath ⟨0, 0⟩ (35 * screenScale) 0 (Float.pi * 1.5) false)
  setStrokeColor Color.white
  setLineWidth (2 * screenScale)
  strokePath (Path.arcPath ⟨0, 0⟩ (35 * screenScale) 0 (Float.pi * 1.5) false)
  restore
  setFillColor Color.white
  fillTextXY "45+scale" (730 * screenScale) (550 * screenScale) font

end Demos

/-
  Demo Grid - Normal demo mode showing all demos in a 2x3 grid layout
  Uses Trellis CSS Grid for proper resize handling.
-/
import Afferent
import Afferent.Layout
import Demos.Shapes
import Demos.Transforms
import Demos.Strokes
import Demos.Gradients
import Demos.Text
import Demos.Animations

open Afferent CanvasM
open Trellis

namespace Demos

/-- Cell configuration: background color, label, scale factor, and render function -/
structure CellConfig where
  bg : Color
  label : String
  scale : Float
  render : Float → Fonts → CanvasM Unit

/-- Get cell configuration by index (0-5, left-to-right, top-to-bottom) -/
def getCellConfig (idx : Nat) : CellConfig :=
  match idx with
  | 0 => ⟨Color.hsva 0.667 0.25 0.20 1.0, "Shapes",     0.45, fun _ _ => renderShapesM⟩
  | 1 => ⟨Color.hsva 0.0   0.25 0.20 1.0, "Transforms", 0.60, fun _ _ => renderTransformsM⟩
  | 2 => ⟨Color.hsva 0.333 0.25 0.20 1.0, "Strokes",    0.51, fun _ _ => renderStrokesM⟩
  | 3 => ⟨Color.hsva 0.125 0.4  0.20 1.0, "Gradients",  0.51, fun _ _ => renderGradientsM⟩
  | 4 => ⟨Color.hsva 0.767 0.25 0.20 1.0, "Text",       0.51, fun _ fonts => renderTextM fonts⟩
  | _ => ⟨Color.hsva 0.75  0.25 0.20 1.0, "Animations", 0.45, fun t _ => renderAnimationsM t⟩

/-- Render the normal demo mode: 2x3 grid of demo cells using Trellis layout -/
def renderDemoGridM (screenScale : Float) (screenWidth screenHeight : Float)
    (fontSmall : Font) (fonts : Fonts) (t : Float) : CanvasM Unit := do
  -- Create 2x3 grid with cells that stretch to fill viewport
  -- 2 columns (1fr each), 3 rows (1fr each)
  let props := GridContainer.withTemplate
    #[.fr 1, .fr 1, .fr 1]  -- 3 rows, each 1fr
    #[.fr 1, .fr 1]          -- 2 columns, each 1fr
  let tree := LayoutNode.gridBox 0 props #[
    LayoutNode.leaf 1 ContentSize.zero,
    LayoutNode.leaf 2 ContentSize.zero,
    LayoutNode.leaf 3 ContentSize.zero,
    LayoutNode.leaf 4 ContentSize.zero,
    LayoutNode.leaf 5 ContentSize.zero,
    LayoutNode.leaf 6 ContentSize.zero
  ]

  -- Run layout algorithm with current screen dimensions
  let result := layout tree screenWidth screenHeight

  -- Debug: draw a marker to confirm we're rendering + show layout count
  setFillColor Color.red
  fillRectXYWH 0 0 50 50
  setFillColor Color.white
  fillTextXY s!"layouts: {result.layouts.size}, w={screenWidth}, h={screenHeight}" 60 30 fontSmall

  -- Render each cell based on layout results
  for cl in result.layouts do
    -- Only process leaf nodes (id 1-6), skip container (id 0)
    if cl.nodeId >= 1 && cl.nodeId <= 6 then
      let rect := cl.borderRect
      let cellIdx := cl.nodeId - 1  -- Convert node ID (1-6) to index (0-5)
      let config := getCellConfig cellIdx

      -- Create afferent rect for clipping
      let cellRect := Rect.mk' rect.x rect.y rect.width rect.height

      -- Clip to cell bounds
      clip cellRect

      -- Draw background
      setFillColor config.bg
      fillRect cellRect

      -- Draw label
      setFillColor (Color.hsva 0.0 0.0 1.0 0.5)
      fillTextXY s!"Cell: {cellIdx % 2},{cellIdx / 2} - {config.label}"
        (rect.x + 10 * screenScale) (rect.y + 20 * screenScale) fontSmall

      -- Render demo content (translated to cell origin, scaled)
      save
      translate rect.x rect.y
      scale (config.scale * screenScale) (config.scale * screenScale)
      config.render t fonts
      restore

      unclip

end Demos

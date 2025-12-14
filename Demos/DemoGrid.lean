/-
  Demo Grid - Normal demo mode showing all demos in a 2x3 grid layout
-/
import Afferent
import Demos.Shapes
import Demos.Transforms
import Demos.Strokes
import Demos.Gradients
import Demos.Text
import Demos.Animations

open Afferent CanvasM

namespace Demos

/-- Render the normal demo mode: 2x3 grid of demo cells -/
def renderDemoGridM (screenScale : Float) (cellWidth cellHeight : Float)
    (fontSmall : Font) (fonts : Fonts) (t : Float) : CanvasM Unit := do
  -- Background colors for each cell
  let bg00 := Color.hsva 0.667 0.25 0.20 1.0  -- Dark blue-gray
  let bg10 := Color.hsva 0.0 0.25 0.20 1.0    -- Dark red-gray
  let bg01 := Color.hsva 0.333 0.25 0.20 1.0  -- Dark green-gray
  let bg11 := Color.hsva 0.125 0.4 0.20 1.0   -- Dark warm gray
  let bg02 := Color.hsva 0.767 0.25 0.20 1.0  -- Dark purple-gray
  let bg12 := Color.hsva 0.75 0.25 0.20 1.0   -- Dark purple-gray

  -- Cell 0,0: Shapes demo (top-left)
  let cellRect00 := Rect.mk' 0 0 cellWidth cellHeight
  clip cellRect00
  setFillColor bg00
  fillRect cellRect00
  setFillColor (Color.hsva 0.0 0.0 1.0 0.5)
  fillTextXY "Cell: 0,0 - Shapes" (10 * screenScale) (20 * screenScale) fontSmall
  save
  scale (0.45 * screenScale) (0.45 * screenScale)
  renderShapesM
  restore
  unclip

  -- Cell 1,0: Transforms demo (top-right)
  let cellRect10 := Rect.mk' cellWidth 0 cellWidth cellHeight
  clip cellRect10
  setFillColor bg10
  fillRect cellRect10
  setFillColor (Color.hsva 0.0 0.0 1.0 0.5)
  fillTextXY "Cell: 1,0 - Transforms" (cellWidth + 10 * screenScale) (20 * screenScale) fontSmall
  save
  translate cellWidth 0
  scale (0.6 * screenScale) (0.6 * screenScale)
  renderTransformsM
  restore
  unclip

  -- Cell 0,1: Strokes demo (middle-left)
  let cellRect01 := Rect.mk' 0 cellHeight cellWidth cellHeight
  clip cellRect01
  setFillColor bg01
  fillRect cellRect01
  setFillColor (Color.hsva 0.0 0.0 1.0 0.5)
  fillTextXY "Cell: 0,1 - Strokes" (10 * screenScale) (cellHeight + 20 * screenScale) fontSmall
  save
  translate 0 cellHeight
  scale (0.51 * screenScale) (0.51 * screenScale)
  renderStrokesM
  restore
  unclip

  -- Cell 1,1: Gradients demo (middle-right)
  let cellRect11 := Rect.mk' cellWidth cellHeight cellWidth cellHeight
  clip cellRect11
  setFillColor bg11
  fillRect cellRect11
  setFillColor (Color.hsva 0.0 0.0 1.0 0.5)
  fillTextXY "Cell: 1,1 - Gradients" (cellWidth + 10 * screenScale) (cellHeight + 20 * screenScale) fontSmall
  save
  translate cellWidth cellHeight
  scale (0.51 * screenScale) (0.51 * screenScale)
  renderGradientsM
  restore
  unclip

  -- Cell 0,2: Text demo (bottom-left)
  let cellRect02 := Rect.mk' 0 (cellHeight * 2) cellWidth cellHeight
  clip cellRect02
  setFillColor bg02
  fillRect cellRect02
  setFillColor (Color.hsva 0.0 0.0 1.0 0.5)
  fillTextXY "Cell: 0,2 - Text" (10 * screenScale) (cellHeight * 2 + 20 * screenScale) fontSmall
  save
  translate 0 (cellHeight * 2)
  scale (0.51 * screenScale) (0.51 * screenScale)
  renderTextM fonts
  restore
  unclip

  -- Cell 1,2: Animations demo (bottom-right)
  let cellRect12 := Rect.mk' cellWidth (cellHeight * 2) cellWidth cellHeight
  clip cellRect12
  setFillColor bg12
  fillRect cellRect12
  setFillColor (Color.hsva 0.0 0.0 1.0 0.5)
  fillTextXY "Cell: 1,2 - Animations" (cellWidth + 10 * screenScale) (cellHeight * 2 + 20 * screenScale) fontSmall
  save
  translate cellWidth (cellHeight * 2)
  scale (0.45 * screenScale) (0.45 * screenScale)
  renderAnimationsM t
  restore
  unclip

end Demos

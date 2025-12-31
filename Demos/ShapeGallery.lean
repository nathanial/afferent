/-
  Shape Gallery Demo - Flip through labeled shapes one at a time
  Use left/right arrow keys to navigate between shapes.
  Includes both untransformed and transformed versions to test tessellation.
-/
import Afferent
import Linalg.Core

open Afferent CanvasM Linalg

namespace Demos

/-- Shape gallery entry: name and render function.
    Render function takes center point and scale, draws the shape. -/
structure ShapeEntry where
  name : String
  render : Point → Float → CanvasM Unit
deriving Inhabited

/-- Helper to create a simple shape entry from a path function -/
def simpleShape (name : String) (pathFn : Point → Float → Path) : ShapeEntry :=
  ⟨name, fun c s => do
    fillPath (pathFn c s)
    strokePath (pathFn c s)⟩

/-- Helper to create a transformed shape entry -/
def transformedShape (name : String) (pathFn : Point → Float → Path)
    (rotation : Float) (scaleX scaleY : Float) : ShapeEntry :=
  ⟨name, fun c s => do
    save
    translate c.x c.y
    rotate rotation
    scale scaleX scaleY
    let origin := Point.mk 0 0
    fillPath (pathFn origin s)
    strokePath (pathFn origin s)
    restore⟩

/-- All shapes available in the gallery -/
def shapeGalleryShapes : Array ShapeEntry := #[
  -- ═══════════════════════════════════════════════════════════════════
  -- BASIC SHAPES (untransformed)
  -- ═══════════════════════════════════════════════════════════════════
  simpleShape "Circle" (fun c s => Path.circle c (80 * s)),
  simpleShape "Ellipse" (fun c s => Path.ellipse c (100 * s) (60 * s)),
  simpleShape "Rectangle" (fun c s => Path.rectangleXYWH (c.x - 80 * s) (c.y - 50 * s) (160 * s) (100 * s)),
  simpleShape "Rounded Rectangle" (fun c s => Path.roundedRect (Rect.mk' (c.x - 80 * s) (c.y - 50 * s) (160 * s) (100 * s)) (20 * s)),

  -- ═══════════════════════════════════════════════════════════════════
  -- CURVED SHAPES (untransformed)
  -- ═══════════════════════════════════════════════════════════════════
  simpleShape "Heart" (fun c s => Path.heart c (100 * s)),
  simpleShape "Semicircle" (fun c s => Path.semicircle c (80 * s)),
  simpleShape "Pie (90 degrees)" (fun c s => Path.pie c (80 * s) 0 (Float.pi / 2)),
  simpleShape "Pie (270 degrees)" (fun c s => Path.pie c (80 * s) 0 (Float.pi * 1.5)),
  simpleShape "Arc (270 degrees)" (fun c s => Path.arcPath c (80 * s) 0 (Float.pi * 1.5) false |>.closePath),

  -- ═══════════════════════════════════════════════════════════════════
  -- POLYGONS (untransformed)
  -- ═══════════════════════════════════════════════════════════════════
  simpleShape "Equilateral Triangle" (fun c s => Path.equilateralTriangle c (80 * s)),
  simpleShape "Pentagon" (fun c s => Path.polygon c (80 * s) 5),
  simpleShape "Hexagon" (fun c s => Path.hexagon c (80 * s)),
  simpleShape "Octagon" (fun c s => Path.octagon c (80 * s)),

  -- ═══════════════════════════════════════════════════════════════════
  -- STARS (untransformed)
  -- ═══════════════════════════════════════════════════════════════════
  simpleShape "Star (5-pointed)" (fun c s => Path.star c (80 * s) (35 * s) 5),
  simpleShape "Star (8-pointed)" (fun c s => Path.star c (80 * s) (50 * s) 8),
  simpleShape "Star (12-pointed)" (fun c s => Path.star c (80 * s) (60 * s) 12),

  -- ═══════════════════════════════════════════════════════════════════
  -- NON-CONVEX SHAPES (untransformed)
  -- ═══════════════════════════════════════════════════════════════════
  simpleShape "L-Shape (non-convex)" (fun c s =>
    Path.empty
      |>.moveTo (Point.mk' (c.x - 60 * s) (c.y - 60 * s))
      |>.lineTo (Point.mk' (c.x + 60 * s) (c.y - 60 * s))
      |>.lineTo (Point.mk' (c.x + 60 * s) (c.y - 20 * s))
      |>.lineTo (Point.mk' (c.x - 20 * s) (c.y - 20 * s))
      |>.lineTo (Point.mk' (c.x - 20 * s) (c.y + 60 * s))
      |>.lineTo (Point.mk' (c.x - 60 * s) (c.y + 60 * s))
      |>.closePath),
  simpleShape "Arrow (non-convex)" (fun c s =>
    Path.empty
      |>.moveTo (Point.mk' c.x (c.y - 80 * s))
      |>.lineTo (Point.mk' (c.x + 60 * s) c.y)
      |>.lineTo (Point.mk' (c.x + 25 * s) c.y)
      |>.lineTo (Point.mk' (c.x + 25 * s) (c.y + 60 * s))
      |>.lineTo (Point.mk' (c.x - 25 * s) (c.y + 60 * s))
      |>.lineTo (Point.mk' (c.x - 25 * s) c.y)
      |>.lineTo (Point.mk' (c.x - 60 * s) c.y)
      |>.closePath),
  simpleShape "Chevron (non-convex)" (fun c s =>
    Path.empty
      |>.moveTo (Point.mk' (c.x - 60 * s) (c.y - 40 * s))
      |>.lineTo (Point.mk' c.x (c.y + 20 * s))
      |>.lineTo (Point.mk' (c.x + 60 * s) (c.y - 40 * s))
      |>.lineTo (Point.mk' (c.x + 60 * s) c.y)
      |>.lineTo (Point.mk' c.x (c.y + 60 * s))
      |>.lineTo (Point.mk' (c.x - 60 * s) c.y)
      |>.closePath),

  -- ═══════════════════════════════════════════════════════════════════
  -- ROTATED SHAPES (45 degrees)
  -- ═══════════════════════════════════════════════════════════════════
  transformedShape "Heart (rotated 45°)" (fun c s => Path.heart c (100 * s)) (Float.pi / 4) 1.0 1.0,
  transformedShape "Star (rotated 45°)" (fun c s => Path.star c (80 * s) (35 * s) 5) (Float.pi / 4) 1.0 1.0,
  transformedShape "Pie 270° (rotated 45°)" (fun c s => Path.pie c (80 * s) 0 (Float.pi * 1.5)) (Float.pi / 4) 1.0 1.0,
  transformedShape "Arrow (rotated 45°)" (fun c s =>
    Path.empty
      |>.moveTo (Point.mk' c.x (c.y - 80 * s))
      |>.lineTo (Point.mk' (c.x + 60 * s) c.y)
      |>.lineTo (Point.mk' (c.x + 25 * s) c.y)
      |>.lineTo (Point.mk' (c.x + 25 * s) (c.y + 60 * s))
      |>.lineTo (Point.mk' (c.x - 25 * s) (c.y + 60 * s))
      |>.lineTo (Point.mk' (c.x - 25 * s) c.y)
      |>.lineTo (Point.mk' (c.x - 60 * s) c.y)
      |>.closePath) (Float.pi / 4) 1.0 1.0,

  -- ═══════════════════════════════════════════════════════════════════
  -- NON-UNIFORM SCALED SHAPES
  -- ═══════════════════════════════════════════════════════════════════
  transformedShape "Circle (scaled 1.5x × 0.75x)" (fun c s => Path.circle c (80 * s)) 0.0 1.5 0.75,
  transformedShape "Heart (scaled 1.5x × 0.75x)" (fun c s => Path.heart c (100 * s)) 0.0 1.5 0.75,
  transformedShape "Star (scaled 0.5x × 1.5x)" (fun c s => Path.star c (80 * s) (35 * s) 5) 0.0 0.5 1.5,
  transformedShape "Hexagon (scaled 1.5x × 0.5x)" (fun c s => Path.hexagon c (80 * s)) 0.0 1.5 0.5,

  -- ═══════════════════════════════════════════════════════════════════
  -- ROTATED + SCALED (combined transforms)
  -- ═══════════════════════════════════════════════════════════════════
  transformedShape "Heart (45° + scale 1.5×0.75)" (fun c s => Path.heart c (100 * s)) (Float.pi / 4) 1.5 0.75,
  transformedShape "Star (30° + scale 1.2×0.8)" (fun c s => Path.star c (80 * s) (35 * s) 5) (Float.pi / 6) 1.2 0.8,
  transformedShape "Pie 270° (60° + scale 0.8×1.3)" (fun c s => Path.pie c (80 * s) 0 (Float.pi * 1.5)) (Float.pi / 3) 0.8 1.3,
  transformedShape "Arc 270° (45° + scale 1.5×0.75)" (fun c s => Path.arcPath c (80 * s) 0 (Float.pi * 1.5) false |>.closePath) (Float.pi / 4) 1.5 0.75,
  transformedShape "L-Shape (45° + scale 1.3×0.7)" (fun c s =>
    Path.empty
      |>.moveTo (Point.mk' (c.x - 60 * s) (c.y - 60 * s))
      |>.lineTo (Point.mk' (c.x + 60 * s) (c.y - 60 * s))
      |>.lineTo (Point.mk' (c.x + 60 * s) (c.y - 20 * s))
      |>.lineTo (Point.mk' (c.x - 20 * s) (c.y - 20 * s))
      |>.lineTo (Point.mk' (c.x - 20 * s) (c.y + 60 * s))
      |>.lineTo (Point.mk' (c.x - 60 * s) (c.y + 60 * s))
      |>.closePath) (Float.pi / 4) 1.3 0.7,

  -- ═══════════════════════════════════════════════════════════════════
  -- EXTREME TRANSFORMS (stress tests)
  -- ═══════════════════════════════════════════════════════════════════
  transformedShape "Circle (very flat 2x × 0.3x)" (fun c s => Path.circle c (80 * s)) 0.0 2.0 0.3,
  transformedShape "Heart (very tall 0.4x × 2x)" (fun c s => Path.heart c (80 * s)) 0.0 0.4 2.0,
  transformedShape "Star (90° + scale 2x × 0.5x)" (fun c s => Path.star c (80 * s) (35 * s) 5) (Float.pi / 2) 2.0 0.5
]

/-- Get the total number of shapes in the gallery -/
def shapeGalleryCount : Nat := shapeGalleryShapes.size

/-- Render a single shape with its label, centered on screen -/
def renderShapeGalleryM (idx : Nat) (screenW screenH : Float) (screenScale : Float)
    (fontLarge fontSmall : Font) : CanvasM Unit := do
  let total := shapeGalleryShapes.size
  let safeIdx := idx % total
  let shape := shapeGalleryShapes[safeIdx]!
  let center := Point.mk (screenW / 2) (screenH / 2)

  -- Draw the shape centered
  setFillColor Color.cyan
  setStrokeColor Color.white
  setLineWidth (2 * screenScale)
  shape.render center 1.5

  -- Shape name at top (centered manually)
  let (nameWidth, _) ← fontLarge.measureText shape.name
  setFillColor Color.white
  fillTextXY shape.name ((screenW - nameWidth) / 2) (80 * screenScale) fontLarge

  -- Index indicator
  let indexText := s!"{safeIdx + 1} / {total}"
  let (indexWidth, _) ← fontSmall.measureText indexText
  setFillColor Color.lightGray
  fillTextXY indexText ((screenW - indexWidth) / 2) (110 * screenScale) fontSmall

  -- Navigation instructions at bottom
  let navText := "Left/Right arrows to navigate"
  let (navWidth, _) ← fontSmall.measureText navText
  setFillColor (Color.gray 0.5)
  fillTextXY navText ((screenW - navWidth) / 2) (screenH - 40 * screenScale) fontSmall

end Demos

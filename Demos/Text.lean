/-
  Text Demo - Font rendering, sizes, colors
-/
import Afferent

open Afferent

namespace Demos

/-- Font bundle for text demo -/
structure Fonts where
  small : Font
  medium : Font
  large : Font
  huge : Font

/-- Render text demo content to canvas using CanvasM -/
def renderTextM (fonts : Fonts) : CanvasM Unit := do
  -- Row 1: Basic text in different sizes
  CanvasM.setFillColor Color.white
  CanvasM.fillTextXY "Small (16pt)" 50 50 fonts.small
  CanvasM.fillTextXY "Medium (24pt)" 50 90 fonts.medium
  CanvasM.fillTextXY "Large (36pt)" 50 140 fonts.large
  CanvasM.fillTextXY "Huge (48pt)" 50 200 fonts.huge

  -- Row 2: Text in different colors
  CanvasM.setFillColor Color.red
  CanvasM.fillTextXY "Red Text" 500 50 fonts.medium
  CanvasM.setFillColor Color.green
  CanvasM.fillTextXY "Green Text" 500 90 fonts.medium
  CanvasM.setFillColor Color.blue
  CanvasM.fillTextXY "Blue Text" 500 130 fonts.medium
  CanvasM.setFillColor Color.yellow
  CanvasM.fillTextXY "Yellow Text" 500 170 fonts.medium
  CanvasM.setFillColor Color.cyan
  CanvasM.fillTextXY "Cyan Text" 500 210 fonts.medium
  CanvasM.setFillColor Color.magenta
  CanvasM.fillTextXY "Magenta Text" 500 250 fonts.medium

  -- Row 3: Showcase text content
  CanvasM.setFillColor Color.white
  CanvasM.fillTextXY "Afferent - A Lean 4 2D Graphics Library" 50 300 fonts.large

  -- Row 4: Mixed content - text with shapes
  CanvasM.setFillColor Color.blue
  CanvasM.fillRect (Rect.mk' 50 350 150 40)
  CanvasM.setFillColor Color.white
  CanvasM.fillTextXY "Text on Shape" 60 380 fonts.small

  CanvasM.setFillColor Color.red
  CanvasM.fillCircle ⟨350, 370⟩ 30
  CanvasM.setFillColor Color.white
  CanvasM.fillTextXY "Labels" 320 420 fonts.small

  CanvasM.setFillColor Color.green
  CanvasM.fillRoundedRect (Rect.mk' 450 350 180 40) 10
  CanvasM.setFillColor Color.black
  CanvasM.fillTextXY "Rounded Button" 460 380 fonts.small

  -- Row 5: Character set sample
  CanvasM.setFillColor Color.white
  CanvasM.fillTextXY "ABCDEFGHIJKLMNOPQRSTUVWXYZ" 50 470 fonts.medium
  CanvasM.fillTextXY "abcdefghijklmnopqrstuvwxyz" 50 510 fonts.medium
  CanvasM.fillTextXY "0123456789 !@#$%^&*()_+-=" 50 550 fonts.medium

  -- Row 6: Semi-transparent text
  CanvasM.setFillColor (Color.rgba 1.0 1.0 1.0 0.7)
  CanvasM.fillTextXY "Semi-transparent" 50 600 fonts.medium
  CanvasM.setFillColor (Color.rgba 1.0 1.0 1.0 0.4)
  CanvasM.fillTextXY "More transparent" 300 600 fonts.medium
  CanvasM.setFillColor (Color.rgba 1.0 1.0 1.0 0.2)
  CanvasM.fillTextXY "Very faint" 550 600 fonts.medium

  -- Row 7: Colored backgrounds with text
  CanvasM.setFillColor (Color.rgba 0.8 0.2 0.2 1.0)
  CanvasM.fillRect (Rect.mk' 50 640 200 40)
  CanvasM.setFillColor Color.white
  CanvasM.fillTextXY "Error Message" 60 670 fonts.small

  CanvasM.setFillColor (Color.rgba 0.2 0.6 0.2 1.0)
  CanvasM.fillRect (Rect.mk' 280 640 200 40)
  CanvasM.setFillColor Color.white
  CanvasM.fillTextXY "Success!" 330 670 fonts.small

  CanvasM.setFillColor (Color.rgba 0.8 0.6 0.1 1.0)
  CanvasM.fillRect (Rect.mk' 510 640 200 40)
  CanvasM.setFillColor Color.black
  CanvasM.fillTextXY "Warning" 570 670 fonts.small

end Demos

/-
  Text Demo - Font rendering, sizes, colors
-/
import Afferent

open Afferent CanvasM

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
  setFillColor Color.white
  fillTextXY "Small (16pt)" 50 50 fonts.small
  fillTextXY "Medium (24pt)" 50 90 fonts.medium
  fillTextXY "Large (36pt)" 50 140 fonts.large
  fillTextXY "Huge (48pt)" 50 200 fonts.huge

  -- Row 2: Text in different colors
  setFillColor Color.red
  fillTextXY "Red Text" 500 50 fonts.medium
  setFillColor Color.green
  fillTextXY "Green Text" 500 90 fonts.medium
  setFillColor Color.blue
  fillTextXY "Blue Text" 500 130 fonts.medium
  setFillColor Color.yellow
  fillTextXY "Yellow Text" 500 170 fonts.medium
  setFillColor Color.cyan
  fillTextXY "Cyan Text" 500 210 fonts.medium
  setFillColor Color.magenta
  fillTextXY "Magenta Text" 500 250 fonts.medium

  -- Row 3: Showcase text content
  setFillColor Color.white
  fillTextXY "Afferent - A Lean 4 2D Graphics Library" 50 300 fonts.large

  -- Row 4: Mixed content - text with shapes
  setFillColor Color.blue
  fillRect (Rect.mk' 50 350 150 40)
  setFillColor Color.white
  fillTextXY "Text on Shape" 60 380 fonts.small

  setFillColor Color.red
  fillCircle ⟨350, 370⟩ 30
  setFillColor Color.white
  fillTextXY "Labels" 320 420 fonts.small

  setFillColor Color.green
  fillRoundedRect (Rect.mk' 450 350 180 40) 10
  setFillColor Color.black
  fillTextXY "Rounded Button" 460 380 fonts.small

  -- Row 5: Character set sample
  setFillColor Color.white
  fillTextXY "ABCDEFGHIJKLMNOPQRSTUVWXYZ" 50 470 fonts.medium
  fillTextXY "abcdefghijklmnopqrstuvwxyz" 50 510 fonts.medium
  fillTextXY "0123456789 !@#$%^&*()_+-=" 50 550 fonts.medium

  -- Row 6: Semi-transparent text
  setFillColor (Color.hsva 0.0 0.0 1.0 0.7)
  fillTextXY "Semi-transparent" 50 600 fonts.medium
  setFillColor (Color.hsva 0.0 0.0 1.0 0.4)
  fillTextXY "More transparent" 300 600 fonts.medium
  setFillColor (Color.hsva 0.0 0.0 1.0 0.2)
  fillTextXY "Very faint" 550 600 fonts.medium

  -- Row 7: Colored backgrounds with text
  setFillColor (Color.hsva 0.0 0.75 0.8 1.0)  -- red
  fillRect (Rect.mk' 50 640 200 40)
  setFillColor Color.white
  fillTextXY "Error Message" 60 670 fonts.small

  setFillColor (Color.hsva 0.333 0.667 0.6 1.0)  -- green
  fillRect (Rect.mk' 280 640 200 40)
  setFillColor Color.white
  fillTextXY "Success!" 330 670 fonts.small

  setFillColor (Color.hsva 0.119 0.875 0.8 1.0)  -- orange/warning
  fillRect (Rect.mk' 510 640 200 40)
  setFillColor Color.black
  fillTextXY "Warning" 570 670 fonts.small

end Demos

/-
  Gradients Demo - Linear and radial gradients
-/
import Afferent

open Afferent CanvasM

namespace Demos

/-- Render gradients demo content to canvas using CanvasM -/
def renderGradientsM : CanvasM Unit := do
  -- Row 1: Linear gradients - horizontal
  setFillLinearGradient ⟨50, 70⟩ ⟨200, 70⟩ gradient![Color.red, Color.yellow]
  fillRect (Rect.mk' 50 30 150 80)

  setFillLinearGradient ⟨230, 70⟩ ⟨380, 70⟩ gradient![Color.blue, Color.cyan]
  fillRect (Rect.mk' 230 30 150 80)

  setFillLinearGradient ⟨410, 70⟩ ⟨560, 70⟩ gradient![Color.green, Color.white]
  fillRect (Rect.mk' 410 30 150 80)

  -- Row 1: Vertical gradient
  setFillLinearGradient ⟨640, 30⟩ ⟨640, 110⟩ gradient![Color.purple, Color.orange]
  fillRect (Rect.mk' 590 30 100 80)

  -- Diagonal gradient
  setFillLinearGradient ⟨720, 30⟩ ⟨870, 110⟩ gradient![Color.magenta, Color.cyan]
  fillRect (Rect.mk' 720 30 150 80)

  -- Row 2: Multi-stop gradients (rainbow) - 7 evenly spaced colors
  setFillLinearGradient ⟨50, 180⟩ ⟨450, 180⟩
    gradient![Color.red, Color.orange, Color.yellow, Color.green, Color.blue, Color.purple, Color.magenta]
  fillRect (Rect.mk' 50 140 400 80)

  -- Sunset gradient
  let sunset : Array GradientStop := #[
    { position := 0.0, color := Color.hsva 0.667 0.667 0.3 1.0 },  -- dark blue
    { position := 0.3, color := Color.hsva 0.833 0.6 0.5 1.0 },    -- purple
    { position := 0.5, color := Color.hsva 0.024 0.778 0.9 1.0 },  -- orange-red
    { position := 0.7, color := Color.hsva 0.083 0.8 1.0 1.0 },    -- orange
    { position := 1.0, color := Color.hsva 0.139 0.6 1.0 1.0 }     -- light yellow
  ]
  setFillLinearGradient ⟨570, 140⟩ ⟨570, 220⟩ sunset
  fillRect (Rect.mk' 480 140 180 80)

  -- Grayscale
  setFillLinearGradient ⟨690, 180⟩ ⟨870, 180⟩ gradient![Color.black, Color.white]
  fillRect (Rect.mk' 690 140 180 80)

  -- Row 3: Radial gradients
  setFillRadialGradient ⟨120, 320⟩ 70 gradient![Color.white, Color.blue]
  fillCircle ⟨120, 320⟩ 70

  setFillRadialGradient ⟨280, 320⟩ 70 gradient![Color.yellow, Color.orange, Color.red]
  fillCircle ⟨280, 320⟩ 70

  let spotlight : Array GradientStop := #[
    { position := 0.0, color := Color.white },
    { position := 0.7, color := Color.hsva 0.0 0.0 1.0 0.3 },
    { position := 1.0, color := Color.hsva 0.0 0.0 1.0 0.0 }
  ]
  setFillRadialGradient ⟨440, 320⟩ 70 spotlight
  fillCircle ⟨440, 320⟩ 70

  setFillRadialGradient ⟨600, 320⟩ 70
    gradient![Color.hsva 0.333 0.5 1.0 1.0, Color.green, Color.hsva 0.333 1.0 0.3 1.0]
  fillCircle ⟨600, 320⟩ 70

  setFillRadialGradient ⟨760, 320⟩ 70 gradient![Color.cyan, Color.magenta]
  fillCircle ⟨760, 320⟩ 70

  -- Row 4: Gradients on different shapes
  setFillLinearGradient ⟨50, 420⟩ ⟨200, 520⟩ gradient![Color.red, Color.blue]
  fillRoundedRect (Rect.mk' 50 420 150 100) 20

  setFillRadialGradient ⟨330, 470⟩ 80 gradient![Color.yellow, Color.purple]
  fillEllipse ⟨330, 470⟩ 80 50

  setFillLinearGradient ⟨460, 410⟩ ⟨580, 530⟩ gradient![Color.yellow, Color.orange, Color.red]
  fillPath (Path.star ⟨520, 470⟩ 60 30 5)

  setFillRadialGradient ⟨700, 450⟩ 80
    gradient![Color.hsva 0.0 0.5 1.0 1.0, Color.red, Color.hsva 0.0 1.0 0.5 1.0]
  fillPath (Path.heart ⟨700, 470⟩ 70)

  -- Row 5: More gradient variations
  let stripes : Array GradientStop := #[
    { position := 0.0, color := Color.red },
    { position := 0.33, color := Color.red },
    { position := 0.34, color := Color.white },
    { position := 0.66, color := Color.white },
    { position := 0.67, color := Color.blue },
    { position := 1.0, color := Color.blue }
  ]
  setFillLinearGradient ⟨50, 610⟩ ⟨200, 610⟩ stripes
  fillRect (Rect.mk' 50 560 150 100)

  let chrome : Array GradientStop := #[
    { position := 0.0, color := Color.hsva 0.0 0.0 0.3 1.0 },
    { position := 0.2, color := Color.hsva 0.0 0.0 0.9 1.0 },
    { position := 0.4, color := Color.hsva 0.0 0.0 0.5 1.0 },
    { position := 0.6, color := Color.hsva 0.0 0.0 0.8 1.0 },
    { position := 0.8, color := Color.hsva 0.0 0.0 0.4 1.0 },
    { position := 1.0, color := Color.hsva 0.0 0.0 0.6 1.0 }
  ]
  setFillLinearGradient ⟨230, 560⟩ ⟨230, 660⟩ chrome
  fillRect (Rect.mk' 230 560 150 100)

  let gold : Array GradientStop := #[
    { position := 0.0, color := Color.hsva 0.1 0.833 0.6 1.0 },
    { position := 0.3, color := Color.hsva 0.125 0.6 1.0 1.0 },
    { position := 0.5, color := Color.hsva 0.111 0.75 0.8 1.0 },
    { position := 0.7, color := Color.hsva 0.133 0.5 1.0 1.0 },
    { position := 1.0, color := Color.hsva 0.104 0.8 0.5 1.0 }
  ]
  setFillLinearGradient ⟨410, 560⟩ ⟨410, 660⟩ gold
  fillRect (Rect.mk' 410 560 150 100)

  setFillRadialGradient ⟨655, 610⟩ 100 #[
    { position := 0.0, color := Color.hsva 0.5 1.0 1.0 1.0 },    -- cyan
    { position := 0.4, color := Color.hsva 0.583 1.0 1.0 0.8 },  -- azure
    { position := 1.0, color := Color.hsva 0.667 1.0 0.3 1.0 }   -- dark blue
  ]
  fillRect (Rect.mk' 590 560 130 100)

  setFillLinearGradient ⟨750, 660⟩ ⟨870, 560⟩
    gradient![Color.hsva 0.778 1.0 0.6 1.0, Color.hsva 0.944 0.6 1.0 1.0]  -- purple to pink
  fillRect (Rect.mk' 750 560 120 100)

end Demos

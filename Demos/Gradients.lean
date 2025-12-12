/-
  Gradients Demo - Linear and radial gradients
-/
import Afferent

open Afferent

namespace Demos

/-- Render gradients demo content to canvas using CanvasM -/
def renderGradientsM : CanvasM Unit := do
  -- Row 1: Linear gradients - horizontal
  let redYellow : Array GradientStop := #[
    { position := 0.0, color := Color.red },
    { position := 1.0, color := Color.yellow }
  ]
  CanvasM.setFillLinearGradient ⟨50, 70⟩ ⟨200, 70⟩ redYellow
  CanvasM.fillRect (Rect.mk' 50 30 150 80)

  let blueCyan : Array GradientStop := #[
    { position := 0.0, color := Color.blue },
    { position := 1.0, color := Color.cyan }
  ]
  CanvasM.setFillLinearGradient ⟨230, 70⟩ ⟨380, 70⟩ blueCyan
  CanvasM.fillRect (Rect.mk' 230 30 150 80)

  let greenWhite : Array GradientStop := #[
    { position := 0.0, color := Color.green },
    { position := 1.0, color := Color.white }
  ]
  CanvasM.setFillLinearGradient ⟨410, 70⟩ ⟨560, 70⟩ greenWhite
  CanvasM.fillRect (Rect.mk' 410 30 150 80)

  -- Row 1: Vertical gradient
  let purpleOrange : Array GradientStop := #[
    { position := 0.0, color := Color.purple },
    { position := 1.0, color := Color.orange }
  ]
  CanvasM.setFillLinearGradient ⟨640, 30⟩ ⟨640, 110⟩ purpleOrange
  CanvasM.fillRect (Rect.mk' 590 30 100 80)

  -- Diagonal gradient
  let magentaCyan : Array GradientStop := #[
    { position := 0.0, color := Color.magenta },
    { position := 1.0, color := Color.cyan }
  ]
  CanvasM.setFillLinearGradient ⟨720, 30⟩ ⟨870, 110⟩ magentaCyan
  CanvasM.fillRect (Rect.mk' 720 30 150 80)

  -- Row 2: Multi-stop gradients (rainbow)
  let rainbow : Array GradientStop := #[
    { position := 0.0, color := Color.red },
    { position := 0.17, color := Color.orange },
    { position := 0.33, color := Color.yellow },
    { position := 0.5, color := Color.green },
    { position := 0.67, color := Color.blue },
    { position := 0.83, color := Color.purple },
    { position := 1.0, color := Color.magenta }
  ]
  CanvasM.setFillLinearGradient ⟨50, 180⟩ ⟨450, 180⟩ rainbow
  CanvasM.fillRect (Rect.mk' 50 140 400 80)

  -- Sunset gradient
  let sunset : Array GradientStop := #[
    { position := 0.0, color := Color.rgba 0.1 0.1 0.3 1.0 },
    { position := 0.3, color := Color.rgba 0.5 0.2 0.5 1.0 },
    { position := 0.5, color := Color.rgba 0.9 0.3 0.2 1.0 },
    { position := 0.7, color := Color.rgba 1.0 0.6 0.2 1.0 },
    { position := 1.0, color := Color.rgba 1.0 0.9 0.4 1.0 }
  ]
  CanvasM.setFillLinearGradient ⟨570, 140⟩ ⟨570, 220⟩ sunset
  CanvasM.fillRect (Rect.mk' 480 140 180 80)

  -- Grayscale
  let grayscale : Array GradientStop := #[
    { position := 0.0, color := Color.black },
    { position := 1.0, color := Color.white }
  ]
  CanvasM.setFillLinearGradient ⟨690, 180⟩ ⟨870, 180⟩ grayscale
  CanvasM.fillRect (Rect.mk' 690 140 180 80)

  -- Row 3: Radial gradients
  let whiteBlue : Array GradientStop := #[
    { position := 0.0, color := Color.white },
    { position := 1.0, color := Color.blue }
  ]
  CanvasM.setFillRadialGradient ⟨120, 320⟩ 70 whiteBlue
  CanvasM.fillCircle ⟨120, 320⟩ 70

  let sunGlow : Array GradientStop := #[
    { position := 0.0, color := Color.yellow },
    { position := 0.5, color := Color.orange },
    { position := 1.0, color := Color.red }
  ]
  CanvasM.setFillRadialGradient ⟨280, 320⟩ 70 sunGlow
  CanvasM.fillCircle ⟨280, 320⟩ 70

  let spotlight : Array GradientStop := #[
    { position := 0.0, color := Color.white },
    { position := 0.7, color := Color.rgba 1.0 1.0 1.0 0.3 },
    { position := 1.0, color := Color.rgba 1.0 1.0 1.0 0.0 }
  ]
  CanvasM.setFillRadialGradient ⟨440, 320⟩ 70 spotlight
  CanvasM.fillCircle ⟨440, 320⟩ 70

  let greenGlow : Array GradientStop := #[
    { position := 0.0, color := Color.rgba 0.5 1.0 0.5 1.0 },
    { position := 0.5, color := Color.green },
    { position := 1.0, color := Color.rgba 0.0 0.3 0.0 1.0 }
  ]
  CanvasM.setFillRadialGradient ⟨600, 320⟩ 70 greenGlow
  CanvasM.fillCircle ⟨600, 320⟩ 70

  let cyanMagenta : Array GradientStop := #[
    { position := 0.0, color := Color.cyan },
    { position := 1.0, color := Color.magenta }
  ]
  CanvasM.setFillRadialGradient ⟨760, 320⟩ 70 cyanMagenta
  CanvasM.fillCircle ⟨760, 320⟩ 70

  -- Row 4: Gradients on different shapes
  CanvasM.setFillLinearGradient ⟨50, 420⟩ ⟨200, 520⟩ #[
    { position := 0.0, color := Color.red },
    { position := 1.0, color := Color.blue }
  ]
  CanvasM.fillRoundedRect (Rect.mk' 50 420 150 100) 20

  CanvasM.setFillRadialGradient ⟨330, 470⟩ 80 #[
    { position := 0.0, color := Color.yellow },
    { position := 1.0, color := Color.purple }
  ]
  CanvasM.fillEllipse ⟨330, 470⟩ 80 50

  CanvasM.setFillLinearGradient ⟨460, 410⟩ ⟨580, 530⟩ #[
    { position := 0.0, color := Color.yellow },
    { position := 0.5, color := Color.orange },
    { position := 1.0, color := Color.red }
  ]
  CanvasM.fillPath (Path.star ⟨520, 470⟩ 60 30 5)

  CanvasM.setFillRadialGradient ⟨700, 450⟩ 80 #[
    { position := 0.0, color := Color.rgba 1.0 0.5 0.5 1.0 },
    { position := 0.5, color := Color.red },
    { position := 1.0, color := Color.rgba 0.5 0.0 0.0 1.0 }
  ]
  CanvasM.fillPath (Path.heart ⟨700, 470⟩ 70)

  -- Row 5: More gradient variations
  let stripes : Array GradientStop := #[
    { position := 0.0, color := Color.red },
    { position := 0.33, color := Color.red },
    { position := 0.34, color := Color.white },
    { position := 0.66, color := Color.white },
    { position := 0.67, color := Color.blue },
    { position := 1.0, color := Color.blue }
  ]
  CanvasM.setFillLinearGradient ⟨50, 610⟩ ⟨200, 610⟩ stripes
  CanvasM.fillRect (Rect.mk' 50 560 150 100)

  let chrome : Array GradientStop := #[
    { position := 0.0, color := Color.rgba 0.3 0.3 0.3 1.0 },
    { position := 0.2, color := Color.rgba 0.9 0.9 0.9 1.0 },
    { position := 0.4, color := Color.rgba 0.5 0.5 0.5 1.0 },
    { position := 0.6, color := Color.rgba 0.8 0.8 0.8 1.0 },
    { position := 0.8, color := Color.rgba 0.4 0.4 0.4 1.0 },
    { position := 1.0, color := Color.rgba 0.6 0.6 0.6 1.0 }
  ]
  CanvasM.setFillLinearGradient ⟨230, 560⟩ ⟨230, 660⟩ chrome
  CanvasM.fillRect (Rect.mk' 230 560 150 100)

  let gold : Array GradientStop := #[
    { position := 0.0, color := Color.rgba 0.6 0.4 0.1 1.0 },
    { position := 0.3, color := Color.rgba 1.0 0.85 0.4 1.0 },
    { position := 0.5, color := Color.rgba 0.8 0.6 0.2 1.0 },
    { position := 0.7, color := Color.rgba 1.0 0.9 0.5 1.0 },
    { position := 1.0, color := Color.rgba 0.5 0.35 0.1 1.0 }
  ]
  CanvasM.setFillLinearGradient ⟨410, 560⟩ ⟨410, 660⟩ gold
  CanvasM.fillRect (Rect.mk' 410 560 150 100)

  CanvasM.setFillRadialGradient ⟨655, 610⟩ 100 #[
    { position := 0.0, color := Color.rgba 0.0 1.0 1.0 1.0 },
    { position := 0.4, color := Color.rgba 0.0 0.5 1.0 0.8 },
    { position := 1.0, color := Color.rgba 0.0 0.0 0.3 1.0 }
  ]
  CanvasM.fillRect (Rect.mk' 590 560 130 100)

  let purplePink : Array GradientStop := #[
    { position := 0.0, color := Color.rgba 0.4 0.0 0.6 1.0 },
    { position := 1.0, color := Color.rgba 1.0 0.4 0.6 1.0 }
  ]
  CanvasM.setFillLinearGradient ⟨750, 660⟩ ⟨870, 560⟩ purplePink
  CanvasM.fillRect (Rect.mk' 750 560 120 100)

end Demos

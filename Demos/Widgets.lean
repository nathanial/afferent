/-
  Widget Demo - Showcasing the widget system using Arbor
-/
import Afferent
import Afferent.Widget
import Arbor
import Trellis

open Afferent CanvasM
open Arbor
open Trellis

namespace Demos

/-- Build a demo widget showcasing text, boxes, and layout. -/
def widgetDemo (fontId : FontId) (smallFontId : FontId) (screenScale : Float := 1.0) : Widget := build do
  let s := fun (v : Float) => v * screenScale
  column (gap := s 20) (style := { backgroundColor := some (Color.gray 0.15), padding := EdgeInsets.uniform (s 30) }) #[
    -- Title
    text' "Widget System Demo" fontId Color.white .center,

    -- Row of colored boxes
    row (gap := s 12) {} #[
      coloredBox Color.red (s 80) (s 80),
      coloredBox Color.green (s 80) (s 80),
      coloredBox Color.blue (s 80) (s 80),
      coloredBox Color.yellow (s 80) (s 80),
      coloredBox Color.cyan (s 80) (s 80)
    ],

    -- Nested layout demo
    row (gap := s 16) {} #[
      card (Color.hsv 0.0 0.6 0.3) (s 12) do  -- dark red
        column (gap := s 8) {} #[
          text' "Card 1" smallFontId Color.white .center,
          coloredBox (Color.hsv 0.0 0.5 0.7) (s 60) (s 40)
        ],
      card (Color.hsv 0.33 0.6 0.3) (s 12) do  -- dark green
        column (gap := s 8) {} #[
          text' "Card 2" smallFontId Color.white .center,
          coloredBox (Color.hsv 0.33 0.5 0.7) (s 60) (s 40)
        ],
      card (Color.hsv 0.66 0.6 0.3) (s 12) do  -- dark blue
        column (gap := s 8) {} #[
          text' "Card 3" smallFontId Color.white .center,
          coloredBox (Color.hsv 0.66 0.5 0.7) (s 60) (s 40)
        ]
    ],

    -- Grid demo
    text' "Grid Layout (3 columns)" smallFontId Color.white .left,
    grid 3 (gap := s 8) {} #[
      coloredBox (Color.hsv 0.0 0.6 0.8) 0 (s 50),
      coloredBox (Color.hsv 0.1 0.6 0.8) 0 (s 50),
      coloredBox (Color.hsv 0.2 0.6 0.8) 0 (s 50),
      coloredBox (Color.hsv 0.3 0.6 0.8) 0 (s 50),
      coloredBox (Color.hsv 0.4 0.6 0.8) 0 (s 50),
      coloredBox (Color.hsv 0.5 0.6 0.8) 0 (s 50)
    ],

    -- Alignment demo
    text' "Alignment Demo" smallFontId Color.white .left,
    row (gap := s 16) {} #[
      box { backgroundColor := some (Color.gray 0.3), minWidth := some (s 100), minHeight := some (s 60) },
      center (style := { backgroundColor := some (Color.gray 0.3), minWidth := some (s 100), minHeight := some (s 60) }) do
        text' "Centered" smallFontId Color.white .center,
      box { backgroundColor := some (Color.gray 0.3), minWidth := some (s 100), minHeight := some (s 60) }
    ]
  ]

/-- Simple scroll demo widget. -/
def scrollDemoWidget (fontId : FontId) (scrollY : Float) (screenScale : Float := 1.0) : Widget := build do
  let s := fun (v : Float) => v * screenScale
  column (gap := s 16) (style := { backgroundColor := some (Color.gray 0.15), padding := EdgeInsets.uniform (s 20) }) #[
    text' "Scroll Container Demo" fontId Color.white .center,

    -- Scroll container with many items
    scroll (style := { backgroundColor := some (Color.gray 0.25), minWidth := some (s 300), minHeight := some (s 200) })
           (s 300) (s 500)  -- content size: 300x500
           { offsetY := scrollY } do
      column (gap := s 8) (style := { padding := EdgeInsets.uniform (s 10) }) #[
        text' "Item 1" fontId Color.white .left,
        coloredBox Color.red (s 280) (s 40),
        text' "Item 2" fontId Color.white .left,
        coloredBox Color.green (s 280) (s 40),
        text' "Item 3" fontId Color.white .left,
        coloredBox Color.blue (s 280) (s 40),
        text' "Item 4" fontId Color.white .left,
        coloredBox Color.yellow (s 280) (s 40),
        text' "Item 5" fontId Color.white .left,
        coloredBox Color.cyan (s 280) (s 40),
        text' "Item 6" fontId Color.white .left,
        coloredBox Color.magenta (s 280) (s 40),
        text' "Item 7" fontId Color.white .left,
        coloredBox Color.orange (s 280) (s 40)
      ]
  ]

/-- Render the widget demo using Arbor (fills available space). -/
def renderWidgetsM (reg : FontRegistry) (fontId : FontId) (smallFontId : FontId)
    (width height : Float) (screenScale : Float := 1.0) : CanvasM Unit := do
  let widget := widgetDemo fontId smallFontId screenScale
  Afferent.Widget.renderArborWidget reg widget width height

/-- Render the widget demo centered on screen. -/
def renderWidgetShapesM (reg : FontRegistry) (fontId : FontId) (smallFontId : FontId)
    (width height : Float) (screenScale : Float := 1.0) : CanvasM Unit := do
  let widget := widgetDemo fontId smallFontId screenScale
  Afferent.Widget.renderArborWidgetCentered reg widget width height

/-- Render the widget demo centered with debug borders around layout cells. -/
def renderWidgetShapesDebugM (reg : FontRegistry) (fontId : FontId) (smallFontId : FontId)
    (width height : Float) (screenScale : Float := 1.0) : CanvasM Unit := do
  let widget := widgetDemo fontId smallFontId screenScale
  Afferent.Widget.renderArborWidgetDebug reg widget width height

end Demos

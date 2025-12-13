/-
  Widget Demo - Showcasing the widget system
-/
import Afferent
import Afferent.Widget

open Afferent CanvasM
open Afferent.Widget

namespace Demos

/-- Build a demo widget showcasing text, boxes, and layout. -/
def widgetDemo (font : Font) (smallFont : Font) : Widget := build do
  column (gap := 20) (style := { backgroundColor := some (Color.gray 0.15), padding := Layout.EdgeInsets.uniform 30 }) #[
    -- Title
    text' "Widget System Demo" font Color.white .center,

    -- Row of colored boxes
    row (gap := 12) {} #[
      coloredBox Color.red 80 80,
      coloredBox Color.green 80 80,
      coloredBox Color.blue 80 80,
      coloredBox Color.yellow 80 80,
      coloredBox Color.cyan 80 80
    ],

    -- Wrapped text demo
    card (Color.gray 0.25) 16 do
      column (gap := 8) {} #[
        text' "Text Wrapping Demo" smallFont Color.white .left,
        wrappedText "This is a longer piece of text that demonstrates automatic line wrapping. When text exceeds the available width, it wraps to the next line automatically." smallFont 400 (Color.gray 0.8) .left
      ],

    -- Nested layout demo
    row (gap := 16) {} #[
      card (Color.hsv 0.0 0.6 0.3) 12 do  -- dark red
        column (gap := 8) {} #[
          text' "Card 1" smallFont Color.white .center,
          coloredBox (Color.hsv 0.0 0.5 0.7) 60 40
        ],
      card (Color.hsv 0.33 0.6 0.3) 12 do  -- dark green
        column (gap := 8) {} #[
          text' "Card 2" smallFont Color.white .center,
          coloredBox (Color.hsv 0.33 0.5 0.7) 60 40
        ],
      card (Color.hsv 0.66 0.6 0.3) 12 do  -- dark blue
        column (gap := 8) {} #[
          text' "Card 3" smallFont Color.white .center,
          coloredBox (Color.hsv 0.66 0.5 0.7) 60 40
        ]
    ],

    -- Grid demo
    text' "Grid Layout (3 columns)" smallFont Color.white .left,
    grid 3 (gap := 8) {} #[
      coloredBox (Color.hsv 0.0 0.6 0.8) 0 50,
      coloredBox (Color.hsv 0.1 0.6 0.8) 0 50,
      coloredBox (Color.hsv 0.2 0.6 0.8) 0 50,
      coloredBox (Color.hsv 0.3 0.6 0.8) 0 50,
      coloredBox (Color.hsv 0.4 0.6 0.8) 0 50,
      coloredBox (Color.hsv 0.5 0.6 0.8) 0 50
    ],

    -- Alignment demo
    text' "Alignment Demo" smallFont Color.white .left,
    row (gap := 16) {} #[
      box { backgroundColor := some (Color.gray 0.3), minWidth := some 100, minHeight := some 60 },
      center (style := { backgroundColor := some (Color.gray 0.3), minWidth := some 100, minHeight := some 60 }) do
        text' "Centered" smallFont Color.white .center,
      box { backgroundColor := some (Color.gray 0.3), minWidth := some 100, minHeight := some 60 }
    ]
  ]

/-- Simple scroll demo widget. -/
def scrollDemoWidget (font : Font) (scrollY : Float) : Widget := build do
  column (gap := 16) (style := { backgroundColor := some (Color.gray 0.15), padding := Layout.EdgeInsets.uniform 20 }) #[
    text' "Scroll Container Demo" font Color.white .center,

    -- Scroll container with many items
    scroll (style := { backgroundColor := some (Color.gray 0.25), minWidth := some 300, minHeight := some 200 })
           300 500  -- content size: 300x500
           { offsetY := scrollY } do
      column (gap := 8) (style := { padding := Layout.EdgeInsets.uniform 10 }) #[
        text' "Item 1" font Color.white .left,
        coloredBox Color.red 280 40,
        text' "Item 2" font Color.white .left,
        coloredBox Color.green 280 40,
        text' "Item 3" font Color.white .left,
        coloredBox Color.blue 280 40,
        text' "Item 4" font Color.white .left,
        coloredBox Color.yellow 280 40,
        text' "Item 5" font Color.white .left,
        coloredBox Color.cyan 280 40,
        text' "Item 6" font Color.white .left,
        coloredBox Color.magenta 280 40,
        text' "Item 7" font Color.white .left,
        coloredBox Color.orange 280 40
      ]
  ]

/-- Render the widget demo. -/
def renderWidgetsM (font : Font) (smallFont : Font) (width height : Float) : CanvasM Unit := do
  let widget := widgetDemo font smallFont
  renderUICentered widget width height

/-- Render the widget demo shapes only (no labels needed since text is in widgets). -/
def renderWidgetShapesM (font : Font) (smallFont : Font) (width height : Float) : CanvasM Unit := do
  renderWidgetsM font smallFont width height

end Demos

/-
  Afferent Widget System
  Declarative, display-only widget system for building UIs.

  Usage:
  ```lean
  import Afferent
  import Afferent.Widget

  open Afferent.Widget

  def myUI (font : Font) : Widget := build do
    column (gap := 16) (style := BoxStyle.card Color.darkGray 24) #[
      text' "Hello, Widgets!" font Color.white .center,
      row (gap := 8) {} #[
        coloredBox Color.red 60 60,
        coloredBox Color.green 60 60,
        coloredBox Color.blue 60 60
      ],
      wrappedText "This text will wrap automatically." font 200 Color.gray
    ]

  def render : CanvasM Unit := do
    let font ← ...
    renderUI (myUI font) 800 600
  ```
-/
import Afferent.Widget.Core
import Afferent.Widget.TextLayout
import Afferent.Widget.Measure
import Afferent.Widget.Render
import Afferent.Widget.Scroll
import Afferent.Widget.DSL
import Afferent.Widget.UI

-- All types and functions are available in the Afferent.Widget namespace
-- after importing this module

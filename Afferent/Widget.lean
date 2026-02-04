/-
  Afferent Widget System
  Declarative, display-only widget system for building UIs.

  This module re-exports Afferent.Arbor (the renderer-agnostic widget library) and provides
  the Afferent rendering backend that converts Arbor RenderCommands to Metal-backed
  CanvasM drawing calls.

  Usage:
  ```lean
  import Afferent
  import Afferent.Widget

  open Afferent.Widget
  open Afferent.Arbor

  def myUI (fontId : FontId) : Widget := build do
    column (gap := 16) (style := { backgroundColor := some (Color.gray 0.2), padding := EdgeInsets.uniform 24 }) #[
      text' "Hello, Widgets!" fontId Color.white .center,
      row (gap := 8) {} #[
        coloredBox Color.red 60 60,
        coloredBox Color.green 60 60,
        coloredBox Color.blue 60 60
      ]
    ]

  def render (reg : FontRegistry) (fontId : FontId) : CanvasM Unit := do
    renderArborWidget reg (myUI fontId) 800 600
  ```
-/

-- Re-export Arbor widget system (now under Afferent.Arbor)
import Afferent.Arbor

-- Afferent-specific backend that renders Arbor widgets via CanvasM
import Afferent.Widget.Backend
import Afferent.Text.Measurer

-- Note: After importing this module, you can use:
-- - Afferent.Arbor.* for widget types and DSL (Widget, build, row, column, etc.)
-- - Afferent.FontRegistry, Afferent.runWithFonts for font management
-- - Afferent.Widget.renderArborWidget for rendering

/-
  Arbor - Renderer-Agnostic Widget Library

  Arbor provides a declarative widget system that generates abstract
  render commands instead of directly rendering to a specific backend.

  Key abstractions:
  - RenderCommand: Abstract drawing operations (fillRect, fillText, etc.)
  - TextMeasurer: Typeclass for measuring text (pluggable backends)
  - Widget: Declarative widget tree (flex, grid, text, scroll, etc.)

  Usage:
  1. Build a widget tree using the DSL (row, column, text', box, etc.)
  2. Measure the tree with measureWidget (using a TextMeasurer backend)
  3. Compute layout with Trellis.layout
  4. Collect render commands with collectCommands
  5. Execute commands with your rendering backend
-/

-- Core types
import Afferent.Arbor.Core.Types
import Afferent.Arbor.Core.TextMeasurer

-- Render commands
import Afferent.Arbor.Render.Command
import Afferent.Arbor.Render.Builder
import Afferent.Arbor.Render.Collect

-- Widget system
import Afferent.Arbor.Widget.Core
import Afferent.Arbor.Widget.DSL
import Afferent.Arbor.Widget.TextLayout
import Afferent.Arbor.Widget.Measure
import Afferent.Arbor.Widget.MeasureCache

-- Event system
import Afferent.Arbor.Event.Types
import Afferent.Arbor.Event.HitTest
import Afferent.Arbor.Event.Scroll
import Afferent.Arbor.App.UI

-- Text-based rendering (for debugging/testing)
import Afferent.Arbor.Text.Canvas
import Afferent.Arbor.Text.Mode
import Afferent.Arbor.Text.Renderer

-- CSS-like styling DSL
import Afferent.Arbor.Style.CSS

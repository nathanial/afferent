/-
  Arbor Text Rendering Modes
  Configuration for different ASCII rendering modes optimized for AI analysis.
-/

namespace Afferent.Arbor.Text

/-- Rendering mode for ASCII output.
    Each mode is optimized for a different use case. -/
inductive RenderMode where
  /-- Pretty visual rendering (current behavior). Good for visual preview. -/
  | visual
  /-- Widget boundaries with IDs and type labels. Shows structure clearly. -/
  | structure
  /-- Pure text tree format showing widget hierarchy. Best for AI analysis. -/
  | hierarchy
  /-- Visual rendering with legend section. Combines visual and metadata. -/
  | combined
deriving Repr, BEq, Inhabited

/-- Configuration for debug-friendly rendering. -/
structure RenderConfig where
  /-- Which rendering mode to use. -/
  mode : RenderMode := .visual
  /-- Show x, y coordinates for each widget. -/
  showCoordinates : Bool := true
  /-- Show width, height dimensions for each widget. -/
  showDimensions : Bool := true
  /-- Show nesting depth for each widget (redundant with tree indentation). -/
  showDepth : Bool := false
  /-- Show widget type (flex, grid, text, rect, etc.). -/
  showWidgetType : Bool := true
  /-- Show style information (background color, padding, etc.). -/
  showStyles : Bool := false
  /-- Maximum length for widget labels before truncation. -/
  maxLabelLength : Nat := 20
  /-- Maximum length for text content preview. -/
  maxContentLength : Nat := 30
deriving Repr, Inhabited

namespace RenderConfig

/-- Default configuration for visual mode. -/
def visualMode : RenderConfig := { mode := .visual }

/-- Default configuration for structure mode. -/
def structureMode : RenderConfig := { mode := .structure }

/-- Default configuration for hierarchy mode. -/
def hierarchyMode : RenderConfig := { mode := .hierarchy }

/-- Default configuration for combined mode. -/
def combinedMode : RenderConfig := { mode := .combined }

/-- Verbose configuration showing all information. -/
def verbose : RenderConfig :=
  { mode := .hierarchy
    showCoordinates := true
    showDimensions := true
    showDepth := true
    showWidgetType := true
    showStyles := true }

end RenderConfig

end Afferent.Arbor.Text

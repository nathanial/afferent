/-
  Chart Core Types
  Common dimension structures shared across chart types.
-/

namespace Afferent.Canopy

/-! ## Base Dimension Structures

These structures provide common fields that are shared across multiple chart types.
Individual charts extend these bases and add their specialized fields.
-/

/-- Basic chart size dimensions. All charts have width and height. -/
structure ChartSize where
  width : Float := 400.0
  height : Float := 250.0
deriving Repr, Inhabited

/-- Standard chart margins for axis-based charts. -/
structure ChartMargins where
  marginTop : Float := 20.0
  marginBottom : Float := 40.0
  marginLeft : Float := 50.0
  marginRight : Float := 20.0
deriving Repr, Inhabited

/-- Grid display options for charts with background grids. -/
structure GridOptions where
  showGridLines : Bool := true
  gridLineCount : Nat := 5
deriving Repr, Inhabited

/-- Common dimensions for axis-based charts (bar, line, area, scatter, etc.).
    Combines size, margins, and grid options. -/
structure AxisChartDimensions extends ChartSize, ChartMargins, GridOptions
deriving Repr, Inhabited

/-- Default axis chart dimensions. -/
def AxisChartDimensions.default : AxisChartDimensions := {}

end Afferent.Canopy

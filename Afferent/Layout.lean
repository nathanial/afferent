/-
  Afferent Layout
  CSS Flexbox and Grid layout system.

  This module re-exports the Trellis layout library and provides conversions
  to Afferent's geometry types.
-/
import Trellis
import Afferent.Core.Types

namespace Afferent.Layout

-- Re-export core types as aliases for backward compatibility
abbrev Length := Trellis.Length
abbrev Dimension := Trellis.Dimension
abbrev EdgeInsets := Trellis.EdgeInsets
abbrev BoxConstraints := Trellis.BoxConstraints

-- Flex types
abbrev FlexDirection := Trellis.FlexDirection
abbrev FlexWrap := Trellis.FlexWrap
abbrev JustifyContent := Trellis.JustifyContent
abbrev AlignItems := Trellis.AlignItems
abbrev AlignContent := Trellis.AlignContent
abbrev FlexContainer := Trellis.FlexContainer
abbrev FlexItem := Trellis.FlexItem

-- Grid types
abbrev TrackSize := Trellis.TrackSize
abbrev GridTrack := Trellis.GridTrack
abbrev GridTemplate := Trellis.GridTemplate
abbrev GridLine := Trellis.GridLine
abbrev GridSpan := Trellis.GridSpan
abbrev GridPlacement := Trellis.GridPlacement
abbrev GridAutoFlow := Trellis.GridAutoFlow
abbrev GridContainer := Trellis.GridContainer
abbrev GridItem := Trellis.GridItem

-- Axis
abbrev AxisInfo := Trellis.AxisInfo

-- Node types
abbrev ContainerKind := Trellis.ContainerKind
abbrev ItemKind := Trellis.ItemKind
abbrev ContentSize := Trellis.ContentSize
abbrev LayoutNode := Trellis.LayoutNode

-- Result types
abbrev LayoutRect := Trellis.LayoutRect
abbrev ComputedLayout := Trellis.ComputedLayout
abbrev LayoutResult := Trellis.LayoutResult

-- Algorithm
def layout := Trellis.layout

end Afferent.Layout

-- Conversions between Trellis and Afferent geometry types
namespace Trellis.LayoutRect

/-- Convert a Trellis LayoutRect to an Afferent Rect. -/
def toAfferentRect (r : Trellis.LayoutRect) : Afferent.Rect :=
  { origin := ⟨r.x, r.y⟩, size := ⟨r.width, r.height⟩ }

end Trellis.LayoutRect

namespace Afferent.Rect

/-- Convert an Afferent Rect to a Trellis LayoutRect. -/
def toLayoutRect (r : Afferent.Rect) : Trellis.LayoutRect :=
  ⟨r.origin.x, r.origin.y, r.size.width, r.size.height⟩

end Afferent.Rect

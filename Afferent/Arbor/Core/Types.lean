/-
  Arbor Core Types
  Re-exports types from Afferent.Core for use in the widget system.
-/
import Afferent.Core.Types

namespace Afferent.Arbor

-- Re-export core types from Afferent
export Afferent (Color Point Size Rect)

-- Re-export namespace members so Point.mk', Rect.mk', etc. work
namespace Point
  export Afferent.Point (zero mk' add sub scale negate distance midpoint lerp toVec2 fromVec2)
end Point

namespace Size
  export Afferent.Size (zero mk' scale area)
end Size

namespace Rect
  export Afferent.Rect (zero mk' x y width height minX minY maxX maxY center topLeft topRight bottomLeft bottomRight contains area)
end Rect

/-- Abstract font identifier with cached metrics.
    Backends map this to actual font handles.
    Metrics are populated when the font is registered. -/
structure FontId where
  id : Nat
  name : String
  size : Float
  /-- Actual line height from font metrics (vertical distance between baselines). -/
  lineHeight : Float := 0
  /-- Distance from baseline to top of tallest glyph. -/
  ascender : Float := 0
  /-- Distance from baseline to bottom of lowest glyph (typically negative). -/
  descender : Float := 0
deriving Repr, BEq, Inhabited

namespace FontId

def default : FontId := ⟨0, "default", 14.0, 16.8, 11.2, -4.8⟩

def withSize (f : FontId) (size : Float) : FontId :=
  -- Scale metrics proportionally when size changes
  let scale := if f.size > 0 then size / f.size else 1.0
  { f with
    size
    lineHeight := f.lineHeight * scale
    ascender := f.ascender * scale
    descender := f.descender * scale }

/-- Create a FontId with metrics from actual font measurements. -/
def withMetrics (f : FontId) (lineHeight ascender descender : Float) : FontId :=
  { f with lineHeight, ascender, descender }

/-- Glyph bounding box height (ascender - descender). -/
def glyphHeight (f : FontId) : Float :=
  f.ascender - f.descender

end FontId

end Afferent.Arbor

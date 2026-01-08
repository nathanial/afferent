/-
  Arbor Core Types
  Basic types used throughout the widget system.
-/
import Tincture

namespace Afferent.Arbor

-- Re-export Color from Tincture
export Tincture (Color)

/-- A 2D point. -/
structure Point where
  x : Float
  y : Float
deriving Repr, BEq, Inhabited

namespace Point

def zero : Point := ⟨0, 0⟩
def mk' (x y : Float) : Point := ⟨x, y⟩

def add (p1 p2 : Point) : Point := ⟨p1.x + p2.x, p1.y + p2.y⟩
def sub (p1 p2 : Point) : Point := ⟨p1.x - p2.x, p1.y - p2.y⟩

instance : Add Point := ⟨add⟩
instance : Sub Point := ⟨sub⟩

end Point

/-- A 2D size. -/
structure Size where
  width : Float
  height : Float
deriving Repr, BEq, Inhabited

namespace Size

def zero : Size := ⟨0, 0⟩
def mk' (w h : Float) : Size := ⟨w, h⟩

end Size

/-- A rectangle defined by origin and size. -/
structure Rect where
  origin : Point
  size : Size
deriving Repr, BEq, Inhabited

namespace Rect

def zero : Rect := ⟨Point.zero, Size.zero⟩

def mk' (x y w h : Float) : Rect :=
  ⟨⟨x, y⟩, ⟨w, h⟩⟩

def x (r : Rect) : Float := r.origin.x
def y (r : Rect) : Float := r.origin.y
def width (r : Rect) : Float := r.size.width
def height (r : Rect) : Float := r.size.height

def right (r : Rect) : Float := r.x + r.width
def bottom (r : Rect) : Float := r.y + r.height

def contains (r : Rect) (px py : Float) : Bool :=
  px >= r.x && px <= r.right && py >= r.y && py <= r.bottom

def containsPoint (r : Rect) (p : Point) : Bool :=
  r.contains p.x p.y

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

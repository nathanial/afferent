/-
  Afferent Core Types
  Basic geometric primitives for 2D graphics.
-/
import Tincture
import Linalg.Vec2

-- Re-export Color type and namespace so existing code using Color.black etc. works
open Tincture (Color)

namespace Afferent

-- Re-export Color type
export Tincture (Color)

-- Re-export Color namespace members so Color.black, Color.rgb etc. work
namespace Color
  export Tincture.Color (
    -- Named colors
    black white red green blue yellow cyan magenta orange purple transparent
    gray darkGray lightGray
    -- Constructors
    rgba rgb hsv hsva fromRgb8
    -- Alpha
    withAlpha
    -- Color math
    lerp premultiply unpremultiply
    -- Adjustments (from Tincture.Adjust)
    lighten darken saturate desaturate
    rotateHue rotateHueDeg
    invert grayscale sepia
    brightness contrast
    fade opacify
    tint shade tone
  )
end Color

/-- A 2D point with x and y coordinates. -/
structure Point where
  x : Float
  y : Float
deriving Repr, BEq, Inhabited

namespace Point

def zero : Point := ⟨0.0, 0.0⟩

def mk' (x y : Float) : Point := ⟨x, y⟩

def add (p1 p2 : Point) : Point :=
  ⟨p1.x + p2.x, p1.y + p2.y⟩

def sub (p1 p2 : Point) : Point :=
  ⟨p1.x - p2.x, p1.y - p2.y⟩

def scale (p : Point) (s : Float) : Point :=
  ⟨p.x * s, p.y * s⟩

def negate (p : Point) : Point :=
  ⟨-p.x, -p.y⟩

def distance (p1 p2 : Point) : Float :=
  let dx := p2.x - p1.x
  let dy := p2.y - p1.y
  Float.sqrt (dx * dx + dy * dy)

def midpoint (p1 p2 : Point) : Point :=
  ⟨(p1.x + p2.x) / 2.0, (p1.y + p2.y) / 2.0⟩

def lerp (p1 p2 : Point) (t : Float) : Point :=
  ⟨p1.x + (p2.x - p1.x) * t, p1.y + (p2.y - p1.y) * t⟩

instance : Add Point := ⟨add⟩
instance : Sub Point := ⟨sub⟩
instance : Neg Point := ⟨negate⟩
instance : HMul Point Float Point := ⟨scale⟩
instance : HMul Float Point Point := ⟨fun s p => scale p s⟩

/-- Convert Point to Linalg Vec2. -/
def toVec2 (p : Point) : Linalg.Vec2 := ⟨p.x, p.y⟩

/-- Create Point from Linalg Vec2. -/
def fromVec2 (v : Linalg.Vec2) : Point := ⟨v.x, v.y⟩

end Point

/-- A 2D size with width and height. -/
structure Size where
  width : Float
  height : Float
deriving Repr, BEq, Inhabited

namespace Size

def zero : Size := ⟨0.0, 0.0⟩

def mk' (width height : Float) : Size := ⟨width, height⟩

def scale (s : Size) (factor : Float) : Size :=
  ⟨s.width * factor, s.height * factor⟩

def area (s : Size) : Float :=
  s.width * s.height

end Size

/-- A rectangle defined by origin point and size. -/
structure Rect where
  origin : Point
  size : Size
deriving Repr, BEq, Inhabited

namespace Rect

def zero : Rect := ⟨Point.zero, Size.zero⟩

def mk' (x y width height : Float) : Rect :=
  ⟨⟨x, y⟩, ⟨width, height⟩⟩

def x (r : Rect) : Float := r.origin.x
def y (r : Rect) : Float := r.origin.y
def width (r : Rect) : Float := r.size.width
def height (r : Rect) : Float := r.size.height

def minX (r : Rect) : Float := r.origin.x
def minY (r : Rect) : Float := r.origin.y
def maxX (r : Rect) : Float := r.origin.x + r.size.width
def maxY (r : Rect) : Float := r.origin.y + r.size.height

def center (r : Rect) : Point :=
  ⟨r.origin.x + r.size.width / 2.0, r.origin.y + r.size.height / 2.0⟩

def topLeft (r : Rect) : Point := r.origin
def topRight (r : Rect) : Point := ⟨r.maxX, r.origin.y⟩
def bottomLeft (r : Rect) : Point := ⟨r.origin.x, r.maxY⟩
def bottomRight (r : Rect) : Point := ⟨r.maxX, r.maxY⟩

def contains (r : Rect) (p : Point) : Bool :=
  p.x >= r.minX && p.x <= r.maxX && p.y >= r.minY && p.y <= r.maxY

def area (r : Rect) : Float :=
  r.size.area

end Rect

end Afferent

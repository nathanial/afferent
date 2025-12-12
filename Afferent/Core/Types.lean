/-
  Afferent Core Types
  Basic geometric primitives and colors for 2D graphics.
-/

namespace Afferent

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

/-- RGBA color with components in range 0.0 to 1.0. -/
structure Color where
  r : Float
  g : Float
  b : Float
  a : Float
deriving Repr, BEq, Inhabited

namespace Color

def rgba (r g b a : Float) : Color := ⟨r, g, b, a⟩
def rgb (r g b : Float) : Color := ⟨r, g, b, 1.0⟩

/-- Create color from HSV values. H in [0,1] (0=red, 0.33=green, 0.67=blue), S and V in [0,1]. -/
def hsv (h s v : Float) : Color :=
  if s == 0.0 then
    rgb v v v
  else
    let h' := h - h.floor  -- normalize to [0, 1)
    let sector := (h' * 6.0).floor
    let f := h' * 6.0 - sector
    let p := v * (1.0 - s)
    let q := v * (1.0 - s * f)
    let t := v * (1.0 - s * (1.0 - f))
    match sector.toUInt8 % 6 with
    | 0 => rgb v t p
    | 1 => rgb q v p
    | 2 => rgb p v t
    | 3 => rgb p q v
    | 4 => rgb t p v
    | _ => rgb v p q

/-- Create color from HSVA values. H in [0,1], S, V, A in [0,1]. -/
def hsva (h s v a : Float) : Color :=
  let c := hsv h s v
  ⟨c.r, c.g, c.b, a⟩

-- Standard colors
def black : Color := rgb 0.0 0.0 0.0
def white : Color := rgb 1.0 1.0 1.0
def red : Color := rgb 1.0 0.0 0.0
def green : Color := rgb 0.0 1.0 0.0
def blue : Color := rgb 0.0 0.0 1.0
def yellow : Color := rgb 1.0 1.0 0.0
def cyan : Color := rgb 0.0 1.0 1.0
def magenta : Color := rgb 1.0 0.0 1.0
def orange : Color := rgb 1.0 0.65 0.0
def purple : Color := rgb 0.5 0.0 0.5
def transparent : Color := rgba 0.0 0.0 0.0 0.0

-- Grays
def gray (value : Float) : Color := rgb value value value
def darkGray : Color := gray 0.25
def lightGray : Color := gray 0.75

def withAlpha (c : Color) (a : Float) : Color :=
  ⟨c.r, c.g, c.b, a⟩

def lerp (c1 c2 : Color) (t : Float) : Color :=
  ⟨c1.r + (c2.r - c1.r) * t,
   c1.g + (c2.g - c1.g) * t,
   c1.b + (c2.b - c1.b) * t,
   c1.a + (c2.a - c1.a) * t⟩

/-- Convert to premultiplied alpha (for compositing). -/
def premultiply (c : Color) : Color :=
  ⟨c.r * c.a, c.g * c.a, c.b * c.a, c.a⟩

/-- Convert from premultiplied alpha back to straight alpha. -/
def unpremultiply (c : Color) : Color :=
  if c.a == 0.0 then transparent
  else ⟨c.r / c.a, c.g / c.a, c.b / c.a, c.a⟩

end Color

end Afferent

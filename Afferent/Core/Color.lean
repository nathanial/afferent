/-
  Afferent Color
  RGBA color type with HSV support.
-/

namespace Afferent

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

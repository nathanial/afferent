/-
  Arbor Text Canvas
  A character grid for ASCII/Unicode text-based rendering.
-/
namespace Afferent.Arbor.Text

/-- A character cell with optional styling. -/
structure Cell where
  char : Char
  /-- Foreground color hint (for terminals that support color). -/
  fg : Option UInt8 := none
  /-- Background color hint. -/
  bg : Option UInt8 := none
deriving Repr, BEq, Inhabited

namespace Cell

def empty : Cell := ⟨' ', none, none⟩
def fromChar (c : Char) : Cell := ⟨c, none, none⟩

end Cell

/-- A 2D grid of character cells. -/
structure Canvas where
  width : Nat
  height : Nat
  cells : Array Cell
deriving Repr, Inhabited

namespace Canvas

/-- Create an empty canvas filled with spaces. -/
def create (width height : Nat) : Canvas :=
  ⟨width, height, Array.replicate (width * height) Cell.empty⟩

/-- Get the index for a coordinate. Returns none if out of bounds. -/
def indexOf (c : Canvas) (x y : Nat) : Option Nat :=
  if x < c.width && y < c.height then
    some (y * c.width + x)
  else
    none

/-- Get the cell at a coordinate. -/
def get (c : Canvas) (x y : Nat) : Cell :=
  match c.indexOf x y with
  | some idx => c.cells.getD idx Cell.empty
  | none => Cell.empty

/-- Set the cell at a coordinate. -/
def set (c : Canvas) (x y : Nat) (cell : Cell) : Canvas :=
  match c.indexOf x y with
  | some idx =>
    if h : idx < c.cells.size then
      { c with cells := c.cells.set idx cell }
    else c
  | none => c

/-- Set a character at a coordinate. -/
def setChar (c : Canvas) (x y : Nat) (ch : Char) : Canvas :=
  c.set x y (Cell.fromChar ch)

/-- Fill a rectangular region with a character. -/
def fillRect (c : Canvas) (x y w h : Nat) (ch : Char) : Canvas := Id.run do
  let mut result := c
  for row in [:h] do
    for col in [:w] do
      result := result.setChar (x + col) (y + row) ch
  result

/-- Draw a horizontal line. -/
def hline (c : Canvas) (x y len : Nat) (ch : Char := '─') : Canvas := Id.run do
  let mut result := c
  for i in [:len] do
    result := result.setChar (x + i) y ch
  result

/-- Draw a vertical line. -/
def vline (c : Canvas) (x y len : Nat) (ch : Char := '│') : Canvas := Id.run do
  let mut result := c
  for i in [:len] do
    result := result.setChar x (y + i) ch
  result

/-- Box-drawing characters for borders. -/
structure BoxChars where
  topLeft : Char
  topRight : Char
  bottomLeft : Char
  bottomRight : Char
  horizontal : Char
  vertical : Char
deriving Repr, BEq, Inhabited

namespace BoxChars

/-- Single-line box drawing characters. -/
def single : BoxChars := ⟨'┌', '┐', '└', '┘', '─', '│'⟩

/-- Double-line box drawing characters. -/
def double : BoxChars := ⟨'╔', '╗', '╚', '╝', '═', '║'⟩

/-- Rounded corner box drawing characters. -/
def rounded : BoxChars := ⟨'╭', '╮', '╰', '╯', '─', '│'⟩

/-- ASCII-only box drawing characters. -/
def ascii : BoxChars := ⟨'+', '+', '+', '+', '-', '|'⟩

/-- Heavy box drawing characters. -/
def heavy : BoxChars := ⟨'┏', '┓', '┗', '┛', '━', '┃'⟩

end BoxChars

/-- Draw a box outline. -/
def strokeBox (c : Canvas) (x y w h : Nat) (chars : BoxChars := .single) : Canvas :=
  if w < 2 || h < 2 then c
  else Id.run do
    let mut result := c
    -- Corners
    result := result.setChar x y chars.topLeft
    result := result.setChar (x + w - 1) y chars.topRight
    result := result.setChar x (y + h - 1) chars.bottomLeft
    result := result.setChar (x + w - 1) (y + h - 1) chars.bottomRight
    -- Horizontal edges
    for i in [1:w-1] do
      result := result.setChar (x + i) y chars.horizontal
      result := result.setChar (x + i) (y + h - 1) chars.horizontal
    -- Vertical edges
    for i in [1:h-1] do
      result := result.setChar x (y + i) chars.vertical
      result := result.setChar (x + w - 1) (y + i) chars.vertical
    result

/-- Draw text at a position. Clips to canvas bounds. -/
def drawText (c : Canvas) (x y : Nat) (text : String) : Canvas := Id.run do
  let mut result := c
  let mut col := x
  for ch in text.toList do
    if col < c.width then
      result := result.setChar col y ch
      col := col + 1
  result

/-- Draw text centered horizontally within a region. -/
def drawTextCentered (c : Canvas) (x y w : Nat) (text : String) : Canvas :=
  let textLen := text.length
  let startX := if textLen >= w then x else x + (w - textLen) / 2
  c.drawText startX y text

/-- Draw text right-aligned within a region. -/
def drawTextRight (c : Canvas) (x y w : Nat) (text : String) : Canvas :=
  let textLen := text.length
  let startX := if textLen >= w then x else x + w - textLen
  c.drawText startX y text

/-- Convert canvas to a string with newlines. -/
def toString (c : Canvas) : String := Id.run do
  let mut lines : Array String := #[]
  for row in [:c.height] do
    let mut line := ""
    for col in [:c.width] do
      line := line.push (c.get col row).char
    -- Trim trailing spaces
    lines := lines.push line.trimRight
  -- Join with newlines, trim trailing empty lines
  let result := "\n".intercalate lines.toList
  result.trimRight

instance : ToString Canvas := ⟨Canvas.toString⟩

/-- Fill the entire canvas with a character. -/
def fill (c : Canvas) (ch : Char) : Canvas :=
  { c with cells := Array.replicate (c.width * c.height) (Cell.fromChar ch) }

/-- Clear the canvas (fill with spaces). -/
def clear (c : Canvas) : Canvas := c.fill ' '

end Canvas

end Afferent.Arbor.Text

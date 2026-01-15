/-
  Arbor Render Commands
  Abstract rendering commands that can be interpreted by different backends.
-/
import Afferent.Arbor.Core.Types
import Afferent.Arbor.Core.Path
import Afferent.Core.Paint

namespace Afferent.Arbor

/-- Text horizontal alignment. -/
inductive TextAlign where
  | left
  | center
  | right
deriving Repr, BEq, Inhabited

/-- Text vertical alignment. -/
inductive TextVAlign where
  | top
  | middle
  | bottom
deriving Repr, BEq, Inhabited

/-- Abstract render command.
    These commands describe what to render without knowing how. -/
inductive RenderCommand where
  /-- Fill a rectangle with a solid color. -/
  | fillRect (rect : Rect) (color : Color) (cornerRadius : Float := 0)

  /-- Fill a rectangle with a gradient or solid fill style. -/
  | fillRectStyle (rect : Rect) (style : Afferent.FillStyle) (cornerRadius : Float := 0)

  /-- Stroke a rectangle outline. -/
  | strokeRect (rect : Rect) (color : Color) (lineWidth : Float) (cornerRadius : Float := 0)

  /-- Fill a circle with a solid color. -/
  | fillCircle (center : Point) (radius : Float) (color : Color)

  /-- Stroke a circle outline. -/
  | strokeCircle (center : Point) (radius : Float) (color : Color) (lineWidth : Float)

  /-- Fill text at a position. -/
  | fillText (text : String) (x y : Float) (font : FontId) (color : Color)

  /-- Fill wrapped/multi-line text within bounds. -/
  | fillTextBlock (text : String) (rect : Rect) (font : FontId) (color : Color)
                  (align : TextAlign) (valign : TextVAlign)

  /-- Fill a convex polygon with a solid color. -/
  | fillPolygon (points : Array Point) (color : Color)

  /-- Stroke a convex polygon outline. -/
  | strokePolygon (points : Array Point) (color : Color) (lineWidth : Float)

  /-- Fill a path with a solid color. -/
  | fillPath (path : Path) (color : Color)

  /-- Fill a path with a gradient or solid fill style. -/
  | fillPathStyle (path : Path) (style : Afferent.FillStyle)

  /-- Stroke a path outline. -/
  | strokePath (path : Path) (color : Color) (lineWidth : Float)

  /-- Push a clipping rectangle onto the clip stack. -/
  | pushClip (rect : Rect)

  /-- Pop the top clipping rectangle from the stack. -/
  | popClip

  /-- Push a translation transform. -/
  | pushTranslate (dx dy : Float)

  /-- Push a rotation transform. -/
  | pushRotate (angle : Float)

  /-- Push a scaling transform. -/
  | pushScale (sx sy : Float)

  /-- Pop the top transform from the stack. -/
  | popTransform

  /-- Save the current graphics state. -/
  | save

  /-- Restore the previously saved graphics state. -/
  | restore
deriving Repr

namespace RenderCommand

/-- Create a filled rectangle command. -/
def fill (x y w h : Float) (color : Color) (radius : Float := 0) : RenderCommand :=
  .fillRect (Rect.mk' x y w h) color radius

/-- Create a filled rectangle command with a fill style. -/
def fillStyle (x y w h : Float) (style : Afferent.FillStyle) (radius : Float := 0) : RenderCommand :=
  .fillRectStyle (Rect.mk' x y w h) style radius

/-- Create a stroked rectangle command. -/
def stroke (x y w h : Float) (color : Color) (lineWidth : Float) (radius : Float := 0) : RenderCommand :=
  .strokeRect (Rect.mk' x y w h) color lineWidth radius

/-- Create a text command. -/
def text (s : String) (x y : Float) (font : FontId) (color : Color) : RenderCommand :=
  .fillText s x y font color

/-- Create a filled polygon command. -/
def fillPoly (points : Array Point) (color : Color) : RenderCommand :=
  .fillPolygon points color

/-- Create a stroked polygon command. -/
def strokePoly (points : Array Point) (color : Color) (lineWidth : Float := 1.0) : RenderCommand :=
  .strokePolygon points color lineWidth

/-- Create a filled circle command. -/
def circle (cx cy radius : Float) (color : Color) : RenderCommand :=
  .fillCircle ⟨cx, cy⟩ radius color

/-- Create a stroked circle command. -/
def strokeCircle' (cx cy radius : Float) (color : Color) (lineWidth : Float := 1.0) : RenderCommand :=
  .strokeCircle ⟨cx, cy⟩ radius color lineWidth

end RenderCommand

/-- A batch of render commands. -/
abbrev RenderCommands := Array RenderCommand

namespace RenderCommands

def empty : RenderCommands := #[]

def single (cmd : RenderCommand) : RenderCommands := #[cmd]

def add (cmds : RenderCommands) (cmd : RenderCommand) : RenderCommands :=
  cmds.push cmd

def merge (cmds1 cmds2 : RenderCommands) : RenderCommands :=
  cmds1 ++ cmds2

/-- Wrap commands in a clip region. -/
def withClip (rect : Rect) (cmds : RenderCommands) : RenderCommands :=
  #[.pushClip rect] ++ cmds ++ #[.popClip]

/-- Wrap commands in a translation. -/
def withTranslate (dx dy : Float) (cmds : RenderCommands) : RenderCommands :=
  #[.pushTranslate dx dy] ++ cmds ++ #[.popTransform]

end RenderCommands

end Afferent.Arbor

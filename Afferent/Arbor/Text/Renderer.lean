/-
  Arbor Text Renderer
  Interprets RenderCommands and draws them to a text Canvas.
  Useful for debugging, testing, and terminal output.
-/
import Afferent.Arbor.Text.Canvas
import Afferent.Arbor.Text.Mode
import Afferent.Arbor.Render.Command
import Afferent.Arbor.Render.Collect
import Afferent.Arbor.Widget.Core
import Afferent.Arbor.Widget.Measure
import Afferent.Arbor.Core.TextMeasurer
import Afferent.Core.Paint
import Trellis

namespace Afferent.Arbor.Text

open Afferent.Arbor

/-- Pad a string on the right with spaces to reach the given width. -/
def padRight (s : String) (width : Nat) : String :=
  if s.length >= width then s
  else s ++ String.ofList (List.replicate (width - s.length) ' ')

/-- Render state for tracking transforms and clips. -/
structure RenderState where
  canvas : Canvas
  /-- Current translation offset (x, y). -/
  translateX : Float := 0
  translateY : Float := 0
  /-- Transform stack for save/restore. -/
  transformStack : Array (Float × Float) := #[]
  /-- Clip stack (not fully implemented in text mode). -/
  clipStack : Array Rect := #[]
deriving Inhabited

namespace RenderState

def create (width height : Nat) : RenderState :=
  ⟨Canvas.create width height, 0, 0, #[], #[]⟩

/-- Convert float coordinates to canvas coordinates with current transform. -/
def toCanvasCoords (s : RenderState) (x y : Float) : (Nat × Nat) :=
  let tx := x + s.translateX
  let ty := y + s.translateY
  (tx.toUInt32.toNat, ty.toUInt32.toNat)

/-- Push current transform onto stack. -/
def pushTransform (s : RenderState) : RenderState :=
  { s with transformStack := s.transformStack.push (s.translateX, s.translateY) }

/-- Pop transform from stack. -/
def popTransform (s : RenderState) : RenderState :=
  match s.transformStack.back? with
  | some (tx, ty) =>
    { s with
      translateX := tx
      translateY := ty
      transformStack := s.transformStack.pop }
  | none => s

/-- Apply translation. -/
def translate (s : RenderState) (dx dy : Float) : RenderState :=
  { s with
    translateX := s.translateX + dx
    translateY := s.translateY + dy }

/-- Push a clip rectangle. -/
def pushClip (s : RenderState) (rect : Rect) : RenderState :=
  { s with clipStack := s.clipStack.push rect }

/-- Pop clip rectangle. -/
def popClip (s : RenderState) : RenderState :=
  { s with clipStack := s.clipStack.pop }

end RenderState

/-- Map color luminance to a fill character.
    Darker colors use denser characters. -/
def colorToFillChar (color : Color) : Char :=
  -- Use RGB average as simple luminance
  let lum := (color.r + color.g + color.b) / 3.0
  if lum < 0.2 then '█'       -- Very dark
  else if lum < 0.4 then '▓'  -- Dark
  else if lum < 0.6 then '▒'  -- Medium
  else if lum < 0.8 then '░'  -- Light
  else ' '                     -- Very light (background)

/-- Execute a single render command. -/
def executeCommand (state : RenderState) (cmd : RenderCommand) : RenderState :=
  match cmd with
  | .fillRect rect color cornerRadius =>
    let (x, y) := state.toCanvasCoords rect.x rect.y
    let w := rect.width.toUInt32.toNat
    let h := rect.height.toUInt32.toNat
    let ch := colorToFillChar color
    -- For rounded corners in text mode, we'll use box drawing
    if cornerRadius > 0 && w >= 2 && h >= 2 then
      -- Draw with rounded corners
      let canvas := state.canvas
        |> (·.fillRect (x + 1) y (w - 2) 1 ch)           -- Top edge
        |> (·.fillRect (x + 1) (y + h - 1) (w - 2) 1 ch) -- Bottom edge
        |> (·.fillRect x (y + 1) w (h - 2) ch)           -- Middle rows
        |> (·.setChar x y '╭')
        |> (·.setChar (x + w - 1) y '╮')
        |> (·.setChar x (y + h - 1) '╰')
        |> (·.setChar (x + w - 1) (y + h - 1) '╯')
      { state with canvas }
    else
      let canvas := state.canvas.fillRect x y w h ch
      { state with canvas }

  | .fillRectStyle rect style cornerRadius =>
    let color := Afferent.FillStyle.toColor style
    let (x, y) := state.toCanvasCoords rect.x rect.y
    let w := rect.width.toUInt32.toNat
    let h := rect.height.toUInt32.toNat
    let ch := colorToFillChar color
    if cornerRadius > 0 && w >= 2 && h >= 2 then
      let canvas := state.canvas
        |> (·.fillRect (x + 1) y (w - 2) 1 ch)           -- Top edge
        |> (·.fillRect (x + 1) (y + h - 1) (w - 2) 1 ch) -- Bottom edge
        |> (·.fillRect x (y + 1) w (h - 2) ch)           -- Middle rows
        |> (·.setChar x y '╭')
        |> (·.setChar (x + w - 1) y '╮')
        |> (·.setChar x (y + h - 1) '╰')
        |> (·.setChar (x + w - 1) (y + h - 1) '╯')
      { state with canvas }
    else
      let canvas := state.canvas.fillRect x y w h ch
      { state with canvas }

  | .strokeRect rect _color _lineWidth cornerRadius =>
    let (x, y) := state.toCanvasCoords rect.x rect.y
    let w := rect.width.toUInt32.toNat
    let h := rect.height.toUInt32.toNat
    let chars := if cornerRadius > 0 then Canvas.BoxChars.rounded else Canvas.BoxChars.single
    let canvas := state.canvas.strokeBox x y w h chars
    { state with canvas }

  | .fillCircle center radius color =>
    -- Approximate circle as filled box in text mode
    let (cx, cy) := state.toCanvasCoords center.x center.y
    let r := radius.toUInt32.toNat
    let ch := colorToFillChar color
    let x := if cx >= r then cx - r else 0
    let y := if cy >= r then cy - r else 0
    let canvas := state.canvas.fillRect x y (r * 2) (r * 2) ch
    { state with canvas }

  | .strokeCircle center radius _color _lineWidth =>
    -- Approximate circle outline as stroked box in text mode
    let (cx, cy) := state.toCanvasCoords center.x center.y
    let r := radius.toUInt32.toNat
    let x := if cx >= r then cx - r else 0
    let y := if cy >= r then cy - r else 0
    let canvas := state.canvas.strokeBox x y (r * 2) (r * 2) Canvas.BoxChars.rounded
    { state with canvas }

  | .strokeLine p1 p2 _color _lineWidth =>
    -- Draw a line using '-' or '|' characters
    let (x1, y1) := state.toCanvasCoords p1.x p1.y
    let (x2, y2) := state.toCanvasCoords p2.x p2.y
    let canvas :=
      if y1 == y2 then
        -- Horizontal line
        let minX := min x1 x2
        let maxX := max x1 x2
        state.canvas.fillRect minX y1 (maxX - minX + 1) 1 '-'
      else if x1 == x2 then
        -- Vertical line
        let minY := min y1 y2
        let maxY := max y1 y2
        state.canvas.fillRect x1 minY 1 (maxY - minY + 1) '|'
      else
        -- Diagonal line - just draw endpoints
        state.canvas.setChar x1 y1 '*' |>.setChar x2 y2 '*'
    { state with canvas }

  | .strokeLineBatch data count _lineWidth =>
    Id.run do
      let mut canvas := state.canvas
      for i in [:count] do
        let base := i * 9
        let x1 := data[base]!
        let y1 := data[base + 1]!
        let x2 := data[base + 2]!
        let y2 := data[base + 3]!
        let (cx1, cy1) := state.toCanvasCoords x1 y1
        let (cx2, cy2) := state.toCanvasCoords x2 y2
        canvas :=
          if cy1 == cy2 then
            let minX := min cx1 cx2
            let maxX := max cx1 cx2
            canvas.fillRect minX cy1 (maxX - minX + 1) 1 '-'
          else if cx1 == cx2 then
            let minY := min cy1 cy2
            let maxY := max cy1 cy2
            canvas.fillRect cx1 minY 1 (maxY - minY + 1) '|'
          else
            canvas.setChar cx1 cy1 '*' |>.setChar cx2 cy2 '*'
      return { state with canvas }

  | .fillCircleBatch data count =>
    Id.run do
      let mut canvas := state.canvas
      for i in [:count] do
        let base := i * 7
        let cx := data[base]!
        let cy := data[base + 1]!
        let (x, y) := state.toCanvasCoords cx cy
        canvas := canvas.setChar x y 'o'
      return { state with canvas }

  | .fillText text x y _font _color =>
    let (cx, cy) := state.toCanvasCoords x y
    let canvas := state.canvas.drawText cx cy text
    { state with canvas }

  | .fillTextBlock text rect _font _color align _valign =>
    let (x, y) := state.toCanvasCoords rect.x rect.y
    let w := rect.width.toUInt32.toNat
    let canvas := match align with
      | .left => state.canvas.drawText x y text
      | .center => state.canvas.drawTextCentered x y w text
      | .right => state.canvas.drawTextRight x y w text
    { state with canvas }

  | .fillPolygon points color =>
    if points.isEmpty then
      state
    else
      let (minX, minY, maxX, maxY) :=
        points.foldl
          (fun (minX, minY, maxX, maxY) p =>
            (min minX p.x, min minY p.y, max maxX p.x, max maxY p.y))
          (points[0]!.x, points[0]!.y, points[0]!.x, points[0]!.y)
      let rect := Rect.mk' minX minY (maxX - minX) (maxY - minY)
      let (x, y) := state.toCanvasCoords rect.x rect.y
      let w := rect.width.toUInt32.toNat
      let h := rect.height.toUInt32.toNat
      let ch := colorToFillChar color
      let canvas := state.canvas.fillRect x y w h ch
      { state with canvas }

  | .strokePolygon points _color _lineWidth =>
    if points.isEmpty then
      state
    else
      let (minX, minY, maxX, maxY) :=
        points.foldl
          (fun (minX, minY, maxX, maxY) p =>
            (min minX p.x, min minY p.y, max maxX p.x, max maxY p.y))
          (points[0]!.x, points[0]!.y, points[0]!.x, points[0]!.y)
      let rect := Rect.mk' minX minY (maxX - minX) (maxY - minY)
      let (x, y) := state.toCanvasCoords rect.x rect.y
      let w := rect.width.toUInt32.toNat
      let h := rect.height.toUInt32.toNat
      let canvas := state.canvas.strokeBox x y w h Canvas.BoxChars.single
      { state with canvas }

  | .fillPath _path _color =>
    -- Path rendering not supported in text mode
    state

  | .fillPathStyle _path _style =>
    -- Path rendering not supported in text mode
    state

  | .strokePath _path _color _lineWidth =>
    -- Path rendering not supported in text mode
    state

  | .fillPolygonInstanced _pathHash _vertices _indices _instances _centerX _centerY =>
    -- Instanced polygon rendering not supported in text mode
    state

  | .strokeArcInstanced _instances _segments =>
    -- Instanced arc rendering not supported in text mode
    state

  | .drawFragment _fragmentHash _primitiveType _params _instanceCount =>
    -- Fragment shader rendering not supported in text mode
    state

  | .fillTessellatedBatch _vertices _indices _vertexCount =>
    -- Tessellated batch rendering not supported in text mode
    state

  | .pushClip rect =>
    state.pushClip rect

  | .popClip =>
    state.popClip

  | .pushTranslate dx dy =>
    state.pushTransform.translate dx dy

  | .pushRotate _angle =>
    state.pushTransform

  | .pushScale _sx _sy =>
    state.pushTransform

  | .popTransform =>
    state.popTransform

  | .save =>
    state.pushTransform

  | .restore =>
    state.popTransform

/-- Execute a sequence of render commands. -/
def executeCommands (state : RenderState) (cmds : Array RenderCommand) : RenderState :=
  cmds.foldl executeCommand state

/-- Render commands to an ASCII string.
    This is the main entry point for text-based rendering.

    Parameters:
    - cmds: The render commands to execute
    - width: Canvas width in characters
    - height: Canvas height in characters

    Returns: A string representation of the rendered output. -/
def renderToAscii (cmds : Array RenderCommand) (width height : Nat) : String :=
  let state := RenderState.create width height
  let finalState := executeCommands state cmds
  finalState.canvas.toString

/-- Render commands to a Canvas for further manipulation. -/
def renderToCanvas (cmds : Array RenderCommand) (width height : Nat) : Canvas :=
  let state := RenderState.create width height
  let finalState := executeCommands state cmds
  finalState.canvas

/-- Render with a border around the entire output. -/
def renderWithBorder (cmds : Array RenderCommand) (width height : Nat)
    (title : String := "") (chars : Canvas.BoxChars := .single) : String := Id.run do
  -- Render content in the inner area
  let innerWidth := width - 2
  let innerHeight := height - 2
  let innerState := RenderState.create innerWidth innerHeight
  let innerFinal := executeCommands innerState cmds
  -- Create outer canvas with border
  let mut outer := Canvas.create width height
  outer := outer.strokeBox 0 0 width height chars
  -- Copy inner content
  for row in [:innerHeight] do
    for col in [:innerWidth] do
      let cell := innerFinal.canvas.get col row
      outer := outer.set (col + 1) (row + 1) cell
  -- Add title if provided
  if !title.isEmpty then
    let titleStr := s!" {title} "
    let startX := 2
    outer := outer.drawText startX 0 titleStr
  outer.toString

/-- Pretty-print a widget layout for debugging.
    Shows boxes with their positions and sizes. -/
def debugLayout (boxes : Array (String × Rect)) (width height : Nat) : String := Id.run do
  let mut cmds : Array RenderCommand := #[]
  for (label, rect) in boxes do
    -- Stroke the box
    cmds := cmds.push (.strokeRect rect Tincture.Color.white 1 0)
    -- Add label
    cmds := cmds.push (.fillText label (rect.x + 1) (rect.y) FontId.default Tincture.Color.white)
  renderToAscii cmds width height

/-- Simple fixed-width text measurer for ASCII rendering.
    Assumes each character is 1 unit wide. -/
structure AsciiMeasurer where
  charWidth : Float := 1.0
  charHeight : Float := 1.0
deriving Repr, Inhabited

namespace AsciiMeasurer

def default : AsciiMeasurer := {}

def measureText (m : AsciiMeasurer) (text : String) : TextMetrics :=
  let width := text.length.toFloat * m.charWidth
  let height := m.charHeight
  ⟨width, height, height * 0.8, height * 0.2, height⟩

def measureChar' (m : AsciiMeasurer) (_c : Char) : Float := m.charWidth

def fontMetrics' (m : AsciiMeasurer) : TextMetrics :=
  ⟨0, m.charHeight, m.charHeight * 0.8, m.charHeight * 0.2, m.charHeight⟩

end AsciiMeasurer

/-- TextMeasurer instance for ASCII rendering using Identity monad. -/
instance : TextMeasurer Id where
  measureText text _fontId := AsciiMeasurer.default.measureText text
  measureChar c _fontId := AsciiMeasurer.default.measureChar' c
  fontMetrics _fontId := AsciiMeasurer.default.fontMetrics'

/-- Render a widget tree to ASCII art.
    Uses fixed-width character measurement (1 char = 1 unit).

    Parameters:
    - widget: The widget tree to render
    - width: Canvas width in characters
    - height: Canvas height in characters

    Returns: ASCII art string representation of the widget. -/
def renderWidget (widget : Widget) (width height : Nat) : String :=
  -- Measure widget with ASCII measurer (1 char = 1 unit)
  let measureResult : MeasureResult := Id.run (measureWidget widget width.toFloat height.toFloat)
  let layoutNode := measureResult.node
  let measuredWidget := measureResult.widget
  -- Compute layout
  let layouts := Trellis.layout layoutNode width.toFloat height.toFloat
  -- Collect render commands
  let cmds := collectCommands measuredWidget layouts
  -- Render to ASCII
  renderToAscii cmds width height

/-- Render a widget tree to ASCII art with debug borders around each widget. -/
def renderWidgetDebug (widget : Widget) (width height : Nat)
    (borderColor : Color := Tincture.Color.white) : String :=
  let measureResult : MeasureResult := Id.run (measureWidget widget width.toFloat height.toFloat)
  let layoutNode := measureResult.node
  let measuredWidget := measureResult.widget
  let layouts := Trellis.layout layoutNode width.toFloat height.toFloat
  let cmds := collectCommandsWithDebug measuredWidget layouts borderColor
  renderToAscii cmds width height

/-- Render a widget tree centered in the canvas. -/
def renderWidgetCentered (widget : Widget) (width height : Nat) : String :=
  let (intrinsicW, intrinsicH) : Float × Float := Id.run (intrinsicSize widget)
  let measureResult : MeasureResult := Id.run (measureWidget widget intrinsicW intrinsicH)
  let layoutNode := measureResult.node
  let measuredWidget := measureResult.widget
  let layouts := Trellis.layout layoutNode intrinsicW intrinsicH
  -- Collect commands
  let cmds := collectCommands measuredWidget layouts
  -- Calculate offset to center
  let offsetX := (width.toFloat - intrinsicW) / 2
  let offsetY := (height.toFloat - intrinsicH) / 2
  -- Wrap in translation
  let centeredCmds := RenderCommands.withTranslate offsetX offsetY cmds
  renderToAscii centeredCmds width height

/-- Render a widget with a border around the canvas. -/
def renderWidgetWithBorder (widget : Widget) (width height : Nat)
    (title : String := "") (chars : Canvas.BoxChars := .single) : String :=
  let measureResult : MeasureResult := Id.run (measureWidget widget (width - 2).toFloat (height - 2).toFloat)
  let layoutNode := measureResult.node
  let measuredWidget := measureResult.widget
  let layouts := Trellis.layout layoutNode (width - 2).toFloat (height - 2).toFloat
  let cmds := collectCommands measuredWidget layouts
  renderWithBorder cmds width height title chars

/-! ## Debug Rendering - WidgetInfo Collection -/

/-- Metadata about a widget for debug output. -/
structure WidgetInfo where
  /-- Widget's unique identifier. -/
  id : WidgetId
  /-- Optional semantic name. -/
  name : Option String
  /-- Widget type as string (flex, grid, text, rect, scroll, spacer). -/
  widgetType : String
  /-- Container variant (row/column for flex, column count for grid). -/
  containerVariant : Option String
  /-- Computed layout rectangle. -/
  rect : Trellis.LayoutRect
  /-- Depth in widget tree (0 = root). -/
  depth : Nat
  /-- Parent widget ID if not root. -/
  parentId : Option WidgetId
  /-- Text content for text widgets. -/
  textContent : Option String
  /-- Style summary string. -/
  styleInfo : Option String
  /-- Number of children. -/
  childCount : Nat
deriving Repr, Inhabited

namespace WidgetInfo

/-- Format widget label like "[0]", "[header]", or "[0:header]". -/
def formatLabel (info : WidgetInfo) (showId : Bool := true) : String :=
  match info.name, showId with
  | some n, true => s!"[{info.id}:{n}]"
  | some n, false => s!"[{n}]"
  | none, _ => s!"[{info.id}]"

/-- Format widget type with variant like "flex/row" or "grid/3cols". -/
def formatType (info : WidgetInfo) : String :=
  match info.containerVariant with
  | some v => s!"{info.widgetType}/{v}"
  | none => info.widgetType

/-- Format coordinates like "(0,0)". -/
def formatCoords (info : WidgetInfo) : String :=
  s!"({info.rect.x.toUInt32},{info.rect.y.toUInt32})"

/-- Format dimensions like "30x12". -/
def formatDims (info : WidgetInfo) : String :=
  s!"{info.rect.width.toUInt32}x{info.rect.height.toUInt32}"

end WidgetInfo

/-- Convert a byte value to a 2-digit hex string. -/
private def toHex2 (n : Nat) : String :=
  let hexChars : Array Char := #['0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F']
  let hi := hexChars[n / 16 % 16]!
  let lo := hexChars[n % 16]!
  String.ofList [hi, lo]

/-- Format a BoxStyle as a summary string. -/
def formatStyle (style : BoxStyle) : String := Id.run do
  let mut parts : Array String := #[]
  if let some bg := style.backgroundColor then
    let r := (bg.r * 255).toUInt8.toNat
    let g := (bg.g * 255).toUInt8.toNat
    let b := (bg.b * 255).toUInt8.toNat
    parts := parts.push s!"bg=#{toHex2 r}{toHex2 g}{toHex2 b}"
  if style.padding.top > 0 || style.padding.left > 0 then
    parts := parts.push s!"pad={style.padding.top.toUInt32}"
  if style.borderWidth > 0 then
    parts := parts.push s!"border={style.borderWidth.toUInt32}"
  if parts.isEmpty then "-" else ", ".intercalate parts.toList

/-- Collect metadata for all widgets in tree. -/
partial def collectWidgetInfo (w : Widget) (layouts : Trellis.LayoutResult)
    (depth : Nat := 0) (parentId : Option WidgetId := none) : Array WidgetInfo := Id.run do
  let some computed := layouts.get w.id | return #[]
  let rect := computed.borderRect

  let (widgetType, containerVariant, textContent, styleInfo, childCount) := match w with
    | .flex _ _ props style children =>
        let variant := if props.direction.isHorizontal then "row" else "column"
        let gapInfo := if props.gap > 0 then s!",gap={props.gap.toUInt32}" else ""
        ("flex", some s!"{variant}{gapInfo}", none, some (formatStyle style), children.size)
    | .grid _ _ props style children =>
        let cols := props.templateColumns.tracks.size
        ("grid", some s!"{cols}cols", none, some (formatStyle style), children.size)
    | .text _ _ content _ _ _ _ _ =>
        ("text", none, some content, none, 0)
    | .rect _ _ style =>
        ("rect", none, none, some (formatStyle style), 0)
    | .scroll _ _ style _ _ _ _ _ =>
        ("scroll", none, none, some (formatStyle style), 1)
    | .spacer _ _ width height =>
        ("spacer", some s!"{width.toUInt32}x{height.toUInt32}", none, none, 0)
    | .custom _ _ style _ =>
        ("custom", none, none, some (formatStyle style), 0)

  let info : WidgetInfo := {
    id := w.id
    name := w.name?
    widgetType
    containerVariant
    rect
    depth
    parentId
    textContent
    styleInfo
    childCount
  }

  let childInfos := w.children.flatMap fun child =>
    collectWidgetInfo child layouts (depth + 1) (some w.id)

  #[info] ++ childInfos

/-! ## Hierarchy Mode Renderer -/

/-- Truncate a string to max length with ellipsis. -/
def truncate (s : String) (maxLen : Nat) : String :=
  if s.length <= maxLen then s
  else s.take (maxLen - 3) ++ "..."

/-- Check if widget at index i is the last sibling (no more widgets at same depth before depth decreases). -/
private def isLastSibling (infos : Array WidgetInfo) (i : Nat) : Bool := Id.run do
  let info := infos[i]!
  for j in [i+1:infos.size] do
    let next := infos[j]!
    if next.depth < info.depth then return true   -- Parent ended, was last
    if next.depth == info.depth then return false -- Sibling exists
  return true  -- End of list, was last

/-- Render widget tree as ASCII tree (hierarchy mode).
    This is the most useful mode for AI analysis. -/
def renderHierarchy (widget : Widget) (width height : Nat)
    (config : RenderConfig := {}) : String := Id.run do
  -- Measure and layout
  let measureResult : MeasureResult := Id.run (measureWidget widget width.toFloat height.toFloat)
  let layouts := Trellis.layout measureResult.node width.toFloat height.toFloat
  let infos := collectWidgetInfo measureResult.widget layouts

  let mut lines : Array String := #[]
  lines := lines.push "WIDGET TREE"
  lines := lines.push "==========="

  -- hasMoreAtLevel[d] = true if there are unprocessed siblings at depth d+1
  -- This is used to draw "│" continuation lines for ancestor branches
  let mut hasMoreAtLevel : Array Bool := #[]

  for i in [:infos.size] do
    let info := infos[i]!
    let isLast := isLastSibling infos i

    -- FIRST: Update hasMoreAtLevel BEFORE building prefix
    -- This ensures siblings share the same prefix
    if info.depth > 0 then
      -- Grow array if needed
      while hasMoreAtLevel.size < info.depth do
        hasMoreAtLevel := hasMoreAtLevel.push false
      -- Shrink array if we've moved up in the tree
      if hasMoreAtLevel.size > info.depth then
        hasMoreAtLevel := hasMoreAtLevel.shrink info.depth
      -- Mark whether there are more siblings at our parent's level
      hasMoreAtLevel := hasMoreAtLevel.set! (info.depth - 1) (!isLast)

    -- Build prefix: one segment per ancestor level (depths 1 to info.depth-1)
    -- For depth 1 nodes, there are 0 prefix segments
    -- For depth 2 nodes, there is 1 prefix segment (for depth 1)
    let mut linePrefix := ""
    if info.depth >= 2 then
      for d in [:info.depth - 1] do
        if d < hasMoreAtLevel.size && hasMoreAtLevel[d]! then
          linePrefix := linePrefix ++ "│   "
        else
          linePrefix := linePrefix ++ "    "

    -- The connector for this node
    let connector := if info.depth == 0 then ""
      else if isLast then "└── "
      else "├── "

    -- Main line: [id:name] type
    let label := info.formatLabel (showId := true)
    let typeStr := info.formatType
    lines := lines.push s!"{linePrefix}{connector}{label} {typeStr}"

    -- Detail lines use continuation prefix
    let detailPrefix := if info.depth == 0 then "    "
      else linePrefix ++ (if isLast then "        " else "│       ")

    if config.showCoordinates || config.showDimensions then
      let coordStr := if config.showCoordinates then info.formatCoords ++ " " else ""
      let dimStr := if config.showDimensions then info.formatDims else ""
      lines := lines.push s!"{detailPrefix}rect: {coordStr}{dimStr}"

    if config.showDepth then
      lines := lines.push s!"{detailPrefix}depth: {info.depth}"

    if config.showStyles then
      if let some style := info.styleInfo then
        lines := lines.push s!"{detailPrefix}style: {style}"

    if let some content := info.textContent then
      let displayContent := truncate content config.maxContentLength
      lines := lines.push s!"{detailPrefix}content: \"{displayContent}\""

  -- Summary section
  lines := lines.push ""
  lines := lines.push "SUMMARY"
  lines := lines.push "-------"
  lines := lines.push s!"Total widgets: {infos.size}"
  let maxDepth := infos.foldl (fun m i => max m i.depth) 0
  lines := lines.push s!"Tree depth: {maxDepth + 1}"
  let containers := infos.filter (·.childCount > 0)
  let flexCount := containers.filter (·.widgetType == "flex") |>.size
  let gridCount := containers.filter (·.widgetType == "grid") |>.size
  lines := lines.push s!"Containers: {containers.size} (flex={flexCount}, grid={gridCount})"
  let leaves := infos.filter (·.childCount == 0)
  let textCount := leaves.filter (·.widgetType == "text") |>.size
  let rectCount := leaves.filter (·.widgetType == "rect") |>.size
  let spacerCount := leaves.filter (·.widgetType == "spacer") |>.size
  lines := lines.push s!"Leaves: {leaves.size} (text={textCount}, rect={rectCount}, spacer={spacerCount})"

  "\n".intercalate lines.toList

/-! ## Combined Mode Renderer -/

/-- Render visual output with legend section (combined mode). -/
def renderCombined (widget : Widget) (width height : Nat)
    (_config : RenderConfig := {}) : String := Id.run do
  -- Measure and layout
  let measureResult : MeasureResult := Id.run (measureWidget widget width.toFloat height.toFloat)
  let layouts := Trellis.layout measureResult.node width.toFloat height.toFloat
  let infos := collectWidgetInfo measureResult.widget layouts

  -- Visual section
  let cmds := collectCommands measureResult.widget layouts
  let visualOutput := renderToAscii cmds width height

  let mut lines : Array String := #[]
  lines := lines.push s!"VISUAL OUTPUT ({width}x{height})"
  lines := lines.push (String.ofList (List.replicate (width + 5) '='))
  lines := lines.push visualOutput
  lines := lines.push ""

  -- Legend section
  lines := lines.push "LEGEND"
  lines := lines.push "------"

  for info in infos do
    let idCol := padRight info.formatLabel 12
    let typeCol := padRight info.formatType 12
    let coordCol := padRight (s!"@ " ++ info.formatCoords) 12
    let dimCol := padRight info.formatDims 8
    let depthCol := padRight s!"d={info.depth}" 6

    let extra := match info.textContent with
      | some c => s!"\"{truncate c 20}\""
      | none => info.styleInfo.getD ""

    lines := lines.push s!"{idCol} {typeCol} {coordCol} {dimCol} {depthCol} {extra}"

  "\n".intercalate lines.toList

/-! ## Structure Mode Renderer -/

/-- Render with widget boundaries labeled (structure mode). -/
def renderStructure (widget : Widget) (width height : Nat)
    (_config : RenderConfig := {}) : String := Id.run do
  -- Measure and layout
  let measureResult : MeasureResult := Id.run (measureWidget widget width.toFloat height.toFloat)
  let layouts := Trellis.layout measureResult.node width.toFloat height.toFloat
  let infos := collectWidgetInfo measureResult.widget layouts

  let mut canvas := Canvas.create width height

  -- PASS 1: Draw box outlines for LEAF widgets only
  -- Containers are implied by layout; showing their boxes creates visual clutter
  for info in infos do
    let x := info.rect.x.toUInt32.toNat
    let y := info.rect.y.toUInt32.toNat
    let w := info.rect.width.toUInt32.toNat
    let h := info.rect.height.toUInt32.toNat
    let isLeaf := info.childCount == 0
    if w >= 2 && h >= 2 && isLeaf then
      canvas := canvas.strokeBox x y w h .single

  -- PASS 2: Draw labels for LEAF widgets only (containers shown via boxes only)
  for info in infos do
    let x := info.rect.x.toUInt32.toNat
    let y := info.rect.y.toUInt32.toNat
    let w := info.rect.width.toUInt32.toNat
    let h := info.rect.height.toUInt32.toNat
    let isLeaf := info.childCount == 0

    if h >= 2 && w >= 2 && isLeaf then
      -- Leaf widget with visible box - draw label in the top border
      let label := info.formatLabel ++ ":" ++ info.widgetType
      let labelTrunc := truncate label (w - 2)
      if labelTrunc.length > 0 && w > 2 then
        canvas := canvas.drawText (x + 1) y labelTrunc

      -- For text widgets with boxes, show content centered
      if let some content := info.textContent then
        if h >= 3 then
          let displayText := truncate content (w - 4)
          let textY := y + h / 2
          canvas := canvas.drawTextCentered x textY w s!"\"{displayText}\""
    else if h < 2 && isLeaf then
      -- Leaf widget without box (like single-line text) - show labeled content
      if let some content := info.textContent then
        let label := s!"[{info.name.getD (toString info.id)}] "
        let displayText := truncate content (w - label.length)
        canvas := canvas.drawText x y (label ++ displayText)

  canvas.toString

/-! ## Unified API -/

/-- Main entry point for debug-friendly widget rendering.
    Use this with different RenderConfig.mode values for different output formats. -/
def renderWidgetWith (widget : Widget) (width height : Nat)
    (config : RenderConfig := {}) : String :=
  match config.mode with
  | .visual => renderWidget widget width height
  | .structure => renderStructure widget width height config
  | .hierarchy => renderHierarchy widget width height config
  | .combined => renderCombined widget width height config

/-- Convenience function for hierarchy mode (most useful for AI). -/
def renderHierarchyMode (widget : Widget) (width height : Nat) : String :=
  renderWidgetWith widget width height { mode := .hierarchy }

/-- Convenience function for structure mode. -/
def renderStructureMode (widget : Widget) (width height : Nat) : String :=
  renderWidgetWith widget width height { mode := .structure }

/-- Convenience function for combined mode. -/
def renderCombinedMode (widget : Widget) (width height : Nat) : String :=
  renderWidgetWith widget width height { mode := .combined }

end Afferent.Arbor.Text

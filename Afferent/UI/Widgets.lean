/-
  Afferent UI Widgets
  Immediate-mode widget functions.
-/
import Afferent.Core.Types
import Afferent.Core.Path
import Afferent.Canvas.Context
import Afferent.UI.Context

namespace Afferent.UI

/-- Button widget. Returns (clicked, updatedContext). -/
def button (ctx : UIContext) (label : String) (rect : Rect) : IO (Bool × UIContext) := do
  let id := label
  let isHot := ctx.input.isMouseOver rect
  let ctx := if isHot then ctx.setHot id else ctx

  -- Handle click interaction
  let wasActive := ctx.isActive id
  let mouseJustPressed := ctx.input.mousePressed .left
  let mouseJustReleased := ctx.input.mouseReleased .left

  -- Become active on press if hot
  let ctx := if isHot && mouseJustPressed then ctx.setActive id else ctx

  -- Click happens on release while still over the button
  let clicked := wasActive && mouseJustReleased && isHot

  -- Clear active on release
  let ctx := if ctx.isActive id && mouseJustReleased then ctx.clearActive else ctx

  -- Determine visual state
  let bgColor :=
    if ctx.isActive id then Style.buttonActive
    else if isHot then Style.buttonHover
    else Style.buttonBg

  -- Draw button background
  ctx.drawCtx.fillRoundedRect rect Style.cornerRadius bgColor

  -- Draw button text (centered)
  let (textW, textH) ← ctx.font.measureText label
  let textX := rect.x + (rect.width - textW) / 2.0
  let textY := rect.y + (rect.height + textH) / 2.0 - 2.0
  ctx.drawCtx.fillTextXY label textX textY ctx.font Style.buttonText

  pure (clicked, ctx)

/-- Label widget. Just draws text, no interaction. -/
def label (ctx : UIContext) (text : String) (pos : Point) : IO UIContext := do
  ctx.drawCtx.fillText text pos ctx.font Style.labelText
  pure ctx

/-- Label with custom color. -/
def labelColored (ctx : UIContext) (text : String) (pos : Point) (color : Color) : IO UIContext := do
  ctx.drawCtx.fillText text pos ctx.font color
  pure ctx

/-- Checkbox widget. Returns (newCheckedState, updatedContext). -/
def checkbox (ctx : UIContext) (id : String) (checked : Bool) (rect : Rect) : IO (Bool × UIContext) := do
  let isHot := ctx.input.isMouseOver rect
  let ctx := if isHot then ctx.setHot id else ctx

  -- Toggle on click (release)
  let mouseJustReleased := ctx.input.mouseReleased .left
  let clicked := isHot && mouseJustReleased
  let newChecked := if clicked then !checked else checked

  -- Draw checkbox background
  let borderColor := if isHot then Style.textBoxFocused else Style.checkboxBorder
  ctx.drawCtx.fillRoundedRect rect Style.cornerRadius Style.checkboxBg
  ctx.drawCtx.strokeRoundedRect rect Style.cornerRadius borderColor Style.borderWidth

  -- Draw checkmark if checked
  if newChecked then
    let inset := 4.0
    let innerRect := Rect.mk' (rect.x + inset) (rect.y + inset)
                              (rect.width - 2*inset) (rect.height - 2*inset)
    ctx.drawCtx.fillRoundedRect innerRect 2.0 Style.checkboxCheck

  pure (newChecked, ctx)

/-- Checkbox with label. The clickable area includes the label. -/
def checkboxLabeled (ctx : UIContext) (labelText : String) (checked : Bool) (pos : Point)
    : IO (Bool × UIContext) := do
  let boxSize := 20.0
  let boxRect := Rect.mk' pos.x pos.y boxSize boxSize

  -- Measure label to determine full clickable area
  let (labelW, _) ← ctx.font.measureText labelText
  let fullRect := Rect.mk' pos.x pos.y (boxSize + 8.0 + labelW) boxSize

  let id := labelText
  let isHot := ctx.input.isMouseOver fullRect
  let ctx := if isHot then ctx.setHot id else ctx

  let mouseJustReleased := ctx.input.mouseReleased .left
  let clicked := isHot && mouseJustReleased
  let newChecked := if clicked then !checked else checked

  -- Draw checkbox box
  let borderColor := if isHot then Style.textBoxFocused else Style.checkboxBorder
  ctx.drawCtx.fillRoundedRect boxRect Style.cornerRadius Style.checkboxBg
  ctx.drawCtx.strokeRoundedRect boxRect Style.cornerRadius borderColor Style.borderWidth

  if newChecked then
    let inset := 4.0
    let innerRect := Rect.mk' (boxRect.x + inset) (boxRect.y + inset)
                              (boxRect.width - 2*inset) (boxRect.height - 2*inset)
    ctx.drawCtx.fillRoundedRect innerRect 2.0 Style.checkboxCheck

  -- Draw label
  let labelPos : Point := ⟨pos.x + boxSize + 8.0, pos.y + boxSize - 4.0⟩
  ctx.drawCtx.fillText labelText labelPos ctx.font Style.labelText

  pure (newChecked, ctx)

/-- Text box widget. Returns (currentText, updatedContext).
    The text is only updated when the widget is active. -/
def textBox (ctx : UIContext) (id : String) (text : String) (rect : Rect) : IO (String × UIContext) := do
  let isHot := ctx.input.isMouseOver rect
  let wasActive := ctx.isActive id
  let ctx := if isHot then ctx.setHot id else ctx

  -- Click to focus
  let mouseJustPressed := ctx.input.mousePressed .left
  let ctx := if isHot && mouseJustPressed then
    ctx.setActive id |>.setTextEdit text text.length
  else ctx

  -- Click elsewhere to unfocus
  let ctx := if wasActive && mouseJustPressed && !isHot then
    ctx.clearActive
  else ctx

  let isActive := ctx.isActive id

  -- Handle text input if active
  let (currentText, ctx) := if isActive then
    let buf := ctx.textEditBuffer
    let cursor := ctx.textEditCursor
    let newChars := ctx.input.textInput

    -- Process each character
    let (newBuf, newCursor) := newChars.foldl (fun (buf, cur) c =>
      if c.toNat == 8 then  -- Backspace
        if cur > 0 then
          let before := buf.take (cur - 1)
          let after := buf.drop cur
          (before ++ after, cur - 1)
        else (buf, cur)
      else if c.toNat == 127 then  -- Delete
        if cur < buf.length then
          let before := buf.take cur
          let after := buf.drop (cur + 1)
          (before ++ after, cur)
        else (buf, cur)
      else if c.toNat >= 32 && c.toNat < 127 then  -- Printable ASCII
        let before := buf.take cur
        let after := buf.drop cur
        (before ++ c.toString ++ after, cur + 1)
      else (buf, cur)
    ) (buf, cursor)

    (newBuf, ctx.setTextEdit newBuf newCursor)
  else
    (text, ctx)

  -- Draw text box
  let borderColor := if isActive then Style.textBoxFocused
                     else if isHot then Style.checkboxBorder
                     else Style.textBoxBorder
  ctx.drawCtx.fillRoundedRect rect Style.cornerRadius Style.textBoxBg
  ctx.drawCtx.strokeRoundedRect rect Style.cornerRadius borderColor Style.borderWidth

  -- Draw text
  let displayText := if isActive then ctx.textEditBuffer else currentText
  let textPos : Point := ⟨rect.x + Style.padding, rect.y + rect.height - Style.padding - 2.0⟩
  ctx.drawCtx.fillText displayText textPos ctx.font Style.textBoxText

  -- Draw cursor if active (blinking)
  if isActive && (ctx.frameCount / 30) % 2 == 0 then
    let cursorText := displayText.take ctx.textEditCursor
    let (cursorX, _) ← ctx.font.measureText cursorText
    let cursorRect := Rect.mk' (rect.x + Style.padding + cursorX)
                               (rect.y + Style.padding)
                               2.0
                               (rect.height - 2.0 * Style.padding)
    ctx.drawCtx.fillRect cursorRect Style.textBoxCursor

  pure (if isActive then ctx.textEditBuffer else currentText, ctx)

/-- Slider widget (horizontal). Returns (newValue, updatedContext). -/
def slider (ctx : UIContext) (id : String) (value : Float) (minVal maxVal : Float) (rect : Rect)
    : IO (Float × UIContext) := do
  let isHot := ctx.input.isMouseOver rect
  let ctx := if isHot then ctx.setHot id else ctx

  -- Drag to change value
  let mouseDown := ctx.input.mouseDown .left
  let mouseJustPressed := ctx.input.mousePressed .left

  let ctx := if isHot && mouseJustPressed then ctx.setActive id else ctx
  let isActive := ctx.isActive id

  -- Calculate new value from mouse position
  let newValue := if isActive && mouseDown then
    let relX := ctx.input.mouseX - rect.x
    let t := relX / rect.width
    let t := if t < 0.0 then 0.0 else if t > 1.0 then 1.0 else t
    minVal + t * (maxVal - minVal)
  else
    value

  -- Release
  let mouseJustReleased := ctx.input.mouseReleased .left
  let ctx := if isActive && mouseJustReleased then ctx.clearActive else ctx

  -- Draw track
  let trackHeight := 4.0
  let trackY := rect.y + (rect.height - trackHeight) / 2.0
  let trackRect := Rect.mk' rect.x trackY rect.width trackHeight
  ctx.drawCtx.fillRoundedRect trackRect 2.0 Style.sliderTrack

  -- Draw fill
  let fillT := (newValue - minVal) / (maxVal - minVal)
  let fillWidth := rect.width * fillT
  let fillRect := Rect.mk' rect.x trackY fillWidth trackHeight
  ctx.drawCtx.fillRoundedRect fillRect 2.0 Style.sliderFill

  -- Draw handle
  let handleRadius := 8.0
  let handleX := rect.x + fillWidth
  let handleY := rect.y + rect.height / 2.0
  let handleColor := if isActive then Style.buttonActive
                     else if isHot then Style.buttonHover
                     else Style.sliderHandle
  ctx.drawCtx.fillCircle ⟨handleX, handleY⟩ handleRadius handleColor
  ctx.drawCtx.strokeCircle ⟨handleX, handleY⟩ handleRadius Style.checkboxBorder 1.0

  pure (newValue, ctx)

end Afferent.UI

/-
  Canopy Modal Widget
  Overlay dialog with backdrop for modals, alerts, and confirmations.
-/
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Widget.Label

namespace Afferent.Canopy

open Afferent.Arbor

/-- Extended state for modal widgets. -/
structure ModalState extends WidgetState where
  isOpen : Bool := false
deriving Repr, BEq, Inhabited

namespace Modal

/-- Dimensions for modal rendering. -/
structure Dimensions where
  minWidth : Float := 400.0
  maxWidth : Float := 600.0
  padding : Float := 24.0
  cornerRadius : Float := 8.0
  backdropOpacity : Float := 0.5
  headerHeight : Float := 48.0
  closeButtonSize : Float := 24.0
deriving Repr, Inhabited

/-- Default modal dimensions. -/
def defaultDimensions : Dimensions := {}

/-- Build an X close button path. -/
def closeButtonPath (x y size : Float) : Arbor.Path :=
  let half := size / 2
  let p1 : Arbor.Point := ⟨x - half, y - half⟩
  let p2 : Arbor.Point := ⟨x + half, y + half⟩
  let p3 : Arbor.Point := ⟨x - half, y + half⟩
  let p4 : Arbor.Point := ⟨x + half, y - half⟩
  Arbor.Path.empty
    |>.moveTo p1
    |>.lineTo p2
    |>.moveTo p3
    |>.lineTo p4

/-- Custom spec for close button (X icon). -/
def closeButtonSpec (theme : Theme) (isHovered : Bool) (dims : Dimensions := defaultDimensions) : CustomSpec := {
  measure := fun _ _ => (dims.closeButtonSize, dims.closeButtonSize)
  collect := fun layout =>
    let rect := layout.contentRect
    let centerX := rect.x + rect.width / 2
    let centerY := rect.y + rect.height / 2
    let iconSize := dims.closeButtonSize * 0.35
    let path := closeButtonPath centerX centerY iconSize
    let color := if isHovered then theme.primary.foreground else theme.textMuted
    #[.strokePath path color 2.0]
  draw := none
}

end Modal

/-- Build a modal backdrop (full-screen semi-transparent overlay).
    - `name`: Widget name for click detection
    - `theme`: Theme for styling
    - `dims`: Dimension configuration
-/
def modalBackdropVisual (name : String) (_theme : Theme)
    (dims : Modal.Dimensions := {}) : WidgetBuilder := do
  let wid ← freshId
  let style : BoxStyle := {
    backgroundColor := some (Tincture.Color.black.withAlpha dims.backdropOpacity)
    width := .percent 1.0
    height := .percent 1.0
    position := .absolute
    top := some 0
    left := some 0
  }
  pure (.rect wid (some name) style)

/-- Build a modal header with title and close button.
    - `closeName`: Widget name for close button (for click detection)
    - `title`: Modal title text
    - `theme`: Theme for styling
    - `closeHovered`: Whether close button is hovered
    - `dims`: Dimension configuration
-/
def modalHeaderVisual (closeName : String) (title : String)
    (theme : Theme) (closeHovered : Bool) (dims : Modal.Dimensions := {}) : WidgetBuilder := do
  let headerStyle : BoxStyle := {
    borderColor := some theme.panel.border
    borderWidth := 0  -- Just bottom border via divider
    padding := Trellis.EdgeInsets.symmetric dims.padding (dims.padding * 0.6)
    minHeight := some dims.headerHeight
  }

  let wid ← freshId
  let props : Trellis.FlexContainer := {
    Trellis.FlexContainer.row 0 with
    alignItems := .center
    justifyContent := .spaceBetween
  }

  let titleWidget ← heading3 title theme

  -- Close button with hover state
  let closeButtonBg := if closeHovered
    then theme.secondary.backgroundHover
    else Tincture.Color.transparent
  let closeButtonStyle : BoxStyle := {
    backgroundColor := some closeButtonBg
    cornerRadius := dims.closeButtonSize / 2
    minWidth := some dims.closeButtonSize
    minHeight := some dims.closeButtonSize
  }
  let closeButtonWid ← freshId
  let closeButtonProps : Trellis.FlexContainer := {
    direction := .row
    alignItems := .center
    justifyContent := .center
  }
  let closeIcon ← custom (Modal.closeButtonSpec theme closeHovered dims) {
    minWidth := some dims.closeButtonSize
    minHeight := some dims.closeButtonSize
  }
  let closeButton : Widget := .flex closeButtonWid (some closeName) closeButtonProps closeButtonStyle #[closeIcon]

  pure (.flex wid none props headerStyle #[titleWidget, closeButton])

/-- Build a modal dialog box with title, content, and close button.
    - `name`: Widget name for the dialog (for click detection)
    - `closeName`: Widget name for close button
    - `title`: Modal title text
    - `theme`: Theme for styling
    - `closeHovered`: Whether close button is hovered
    - `dims`: Dimension configuration
    - `content`: Content widget builder
-/
def modalDialogVisual (name : String) (closeName : String) (title : String)
    (theme : Theme) (closeHovered : Bool) (dims : Modal.Dimensions := {})
    (content : WidgetBuilder) : WidgetBuilder := do
  -- Header
  let header ← modalHeaderVisual closeName title theme closeHovered dims

  -- Divider
  let dividerStyle : BoxStyle := {
    backgroundColor := some theme.panel.border
    width := .percent 1.0
    height := .length 1.0
  }
  let dividerWid ← freshId
  let divider : Widget := .rect dividerWid none dividerStyle

  -- Content area
  let contentWidget ← content
  let contentStyle : BoxStyle := {
    padding := Trellis.EdgeInsets.uniform dims.padding
    flexItem := some (Trellis.FlexItem.growing 1)
  }
  let contentWid ← freshId
  let contentProps : Trellis.FlexContainer := {
    direction := .column
    gap := 0
  }
  let contentPanel : Widget := .flex contentWid none contentProps contentStyle #[contentWidget]

  -- Dialog box
  let dialogStyle : BoxStyle := {
    backgroundColor := some theme.panel.background
    borderColor := some theme.panel.border
    borderWidth := 1
    cornerRadius := dims.cornerRadius
    minWidth := some dims.minWidth
    maxWidth := some dims.maxWidth
  }
  let dialogWid ← freshId
  let dialogProps : Trellis.FlexContainer := {
    direction := .column
    gap := 0
  }

  pure (.flex dialogWid (some name) dialogProps dialogStyle #[header, divider, contentPanel])

/-- Build a complete modal with backdrop + centered dialog.
    - `name`: Base widget name for the modal
    - `backdropName`: Widget name for backdrop (for click-outside detection)
    - `closeName`: Widget name for close button
    - `title`: Modal title text
    - `isOpen`: Whether modal is open (returns empty widget if false)
    - `theme`: Theme for styling
    - `closeHovered`: Whether close button is hovered
    - `dims`: Dimension configuration
    - `content`: Content widget builder
-/
def modalVisual (name : String) (backdropName : String) (closeName : String)
    (title : String) (isOpen : Bool) (theme : Theme)
    (closeHovered : Bool := false) (dims : Modal.Dimensions := {})
    (content : WidgetBuilder) : WidgetBuilder := do
  if !isOpen then
    -- Return empty spacer when closed
    spacer 0 0
  else
    -- Backdrop
    let backdrop ← modalBackdropVisual backdropName theme dims

    -- Dialog
    let dialog ← modalDialogVisual name closeName title theme closeHovered dims content

    -- Centering container (full-screen flex that centers the dialog)
    let centerStyle : BoxStyle := {
      width := .percent 1.0
      height := .percent 1.0
      position := .absolute
      top := some 0
      left := some 0
    }
    let centerWid ← freshId
    let centerProps : Trellis.FlexContainer := {
      direction := .column
      justifyContent := .center
      alignItems := .center
    }
    let centerContainer : Widget := .flex centerWid none centerProps centerStyle #[dialog]

    -- Outer container (absolutely positioned full-screen overlay)
    -- Contains backdrop and centered dialog, both also absolute
    let outerWid ← freshId
    let outerProps : Trellis.FlexContainer := {
      direction := .column
      gap := 0
    }
    let outerStyle : BoxStyle := {
      width := .percent 1.0
      height := .percent 1.0
      position := .absolute
      top := some 0
      left := some 0
    }

    pure (.flex outerWid none outerProps outerStyle #[backdrop, centerContainer])

end Afferent.Canopy

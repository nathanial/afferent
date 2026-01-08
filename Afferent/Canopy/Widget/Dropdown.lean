/-
  Canopy Dropdown Widget
  Single-selection dropdown/select widget.
-/
import Afferent.Canopy.Core
import Afferent.Canopy.Theme

namespace Afferent.Canopy

open Afferent.Arbor

/-- Extended state for dropdown widgets. -/
structure DropdownState extends WidgetState where
  selectedIndex : Nat := 0
  isOpen : Bool := false
  hoveredOption : Option Nat := none
deriving Repr, BEq, Inhabited

namespace Dropdown

/-- Dimensions for dropdown rendering. -/
structure Dimensions where
  minWidth : Float := 180.0
  itemHeight : Float := 32.0
  padding : Float := 10.0
  arrowWidth : Float := 20.0
deriving Repr, Inhabited

/-- Default dropdown dimensions. -/
def defaultDimensions : Dimensions := {}

/-- Build a chevron path (V shape pointing down or up). -/
def chevronPath (x y : Float) (isOpen : Bool) : Arbor.Path :=
  let chevronSize : Float := 6.0
  let halfSize := chevronSize / 2
  let (y1, y2) := if isOpen then
    (y + halfSize * 0.5, y - halfSize * 0.5)  -- Pointing up
  else
    (y - halfSize * 0.5, y + halfSize * 0.5)  -- Pointing down
  let p1 : Arbor.Point := ⟨x - chevronSize, y1⟩
  let p2 : Arbor.Point := ⟨x, y2⟩
  let p3 : Arbor.Point := ⟨x + chevronSize, y1⟩
  Arbor.Path.empty
    |>.moveTo p1
    |>.lineTo p2
    |>.lineTo p3

/-- Custom spec for dropdown arrow (downward chevron). -/
def arrowSpec (isOpen : Bool) (theme : Theme) (dims : Dimensions := defaultDimensions) : CustomSpec := {
  measure := fun _ _ => (dims.arrowWidth, dims.itemHeight)
  collect := fun layout =>
    let rect := layout.contentRect
    let cmds : Array RenderCommand := #[]
    let centerX := rect.x + rect.width / 2
    let centerY := rect.y + rect.height / 2
    let path := chevronPath centerX centerY isOpen
    cmds.push (.strokePath path theme.textMuted 2.0)
  draw := none
}

/-- Build a checkmark path for menu items. -/
def checkmarkPath (x y : Float) : Arbor.Path :=
  let p1 : Arbor.Point := ⟨x - 5, y⟩
  let p2 : Arbor.Point := ⟨x - 1, y + 4⟩
  let p3 : Arbor.Point := ⟨x + 6, y - 4⟩
  Arbor.Path.empty
    |>.moveTo p1
    |>.lineTo p2
    |>.lineTo p3

/-- Custom spec for checkmark in menu item. -/
def checkmarkSpec (theme : Theme) : CustomSpec := {
  measure := fun _ _ => (20.0, 20.0)
  collect := fun layout =>
    let rect := layout.contentRect
    let cmds : Array RenderCommand := #[]
    let centerX := rect.x + rect.width / 2
    let centerY := rect.y + rect.height / 2
    let path := checkmarkPath centerX centerY
    cmds.push (.strokePath path theme.primary.foreground 2.0)
  draw := none
}

end Dropdown

/-- Build a visual dropdown trigger button.
    - `name`: Widget name for hit testing (should be the trigger name)
    - `selectedText`: Text to display (the selected option)
    - `isOpen`: Whether dropdown is currently open
    - `theme`: Theme for styling
    - `state`: Widget interaction state (hover, focus, etc.)
-/
def dropdownTriggerVisual (name : String) (selectedText : String) (isOpen : Bool)
    (theme : Theme) (state : WidgetState := {}) : WidgetBuilder := do
  let dims := Dropdown.defaultDimensions
  let bgColor := if state.hovered || isOpen then theme.input.backgroundHover else theme.input.background
  let borderColor := if state.focused then theme.input.borderFocused else theme.input.border

  let triggerStyle : BoxStyle := {
    backgroundColor := some bgColor
    borderColor := some borderColor
    borderWidth := 1
    cornerRadius := theme.cornerRadius
    padding := Trellis.EdgeInsets.symmetric dims.padding (dims.padding * 0.6)
    minWidth := some dims.minWidth
    minHeight := some dims.itemHeight
  }

  let wid ← freshId
  let props : Trellis.FlexContainer := {
    Trellis.FlexContainer.row 0 with
    alignItems := .center
    justifyContent := .spaceBetween
  }

  let textWidget ← text' selectedText theme.font theme.text .left
  let arrowWidget ← custom (Dropdown.arrowSpec isOpen theme dims) {
    minWidth := some dims.arrowWidth
    minHeight := some dims.itemHeight
  }

  pure (.flex wid (some name) props triggerStyle #[textWidget, arrowWidget])

/-- Build a visual dropdown menu item.
    - `name`: Widget name for hit testing (should be option-specific)
    - `optionText`: Text to display
    - `isSelected`: Whether this option is currently selected
    - `isHovered`: Whether this option is being hovered
    - `isFirst`: Whether this is the first item (for rounded corners)
    - `isLast`: Whether this is the last item (for rounded corners)
    - `theme`: Theme for styling
-/
def dropdownMenuItemVisual (name : String) (optionText : String)
    (isSelected : Bool) (isHovered : Bool) (isFirst : Bool) (isLast : Bool)
    (theme : Theme) : WidgetBuilder := do
  let dims := Dropdown.defaultDimensions
  let bgColor := if isHovered then theme.input.backgroundHover
    else if isSelected then theme.primary.background.withAlpha 0.15
    else theme.input.background
  let textColor := if isSelected then theme.primary.foreground else theme.text

  -- Calculate corner radius for first/last items
  let cornerRadius := if isFirst && isLast then theme.cornerRadius
    else if isFirst then 0  -- Actually want top corners only, but we'll simplify
    else if isLast then 0   -- Actually want bottom corners only
    else 0

  let itemStyle : BoxStyle := {
    backgroundColor := some bgColor
    cornerRadius := cornerRadius
    padding := Trellis.EdgeInsets.symmetric dims.padding (dims.padding * 0.5)
    minWidth := some dims.minWidth
    minHeight := some dims.itemHeight
  }

  let wid ← freshId
  let props : Trellis.FlexContainer := {
    Trellis.FlexContainer.row 8 with
    alignItems := .center
  }

  let textWidget ← text' optionText theme.font textColor .left

  if isSelected then
    let checkWidget ← custom (Dropdown.checkmarkSpec theme) {
      minWidth := some 20
      minHeight := some 20
    }
    pure (.flex wid (some name) props itemStyle #[checkWidget, textWidget])
  else
    -- Add spacer for alignment when no checkmark
    let spacerWidget ← spacer 20 0
    pure (.flex wid (some name) props itemStyle #[spacerWidget, textWidget])

/-- Build a complete visual dropdown widget.
    - `name`: Base widget name for the dropdown
    - `triggerName`: Widget name for the trigger button
    - `optionNameFn`: Function to generate option widget names from index
    - `options`: Array of option strings
    - `selectedIndex`: Currently selected option index
    - `isOpen`: Whether dropdown menu is open
    - `hoveredOption`: Currently hovered option index (if any)
    - `theme`: Theme for styling
    - `state`: Widget interaction state for trigger
-/
def dropdownVisual (name : String) (triggerName : String)
    (optionNameFn : Nat → String) (options : Array String) (selectedIndex : Nat)
    (isOpen : Bool) (hoveredOption : Option Nat) (theme : Theme)
    (state : WidgetState := {}) : WidgetBuilder := do
  let dims := Dropdown.defaultDimensions
  let selectedText := options.getD selectedIndex "Select..."

  let trigger ← dropdownTriggerVisual triggerName selectedText isOpen theme state

  if isOpen then
    -- Build menu items
    let mut menuItems : Array Widget := #[]
    for i in [:options.size] do
      let optText := options.getD i ""
      let isSel := i == selectedIndex
      let isHov := hoveredOption == some i
      let isFirst := i == 0
      let isLast := i == options.size - 1
      let itemWidget ← dropdownMenuItemVisual (optionNameFn i) optText isSel isHov isFirst isLast theme
      menuItems := menuItems.push itemWidget

    -- Menu container with border
    let menuOffset := dims.itemHeight + 4
    let menuHeight := dims.itemHeight * options.size.toFloat
    let menuStyle : BoxStyle := {
      backgroundColor := some theme.input.background
      borderColor := some theme.input.border
      borderWidth := 1
      cornerRadius := theme.cornerRadius
      width := .percent 1.0
      height := .length menuHeight
      position := .absolute
      top := some menuOffset
      left := some 0
      -- No padding - items handle their own padding
    }

    let menuWid ← freshId
    let menuProps : Trellis.FlexContainer := {
      direction := .column
      gap := 0
    }
    let menu : Widget := .flex menuWid none menuProps menuStyle menuItems

    -- Outer container (column with trigger + menu)
    let outerWid ← freshId
    let outerProps : Trellis.FlexContainer := {
      direction := .column
      gap := 0
    }
    pure (.flex outerWid (some name) outerProps {} #[trigger, menu])
  else
    -- Just the trigger when closed
    let outerWid ← freshId
    let outerProps : Trellis.FlexContainer := {
      direction := .column
      gap := 0
    }
    pure (.flex outerWid (some name) outerProps {} #[trigger])

end Afferent.Canopy

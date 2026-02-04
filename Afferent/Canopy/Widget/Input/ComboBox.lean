/-
  Canopy ComboBox Widget
  Dropdown with text input for filtering/search - combines TextInput and Dropdown patterns.
-/
import Reactive
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Widget.Input.TextInput
import Afferent.Canopy.Widget.Input.Dropdown
import Afferent.Canopy.Reactive.Component

namespace Afferent.Canopy

open Afferent.Arbor hiding Event

/-- Configuration for ComboBox widget. -/
structure ComboBoxConfig where
  minWidth : Float := 200.0
  itemHeight : Float := 32.0
  maxVisibleItems : Nat := 5       -- Max items before scrolling
  allowFreeText : Bool := false    -- Allow values not in options list
  caseSensitive : Bool := false    -- Case-sensitive filtering
  cornerRadius : Float := 6.0
deriving Repr, Inhabited

/-- Extended state for ComboBox widgets. -/
structure ComboBoxState where
  inputText : String := ""
  cursor : Nat := 0
  cursorPixelX : Float := 0
  isOpen : Bool := false
  hoveredOption : Option Nat := none
  focused : Bool := false
deriving Repr, BEq, Inhabited

namespace ComboBoxState

/-- Insert a character at cursor position. -/
def insertChar (s : ComboBoxState) (c : Char) : ComboBoxState :=
  let before := s.inputText.take s.cursor
  let after := s.inputText.drop s.cursor
  { s with
    inputText := before ++ c.toString ++ after
    cursor := s.cursor + 1 }

/-- Delete character before cursor (backspace). -/
def deleteBackward (s : ComboBoxState) : ComboBoxState :=
  if s.cursor > 0 then
    let before := s.inputText.take (s.cursor - 1)
    let after := s.inputText.drop s.cursor
    { s with
      inputText := before ++ after
      cursor := s.cursor - 1 }
  else s

/-- Delete character at cursor (delete key). -/
def deleteForward (s : ComboBoxState) : ComboBoxState :=
  if s.cursor < s.inputText.length then
    let before := s.inputText.take s.cursor
    let after := s.inputText.drop (s.cursor + 1)
    { s with inputText := before ++ after }
  else s

/-- Move cursor left. -/
def moveCursorLeft (s : ComboBoxState) : ComboBoxState :=
  if s.cursor > 0 then { s with cursor := s.cursor - 1 }
  else s

/-- Move cursor right. -/
def moveCursorRight (s : ComboBoxState) : ComboBoxState :=
  if s.cursor < s.inputText.length then { s with cursor := s.cursor + 1 }
  else s

/-- Move cursor to start. -/
def moveCursorStart (s : ComboBoxState) : ComboBoxState :=
  { s with cursor := 0 }

/-- Move cursor to end. -/
def moveCursorEnd (s : ComboBoxState) : ComboBoxState :=
  { s with cursor := s.inputText.length }

/-- Set text value (for selection). -/
def setText (s : ComboBoxState) (text : String) : ComboBoxState :=
  { s with inputText := text, cursor := text.length }

end ComboBoxState

namespace ComboBox

/-- Check if a string contains a substring. -/
private def containsSubstr (s : String) (sub : String) : Bool :=
  if sub.isEmpty then true
  else if sub.length > s.length then false
  else
    let subLen := sub.length
    let maxStart := s.length - subLen
    (List.range (maxStart + 1)).any fun i =>
      (s.drop i).take subLen == sub

/-- Filter options based on query text. Returns array of (originalIndex, optionText). -/
def filterOptions (options : Array String) (query : String) (caseSensitive : Bool)
    : Array (Nat × String) :=
  if query.isEmpty then
    options.mapIdx fun i opt => (i, opt)
  else
    let queryNorm := if caseSensitive then query else query.toLower
    let result := options.foldl (init := (#[], 0)) fun (acc, idx) opt =>
      let optNorm := if caseSensitive then opt else opt.toLower
      if containsSubstr optNorm queryNorm then
        (acc.push (idx, opt), idx + 1)
      else
        (acc, idx + 1)
    result.1

/-- Handle key press for combo box. -/
def handleKeyPress (e : KeyEvent) (state : ComboBoxState) : ComboBoxState :=
  if e.modifiers.cmd then
    match e.key with
    | .left => state.moveCursorStart
    | .right => state.moveCursorEnd
    | _ => state
  else
    match e.key with
    | .char c => (state.insertChar c)
    | .space => (state.insertChar ' ')
    | .backspace => state.deleteBackward
    | .delete => state.deleteForward
    | .left => state.moveCursorLeft
    | .right => state.moveCursorRight
    | .home => state.moveCursorStart
    | .«end» => state.moveCursorEnd
    | _ => state

/-- Compute cursor pixel position by measuring text before cursor. -/
def computeCursorPixelX (font : Afferent.Font) (state : ComboBoxState) : IO ComboBoxState := do
  let textBeforeCursor := state.inputText.take state.cursor
  let (width, _) ← font.measureText textBeforeCursor
  pure { state with cursorPixelX := width }

/-- Custom spec for combo box input text with cursor. -/
def inputSpec (displayText : String) (placeholder : String) (showPlaceholder : Bool)
    (cursorPixelX : Float) (focused : Bool) (theme : Theme) : CustomSpec := {
  measure := fun _ _ =>
    let lineHeight := theme.font.lineHeight
    let height := lineHeight + 4
    (0, height)
  collect := fun layout =>
    let rect := layout.contentRect
    let text := if showPlaceholder then placeholder else displayText
    let textColor := if showPlaceholder then theme.textMuted else theme.text
    let lineHeight := theme.font.lineHeight
    let ascender := theme.font.ascender
    let verticalOffset := (rect.height - lineHeight) / 2
    let textY := rect.y + verticalOffset + ascender
    RenderM.build do
      RenderM.fillText text rect.x textY theme.font textColor
      if focused then
        let cursorX := rect.x + cursorPixelX
        let cursorY := rect.y + verticalOffset
        let cursorH := lineHeight
        RenderM.fillRect (Arbor.Rect.mk' cursorX cursorY 2 cursorH) theme.focusRing 0
  draw := none
}

end ComboBox

/-- Build the visual representation of a combo box input (trigger area).
    - `name`: Widget name for hit testing
    - `theme`: Theme for styling
    - `state`: Current combo box state
    - `placeholder`: Placeholder text when empty
    - `config`: ComboBox configuration
-/
def comboBoxInputVisual (name : String) (theme : Theme)
    (state : ComboBoxState) (placeholder : String)
    (config : ComboBoxConfig := {}) : WidgetBuilder := do
  let colors := theme.input
  let bgColor := if state.focused then colors.backgroundHover else colors.background
  let borderColor := if state.focused then colors.borderFocused else colors.border

  let verticalPadding := theme.padding * 0.5 * 2
  let minHeight := theme.font.lineHeight + verticalPadding + 4

  let containerStyle : BoxStyle := {
    backgroundColor := some bgColor
    borderColor := some borderColor
    borderWidth := if state.focused then 2 else 1
    cornerRadius := config.cornerRadius
    padding := Trellis.EdgeInsets.symmetric theme.padding (theme.padding * 0.5)
    minWidth := some config.minWidth
    minHeight := some minHeight
  }

  let showPlaceholder := state.inputText.isEmpty && !state.focused

  -- Text input area (grows to fill)
  let inputContent ← custom (ComboBox.inputSpec state.inputText placeholder showPlaceholder
      state.cursorPixelX state.focused theme) {
    flexItem := some (Trellis.FlexItem.growing 1)
  }

  -- Dropdown arrow
  let arrowWidget ← custom (Dropdown.arrowSpec state.isOpen theme) {
    minWidth := some 20
    minHeight := some config.itemHeight
  }

  let wid ← freshId
  let props : Trellis.FlexContainer := {
    direction := .row
    alignItems := .center
    justifyContent := .spaceBetween
    gap := 4
  }
  pure (.flex wid (some name) props containerStyle #[inputContent, arrowWidget])

/-- Build a visual combo box menu item.
    - `name`: Widget name for hit testing
    - `optionText`: Text to display
    - `isHovered`: Whether this option is being hovered
    - `isFirst`: Whether this is the first item
    - `isLast`: Whether this is the last item
    - `theme`: Theme for styling
-/
def comboBoxMenuItemVisual (name : String) (optionText : String)
    (isHovered : Bool) (isFirst : Bool) (isLast : Bool)
    (theme : Theme) (config : ComboBoxConfig := {}) : WidgetBuilder := do
  let bgColor := if isHovered then theme.input.backgroundHover else theme.input.background
  let textColor := theme.text

  let cornerRadius := if isFirst && isLast then config.cornerRadius
    else if isFirst then 0
    else if isLast then 0
    else 0

  let itemStyle : BoxStyle := {
    backgroundColor := some bgColor
    cornerRadius := cornerRadius
    padding := Trellis.EdgeInsets.symmetric theme.padding (theme.padding * 0.5)
    minWidth := some config.minWidth
    minHeight := some config.itemHeight
  }

  let wid ← freshId
  let props : Trellis.FlexContainer := {
    Trellis.FlexContainer.row 8 with
    alignItems := .center
  }

  let textWidget ← text' optionText theme.font textColor .left
  pure (.flex wid (some name) props itemStyle #[textWidget])

/-- Build a complete visual combo box widget.
    - `containerName`: Base widget name for the container
    - `inputName`: Widget name for the input area
    - `optionNameFn`: Function to generate option widget names from index
    - `filteredOptions`: Array of (originalIndex, optionText) for filtered options
    - `state`: Current combo box state
    - `theme`: Theme for styling
    - `placeholder`: Placeholder text
    - `config`: ComboBox configuration
-/
def comboBoxVisual (containerName : String) (inputName : String)
    (optionNameFn : Nat → String)
    (filteredOptions : Array (Nat × String)) (state : ComboBoxState)
    (theme : Theme) (placeholder : String)
    (config : ComboBoxConfig := {}) : WidgetBuilder := do
  let input ← comboBoxInputVisual inputName theme state placeholder config

  if state.isOpen && !filteredOptions.isEmpty then
    -- Build menu items
    let mut menuItems : Array Widget := #[]
    for i in [:filteredOptions.size] do
      let (origIdx, optText) := filteredOptions.getD i (0, "")
      let isHov := state.hoveredOption == some i
      let isFirst := i == 0
      let isLast := i == filteredOptions.size - 1
      let itemWidget ← comboBoxMenuItemVisual (optionNameFn origIdx) optText isHov isFirst isLast theme config
      menuItems := menuItems.push itemWidget

    -- Calculate menu height (capped by maxVisibleItems)
    let visibleItems := min filteredOptions.size config.maxVisibleItems
    let menuHeight := config.itemHeight * visibleItems.toFloat
    let menuOffset := config.itemHeight + 4

    let menuStyle : BoxStyle := {
      backgroundColor := some theme.input.background
      borderColor := some theme.input.border
      borderWidth := 1
      cornerRadius := config.cornerRadius
      width := .percent 1.0
      height := .length menuHeight
      position := .absolute
      layer := .overlay
      top := some menuOffset
      left := some 0
    }

    let menuWid ← freshId
    let menuProps : Trellis.FlexContainer := {
      direction := .column
      gap := 0
    }
    let menu : Widget := .flex menuWid none menuProps menuStyle menuItems

    -- Outer container
    let outerWid ← freshId
    let outerProps : Trellis.FlexContainer := {
      direction := .column
      gap := 0
    }
    pure (.flex outerWid (some containerName) outerProps {} #[input, menu])
  else
    -- Just the input when closed
    let outerWid ← freshId
    let outerProps : Trellis.FlexContainer := {
      direction := .column
      gap := 0
    }
    pure (.flex outerWid (some containerName) outerProps {} #[input])

/-! ## Reactive ComboBox Components (FRP-based) -/

open Reactive Reactive.Host
open Afferent.Canopy.Reactive

/-- ComboBox result - events and dynamics. -/
structure ComboBoxResult where
  onChange : Reactive.Event Spider String   -- Text input changes
  onSelect : Reactive.Event Spider String   -- Option selected from dropdown
  value : Reactive.Dynamic Spider String    -- Current input value
  isOpen : Reactive.Dynamic Spider Bool

/-- Input event type for combo box state machine. -/
private inductive ComboBoxEvent
  | key (kd : KeyData)
  | selectOption (text : String)
  | toggleOpen
  | close
  | setHover (idx : Option Nat)

/-- Create a reactive combo box component using WidgetM.
    Uses the default font from WidgetM context (set via createInputs).
    - `options`: Array of option strings to filter/select from
    - `placeholder`: Placeholder text when empty
    - `initialValue`: Initial text value
    - `config`: ComboBox configuration
-/
def comboBox (options : Array String) (placeholder : String := "Type to search...")
    (initialValue : String := "") (config : ComboBoxConfig := {}) : WidgetM ComboBoxResult := do
  let theme ← getThemeW
  let font ← getFontW
  let containerName ← registerComponentW "combobox" (isInteractive := false)
  let inputName ← registerComponentW "combobox-input" (isInput := true)

  -- Register option names (for max options)
  let mut optionNames : Array String := #[]
  for _ in options do
    let name ← registerComponentW "combobox-option"
    optionNames := optionNames.push name
  let optionNameFn (i : Nat) : String := optionNames.getD i ""

  let events ← getEventsW
  let focusedInput := events.registry.focusedInput
  let fireFocusedInput := events.registry.fireFocus

  let inputClicks ← useClick inputName
  let allClicks ← useAllClicks
  let keyEvents ← useKeyboard

  let isFocused ← Dynamic.mapM (· == some inputName) focusedInput

  -- Click input to focus
  let notFocused ← Dynamic.mapM (· != some inputName) focusedInput
  let focusClicks ← Event.gateM notFocused.current inputClicks
  let focusAction ← Event.mapM (fun _ => fireFocusedInput (some inputName)) focusClicks
  performEvent_ focusAction

  -- Initial state
  let initialState : ComboBoxState := {
    inputText := initialValue
    cursor := initialValue.length
    cursorPixelX := 0.0
    isOpen := false
    hoveredOption := none
    focused := false
  }
  let initialState ← SpiderM.liftIO (ComboBox.computeCursorPixelX font initialState)

  -- Map events to unified type
  let gatedKeys ← Event.gateM isFocused.current keyEvents

  -- Escape key closes dropdown
  let escapeEvents ← Event.filterM (fun kd => kd.event.key == .escape) gatedKeys
  let closeOnEscape ← Event.mapM (fun _ => ComboBoxEvent.close) escapeEvents

  -- Regular key input (excluding escape/enter)
  let textKeyEvents ← Event.filterM
    (fun kd => kd.event.key != .escape && kd.event.key != .enter) gatedKeys
  let keyInputEvents ← Event.mapM ComboBoxEvent.key textKeyEvents

  -- Click outside closes dropdown
  let isClickOutside (data : ClickData) : Bool :=
    !hitWidget data containerName && !hitWidget data inputName

  -- Find which option was clicked (if any)
  let findClickedOption (data : ClickData) : Option Nat :=
    (List.range options.size).findSome? fun i =>
      if hitWidget data (optionNameFn i) then some i else none

  let optionClicks ← Event.mapMaybeM findClickedOption allClicks
  let selectEvents ← Event.mapM
    (fun idx => ComboBoxEvent.selectOption (options.getD idx "")) optionClicks

  let outsideClicks ← Event.filterM isClickOutside allClicks
  let closeOnOutside ← Event.mapM (fun _ => ComboBoxEvent.close) outsideClicks

  -- Hover tracking for options
  let mut optionHoverEvents : Array (Reactive.Event Spider ComboBoxEvent) := #[]
  for i in [:options.size] do
    let hoverChanges ← Event.selectM events.hoverFan (optionNameFn i)
    let hoverEvent ← Event.mapM
      (fun hovered => ComboBoxEvent.setHover (if hovered then some i else none))
      hoverChanges
    optionHoverEvents := optionHoverEvents.push hoverEvent

  let ctx ← SpiderM.getTimelineCtx
  let neverHover ← SpiderM.liftIO (Reactive.Event.never ctx)
  let hoverEvents ← match optionHoverEvents.toList with
    | [] => pure neverHover
    | evts => Event.leftmostM evts

  -- Combine all events
  let allEvents ← Event.leftmostM
    [keyInputEvents, selectEvents, closeOnEscape, closeOnOutside, hoverEvents]

  -- State machine
  let comboState ← Reactive.foldDynM
    (fun (event : ComboBoxEvent) (state : ComboBoxState) => SpiderM.liftIO do
      match event with
      | .key kd =>
        let updated := ComboBox.handleKeyPress kd.event state
        let updated := { updated with isOpen := true }  -- Open on typing
        ComboBox.computeCursorPixelX font updated
      | .selectOption text =>
        let updated := state.setText text
        let updated := { updated with isOpen := false, hoveredOption := none }
        ComboBox.computeCursorPixelX font updated
      | .toggleOpen =>
        pure { state with isOpen := !state.isOpen }
      | .close =>
        pure { state with isOpen := false, hoveredOption := none }
      | .setHover idx =>
        pure { state with hoveredOption := idx })
    initialState allEvents

  -- Extract dynamics
  let value ← Dynamic.mapM (·.inputText) comboState
  let isOpen ← Dynamic.mapM (·.isOpen) comboState

  -- Extract events
  let stateChanges ← Dynamic.changesM comboState
  let textChanges ← Event.mapMaybeM
    (fun (old, new) => if old.inputText != new.inputText then some new.inputText else none)
    stateChanges
  let onChange := textChanges

  let onSelect ← Event.mapMaybeM
    (fun evt => match evt with
      | .selectOption text => some text
      | _ => none)
    allEvents

  -- Render with dynWidget
  let renderState ← Dynamic.zipWithM (fun s f => (s, f)) comboState focusedInput
  let _ ← dynWidget renderState fun (state, focused) => do
    let isFoc := focused == some inputName
    let stateWithFocus := { state with focused := isFoc }
    let filteredOptions := ComboBox.filterOptions options state.inputText config.caseSensitive
    emit do pure (comboBoxVisual containerName inputName optionNameFn
        filteredOptions stateWithFocus theme placeholder config)

  pure { onChange, onSelect, value, isOpen }

end Afferent.Canopy

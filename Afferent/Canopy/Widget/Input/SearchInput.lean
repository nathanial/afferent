/-
  Canopy SearchInput Widget
  Text input with search icon and clear button.
-/
import Reactive
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Widget.Input.TextInput
import Afferent.Canopy.Reactive.Component

namespace Afferent.Canopy

open Afferent.Arbor hiding Event

namespace SearchInput

/-- Dimensions for search input rendering. -/
structure Dimensions where
  iconSize : Float := 16.0
  iconPadding : Float := 8.0
  clearButtonSize : Float := 18.0
deriving Repr, Inhabited

/-- Default search input dimensions. -/
def defaultDimensions : Dimensions := {}

/-- Create a search icon path (magnifying glass). -/
def searchIconPath (cx cy radius : Float) : Afferent.Path :=
  -- Use built-in circle helper for the lens
  let circle := Afferent.Path.circle ⟨cx, cy⟩ radius
  -- Handle extends from bottom-right of circle at 45 degrees
  let handleStartX := cx + radius * 0.707  -- cos(45°)
  let handleStartY := cy + radius * 0.707  -- sin(45°)
  let handleLen := radius * 0.7
  let handleEndX := handleStartX + handleLen * 0.707
  let handleEndY := handleStartY + handleLen * 0.707
  circle
    |>.moveTo ⟨handleStartX, handleStartY⟩
    |>.lineTo ⟨handleEndX, handleEndY⟩

/-- Create a clear button path (X icon). -/
def clearButtonPath (cx cy halfSize : Float) : Afferent.Path :=
  let p1 : Arbor.Point := ⟨cx - halfSize, cy - halfSize⟩
  let p2 : Arbor.Point := ⟨cx + halfSize, cy + halfSize⟩
  let p3 : Arbor.Point := ⟨cx + halfSize, cy - halfSize⟩
  let p4 : Arbor.Point := ⟨cx - halfSize, cy + halfSize⟩
  Afferent.Path.empty
    |>.moveTo p1
    |>.lineTo p2
    |>.moveTo p3
    |>.lineTo p4

/-- Custom spec for search icon. -/
def searchIconSpec (theme : Theme) (dims : Dimensions := defaultDimensions) : CustomSpec := {
  measure := fun _ _ => (dims.iconSize + dims.iconPadding, dims.iconSize)
  collect := fun layout =>
    let rect := layout.contentRect
    let centerX := rect.x + dims.iconSize / 2
    let centerY := rect.y + rect.height / 2
    let radius := dims.iconSize * 0.35
    let path := searchIconPath centerX centerY radius
    RenderM.build do
      RenderM.strokePath path theme.textMuted 1.5
  draw := none
}

/-- Custom spec for clear button (X icon). -/
def clearButtonSpec (theme : Theme) (isHovered : Bool) (dims : Dimensions := defaultDimensions) : CustomSpec := {
  measure := fun _ _ => (dims.clearButtonSize, dims.clearButtonSize)
  collect := fun layout =>
    let rect := layout.contentRect
    let centerX := rect.x + rect.width / 2
    let centerY := rect.y + rect.height / 2
    let iconSize := dims.clearButtonSize * 0.25
    let path := clearButtonPath centerX centerY iconSize
    let color := if isHovered then theme.text else theme.textMuted
    RenderM.build do
      RenderM.strokePath path color 1.5
  draw := none
}

/-- Custom spec for search input text with cursor (reuses TextInput pattern). -/
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

end SearchInput

/-- Build the visual representation of a search input.
    - `name`: Widget name for the text area
    - `clearName`: Widget name for the clear button
    - `theme`: Theme for styling
    - `state`: Current text input state
    - `placeholder`: Placeholder text when empty
    - `clearHovered`: Whether the clear button is hovered
-/
def searchInputVisual (name : String) (clearName : String) (theme : Theme)
    (state : TextInputState) (placeholder : String) (clearHovered : Bool)
    (dims : SearchInput.Dimensions := {}) : WidgetBuilder := do
  let colors := theme.input
  let bgColor := if state.disabled then colors.backgroundDisabled else colors.background
  let borderColor := if state.focused then colors.borderFocused else colors.border

  let verticalPadding := theme.padding * 0.5 * 2
  let minHeight := theme.font.lineHeight + verticalPadding + 4

  let containerStyle : BoxStyle := {
    backgroundColor := some bgColor
    borderColor := some borderColor
    borderWidth := if state.focused then 2 else 1
    cornerRadius := theme.cornerRadius
    padding := Trellis.EdgeInsets.symmetric (theme.padding * 0.5) (theme.padding * 0.5)
    minWidth := some 200
    minHeight := some minHeight
  }

  -- Search icon on left
  let searchIcon ← custom (SearchInput.searchIconSpec theme dims) {}

  -- Text input area (grows to fill)
  let showPlaceholder := state.value.isEmpty && !state.focused
  let inputContent ← custom (SearchInput.inputSpec state.value placeholder showPlaceholder
      state.cursorPixelX state.focused theme) {
    flexItem := some (Trellis.FlexItem.growing 1)
  }

  -- Clear button (only visible when text is present)
  let clearWidgets ← if state.value.isEmpty then
    pure #[]
  else do
    let clearButtonWid ← freshId
    let clearButtonStyle : BoxStyle := {
      cornerRadius := dims.clearButtonSize / 2
      backgroundColor := if clearHovered then some (theme.textMuted.withAlpha 0.2) else none
    }
    let clearIcon ← custom (SearchInput.clearButtonSpec theme clearHovered dims) {
      minWidth := some dims.clearButtonSize
      minHeight := some dims.clearButtonSize
    }
    let clearButton : Widget := .flex clearButtonWid (some clearName) {
      direction := .row
      alignItems := .center
      justifyContent := .center
    } clearButtonStyle #[clearIcon]
    pure #[clearButton]

  -- Container with horizontal layout
  let wid ← freshId
  let props : Trellis.FlexContainer := {
    direction := .row
    alignItems := .center
    justifyContent := .flexStart
    gap := 4
  }
  pure (.flex wid (some name) props containerStyle (#[searchIcon, inputContent] ++ clearWidgets))

/-! ## Reactive SearchInput Components (FRP-based) -/

open Reactive Reactive.Host
open Afferent.Canopy.Reactive

/-- SearchInput result - events and dynamics. -/
structure SearchInputResult where
  onChange : Reactive.Event Spider String
  onSearch : Reactive.Event Spider String
  onClear : Reactive.Event Spider Unit
  onFocus : Reactive.Event Spider Unit
  onBlur : Reactive.Event Spider Unit
  text : Reactive.Dynamic Spider String
  isFocused : Reactive.Dynamic Spider Bool

/-- Input event type for search input state machine. -/
private inductive SearchInputEvent
  | key (kd : KeyData)
  | clear

/-- Create a reactive search input component using WidgetM.
    Uses the default font from WidgetM context (set via createInputs).
    - `placeholder`: Placeholder text when empty
    - `initialValue`: Initial text value
-/
def searchInput (placeholder : String := "Search...") (initialValue : String := "")
    : WidgetM SearchInputResult := do
  let theme ← getThemeW
  let font ← getFontW
  let name ← registerComponentW "search-input" (isInput := true)
  let clearName ← registerComponentW "search-clear"
  let events ← getEventsW
  let focusedInput := events.registry.focusedInput
  let fireFocusedInput := events.registry.fireFocus

  let clicks ← useClick name
  let clearClicks ← useClick clearName
  let clearHovered ← useHover clearName
  let keyEvents ← useKeyboard

  let isFocused ← Dynamic.mapM (· == some name) focusedInput

  -- Focus/blur events
  let focusChanges ← Dynamic.changesM focusedInput
  let focusEvents ← Event.filterM
    (fun (old, new) => old != some name && new == some name) focusChanges
  let onFocus ← Event.voidM focusEvents
  let blurEvents ← Event.filterM
    (fun (old, new) => old == some name && new != some name) focusChanges
  let onBlur ← Event.voidM blurEvents

  -- Click to focus
  let notFocused ← Dynamic.mapM (· != some name) focusedInput
  let focusClicks ← Event.gateM notFocused.current clicks
  let focusAction ← Event.mapM (fun _ => fireFocusedInput (some name)) focusClicks
  performEvent_ focusAction

  -- Initial state
  let initialState : TextInputState := {
    value := initialValue
    cursor := initialValue.length
    cursorPixelX := 0.0
  }
  let initialState ← SpiderM.liftIO (TextInput.computeCursorPixelX font initialState)

  -- Handle Enter key to trigger search
  let gatedKeys ← Event.gateM isFocused.current keyEvents
  let enterEvents ← Event.filterM (fun kd => kd.event.key == .enter) gatedKeys

  -- Map events to unified type
  let keyInputEvents ← Event.mapM SearchInputEvent.key gatedKeys
  let clearInputEvents ← Event.mapM (fun _ => SearchInputEvent.clear) clearClicks

  -- Combine all input events
  let allInputEvents ← Event.leftmostM [keyInputEvents, clearInputEvents]

  -- State machine for text
  let textState ← Reactive.foldDynM
    (fun (event : SearchInputEvent) (state : TextInputState) => SpiderM.liftIO do
      match event with
      | .key kd =>
        let updated := TextInput.handleKeyPress kd.event state none
        TextInput.computeCursorPixelX font updated
      | .clear =>
        pure { value := "", cursor := 0, cursorPixelX := 0.0 : TextInputState })
    initialState allInputEvents

  -- Extract text changes
  let textChanges ← Dynamic.changesM textState
  let valueChanges ← Event.mapMaybeM
    (fun (old, new) => if old.value != new.value then some new.value else none)
    textChanges
  let onChange := valueChanges

  let text ← Dynamic.mapM (·.value) textState

  -- Search event fires on Enter with current text
  let onSearch ← Event.tagM text.current enterEvents
  let onClear ← Event.voidM clearClicks

  -- Render with dynWidget
  let renderState ← Dynamic.zipWith3M (fun s f ch => (s, f, ch)) textState focusedInput clearHovered
  let _ ← dynWidget renderState fun (state, focused, clearHov) => do
    let isFoc := focused == some name
    emit do pure (searchInputVisual name clearName theme { state with focused := isFoc }
        placeholder clearHov)

  pure { onChange, onSearch, onClear, onFocus, onBlur, text, isFocused }

end Afferent.Canopy

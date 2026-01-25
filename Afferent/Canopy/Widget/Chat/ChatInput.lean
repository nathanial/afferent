/-
  Canopy Chat Input
  Text input with send button for composing chat messages.
-/
import Reactive
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Reactive.Component
import Afferent.Canopy.Widget.Input.TextInput
import Afferent.Canopy.Widget.Input.Button
import Afferent.Canopy.Widget.Chat.Types

namespace Afferent.Canopy.Chat

open Afferent.Arbor hiding Event
open Reactive Reactive.Host
open Afferent.Canopy
open Afferent.Canopy.Reactive

/-- Result from chatInput widget. -/
structure ChatInputResult where
  /-- Fires with message content when user submits (Enter key or Send button). -/
  onSubmit : Reactive.Event Spider String
  /-- Current input text. -/
  inputText : Reactive.Dynamic Spider String
  /-- Whether the input is focused. -/
  isFocused : Reactive.Dynamic Spider Bool

/-- Configuration for the chat input. -/
structure ChatInputConfig where
  /-- Width of the input area in pixels. -/
  width : Float := 600
  /-- Placeholder text for the input. -/
  placeholder : String := "Type a message..."
  /-- Label for the send button. -/
  sendButtonLabel : String := "Send"
  /-- Gap between input and button. -/
  gap : Float := 8
  /-- Padding around the input area. -/
  padding : Float := 12
deriving Repr, Inhabited

namespace ChatInputConfig

/-- Default configuration. -/
def default : ChatInputConfig := {}

end ChatInputConfig

/-- Create a reactive chat input widget.

    Combines a text input with a send button. Submits on Enter key press
    or Send button click. Clears the input after submission.

    - `theme`: Theme for styling
    - `font`: Font for text input
    - `config`: Configuration
    - `isLoading`: Dynamic indicating if a request is in progress (disables input) -/
def chatInput (theme : Theme) (font : Afferent.Font) (config : ChatInputConfig)
    (isLoading : Reactive.Dynamic Spider Bool) : WidgetM ChatInputResult := do
  -- Create the text input
  let inputResult ← textInput theme font config.placeholder ""

  -- Get keyboard events for Enter key detection
  let keyEvents ← useKeyboard

  -- Filter for Enter key when input is focused
  let enterPressed ← Event.filterM
    (fun keyData => keyData.event.key == .enter)
    keyEvents

  -- Gate Enter by input being focused
  let gatedEnter ← Event.gateM inputResult.isFocused.current enterPressed

  -- Create submit trigger
  let (submitTrigger, fireSubmit) ← Reactive.newTriggerEvent

  -- Handle Enter key submission
  let enterSubmitAction ← Event.mapM
    (fun _ => do
      let text ← inputResult.text.sample
      if !text.isEmpty then
        fireSubmit text)
    gatedEnter
  performEvent_ enterSubmitAction

  -- Create send button
  let notLoading ← Dynamic.mapM (fun l => !l) isLoading
  let inputEmpty ← Dynamic.mapM (fun t => t.isEmpty) inputResult.text
  let canSend ← Dynamic.zipWithM (fun nl ie => nl && !ie) notLoading inputEmpty

  -- Create the send button
  let sendClick ← button config.sendButtonLabel theme .primary

  -- Handle button click submission
  let gatedSendClick ← Event.gateM canSend.current sendClick
  let buttonSubmitAction ← Event.mapM
    (fun _ => do
      let text ← inputResult.text.sample
      if !text.isEmpty then
        fireSubmit text)
    gatedSendClick
  performEvent_ buttonSubmitAction

  pure {
    onSubmit := submitTrigger
    inputText := inputResult.text
    isFocused := inputResult.isFocused
  }

/-- Simpler chat input visual wrapper that just places input and button in a row.
    Use this for custom input handling. -/
def chatInputRow (theme : Theme) (font : Afferent.Font) (config : ChatInputConfig)
    : WidgetM TextInputResult := do
  row' (gap := config.gap) (style := { width := .percent 1.0 }) do
    -- Text input takes up remaining space
    let inputResult ← textInput theme font config.placeholder ""

    -- Send button
    let _ ← button config.sendButtonLabel theme .primary

    pure inputResult

end Afferent.Canopy.Chat

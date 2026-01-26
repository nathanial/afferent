/-
  Canopy Chat Widget
  AI chat interface with streaming support.
-/
import Reactive
import Oracle.Reactive.Conversation
import Afferent.Arbor
import Afferent.Arbor.Widget.Measure
import Afferent.Text.Measurer
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Reactive.Component
import Afferent.Canopy.Widget.Layout.Scroll
import Afferent.Canopy.Widget.Input.TextInput
import Afferent.Canopy.Widget.Input.Button

namespace Afferent.Canopy.Chat

open Afferent.Arbor hiding Event

/-- Role of a message in the chat. -/
inductive ChatRole where
  | user
  | assistant
  | system
deriving Repr, BEq, Inhabited

def ChatRole.toString : ChatRole → String
  | .user => "user"
  | .assistant => "assistant"
  | .system => "system"

def ChatRole.isUser : ChatRole → Bool
  | .user => true
  | _ => false

def ChatRole.isAssistant : ChatRole → Bool
  | .assistant => true
  | _ => false

/-- A single message in the chat history. -/
structure ChatMessage where
  /-- Unique identifier for the message. -/
  id : Nat
  /-- Role of the sender. -/
  role : ChatRole
  /-- Content of the message. -/
  content : String
  /-- Timestamp when the message was created. -/
  timestamp : Nat := 0
  /-- Whether this message is currently being streamed. -/
  isStreaming : Bool := false
deriving Repr, BEq, Inhabited

/-- Create a user message. -/
def ChatMessage.user (id : Nat) (content : String) : ChatMessage :=
  { id, role := .user, content }

/-- Create an assistant message. -/
def ChatMessage.assistant (id : Nat) (content : String) (isStreaming : Bool := false) : ChatMessage :=
  { id, role := .assistant, content, isStreaming }

/-- Create a system message. -/
def ChatMessage.system (id : Nat) (content : String) : ChatMessage :=
  { id, role := .system, content }

/-- Update the content of a streaming message. -/
def ChatMessage.updateContent (msg : ChatMessage) (newContent : String) : ChatMessage :=
  { msg with content := newContent }

/-- Mark a message as finished streaming. -/
def ChatMessage.finishStreaming (msg : ChatMessage) : ChatMessage :=
  { msg with isStreaming := false }

/-- Configuration for the chat widget. -/
structure ChatWidgetConfig where
  /-- Width of the chat widget in pixels. -/
  width : Float := 600
  /-- Height of the chat widget in pixels. -/
  height : Float := 500
  /-- Fill available width instead of using fixed width. -/
  fillWidth : Bool := false
  /-- Fill available height instead of using fixed height. -/
  fillHeight : Bool := false
  /-- Maximum width for message bubbles (relative to chat width). -/
  maxMessageWidth : Float := 0.75
  /-- Placeholder text for the input field. -/
  inputPlaceholder : String := "Type a message..."
  /-- System prompt to initialize the conversation. -/
  systemPrompt : Option String := none
  /-- Show timestamps on messages. -/
  showTimestamps : Bool := false
  /-- Enable auto-scroll to bottom on new messages. -/
  autoScroll : Bool := true
deriving Repr, Inhabited

/-- Default configuration. -/
def ChatWidgetConfig.default : ChatWidgetConfig := {}

/-- Create a configuration with custom dimensions. -/
def ChatWidgetConfig.withSize (width height : Float) : ChatWidgetConfig :=
  { width, height }

/-- Create a configuration with a system prompt. -/
def ChatWidgetConfig.withSystemPrompt (prompt : String) : ChatWidgetConfig :=
  { systemPrompt := some prompt }

/-- State of the chat widget. -/
inductive ChatWidgetState where
  /-- Ready for input. -/
  | idle
  /-- Sending a message to the API. -/
  | sending
  /-- Receiving a streaming response. -/
  | streaming
  /-- An error occurred. -/
  | error (message : String)
deriving Repr, Inhabited

instance : BEq ChatWidgetState where
  beq a b := match a, b with
    | .idle, .idle => true
    | .sending, .sending => true
    | .streaming, .streaming => true
    | .error m1, .error m2 => m1 == m2
    | _, _ => false

def ChatWidgetState.isIdle : ChatWidgetState → Bool
  | .idle => true
  | _ => false

def ChatWidgetState.isBusy : ChatWidgetState → Bool
  | .sending => true
  | .streaming => true
  | _ => false

def ChatWidgetState.isError : ChatWidgetState → Bool
  | .error _ => true
  | _ => false

def ChatWidgetState.errorMessage : ChatWidgetState → Option String
  | .error msg => some msg
  | _ => none

/-- Colors for message bubbles. -/
structure MessageBubbleColors where
  /-- Background color for user messages. -/
  userBackground : Color
  /-- Background color for assistant messages. -/
  assistantBackground : Color
  /-- Text color for user messages. -/
  userText : Color
  /-- Text color for assistant messages. -/
  assistantText : Color
deriving Repr, Inhabited

/-- Default colors for dark theme. -/
def MessageBubbleColors.forDarkTheme : MessageBubbleColors := {
  userBackground := Color.fromRgb8 59 130 246      -- Blue-500
  assistantBackground := Color.gray 0.18
  userText := Color.white
  assistantText := Color.gray 0.9
}

/-- Default colors for light theme. -/
def MessageBubbleColors.forLightTheme : MessageBubbleColors := {
  userBackground := Color.fromRgb8 59 130 246
  assistantBackground := Color.gray 0.92
  userText := Color.white
  assistantText := Color.gray 0.1
}

/-- Create colors from a theme. -/
def MessageBubbleColors.fromTheme (theme : Theme) : MessageBubbleColors := {
  userBackground := theme.primary.background
  assistantBackground := theme.panel.background
  userText := theme.primary.foreground
  assistantText := theme.text
}

/-- Configuration for rendering a message bubble. -/
structure MessageBubbleConfig where
  /-- Maximum width of the bubble in pixels. -/
  maxWidth : Float := 400
  /-- Padding inside the bubble. -/
  padding : Float := 12
  /-- Corner radius for bubbles. -/
  cornerRadius : Float := 12
  /-- Gap between role label and content (if role label shown). -/
  contentGap : Float := 4
  /-- Font for message content. -/
  font : FontId := FontId.default
  /-- Colors for the bubbles. -/
  colors : MessageBubbleColors := MessageBubbleColors.forDarkTheme
deriving Repr, Inhabited

/-- Default configuration. -/
def MessageBubbleConfig.default : MessageBubbleConfig := {}

/-- Create a config from theme. -/
def MessageBubbleConfig.fromTheme (theme : Theme) : MessageBubbleConfig := {
  padding := theme.padding
  cornerRadius := theme.cornerRadius
  font := theme.font
  colors := MessageBubbleColors.fromTheme theme
}

/-- Build a message bubble visual (pure WidgetBuilder).

    User messages are right-aligned with a colored background.
    Assistant messages are left-aligned with a neutral background.
    Streaming messages show a cursor indicator. -/
def messageBubbleVisual (msg : ChatMessage) (config : MessageBubbleConfig) : WidgetBuilder := do
  let isUser := msg.role.isUser
  let bgColor := if isUser then config.colors.userBackground else config.colors.assistantBackground
  let textColor := if isUser then config.colors.userText else config.colors.assistantText

  -- Content with optional streaming indicator
  let displayContent := if msg.isStreaming && !msg.content.isEmpty
    then msg.content ++ " \u25CF"  -- Filled circle as cursor
    else if msg.isStreaming
    then "\u25CF"  -- Show cursor even when empty
    else msg.content

  -- Create the text content with wrapping
  let textWidget ← wrappedText displayContent config.font config.maxWidth textColor

  -- Bubble style
  let bubbleStyle : BoxStyle := {
    backgroundColor := some bgColor
    cornerRadius := config.cornerRadius
    padding := Trellis.EdgeInsets.uniform config.padding
    maxWidth := some config.maxWidth
  }

  -- Wrap text in a styled container
  let bubble ← padded 0 do
    let wid ← freshId
    let props := Trellis.FlexContainer.column 0
    pure (.flex wid none props bubbleStyle #[textWidget])

  -- Row alignment: right for user, left for assistant
  let rowProps : Trellis.FlexContainer := {
    direction := .row
    justifyContent := if isUser then .flexEnd else .flexStart
    alignItems := .flexStart
    gap := 0
  }

  -- Full-width row to enable alignment
  -- shrink := 0 prevents message from being compressed in scroll container
  let rowStyle : BoxStyle := {
    width := .percent 1.0
    flexItem := some { Trellis.FlexItem.default with shrink := 0 }
  }
  let wid ← freshId
  pure (.flex wid none rowProps rowStyle #[bubble])

/-- Build a compact message bubble (no alignment row, just the bubble itself).
    Useful when embedding in a custom layout. -/
def messageBubbleCompact (msg : ChatMessage) (config : MessageBubbleConfig) : WidgetBuilder := do
  let isUser := msg.role.isUser
  let bgColor := if isUser then config.colors.userBackground else config.colors.assistantBackground
  let textColor := if isUser then config.colors.userText else config.colors.assistantText

  let displayContent := if msg.isStreaming && !msg.content.isEmpty
    then msg.content ++ " \u25CF"
    else if msg.isStreaming
    then "\u25CF"
    else msg.content

  let textWidget ← wrappedText displayContent config.font config.maxWidth textColor

  let bubbleStyle : BoxStyle := {
    backgroundColor := some bgColor
    cornerRadius := config.cornerRadius
    padding := Trellis.EdgeInsets.uniform config.padding
    maxWidth := some config.maxWidth
  }

  let wid ← freshId
  let props := Trellis.FlexContainer.column 0
  pure (.flex wid none props bubbleStyle #[textWidget])

open Reactive Reactive.Host
open Afferent.Canopy.Reactive

/-- Result from messageList widget. -/
structure MessageListResult where
  /-- Current scroll state. -/
  scrollState : Reactive.Dynamic Spider ScrollState

/-- Configuration for the message list. -/
structure MessageListConfig where
  /-- Width of the message list in pixels. -/
  width : Float := 600
  /-- Height of the message list in pixels. -/
  height : Float := 400
  /-- Fill available width instead of using fixed width. -/
  fillWidth : Bool := false
  /-- Fill available height instead of using fixed height. -/
  fillHeight : Bool := false
  /-- Gap between messages. -/
  messageGap : Float := 12
  /-- Padding around the message list. -/
  padding : Float := 16
  /-- Configuration for message bubbles. -/
  bubbleConfig : MessageBubbleConfig := {}
deriving Repr, Inhabited

/-- Default configuration. -/
def MessageListConfig.default : MessageListConfig := {}

/-- Create from theme with custom dimensions. -/
def MessageListConfig.fromTheme (theme : Theme) (width height : Float) : MessageListConfig := {
  width
  height
  padding := theme.padding
  bubbleConfig := MessageBubbleConfig.fromTheme theme
}

/-- Build visual representation of message list (pure WidgetBuilder). -/
def messageListVisual (name : String) (messages : Array ChatMessage) (config : MessageListConfig)
    (scrollState : ScrollState) (contentHeight : Float) (theme : Theme) : WidgetBuilder := do
  -- Build message bubble builders
  let msgBuilders : Array WidgetBuilder := messages.map fun msg =>
    messageBubbleVisual msg config.bubbleConfig

  -- Column of messages with gap
  -- IMPORTANT: shrink := 0 prevents content from being compressed inside the scroll viewport
  let contentStyle : BoxStyle := {
    width := .percent 1.0
    padding := Trellis.EdgeInsets.uniform config.padding
    flexItem := some { Trellis.FlexItem.default with shrink := 0 }
  }
  let content := column (gap := config.messageGap) (style := contentStyle) msgBuilders

  -- Scroll container style (conditional based on fill options)
  let scrollStyle : BoxStyle := {
    width := if config.fillWidth then .percent 1.0 else .auto
    height := if config.fillHeight then .percent 1.0 else .auto
    minWidth := if config.fillWidth then none else some config.width
    minHeight := if config.fillHeight then none else some config.height
    maxWidth := if config.fillWidth then none else some config.width
    maxHeight := if config.fillHeight then none else some config.height
    flexItem := if config.fillHeight || config.fillWidth
                then some (Trellis.FlexItem.growing 1)
                else none
  }

  let scrollbarConfig := buildScrollbarConfig
    { width := config.width
      height := config.height
      verticalScroll := true
      horizontalScroll := false
      scrollbarVisibility := .always }
    theme

  namedScroll name scrollStyle config.width contentHeight scrollState scrollbarConfig content

/-- Create a reactive message list widget.

    Takes a Dynamic of messages and renders them in a scrollable container.
    Auto-scrolls to bottom when new messages arrive (if enabled).

    - `messages`: Dynamic array of chat messages
    - `config`: Configuration for the list
    - `autoScroll`: Whether to auto-scroll to bottom on new messages (default true) -/
def messageList (messages : Reactive.Dynamic Spider (Array ChatMessage)) (config : MessageListConfig)
    (autoScroll : Bool := true) : WidgetM MessageListResult := do
  let theme ← getThemeW
  let name ← registerComponentW "chat-message-list"
  let scrollEvents ← useScroll name
  let allClicks ← useAllClicks
  let allHovers ← useAllHovers
  let allMouseUp ← useAllMouseUp

  -- Track content height (estimate based on message count)
  -- Initialize with large content height to allow scrolling before first render
  let contentHeightRef ← SpiderM.liftIO (IO.mkRef (config.height * 10.0))

  -- Initial scroll state (at bottom)
  let initialScroll : ScrollState := { offsetX := 0, offsetY := 0 }

  -- Merge scroll-related events
  -- All Event functions return SpiderM, so we lift to WidgetM via StateT.lift ∘ liftM.
  let liftSpider {α : Type} : SpiderM α → WidgetM α := fun m => StateT.lift (liftM m)
  let wheelEvents : Reactive.Event Spider ScrollInputEvent ←
    liftSpider (Event.mapM ScrollInputEvent.wheel scrollEvents)
  let clickEvents : Reactive.Event Spider ScrollInputEvent ←
    liftSpider (Event.mapM ScrollInputEvent.click allClicks)
  let hoverEvents : Reactive.Event Spider ScrollInputEvent ←
    liftSpider (Event.mapM ScrollInputEvent.hover allHovers)
  let mouseUpEvents : Reactive.Event Spider ScrollInputEvent ←
    liftSpider (Event.mapM (fun _ => ScrollInputEvent.mouseUp) allMouseUp)

  -- Merge all events
  let allInputEvents : Reactive.Event Spider ScrollInputEvent ←
    liftSpider (Event.leftmostM [wheelEvents, clickEvents, hoverEvents, mouseUpEvents])

  -- Scroll state accumulator (call foldDynM directly in WidgetM, not wrapped in liftSpider)
  let scrollState ← Reactive.foldDynM
    (fun event state => do
      let contentH ← SpiderM.liftIO contentHeightRef.get
      match event with
      | .wheel scrollData =>
        let dy := -scrollData.scroll.deltaY * 20.0
        let newScroll := state.scroll.scrollBy 0 dy config.width config.height config.width contentH
        pure { state with scroll := newScroll }
      | .click _clickData =>
        pure state
      | .hover _hoverData =>
        pure state
      | .mouseUp =>
        pure { state with drag := {} })
    ({ scroll := initialScroll, drag := {} } : ScrollCombinedState)
    allInputEvents

  let justScroll ← Dynamic.mapM (·.scroll) scrollState

  -- Auto-scroll to bottom when messages change
  if autoScroll then
    let messageCount ← Dynamic.mapM (·.size) messages
    let countChanges ← Dynamic.changesM messageCount
    let scrollToBottomAction ← Event.mapM
      (fun (_old, _new) => do
        let contentH ← contentHeightRef.get
        let _maxScroll := max 0 (contentH - config.height)
        -- We can't directly set scroll state since it's managed by foldDynM
        -- The actual scroll-to-bottom will be handled by rendering at max offset
        pure ())
      countChanges
    performEvent_ scrollToBottomAction

  -- Get font registry for real text measurement
  let fontRegistry ← getFontRegistry

  -- Render using dynWidget
  let renderState ← Dynamic.zipWithM (fun msgs scroll => (msgs, scroll)) messages justScroll
  let _ ← dynWidget renderState fun (msgs, scroll) => do
    -- Build content widget tree (same as messageListVisual's inner content)
    let msgWidgets : Array WidgetBuilder := msgs.map fun msg =>
      messageBubbleVisual msg config.bubbleConfig
    let contentStyle : BoxStyle := {
      width := .percent 1.0
      padding := Trellis.EdgeInsets.uniform config.padding
    }
    let contentBuilder := column (gap := config.messageGap) (style := contentStyle) msgWidgets
    let (contentWidget, _) := contentBuilder.run {}

    -- Measure actual content height using real font measurement
    let measureResult ← SpiderM.liftIO (runWithFonts fontRegistry (measureWidget contentWidget config.width config.height))
    let (_, measuredH) := nodeContentSize measureResult.node

    -- Use measured height (minimum of viewport height for empty content)
    let contentH := max config.padding measuredH
    SpiderM.liftIO (contentHeightRef.set contentH)

    -- Auto-scroll: if at or near bottom, stay at bottom
    let maxScroll := max 0 (contentH - config.height)
    let effectiveScroll := if autoScroll && scroll.offsetY >= maxScroll - 10
      then { scroll with offsetY := maxScroll }
      else scroll

    emit do pure (messageListVisual name msgs config effectiveScroll contentH theme)

  pure { scrollState := justScroll }

/-- Simple message list that just renders messages without scroll management.
    Use this when you want to manage scrolling yourself. -/
def messageListSimple (messages : Array ChatMessage) (config : MessageListConfig)
    (_theme : Theme) : WidgetM Unit := do
  -- Build message bubbles
  for msg in messages do
    emit do pure (messageBubbleVisual msg config.bubbleConfig)

open Afferent.Canopy

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

/-- Default configuration. -/
def ChatInputConfig.default : ChatInputConfig := {}

/-- Create a reactive chat input widget.

    Combines a text input with a send button. Submits on Enter key press
    or Send button click. Clears the input after submission.
    Uses the default font from WidgetM context (set via createInputs).

    - `config`: Configuration
    - `isLoading`: Dynamic indicating if a request is in progress (disables input) -/
def chatInput (config : ChatInputConfig)
    (isLoading : Reactive.Dynamic Spider Bool) : WidgetM ChatInputResult := do
  -- Create the text input wrapped in a growing container so it fills available width
  let inputResult ← column' (gap := 0) (style := {
    flexItem := some (Trellis.FlexItem.growing 1)
    width := .percent 1.0
  }) do
    textInput config.placeholder ""

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
  let sendClick ← button config.sendButtonLabel .primary

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
def chatInputRow (config : ChatInputConfig) : WidgetM TextInputResult := do
  row' (gap := config.gap) (style := { width := .percent 1.0 }) do
    -- Text input takes up remaining space with growing flex item
    let inputResult ← column' (gap := 0) (style := {
      flexItem := some (Trellis.FlexItem.growing 1)
      width := .percent 1.0
    }) do
      textInput config.placeholder ""

    -- Send button
    let _ ← button config.sendButtonLabel .primary

    pure inputResult

open Oracle
open Oracle.Reactive

/-- Result from chatWidget. -/
structure ChatWidgetResult where
  /-- The underlying conversation manager for programmatic control. -/
  manager : ConversationManager
  /-- Observable state of the chat widget. -/
  state : Reactive.Dynamic Spider ChatWidgetState
  /-- Observable list of messages (converted to ChatMessage format). -/
  messages : Reactive.Dynamic Spider (Array ChatMessage)

/-- Convert an Oracle.Role to ChatRole. -/
def roleFromOracle : Oracle.Role → ChatRole
  | .user => .user
  | .assistant => .assistant
  | .system => .system
  | .tool => .assistant  -- Treat tool responses as assistant
  | .developer => .system  -- Treat developer as system

/-- Convert an Oracle.Message to ChatMessage. -/
def messageFromOracle (id : Nat) (msg : Oracle.Message) : ChatMessage := {
  id := id
  role := roleFromOracle msg.role
  content := msg.content.asString
  timestamp := 0
  isStreaming := false
}

/-- Convert Oracle Conversation to Array ChatMessage. -/
def messagesFromConversation (conv : Oracle.Reactive.Conversation) : Array ChatMessage :=
  conv.messages.mapIdx fun idx msg =>
    messageFromOracle idx msg

/-- Convert ConversationState to ChatWidgetState. -/
def stateFromConversation : Oracle.Reactive.ConversationState → ChatWidgetState
  | .idle => .idle
  | .sending => .sending
  | .streaming => .streaming
  | .error err => .error (toString err)

/-- Create a reactive chat widget.

    This is the main entry point for the chat widget. It creates a full
    chat interface with:
    - Scrollable message list with user/assistant bubbles
    - Text input with send button
    - Streaming response display
    - Cancel support via Escape key
    Uses the default font from WidgetM context (set via createInputs).

    - `client`: ReactiveClient for API calls
    - `config`: Widget configuration
    - `systemPrompt`: Optional system prompt for the conversation -/
def chatWidget (client : ReactiveClient) (config : ChatWidgetConfig := {})
    (systemPrompt : Option String := none) : WidgetM ChatWidgetResult := do
  let theme ← getThemeW
  -- Create the conversation manager
  let effectiveSystemPrompt := systemPrompt.orElse (fun _ => config.systemPrompt)
  let manager ← (ConversationManager.new client effectiveSystemPrompt : SpiderM ConversationManager)

  -- Convert manager state to widget state
  let widgetState ← Dynamic.mapM stateFromConversation manager.state

  -- Convert conversation messages to ChatMessage array
  let baseMessages ← Dynamic.mapM messagesFromConversation manager.conversation

  -- Get streaming status as Bool (has BEq, unlike Option StreamingRequestOutput)
  let isStreaming ← Dynamic.mapM (· == .streaming) widgetState

  -- Get streaming content using bindOptionM
  -- When currentStream is Some, track the stream's content dynamic
  -- When None, use empty string
  let streamingContent : Reactive.Dynamic Spider String ←
    Dynamic.bindOptionM manager.currentStream (·.content) ""

  -- Combine base messages with streaming flag and content
  -- Using zipWith3M: first two args need BEq (Array ChatMessage, Bool), result needs BEq
  -- Third arg (String) doesn't need BEq
  let allMessages : Reactive.Dynamic Spider (Array ChatMessage) ← Dynamic.zipWith3M
    (fun msgs streaming content =>
      if streaming then
        -- Add a streaming assistant message with current content
        let streamingMsg : ChatMessage := {
          id := msgs.size
          role := .assistant
          content := content
          isStreaming := true
        }
        msgs.push streamingMsg
      else msgs)
    baseMessages isStreaming streamingContent

  -- Configure message list
  let msgListConfig : MessageListConfig := {
    width := config.width
    height := config.height - 80  -- Reserve space for input
    fillWidth := config.fillWidth
    fillHeight := config.fillHeight
    messageGap := 12
    padding := 16
    bubbleConfig := {
      maxWidth := config.width * config.maxMessageWidth
      font := theme.font
      colors := MessageBubbleColors.fromTheme theme
    }
  }

  -- Configure input
  let inputConfig : ChatInputConfig := {
    width := config.width
    placeholder := config.inputPlaceholder
    gap := 8
    padding := 12
  }

  -- Create the loading indicator dynamic
  let isLoading ← Dynamic.mapM (fun s => s.isBusy) widgetState

  -- Main layout: column with message list + input area
  let outerStyle : BoxStyle := {
    width := if config.fillWidth then .percent 1.0 else .auto
    height := if config.fillHeight then .percent 1.0 else .auto
    minWidth := if config.fillWidth then none else some config.width
    minHeight := if config.fillHeight then none else some config.height
    flexItem := if config.fillHeight || config.fillWidth
                then some (Trellis.FlexItem.growing 1)
                else none
  }
  column' (gap := 0) (style := outerStyle) do
    -- Message list
    let _ ← messageList allMessages msgListConfig config.autoScroll

    -- Input area (fixed at bottom)
    row' (gap := inputConfig.gap) (style := {
      width := .percent 1.0
      padding := Trellis.EdgeInsets.symmetric inputConfig.padding inputConfig.padding
      backgroundColor := some theme.panel.background
    }) do
      -- Create the chat input
      let inputResult ← chatInput inputConfig isLoading

      -- Wire up submit to send message
      let sendAction ← Event.mapM
        (fun text => manager.sendMessage text)
        inputResult.onSubmit
      performEvent_ sendAction

  -- Handle Escape key to cancel
  let keyEvents ← useKeyboard
  let escapePressed ← Event.filterM (fun k => k.event.key == .escape) keyEvents
  let cancelAction ← Event.mapM (fun _ => manager.cancelCurrent) escapePressed
  performEvent_ cancelAction

  pure { manager, state := widgetState, messages := allMessages }

/-- Simpler chat widget that just takes an API key.
    Creates a ReactiveClient internally. -/
def chatWidgetWithApiKey (apiKey : String) (model : String := Oracle.Models.geminiFlash)
    (config : ChatWidgetConfig := {}) (systemPrompt : Option String := none)
    : WidgetM ChatWidgetResult := do
  let client := ReactiveClient.withModel apiKey model
  chatWidget client config systemPrompt

end Afferent.Canopy.Chat

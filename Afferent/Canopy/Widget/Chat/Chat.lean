/-
  Canopy Chat Widget
  Complete chat interface for AI conversations with streaming support.
-/
import Reactive
import Oracle.Reactive.Conversation
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Reactive.Component
import Afferent.Canopy.Widget.Layout.Scroll
import Afferent.Canopy.Widget.Display.Spinner
import Afferent.Canopy.Widget.Chat.Types
import Afferent.Canopy.Widget.Chat.MessageBubble
import Afferent.Canopy.Widget.Chat.MessageList
import Afferent.Canopy.Widget.Chat.ChatInput

namespace Afferent.Canopy.Chat

open Afferent.Arbor hiding Event
open Reactive Reactive.Host
open Afferent.Canopy
open Afferent.Canopy.Reactive
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

    - `client`: ReactiveClient for API calls
    - `theme`: Theme for styling
    - `font`: Font for text rendering
    - `config`: Widget configuration
    - `systemPrompt`: Optional system prompt for the conversation -/
def chatWidget (client : ReactiveClient) (theme : Theme) (font : Afferent.Font)
    (config : ChatWidgetConfig := {}) (systemPrompt : Option String := none)
    : WidgetM ChatWidgetResult := do
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
  column' (gap := 0) (style := { minWidth := some config.width, minHeight := some config.height }) do
    -- Message list
    let _ ← messageList allMessages msgListConfig theme config.autoScroll

    -- Input area (fixed at bottom)
    row' (gap := inputConfig.gap) (style := {
      width := .percent 1.0
      padding := Trellis.EdgeInsets.symmetric inputConfig.padding inputConfig.padding
      backgroundColor := some theme.panel.background
    }) do
      -- Create the chat input
      let inputResult ← chatInput theme font inputConfig isLoading

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
def chatWidgetWithApiKey (apiKey : String) (model : String := "anthropic/claude-sonnet-4")
    (theme : Theme) (font : Afferent.Font)
    (config : ChatWidgetConfig := {}) (systemPrompt : Option String := none)
    : WidgetM ChatWidgetResult := do
  let client := ReactiveClient.withModel apiKey model
  chatWidget client theme font config systemPrompt

end Afferent.Canopy.Chat

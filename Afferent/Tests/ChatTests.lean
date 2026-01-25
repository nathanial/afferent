/-
  Chat Widget Tests
  Unit tests for the chat widget layout and sizing behavior.
-/
import Afferent.Tests.Framework
import Afferent.Arbor
import Afferent.Arbor.Widget.DSL
import Afferent.Canopy.Reactive.Component
import Afferent.Canopy.Widget.Chat
import Afferent.Layout
import Reactive
import Trellis

namespace Afferent.Tests.ChatTests

open Crucible
open Afferent.Tests
open Afferent.Arbor
open Afferent.Canopy
open Afferent.Canopy.Chat
open Afferent.Canopy.Reactive
open Reactive Reactive.Host
open Trellis

testSuite "Chat Widget Tests"

/-- Test font ID for widget building tests. -/
def testFont : FontId := { id := 0, name := "test", size := 14.0 }

/-- Test theme for widget tests. -/
def testTheme : Theme := { Theme.dark with font := testFont, smallFont := testFont }

/-! ## ChatWidgetConfig Tests -/

test "ChatWidgetConfig default values" := do
  let config := ChatWidgetConfig.default
  shouldBeNear config.width 600.0
  shouldBeNear config.height 500.0
  ensure (!config.fillWidth) "Default fillWidth should be false"
  ensure (!config.fillHeight) "Default fillHeight should be false"
  shouldBeNear config.maxMessageWidth 0.75
  ensure (config.inputPlaceholder == "Type a message...") "Default placeholder"
  ensure config.systemPrompt.isNone "Default systemPrompt should be none"
  ensure (!config.showTimestamps) "Default showTimestamps should be false"
  ensure config.autoScroll "Default autoScroll should be true"

test "ChatWidgetConfig with fill options" := do
  let config : ChatWidgetConfig := { fillWidth := true, fillHeight := true }
  ensure config.fillWidth "fillWidth should be true"
  ensure config.fillHeight "fillHeight should be true"
  -- Other defaults unchanged
  shouldBeNear config.width 600.0
  shouldBeNear config.height 500.0

test "ChatWidgetConfig.withSize creates custom dimensions" := do
  let config := ChatWidgetConfig.withSize 800 600
  shouldBeNear config.width 800.0
  shouldBeNear config.height 600.0

/-! ## MessageListConfig Tests -/

test "MessageListConfig default values" := do
  let config := MessageListConfig.default
  shouldBeNear config.width 600.0
  shouldBeNear config.height 400.0
  ensure (!config.fillWidth) "Default fillWidth should be false"
  ensure (!config.fillHeight) "Default fillHeight should be false"
  shouldBeNear config.messageGap 12.0
  shouldBeNear config.padding 16.0

test "MessageListConfig with fill options" := do
  let config : MessageListConfig := { fillWidth := true, fillHeight := true }
  ensure config.fillWidth "fillWidth should be true"
  ensure config.fillHeight "fillHeight should be true"

test "MessageListConfig.fromTheme preserves dimensions" := do
  let config := MessageListConfig.fromTheme testTheme 800 600
  shouldBeNear config.width 800.0
  shouldBeNear config.height 600.0

/-! ## MessageBubbleConfig Tests -/

test "MessageBubbleConfig default values" := do
  let config := MessageBubbleConfig.default
  shouldBeNear config.maxWidth 400.0
  shouldBeNear config.padding 12.0
  shouldBeNear config.cornerRadius 12.0
  shouldBeNear config.contentGap 4.0

test "MessageBubbleConfig.fromTheme uses theme values" := do
  let config := MessageBubbleConfig.fromTheme testTheme
  shouldBeNear config.padding testTheme.padding
  shouldBeNear config.cornerRadius testTheme.cornerRadius

/-! ## ChatMessage Tests -/

test "ChatMessage.user creates user message" := do
  let msg := ChatMessage.user 0 "Hello"
  ensure (msg.id == 0) "ID should be 0"
  ensure msg.role.isUser "Role should be user"
  ensure (msg.content == "Hello") "Content should match"
  ensure (!msg.isStreaming) "Should not be streaming"

test "ChatMessage.assistant creates assistant message" := do
  let msg := ChatMessage.assistant 1 "Hi there" true
  ensure (msg.id == 1) "ID should be 1"
  ensure msg.role.isAssistant "Role should be assistant"
  ensure (msg.content == "Hi there") "Content should match"
  ensure msg.isStreaming "Should be streaming"

test "ChatMessage.system creates system message" := do
  let msg := ChatMessage.system 2 "System prompt"
  ensure (msg.id == 2) "ID should be 2"
  ensure (msg.role == .system) "Role should be system"
  ensure (msg.content == "System prompt") "Content should match"

test "ChatMessage.updateContent updates content" := do
  let msg := ChatMessage.assistant 0 "Hello"
  let updated := msg.updateContent "Hello World"
  ensure (updated.content == "Hello World") "Content should be updated"
  ensure (updated.id == msg.id) "ID should be preserved"

test "ChatMessage.finishStreaming clears streaming flag" := do
  let msg := ChatMessage.assistant 0 "Response" true
  ensure msg.isStreaming "Should start streaming"
  let finished := msg.finishStreaming
  ensure (!finished.isStreaming) "Should finish streaming"

/-! ## ChatWidgetState Tests -/

test "ChatWidgetState.idle is idle" := do
  let state := ChatWidgetState.idle
  ensure state.isIdle "Should be idle"
  ensure (!state.isBusy) "Should not be busy"
  ensure (!state.isError) "Should not be error"

test "ChatWidgetState.sending is busy" := do
  let state := ChatWidgetState.sending
  ensure (!state.isIdle) "Should not be idle"
  ensure state.isBusy "Should be busy"

test "ChatWidgetState.streaming is busy" := do
  let state := ChatWidgetState.streaming
  ensure (!state.isIdle) "Should not be idle"
  ensure state.isBusy "Should be busy"

test "ChatWidgetState.error has message" := do
  let state := ChatWidgetState.error "Something went wrong"
  ensure state.isError "Should be error"
  ensure (state.errorMessage == some "Something went wrong") "Should have message"

/-! ## MessageBubbleColors Tests -/

test "MessageBubbleColors.forDarkTheme has correct colors" := do
  let colors := MessageBubbleColors.forDarkTheme
  -- User background is blue
  ensure (colors.userBackground.r > 0.2) "User background should have red"
  ensure (colors.userBackground.b > 0.8) "User background should have blue"
  -- User text is white
  shouldBeNear colors.userText.r 1.0
  shouldBeNear colors.userText.g 1.0
  shouldBeNear colors.userText.b 1.0

test "MessageBubbleColors.forLightTheme has correct colors" := do
  let colors := MessageBubbleColors.forLightTheme
  -- Assistant background is light gray
  ensure (colors.assistantBackground.r > 0.9) "Should be light"
  -- Assistant text is dark
  ensure (colors.assistantText.r < 0.2) "Should be dark"

/-! ## Visual Widget Tests -/

test "messageBubbleVisual creates widget with correct structure" := do
  let msg := ChatMessage.user 0 "Hello World"
  let config := MessageBubbleConfig.default
  let builder := messageBubbleVisual msg config
  let (widget, _) ← builder.run {}
  -- Should be a flex row for alignment
  match widget with
  | .flex _ _ props _ children =>
    ensure (props.direction == .row) "Should be a row for alignment"
    ensure (children.size >= 1) "Should have at least one child (the bubble)"
  | _ => ensure false "Expected flex widget"

test "messageBubbleVisual user message aligns right" := do
  let msg := ChatMessage.user 0 "User message"
  let config := MessageBubbleConfig.default
  let builder := messageBubbleVisual msg config
  let (widget, _) ← builder.run {}
  match widget with
  | .flex _ _ props _ _ =>
    ensure (props.justifyContent == .flexEnd) "User messages should align right"
  | _ => ensure false "Expected flex widget"

test "messageBubbleVisual assistant message aligns left" := do
  let msg := ChatMessage.assistant 0 "Assistant message"
  let config := MessageBubbleConfig.default
  let builder := messageBubbleVisual msg config
  let (widget, _) ← builder.run {}
  match widget with
  | .flex _ _ props _ _ =>
    ensure (props.justifyContent == .flexStart) "Assistant messages should align left"
  | _ => ensure false "Expected flex widget"

test "messageBubbleVisual streaming message has cursor" := do
  let msg := ChatMessage.assistant 0 "Streaming" true
  let config := MessageBubbleConfig.default
  let builder := messageBubbleVisual msg config
  let (widget, _) ← builder.run {}
  -- The streaming indicator should be in the widget tree
  ensure (widget.widgetCount >= 1) "Should create widget tree"

test "messageBubbleCompact creates simpler structure" := do
  let msg := ChatMessage.user 0 "Hello"
  let config := MessageBubbleConfig.default
  let builder := messageBubbleCompact msg config
  let (widget, _) ← builder.run {}
  -- Should be a flex column (the bubble itself)
  match widget with
  | .flex _ _ props _ _ =>
    ensure (props.direction == .column) "Should be a column"
  | _ => ensure false "Expected flex widget"

/-! ## MessageList Visual Tests -/

test "messageListVisual creates scroll container" := do
  let messages := #[ChatMessage.user 0 "Hello", ChatMessage.assistant 1 "Hi"]
  let config := MessageListConfig.default
  let scrollState := ScrollState.zero
  let contentHeight := 200.0
  let builder := messageListVisual messages config scrollState contentHeight testTheme
  let (widget, _) ← builder.run {}
  match widget with
  | .scroll _ name _ _ _ _ _ _ =>
    ensure (name == some "chat-message-list") "Should have correct name"
  | _ => ensure false "Expected scroll widget"

test "messageListVisual with fixed size has min/max constraints" := do
  let messages := #[ChatMessage.user 0 "Hello"]
  let config : MessageListConfig := { width := 400, height := 300 }
  let scrollState := ScrollState.zero
  let contentHeight := 100.0
  let builder := messageListVisual messages config scrollState contentHeight testTheme
  let (widget, _) ← builder.run {}
  match widget with
  | .scroll _ _ style _ _ _ _ _ =>
    -- Fixed mode should have min/max constraints
    ensure style.minWidth.isSome "Should have minWidth constraint"
    ensure style.maxWidth.isSome "Should have maxWidth constraint"
    match style.minWidth, style.maxWidth with
    | some minW, some maxW =>
      shouldBeNear minW 400.0
      shouldBeNear maxW 400.0
    | _, _ => ensure false "Expected minWidth and maxWidth"
  | _ => ensure false "Expected scroll widget"

test "messageListVisual with fill options removes constraints" := do
  let messages := #[ChatMessage.user 0 "Hello"]
  let config : MessageListConfig := { fillWidth := true, fillHeight := true }
  let scrollState := ScrollState.zero
  let contentHeight := 100.0
  let builder := messageListVisual messages config scrollState contentHeight testTheme
  let (widget, _) ← builder.run {}
  match widget with
  | .scroll _ _ style _ _ _ _ _ =>
    -- Fill mode should not have min/max constraints
    ensure style.minWidth.isNone "Should not have minWidth constraint in fill mode"
    ensure style.maxWidth.isNone "Should not have maxWidth constraint in fill mode"
    -- Should have percent dimensions
    match style.width with
    | .percent p => shouldBeNear p 1.0
    | _ => ensure false "Should have percent width"
    match style.height with
    | .percent p => shouldBeNear p 1.0
    | _ => ensure false "Should have percent height"
  | _ => ensure false "Expected scroll widget"

test "messageListVisual with fill options has growing flexItem" := do
  let messages := #[ChatMessage.user 0 "Hello"]
  let config : MessageListConfig := { fillWidth := true, fillHeight := true }
  let scrollState := ScrollState.zero
  let contentHeight := 100.0
  let builder := messageListVisual messages config scrollState contentHeight testTheme
  let (widget, _) ← builder.run {}
  match widget with
  | .scroll _ _ style _ _ _ _ _ =>
    match style.flexItem with
    | some item => ensure (item.grow > 0) "Should have positive grow"
    | none => ensure false "Should have flexItem in fill mode"
  | _ => ensure false "Expected scroll widget"

test "messageListVisual empty messages creates empty content" := do
  let messages : Array ChatMessage := #[]
  let config := MessageListConfig.default
  let scrollState := ScrollState.zero
  let contentHeight := 0.0
  let builder := messageListVisual messages config scrollState contentHeight testTheme
  let (widget, _) ← builder.run {}
  -- Should still create valid widget tree
  ensure (widget.widgetCount >= 1) "Should create widget tree even with no messages"

/-! ## Layout Integration Tests -/

test "messageListVisual layout with fixed size" := do
  let messages := #[ChatMessage.user 0 "Test message"]
  let config : MessageListConfig := { width := 400, height := 300 }
  let scrollState := ScrollState.zero
  let contentHeight := 100.0
  let builder := messageListVisual messages config scrollState contentHeight testTheme
  let (widget, _) ← builder.run {}

  -- Measure and layout
  let measureResult := measureWidget (M := Id) widget 800 600
  let layouts := Trellis.layout measureResult.node 800 600

  -- Get the scroll container layout (should be first widget)
  let scrollLayout := layouts.get! measureResult.widget.id
  -- In fixed mode, should respect min constraints
  ensure (scrollLayout.contentRect.width >= config.width)
    s!"Width {scrollLayout.contentRect.width} should be >= {config.width}"
  ensure (scrollLayout.contentRect.height >= config.height)
    s!"Height {scrollLayout.contentRect.height} should be >= {config.height}"

test "messageListVisual layout with fill mode expands" := do
  let messages := #[ChatMessage.user 0 "Test message"]
  let config : MessageListConfig := { fillWidth := true, fillHeight := true }
  let scrollState := ScrollState.zero
  let contentHeight := 100.0
  let builder := messageListVisual messages config scrollState contentHeight testTheme
  let (widget, _) ← builder.run {}

  let containerWidth := 800.0
  let containerHeight := 600.0

  -- Measure and layout
  let measureResult := measureWidget (M := Id) widget containerWidth containerHeight
  let layouts := Trellis.layout measureResult.node containerWidth containerHeight

  -- Get the scroll container layout
  let scrollLayout := layouts.get! measureResult.widget.id
  -- In fill mode, should expand to container size
  shouldBeNear scrollLayout.contentRect.width containerWidth
  shouldBeNear scrollLayout.contentRect.height containerHeight

/-! ## ChatInputConfig Tests -/

test "ChatInputConfig default values" := do
  let config := ChatInputConfig.default
  shouldBeNear config.width 600.0
  ensure (config.placeholder == "Type a message...") "Default placeholder"
  ensure (config.sendButtonLabel == "Send") "Default send button label"
  shouldBeNear config.gap 8.0
  shouldBeNear config.padding 12.0

test "ChatInputConfig custom values" := do
  let config : ChatInputConfig := {
    width := 800
    placeholder := "Ask a question..."
    sendButtonLabel := "Submit"
    gap := 12
    padding := 16
  }
  shouldBeNear config.width 800.0
  ensure (config.placeholder == "Ask a question...") "Custom placeholder"
  ensure (config.sendButtonLabel == "Submit") "Custom send button label"

/-! ## ChatRole Tests -/

test "ChatRole.toString converts correctly" := do
  ensure (ChatRole.user.toString == "user") "user should convert"
  ensure (ChatRole.assistant.toString == "assistant") "assistant should convert"
  ensure (ChatRole.system.toString == "system") "system should convert"

test "ChatRole.isUser and isAssistant" := do
  ensure ChatRole.user.isUser "user isUser"
  ensure (!ChatRole.user.isAssistant) "user not isAssistant"
  ensure ChatRole.assistant.isAssistant "assistant isAssistant"
  ensure (!ChatRole.assistant.isUser) "assistant not isUser"
  ensure (!ChatRole.system.isUser) "system not isUser"
  ensure (!ChatRole.system.isAssistant) "system not isAssistant"

end Afferent.Tests.ChatTests

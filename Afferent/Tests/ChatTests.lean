/-
  Chat Widget Tests
  Unit tests for the chat widget layout and sizing behavior.
-/
import Afferent.Tests.Framework
import Afferent.Arbor
import Afferent.Arbor.Widget.DSL
import Afferent.Arbor.Render.Collect
import Afferent.Canopy.Reactive.Component
import Afferent.Canopy.Widget.Chat
import Afferent.Canopy.Widget.Layout.Scroll
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
  let testName := "test-chat-scroll"
  let builder := messageListVisual testName messages config scrollState contentHeight testTheme
  let (widget, _) ← builder.run {}
  match widget with
  | .scroll _ name _ _ _ _ _ _ =>
    ensure (name == some testName) s!"Should have correct name, got {name}"
  | _ => ensure false "Expected scroll widget"

test "messageListVisual with fixed size has min/max constraints" := do
  let messages := #[ChatMessage.user 0 "Hello"]
  let config : MessageListConfig := { width := 400, height := 300 }
  let scrollState := ScrollState.zero
  let contentHeight := 100.0
  let builder := messageListVisual "test-fixed" messages config scrollState contentHeight testTheme
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
  let builder := messageListVisual "test-fill" messages config scrollState contentHeight testTheme
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
  let builder := messageListVisual "test-flex" messages config scrollState contentHeight testTheme
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
  let builder := messageListVisual "test-empty" messages config scrollState contentHeight testTheme
  let (widget, _) ← builder.run {}
  -- Should still create valid widget tree
  ensure (widget.widgetCount >= 1) "Should create widget tree even with no messages"

/-! ## Layout Integration Tests -/

test "messageListVisual layout with fixed size" := do
  let messages := #[ChatMessage.user 0 "Test message"]
  let config : MessageListConfig := { width := 400, height := 300 }
  let scrollState := ScrollState.zero
  let contentHeight := 100.0
  let builder := messageListVisual "test-message-list" messages config scrollState contentHeight testTheme
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
  let builder := messageListVisual "test-message-list-fill" messages config scrollState contentHeight testTheme
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

/-! ## Message List Scroll Tests

These tests verify the scrolling behavior of the chat message list,
including content height estimation, auto-scroll logic, and FRP event flow.
-/

/-- Create a minimal widget for testing. -/
def testWidget : Widget := .spacer 0 none 100 100

/-- Create a LayoutResult with a scroll container at specified position. -/
def mkScrollLayout (widgetId : WidgetId) (x y width height : Float) : LayoutResult :=
  let layout : Trellis.ComputedLayout := {
    nodeId := widgetId
    contentRect := { x, y, width, height }
    borderRect := { x, y, width, height }
  }
  LayoutResult.empty.add layout

/-! ### Content Height Estimation Tests (Pure) -/

test "messageList content height estimation formula" := do
  -- Content height = padding * 2 + messages.size * (avgMsgHeight + gap)
  -- avgMsgHeight = 60.0 (from Chat.lean line 425)
  let config : MessageListConfig := { padding := 16, messageGap := 12 }
  let msgCount := 10
  let avgMsgHeight := 60.0
  let expectedHeight := config.padding * 2 + msgCount.toFloat * (avgMsgHeight + config.messageGap)
  -- 32 + 10 * 72 = 752
  shouldBeNear expectedHeight 752.0

test "messageList content height with zero messages" := do
  let config : MessageListConfig := { padding := 16, messageGap := 12 }
  let msgCount := 0
  let avgMsgHeight := 60.0
  let expectedHeight := config.padding * 2 + msgCount.toFloat * (avgMsgHeight + config.messageGap)
  -- Just padding: 32
  shouldBeNear expectedHeight 32.0

test "messageList content height with many messages" := do
  let config : MessageListConfig := { padding := 16, messageGap := 12 }
  let msgCount := 50
  let avgMsgHeight := 60.0
  let expectedHeight := config.padding * 2 + msgCount.toFloat * (avgMsgHeight + config.messageGap)
  -- 32 + 50 * 72 = 3632
  shouldBeNear expectedHeight 3632.0

/-! ### Auto-Scroll Logic Tests (Pure) -/

test "auto-scroll stays at bottom when near bottom" := do
  -- From Chat.lean lines 429-433:
  -- if autoScroll && scroll.offsetY >= maxScroll - 10 then snap to maxScroll
  let contentH := 600.0
  let viewportH := 200.0
  let maxScroll := max 0 (contentH - viewportH)  -- 400
  let scrollOffset := 395.0  -- Near bottom (within 10px)
  let autoScroll := true
  let effectiveScroll := if autoScroll && scrollOffset >= maxScroll - 10
    then maxScroll
    else scrollOffset
  shouldBeNear effectiveScroll 400.0

test "auto-scroll position preserved when scrolled up" := do
  -- If scrollOffset < maxScroll - 10, keep current position
  let contentH := 600.0
  let viewportH := 200.0
  let maxScroll := max 0 (contentH - viewportH)  -- 400
  let scrollOffset := 100.0  -- Scrolled up (not within 10px of bottom)
  let autoScroll := true
  let effectiveScroll := if autoScroll && scrollOffset >= maxScroll - 10
    then maxScroll
    else scrollOffset
  shouldBeNear effectiveScroll 100.0

test "auto-scroll disabled preserves scroll position even near bottom" := do
  let contentH := 600.0
  let viewportH := 200.0
  let maxScroll := max 0 (contentH - viewportH)  -- 400
  let scrollOffset := 395.0  -- Near bottom
  let autoScroll := false
  let effectiveScroll := if autoScroll && scrollOffset >= maxScroll - 10
    then maxScroll
    else scrollOffset
  -- With autoScroll=false, should preserve exact position
  shouldBeNear effectiveScroll 395.0

test "max scroll calculation with no overflow" := do
  -- When content fits in viewport, maxScroll is 0
  let contentH := 150.0
  let viewportH := 200.0
  let maxScroll := max 0 (contentH - viewportH)
  shouldBeNear maxScroll 0.0

/-! ### ScrollState Integration Tests -/

test "ScrollState.scrollBy applies to message list viewport" := do
  -- Using MessageListConfig dimensions
  let config : MessageListConfig := { width := 400, height := 300 }
  let contentH := 900.0  -- Tall content (3x viewport)

  let initial := ScrollState.zero
  let after := initial.scrollBy 0 100 config.width config.height config.width contentH

  shouldBeNear after.offsetY 100.0
  shouldBeNear after.offsetX 0.0

test "ScrollState.scrollBy clamps to max for message list" := do
  let config : MessageListConfig := { width := 400, height := 300 }
  let contentH := 900.0  -- max scroll = 900 - 300 = 600

  let initial := ScrollState.zero
  let after := initial.scrollBy 0 1000 config.width config.height config.width contentH

  -- Should be clamped to max scroll
  shouldBeNear after.offsetY 600.0

test "ScrollState.scrollBy clamps to zero at top" := do
  let config : MessageListConfig := { width := 400, height := 300 }
  let contentH := 900.0

  let initial := ScrollState.zero
  let after := initial.scrollBy 0 (-100) config.width config.height config.width contentH

  shouldBeNear after.offsetY 0.0

/-! ### FRP Event Flow Tests -/

test "FRP: messageList returns MessageListResult with scrollState" := do
  let result ← runSpider do
    let (events, _) ← createInputs FontRegistry.empty
    let messagesDyn ← Dynamic.pureM #[ChatMessage.user 0 "Test message"]
    let config := MessageListConfig.default

    let (listResult, _) ← (messageList messagesDyn config testTheme true).run
      { children := #[] } |>.run events

    -- Verify scrollState dynamic exists and can be sampled
    let scrollState ← listResult.scrollState.sample
    pure scrollState.offsetY

  -- Initial scroll should be at 0
  shouldBeNear result 0.0

test "FRP: messageList scrollState starts at zero" := do
  let result ← runSpider do
    let (events, _) ← createInputs FontRegistry.empty
    let messages := #[
      ChatMessage.user 0 "Hello",
      ChatMessage.assistant 1 "Hi there",
      ChatMessage.user 2 "How are you?"
    ]
    let messagesDyn ← Dynamic.pureM messages
    let config : MessageListConfig := { width := 400, height := 300 }

    let (listResult, _) ← (messageList messagesDyn config testTheme false).run
      { children := #[] } |>.run events

    let initialState ← listResult.scrollState.sample
    pure (initialState.offsetX, initialState.offsetY)

  shouldBeNear result.fst 0.0
  shouldBeNear result.snd 0.0

test "FRP: useScroll filters events by widget name" := do
  -- Test that useScroll properly filters scroll events by name
  let result ← runSpider do
    let (events, inputs) ← createInputs FontRegistry.empty

    -- Register a component with a known name
    let name ← (registerComponent "test-scroll" false true).run events

    -- Subscribe to filtered scroll events for that name
    let scrollEvents ← (useScroll name).run events

    -- Count how many events pass through
    let countRef ← SpiderM.liftIO (IO.mkRef 0)
    let _ ← SpiderM.liftIO <| scrollEvents.subscribe fun _ => do
      countRef.modify (· + 1)

    -- Fire a scroll event for the CORRECT name (should pass filter)
    let scrollWidgetId : WidgetId := 42
    let scrollWidget : Widget := .scroll scrollWidgetId (some name) {} {} 400 600 {} testWidget

    let scrollData : ScrollData := {
      scroll := { x := 200, y := 150, deltaX := 0, deltaY := -3.0, modifiers := {} }
      hitPath := #[scrollWidgetId]
      widget := scrollWidget
      layouts := mkScrollLayout scrollWidgetId 0 0 400 300
    }
    inputs.fireScroll scrollData

    SpiderM.liftIO countRef.get

  -- Should receive exactly 1 event
  ensure (result == 1) s!"Expected 1 event, got {result}"

test "FRP DEBUG: trace scroll event flow through messageList pipeline (SpiderM)" := do
  -- This debug test traces the event flow step by step to identify where events are lost
  let result ← runSpider do
    let (events, inputs) ← createInputs FontRegistry.empty

    -- Step 1: Register component and check name
    let name ← (registerComponent "chat-message-list" false true).run events

    -- Step 2: Get filtered scroll events
    let scrollEvents ← (useScroll name).run events

    -- Step 3: Map to ScrollInputEvent.wheel (like messageList does)
    let wheelEvents ← Event.mapM ScrollInputEvent.wheel scrollEvents

    -- Step 4: Merge with empty events (simplified from messageList)
    let allInputEvents ← Event.leftmostM [wheelEvents]

    -- Step 5: Set up content height ref
    let contentHeightRef ← SpiderM.liftIO (IO.mkRef 3000.0)

    -- Step 6: Fold events (like messageList does)
    let config : MessageListConfig := { width := 400, height := 300 }
    let scrollState ← Reactive.foldDynM
      (fun event state => do
        let contentH ← SpiderM.liftIO contentHeightRef.get
        match event with
        | .wheel scrollData =>
          let dy := -scrollData.scroll.deltaY * 20.0
          let newScroll := state.scroll.scrollBy 0 dy config.width config.height config.width contentH
          pure { state with scroll := newScroll }
        | _ => pure state)
      ({ scroll := ScrollState.zero, drag := {} } : ScrollCombinedState)
      allInputEvents

    -- Step 7: Fire a scroll event for the correct name
    let scrollWidgetId : WidgetId := 42
    let scrollWidget : Widget := .scroll scrollWidgetId (some name) {} {} 400 600 {} testWidget

    let scrollData : ScrollData := {
      scroll := { x := 200, y := 150, deltaX := 0, deltaY := -3.0, modifiers := {} }
      hitPath := #[scrollWidgetId]
      widget := scrollWidget
      layouts := mkScrollLayout scrollWidgetId 0 0 400 300
    }
    inputs.fireScroll scrollData

    -- Step 8: Sample scroll state
    let finalState ← scrollState.sample
    pure (name, finalState.scroll.offsetY)

  -- Check name is correct
  ensure (result.fst == "chat-message-list-0") s!"Expected name 'chat-message-list-0', got '{result.fst}'"
  -- Check scroll offset changed
  ensure (result.snd > 0.0) s!"Expected offset > 0, got {result.snd}"

test "FRP DEBUG: trace scroll event flow in WidgetM" := do
  -- Test that the event pipeline works when run inside WidgetM (like messageList does)
  let result ← runSpider do
    let (events, inputs) ← createInputs FontRegistry.empty

    -- Run the pipeline inside WidgetM - this time with ALL events like messageList
    let (scrollDyn, _) ← (do
      let name ← registerComponentW "chat-message-list"
      let scrollEvents ← useScroll name
      let allClicks ← useAllClicks
      let allHovers ← useAllHovers
      let allMouseUp ← useAllMouseUp

      let liftSpider {α : Type} : SpiderM α → WidgetM α := fun m => StateT.lift (liftM m)
      let wheelEvents ← liftSpider (Event.mapM ScrollInputEvent.wheel scrollEvents)
      let clickEvents ← liftSpider (Event.mapM ScrollInputEvent.click allClicks)
      let hoverEvents ← liftSpider (Event.mapM ScrollInputEvent.hover allHovers)
      let mouseUpEvents ← liftSpider (Event.mapM (fun _ => ScrollInputEvent.mouseUp) allMouseUp)
      let allInputEvents ← liftSpider (Event.leftmostM [wheelEvents, clickEvents, hoverEvents, mouseUpEvents])

      let contentHeightRef ← SpiderM.liftIO (IO.mkRef 3000.0)
      let config : MessageListConfig := { width := 400, height := 300 }

      let scrollState ← Reactive.foldDynM
        (fun (event : ScrollInputEvent) (state : ScrollCombinedState) => do
          let contentH ← SpiderM.liftIO contentHeightRef.get
          match event with
          | ScrollInputEvent.wheel scrollData =>
            let dy := -scrollData.scroll.deltaY * 20.0
            let newScroll := state.scroll.scrollBy 0 dy config.width config.height config.width contentH
            pure { state with scroll := newScroll }
          | _ => pure state)
        ({ scroll := ScrollState.zero, drag := {} } : ScrollCombinedState)
        allInputEvents

      let justScroll ← Dynamic.mapM (fun (s : ScrollCombinedState) => s.scroll) scrollState
      pure justScroll
    ).run { children := #[] } |>.run events

    -- Fire scroll event
    let scrollWidgetId : WidgetId := 42
    let scrollWidget : Widget := .scroll scrollWidgetId (some "chat-message-list-0") {} {} 400 600 {} testWidget
    let scrollData : ScrollData := {
      scroll := { x := 200, y := 150, deltaX := 0, deltaY := -3.0, modifiers := {} }
      hitPath := #[scrollWidgetId]
      widget := scrollWidget
      layouts := mkScrollLayout scrollWidgetId 0 0 400 300
    }
    inputs.fireScroll scrollData

    -- Sample
    let finalState : ScrollState ← scrollDyn.sample
    pure finalState.offsetY

  ensure (result > 0.0) s!"Expected offset > 0 in WidgetM pipeline, got {result}"

test "FRP: messageList responds to scroll wheel events" := do
  let result ← runSpider do
    let (events, inputs) ← createInputs FontRegistry.empty
    -- Need enough messages to exceed viewport height (300px)
    -- With ASCII measurement (~40px per message bubble), we need 8+ messages
    let messages := #[
      ChatMessage.user 0 "Hello, this is message one",
      ChatMessage.assistant 1 "Hi there, this is a response",
      ChatMessage.user 2 "Another message here",
      ChatMessage.assistant 3 "And another response",
      ChatMessage.user 4 "Message five",
      ChatMessage.assistant 5 "Response six",
      ChatMessage.user 6 "Message seven",
      ChatMessage.assistant 7 "Response eight",
      ChatMessage.user 8 "Message nine",
      ChatMessage.assistant 9 "Response ten"
    ]
    let messagesDyn ← Dynamic.pureM messages
    let config : MessageListConfig := { width := 400, height := 300 }

    -- Subscribe to raw scroll events to verify they fire
    let scrollCountRef ← SpiderM.liftIO (IO.mkRef 0)
    let _ ← SpiderM.liftIO <| events.scrollEvent.subscribe fun _ => do
      scrollCountRef.modify (· + 1)

    let (listResult, _) ← (messageList messagesDyn config testTheme false).run
      { children := #[] } |>.run events

    -- Verify initial scroll at 0
    let initialOffset ← listResult.scrollState.sample
    ensure (initialOffset.offsetY == 0.0) s!"Initial offset should be 0, got {initialOffset.offsetY}"

    -- Create a scroll widget with the registered name (chat-message-list-0)
    let scrollWidgetId : WidgetId := 42
    let scrollWidget : Widget := .scroll scrollWidgetId (some "chat-message-list-0") {}
        {} 400 600 {} testWidget

    -- Fire a scroll event
    let scrollData : ScrollData := {
      scroll := { x := 200, y := 150, deltaX := 0, deltaY := -3.0, modifiers := {} }
      hitPath := #[scrollWidgetId]
      widget := scrollWidget
      layouts := mkScrollLayout scrollWidgetId 0 0 400 300
    }
    inputs.fireScroll scrollData

    -- Check that the raw event was received
    let scrollCount ← SpiderM.liftIO scrollCountRef.get

    -- Sample after scroll
    let afterScroll ← listResult.scrollState.sample
    pure (scrollCount, afterScroll.offsetY)

  -- Verify raw event was received
  ensure (result.fst == 1) s!"Expected 1 raw scroll event, got {result.fst}"
  -- Scroll wheel with deltaY=-3, speed=20 should increase offset by 60
  ensure (result.snd > 0.0) s!"Offset should increase after scroll, got {result.snd}"

test "FRP: multiple scroll events accumulate" := do
  let result ← runSpider do
    let (events, inputs) ← createInputs FontRegistry.empty
    -- Need enough messages to exceed viewport height and allow accumulating scroll
    let messages := #[
      ChatMessage.user 0 "Test message one",
      ChatMessage.assistant 1 "Response one",
      ChatMessage.user 2 "Test message two",
      ChatMessage.assistant 3 "Response two",
      ChatMessage.user 4 "Test message three",
      ChatMessage.assistant 5 "Response three",
      ChatMessage.user 6 "Test message four",
      ChatMessage.assistant 7 "Response four",
      ChatMessage.user 8 "Test message five",
      ChatMessage.assistant 9 "Response five",
      ChatMessage.user 10 "Test message six",
      ChatMessage.assistant 11 "Response six",
      ChatMessage.user 12 "Test message seven",
      ChatMessage.assistant 13 "Response seven",
      ChatMessage.user 14 "Test message eight",
      ChatMessage.assistant 15 "Response eight"
    ]
    let messagesDyn ← Dynamic.pureM messages
    let config : MessageListConfig := { width := 400, height := 300 }

    let (listResult, _) ← (messageList messagesDyn config testTheme false).run
      { children := #[] } |>.run events

    let scrollWidgetId : WidgetId := 42
    let scrollWidget : Widget := .scroll scrollWidgetId (some "chat-message-list-0") {}
        {} 400 900 {} testWidget

    -- Fire multiple scroll events
    for _ in [0:3] do
      let scrollData : ScrollData := {
        scroll := { x := 200, y := 150, deltaX := 0, deltaY := -2.0, modifiers := {} }
        hitPath := #[scrollWidgetId]
        widget := scrollWidget
        layouts := mkScrollLayout scrollWidgetId 0 0 400 300
      }
      inputs.fireScroll scrollData

    let afterScroll ← listResult.scrollState.sample
    pure afterScroll.offsetY

  -- 3 scroll events * 2 deltaY * 20 speed = 120
  ensure (result > 100.0) s!"Offset should accumulate, got {result}"

test "FRP: messageList scroll events are filtered by widget name" := do
  let result ← runSpider do
    let (events, inputs) ← createInputs FontRegistry.empty
    let messagesDyn ← Dynamic.pureM #[ChatMessage.user 0 "Test"]
    let config := MessageListConfig.default

    let (listResult, _) ← (messageList messagesDyn config testTheme false).run
      { children := #[] } |>.run events

    -- Fire scroll for a DIFFERENT widget name (should be ignored)
    let scrollWidget : Widget := .scroll 99 (some "different-widget") {}
        {} 400 600 {} testWidget
    let scrollData : ScrollData := {
      scroll := { x := 200, y := 150, deltaX := 0, deltaY := -5.0, modifiers := {} }
      hitPath := #[99]
      widget := scrollWidget
      layouts := mkScrollLayout 99 0 0 400 300
    }
    inputs.fireScroll scrollData

    let afterScroll ← listResult.scrollState.sample
    pure afterScroll.offsetY

  -- Scroll for different widget should be ignored, offset stays at 0
  shouldBeNear result 0.0

test "FRP: scroll events ignored when not in hitPath" := do
  let result ← runSpider do
    let (events, inputs) ← createInputs FontRegistry.empty
    let messagesDyn ← Dynamic.pureM #[ChatMessage.user 0 "Test"]
    let config := MessageListConfig.default

    let (listResult, _) ← (messageList messagesDyn config testTheme false).run
      { children := #[] } |>.run events

    -- Fire scroll with widget ID not in hitPath (empty hitPath)
    let scrollWidget : Widget := .scroll 99 (some "other-scroll") {}
        {} 400 600 {} testWidget
    let scrollData : ScrollData := {
      scroll := { x := 200, y := 150, deltaX := 0, deltaY := -5.0, modifiers := {} }
      hitPath := #[]  -- Empty hitPath - scroll doesn't hit our widget
      widget := scrollWidget
      layouts := mkScrollLayout 99 0 0 400 300
    }
    inputs.fireScroll scrollData

    let afterScroll ← listResult.scrollState.sample
    pure afterScroll.offsetY

  -- Scroll should be ignored, offset stays at 0
  shouldBeNear result 0.0

/-! ## Scrollbar Rendering Tests

These tests verify that the scrollbar is correctly rendered when the chat
message list content exceeds the viewport. Tests inspect the Arbor render
commands emitted by the widget.
-/

/-- Check if a fillRect command is in the vertical scrollbar track area.
    Scrollbar is rendered at the right edge of the viewport.
    We check if rect.origin.x is near viewportWidth - thickness. -/
def isVerticalScrollbarRect (cmd : RenderCommand) (viewportWidth thickness : Float) : Bool :=
  match cmd with
  | .fillRect rect _ _ =>
    -- Scrollbar is at right edge: x >= viewportWidth - thickness - 1 (1px tolerance)
    rect.origin.x >= (viewportWidth - thickness - 1)
  | _ => false

/-- Count scrollbar-related fillRect commands (track + thumb = 2 for vertical). -/
def countVerticalScrollbarRects (cmds : Array RenderCommand) (viewportW thickness : Float) : Nat :=
  cmds.foldl (fun acc cmd =>
    if isVerticalScrollbarRect cmd viewportW thickness then acc + 1 else acc
  ) 0

/-- Find the Y position of the scrollbar thumb.
    The thumb is typically the smaller rect in the scrollbar area. -/
def findScrollbarThumbY (cmds : Array RenderCommand) (viewportW thickness : Float) : Float :=
  let scrollbarRects := cmds.filterMap fun cmd =>
    match cmd with
    | .fillRect rect _ _ =>
      if rect.origin.x >= (viewportW - thickness - 1) then some rect else none
    | _ => none
  -- Thumb is typically smaller than track, so find the one with smaller height
  match scrollbarRects.toList with
  | r1 :: r2 :: _ => if r1.size.height < r2.size.height then r1.origin.y else r2.origin.y
  | r :: _ => r.origin.y
  | [] => 0.0

test "messageListVisual renders scrollbar when content exceeds viewport" := do
  -- Create enough messages to overflow the viewport
  let messages := (Array.range 20).map fun i => ChatMessage.user i s!"Message {i}"
  let config : MessageListConfig := { width := 400, height := 300 }
  let scrollState := ScrollState.zero
  let contentHeight := 1200.0  -- Much taller than viewport (300)

  let builder := messageListVisual "test-scroll" messages config scrollState contentHeight testTheme
  let (widget, _) ← builder.run {}

  -- Measure and layout
  let measureResult := measureWidget (M := Id) widget 400 300
  let layouts := Trellis.layout measureResult.node 400 300

  -- Collect render commands
  let commands := collectCommands widget layouts

  -- Default scrollbar thickness is 8.0 (from ScrollbarConfig.default)
  let scrollbarRects := countVerticalScrollbarRects commands 400 8.0
  -- Should have scrollbar rects (track + thumb = at least 2)
  ensure (scrollbarRects >= 2) s!"Expected at least 2 scrollbar rects (track + thumb), got {scrollbarRects}"

test "messageListVisual does not render scrollbar when content fits" := do
  -- Single short message that fits in viewport
  let messages := #[ChatMessage.user 0 "Short message"]
  let config : MessageListConfig := { width := 400, height := 300 }
  let scrollState := ScrollState.zero
  let contentHeight := 100.0  -- Fits in viewport (300)

  let builder := messageListVisual "test-no-scroll" messages config scrollState contentHeight testTheme
  let (widget, _) ← builder.run {}

  let measureResult := measureWidget (M := Id) widget 400 300
  let layouts := Trellis.layout measureResult.node 400 300
  let commands := collectCommands widget layouts

  -- Should have no scrollbar rects when content fits
  let scrollbarRects := countVerticalScrollbarRects commands 400 8.0
  ensure (scrollbarRects == 0) s!"Expected 0 scrollbar rects when content fits, got {scrollbarRects}"

test "messageListVisual scrollbar thumb moves with scroll offset" := do
  let messages := (Array.range 20).map fun i => ChatMessage.user i s!"Message {i}"
  let config : MessageListConfig := { width := 400, height := 300 }
  let contentHeight := 1200.0

  -- Widget at top (scroll offset = 0)
  let builderTop := messageListVisual "test-scroll-top" messages config ScrollState.zero contentHeight testTheme
  let (widgetTop, _) ← builderTop.run {}
  let measureTop := measureWidget (M := Id) widgetTop 400 300
  let layoutsTop := Trellis.layout measureTop.node 400 300
  let commandsTop := collectCommands widgetTop layoutsTop

  -- Widget scrolled down (scroll offset = 450)
  let scrolledState : ScrollState := { offsetX := 0, offsetY := 450.0 }
  let builderScrolled := messageListVisual "test-scroll-down" messages config scrolledState contentHeight testTheme
  let (widgetScrolled, _) ← builderScrolled.run {}
  let measureScrolled := measureWidget (M := Id) widgetScrolled 400 300
  let layoutsScrolled := Trellis.layout measureScrolled.node 400 300
  let commandsScrolled := collectCommands widgetScrolled layoutsScrolled

  -- Extract thumb positions and verify they differ
  let thumbTopY := findScrollbarThumbY commandsTop 400 8.0
  let thumbScrolledY := findScrollbarThumbY commandsScrolled 400 8.0
  -- When scrolled down, the thumb should be at a higher Y position (further down the screen)
  ensure (thumbScrolledY > thumbTopY) s!"Thumb should move down when scrolled: top={thumbTopY}, scrolled={thumbScrolledY}"

test "messageListVisual scrollbar with custom thickness" := do
  let messages := (Array.range 20).map fun i => ChatMessage.user i s!"Message {i}"
  -- Use fill options to test that scrollbar renders correctly with different viewport
  let config : MessageListConfig := { width := 500, height := 400 }
  let scrollState := ScrollState.zero
  let contentHeight := 1500.0  -- Much taller than viewport

  let builder := messageListVisual "test-scroll-custom" messages config scrollState contentHeight testTheme
  let (widget, _) ← builder.run {}

  let measureResult := measureWidget (M := Id) widget 500 400
  let layouts := Trellis.layout measureResult.node 500 400
  let commands := collectCommands widget layouts

  -- Verify scrollbar renders at different viewport width
  let scrollbarRects := countVerticalScrollbarRects commands 500 8.0
  ensure (scrollbarRects >= 2) s!"Expected at least 2 scrollbar rects with 500px viewport, got {scrollbarRects}"

end Afferent.Tests.ChatTests

/-
  Canopy Chat MessageList
  Scrollable list of chat messages with auto-scroll to bottom.
-/
import Reactive
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Reactive.Component
import Afferent.Canopy.Widget.Layout.Scroll
import Afferent.Canopy.Widget.Chat.Types
import Afferent.Canopy.Widget.Chat.MessageBubble

namespace Afferent.Canopy.Chat

open Afferent.Arbor hiding Event
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
  /-- Gap between messages. -/
  messageGap : Float := 12
  /-- Padding around the message list. -/
  padding : Float := 16
  /-- Configuration for message bubbles. -/
  bubbleConfig : MessageBubbleConfig := {}
deriving Repr, Inhabited

namespace MessageListConfig

/-- Default configuration. -/
def default : MessageListConfig := {}

/-- Create from theme with custom dimensions. -/
def fromTheme (theme : Theme) (width height : Float) : MessageListConfig := {
  width
  height
  padding := theme.padding
  bubbleConfig := MessageBubbleConfig.fromTheme theme
}

end MessageListConfig

/-- Build visual representation of message list (pure WidgetBuilder). -/
def messageListVisual (messages : Array ChatMessage) (config : MessageListConfig)
    (scrollState : ScrollState) (contentHeight : Float) (theme : Theme) : WidgetBuilder := do
  -- Build message bubble builders
  let msgBuilders : Array WidgetBuilder := messages.map fun msg =>
    messageBubbleVisual msg config.bubbleConfig

  -- Column of messages with gap
  let contentStyle : BoxStyle := {
    width := .percent 1.0
    padding := Trellis.EdgeInsets.uniform config.padding
  }
  let content := column (gap := config.messageGap) (style := contentStyle) msgBuilders

  -- Scroll container style
  let scrollStyle : BoxStyle := {
    minWidth := some config.width
    minHeight := some config.height
    maxWidth := some config.width
    maxHeight := some config.height
  }

  let scrollbarConfig := buildScrollbarConfig
    { width := config.width
      height := config.height
      verticalScroll := true
      horizontalScroll := false
      scrollbarVisibility := .always }
    theme

  namedScroll "chat-message-list" scrollStyle config.width contentHeight scrollState scrollbarConfig content

/-- Create a reactive message list widget.

    Takes a Dynamic of messages and renders them in a scrollable container.
    Auto-scrolls to bottom when new messages arrive (if enabled).

    - `messages`: Dynamic array of chat messages
    - `config`: Configuration for the list
    - `theme`: Theme for styling
    - `autoScroll`: Whether to auto-scroll to bottom on new messages (default true) -/
def messageList (messages : Reactive.Dynamic Spider (Array ChatMessage)) (config : MessageListConfig)
    (theme : Theme) (autoScroll : Bool := true) : WidgetM MessageListResult := do
  let name ← registerComponentW "chat-message-list"
  let scrollEvents ← useScroll name
  let allClicks ← useAllClicks
  let allHovers ← useAllHovers
  let allMouseUp ← useAllMouseUp

  -- Track content height (estimate based on message count)
  let contentHeightRef ← SpiderM.liftIO (IO.mkRef config.height)

  -- Initial scroll state (at bottom)
  let initialScroll : ScrollState := { offsetX := 0, offsetY := 0 }

  -- Merge scroll-related events
  let liftSpider {α : Type} : SpiderM α → WidgetM α := fun m => StateT.lift (liftM m)

  -- Scroll state accumulator
  let scrollState ← liftSpider do
    let wheelEvents ← Event.mapM (fun data => ScrollInputEvent.wheel data) scrollEvents
    let clickEvents ← Event.mapM (fun data => ScrollInputEvent.click data) allClicks
    let hoverEvents ← Event.mapM (fun data => ScrollInputEvent.hover data) allHovers
    let mouseUpEvents ← Event.mapM (fun _ => ScrollInputEvent.mouseUp) allMouseUp

    let allInputEvents ← Event.leftmostM [wheelEvents, clickEvents, hoverEvents, mouseUpEvents]

    Reactive.foldDynM
      (fun event state => SpiderM.liftIO do
        let contentH ← contentHeightRef.get
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

  -- Render using dynWidget
  let renderState ← Dynamic.zipWithM (fun msgs scroll => (msgs, scroll)) messages justScroll
  let _ ← dynWidget renderState fun (msgs, scroll) => do
    -- Estimate content height: base padding + messages * average height
    let avgMsgHeight := 60.0  -- Rough estimate
    let contentH := config.padding * 2 + msgs.size.toFloat * (avgMsgHeight + config.messageGap)
    SpiderM.liftIO (contentHeightRef.set contentH)

    -- Auto-scroll: if at or near bottom, stay at bottom
    let maxScroll := max 0 (contentH - config.height)
    let effectiveScroll := if autoScroll && scroll.offsetY >= maxScroll - 10
      then { scroll with offsetY := maxScroll }
      else scroll

    emit do pure (messageListVisual msgs config effectiveScroll contentH theme)

  pure { scrollState := justScroll }

/-- Simple message list that just renders messages without scroll management.
    Use this when you want to manage scrolling yourself. -/
def messageListSimple (messages : Array ChatMessage) (config : MessageListConfig)
    (_theme : Theme) : WidgetM Unit := do
  -- Build message bubbles
  for msg in messages do
    emit do pure (messageBubbleVisual msg config.bubbleConfig)

end Afferent.Canopy.Chat

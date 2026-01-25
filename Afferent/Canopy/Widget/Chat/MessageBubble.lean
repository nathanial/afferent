/-
  Canopy Chat MessageBubble
  Visual representation of a single chat message.
-/
import Afferent.Arbor
import Afferent.Canopy.Theme
import Afferent.Canopy.Widget.Chat.Types

namespace Afferent.Canopy.Chat

open Afferent.Arbor

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

namespace MessageBubbleColors

/-- Default colors for dark theme. -/
def forDarkTheme : MessageBubbleColors := {
  userBackground := Color.fromRgb8 59 130 246      -- Blue-500
  assistantBackground := Color.gray 0.18
  userText := Color.white
  assistantText := Color.gray 0.9
}

/-- Default colors for light theme. -/
def forLightTheme : MessageBubbleColors := {
  userBackground := Color.fromRgb8 59 130 246
  assistantBackground := Color.gray 0.92
  userText := Color.white
  assistantText := Color.gray 0.1
}

/-- Create colors from a theme. -/
def fromTheme (theme : Theme) : MessageBubbleColors := {
  userBackground := theme.primary.background
  assistantBackground := theme.panel.background
  userText := theme.primary.foreground
  assistantText := theme.text
}

end MessageBubbleColors

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

namespace MessageBubbleConfig

/-- Default configuration. -/
def default : MessageBubbleConfig := {}

/-- Create a config from theme. -/
def fromTheme (theme : Theme) : MessageBubbleConfig := {
  padding := theme.padding
  cornerRadius := theme.cornerRadius
  font := theme.font
  colors := MessageBubbleColors.fromTheme theme
}

end MessageBubbleConfig

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
  let rowStyle : BoxStyle := { width := .percent 1.0 }
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

end Afferent.Canopy.Chat

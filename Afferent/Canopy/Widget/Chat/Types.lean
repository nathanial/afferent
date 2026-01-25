/-
  Canopy Chat Widget Types
  Core types for AI chat interface.
-/
import Afferent.Canopy.Core
import Afferent.Canopy.Theme

namespace Afferent.Canopy.Chat

/-- Role of a message in the chat. -/
inductive ChatRole where
  | user
  | assistant
  | system
deriving Repr, BEq, Inhabited

namespace ChatRole

def toString : ChatRole → String
  | .user => "user"
  | .assistant => "assistant"
  | .system => "system"

def isUser : ChatRole → Bool
  | .user => true
  | _ => false

def isAssistant : ChatRole → Bool
  | .assistant => true
  | _ => false

end ChatRole

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

namespace ChatMessage

/-- Create a user message. -/
def user (id : Nat) (content : String) : ChatMessage :=
  { id, role := .user, content }

/-- Create an assistant message. -/
def assistant (id : Nat) (content : String) (isStreaming : Bool := false) : ChatMessage :=
  { id, role := .assistant, content, isStreaming }

/-- Create a system message. -/
def system (id : Nat) (content : String) : ChatMessage :=
  { id, role := .system, content }

/-- Update the content of a streaming message. -/
def updateContent (msg : ChatMessage) (newContent : String) : ChatMessage :=
  { msg with content := newContent }

/-- Mark a message as finished streaming. -/
def finishStreaming (msg : ChatMessage) : ChatMessage :=
  { msg with isStreaming := false }

end ChatMessage

/-- Configuration for the chat widget. -/
structure ChatWidgetConfig where
  /-- Width of the chat widget in pixels. -/
  width : Float := 600
  /-- Height of the chat widget in pixels. -/
  height : Float := 500
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

namespace ChatWidgetConfig

/-- Default configuration. -/
def default : ChatWidgetConfig := {}

/-- Create a configuration with custom dimensions. -/
def withSize (width height : Float) : ChatWidgetConfig :=
  { width, height }

/-- Create a configuration with a system prompt. -/
def withSystemPrompt (prompt : String) : ChatWidgetConfig :=
  { systemPrompt := some prompt }

end ChatWidgetConfig

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

namespace ChatWidgetState

instance : BEq ChatWidgetState where
  beq a b := match a, b with
    | .idle, .idle => true
    | .sending, .sending => true
    | .streaming, .streaming => true
    | .error m1, .error m2 => m1 == m2
    | _, _ => false

def isIdle : ChatWidgetState → Bool
  | .idle => true
  | _ => false

def isBusy : ChatWidgetState → Bool
  | .sending => true
  | .streaming => true
  | _ => false

def isError : ChatWidgetState → Bool
  | .error _ => true
  | _ => false

def errorMessage : ChatWidgetState → Option String
  | .error msg => some msg
  | _ => none

end ChatWidgetState

end Afferent.Canopy.Chat

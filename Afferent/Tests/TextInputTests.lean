/-
  TextInput Widget Tests
  Unit tests for TextInputState operations and handleKeyPress.
-/
import Afferent.Tests.Framework
import Afferent.Canopy.Widget.Input.TextInput
import Afferent.Arbor.Event.Types

namespace Afferent.Tests.TextInputTests

open Crucible
open Afferent.Tests
open Afferent.Canopy
open Afferent.Arbor

testSuite "TextInput Tests"

/-! ## TextInputState Basic Operations -/

test "insertChar inserts at cursor position" := do
  let state : TextInputState := { value := "hello", cursor := 2 }
  let result := state.insertChar 'X'
  ensure (result.value == "heXllo") s!"Expected 'heXllo', got '{result.value}'"
  ensure (result.cursor == 3) s!"Expected cursor 3, got {result.cursor}"

test "insertChar at start" := do
  let state : TextInputState := { value := "hello", cursor := 0 }
  let result := state.insertChar 'X'
  ensure (result.value == "Xhello") s!"Expected 'Xhello', got '{result.value}'"
  ensure (result.cursor == 1) s!"Expected cursor 1, got {result.cursor}"

test "insertChar at end" := do
  let state : TextInputState := { value := "hello", cursor := 5 }
  let result := state.insertChar 'X'
  ensure (result.value == "helloX") s!"Expected 'helloX', got '{result.value}'"
  ensure (result.cursor == 6) s!"Expected cursor 6, got {result.cursor}"

test "insertChar into empty string" := do
  let state : TextInputState := { value := "", cursor := 0 }
  let result := state.insertChar 'a'
  ensure (result.value == "a") s!"Expected 'a', got '{result.value}'"
  ensure (result.cursor == 1) s!"Expected cursor 1, got {result.cursor}"

/-! ## Delete Operations -/

test "deleteBackward removes char before cursor" := do
  let state : TextInputState := { value := "hello", cursor := 3 }
  let result := state.deleteBackward
  ensure (result.value == "helo") s!"Expected 'helo', got '{result.value}'"
  ensure (result.cursor == 2) s!"Expected cursor 2, got {result.cursor}"

test "deleteBackward at start does nothing" := do
  let state : TextInputState := { value := "hello", cursor := 0 }
  let result := state.deleteBackward
  ensure (result.value == "hello") s!"Expected 'hello', got '{result.value}'"
  ensure (result.cursor == 0) s!"Expected cursor 0, got {result.cursor}"

test "deleteForward removes char at cursor" := do
  let state : TextInputState := { value := "hello", cursor := 2 }
  let result := state.deleteForward
  ensure (result.value == "helo") s!"Expected 'helo', got '{result.value}'"
  ensure (result.cursor == 2) s!"Expected cursor 2, got {result.cursor}"

test "deleteForward at end does nothing" := do
  let state : TextInputState := { value := "hello", cursor := 5 }
  let result := state.deleteForward
  ensure (result.value == "hello") s!"Expected 'hello', got '{result.value}'"
  ensure (result.cursor == 5) s!"Expected cursor 5, got {result.cursor}"

/-! ## Bulk Delete Operations -/

test "deleteToEnd removes text after cursor" := do
  let state : TextInputState := { value := "hello world", cursor := 5 }
  let result := state.deleteToEnd
  ensure (result.value == "hello") s!"Expected 'hello', got '{result.value}'"
  ensure (result.cursor == 5) s!"Expected cursor 5, got {result.cursor}"

test "deleteToEnd at end does nothing" := do
  let state : TextInputState := { value := "hello", cursor := 5 }
  let result := state.deleteToEnd
  ensure (result.value == "hello") s!"Expected 'hello', got '{result.value}'"
  ensure (result.cursor == 5) s!"Expected cursor 5, got {result.cursor}"

test "deleteToEnd at start clears all" := do
  let state : TextInputState := { value := "hello", cursor := 0 }
  let result := state.deleteToEnd
  ensure (result.value == "") s!"Expected '', got '{result.value}'"
  ensure (result.cursor == 0) s!"Expected cursor 0, got {result.cursor}"

test "deleteToStart removes text before cursor" := do
  let state : TextInputState := { value := "hello world", cursor := 6 }
  let result := state.deleteToStart
  ensure (result.value == "world") s!"Expected 'world', got '{result.value}'"
  ensure (result.cursor == 0) s!"Expected cursor 0, got {result.cursor}"

test "deleteToStart at start does nothing" := do
  let state : TextInputState := { value := "hello", cursor := 0 }
  let result := state.deleteToStart
  ensure (result.value == "hello") s!"Expected 'hello', got '{result.value}'"
  ensure (result.cursor == 0) s!"Expected cursor 0, got {result.cursor}"

test "deleteToStart at end clears all" := do
  let state : TextInputState := { value := "hello", cursor := 5 }
  let result := state.deleteToStart
  ensure (result.value == "") s!"Expected '', got '{result.value}'"
  ensure (result.cursor == 0) s!"Expected cursor 0, got {result.cursor}"

/-! ## Cursor Movement -/

test "moveCursorLeft decrements cursor" := do
  let state : TextInputState := { value := "hello", cursor := 3 }
  let result := state.moveCursorLeft
  ensure (result.cursor == 2) s!"Expected cursor 2, got {result.cursor}"

test "moveCursorLeft at start does nothing" := do
  let state : TextInputState := { value := "hello", cursor := 0 }
  let result := state.moveCursorLeft
  ensure (result.cursor == 0) s!"Expected cursor 0, got {result.cursor}"

test "moveCursorRight increments cursor" := do
  let state : TextInputState := { value := "hello", cursor := 3 }
  let result := state.moveCursorRight
  ensure (result.cursor == 4) s!"Expected cursor 4, got {result.cursor}"

test "moveCursorRight at end does nothing" := do
  let state : TextInputState := { value := "hello", cursor := 5 }
  let result := state.moveCursorRight
  ensure (result.cursor == 5) s!"Expected cursor 5, got {result.cursor}"

test "moveCursorStart moves to position 0" := do
  let state : TextInputState := { value := "hello", cursor := 3 }
  let result := state.moveCursorStart
  ensure (result.cursor == 0) s!"Expected cursor 0, got {result.cursor}"

test "moveCursorEnd moves to string length" := do
  let state : TextInputState := { value := "hello", cursor := 2 }
  let result := state.moveCursorEnd
  ensure (result.cursor == 5) s!"Expected cursor 5, got {result.cursor}"

/-! ## handleKeyPress Character Input -/

test "handleKeyPress inserts lowercase character" := do
  let state : TextInputState := { value := "hi", cursor := 2 }
  let event : KeyEvent := { key := .char 'a', modifiers := {} }
  let result := TextInput.handleKeyPress event state none
  ensure (result.value == "hia") s!"Expected 'hia', got '{result.value}'"

test "handleKeyPress with shift inserts uppercase character" := do
  let state : TextInputState := { value := "hi", cursor := 2 }
  let event : KeyEvent := { key := .char 'a', modifiers := { shift := true } }
  let result := TextInput.handleKeyPress event state none
  ensure (result.value == "hiA") s!"Expected 'hiA', got '{result.value}'"

test "handleKeyPress space key inserts space" := do
  let state : TextInputState := { value := "hi", cursor := 2 }
  let event : KeyEvent := { key := .space, modifiers := {} }
  let result := TextInput.handleKeyPress event state none
  ensure (result.value == "hi ") s!"Expected 'hi ', got '{result.value}'"

/-! ## handleKeyPress Punctuation and Symbols -/

test "handleKeyPress inserts comma" := do
  let state : TextInputState := { value := "hi", cursor := 2 }
  let event : KeyEvent := { key := .char ',', modifiers := {} }
  let result := TextInput.handleKeyPress event state none
  ensure (result.value == "hi,") s!"Expected 'hi,', got '{result.value}'"

test "handleKeyPress shift+comma inserts less-than" := do
  let state : TextInputState := { value := "a", cursor := 1 }
  let event : KeyEvent := { key := .char ',', modifiers := { shift := true } }
  let result := TextInput.handleKeyPress event state none
  ensure (result.value == "a<") s!"Expected 'a<', got '{result.value}'"

test "handleKeyPress inserts period" := do
  let state : TextInputState := { value := "hi", cursor := 2 }
  let event : KeyEvent := { key := .char '.', modifiers := {} }
  let result := TextInput.handleKeyPress event state none
  ensure (result.value == "hi.") s!"Expected 'hi.', got '{result.value}'"

test "handleKeyPress shift+period inserts greater-than" := do
  let state : TextInputState := { value := "a", cursor := 1 }
  let event : KeyEvent := { key := .char '.', modifiers := { shift := true } }
  let result := TextInput.handleKeyPress event state none
  ensure (result.value == "a>") s!"Expected 'a>', got '{result.value}'"

test "handleKeyPress inserts brackets" := do
  let state : TextInputState := { value := "", cursor := 0 }
  let event1 : KeyEvent := { key := .char '[', modifiers := {} }
  let event2 : KeyEvent := { key := .char ']', modifiers := {} }
  let result1 := TextInput.handleKeyPress event1 state none
  let result2 := TextInput.handleKeyPress event2 result1 none
  ensure (result2.value == "[]") s!"Expected '[]', got '{result2.value}'"

test "handleKeyPress shift+brackets inserts braces" := do
  let state : TextInputState := { value := "", cursor := 0 }
  let event1 : KeyEvent := { key := .char '[', modifiers := { shift := true } }
  let event2 : KeyEvent := { key := .char ']', modifiers := { shift := true } }
  let result1 := TextInput.handleKeyPress event1 state none
  let result2 := TextInput.handleKeyPress event2 result1 none
  let expected := "{" ++ "}"
  ensure (result2.value == expected) s!"Expected braces, got '{result2.value}'"

test "handleKeyPress shift+3 inserts hash" := do
  let state : TextInputState := { value := "", cursor := 0 }
  let event : KeyEvent := { key := .char '3', modifiers := { shift := true } }
  let result := TextInput.handleKeyPress event state none
  ensure (result.value == "#") s!"Expected '#', got '{result.value}'"

test "handleKeyPress shift+1 inserts exclamation" := do
  let state : TextInputState := { value := "hi", cursor := 2 }
  let event : KeyEvent := { key := .char '1', modifiers := { shift := true } }
  let result := TextInput.handleKeyPress event state none
  ensure (result.value == "hi!") s!"Expected 'hi!', got '{result.value}'"

test "handleKeyPress inserts semicolon and colon" := do
  let state : TextInputState := { value := "", cursor := 0 }
  let event1 : KeyEvent := { key := .char ';', modifiers := {} }
  let result1 := TextInput.handleKeyPress event1 state none
  ensure (result1.value == ";") s!"Expected ';', got '{result1.value}'"
  let event2 : KeyEvent := { key := .char ';', modifiers := { shift := true } }
  let result2 := TextInput.handleKeyPress event2 result1 none
  ensure (result2.value == ";:") s!"Expected ';:', got '{result2.value}'"

test "handleKeyPress inserts minus and underscore" := do
  let state : TextInputState := { value := "a", cursor := 1 }
  let event1 : KeyEvent := { key := .char '-', modifiers := {} }
  let result1 := TextInput.handleKeyPress event1 state none
  ensure (result1.value == "a-") s!"Expected 'a-', got '{result1.value}'"
  let event2 : KeyEvent := { key := .char '-', modifiers := { shift := true } }
  let result2 := TextInput.handleKeyPress event2 result1 none
  ensure (result2.value == "a-_") s!"Expected 'a-_', got '{result2.value}'"

/-! ## handleKeyPress Delete Keys -/

test "handleKeyPress backspace deletes backward" := do
  let state : TextInputState := { value := "hello", cursor := 3 }
  let event : KeyEvent := { key := .backspace, modifiers := {} }
  let result := TextInput.handleKeyPress event state none
  ensure (result.value == "helo") s!"Expected 'helo', got '{result.value}'"
  ensure (result.cursor == 2) s!"Expected cursor 2, got {result.cursor}"

test "handleKeyPress delete key deletes forward" := do
  let state : TextInputState := { value := "hello", cursor := 2 }
  let event : KeyEvent := { key := .delete, modifiers := {} }
  let result := TextInput.handleKeyPress event state none
  ensure (result.value == "helo") s!"Expected 'helo', got '{result.value}'"

/-! ## handleKeyPress Arrow Keys -/

test "handleKeyPress left arrow moves cursor left" := do
  let state : TextInputState := { value := "hello", cursor := 3 }
  let event : KeyEvent := { key := .left, modifiers := {} }
  let result := TextInput.handleKeyPress event state none
  ensure (result.cursor == 2) s!"Expected cursor 2, got {result.cursor}"

test "handleKeyPress right arrow moves cursor right" := do
  let state : TextInputState := { value := "hello", cursor := 3 }
  let event : KeyEvent := { key := .right, modifiers := {} }
  let result := TextInput.handleKeyPress event state none
  ensure (result.cursor == 4) s!"Expected cursor 4, got {result.cursor}"

/-! ## handleKeyPress Command Key Shortcuts -/

test "handleKeyPress cmd+left jumps to start" := do
  let state : TextInputState := { value := "hello world", cursor := 7 }
  let event : KeyEvent := { key := .left, modifiers := { cmd := true } }
  let result := TextInput.handleKeyPress event state none
  ensure (result.cursor == 0) s!"Expected cursor 0, got {result.cursor}"

test "handleKeyPress cmd+right jumps to end" := do
  let state : TextInputState := { value := "hello world", cursor := 3 }
  let event : KeyEvent := { key := .right, modifiers := { cmd := true } }
  let result := TextInput.handleKeyPress event state none
  ensure (result.cursor == 11) s!"Expected cursor 11, got {result.cursor}"

test "handleKeyPress cmd+delete deletes to end" := do
  let state : TextInputState := { value := "hello world", cursor := 5 }
  let event : KeyEvent := { key := .delete, modifiers := { cmd := true } }
  let result := TextInput.handleKeyPress event state none
  ensure (result.value == "hello") s!"Expected 'hello', got '{result.value}'"
  ensure (result.cursor == 5) s!"Expected cursor 5, got {result.cursor}"

test "handleKeyPress cmd+backspace deletes to start" := do
  let state : TextInputState := { value := "hello world", cursor := 6 }
  let event : KeyEvent := { key := .backspace, modifiers := { cmd := true } }
  let result := TextInput.handleKeyPress event state none
  ensure (result.value == "world") s!"Expected 'world', got '{result.value}'"
  ensure (result.cursor == 0) s!"Expected cursor 0, got {result.cursor}"

/-! ## handleKeyPress Ctrl Key Chords -/

test "handleKeyPress ctrl+delete deletes to end" := do
  let state : TextInputState := { value := "hello world", cursor := 5 }
  let event : KeyEvent := { key := .delete, modifiers := { ctrl := true } }
  let result := TextInput.handleKeyPress event state none
  ensure (result.value == "hello") s!"Expected 'hello', got '{result.value}'"
  ensure (result.cursor == 5) s!"Expected cursor 5, got {result.cursor}"

test "handleKeyPress ctrl+backspace deletes to start" := do
  let state : TextInputState := { value := "hello world", cursor := 6 }
  let event : KeyEvent := { key := .backspace, modifiers := { ctrl := true } }
  let result := TextInput.handleKeyPress event state none
  ensure (result.value == "world") s!"Expected 'world', got '{result.value}'"
  ensure (result.cursor == 0) s!"Expected cursor 0, got {result.cursor}"

test "handleKeyPress ctrl+delete at end does nothing" := do
  let state : TextInputState := { value := "hello", cursor := 5 }
  let event : KeyEvent := { key := .delete, modifiers := { ctrl := true } }
  let result := TextInput.handleKeyPress event state none
  ensure (result.value == "hello") s!"Expected 'hello', got '{result.value}'"
  ensure (result.cursor == 5) s!"Expected cursor 5, got {result.cursor}"

test "handleKeyPress ctrl+backspace at start does nothing" := do
  let state : TextInputState := { value := "hello", cursor := 0 }
  let event : KeyEvent := { key := .backspace, modifiers := { ctrl := true } }
  let result := TextInput.handleKeyPress event state none
  ensure (result.value == "hello") s!"Expected 'hello', got '{result.value}'"
  ensure (result.cursor == 0) s!"Expected cursor 0, got {result.cursor}"

/-! ## handleKeyPress maxLen Enforcement -/

test "handleKeyPress respects maxLen for character insertion" := do
  let state : TextInputState := { value := "hello", cursor := 5 }
  let event : KeyEvent := { key := .char 'x', modifiers := {} }
  let result := TextInput.handleKeyPress event state (some 5)
  ensure (result.value == "hello") s!"Expected 'hello', got '{result.value}'"

test "handleKeyPress allows character when under maxLen" := do
  let state : TextInputState := { value := "hell", cursor := 4 }
  let event : KeyEvent := { key := .char 'o', modifiers := {} }
  let result := TextInput.handleKeyPress event state (some 5)
  ensure (result.value == "hello") s!"Expected 'hello', got '{result.value}'"

test "handleKeyPress respects maxLen for space" := do
  let state : TextInputState := { value := "12345", cursor := 5 }
  let event : KeyEvent := { key := .space, modifiers := {} }
  let result := TextInput.handleKeyPress event state (some 5)
  ensure (result.value == "12345") s!"Expected '12345', got '{result.value}'"

/-! ## Home/End Keys -/

test "handleKeyPress home key moves to start" := do
  let state : TextInputState := { value := "hello", cursor := 3 }
  let event : KeyEvent := { key := .home, modifiers := {} }
  let result := TextInput.handleKeyPress event state none
  ensure (result.cursor == 0) s!"Expected cursor 0, got {result.cursor}"

test "handleKeyPress end key moves to end" := do
  let state : TextInputState := { value := "hello", cursor := 2 }
  let event : KeyEvent := { key := .«end», modifiers := {} }
  let result := TextInput.handleKeyPress event state none
  ensure (result.cursor == 5) s!"Expected cursor 5, got {result.cursor}"

end Afferent.Tests.TextInputTests

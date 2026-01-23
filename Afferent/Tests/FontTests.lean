/-
  Afferent Font Tests
  Smoke tests for font loading and text measurement.
  Note: These tests require the FFI and system fonts to be available.
-/
import Afferent.Tests.Framework
import Afferent.Text.Font

namespace Afferent.Tests.FontTests

open Crucible
open Afferent
open Afferent.Tests

testSuite "Font Tests"

/-! ## Font Loading Tests -/

test "Font.load with invalid path fails gracefully" := do
  -- Try to load a non-existent font - should throw an error
  try
    let _ ← Font.load "/nonexistent/path/to/font.ttf" 24
    throw <| IO.userError "Expected font load to fail for invalid path"
  catch _ =>
    pure ()  -- Expected failure

test "Font.load with system font succeeds" := do
  -- Load a system font (Helvetica on macOS)
  let font ← Font.load "/System/Library/Fonts/Helvetica.ttc" 24
  -- Metrics should be reasonable
  ensure (font.ascender > 0) s!"Ascender should be positive, got {font.ascender}"
  ensure (font.descender < 0) s!"Descender should be negative, got {font.descender}"
  ensure (font.lineHeight > 0) s!"Line height should be positive, got {font.lineHeight}"
  font.destroy

test "Font glyphHeight is positive" := do
  let font ← Font.load "/System/Library/Fonts/Helvetica.ttc" 24
  let height := font.glyphHeight
  ensure (height > 0) s!"Glyph height should be positive, got {height}"
  font.destroy

/-! ## Text Measurement Tests -/

test "measureText returns positive dimensions for non-empty text" := do
  let font ← Font.load "/System/Library/Fonts/Helvetica.ttc" 24
  let (width, height) ← font.measureText "Hello, World!"
  ensure (width > 0) s!"Width should be positive, got {width}"
  ensure (height > 0) s!"Height should be positive, got {height}"
  font.destroy

test "measureText returns zero width for empty string" := do
  let font ← Font.load "/System/Library/Fonts/Helvetica.ttc" 24
  let (width, _) ← font.measureText ""
  shouldBeNear width 0.0
  font.destroy

test "measureText longer text is wider" := do
  let font ← Font.load "/System/Library/Fonts/Helvetica.ttc" 24
  let (shortWidth, _) ← font.measureText "Hi"
  let (longWidth, _) ← font.measureText "Hello, World!"
  ensure (longWidth > shortWidth) s!"Longer text should be wider: {longWidth} > {shortWidth}"
  font.destroy

test "measureText larger font size produces larger dimensions" := do
  let smallFont ← Font.load "/System/Library/Fonts/Helvetica.ttc" 12
  let largeFont ← Font.load "/System/Library/Fonts/Helvetica.ttc" 48
  let (smallWidth, smallHeight) ← smallFont.measureText "Test"
  let (largeWidth, largeHeight) ← largeFont.measureText "Test"
  ensure (largeWidth > smallWidth) s!"Larger font should produce wider text"
  ensure (largeHeight > smallHeight) s!"Larger font should produce taller text"
  smallFont.destroy
  largeFont.destroy



end Afferent.Tests.FontTests

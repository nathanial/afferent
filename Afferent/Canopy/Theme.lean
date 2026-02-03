/-
  Canopy Theme
  Color palettes and styling for widget components.
-/
import Afferent.Arbor

namespace Afferent.Canopy

open Afferent.Arbor

/-- Color palette for interactive elements in different states. -/
structure InteractiveColors where
  background : Color
  backgroundHover : Color
  backgroundPressed : Color
  backgroundDisabled : Color
  foreground : Color
  foregroundDisabled : Color
  border : Color
  borderFocused : Color
deriving Repr, BEq, Inhabited

/-- Scrollbar styling colors. -/
structure ScrollbarColors where
  /-- Scrollbar track background. -/
  track : Color
  /-- Scrollbar thumb (normal state). -/
  thumb : Color
  /-- Scrollbar thumb (hovered). -/
  thumbHover : Color
  /-- Scrollbar thumb (dragging). -/
  thumbActive : Color
deriving Repr, BEq, Inhabited

/-- Complete theme for Canopy widgets. -/
structure Theme where
  /-- Primary action buttons (filled, prominent). -/
  primary : InteractiveColors
  /-- Secondary buttons (filled, less prominent). -/
  secondary : InteractiveColors
  /-- Destructive action buttons. -/
  danger : InteractiveColors
  /-- Success/confirm action buttons. -/
  success : InteractiveColors
  /-- Outline-style buttons (border only). -/
  outline : InteractiveColors
  /-- Text input fields. -/
  input : InteractiveColors
  /-- Panel/card backgrounds. -/
  panel : InteractiveColors
  /-- Scrollbar styling. -/
  scrollbar : ScrollbarColors
  /-- Default text color. -/
  text : Color
  /-- Muted/secondary text color. -/
  textMuted : Color
  /-- Focus ring indicator color. -/
  focusRing : Color
  /-- Corner radius for buttons and inputs. -/
  cornerRadius : Float := 6.0
  /-- Default padding for interactive elements. -/
  padding : Float := 12.0
  /-- Default font. -/
  font : FontId := FontId.default
  /-- Small font for captions. -/
  smallFont : FontId := FontId.default.withSize 12
deriving Repr, Inhabited

namespace Theme

/-- Dark theme with blue accent colors. -/
def dark : Theme := {
  primary := {
    background := Color.fromRgb8 59 130 246         -- Blue-500
    backgroundHover := Color.fromRgb8 96 165 250    -- Blue-400
    backgroundPressed := Color.fromRgb8 37 99 235   -- Blue-600
    backgroundDisabled := Color.gray 0.3
    foreground := Color.white
    foregroundDisabled := Color.gray 0.5
    border := Color.fromRgb8 59 130 246
    borderFocused := Color.fromRgb8 147 197 253     -- Blue-300
  }
  secondary := {
    background := Color.gray 0.25
    backgroundHover := Color.gray 0.3
    backgroundPressed := Color.gray 0.2
    backgroundDisabled := Color.gray 0.15
    foreground := Color.white
    foregroundDisabled := Color.gray 0.5
    border := Color.gray 0.4
    borderFocused := Color.gray 0.6
  }
  danger := {
    background := Color.fromRgb8 239 68 68          -- Red-500
    backgroundHover := Color.fromRgb8 248 113 113   -- Red-400
    backgroundPressed := Color.fromRgb8 220 38 38   -- Red-600
    backgroundDisabled := Color.gray 0.3
    foreground := Color.white
    foregroundDisabled := Color.gray 0.5
    border := Color.fromRgb8 239 68 68
    borderFocused := Color.fromRgb8 252 165 165     -- Red-300
  }
  success := {
    background := Color.fromRgb8 34 197 94          -- Green-500
    backgroundHover := Color.fromRgb8 74 222 128    -- Green-400
    backgroundPressed := Color.fromRgb8 22 163 74   -- Green-600
    backgroundDisabled := Color.gray 0.3
    foreground := Color.white
    foregroundDisabled := Color.gray 0.5
    border := Color.fromRgb8 34 197 94
    borderFocused := Color.fromRgb8 134 239 172     -- Green-300
  }
  outline := {
    background := Color.transparent
    backgroundHover := Color.gray 0.1
    backgroundPressed := Color.gray 0.15
    backgroundDisabled := Color.transparent
    foreground := Color.white
    foregroundDisabled := Color.gray 0.5
    border := Color.gray 0.4
    borderFocused := Color.fromRgb8 147 197 253
  }
  input := {
    background := Color.gray 0.1
    backgroundHover := Color.gray 0.12
    backgroundPressed := Color.gray 0.1
    backgroundDisabled := Color.gray 0.05
    foreground := Color.white
    foregroundDisabled := Color.gray 0.5
    border := Color.gray 0.3
    borderFocused := Color.fromRgb8 59 130 246
  }
  panel := {
    background := Color.gray 0.12
    backgroundHover := Color.gray 0.15
    backgroundPressed := Color.gray 0.12
    backgroundDisabled := Color.gray 0.08
    foreground := Color.white
    foregroundDisabled := Color.gray 0.5
    border := Color.gray 0.25
    borderFocused := Color.gray 0.4
  }
  scrollbar := {
    track := Color.gray 0.15
    thumb := Color.gray 0.35
    thumbHover := Color.gray 0.45
    thumbActive := Color.gray 0.55
  }
  text := Color.white
  textMuted := Color.gray 0.6
  focusRing := Color.fromRgb8 59 130 246
  cornerRadius := 6.0
  padding := 12.0
}

/-- Light theme with blue accent colors. -/
def light : Theme := {
  primary := {
    background := Color.fromRgb8 59 130 246
    backgroundHover := Color.fromRgb8 37 99 235
    backgroundPressed := Color.fromRgb8 29 78 216
    backgroundDisabled := Color.gray 0.7
    foreground := Color.white
    foregroundDisabled := Color.gray 0.5
    border := Color.fromRgb8 59 130 246
    borderFocused := Color.fromRgb8 37 99 235
  }
  secondary := {
    background := Color.gray 0.9
    backgroundHover := Color.gray 0.85
    backgroundPressed := Color.gray 0.8
    backgroundDisabled := Color.gray 0.95
    foreground := Color.gray 0.1
    foregroundDisabled := Color.gray 0.5
    border := Color.gray 0.7
    borderFocused := Color.gray 0.5
  }
  danger := {
    background := Color.fromRgb8 239 68 68
    backgroundHover := Color.fromRgb8 220 38 38
    backgroundPressed := Color.fromRgb8 185 28 28
    backgroundDisabled := Color.gray 0.8
    foreground := Color.white
    foregroundDisabled := Color.gray 0.5
    border := Color.fromRgb8 239 68 68
    borderFocused := Color.fromRgb8 220 38 38
  }
  success := {
    background := Color.fromRgb8 34 197 94
    backgroundHover := Color.fromRgb8 22 163 74
    backgroundPressed := Color.fromRgb8 21 128 61
    backgroundDisabled := Color.gray 0.85
    foreground := Color.white
    foregroundDisabled := Color.gray 0.5
    border := Color.fromRgb8 34 197 94
    borderFocused := Color.fromRgb8 22 163 74
  }
  outline := {
    background := Color.transparent
    backgroundHover := Color.gray 0.95
    backgroundPressed := Color.gray 0.9
    backgroundDisabled := Color.transparent
    foreground := Color.gray 0.1
    foregroundDisabled := Color.gray 0.5
    border := Color.gray 0.6
    borderFocused := Color.fromRgb8 59 130 246
  }
  input := {
    background := Color.white
    backgroundHover := Color.gray 0.98
    backgroundPressed := Color.white
    backgroundDisabled := Color.gray 0.95
    foreground := Color.gray 0.1
    foregroundDisabled := Color.gray 0.5
    border := Color.gray 0.6
    borderFocused := Color.fromRgb8 59 130 246
  }
  panel := {
    background := Color.white
    backgroundHover := Color.gray 0.98
    backgroundPressed := Color.white
    backgroundDisabled := Color.gray 0.95
    foreground := Color.gray 0.1
    foregroundDisabled := Color.gray 0.5
    border := Color.gray 0.8
    borderFocused := Color.gray 0.6
  }
  scrollbar := {
    track := Color.gray 0.9
    thumb := Color.gray 0.7
    thumbHover := Color.gray 0.6
    thumbActive := Color.gray 0.5
  }
  text := Color.gray 0.1
  textMuted := Color.gray 0.5
  focusRing := Color.fromRgb8 59 130 246
  cornerRadius := 6.0
  padding := 12.0
}

end Theme

end Afferent.Canopy

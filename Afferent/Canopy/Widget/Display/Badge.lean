/-
  Canopy Badge Widget
  Small status indicators or count displays.
-/
import Afferent.Canopy.Theme
import Afferent.Canopy.Reactive.Component

namespace Afferent.Canopy

open Afferent.Arbor hiding Event
open Reactive Reactive.Host
open Afferent.Canopy.Reactive

/-- Badge visual variants for semantic coloring. -/
inductive BadgeVariant where
  | primary    -- Blue, for primary information
  | secondary  -- Gray, for secondary information
  | success    -- Green, for success states
  | warning    -- Yellow/orange, for warnings
  | error      -- Red, for errors
  | info       -- Cyan, for informational
deriving Repr, BEq, Inhabited

namespace Badge

/-- Get background color for a badge variant. -/
def variantBackgroundColor (theme : Theme) : BadgeVariant → Color
  | .primary   => theme.primary.background
  | .secondary => theme.secondary.background
  | .success   => Color.fromRgb8 34 197 94    -- Green-500
  | .warning   => Color.fromRgb8 234 179 8    -- Yellow-500
  | .error     => Color.fromRgb8 239 68 68    -- Red-500
  | .info      => Color.fromRgb8 6 182 212    -- Cyan-500

/-- Get text color for a badge variant (always white for good contrast). -/
def variantTextColor (_theme : Theme) : BadgeVariant → Color
  | _ => Color.white

/-- Default badge dimensions. -/
structure Dimensions where
  paddingH : Float := 8.0
  paddingV : Float := 2.0
  cornerRadius : Float := 10.0
  fontSize : Float := 12.0
deriving Repr, Inhabited

def defaultDimensions : Dimensions := {}

end Badge

/-- Create a badge with text content.
    - `content`: Text to display in the badge
    - `theme`: Theme for colors
    - `variant`: Color variant (default: primary)
-/
def badge (content : String) (theme : Theme)
    (variant : BadgeVariant := .primary) : WidgetBuilder := do
  let dims := Badge.defaultDimensions
  let bgColor := Badge.variantBackgroundColor theme variant
  let textColor := Badge.variantTextColor theme variant
  let font := theme.smallFont.withSize dims.fontSize

  let style : BoxStyle := {
    backgroundColor := some bgColor
    cornerRadius := dims.cornerRadius
    padding := Trellis.EdgeInsets.symmetric dims.paddingH dims.paddingV
  }

  center (style := style) do
    text' content font textColor .center

/-- Create a badge showing a count.
    - `count`: Number to display
    - `theme`: Theme for colors
    - `variant`: Color variant (default: primary)
    - `maxDisplay`: Maximum count to display before showing "N+" (default: 99)
-/
def badgeCount (count : Nat) (theme : Theme)
    (variant : BadgeVariant := .primary)
    (maxDisplay : Nat := 99) : WidgetBuilder := do
  let displayText := if count > maxDisplay then s!"{maxDisplay}+" else toString count
  badge displayText theme variant

/-- WidgetM wrapper for badge. -/
def badge' (content : String) (variant : BadgeVariant := .primary) : WidgetM Unit := do
  let theme ← getThemeW
  emit (pure (badge content theme variant))

/-- WidgetM wrapper for badgeCount. -/
def badgeCount' (count : Nat) (variant : BadgeVariant := .primary)
    (maxDisplay : Nat := 99) : WidgetM Unit := do
  let theme ← getThemeW
  emit (pure (badgeCount count theme variant maxDisplay))

end Afferent.Canopy

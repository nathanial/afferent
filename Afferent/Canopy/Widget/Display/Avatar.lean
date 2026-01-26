/-
  Canopy Avatar Widget
  Circular user/entity representation with initials.
-/
import Afferent.Canopy.Theme
import Afferent.Canopy.Reactive.Component

namespace Afferent.Canopy

open Afferent.Arbor hiding Event
open Reactive Reactive.Host
open Afferent.Canopy.Reactive

/-- Avatar size options. -/
inductive AvatarSize where
  | small   -- 24px
  | medium  -- 32px
  | large   -- 48px
deriving Repr, BEq, Inhabited

namespace Avatar

/-- Get pixel size for an avatar size. -/
def sizePixels : AvatarSize → Float
  | .small  => 24.0
  | .medium => 32.0
  | .large  => 48.0

/-- Get font size for initials based on avatar size. -/
def fontSize : AvatarSize → Float
  | .small  => 10.0
  | .medium => 14.0
  | .large  => 20.0

/-- Default avatar colors for when no specific color is provided. -/
def defaultColors : Array Color := #[
  Color.fromRgb8 59 130 246,   -- Blue
  Color.fromRgb8 139 92 246,   -- Purple
  Color.fromRgb8 236 72 153,   -- Pink
  Color.fromRgb8 234 179 8,    -- Yellow
  Color.fromRgb8 34 197 94,    -- Green
  Color.fromRgb8 6 182 212     -- Cyan
]

/-- Generate a consistent color from a string (for automatic avatar colors). -/
def colorFromString (s : String) : Color :=
  -- Pick a color from the palette based on string hash
  let idx := (s.hash % defaultColors.size.toUInt64).toNat
  defaultColors.getD idx (Color.fromRgb8 59 130 246)

end Avatar

/-- Create an avatar with initials.
    - `initials`: 1-2 characters to display (typically first letters of name)
    - `theme`: Theme for styling
    - `size`: Avatar size (default: medium)
    - `backgroundColor`: Override background color (auto-generated from initials if not provided)
-/
def avatar (initials : String) (theme : Theme)
    (size : AvatarSize := .medium)
    (backgroundColor : Option Color := none) : WidgetBuilder := do
  let pixels := Avatar.sizePixels size
  let fontSz := Avatar.fontSize size
  let font := theme.font.withSize fontSz
  let bgColor := backgroundColor.getD (Avatar.colorFromString initials)

  let style : BoxStyle := {
    backgroundColor := some bgColor
    cornerRadius := pixels / 2.0  -- Perfect circle
    minWidth := some pixels
    minHeight := some pixels
    maxWidth := some pixels
    maxHeight := some pixels
  }

  center (style := style) do
    text' initials.toUpper font Color.white .center

/-- Create an avatar with a label to the right.
    - `initials`: 1-2 characters for the avatar
    - `label`: Text label to display beside the avatar
    - `theme`: Theme for styling
    - `size`: Avatar size (default: medium)
    - `backgroundColor`: Override background color
-/
def avatarWithLabel (initials : String) (label : String) (theme : Theme)
    (size : AvatarSize := .medium)
    (backgroundColor : Option Color := none) : WidgetBuilder := do
  let avatarWidget ← avatar initials theme size backgroundColor
  let labelWidget ← text' label theme.font theme.text .left
  let props : Trellis.FlexContainer := { Trellis.FlexContainer.row 8 with alignItems := .center }
  flexRow props (style := {}) #[
    pure avatarWidget,
    pure labelWidget
  ]

/-- WidgetM wrapper for avatar. -/
def avatar' (initials : String)
    (size : AvatarSize := .medium)
    (backgroundColor : Option Color := none) : WidgetM Unit := do
  let theme ← getThemeW
  emit (pure (avatar initials theme size backgroundColor))

/-- WidgetM wrapper for avatarWithLabel. -/
def avatarWithLabel' (initials : String) (label : String)
    (size : AvatarSize := .medium)
    (backgroundColor : Option Color := none) : WidgetM Unit := do
  let theme ← getThemeW
  emit (pure (avatarWithLabel initials label theme size backgroundColor))

end Afferent.Canopy

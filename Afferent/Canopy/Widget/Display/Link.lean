/-
  Canopy Link Widget
  Clickable text for navigation with hover effects.
-/
import Reactive
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Reactive.Component

namespace Afferent.Canopy

open Afferent.Arbor hiding Event
open Reactive Reactive.Host
open Afferent.Canopy.Reactive

/-- Build the visual for a link given its state.
    Uses text with a colored underline box below. -/
def linkVisual (name : String) (linkText : String) (theme : Theme)
    (color : Color) (hovered : Bool) : WidgetBuilder := do
  let displayColor := if hovered then color.lighten 0.2 else color

  -- Create a column with text on top and a thin line below
  namedColumn name (gap := 0) (style := {}) #[
    text' linkText theme.font displayColor .left,
    coloredBox displayColor (linkText.length.toFloat * 8.0) 1.0
  ]

/-- Create a clickable link.
    - `linkText`: Link text to display
    - `color`: Override link color (uses primary color by default)
    Returns the click event for handling navigation.
-/
def link (linkText : String) (color : Option Color := none)
    : WidgetM (Reactive.Event Spider Unit) := do
  let theme ← getThemeW
  let name ← registerComponentW "link"
  let isHovered ← useHover name
  let onClick ← useClick name
  let linkColor := color.getD theme.primary.background

  let _ ← dynWidget isHovered fun hovered => do
    emit (pure (linkVisual name linkText theme linkColor hovered))

  pure onClick

/-- Create a link with an icon prefix.
    - `linkText`: Link text to display
    - `icon`: Icon character/emoji to show before text
    - `color`: Override link color (uses primary color by default)
    Returns the click event for handling navigation.
-/
def linkWithIcon (linkText : String) (icon : String)
    (color : Option Color := none) : WidgetM (Reactive.Event Spider Unit) := do
  let theme ← getThemeW
  let name ← registerComponentW "link-with-icon"
  let isHovered ← useHover name
  let onClick ← useClick name
  let linkColor := color.getD theme.primary.background

  let _ ← dynWidget isHovered fun hovered => do
    let displayColor := if hovered then linkColor.lighten 0.2 else linkColor
    emit (pure (
      namedRow name (gap := 4) (style := {}) #[
        text' icon theme.font displayColor .left,
        text' linkText theme.font displayColor .left
      ]))

  pure onClick

end Afferent.Canopy

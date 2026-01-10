/-
  Canopy Label Widget
  Styled text with semantic variants.
-/
import Afferent.Canopy.Theme

namespace Afferent.Canopy

open Afferent.Arbor hiding Event

/-- Label text variants for semantic styling. -/
inductive LabelVariant where
  | body      -- Normal body text
  | heading1  -- Large heading (h1)
  | heading2  -- Medium heading (h2)
  | heading3  -- Small heading (h3)
  | caption   -- Small muted text
deriving Repr, BEq, Inhabited

namespace Label

/-- Get font size multiplier for a variant. -/
def variantSizeMultiplier : LabelVariant → Float
  | .body     => 1.0
  | .heading1 => 1.75
  | .heading2 => 1.5
  | .heading3 => 1.25
  | .caption  => 0.85

/-- Check if variant should use muted color by default. -/
def variantIsMuted : LabelVariant → Bool
  | .caption => true
  | _        => false

end Label

/-- Create a styled label widget.
    - `content`: The text to display
    - `theme`: Theme for colors and fonts
    - `variant`: Size/style variant (default: body)
    - `color`: Override color (uses theme text color by default)
    - `align`: Text alignment (default: left)
    - `maxWidth`: Max width for text wrapping (none = no wrap)
-/
def label (content : String) (theme : Theme)
    (variant : LabelVariant := .body)
    (color : Option Color := none)
    (align : TextAlign := .left)
    (maxWidth : Option Float := none) : WidgetBuilder := do
  let fontSize := theme.font.size * Label.variantSizeMultiplier variant
  let font := theme.font.withSize fontSize
  let textColor := color.getD (if Label.variantIsMuted variant then theme.textMuted else theme.text)
  text' content font textColor align maxWidth

/-- Create a heading 1 label. -/
def heading1 (content : String) (theme : Theme)
    (color : Option Color := none)
    (align : TextAlign := .left) : WidgetBuilder :=
  label content theme .heading1 color align

/-- Create a heading 2 label. -/
def heading2 (content : String) (theme : Theme)
    (color : Option Color := none)
    (align : TextAlign := .left) : WidgetBuilder :=
  label content theme .heading2 color align

/-- Create a heading 3 label. -/
def heading3 (content : String) (theme : Theme)
    (color : Option Color := none)
    (align : TextAlign := .left) : WidgetBuilder :=
  label content theme .heading3 color align

/-- Create a caption label (small, muted text). -/
def caption (content : String) (theme : Theme)
    (color : Option Color := none)
    (align : TextAlign := .left) : WidgetBuilder :=
  label content theme .caption color align

/-- Create a body text label. -/
def bodyText (content : String) (theme : Theme)
    (color : Option Color := none)
    (align : TextAlign := .left)
    (maxWidth : Option Float := none) : WidgetBuilder :=
  label content theme .body color align maxWidth

end Afferent.Canopy

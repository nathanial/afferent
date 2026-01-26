/-
  Canopy Chip Widget
  Tag/label components with optional removal.
-/
import Reactive
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Reactive.Component

namespace Afferent.Canopy

open Afferent.Arbor hiding Event
open Reactive Reactive.Host
open Afferent.Canopy.Reactive

/-- Chip visual variants. -/
inductive ChipVariant where
  | filled    -- Solid background
  | outlined  -- Border only
deriving Repr, BEq, Inhabited

/-- Result from chip widget with optional removal event. -/
structure ChipResult where
  /-- Event that fires when the remove button is clicked (if removable). -/
  onRemove : Option (Reactive.Event Spider Unit)

namespace Chip

/-- Default chip dimensions. -/
structure Dimensions where
  paddingH : Float := 12.0
  paddingV : Float := 6.0
  cornerRadius : Float := 16.0  -- Pill shape
  fontSize : Float := 13.0
  removeButtonSize : Float := 16.0
  gap : Float := 4.0
deriving Repr, Inhabited

def defaultDimensions : Dimensions := {}

/-- Get colors for a chip variant. -/
def variantColors (theme : Theme) : ChipVariant → (Color × Color × Color)
  -- (background, text, border)
  | .filled   => (theme.secondary.background, theme.secondary.foreground, Color.transparent)
  | .outlined => (Color.transparent, theme.text, theme.secondary.border)

end Chip

/-- Build the visual for a chip. -/
def chipVisual (name : String) (label : String) (theme : Theme)
    (variant : ChipVariant) (removable : Bool)
    (removeHovered : Bool := false) : WidgetBuilder := do
  let dims := Chip.defaultDimensions
  let (bgColor, textColor, borderColor) := Chip.variantColors theme variant
  let font := theme.font.withSize dims.fontSize

  let style : BoxStyle := {
    backgroundColor := some bgColor
    borderColor := if borderColor.a > 0 then some borderColor else none
    borderWidth := if borderColor.a > 0 then 1.0 else 0
    cornerRadius := dims.cornerRadius
    padding := Trellis.EdgeInsets.symmetric dims.paddingH dims.paddingV
  }

  if removable then
    let removeStyle : BoxStyle := {
      backgroundColor := some (if removeHovered then theme.secondary.backgroundHover else Color.transparent)
      cornerRadius := dims.removeButtonSize / 2
      minWidth := some dims.removeButtonSize
      minHeight := some dims.removeButtonSize
      maxWidth := some dims.removeButtonSize
      maxHeight := some dims.removeButtonSize
    }
    namedRow name (gap := dims.gap) (style := style) #[
      text' label font textColor .left,
      namedCenter (name ++ "-remove") (style := removeStyle) do
        text' "×" font textColor .center
    ]
  else
    namedCenter name (style := style) do
      text' label font textColor .left

/-- Create a chip with optional removal.
    - `label`: Text to display in the chip
    - `variant`: Visual variant (filled or outlined)
    - `removable`: Whether to show a remove button
    Returns ChipResult with optional onRemove event.
-/
def chip (label : String)
    (variant : ChipVariant := .filled)
    (removable : Bool := false) : WidgetM ChipResult := do
  let theme ← getThemeW
  let name ← registerComponentW "chip"

  if removable then
    let removeName := name ++ "-remove"
    let isRemoveHovered ← useHover removeName
    let onRemove ← useClick removeName

    let _ ← dynWidget isRemoveHovered fun hovered => do
      emit do pure (chipVisual name label theme variant true hovered)

    pure { onRemove := some onRemove }
  else
    emit do pure (chipVisual name label theme variant false false)
    pure { onRemove := none }

/-- Create a simple non-removable chip. -/
def simpleChip (label : String)
    (variant : ChipVariant := .filled) : WidgetM Unit := do
  let _ ← chip label variant false
  pure ()

/-- Create a row of chips from labels. -/
def chipGroup (labels : Array String)
    (variant : ChipVariant := .filled)
    (removable : Bool := false) : WidgetM (Array ChipResult) := do
  row' (gap := 8) (style := {}) do
    let mut results := #[]
    for label in labels do
      let result ← chip label variant removable
      results := results.push result
    pure results

end Afferent.Canopy

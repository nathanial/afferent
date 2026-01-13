/-
  Canopy Panel Widget
  Container with styled background and border.
-/
import Afferent.Canopy.Theme
import Afferent.Canopy.Widget.Display.Label

namespace Afferent.Canopy

open Afferent.Arbor hiding Event

/-- Panel visual variants. -/
inductive PanelVariant where
  | elevated  -- Background with subtle border (card-like)
  | outlined  -- Border only, transparent background
  | filled    -- Solid background, no border
deriving Repr, BEq, Inhabited

/-- Create a panel container.
    - `variant`: Visual style (elevated, outlined, or filled)
    - `theme`: Theme for colors
    - `padding`: Inner padding (defaults to theme padding)
    - `children`: Child widgets to render inside
-/
def panel (variant : PanelVariant := .elevated)
    (theme : Theme) (padding : Float := theme.padding)
    (children : ChildBuilder Unit) : WidgetBuilder := do
  let colors := theme.panel

  let style : BoxStyle := match variant with
    | .elevated => {
        backgroundColor := some colors.background
        borderColor := some (colors.border.withAlpha 0.5)
        borderWidth := 1
        cornerRadius := theme.cornerRadius
        padding := Trellis.EdgeInsets.uniform padding
      }
    | .outlined => {
        backgroundColor := none
        borderColor := some colors.border
        borderWidth := 1
        cornerRadius := theme.cornerRadius
        padding := Trellis.EdgeInsets.uniform padding
      }
    | .filled => {
        backgroundColor := some colors.background
        borderColor := none
        borderWidth := 0
        cornerRadius := theme.cornerRadius
        padding := Trellis.EdgeInsets.uniform padding
      }

  vbox (gap := 0) (style := style) children

/-- Create an elevated panel (background + subtle border). -/
def elevatedPanel (theme : Theme) (padding : Float := theme.padding)
    (children : ChildBuilder Unit) : WidgetBuilder :=
  panel .elevated theme padding children

/-- Create an outlined panel (border only). -/
def outlinedPanel (theme : Theme) (padding : Float := theme.padding)
    (children : ChildBuilder Unit) : WidgetBuilder :=
  panel .outlined theme padding children

/-- Create a filled panel (solid background). -/
def filledPanel (theme : Theme) (padding : Float := theme.padding)
    (children : ChildBuilder Unit) : WidgetBuilder :=
  panel .filled theme padding children

/-- Create a panel with a title header.
    - `title`: Header text
    - `variant`: Visual style
    - `theme`: Theme for styling
    - `children`: Content below the title
-/
def titledPanel (title : String)
    (variant : PanelVariant := .elevated)
    (theme : Theme)
    (children : ChildBuilder Unit) : WidgetBuilder := do
  panel variant theme theme.padding do
    vbox (gap := 8) (style := {}) do
      heading3 title theme
      vbox (gap := 0) (style := {}) children

/-- Create a simple card (elevated panel with default styling). -/
def simpleCard (theme : Theme) (children : ChildBuilder Unit) : WidgetBuilder :=
  elevatedPanel theme theme.padding children

end Afferent.Canopy

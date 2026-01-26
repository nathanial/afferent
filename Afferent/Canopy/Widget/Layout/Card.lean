/-
  Canopy Card Widget
  Container with optional header and footer sections.
-/
import Afferent.Canopy.Theme
import Afferent.Canopy.Widget.Display.Label
import Afferent.Canopy.Widget.Display.Separator
import Afferent.Canopy.Reactive.Component

namespace Afferent.Canopy

open Afferent.Arbor hiding Event

/-- Card visual variants. -/
inductive CardVariant where
  | elevated  -- Background with shadow-like border
  | outlined  -- Border only
  | filled    -- Solid background
deriving Repr, BEq, Inhabited

/-- Create a card container.
    - `variant`: Visual style (elevated, outlined, filled)
    - `theme`: Theme for colors
    - `padding`: Inner padding
    - `children`: Child widgets to render inside
-/
def card (variant : CardVariant := .elevated)
    (theme : Theme) (padding : Float := 16.0)
    (children : ChildBuilder Unit) : WidgetBuilder := do
  let colors := theme.panel

  let style : BoxStyle := match variant with
    | .elevated => {
        backgroundColor := some colors.background
        borderColor := some (colors.border.withAlpha 0.3)
        borderWidth := 1
        cornerRadius := theme.cornerRadius + 2
        padding := Trellis.EdgeInsets.uniform padding
      }
    | .outlined => {
        backgroundColor := none
        borderColor := some colors.border
        borderWidth := 1
        cornerRadius := theme.cornerRadius + 2
        padding := Trellis.EdgeInsets.uniform padding
      }
    | .filled => {
        backgroundColor := some (colors.background.withAlpha 0.9)
        borderColor := none
        borderWidth := 0
        cornerRadius := theme.cornerRadius + 2
        padding := Trellis.EdgeInsets.uniform padding
      }

  vbox (gap := 0) (style := style) children

/-- Create an elevated card. -/
def elevatedCard (theme : Theme) (padding : Float := 16.0)
    (children : ChildBuilder Unit) : WidgetBuilder :=
  card .elevated theme padding children

/-- Create an outlined card. -/
def outlinedCard (theme : Theme) (padding : Float := 16.0)
    (children : ChildBuilder Unit) : WidgetBuilder :=
  card .outlined theme padding children

/-- Create a filled card. -/
def filledCard (theme : Theme) (padding : Float := 16.0)
    (children : ChildBuilder Unit) : WidgetBuilder :=
  card .filled theme padding children

/-- Create a card with a header title.
    - `title`: Header text
    - `variant`: Visual style
    - `theme`: Theme for styling
    - `children`: Content below the header
-/
def cardWithHeader (title : String)
    (variant : CardVariant := .elevated)
    (theme : Theme)
    (children : ChildBuilder Unit) : WidgetBuilder := do
  let colors := theme.panel

  let outerStyle : BoxStyle := match variant with
    | .elevated => {
        backgroundColor := some colors.background
        borderColor := some (colors.border.withAlpha 0.3)
        borderWidth := 1
        cornerRadius := theme.cornerRadius + 2
      }
    | .outlined => {
        backgroundColor := none
        borderColor := some colors.border
        borderWidth := 1
        cornerRadius := theme.cornerRadius + 2
      }
    | .filled => {
        backgroundColor := some (colors.background.withAlpha 0.9)
        borderColor := none
        borderWidth := 0
        cornerRadius := theme.cornerRadius + 2
      }

  vbox (gap := 0) (style := outerStyle) do
    -- Header section
    let headerStyle : BoxStyle := {
      padding := Trellis.EdgeInsets.symmetric 16 12
    }
    vbox (gap := 0) (style := headerStyle) do
      heading3 title theme

    -- Separator
    hseparator theme 1 0

    -- Content section
    let contentStyle : BoxStyle := {
      padding := Trellis.EdgeInsets.uniform 16
    }
    vbox (gap := 8) (style := contentStyle) children

/-- Create a card with header and footer sections.
    - `title`: Header text
    - `variant`: Visual style
    - `theme`: Theme for styling
    - `content`: Main content
    - `footer`: Footer content
-/
def cardWithHeaderFooter (title : String)
    (variant : CardVariant := .elevated)
    (theme : Theme)
    (content : ChildBuilder Unit)
    (footer : ChildBuilder Unit) : WidgetBuilder := do
  let colors := theme.panel

  let outerStyle : BoxStyle := match variant with
    | .elevated => {
        backgroundColor := some colors.background
        borderColor := some (colors.border.withAlpha 0.3)
        borderWidth := 1
        cornerRadius := theme.cornerRadius + 2
      }
    | .outlined => {
        backgroundColor := none
        borderColor := some colors.border
        borderWidth := 1
        cornerRadius := theme.cornerRadius + 2
      }
    | .filled => {
        backgroundColor := some (colors.background.withAlpha 0.9)
        borderColor := none
        borderWidth := 0
        cornerRadius := theme.cornerRadius + 2
      }

  vbox (gap := 0) (style := outerStyle) do
    -- Header section
    let headerStyle : BoxStyle := {
      padding := Trellis.EdgeInsets.symmetric 16 12
    }
    vbox (gap := 0) (style := headerStyle) do
      heading3 title theme

    -- Top separator
    hseparator theme 1 0

    -- Content section
    let contentStyle : BoxStyle := {
      padding := Trellis.EdgeInsets.uniform 16
      flexItem := some (Trellis.FlexItem.growing 1)
    }
    vbox (gap := 8) (style := contentStyle) content

    -- Bottom separator
    hseparator theme 1 0

    -- Footer section
    let footerStyle : BoxStyle := {
      padding := Trellis.EdgeInsets.symmetric 16 12
    }
    hbox (gap := 8) (style := footerStyle) footer

/-! ## WidgetM Wrappers -/

open Reactive Reactive.Host
open Afferent.Canopy.Reactive

/-- Create an elevated card container. -/
def elevatedCard' (padding : Float := 16.0) (children : WidgetM α) : WidgetM α := do
  let theme ← getThemeW
  let (result, childRenders) ← runWidgetChildren children
  emit do
    let widgets ← childRenders.mapM id
    let content := column (gap := 0) (style := {}) widgets
    pure (elevatedCard theme padding content)
  pure result

/-- Create an outlined card container. -/
def outlinedCard' (padding : Float := 16.0) (children : WidgetM α) : WidgetM α := do
  let theme ← getThemeW
  let (result, childRenders) ← runWidgetChildren children
  emit do
    let widgets ← childRenders.mapM id
    let content := column (gap := 0) (style := {}) widgets
    pure (outlinedCard theme padding content)
  pure result

/-- Create a filled card container. -/
def filledCard' (padding : Float := 16.0) (children : WidgetM α) : WidgetM α := do
  let theme ← getThemeW
  let (result, childRenders) ← runWidgetChildren children
  emit do
    let widgets ← childRenders.mapM id
    let content := column (gap := 0) (style := {}) widgets
    pure (filledCard theme padding content)
  pure result

/-- Create a card with header. -/
def cardWithHeader' (title : String) (variant : CardVariant := .elevated)
    (children : WidgetM α) : WidgetM α := do
  let theme ← getThemeW
  let (result, childRenders) ← runWidgetChildren children
  emit do
    let widgets ← childRenders.mapM id
    let content := column (gap := 8) (style := {}) widgets
    pure (cardWithHeader title variant theme content)
  pure result

end Afferent.Canopy

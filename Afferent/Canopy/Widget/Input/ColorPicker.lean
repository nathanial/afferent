/-
  Canopy ColorPicker Widget
  HSV-based color picker with saturation/value square, hue bar, and optional alpha bar.
-/
import Reactive
import Tincture
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Reactive.Component

namespace Afferent.Canopy

open Afferent.Arbor hiding Event
open Reactive Reactive.Host
open Afferent.Canopy.Reactive
open Tincture (HSV Color)

/-- Configuration for the color picker widget. -/
structure ColorPickerConfig where
  /-- Width and height of the saturation/value square. -/
  squareSize : Float := 180.0
  /-- Width of the hue bar. -/
  hueBarWidth : Float := 24.0
  /-- Width of the alpha bar (0 to hide). -/
  alphaBarWidth : Float := 24.0
  /-- Gap between components. -/
  gap : Float := 8.0
  /-- Height of the color preview. -/
  previewHeight : Float := 30.0
  /-- Radius of the SV indicator circle. -/
  svIndicatorRadius : Float := 6.0
  /-- Height of bar indicators. -/
  barIndicatorHeight : Float := 4.0
  /-- Corner radius for bars and preview. -/
  cornerRadius : Float := 4.0
deriving Repr, Inhabited

namespace ColorPickerConfig

def default : ColorPickerConfig := {}

/-- Config without alpha bar. -/
def noAlpha : ColorPickerConfig := { alphaBarWidth := 0 }

end ColorPickerConfig

/-- Which component is being dragged. -/
inductive ColorPickerDragTarget where
  | none
  | svSquare
  | hueBar
  | alphaBar
deriving Repr, BEq, Inhabited

/-- Internal state for color picker. -/
structure ColorPickerState where
  /-- Current HSV values. -/
  hsv : HSV := { h := 0.0, s := 1.0, v := 1.0 }
  /-- Current alpha value. -/
  alpha : Float := 1.0
  /-- Which component is being dragged. -/
  dragTarget : ColorPickerDragTarget := .none
deriving Repr, BEq, Inhabited

/-- Result from colorPicker widget. -/
structure ColorPickerResult where
  /-- Event that fires when color changes. -/
  onChange : Reactive.Event Spider Color
  /-- Current color as a Dynamic. -/
  color : Reactive.Dynamic Spider Color
  /-- Current HSV values as a Dynamic. -/
  hsv : Reactive.Dynamic Spider HSV
  /-- Current alpha as a Dynamic. -/
  alpha : Reactive.Dynamic Spider Float

/-- Event type for color picker inputs. -/
inductive ColorPickerInputEvent where
  | click (data : ClickData)
  | hover (data : HoverData)
  | mouseUp

namespace ColorPicker

/-- Clamp a Float to [0, 1]. -/
def clamp01 (x : Float) : Float :=
  if x < 0.0 then 0.0 else if x > 1.0 then 1.0 else x

/-- Min of two Floats. -/
def minFloat (a b : Float) : Float := if a < b then a else b

/-- Convert position in SV square to (saturation, value). -/
def svFromPosition (rect : Trellis.LayoutRect) (x y : Float) : Float × Float :=
  let s := clamp01 ((x - rect.x) / rect.width)
  let v := clamp01 (1.0 - (y - rect.y) / rect.height)
  (s, v)

/-- Convert position in hue bar to hue value. -/
def hueFromPosition (rect : Trellis.LayoutRect) (y : Float) : Float :=
  clamp01 ((y - rect.y) / rect.height)

/-- Convert position in alpha bar to alpha value. -/
def alphaFromPosition (rect : Trellis.LayoutRect) (y : Float) : Float :=
  clamp01 (1.0 - (y - rect.y) / rect.height)

/-- Check if a point is within a widget's bounds. -/
def isInWidget (widget : Widget) (layouts : Trellis.LayoutResult)
    (widgetName : String) (x y : Float) : Bool :=
  match findWidgetIdByName widget widgetName with
  | some wid =>
    match layouts.get wid with
    | some layout =>
      let rect := layout.contentRect
      x >= rect.x && x <= rect.x + rect.width &&
      y >= rect.y && y <= rect.y + rect.height
    | none => false
  | none => false

/-- Get rect for a named widget. -/
def getWidgetRect (widget : Widget) (layouts : Trellis.LayoutResult)
    (widgetName : String) : Option Trellis.LayoutRect :=
  match findWidgetIdByName widget widgetName with
  | some wid =>
    match layouts.get wid with
    | some layout => some layout.contentRect
    | none => none
  | none => none

/-- CustomSpec for saturation/value square.
    Uses two overlapping gradients: horizontal (white→hue) and vertical (transparent→black). -/
def svSquareSpec (hue saturation value : Float) (size : Float)
    (indicatorRadius : Float) : CustomSpec := {
  measure := fun _ _ => (size, size)
  collect := fun layout =>
    let rect := layout.contentRect

    -- Layer 1: Horizontal gradient (white → pure hue color) for saturation
    let pureHueColor := HSV.toColor { h := hue, s := 1.0, v := 1.0 } 1.0
    let leftPt := Afferent.Point.mk' rect.x (rect.y + rect.height / 2)
    let rightPt := Afferent.Point.mk' (rect.x + rect.width) (rect.y + rect.height / 2)
    let satStops : Array Afferent.GradientStop := #[
      { position := 0.0, color := Color.white },
      { position := 1.0, color := pureHueColor }
    ]
    let satStyle := Afferent.FillStyle.linearGradient leftPt rightPt satStops
    let svRect := Arbor.Rect.mk' rect.x rect.y rect.width rect.height

    -- Layer 2: Vertical gradient (transparent → black) for value
    let topPt := Afferent.Point.mk' (rect.x + rect.width / 2) rect.y
    let bottomPt := Afferent.Point.mk' (rect.x + rect.width / 2) (rect.y + rect.height)
    let valStops : Array Afferent.GradientStop := #[
      { position := 0.0, color := { Color.black with a := 0.0 } },
      { position := 1.0, color := Color.black }
    ]
    let valStyle := Afferent.FillStyle.linearGradient topPt bottomPt valStops

    RenderM.build do
      -- Draw both gradient layers
      RenderM.fillRectStyle svRect satStyle 0
      RenderM.fillRectStyle svRect valStyle 0

      -- Draw selection indicator (white circle with black outline)
      let indicatorX := rect.x + saturation * rect.width
      let indicatorY := rect.y + (1.0 - value) * rect.height
      let indicatorRect := Arbor.Rect.mk'
        (indicatorX - indicatorRadius) (indicatorY - indicatorRadius)
        (indicatorRadius * 2) (indicatorRadius * 2)
      RenderM.strokeRect indicatorRect Color.black 2.0 indicatorRadius
      RenderM.strokeRect indicatorRect Color.white 1.0 indicatorRadius
}

/-- CustomSpec for vertical hue bar.
    Uses a 7-stop linear gradient for smooth HSV spectrum. -/
def hueBarSpec (selectedHue : Float) (width height : Float)
    (indicatorHeight cornerRadius : Float) : CustomSpec := {
  measure := fun _ _ => (width, height)
  collect := fun layout =>
    let rect := layout.contentRect

    -- 7-stop vertical gradient for HSV hue spectrum (red → yellow → green → cyan → blue → magenta → red)
    let topPt := Afferent.Point.mk' (rect.x + rect.width / 2) rect.y
    let bottomPt := Afferent.Point.mk' (rect.x + rect.width / 2) (rect.y + rect.height)
    -- Compute hue colors explicitly
    let red     := HSV.toColor (HSV.mk 0.0   1.0 1.0) 1.0
    let yellow  := HSV.toColor (HSV.mk 0.167 1.0 1.0) 1.0
    let green   := HSV.toColor (HSV.mk 0.333 1.0 1.0) 1.0
    let cyan    := HSV.toColor (HSV.mk 0.5   1.0 1.0) 1.0
    let blue    := HSV.toColor (HSV.mk 0.667 1.0 1.0) 1.0
    let magenta := HSV.toColor (HSV.mk 0.833 1.0 1.0) 1.0
    let hueStops : Array Afferent.GradientStop := #[
      { position := 0.0,   color := red },
      { position := 0.167, color := yellow },
      { position := 0.333, color := green },
      { position := 0.5,   color := cyan },
      { position := 0.667, color := blue },
      { position := 0.833, color := magenta },
      { position := 1.0,   color := red }
    ]
    let hueStyle := Afferent.FillStyle.linearGradient topPt bottomPt hueStops
    let hueRect := Arbor.Rect.mk' rect.x rect.y rect.width rect.height

    RenderM.build do
      RenderM.fillRectStyle hueRect hueStyle cornerRadius

      -- Draw hue indicator
      let indicatorY := rect.y + selectedHue * rect.height - indicatorHeight / 2
      let indicatorRect := Arbor.Rect.mk' rect.x indicatorY rect.width indicatorHeight
      RenderM.fillRect indicatorRect Color.white cornerRadius
      RenderM.strokeRect indicatorRect (Color.gray 0.3) 1.0 cornerRadius
}

/-- CustomSpec for vertical alpha bar with checkerboard.
    Uses a 2-stop linear gradient from opaque to transparent. -/
def alphaBarSpec (selectedAlpha : Float) (currentHSV : HSV)
    (width height : Float) (indicatorHeight cornerRadius : Float) : CustomSpec := {
  measure := fun _ _ => (width, height)
  collect := fun layout =>
    let rect := layout.contentRect

    -- Draw checkerboard background for transparency visualization
    let checkSize : Float := 6.0
    let rows := (rect.height / checkSize).ceil.toUInt32.toNat
    let cols := (rect.width / checkSize).ceil.toUInt32.toNat

    RenderM.build do
      for row in [:rows] do
        for col in [:cols] do
          let isLight := (row + col) % 2 == 0
          let color := if isLight then Color.gray 0.8 else Color.gray 0.5
          let checkRect := Arbor.Rect.mk'
            (rect.x + col.toFloat * checkSize)
            (rect.y + row.toFloat * checkSize)
            (minFloat checkSize (rect.width - col.toFloat * checkSize))
            (minFloat checkSize (rect.height - row.toFloat * checkSize))
          RenderM.fillRect checkRect color 0

      -- Vertical gradient from opaque (top) to transparent (bottom)
      let baseColor := HSV.toColor currentHSV 1.0
      let topPt := Afferent.Point.mk' (rect.x + rect.width / 2) rect.y
      let bottomPt := Afferent.Point.mk' (rect.x + rect.width / 2) (rect.y + rect.height)
      let alphaStops : Array Afferent.GradientStop := #[
        { position := 0.0, color := baseColor },
        { position := 1.0, color := { baseColor with a := 0.0 } }
      ]
      let alphaStyle := Afferent.FillStyle.linearGradient topPt bottomPt alphaStops
      let alphaRect := Arbor.Rect.mk' rect.x rect.y rect.width rect.height
      RenderM.fillRectStyle alphaRect alphaStyle 0

      -- Draw alpha indicator
      let indicatorY := rect.y + (1.0 - selectedAlpha) * rect.height - indicatorHeight / 2
      let indicatorRect := Arbor.Rect.mk' rect.x indicatorY rect.width indicatorHeight
      RenderM.fillRect indicatorRect Color.white cornerRadius
      RenderM.strokeRect indicatorRect (Color.gray 0.3) 1.0 cornerRadius
}

/-- CustomSpec for color preview rectangle. -/
def colorPreviewSpec (color : Color) (width height cornerRadius : Float) : CustomSpec := {
  measure := fun _ _ => (width, height)
  collect := fun layout =>
    let rect := layout.contentRect
    -- Draw checkerboard background for alpha visualization
    let checkSize : Float := 6.0
    let rows := (rect.height / checkSize).ceil.toUInt32.toNat
    let cols := (rect.width / checkSize).ceil.toUInt32.toNat

    RenderM.build do
      for row in [:rows] do
        for col in [:cols] do
          let isLight := (row + col) % 2 == 0
          let checkColor := if isLight then Color.gray 0.8 else Color.gray 0.5
          let checkRect := Arbor.Rect.mk'
            (rect.x + col.toFloat * checkSize)
            (rect.y + row.toFloat * checkSize)
            (minFloat checkSize (rect.width - col.toFloat * checkSize))
            (minFloat checkSize (rect.height - row.toFloat * checkSize))
          RenderM.fillRect checkRect checkColor 0

      -- Draw current color overlay
      let previewRect := Arbor.Rect.mk' rect.x rect.y rect.width rect.height
      RenderM.fillRect previewRect color cornerRadius
      RenderM.strokeRect previewRect (Color.gray 0.3) 1.0 cornerRadius
}

end ColorPicker

/-- Build the visual representation of the color picker. -/
def colorPickerVisual (pickerName svName hueName alphaName : String)
    (config : ColorPickerConfig) (state : ColorPickerState)
    (_theme : Theme) : WidgetBuilder := do
  let hsv := state.hsv
  let alpha := state.alpha
  let currentColor := HSV.toColor hsv alpha

  -- SV Square widget
  let svSquare ← namedCustom svName (ColorPicker.svSquareSpec hsv.h hsv.s hsv.v config.squareSize
                         config.svIndicatorRadius) {
    minWidth := some config.squareSize
    minHeight := some config.squareSize
  }

  -- Hue bar widget
  let hueBar ← namedCustom hueName (ColorPicker.hueBarSpec hsv.h config.hueBarWidth config.squareSize
                       config.barIndicatorHeight config.cornerRadius) {
    minWidth := some config.hueBarWidth
    minHeight := some config.squareSize
  }

  -- Alpha bar widget (optional)
  let showAlpha := config.alphaBarWidth > 0
  let alphaWidgetOpt ← if showAlpha then
    let alphaBar ← namedCustom alphaName (ColorPicker.alphaBarSpec alpha hsv config.alphaBarWidth
                           config.squareSize config.barIndicatorHeight
                           config.cornerRadius) {
      minWidth := some config.alphaBarWidth
      minHeight := some config.squareSize
    }
    pure (some alphaBar)
  else
    pure none

  -- Preview widget
  let previewW := config.hueBarWidth +
                  (if showAlpha then config.gap + config.alphaBarWidth else 0)
  let preview ← custom (ColorPicker.colorPreviewSpec currentColor previewW
                        config.previewHeight config.cornerRadius) {
    minWidth := some previewW
    minHeight := some config.previewHeight
  }

  -- Layout: Row containing [SV square, Column of [hue+alpha row, preview]]
  let bars ← match alphaWidgetOpt with
    | some alphaW => row (gap := config.gap) (style := {}) #[pure hueBar, pure alphaW]
    | none => pure hueBar

  let rightColumn ← column (gap := config.gap) (style := {}) #[pure bars, pure preview]

  namedRow pickerName (gap := config.gap) (style := {}) #[pure svSquare, pure rightColumn]

/-- Create a reactive color picker component.
    - `initialColor`: Initial color value
    - `config`: Optional configuration
-/
def colorPicker (initialColor : Color := Color.red) (config : ColorPickerConfig := {})
    : WidgetM ColorPickerResult := do
  let theme ← getThemeW
  -- Register component names
  let pickerName ← registerComponentW "colorpicker"
  let svName ← registerComponentW "colorpicker-sv"
  let hueName ← registerComponentW "colorpicker-hue"
  let alphaName ← registerComponentW "colorpicker-alpha"

  -- Get event streams
  let allClicks ← useAllClicks
  let allHovers ← useAllHovers
  let allMouseUp ← useAllMouseUp

  -- Convert initial color to HSV
  let initialHSV := HSV.fromColor initialColor
  let initialState : ColorPickerState := {
    hsv := initialHSV
    alpha := initialColor.a
    dragTarget := .none
  }

  -- Convert events to unified stream
  let liftSpider {α : Type} : SpiderM α → WidgetM α := fun m => StateT.lift (liftM m)
  let clickEvents ← liftSpider (Event.mapM ColorPickerInputEvent.click allClicks)
  let hoverEvents ← liftSpider (Event.mapM ColorPickerInputEvent.hover allHovers)
  let mouseUpEvents ← liftSpider (Event.mapM (fun _ => ColorPickerInputEvent.mouseUp) allMouseUp)
  let allInputEvents ← liftSpider (Event.leftmostM [clickEvents, hoverEvents, mouseUpEvents])

  -- Fold events into state
  let combinedState ← Reactive.foldDynM
    (fun (event : ColorPickerInputEvent) state => do
      match event with
      | .click clickData =>
        let x := clickData.click.x
        let y := clickData.click.y
        let widget := clickData.widget
        let layouts := clickData.layouts

        -- Check which component was clicked
        if ColorPicker.isInWidget widget layouts svName x y then
          match ColorPicker.getWidgetRect widget layouts svName with
          | some rect =>
            let (s, v) := ColorPicker.svFromPosition rect x y
            let newHSV := { state.hsv with s, v }
            pure { state with hsv := newHSV, dragTarget := .svSquare }
          | none => pure state
        else if ColorPicker.isInWidget widget layouts hueName x y then
          match ColorPicker.getWidgetRect widget layouts hueName with
          | some rect =>
            let h := ColorPicker.hueFromPosition rect y
            let newHSV := { state.hsv with h }
            pure { state with hsv := newHSV, dragTarget := .hueBar }
          | none => pure state
        else if config.alphaBarWidth > 0 && ColorPicker.isInWidget widget layouts alphaName x y then
          match ColorPicker.getWidgetRect widget layouts alphaName with
          | some rect =>
            let a := ColorPicker.alphaFromPosition rect y
            pure { state with alpha := a, dragTarget := .alphaBar }
          | none => pure state
        else
          pure { state with dragTarget := .none }

      | .hover hoverData =>
        if state.dragTarget == .none then
          pure state
        else
          let x := hoverData.x
          let y := hoverData.y
          let widget := hoverData.widget
          let layouts := hoverData.layouts

          match state.dragTarget with
          | .svSquare =>
            match ColorPicker.getWidgetRect widget layouts svName with
            | some rect =>
              let (s, v) := ColorPicker.svFromPosition rect x y
              let newHSV := { state.hsv with s, v }
              pure { state with hsv := newHSV }
            | none => pure state
          | .hueBar =>
            match ColorPicker.getWidgetRect widget layouts hueName with
            | some rect =>
              let h := ColorPicker.hueFromPosition rect y
              let newHSV := { state.hsv with h }
              pure { state with hsv := newHSV }
            | none => pure state
          | .alphaBar =>
            match ColorPicker.getWidgetRect widget layouts alphaName with
            | some rect =>
              let a := ColorPicker.alphaFromPosition rect y
              pure { state with alpha := a }
            | none => pure state
          | .none => pure state

      | .mouseUp =>
        pure { state with dragTarget := .none }
    )
    initialState
    allInputEvents

  -- Derive output dynamics
  let colorDyn ← Dynamic.mapM (fun s => HSV.toColor s.hsv s.alpha) combinedState
  let hsvDyn ← Dynamic.mapM (fun s => s.hsv) combinedState
  let alphaDyn ← Dynamic.mapM (fun s => s.alpha) combinedState

  -- Create change event
  let onChange ← Event.mapM (fun s => HSV.toColor s.hsv s.alpha) combinedState.updated

  -- Use dynWidget for efficient change-driven rebuilds
  let _ ← dynWidget combinedState fun s => do
    emit do pure (colorPickerVisual pickerName svName hueName alphaName config s theme)

  pure { onChange, color := colorDyn, hsv := hsvDyn, alpha := alphaDyn }

end Afferent.Canopy

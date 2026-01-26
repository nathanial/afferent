/-
  Canopy CandlestickChart Widget
  Financial OHLC (Open, High, Low, Close) candlestick chart for stock/market data.
-/
import Reactive
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Reactive.Component
import Afferent.Canopy.Widget.Charts.Core

namespace Afferent.Canopy

open Afferent.Arbor hiding Event

namespace CandlestickChart

/-- Dimensions and styling for candlestick chart rendering. -/
structure Dimensions extends AxisChartDimensions where
  width := 450.0
  height := 300.0
  marginLeft := 60.0
  candleGap : Float := 4.0
  wickWidth : Float := 1.5
  showVolumeBar : Bool := false
  volumeBarHeight : Float := 50.0
deriving Repr, Inhabited

/-- Default candlestick chart dimensions. -/
def defaultDimensions : Dimensions := {}

/-- Colors for bullish and bearish candles. -/
structure CandleColors where
  bullish : Color := Color.rgba 0.15 0.68 0.38 1.0      -- Green
  bearish : Color := Color.rgba 0.90 0.25 0.20 1.0      -- Red
  bullishWick : Color := Color.rgba 0.15 0.68 0.38 1.0  -- Same as body
  bearishWick : Color := Color.rgba 0.90 0.25 0.20 1.0  -- Same as body
deriving Repr, Inhabited

/-- Default candle colors. -/
def defaultColors : CandleColors := {}

/-- A single candlestick data point (OHLC). -/
structure Candle where
  /-- Opening price. -/
  openPrice : Float
  /-- Highest price. -/
  highPrice : Float
  /-- Lowest price. -/
  lowPrice : Float
  /-- Closing price. -/
  closePrice : Float
  /-- Optional volume. -/
  volume : Option Float := none
  /-- Optional label (e.g., date). -/
  label : Option String := none
deriving Repr, Inhabited, BEq

/-- Candlestick chart data. -/
structure Data where
  /-- Array of candles in chronological order. -/
  candles : Array Candle
  /-- Optional fixed price range (auto-computed if none). -/
  minPrice : Option Float := none
  maxPrice : Option Float := none
deriving Repr, Inhabited, BEq

/-- Check if a candle is bullish (close >= open). -/
def Candle.isBullish (c : Candle) : Bool := c.closePrice >= c.openPrice

/-- Format a price value for axis labels. -/
private def formatPrice (v : Float) : String :=
  if v >= 1000 then
    let whole := v.floor.toUInt32
    s!"{whole}"
  else if v >= 100 then
    let whole := v.floor.toUInt32
    s!"{whole}"
  else
    -- Show one decimal place for smaller values
    let whole := v.floor.toInt32
    let frac := ((v - v.floor).abs * 10).floor.toUInt32
    s!"{whole}.{frac}"

/-- Find price range across all candles. -/
private def findPriceRange (data : Data) : (Float × Float) :=
  match (data.minPrice, data.maxPrice) with
  | (some minP, some maxP) => (minP, maxP)
  | _ =>
    let (minP, maxP) := Id.run do
      let mut minV : Float := 0.0
      let mut maxV : Float := 0.0
      let mut initialized := false
      for c in data.candles do
        if !initialized then
          minV := c.lowPrice
          maxV := c.highPrice
          initialized := true
        else
          if c.lowPrice < minV then minV := c.lowPrice
          if c.highPrice > maxV then maxV := c.highPrice
      (minV, maxV)
    -- Add some padding
    let range := maxP - minP
    let padding := range * 0.05
    (minP - padding, maxP + padding)

/-- Custom spec for candlestick chart rendering. -/
def candlestickChartSpec (data : Data) (theme : Theme)
    (colors : CandleColors := defaultColors)
    (dims : Dimensions := defaultDimensions) : CustomSpec := {
  measure := fun _ _ => (dims.marginLeft + dims.marginRight + 50, dims.marginTop + dims.marginBottom + 30)
  collect := fun layout =>
    let rect := layout.contentRect
    let actualWidth := rect.width
    let actualHeight := rect.height

    let numCandles := data.candles.size
    if numCandles == 0 then #[] else

    -- Calculate chart area
    let chartX := rect.x + dims.marginLeft
    let chartY := rect.y + dims.marginTop
    let chartWidth := actualWidth - dims.marginLeft - dims.marginRight
    let chartHeight := actualHeight - dims.marginTop - dims.marginBottom

    -- Find price range
    let (minPrice, maxPrice) := findPriceRange data
    let priceRange := maxPrice - minPrice
    let priceRange := if priceRange <= 0.0 then 1.0 else priceRange

    -- Calculate candle width
    let totalGapWidth := dims.candleGap * (numCandles + 1).toFloat
    let candleWidth := (chartWidth - totalGapWidth) / numCandles.toFloat

    -- Helper to convert price to Y coordinate
    let priceToY := fun (price : Float) =>
      chartY + chartHeight - ((price - minPrice) / priceRange) * chartHeight

    -- Label indices for X-axis
    let labelIndices := if numCandles <= 5 then
      Array.range numCandles
    else if numCandles <= 10 then
      #[0, numCandles / 2, numCandles - 1]
    else
      #[0, numCandles / 4, numCandles / 2, 3 * numCandles / 4, numCandles - 1]

    RenderM.build do
      -- Draw background
      let bgRect := Arbor.Rect.mk' rect.x rect.y actualWidth actualHeight
      RenderM.fillRect bgRect (theme.panel.background.withAlpha 0.3) 6.0

      -- Draw grid lines
      if dims.showGridLines && dims.gridLineCount > 0 then
        for i in [0:dims.gridLineCount + 1] do
          let ratio := i.toFloat / dims.gridLineCount.toFloat
          let lineY := chartY + chartHeight - (ratio * chartHeight)
          let lineRect := Arbor.Rect.mk' chartX lineY chartWidth 1.0
          RenderM.fillRect lineRect (Color.gray 0.3) 0.0

      -- Draw candles
      for i in [0:numCandles] do
        let candle := data.candles[i]!
        let isBullish := candle.isBullish

        -- Calculate X position
        let candleX := chartX + dims.candleGap + i.toFloat * (candleWidth + dims.candleGap)
        let candleCenterX := candleX + candleWidth / 2

        -- Calculate Y positions
        let highY := priceToY candle.highPrice
        let lowY := priceToY candle.lowPrice
        let openY := priceToY candle.openPrice
        let closeY := priceToY candle.closePrice

        -- Body top and bottom
        let bodyTop := min openY closeY
        let bodyBottom := max openY closeY
        let bodyHeight := max 1.0 (bodyBottom - bodyTop)

        -- Colors
        let bodyColor := if isBullish then colors.bullish else colors.bearish
        let wickColor := if isBullish then colors.bullishWick else colors.bearishWick

        -- Draw upper wick (from body top to high)
        let upperWickRect := Arbor.Rect.mk'
          (candleCenterX - dims.wickWidth / 2) highY dims.wickWidth (bodyTop - highY)
        RenderM.fillRect upperWickRect wickColor 0.0

        -- Draw lower wick (from body bottom to low)
        let lowerWickRect := Arbor.Rect.mk'
          (candleCenterX - dims.wickWidth / 2) bodyBottom dims.wickWidth (lowY - bodyBottom)
        RenderM.fillRect lowerWickRect wickColor 0.0

        -- Draw body
        let bodyRect := Arbor.Rect.mk' candleX bodyTop candleWidth bodyHeight
        RenderM.fillRect bodyRect bodyColor 1.0

      -- Draw Y-axis labels (prices)
      if dims.gridLineCount > 0 then
        for i in [0:dims.gridLineCount + 1] do
          let ratio := i.toFloat / dims.gridLineCount.toFloat
          let price := minPrice + ratio * priceRange
          let labelY := chartY + chartHeight - (ratio * chartHeight) + 4
          let labelText := formatPrice price
          RenderM.fillText labelText (rect.x + 4) labelY theme.smallFont theme.textMuted

      -- Draw X-axis labels (dates/times)
      for idx in labelIndices do
        if idx < numCandles then
          let candle := data.candles[idx]!
          match candle.label with
          | some label =>
            let candleX := chartX + dims.candleGap + idx.toFloat * (candleWidth + dims.candleGap)
            let labelX := candleX + candleWidth / 2
            let labelY := chartY + chartHeight + 16
            RenderM.fillText label labelX labelY theme.smallFont theme.text
          | none => pure ()

      -- Draw axes
      let axisColor := Color.gray 0.5
      let yAxisRect := Arbor.Rect.mk' chartX chartY 1.0 chartHeight
      RenderM.fillRect yAxisRect axisColor 0.0
      let xAxisRect := Arbor.Rect.mk' chartX (chartY + chartHeight) chartWidth 1.0
      RenderM.fillRect xAxisRect axisColor 0.0

  draw := none
}

end CandlestickChart

/-- Build a candlestick chart visual (WidgetBuilder version).
    - `name`: Widget name for identification
    - `data`: Candlestick chart data with OHLC candles
    - `theme`: Theme for styling
    - `colors`: Candle colors for bullish/bearish
    - `dims`: Chart dimensions
-/
def candlestickChartVisual (name : String) (data : CandlestickChart.Data)
    (theme : Theme) (colors : CandlestickChart.CandleColors := CandlestickChart.defaultColors)
    (dims : CandlestickChart.Dimensions := CandlestickChart.defaultDimensions)
    : WidgetBuilder := do
  let wid ← freshId
  let chart ← custom (CandlestickChart.candlestickChartSpec data theme colors dims) {
    width := .percent 1.0
    height := .percent 1.0
    flexItem := some (Trellis.FlexItem.growing 1)
  }
  let style : BoxStyle := { width := .percent 1.0, height := .percent 1.0, minWidth := some dims.width, minHeight := some dims.height, flexItem := some (Trellis.FlexItem.growing 1) }
  let props : Trellis.FlexContainer := { Trellis.FlexContainer.column 0 with alignItems := .stretch }
  pure (.flex wid (some name) props style #[chart])

/-! ## Reactive CandlestickChart Components (FRP-based)

These use WidgetM for declarative composition.
-/

open Reactive Reactive.Host
open Afferent.Canopy.Reactive

/-- CandlestickChart result - provides access to chart state. -/
structure CandlestickChartResult where
  /-- The data being displayed. -/
  data : Dyn CandlestickChart.Data

/-- Create a candlestick chart component using WidgetM with dynamic data.
    The chart automatically rebuilds when the data Dynamic changes.
    - `data`: Dynamic candlestick chart data with OHLC candles
    - `theme`: Theme for styling
    - `colors`: Candle colors
    - `dims`: Chart dimensions
-/
def candlestickChart (data : Dyn CandlestickChart.Data)
    (colors : CandlestickChart.CandleColors := CandlestickChart.defaultColors)
    (dims : CandlestickChart.Dimensions := CandlestickChart.defaultDimensions)
    : WidgetM CandlestickChartResult := do
  let theme ← getThemeW
  let _ ← dynWidget data fun currentData => do
    let name ← registerComponentW "candlestick-chart" (isInteractive := false)
    emit do pure (candlestickChartVisual name currentData theme colors dims)

  pure { data }

/-- Bundled OHLC price data for candlestick arrays. -/
structure OHLCArrays where
  opens : Array Float
  highs : Array Float
  lows : Array Float
  closes : Array Float
deriving Repr, Inhabited, BEq

/-- Create a candlestick chart from dynamic OHLC arrays.
    - `ohlc`: Dynamic OHLC price arrays
    - `labels`: Optional date/time labels
    - `theme`: Theme for styling
    - `colors`: Candle colors
    - `dims`: Chart dimensions
-/
def candlestickChartFromArrays (ohlc : Dyn OHLCArrays)
    (labels : Array String := #[])
    (colors : CandlestickChart.CandleColors := CandlestickChart.defaultColors)
    (dims : CandlestickChart.Dimensions := CandlestickChart.defaultDimensions)
    : WidgetM CandlestickChartResult := do
  let dataDyn ← Dynamic.mapM (fun currentOhlc => Id.run do
    let numCandles := min currentOhlc.opens.size (min currentOhlc.highs.size (min currentOhlc.lows.size currentOhlc.closes.size))
    let mut result : Array CandlestickChart.Candle := #[]
    for i in [0:numCandles] do
      let label := if i < labels.size then some labels[i]! else none
      result := result.push {
        openPrice := currentOhlc.opens[i]!
        highPrice := currentOhlc.highs[i]!
        lowPrice := currentOhlc.lows[i]!
        closePrice := currentOhlc.closes[i]!
        label
      }
    ({ candles := result } : CandlestickChart.Data)
  ) ohlc
  candlestickChart dataDyn colors dims

end Afferent.Canopy

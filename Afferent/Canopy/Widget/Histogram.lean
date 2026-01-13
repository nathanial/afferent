/-
  Canopy Histogram Widget
  Histogram for showing frequency distribution of numerical data across bins.
-/
import Reactive
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Reactive.Component

namespace Afferent.Canopy

open Afferent.Arbor hiding Event

/-- Histogram color variant. -/
inductive HistogramVariant where
  | primary
  | secondary
  | success
  | warning
  | error
deriving Repr, BEq, Inhabited

namespace Histogram

/-- Dimensions and styling for histogram rendering. -/
structure Dimensions where
  width : Float := 400.0
  height : Float := 250.0
  marginTop : Float := 20.0
  marginBottom : Float := 40.0
  marginLeft : Float := 50.0
  marginRight : Float := 20.0
  barGap : Float := 1.0
  cornerRadius : Float := 0.0
  showGridLines : Bool := true
  gridLineCount : Nat := 5
  showBinLabels : Bool := true
  showFrequencyLabels : Bool := true
deriving Repr, Inhabited

/-- Default histogram dimensions. -/
def defaultDimensions : Dimensions := {}

/-- Configuration for histogram binning. -/
structure BinConfig where
  /-- Number of bins to create. If none, uses Sturges' formula. -/
  binCount : Option Nat := none
  /-- Optional explicit bin edges. Overrides binCount if provided. -/
  binEdges : Option (Array Float) := none
  /-- Whether to normalize to show density instead of counts. -/
  normalize : Bool := false
deriving Repr, Inhabited

/-- Default bin configuration. -/
def defaultBinConfig : BinConfig := {}

/-- A computed histogram bin with its range and count. -/
structure Bin where
  /-- Lower edge of the bin (inclusive). -/
  lower : Float
  /-- Upper edge of the bin (exclusive, except for last bin). -/
  upper : Float
  /-- Count of values in this bin. -/
  count : Nat
  /-- Density (count / total / binWidth) if normalized. -/
  density : Float
deriving Repr, Inhabited

/-- Get the fill color for a variant. -/
def variantColor (variant : HistogramVariant) (theme : Theme) : Color :=
  match variant with
  | .primary => theme.primary.background
  | .secondary => theme.secondary.background
  | .success => Color.rgba 0.2 0.8 0.3 1.0
  | .warning => Color.rgba 1.0 0.7 0.0 1.0
  | .error => Color.rgba 0.9 0.2 0.2 1.0

/-- Format a float value for axis labels. -/
private def formatValue (v : Float) : String :=
  if v >= 1000000 then
    s!"{(v / 1000000).floor.toUInt32}M"
  else if v >= 1000 then
    s!"{(v / 1000).floor.toUInt32}K"
  else if v == v.floor then
    s!"{v.floor.toInt32}"
  else
    let whole := v.floor.toInt32
    let frac := ((v - v.floor).abs * 10).floor.toUInt32
    s!"{whole}.{frac}"

/-- Calculate the optimal number of bins using Sturges' formula. -/
private def sturgesBinCount (n : Nat) : Nat :=
  if n <= 1 then 1
  else
    -- Sturges' formula: k = ceil(log2(n) + 1)
    -- Approximate log2 using successive halving
    let rec log2Approx (x : Nat) (acc : Nat) : Nat :=
      if x <= 1 then acc else log2Approx (x / 2) (acc + 1)
    let k := log2Approx n 0 + 1
    max 1 (min k 50)  -- Cap at 50 bins

/-- Compute histogram bins from raw data. -/
def computeBins (data : Array Float) (config : BinConfig := defaultBinConfig) : Array Bin := Id.run do
  if data.isEmpty then return #[]

  -- Find data range
  let first := data[0]!
  let mut minVal := first
  let mut maxVal := first
  for v in data do
    if v < minVal then minVal := v
    if v > maxVal then maxVal := v

  -- Handle single value case
  if minVal == maxVal then
    return #[{ lower := minVal - 0.5, upper := maxVal + 0.5, count := data.size, density := 1.0 }]

  -- Determine bin edges
  let binEdges : Array Float := match config.binEdges with
    | some edges => edges
    | none =>
      let numBins := config.binCount.getD (sturgesBinCount data.size)
      let binWidth := (maxVal - minVal) / numBins.toFloat
      -- Add small epsilon to max to include the maximum value
      let edges := Array.range (numBins + 1) |>.map fun i =>
        minVal + i.toFloat * binWidth
      -- Ensure last edge is slightly past max
      edges.set! numBins (maxVal + binWidth * 0.001)

  if binEdges.size < 2 then return #[]

  -- Initialize bins
  let numBins := binEdges.size - 1
  let mut counts : Array Nat := Array.replicate numBins 0

  -- Count values in each bin
  for v in data do
    -- Find the bin for this value
    for i in [0:numBins] do
      let lower := binEdges[i]!
      let upper := binEdges[i + 1]!
      -- Last bin is inclusive on both ends
      let inBin := if i == numBins - 1 then v >= lower && v <= upper
                   else v >= lower && v < upper
      if inBin then
        counts := counts.set! i (counts[i]! + 1)
        break

  -- Build bin structures
  let totalCount := data.size.toFloat
  let mut bins : Array Bin := #[]
  for i in [0:numBins] do
    let lower := binEdges[i]!
    let upper := binEdges[i + 1]!
    let count := counts[i]!
    let binWidth := upper - lower
    let density := if config.normalize && binWidth > 0.0 then
      count.toFloat / totalCount / binWidth
    else
      count.toFloat / totalCount
    bins := bins.push { lower, upper, count, density }

  return bins

/-- Custom spec for histogram rendering. -/
def histogramSpec (bins : Array Bin) (variant : HistogramVariant) (theme : Theme)
    (dims : Dimensions := defaultDimensions) (showDensity : Bool := false) : CustomSpec := {
  measure := fun _ _ => (dims.marginLeft + dims.marginRight + 50, dims.marginTop + dims.marginBottom + 30)
  collect := fun layout =>
    let rect := layout.contentRect
    let cmds : Array RenderCommand := #[]

    -- Use actual container size for responsive layout
    let actualWidth := rect.width
    let actualHeight := rect.height

    -- Calculate chart area
    let chartX := rect.x + dims.marginLeft
    let chartY := rect.y + dims.marginTop
    let chartWidth := actualWidth - dims.marginLeft - dims.marginRight
    let chartHeight := actualHeight - dims.marginTop - dims.marginBottom

    if bins.isEmpty then cmds else

    -- Find max frequency/density for scaling
    let maxY : Float := Id.run do
      let first := if showDensity then bins[0]!.density else bins[0]!.count.toFloat
      let mut maxVal := first
      for b in bins do
        let val := if showDensity then b.density else b.count.toFloat
        if val > maxVal then maxVal := val
      if maxVal <= 0.0 then (1.0 : Float) else maxVal * 1.1  -- Add 10% headroom

    -- Find X range from bin edges
    let minX := bins[0]!.lower
    let maxX := bins[bins.size - 1]!.upper
    let rangeX := maxX - minX

    -- Draw background
    let bgRect := Arbor.Rect.mk' rect.x rect.y actualWidth actualHeight
    let cmds := cmds.push (.fillRect bgRect (theme.panel.background.withAlpha 0.3) 6.0)

    -- Draw horizontal grid lines
    let cmds := if dims.showGridLines && dims.gridLineCount > 0 then
      Id.run do
        let mut cmds := cmds
        for i in [0:dims.gridLineCount + 1] do
          let ratio := i.toFloat / dims.gridLineCount.toFloat
          let lineY := chartY + chartHeight - (ratio * chartHeight)
          let lineRect := Arbor.Rect.mk' chartX lineY chartWidth 1.0
          cmds := cmds.push (.fillRect lineRect (Color.gray 0.3) 0.0)
        cmds
    else cmds

    -- Draw histogram bars
    let fillColor := variantColor variant theme
    let cmds := Id.run do
      let mut cmds := cmds
      for b in bins do
        let binWidth := b.upper - b.lower
        let barX := chartX + ((b.lower - minX) / rangeX) * chartWidth
        let barWidth := (binWidth / rangeX) * chartWidth - dims.barGap
        let yVal := if showDensity then b.density else b.count.toFloat
        let barHeight := (yVal / maxY) * chartHeight
        let barY := chartY + chartHeight - barHeight
        let barRect := Arbor.Rect.mk' barX barY (max 1.0 barWidth) barHeight
        cmds := cmds.push (.fillRect barRect fillColor dims.cornerRadius)
      cmds

    -- Draw Y-axis labels (frequency/density)
    let cmds := if dims.showFrequencyLabels && dims.gridLineCount > 0 then
      Id.run do
        let mut cmds := cmds
        for i in [0:dims.gridLineCount + 1] do
          let ratio := i.toFloat / dims.gridLineCount.toFloat
          let value := ratio * maxY
          let labelY := chartY + chartHeight - (ratio * chartHeight) + 4
          let labelText := if showDensity then
            let pct := (value * 100).floor.toUInt32
            s!"{pct}%"
          else
            formatValue value
          cmds := cmds.push (.fillText labelText (rect.x + 4) labelY theme.smallFont theme.textMuted)
        cmds
    else cmds

    -- Draw X-axis labels (bin edges)
    let cmds := if dims.showBinLabels then
      Id.run do
        let mut cmds := cmds
        -- Show a few key bin edges
        let labelCount := min 6 (bins.size + 1)
        let step := if labelCount > 1 then bins.size / (labelCount - 1) else 1
        for i in [0:labelCount] do
          let binIdx := min (i * step) bins.size
          let value := if binIdx < bins.size then bins[binIdx]!.lower else bins[bins.size - 1]!.upper
          let labelX := chartX + ((value - minX) / rangeX) * chartWidth
          let labelY := chartY + chartHeight + 16
          cmds := cmds.push (.fillText (formatValue value) labelX labelY theme.smallFont theme.textMuted)
        cmds
    else cmds

    -- Draw axes
    let axisColor := Color.gray 0.5
    let yAxisRect := Arbor.Rect.mk' chartX chartY 1.0 chartHeight
    let cmds := cmds.push (.fillRect yAxisRect axisColor 0.0)
    let xAxisRect := Arbor.Rect.mk' chartX (chartY + chartHeight) chartWidth 1.0
    cmds.push (.fillRect xAxisRect axisColor 0.0)

  draw := none
}

/-- Custom spec for histogram with pre-computed bin counts (useful for categorical data). -/
def histogramFromCountsSpec (labels : Array String) (counts : Array Nat)
    (variant : HistogramVariant) (theme : Theme)
    (dims : Dimensions := defaultDimensions) : CustomSpec := {
  measure := fun _ _ => (dims.marginLeft + dims.marginRight + 50, dims.marginTop + dims.marginBottom + 30)
  collect := fun layout =>
    let rect := layout.contentRect
    let cmds : Array RenderCommand := #[]

    -- Use actual container size for responsive layout
    let actualWidth := rect.width
    let actualHeight := rect.height

    let chartX := rect.x + dims.marginLeft
    let chartY := rect.y + dims.marginTop
    let chartWidth := actualWidth - dims.marginLeft - dims.marginRight
    let chartHeight := actualHeight - dims.marginTop - dims.marginBottom

    let binCount := min labels.size counts.size
    if binCount == 0 then cmds else

    -- Find max count
    let maxCount := Id.run do
      let mut maxVal : Nat := 0
      for c in counts do
        if c > maxVal then maxVal := c
      if maxVal == 0 then 1 else maxVal

    let barWidth := (chartWidth - dims.barGap * (binCount - 1).toFloat) / binCount.toFloat

    -- Draw background
    let bgRect := Arbor.Rect.mk' rect.x rect.y actualWidth actualHeight
    let cmds := cmds.push (.fillRect bgRect (theme.panel.background.withAlpha 0.3) 6.0)

    -- Draw horizontal grid lines
    let cmds := if dims.showGridLines && dims.gridLineCount > 0 then
      Id.run do
        let mut cmds := cmds
        for i in [0:dims.gridLineCount + 1] do
          let ratio := i.toFloat / dims.gridLineCount.toFloat
          let lineY := chartY + chartHeight - (ratio * chartHeight)
          let lineRect := Arbor.Rect.mk' chartX lineY chartWidth 1.0
          cmds := cmds.push (.fillRect lineRect (Color.gray 0.3) 0.0)
        cmds
    else cmds

    -- Draw bars
    let fillColor := variantColor variant theme
    let cmds := Id.run do
      let mut cmds := cmds
      for i in [0:binCount] do
        let count := counts[i]!
        let barX := chartX + i.toFloat * (barWidth + dims.barGap)
        let barHeight := (count.toFloat / maxCount.toFloat) * chartHeight
        let barY := chartY + chartHeight - barHeight
        let barRect := Arbor.Rect.mk' barX barY barWidth barHeight
        cmds := cmds.push (.fillRect barRect fillColor dims.cornerRadius)
      cmds

    -- Draw Y-axis labels
    let cmds := if dims.showFrequencyLabels && dims.gridLineCount > 0 then
      Id.run do
        let mut cmds := cmds
        for i in [0:dims.gridLineCount + 1] do
          let ratio := i.toFloat / dims.gridLineCount.toFloat
          let value := ratio * maxCount.toFloat
          let labelY := chartY + chartHeight - (ratio * chartHeight) + 4
          cmds := cmds.push (.fillText (formatValue value) (rect.x + 4) labelY theme.smallFont theme.textMuted)
        cmds
    else cmds

    -- Draw X-axis labels
    let cmds := if dims.showBinLabels then
      Id.run do
        let mut cmds := cmds
        for i in [0:binCount] do
          let label := labels[i]!
          let labelX := chartX + i.toFloat * (barWidth + dims.barGap) + barWidth / 2
          let labelY := chartY + chartHeight + 16
          cmds := cmds.push (.fillText label labelX labelY theme.smallFont theme.textMuted)
        cmds
    else cmds

    -- Draw axes
    let axisColor := Color.gray 0.5
    let yAxisRect := Arbor.Rect.mk' chartX chartY 1.0 chartHeight
    let cmds := cmds.push (.fillRect yAxisRect axisColor 0.0)
    let xAxisRect := Arbor.Rect.mk' chartX (chartY + chartHeight) chartWidth 1.0
    cmds.push (.fillRect xAxisRect axisColor 0.0)

  draw := none
}

end Histogram

/-- Build a histogram visual from raw data (WidgetBuilder version).
    Automatically computes bins from the data.
    - `name`: Widget name for identification
    - `data`: Array of numerical values to bin
    - `variant`: Color variant for bars
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
    - `binConfig`: Configuration for binning (number of bins, edges, normalization)
-/
def histogramVisual (name : String) (data : Array Float)
    (variant : HistogramVariant := .primary) (theme : Theme)
    (dims : Histogram.Dimensions := Histogram.defaultDimensions)
    (binConfig : Histogram.BinConfig := Histogram.defaultBinConfig) : WidgetBuilder := do
  let wid ← freshId
  let bins := Histogram.computeBins data binConfig
  let chart ← custom (Histogram.histogramSpec bins variant theme dims binConfig.normalize) {
    width := .percent 1.0
    height := .percent 1.0
    flexItem := some (Trellis.FlexItem.growing 1)
  }
  let style : BoxStyle := { width := .percent 1.0, height := .percent 1.0, flexItem := some (Trellis.FlexItem.growing 1) }
  let props : Trellis.FlexContainer := { Trellis.FlexContainer.column 0 with alignItems := .stretch }
  pure (.flex wid (some name) props style #[chart])

/-- Build a histogram visual from pre-computed bins (WidgetBuilder version).
    - `name`: Widget name for identification
    - `bins`: Pre-computed histogram bins
    - `variant`: Color variant for bars
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
    - `showDensity`: Whether to show density instead of counts
-/
def histogramFromBinsVisual (name : String) (bins : Array Histogram.Bin)
    (variant : HistogramVariant := .primary) (theme : Theme)
    (dims : Histogram.Dimensions := Histogram.defaultDimensions)
    (showDensity : Bool := false) : WidgetBuilder := do
  let wid ← freshId
  let chart ← custom (Histogram.histogramSpec bins variant theme dims showDensity) {
    width := .percent 1.0
    height := .percent 1.0
    flexItem := some (Trellis.FlexItem.growing 1)
  }
  let style : BoxStyle := { width := .percent 1.0, height := .percent 1.0, flexItem := some (Trellis.FlexItem.growing 1) }
  let props : Trellis.FlexContainer := { Trellis.FlexContainer.column 0 with alignItems := .stretch }
  pure (.flex wid (some name) props style #[chart])

/-- Build a histogram visual from categorical counts (WidgetBuilder version).
    - `name`: Widget name for identification
    - `labels`: Category labels
    - `counts`: Count for each category
    - `variant`: Color variant for bars
    - `theme`: Theme for styling
    - `dims`: Chart dimensions
-/
def histogramFromCountsVisual (name : String) (labels : Array String) (counts : Array Nat)
    (variant : HistogramVariant := .primary) (theme : Theme)
    (dims : Histogram.Dimensions := Histogram.defaultDimensions) : WidgetBuilder := do
  let wid ← freshId
  let chart ← custom (Histogram.histogramFromCountsSpec labels counts variant theme dims) {
    width := .percent 1.0
    height := .percent 1.0
    flexItem := some (Trellis.FlexItem.growing 1)
  }
  let style : BoxStyle := { width := .percent 1.0, height := .percent 1.0, flexItem := some (Trellis.FlexItem.growing 1) }
  let props : Trellis.FlexContainer := { Trellis.FlexContainer.column 0 with alignItems := .stretch }
  pure (.flex wid (some name) props style #[chart])

/-! ## Reactive Histogram Components (FRP-based)

These use WidgetM for declarative composition.
-/

open Reactive Reactive.Host
open Afferent.Canopy.Reactive

/-- Histogram result - provides access to computed bins. -/
structure HistogramResult where
  /-- The computed bins. -/
  bins : Reactive.Dynamic Spider (Array Histogram.Bin)

/-- Create a histogram component from raw data using WidgetM.
    Automatically computes bins using Sturges' formula or specified count.
    - `data`: Array of numerical values to bin
    - `theme`: Theme for styling
    - `variant`: Color variant for bars
    - `dims`: Chart dimensions
    - `binConfig`: Configuration for binning
-/
def histogram (data : Array Float)
    (theme : Theme) (variant : HistogramVariant := .primary)
    (dims : Histogram.Dimensions := Histogram.defaultDimensions)
    (binConfig : Histogram.BinConfig := Histogram.defaultBinConfig)
    : WidgetM HistogramResult := do
  let name ← registerComponentW "histogram" (isInteractive := false)

  let bins := Histogram.computeBins data binConfig
  let binsDyn ← Dynamic.pureM bins

  emit do
    pure (histogramFromBinsVisual name bins variant theme dims binConfig.normalize)

  pure { bins := binsDyn }

/-- Create a histogram from pre-computed bins.
    - `bins`: Pre-computed histogram bins
    - `theme`: Theme for styling
    - `variant`: Color variant for bars
    - `dims`: Chart dimensions
    - `showDensity`: Whether to show density instead of counts
-/
def histogramFromBins (bins : Array Histogram.Bin)
    (theme : Theme) (variant : HistogramVariant := .primary)
    (dims : Histogram.Dimensions := Histogram.defaultDimensions)
    (showDensity : Bool := false)
    : WidgetM HistogramResult := do
  let name ← registerComponentW "histogram" (isInteractive := false)

  let binsDyn ← Dynamic.pureM bins

  emit do
    pure (histogramFromBinsVisual name bins variant theme dims showDensity)

  pure { bins := binsDyn }

/-- Histogram from counts result. -/
structure HistogramFromCountsResult where
  counts : Reactive.Dynamic Spider (Array Nat)

/-- Create a histogram from categorical labels and counts.
    - `labels`: Category labels
    - `counts`: Count for each category
    - `theme`: Theme for styling
    - `variant`: Color variant for bars
    - `dims`: Chart dimensions
-/
def histogramFromCounts (labels : Array String) (counts : Array Nat)
    (theme : Theme) (variant : HistogramVariant := .primary)
    (dims : Histogram.Dimensions := Histogram.defaultDimensions)
    : WidgetM HistogramFromCountsResult := do
  let name ← registerComponentW "histogram" (isInteractive := false)

  let countsDyn ← Dynamic.pureM counts

  emit do
    pure (histogramFromCountsVisual name labels counts variant theme dims)

  pure { counts := countsDyn }

/-- Create a histogram that updates based on an external bin event stream.
    - `initialBins`: Initial computed bins
    - `binUpdates`: Event stream of bin updates
    - `theme`: Theme for styling
    - `variant`: Color variant
    - `dims`: Chart dimensions
    - `showDensity`: Whether to show density instead of counts
-/
def histogramWithEvents (initialBins : Array Histogram.Bin)
    (binUpdates : Reactive.Event Spider (Array Histogram.Bin))
    (theme : Theme) (variant : HistogramVariant := .primary)
    (dims : Histogram.Dimensions := Histogram.defaultDimensions)
    (showDensity : Bool := false)
    : WidgetM HistogramResult := do
  let name ← registerComponentW "histogram" (isInteractive := false)

  let binsDyn ← Reactive.holdDyn initialBins binUpdates

  emit do
    let bins ← binsDyn.sample
    pure (histogramFromBinsVisual name bins variant theme dims showDensity)

  pure { bins := binsDyn }

end Afferent.Canopy

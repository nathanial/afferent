/-
  Performance / Stress Tests for Canopy Chart Widgets

  Tests chart rendering with large datasets to measure per-frame
  computation costs and identify expensive operations.
-/

import Crucible
import Chronos
import Trellis
import Afferent.Canopy.Theme
import Afferent.Canopy.Widget.Charts.TreemapChart
import Afferent.Canopy.Widget.Charts.StackedAreaChart
import Afferent.Canopy.Widget.Charts.Heatmap
import Afferent.Canopy.Widget.Charts.SankeyDiagram
import Afferent.Canopy.Widget.Charts.BarChart
import Afferent.Canopy.Widget.Charts.LineChart
import Afferent.Canopy.Widget.Charts.ScatterPlot
import Afferent.Canopy.Widget.Charts.AreaChart
import Afferent.Canopy.Widget.Charts.BoxPlot
import Afferent.Canopy.Widget.Charts.BubbleChart
import Afferent.Canopy.Widget.Charts.CandlestickChart
import Afferent.Canopy.Widget.Charts.DonutChart
import Afferent.Canopy.Widget.Charts.PieChart
import Afferent.Canopy.Widget.Charts.FunnelChart
import Afferent.Canopy.Widget.Charts.GaugeChart
import Afferent.Canopy.Widget.Charts.GroupedBarChart
import Afferent.Canopy.Widget.Charts.StackedBarChart
import Afferent.Canopy.Widget.Charts.HorizontalBarChart
import Afferent.Canopy.Widget.Charts.Histogram
import Afferent.Canopy.Widget.Charts.RadarChart
import Afferent.Canopy.Widget.Charts.WaterfallChart

namespace Afferent.Tests.ChartPerformanceTests

open Crucible
open Afferent.Canopy
open Afferent.Arbor
open Trellis

testSuite "Chart Performance Tests"

/-! ## Helper Functions -/

/-- Force strict evaluation to defeat lazy evaluation for timing. -/
@[inline] def strictEval {α : Type} (x : α) : IO α := do
  let ref ← IO.mkRef x
  ref.get

/-- Create a simple computed layout for testing chart collect functions. -/
def mockLayout (width height : Float) : ComputedLayout :=
  let rect : LayoutRect := { x := 0, y := 0, width := width, height := height }
  ComputedLayout.simple 0 rect

/-! ## Data Generators -/

/-- Generate N float values with pseudo-random distribution. -/
def generateValues (n : Nat) : Array Float := Id.run do
  let mut result : Array Float := #[]
  for i in [:n] do
    -- Simple pseudo-random pattern: sin-based variation
    let base := 50.0 + 30.0 * Float.sin (i.toFloat * 0.1)
    let variation := (i % 17).toFloat * 2.0
    result := result.push (base + variation)
  result

/-- Generate N scatter plot points with pseudo-random positions. -/
def generateScatterPoints (n : Nat) : Array ScatterPlot.DataPoint := Id.run do
  let mut result : Array ScatterPlot.DataPoint := #[]
  for i in [:n] do
    let x := (i % 1000).toFloat + Float.sin (i.toFloat * 0.05) * 50.0
    let y := (i % 800).toFloat + Float.cos (i.toFloat * 0.07) * 40.0
    result := result.push { x, y }
  result

/-- Generate N flat treemap nodes (no hierarchy). -/
def generateFlatTreemapNodes (n : Nat) : TreemapChart.Data := Id.run do
  let mut nodes : Array TreemapChart.TreeNode := #[]
  for i in [:n] do
    let value := 10.0 + (i % 100).toFloat * 5.0 + Float.sin (i.toFloat * 0.1) * 20.0
    nodes := nodes.push {
      label := s!"Node {i}"
      value := value
    }
  { nodes }

/-- Generate hierarchical treemap with given depth and fanout.
    Total nodes = sum of fanout^i for i in 0..depth = (fanout^(depth+1) - 1) / (fanout - 1) -/
def generateHierarchicalTreemap (depth fanOut : Nat) : TreemapChart.Data :=
  -- Build level with path-based IDs to avoid mutable state
  let rec buildLevel (currentDepth : Nat) (pathId : Nat) : Array TreemapChart.TreeNode :=
    if currentDepth >= depth then
      -- Leaf level
      Array.ofFn fun (i : Fin fanOut) =>
        let nodeId := pathId * fanOut + i.val
        let value := 10.0 + (nodeId % 50).toFloat * 2.0
        { label := s!"Leaf {nodeId}", value := value }
    else
      -- Internal level
      Array.ofFn fun (i : Fin fanOut) =>
        let nodeId := pathId * fanOut + i.val
        let children := buildLevel (currentDepth + 1) nodeId
        { label := s!"Branch {nodeId}", value := 0, children := children }
  { nodes := buildLevel 0 0 }

/-- Generate stacked area chart data with multiple series. -/
def generateStackedData (numSeries numPoints : Nat) : StackedAreaChart.Data := Id.run do
  let mut series : Array StackedAreaChart.Series := #[]
  for s in [:numSeries] do
    let mut values : Array Float := #[]
    for p in [:numPoints] do
      let base := 20.0 + (s * 10).toFloat
      let variation := Float.sin ((p.toFloat + s.toFloat * 7.0) * 0.1) * 15.0
      values := values.push (base + variation)
    series := series.push {
      name := s!"Series {s}"
      values := values
    }

  let mut labels : Array String := #[]
  for i in [:numPoints] do
    labels := labels.push s!"P{i}"

  { labels, series }

/-- Generate heatmap grid data. -/
def generateHeatmapGrid (rows cols : Nat) : Heatmap.Data := Id.run do
  let mut values : Array (Array Float) := #[]
  for r in [:rows] do
    let mut row : Array Float := #[]
    for c in [:cols] do
      -- Create a gradient pattern with some variation
      let base := (r.toFloat / rows.toFloat + c.toFloat / cols.toFloat) * 50.0
      let variation := Float.sin (r.toFloat * 0.2) * Float.cos (c.toFloat * 0.2) * 20.0
      row := row.push (base + variation)
    values := values.push row

  { values }

/-- Generate Sankey diagram data with columns of nodes. -/
def generateSankeyData (numColumns nodesPerCol : Nat) : SankeyDiagram.Data := Id.run do
  let mut nodes : Array SankeyDiagram.Node := #[]
  let mut links : Array SankeyDiagram.Link := #[]

  -- Create nodes for each column
  for col in [:numColumns] do
    for n in [:nodesPerCol] do
      let nodeId := s!"c{col}n{n}"
      nodes := nodes.push {
        id := nodeId
        label := s!"Node {col}-{n}"
        column := col
      }

  -- Create links between adjacent columns
  for col in [:numColumns - 1] do
    for srcN in [:nodesPerCol] do
      let srcId := s!"c{col}n{srcN}"
      -- Connect to 2-3 nodes in next column
      let numLinks := 2 + (srcN % 2)
      for linkIdx in [:numLinks] do
        let tgtN := (srcN + linkIdx) % nodesPerCol
        let tgtId := s!"c{col + 1}n{tgtN}"
        let value := 10.0 + (srcN * 5 + linkIdx * 3).toFloat
        links := links.push {
          source := srcId
          target := tgtId
          value := value
        }

  { nodes, links }

/-- Generate area chart series data. -/
def generateAreaSeries (numSeries numPoints : Nat) : Array AreaChart.Series := Id.run do
  let mut result : Array AreaChart.Series := #[]
  for s in [:numSeries] do
    let mut values : Array Float := #[]
    for p in [:numPoints] do
      let base := 30.0 + (s * 8).toFloat
      let variation := Float.sin ((p.toFloat + s.toFloat * 5.0) * 0.1) * 20.0
      values := values.push (base + variation)
    result := result.push {
      values := values
      label := some s!"Series {s}"
    }
  result

/-- Generate box plot summary data. -/
def generateBoxPlotData (n : Nat) : Array BoxPlot.Summary := Id.run do
  let mut result : Array BoxPlot.Summary := #[]
  for i in [:n] do
    let base := 20.0 + (i % 50).toFloat * 2.0
    let spread := 10.0 + (i % 20).toFloat
    -- Generate outliers for some boxes
    let outliers := if i % 5 == 0 then
      #[base - spread * 2.5, base + spread * 2.5]
    else
      #[]
    result := result.push {
      min := base - spread
      q1 := base - spread * 0.5
      median := base
      q3 := base + spread * 0.5
      max := base + spread
      outliers := outliers
      label := some s!"Box {i}"
    }
  result

/-- Generate bubble chart data points. -/
def generateBubblePoints (n : Nat) : Array BubbleChart.DataPoint := Id.run do
  let mut result : Array BubbleChart.DataPoint := #[]
  for i in [:n] do
    let x := (i % 1000).toFloat + Float.sin (i.toFloat * 0.03) * 100.0
    let y := (i % 800).toFloat + Float.cos (i.toFloat * 0.05) * 80.0
    let size := 5.0 + (i % 30).toFloat * 2.0
    result := result.push { x, y, size }
  result

/-- Generate candlestick chart data. -/
def generateCandlestickData (n : Nat) : Array CandlestickChart.Candle := Id.run do
  let mut result : Array CandlestickChart.Candle := #[]
  let mut price := 100.0
  for i in [:n] do
    -- Simulate price movement
    let change := Float.sin (i.toFloat * 0.1) * 5.0 + (i % 7).toFloat - 3.0
    let openPrice := price
    let closePrice := price + change
    let highPrice := max openPrice closePrice + (i % 5).toFloat
    let lowPrice := min openPrice closePrice - (i % 4).toFloat
    price := closePrice
    result := result.push {
      openPrice := openPrice
      highPrice := highPrice
      lowPrice := lowPrice
      closePrice := closePrice
      label := some s!"Day {i}"
    }
  result

/-- Generate pie/donut chart slices. -/
def generateSlices (n : Nat) : Array PieChart.Slice := Id.run do
  let mut result : Array PieChart.Slice := #[]
  for i in [:n] do
    let value := 10.0 + (i % 20).toFloat * 5.0 + Float.sin (i.toFloat * 0.3) * 8.0
    result := result.push {
      value := value
      label := some s!"Slice {i}"
    }
  result

/-- Generate funnel chart stages. -/
def generateFunnelStages (n : Nat) : Array FunnelChart.Stage := Id.run do
  let mut result : Array FunnelChart.Stage := #[]
  let mut value := 1000.0
  for i in [:n] do
    result := result.push {
      label := s!"Stage {i}"
      value := value
    }
    -- Decrease by 5-20% each stage
    let dropRate := 0.85 - (i % 10).toFloat * 0.01
    value := value * dropRate
  result

/-- Generate gauge chart data with segments. -/
def generateGaugeData (numSegments : Nat) : GaugeChart.Data := Id.run do
  let mut segments : Array GaugeChart.Segment := #[]
  for i in [:numSegments] do
    let startFrac := i.toFloat / numSegments.toFloat
    let endFrac := (i + 1).toFloat / numSegments.toFloat
    -- Alternate colors
    let color := if i % 3 == 0 then Color.rgba 0.2 0.7 0.3 1.0
                 else if i % 3 == 1 then Color.rgba 0.9 0.7 0.1 1.0
                 else Color.rgba 0.9 0.3 0.2 1.0
    segments := segments.push { startFrac, endFrac, color }
  {
    value := 65.0
    minValue := 0.0
    maxValue := 100.0
    label := some "Performance"
    segments := segments
  }

/-- Generate grouped/stacked bar chart data. -/
def generateGroupedBarData (numCategories numSeries : Nat) : GroupedBarChart.Data := Id.run do
  let mut categories : Array String := #[]
  for i in [:numCategories] do
    categories := categories.push s!"Cat {i}"

  let mut series : Array GroupedBarChart.Series := #[]
  for s in [:numSeries] do
    let mut values : Array Float := #[]
    for c in [:numCategories] do
      let value := 20.0 + (s * 5 + c * 3).toFloat + Float.sin ((s + c).toFloat * 0.2) * 15.0
      values := values.push value
    series := series.push {
      name := s!"Series {s}"
      values := values
    }

  { categories, series }

/-- Generate radar chart data. -/
def generateRadarData (numAxes numSeries : Nat) : RadarChart.Data := Id.run do
  let mut axisLabels : Array String := #[]
  for i in [:numAxes] do
    axisLabels := axisLabels.push s!"Axis {i}"

  let mut series : Array RadarChart.Series := #[]
  for s in [:numSeries] do
    let mut values : Array Float := #[]
    for a in [:numAxes] do
      let value := 0.3 + Float.sin ((s.toFloat + a.toFloat) * 0.5) * 0.3 + (a % 5).toFloat * 0.1
      values := values.push (min 1.0 (max 0.0 value))
    series := series.push {
      name := s!"Series {s}"
      values := values
    }

  { axisLabels, series }

/-- Generate waterfall chart data. -/
def generateWaterfallData (n : Nat) : WaterfallChart.Data := Id.run do
  let mut bars : Array WaterfallChart.Bar := #[]
  -- Start with initial value
  bars := bars.push { label := "Start", value := 100.0, barType := .initial }

  for i in [:n - 2] do  -- Leave room for total
    let isIncrease := i % 3 != 2
    let value := 5.0 + (i % 15).toFloat * 2.0
    if isIncrease then
      bars := bars.push { label := s!"+{i}", value := value, barType := .increase }
    else
      bars := bars.push { label := s!"-{i}", value := value, barType := .decrease }

  -- End with total
  bars := bars.push { label := "Total", value := 0.0, barType := .total }
  { bars }

/-! ## TreemapChart Tests (HIGH PRIORITY - squarify is O(n² log n)) -/

test "perf: TreemapChart squarify 1000 nodes" := do
  let data := generateFlatTreemapNodes 1000
  let theme := Theme.dark
  let dims := TreemapChart.defaultDimensions
  let spec := TreemapChart.treemapChartSpec data theme dims
  let layout := mockLayout 800 600

  let start ← Chronos.MonotonicTime.now
  for _ in [:10] do
    let _ ← strictEval (spec.collect layout)
  let elapsed ← start.elapsed

  IO.println s!"  [TreemapChart 1000 nodes x10: {elapsed}]"

test "perf: TreemapChart squarify 5000 nodes" := do
  let data := generateFlatTreemapNodes 5000
  let theme := Theme.dark
  let dims := TreemapChart.defaultDimensions
  let spec := TreemapChart.treemapChartSpec data theme dims
  let layout := mockLayout 1200 800

  let start ← Chronos.MonotonicTime.now
  for _ in [:5] do
    let _ ← strictEval (spec.collect layout)
  let elapsed ← start.elapsed

  IO.println s!"  [TreemapChart 5000 nodes x5: {elapsed}]"

test "perf: TreemapChart squarify 10000 nodes" := do
  let data := generateFlatTreemapNodes 10000
  let theme := Theme.dark
  let dims := TreemapChart.defaultDimensions
  let spec := TreemapChart.treemapChartSpec data theme dims
  let layout := mockLayout 1600 1000

  let start ← Chronos.MonotonicTime.now
  for _ in [:3] do
    let _ ← strictEval (spec.collect layout)
  let elapsed ← start.elapsed

  IO.println s!"  [TreemapChart 10000 nodes x3: {elapsed}]"

test "perf: TreemapChart hierarchical depth=3 fanout=10 (1111 nodes)" := do
  let data := generateHierarchicalTreemap 3 10
  let theme := Theme.dark
  let dims := TreemapChart.defaultDimensions
  let spec := TreemapChart.treemapChartSpec data theme dims
  let layout := mockLayout 1000 800

  let start ← Chronos.MonotonicTime.now
  for _ in [:10] do
    let _ ← strictEval (spec.collect layout)
  let elapsed ← start.elapsed

  IO.println s!"  [TreemapChart hierarchical 3x10 x10: {elapsed}]"

/-! ## StackedAreaChart Tests (cumulative sums O(series × points)) -/

test "perf: StackedAreaChart 10 series x 1000 points" := do
  let data := generateStackedData 10 1000
  let theme := Theme.dark
  let dims := StackedAreaChart.defaultDimensions
  let spec := StackedAreaChart.stackedAreaChartSpec data theme dims
  let layout := mockLayout 800 400

  let start ← Chronos.MonotonicTime.now
  for _ in [:20] do
    let _ ← strictEval (spec.collect layout)
  let elapsed ← start.elapsed

  IO.println s!"  [StackedAreaChart 10x1000 x20: {elapsed}]"

test "perf: StackedAreaChart 20 series x 5000 points" := do
  let data := generateStackedData 20 5000
  let theme := Theme.dark
  let dims := StackedAreaChart.defaultDimensions
  let spec := StackedAreaChart.stackedAreaChartSpec data theme dims
  let layout := mockLayout 1200 600

  let start ← Chronos.MonotonicTime.now
  for _ in [:5] do
    let _ ← strictEval (spec.collect layout)
  let elapsed ← start.elapsed

  IO.println s!"  [StackedAreaChart 20x5000 x5: {elapsed}]"

test "perf: StackedAreaChart 50 series x 10000 points" := do
  let data := generateStackedData 50 10000
  let theme := Theme.dark
  let dims := StackedAreaChart.defaultDimensions
  let spec := StackedAreaChart.stackedAreaChartSpec data theme dims
  let layout := mockLayout 1600 800

  let start ← Chronos.MonotonicTime.now
  for _ in [:2] do
    let _ ← strictEval (spec.collect layout)
  let elapsed ← start.elapsed

  IO.println s!"  [StackedAreaChart 50x10000 x2: {elapsed}]"

/-! ## Heatmap Tests (range finding O(rows × cols)) -/

test "perf: Heatmap 100x100 grid (10K cells)" := do
  let data := generateHeatmapGrid 100 100
  let theme := Theme.dark
  let dims := Heatmap.defaultDimensions
  let spec := Heatmap.heatmapSpec data .viridis theme dims
  let layout := mockLayout 800 600

  let start ← Chronos.MonotonicTime.now
  for _ in [:20] do
    let _ ← strictEval (spec.collect layout)
  let elapsed ← start.elapsed

  IO.println s!"  [Heatmap 100x100 x20: {elapsed}]"

test "perf: Heatmap 300x300 grid (90K cells)" := do
  let data := generateHeatmapGrid 300 300
  let theme := Theme.dark
  let dims := Heatmap.defaultDimensions
  let spec := Heatmap.heatmapSpec data .viridis theme dims
  let layout := mockLayout 1200 900

  let start ← Chronos.MonotonicTime.now
  for _ in [:5] do
    let _ ← strictEval (spec.collect layout)
  let elapsed ← start.elapsed

  IO.println s!"  [Heatmap 300x300 x5: {elapsed}]"

test "perf: Heatmap 500x500 grid (250K cells)" := do
  let data := generateHeatmapGrid 500 500
  let theme := Theme.dark
  let dims := Heatmap.defaultDimensions
  let spec := Heatmap.heatmapSpec data .viridis theme dims
  let layout := mockLayout 1600 1200

  let start ← Chronos.MonotonicTime.now
  for _ in [:2] do
    let _ ← strictEval (spec.collect layout)
  let elapsed ← start.elapsed

  IO.println s!"  [Heatmap 500x500 x2: {elapsed}]"

/-! ## SankeyDiagram Tests (cached layout - comparing layout vs render) -/

test "perf: SankeyDiagram layout computation 50 nodes" := do
  let data := generateSankeyData 5 10  -- 5 columns × 10 nodes = 50 nodes
  let dims := SankeyDiagram.defaultDimensions

  -- Time layout computation
  let start ← Chronos.MonotonicTime.now
  for _ in [:50] do
    let _ ← strictEval (SankeyDiagram.computeLayout data dims)
  let layoutTime ← start.elapsed

  -- Time cached render
  let cachedLayout := SankeyDiagram.computeLayout data dims
  let theme := Theme.dark
  let spec := SankeyDiagram.sankeyDiagramSpecCached cachedLayout data theme dims
  let layout := mockLayout 800 500

  let start2 ← Chronos.MonotonicTime.now
  for _ in [:50] do
    let _ ← strictEval (spec.collect layout)
  let renderTime ← start2.elapsed

  IO.println s!"  [SankeyDiagram 50 nodes: layout={layoutTime}, render={renderTime}]"

test "perf: SankeyDiagram layout computation 200 nodes" := do
  let data := generateSankeyData 10 20  -- 10 columns × 20 nodes = 200 nodes
  let dims := SankeyDiagram.defaultDimensions

  -- Time layout computation
  let start ← Chronos.MonotonicTime.now
  for _ in [:20] do
    let _ ← strictEval (SankeyDiagram.computeLayout data dims)
  let layoutTime ← start.elapsed

  -- Time cached render
  let cachedLayout := SankeyDiagram.computeLayout data dims
  let theme := Theme.dark
  let spec := SankeyDiagram.sankeyDiagramSpecCached cachedLayout data theme dims
  let layout := mockLayout 1200 800

  let start2 ← Chronos.MonotonicTime.now
  for _ in [:20] do
    let _ ← strictEval (spec.collect layout)
  let renderTime ← start2.elapsed

  IO.println s!"  [SankeyDiagram 200 nodes: layout={layoutTime}, render={renderTime}]"

/-! ## Baseline Chart Tests (Simple O(n) - BarChart, LineChart, ScatterPlot) -/

test "perf: BarChart 10000 bars" := do
  let data := generateValues 10000
  let labels : Array String := #[]  -- No labels for perf test
  let theme := Theme.dark
  let dims := BarChart.defaultDimensions
  let spec := BarChart.barChartSpec data labels .primary theme dims
  let layout := mockLayout 10000 400  -- Very wide to fit all bars

  let start ← Chronos.MonotonicTime.now
  for _ in [:10] do
    let _ ← strictEval (spec.collect layout)
  let elapsed ← start.elapsed

  IO.println s!"  [BarChart 10000 bars x10: {elapsed}]"

test "perf: BarChart 50000 bars" := do
  let data := generateValues 50000
  let labels : Array String := #[]
  let theme := Theme.dark
  let dims := BarChart.defaultDimensions
  let spec := BarChart.barChartSpec data labels .primary theme dims
  let layout := mockLayout 50000 400

  let start ← Chronos.MonotonicTime.now
  for _ in [:3] do
    let _ ← strictEval (spec.collect layout)
  let elapsed ← start.elapsed

  IO.println s!"  [BarChart 50000 bars x3: {elapsed}]"

test "perf: LineChart 10000 points" := do
  let data := generateValues 10000
  let labels : Array String := #[]
  let theme := Theme.dark
  let dims := LineChart.defaultDimensions
  let spec := LineChart.lineChartSpec data labels .primary theme dims
  let layout := mockLayout 2000 400

  let start ← Chronos.MonotonicTime.now
  for _ in [:10] do
    let _ ← strictEval (spec.collect layout)
  let elapsed ← start.elapsed

  IO.println s!"  [LineChart 10000 points x10: {elapsed}]"

test "perf: LineChart 100000 points" := do
  let data := generateValues 100000
  let labels : Array String := #[]
  let theme := Theme.dark
  let dims := LineChart.defaultDimensions
  let spec := LineChart.lineChartSpec data labels .primary theme dims
  let layout := mockLayout 5000 600

  let start ← Chronos.MonotonicTime.now
  for _ in [:2] do
    let _ ← strictEval (spec.collect layout)
  let elapsed ← start.elapsed

  IO.println s!"  [LineChart 100000 points x2: {elapsed}]"

test "perf: ScatterPlot 50000 points" := do
  let points := generateScatterPoints 50000
  let theme := Theme.dark
  let dims := ScatterPlot.defaultDimensions
  let spec := ScatterPlot.scatterPlotSpec points theme dims
  let layout := mockLayout 1000 800

  let start ← Chronos.MonotonicTime.now
  for _ in [:5] do
    let _ ← strictEval (spec.collect layout)
  let elapsed ← start.elapsed

  IO.println s!"  [ScatterPlot 50000 points x5: {elapsed}]"

test "perf: ScatterPlot 200000 points" := do
  let points := generateScatterPoints 200000
  let theme := Theme.dark
  let dims := ScatterPlot.defaultDimensions
  let spec := ScatterPlot.scatterPlotSpec points theme dims
  let layout := mockLayout 1500 1000

  let start ← Chronos.MonotonicTime.now
  for _ in [:2] do
    let _ ← strictEval (spec.collect layout)
  let elapsed ← start.elapsed

  IO.println s!"  [ScatterPlot 200000 points x2: {elapsed}]"

/-! ## AreaChart Tests -/

test "perf: AreaChart 10000 points" := do
  let data := generateValues 10000
  let labels : Array String := #[]
  let theme := Theme.dark
  let dims := AreaChart.defaultDimensions
  let spec := AreaChart.areaChartSpec data labels .primary theme dims
  let layout := mockLayout 2000 400

  let start ← Chronos.MonotonicTime.now
  for _ in [:10] do
    let _ ← strictEval (spec.collect layout)
  let elapsed ← start.elapsed

  IO.println s!"  [AreaChart 10000 points x10: {elapsed}]"

test "perf: AreaChart 5 series x 5000 points" := do
  let series := generateAreaSeries 5 5000
  let labels : Array String := #[]
  let theme := Theme.dark
  let dims := AreaChart.defaultDimensions
  let spec := AreaChart.multiSeriesSpec series labels theme dims
  let layout := mockLayout 2000 400

  let start ← Chronos.MonotonicTime.now
  for _ in [:5] do
    let _ ← strictEval (spec.collect layout)
  let elapsed ← start.elapsed

  IO.println s!"  [AreaChart 5x5000 x5: {elapsed}]"

/-! ## BoxPlot Tests -/

test "perf: BoxPlot 1000 boxes" := do
  let data := generateBoxPlotData 1000
  let theme := Theme.dark
  let dims := BoxPlot.defaultDimensions
  let spec := BoxPlot.boxPlotSpec data theme dims
  let layout := mockLayout 5000 400

  let start ← Chronos.MonotonicTime.now
  for _ in [:10] do
    let _ ← strictEval (spec.collect layout)
  let elapsed ← start.elapsed

  IO.println s!"  [BoxPlot 1000 boxes x10: {elapsed}]"

test "perf: BoxPlot 5000 boxes" := do
  let data := generateBoxPlotData 5000
  let theme := Theme.dark
  let dims := BoxPlot.defaultDimensions
  let spec := BoxPlot.boxPlotSpec data theme dims
  let layout := mockLayout 20000 400

  let start ← Chronos.MonotonicTime.now
  for _ in [:3] do
    let _ ← strictEval (spec.collect layout)
  let elapsed ← start.elapsed

  IO.println s!"  [BoxPlot 5000 boxes x3: {elapsed}]"

/-! ## Histogram Tests -/

test "perf: Histogram 100000 raw values" := do
  let data := generateValues 100000
  let theme := Theme.dark
  let dims := Histogram.defaultDimensions
  let config : Histogram.BinConfig := { binCount := some 50 }
  let bins := Histogram.computeBins data config
  let spec := Histogram.histogramSpec bins .primary theme dims
  let layout := mockLayout 800 400

  let start ← Chronos.MonotonicTime.now
  for _ in [:5] do
    let _ ← strictEval (spec.collect layout)
  let elapsed ← start.elapsed

  IO.println s!"  [Histogram 100000 values x5: {elapsed}]"

test "perf: Histogram 500000 raw values" := do
  let data := generateValues 500000
  let theme := Theme.dark
  let dims := Histogram.defaultDimensions
  let config : Histogram.BinConfig := { binCount := some 100 }
  let bins := Histogram.computeBins data config
  let spec := Histogram.histogramSpec bins .primary theme dims
  let layout := mockLayout 1200 600

  let start ← Chronos.MonotonicTime.now
  for _ in [:2] do
    let _ ← strictEval (spec.collect layout)
  let elapsed ← start.elapsed

  IO.println s!"  [Histogram 500000 values x2: {elapsed}]"

/-! ## BubbleChart Tests -/

test "perf: BubbleChart 10000 bubbles" := do
  let points := generateBubblePoints 10000
  let theme := Theme.dark
  let dims := BubbleChart.defaultDimensions
  let spec := BubbleChart.bubbleChartSpec points theme dims
  let layout := mockLayout 1000 800

  let start ← Chronos.MonotonicTime.now
  for _ in [:10] do
    let _ ← strictEval (spec.collect layout)
  let elapsed ← start.elapsed

  IO.println s!"  [BubbleChart 10000 bubbles x10: {elapsed}]"

test "perf: BubbleChart 50000 bubbles" := do
  let points := generateBubblePoints 50000
  let theme := Theme.dark
  let dims := BubbleChart.defaultDimensions
  let spec := BubbleChart.bubbleChartSpec points theme dims
  let layout := mockLayout 1500 1000

  let start ← Chronos.MonotonicTime.now
  for _ in [:3] do
    let _ ← strictEval (spec.collect layout)
  let elapsed ← start.elapsed

  IO.println s!"  [BubbleChart 50000 bubbles x3: {elapsed}]"

/-! ## CandlestickChart Tests -/

test "perf: CandlestickChart 1000 candles" := do
  let candles := generateCandlestickData 1000
  let theme := Theme.dark
  let dims := CandlestickChart.defaultDimensions
  let data : CandlestickChart.Data := { candles }
  let spec := CandlestickChart.candlestickChartSpec data theme (dims := dims)
  let layout := mockLayout 2000 400

  let start ← Chronos.MonotonicTime.now
  for _ in [:10] do
    let _ ← strictEval (spec.collect layout)
  let elapsed ← start.elapsed

  IO.println s!"  [CandlestickChart 1000 candles x10: {elapsed}]"

test "perf: CandlestickChart 5000 candles" := do
  let candles := generateCandlestickData 5000
  let theme := Theme.dark
  let dims := CandlestickChart.defaultDimensions
  let data : CandlestickChart.Data := { candles }
  let spec := CandlestickChart.candlestickChartSpec data theme (dims := dims)
  let layout := mockLayout 8000 600

  let start ← Chronos.MonotonicTime.now
  for _ in [:3] do
    let _ ← strictEval (spec.collect layout)
  let elapsed ← start.elapsed

  IO.println s!"  [CandlestickChart 5000 candles x3: {elapsed}]"

/-! ## PieChart Tests -/

test "perf: PieChart 100 slices" := do
  let slices := generateSlices 100
  let theme := Theme.dark
  let dims := PieChart.defaultDimensions
  let spec := PieChart.pieChartSpec slices theme dims
  let layout := mockLayout 400 400

  let start ← Chronos.MonotonicTime.now
  for _ in [:20] do
    let _ ← strictEval (spec.collect layout)
  let elapsed ← start.elapsed

  IO.println s!"  [PieChart 100 slices x20: {elapsed}]"

test "perf: PieChart 1000 slices" := do
  let slices := generateSlices 1000
  let theme := Theme.dark
  let dims := PieChart.defaultDimensions
  let spec := PieChart.pieChartSpec slices theme dims
  let layout := mockLayout 600 600

  let start ← Chronos.MonotonicTime.now
  for _ in [:5] do
    let _ ← strictEval (spec.collect layout)
  let elapsed ← start.elapsed

  IO.println s!"  [PieChart 1000 slices x5: {elapsed}]"

/-! ## DonutChart Tests -/

test "perf: DonutChart 100 slices" := do
  let slices := generateSlices 100
  -- Convert PieChart.Slice to DonutChart.Slice
  let donutSlices := slices.map fun s => DonutChart.Slice.mk s.value s.label s.color
  let theme := Theme.dark
  let dims := DonutChart.defaultDimensions
  let spec := DonutChart.donutChartSpec donutSlices theme dims
  let layout := mockLayout 400 400

  let start ← Chronos.MonotonicTime.now
  for _ in [:20] do
    let _ ← strictEval (spec.collect layout)
  let elapsed ← start.elapsed

  IO.println s!"  [DonutChart 100 slices x20: {elapsed}]"

test "perf: DonutChart 1000 slices" := do
  let slices := generateSlices 1000
  let donutSlices := slices.map fun s => DonutChart.Slice.mk s.value s.label s.color
  let theme := Theme.dark
  let dims := DonutChart.defaultDimensions
  let spec := DonutChart.donutChartSpec donutSlices theme dims
  let layout := mockLayout 600 600

  let start ← Chronos.MonotonicTime.now
  for _ in [:5] do
    let _ ← strictEval (spec.collect layout)
  let elapsed ← start.elapsed

  IO.println s!"  [DonutChart 1000 slices x5: {elapsed}]"

/-! ## FunnelChart Tests -/

test "perf: FunnelChart 50 stages" := do
  let stages := generateFunnelStages 50
  let theme := Theme.dark
  let dims := FunnelChart.defaultDimensions
  let data : FunnelChart.Data := { stages }
  let spec := FunnelChart.funnelChartSpec data theme dims
  let layout := mockLayout 400 800

  let start ← Chronos.MonotonicTime.now
  for _ in [:20] do
    let _ ← strictEval (spec.collect layout)
  let elapsed ← start.elapsed

  IO.println s!"  [FunnelChart 50 stages x20: {elapsed}]"

test "perf: FunnelChart 500 stages" := do
  let stages := generateFunnelStages 500
  let theme := Theme.dark
  let dims := FunnelChart.defaultDimensions
  let data : FunnelChart.Data := { stages }
  let spec := FunnelChart.funnelChartSpec data theme dims
  let layout := mockLayout 600 2000

  let start ← Chronos.MonotonicTime.now
  for _ in [:5] do
    let _ ← strictEval (spec.collect layout)
  let elapsed ← start.elapsed

  IO.println s!"  [FunnelChart 500 stages x5: {elapsed}]"

/-! ## GaugeChart Tests -/

test "perf: GaugeChart 100 segments" := do
  let data := generateGaugeData 100
  let theme := Theme.dark
  let dims := GaugeChart.defaultDimensions
  let spec := GaugeChart.gaugeChartSpec data theme (dims := dims)
  let layout := mockLayout 300 200

  let start ← Chronos.MonotonicTime.now
  for _ in [:20] do
    let _ ← strictEval (spec.collect layout)
  let elapsed ← start.elapsed

  IO.println s!"  [GaugeChart 100 segments x20: {elapsed}]"

test "perf: GaugeChart 1000 segments" := do
  let data := generateGaugeData 1000
  let theme := Theme.dark
  let dims := GaugeChart.defaultDimensions
  let spec := GaugeChart.gaugeChartSpec data theme (dims := dims)
  let layout := mockLayout 400 300

  let start ← Chronos.MonotonicTime.now
  for _ in [:5] do
    let _ ← strictEval (spec.collect layout)
  let elapsed ← start.elapsed

  IO.println s!"  [GaugeChart 1000 segments x5: {elapsed}]"

/-! ## GroupedBarChart Tests -/

test "perf: GroupedBarChart 100 categories x 10 series" := do
  let data := generateGroupedBarData 100 10
  let theme := Theme.dark
  let dims := GroupedBarChart.defaultDimensions
  let spec := GroupedBarChart.groupedBarChartSpec data theme dims
  let layout := mockLayout 2000 400

  let start ← Chronos.MonotonicTime.now
  for _ in [:10] do
    let _ ← strictEval (spec.collect layout)
  let elapsed ← start.elapsed

  IO.println s!"  [GroupedBarChart 100x10 x10: {elapsed}]"

test "perf: GroupedBarChart 500 categories x 20 series" := do
  let data := generateGroupedBarData 500 20
  let theme := Theme.dark
  let dims := GroupedBarChart.defaultDimensions
  let spec := GroupedBarChart.groupedBarChartSpec data theme dims
  let layout := mockLayout 8000 600

  let start ← Chronos.MonotonicTime.now
  for _ in [:3] do
    let _ ← strictEval (spec.collect layout)
  let elapsed ← start.elapsed

  IO.println s!"  [GroupedBarChart 500x20 x3: {elapsed}]"

/-! ## StackedBarChart Tests -/

test "perf: StackedBarChart 100 categories x 10 series" := do
  let data := generateGroupedBarData 100 10
  -- Convert to StackedBarChart.Data
  let stackedData : StackedBarChart.Data := {
    categories := data.categories
    series := data.series.map fun s => StackedBarChart.Series.mk s.name s.values s.color
  }
  let theme := Theme.dark
  let dims := StackedBarChart.defaultDimensions
  let spec := StackedBarChart.stackedBarChartSpec stackedData theme dims
  let layout := mockLayout 2000 400

  let start ← Chronos.MonotonicTime.now
  for _ in [:10] do
    let _ ← strictEval (spec.collect layout)
  let elapsed ← start.elapsed

  IO.println s!"  [StackedBarChart 100x10 x10: {elapsed}]"

test "perf: StackedBarChart 500 categories x 20 series" := do
  let data := generateGroupedBarData 500 20
  let stackedData : StackedBarChart.Data := {
    categories := data.categories
    series := data.series.map fun s => StackedBarChart.Series.mk s.name s.values s.color
  }
  let theme := Theme.dark
  let dims := StackedBarChart.defaultDimensions
  let spec := StackedBarChart.stackedBarChartSpec stackedData theme dims
  let layout := mockLayout 8000 600

  let start ← Chronos.MonotonicTime.now
  for _ in [:3] do
    let _ ← strictEval (spec.collect layout)
  let elapsed ← start.elapsed

  IO.println s!"  [StackedBarChart 500x20 x3: {elapsed}]"

/-! ## HorizontalBarChart Tests -/

test "perf: HorizontalBarChart 10000 bars" := do
  let data := generateValues 10000
  let labels : Array String := #[]
  let theme := Theme.dark
  let dims := HorizontalBarChart.defaultDimensions
  let spec := HorizontalBarChart.horizontalBarChartSpec data labels .primary theme dims
  let layout := mockLayout 600 20000

  let start ← Chronos.MonotonicTime.now
  for _ in [:10] do
    let _ ← strictEval (spec.collect layout)
  let elapsed ← start.elapsed

  IO.println s!"  [HorizontalBarChart 10000 bars x10: {elapsed}]"

test "perf: HorizontalBarChart 50000 bars" := do
  let data := generateValues 50000
  let labels : Array String := #[]
  let theme := Theme.dark
  let dims := HorizontalBarChart.defaultDimensions
  let spec := HorizontalBarChart.horizontalBarChartSpec data labels .primary theme dims
  let layout := mockLayout 800 100000

  let start ← Chronos.MonotonicTime.now
  for _ in [:3] do
    let _ ← strictEval (spec.collect layout)
  let elapsed ← start.elapsed

  IO.println s!"  [HorizontalBarChart 50000 bars x3: {elapsed}]"

/-! ## RadarChart Tests -/

test "perf: RadarChart 20 axes x 10 series" := do
  let data := generateRadarData 20 10
  let theme := Theme.dark
  let dims := RadarChart.defaultDimensions
  let spec := RadarChart.radarChartSpec data theme dims
  let layout := mockLayout 400 400

  let start ← Chronos.MonotonicTime.now
  for _ in [:20] do
    let _ ← strictEval (spec.collect layout)
  let elapsed ← start.elapsed

  IO.println s!"  [RadarChart 20x10 x20: {elapsed}]"

test "perf: RadarChart 50 axes x 20 series" := do
  let data := generateRadarData 50 20
  let theme := Theme.dark
  let dims := RadarChart.defaultDimensions
  let spec := RadarChart.radarChartSpec data theme dims
  let layout := mockLayout 600 600

  let start ← Chronos.MonotonicTime.now
  for _ in [:10] do
    let _ ← strictEval (spec.collect layout)
  let elapsed ← start.elapsed

  IO.println s!"  [RadarChart 50x20 x10: {elapsed}]"

/-! ## WaterfallChart Tests -/

test "perf: WaterfallChart 500 bars" := do
  let data := generateWaterfallData 500
  let theme := Theme.dark
  let dims := WaterfallChart.defaultDimensions
  let spec := WaterfallChart.waterfallChartSpec data theme (dims := dims)
  let layout := mockLayout 2000 400

  let start ← Chronos.MonotonicTime.now
  for _ in [:10] do
    let _ ← strictEval (spec.collect layout)
  let elapsed ← start.elapsed

  IO.println s!"  [WaterfallChart 500 bars x10: {elapsed}]"

test "perf: WaterfallChart 2000 bars" := do
  let data := generateWaterfallData 2000
  let theme := Theme.dark
  let dims := WaterfallChart.defaultDimensions
  let spec := WaterfallChart.waterfallChartSpec data theme (dims := dims)
  let layout := mockLayout 6000 600

  let start ← Chronos.MonotonicTime.now
  for _ in [:3] do
    let _ ← strictEval (spec.collect layout)
  let elapsed ← start.elapsed

  IO.println s!"  [WaterfallChart 2000 bars x3: {elapsed}]"

end Afferent.Tests.ChartPerformanceTests

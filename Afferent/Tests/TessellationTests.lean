/-
  Afferent Tessellation Tests
  Unit tests for geometry generation without GPU/Metal.
-/
import Afferent.Tests.Framework
import Afferent.Core.Types
import Afferent.Core.Path
import Afferent.Core.Paint
import Afferent.Render.Tessellation

namespace Afferent.Tests.TessellationTests

open Afferent
open Afferent.Tests
open Afferent.Tessellation

/-! ## Basic Tessellation Tests -/

def test_tessellateRect_vertexCount : TestCase := {
  name := "tessellateRect produces 4 vertices (24 floats)"
  run := do
    let rect := Rect.mk' 0 0 100 100
    let result := tessellateRect rect Color.red
    -- 4 vertices × 6 floats each (x, y, r, g, b, a) = 24
    ensure (result.vertices.size == 24) s!"Expected 24 floats, got {result.vertices.size}"
}

def test_tessellateRect_indexCount : TestCase := {
  name := "tessellateRect produces 6 indices (2 triangles)"
  run := do
    let rect := Rect.mk' 0 0 100 100
    let result := tessellateRect rect Color.red
    -- 2 triangles × 3 indices = 6
    ensure (result.indices.size == 6) s!"Expected 6 indices, got {result.indices.size}"
}

def test_tessellateRect_vertexPositions : TestCase := {
  name := "tessellateRect has correct corner positions"
  run := do
    let rect := Rect.mk' 10 20 100 50
    let result := tessellateRect rect Color.red
    -- Vertex 0 (top-left): x=10, y=20
    shouldBeNear result.vertices[0]! 10.0
    shouldBeNear result.vertices[1]! 20.0
    -- Vertex 1 (top-right): x=110, y=20
    shouldBeNear result.vertices[6]! 110.0
    shouldBeNear result.vertices[7]! 20.0
    -- Vertex 2 (bottom-right): x=110, y=70
    shouldBeNear result.vertices[12]! 110.0
    shouldBeNear result.vertices[13]! 70.0
    -- Vertex 3 (bottom-left): x=10, y=70
    shouldBeNear result.vertices[18]! 10.0
    shouldBeNear result.vertices[19]! 70.0
}

def test_tessellateRect_colors : TestCase := {
  name := "tessellateRect assigns correct color to all vertices"
  run := do
    let rect := Rect.mk' 0 0 100 100
    let color := Color.rgba 0.5 0.25 0.75 1.0
    let result := tessellateRect rect color
    -- Check color at each of the 4 vertices
    for i in [0, 1, 2, 3] do
      let base := i * 6
      shouldBeNear result.vertices[base + 2]! 0.5   -- r
      shouldBeNear result.vertices[base + 3]! 0.25  -- g
      shouldBeNear result.vertices[base + 4]! 0.75  -- b
      shouldBeNear result.vertices[base + 5]! 1.0   -- a
}

/-! ## Triangle Fan Tests -/

def test_triangulateConvexFan_triangle : TestCase := {
  name := "triangulateConvexFan with 3 vertices produces 1 triangle"
  run := do
    let indices := triangulateConvexFan 3
    ensure (indices.size == 3) s!"Expected 3 indices, got {indices.size}"
    ensure (indices[0]! == 0) "First index should be 0"
    ensure (indices[1]! == 1) "Second index should be 1"
    ensure (indices[2]! == 2) "Third index should be 2"
}

def test_triangulateConvexFan_quad : TestCase := {
  name := "triangulateConvexFan with 4 vertices produces 2 triangles"
  run := do
    let indices := triangulateConvexFan 4
    ensure (indices.size == 6) s!"Expected 6 indices, got {indices.size}"
}

def test_triangulateConvexFan_pentagon : TestCase := {
  name := "triangulateConvexFan with 5 vertices produces 3 triangles"
  run := do
    let indices := triangulateConvexFan 5
    ensure (indices.size == 9) s!"Expected 9 indices, got {indices.size}"
}

def test_triangulateConvexFan_degenerate : TestCase := {
  name := "triangulateConvexFan with < 3 vertices produces empty"
  run := do
    let indices0 := triangulateConvexFan 0
    let indices1 := triangulateConvexFan 1
    let indices2 := triangulateConvexFan 2
    ensure (indices0.size == 0) "0 vertices should produce 0 indices"
    ensure (indices1.size == 0) "1 vertex should produce 0 indices"
    ensure (indices2.size == 0) "2 vertices should produce 0 indices"
}

/-! ## Path to Polygon Tests -/

def test_pathToPolygon_rectangle : TestCase := {
  name := "pathToPolygon extracts 4 points from rectangle path"
  run := do
    let path := Path.rectangle (Rect.mk' 0 0 100 100)
    let points := pathToPolygon path
    -- moveTo + 3 lineTo + closePath = 4 points
    ensure (points.size == 4) s!"Expected 4 points, got {points.size}"
}

def test_pathToPolygonWithClosed_rectangle : TestCase := {
  name := "pathToPolygonWithClosed detects rectangle as closed"
  run := do
    let path := Path.rectangle (Rect.mk' 0 0 100 100)
    let (_, isClosed) := pathToPolygonWithClosed path
    ensure isClosed "Rectangle path should be detected as closed"
}

def test_pathToPolygonWithClosed_rectCommand : TestCase := {
  name := "pathToPolygonWithClosed detects rect command as closed"
  run := do
    let path := Path.empty.rect (Rect.mk' 0 0 100 100)
    let (_, isClosed) := pathToPolygonWithClosed path
    ensure isClosed "Rect command should be detected as closed"
}

def test_pathToPolygonWithClosed_openPath : TestCase := {
  name := "pathToPolygonWithClosed detects open path"
  run := do
    let path := Path.empty
      |>.moveTo ⟨0, 0⟩
      |>.lineTo ⟨100, 0⟩
      |>.lineTo ⟨100, 100⟩
    let (_, isClosed) := pathToPolygonWithClosed path
    ensure (!isClosed) "Path without closePath should be detected as open"
}

def test_pathToPolygon_polygon : TestCase := {
  name := "pathToPolygon extracts correct points from hexagon"
  run := do
    let path := Path.polygon ⟨100, 100⟩ 50 6
    let points := pathToPolygon path
    ensure (points.size == 6) s!"Expected 6 points for hexagon, got {points.size}"
}

/-! ## Bezier Flattening Tests -/

def test_flattenCubicBezier_straightLine : TestCase := {
  name := "flattenCubicBezier on straight line produces few points"
  run := do
    -- A "bezier" that is actually a straight line
    let p0 := Point.mk' 0 0
    let p1 := Point.mk' 33 0
    let p2 := Point.mk' 66 0
    let p3 := Point.mk' 100 0
    let result := flattenCubicBezier p0 p1 p2 p3 0.5
    -- Should produce just the endpoint for a straight line
    ensure (result.size <= 2) s!"Straight bezier should produce few points, got {result.size}"
}

def test_flattenCubicBezier_curveProducesPoints : TestCase := {
  name := "flattenCubicBezier on curve produces multiple points"
  run := do
    -- An actual curve
    let p0 := Point.mk' 0 0
    let p1 := Point.mk' 0 100
    let p2 := Point.mk' 100 100
    let p3 := Point.mk' 100 0
    let result := flattenCubicBezier p0 p1 p2 p3 1.0
    -- Should produce multiple points for a significant curve
    ensure (result.size >= 2) s!"Curved bezier should produce multiple points, got {result.size}"
}

def test_flattenCubicBezier_endpointCorrect : TestCase := {
  name := "flattenCubicBezier ends at p3"
  run := do
    let p0 := Point.mk' 0 0
    let p1 := Point.mk' 50 100
    let p2 := Point.mk' 100 100
    let p3 := Point.mk' 150 50
    let result := flattenCubicBezier p0 p1 p2 p3 0.5
    ensure (result.size > 0) "Should produce at least one point"
    let lastPt := result[result.size - 1]!
    shouldBeNear lastPt.x 150.0
    shouldBeNear lastPt.y 50.0
}

/-! ## Gradient Sampling Tests -/

def test_interpolateGradientStops_start : TestCase := {
  name := "interpolateGradientStops at t=0 returns first color"
  run := do
    let stops := #[
      { position := 0.0, color := Color.red : GradientStop },
      { position := 1.0, color := Color.blue }
    ]
    let result := interpolateGradientStops stops 0.0
    shouldBeNear result.r 1.0
    shouldBeNear result.g 0.0
    shouldBeNear result.b 0.0
}

def test_interpolateGradientStops_end : TestCase := {
  name := "interpolateGradientStops at t=1 returns last color"
  run := do
    let stops := #[
      { position := 0.0, color := Color.red : GradientStop },
      { position := 1.0, color := Color.blue }
    ]
    let result := interpolateGradientStops stops 1.0
    shouldBeNear result.r 0.0
    shouldBeNear result.g 0.0
    shouldBeNear result.b 1.0
}

def test_interpolateGradientStops_middle : TestCase := {
  name := "interpolateGradientStops at t=0.5 interpolates colors"
  run := do
    let stops := #[
      { position := 0.0, color := Color.red : GradientStop },
      { position := 1.0, color := Color.blue }
    ]
    let result := interpolateGradientStops stops 0.5
    shouldBeNear result.r 0.5
    shouldBeNear result.g 0.0
    shouldBeNear result.b 0.5
}

def test_interpolateGradientStops_multiStop : TestCase := {
  name := "interpolateGradientStops with 3 stops"
  run := do
    let stops := #[
      { position := 0.0, color := Color.red : GradientStop },
      { position := 0.5, color := Color.green },
      { position := 1.0, color := Color.blue }
    ]
    -- At 0.25: between red and green
    let result := interpolateGradientStops stops 0.25
    shouldBeNear result.r 0.5  -- halfway from red(1) to green(0)
    shouldBeNear result.g 0.5  -- halfway from red(0) to green(1)
}

def test_sampleLinearGradient_horizontal : TestCase := {
  name := "sampleLinearGradient samples correctly along horizontal"
  run := do
    let start := Point.mk' 0 50
    let finish := Point.mk' 100 50
    let stops := #[
      { position := 0.0, color := Color.black : GradientStop },
      { position := 1.0, color := Color.white }
    ]
    -- Sample at middle of gradient
    let midColor := sampleLinearGradient start finish stops ⟨50, 50⟩
    shouldBeNear midColor.r 0.5
    shouldBeNear midColor.g 0.5
    shouldBeNear midColor.b 0.5
}

def test_sampleRadialGradient_center : TestCase := {
  name := "sampleRadialGradient at center returns first stop"
  run := do
    let center := Point.mk' 100 100
    let radius := 50.0
    let stops := #[
      { position := 0.0, color := Color.red : GradientStop },
      { position := 1.0, color := Color.blue }
    ]
    let result := sampleRadialGradient center radius stops center
    shouldBeNear result.r 1.0
    shouldBeNear result.b 0.0
}

def test_sampleRadialGradient_edge : TestCase := {
  name := "sampleRadialGradient at edge returns last stop"
  run := do
    let center := Point.mk' 100 100
    let radius := 50.0
    let stops := #[
      { position := 0.0, color := Color.red : GradientStop },
      { position := 1.0, color := Color.blue }
    ]
    -- Point at distance = radius
    let edgePoint := Point.mk' 150 100
    let result := sampleRadialGradient center radius stops edgePoint
    shouldBeNear result.r 0.0
    shouldBeNear result.b 1.0
}

/-! ## NDC Conversion Tests -/

def test_pixelToNDC_topLeft : TestCase := {
  name := "pixelToNDC converts top-left (0,0) to (-1, 1)"
  run := do
    let result := pixelToNDC 0 0 100 100
    shouldBeNear result.x (-1.0)
    shouldBeNear result.y 1.0
}

def test_pixelToNDC_bottomRight : TestCase := {
  name := "pixelToNDC converts bottom-right to (1, -1)"
  run := do
    let result := pixelToNDC 100 100 100 100
    shouldBeNear result.x 1.0
    shouldBeNear result.y (-1.0)
}

def test_pixelToNDC_center : TestCase := {
  name := "pixelToNDC converts center to (0, 0)"
  run := do
    let result := pixelToNDC 50 50 100 100
    shouldBeNear result.x 0.0
    shouldBeNear result.y 0.0
}

/-! ## Stroke Tessellation Tests -/

def test_expandPolylineToStroke_basic : TestCase := {
  name := "expandPolylineToStroke produces left and right edges"
  run := do
    let points := #[Point.mk' 0 0, Point.mk' 100 0, Point.mk' 100 100]
    let (left, right) := expandPolylineToStroke points 5.0 .butt .miter 10.0
    ensure (left.size > 0) "Should produce left edge points"
    ensure (right.size > 0) "Should produce right edge points"
    ensure (left.size == right.size) s!"Left ({left.size}) and right ({right.size}) should have same size"
}

def test_expandPolylineToStroke_edgeCount : TestCase := {
  name := "expandPolylineToStroke produces one edge pair per input point"
  run := do
    let points := #[Point.mk' 0 0, Point.mk' 100 0, Point.mk' 200 0]
    let (left, right) := expandPolylineToStroke points 5.0 .butt .miter 10.0
    -- 3 input points should produce 3 left and 3 right points
    ensure (left.size == 3) s!"Expected 3 left points, got {left.size}"
    ensure (right.size == 3) s!"Expected 3 right points, got {right.size}"
}

def test_strokeEdgesToTriangles_basic : TestCase := {
  name := "strokeEdgesToTriangles produces valid triangle mesh"
  run := do
    let left := #[Point.mk' 0 5, Point.mk' 100 5, Point.mk' 200 5]
    let right := #[Point.mk' 0 (-5), Point.mk' 100 (-5), Point.mk' 200 (-5)]
    let result := strokeEdgesToTriangles left right Color.red
    -- 3 pairs → 2 quads → 4 triangles → 12 indices
    ensure (result.indices.size == 12) s!"Expected 12 indices, got {result.indices.size}"
    -- 6 vertices (3 left + 3 right interleaved)
    ensure (result.vertices.size == 36) s!"Expected 36 floats (6 vertices), got {result.vertices.size}"
}

def test_tessellateStroke_closedPath : TestCase := {
  name := "tessellateStroke handles closed paths"
  run := do
    let path := Path.rectangle (Rect.mk' 0 0 100 100)
    let style := { StrokeStyle.default with lineWidth := 4.0 }
    let result := tessellateStroke path style
    ensure (result.vertices.size > 0) "Should produce vertices for closed path"
    ensure (result.indices.size > 0) "Should produce indices for closed path"
}

/-! ## Convex Path Tessellation Tests -/

def test_tessellateConvexPath_triangle : TestCase := {
  name := "tessellateConvexPath produces correct output for triangle"
  run := do
    let path := Path.triangle ⟨50, 0⟩ ⟨0, 100⟩ ⟨100, 100⟩
    let result := tessellateConvexPath path Color.green
    -- Triangle: 3 vertices × 6 floats = 18
    ensure (result.vertices.size == 18) s!"Expected 18 floats, got {result.vertices.size}"
    -- 1 triangle = 3 indices
    ensure (result.indices.size == 3) s!"Expected 3 indices, got {result.indices.size}"
}

def test_tessellateConvexPath_hexagon : TestCase := {
  name := "tessellateConvexPath produces correct output for hexagon"
  run := do
    let path := Path.hexagon ⟨100, 100⟩ 50
    let result := tessellateConvexPath path Color.blue
    -- Hexagon: 6 vertices × 6 floats = 36
    ensure (result.vertices.size == 36) s!"Expected 36 floats, got {result.vertices.size}"
    -- 4 triangles (fan from first vertex) = 12 indices
    ensure (result.indices.size == 12) s!"Expected 12 indices, got {result.indices.size}"
}

/-! ## Test Runner -/

def allTests : List TestCase := [
  -- Basic tessellation
  test_tessellateRect_vertexCount,
  test_tessellateRect_indexCount,
  test_tessellateRect_vertexPositions,
  test_tessellateRect_colors,
  -- Triangle fan
  test_triangulateConvexFan_triangle,
  test_triangulateConvexFan_quad,
  test_triangulateConvexFan_pentagon,
  test_triangulateConvexFan_degenerate,
  -- Path to polygon
  test_pathToPolygon_rectangle,
  test_pathToPolygonWithClosed_rectangle,
  test_pathToPolygonWithClosed_rectCommand,
  test_pathToPolygonWithClosed_openPath,
  test_pathToPolygon_polygon,
  -- Bezier flattening
  test_flattenCubicBezier_straightLine,
  test_flattenCubicBezier_curveProducesPoints,
  test_flattenCubicBezier_endpointCorrect,
  -- Gradient sampling
  test_interpolateGradientStops_start,
  test_interpolateGradientStops_end,
  test_interpolateGradientStops_middle,
  test_interpolateGradientStops_multiStop,
  test_sampleLinearGradient_horizontal,
  test_sampleRadialGradient_center,
  test_sampleRadialGradient_edge,
  -- NDC conversion
  test_pixelToNDC_topLeft,
  test_pixelToNDC_bottomRight,
  test_pixelToNDC_center,
  -- Stroke tessellation
  test_expandPolylineToStroke_basic,
  test_expandPolylineToStroke_edgeCount,
  test_strokeEdgesToTriangles_basic,
  test_tessellateStroke_closedPath,
  -- Convex path tessellation
  test_tessellateConvexPath_triangle,
  test_tessellateConvexPath_hexagon
]

def runAllTests : IO UInt32 :=
  runTests "Tessellation Tests" allTests

end Afferent.Tests.TessellationTests

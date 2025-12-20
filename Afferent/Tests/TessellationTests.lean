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

open Crucible
open Afferent
open Afferent.Tests
open Afferent.Tessellation

testSuite "Tessellation Tests"

/-! ## Basic Tessellation Tests -/

test "tessellateRect produces 4 vertices (24 floats)" := do
  let rect := Rect.mk' 0 0 100 100
  let result := tessellateRect rect Color.red
  -- 4 vertices × 6 floats each (x, y, r, g, b, a) = 24
  ensure (result.vertices.size == 24) s!"Expected 24 floats, got {result.vertices.size}"

test "tessellateRect produces 6 indices (2 triangles)" := do
  let rect := Rect.mk' 0 0 100 100
  let result := tessellateRect rect Color.red
  -- 2 triangles × 3 indices = 6
  ensure (result.indices.size == 6) s!"Expected 6 indices, got {result.indices.size}"

test "tessellateRect has correct corner positions" := do
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

test "tessellateRect assigns correct color to all vertices" := do
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

/-! ## Triangle Fan Tests -/

test "triangulateConvexFan with 3 vertices produces 1 triangle" := do
  let indices := triangulateConvexFan 3
  ensure (indices.size == 3) s!"Expected 3 indices, got {indices.size}"
  ensure (indices[0]! == 0) "First index should be 0"
  ensure (indices[1]! == 1) "Second index should be 1"
  ensure (indices[2]! == 2) "Third index should be 2"

test "triangulateConvexFan with 4 vertices produces 2 triangles" := do
  let indices := triangulateConvexFan 4
  ensure (indices.size == 6) s!"Expected 6 indices, got {indices.size}"

test "triangulateConvexFan with 5 vertices produces 3 triangles" := do
  let indices := triangulateConvexFan 5
  ensure (indices.size == 9) s!"Expected 9 indices, got {indices.size}"

test "triangulateConvexFan with < 3 vertices produces empty" := do
  let indices0 := triangulateConvexFan 0
  let indices1 := triangulateConvexFan 1
  let indices2 := triangulateConvexFan 2
  ensure (indices0.size == 0) "0 vertices should produce 0 indices"
  ensure (indices1.size == 0) "1 vertex should produce 0 indices"
  ensure (indices2.size == 0) "2 vertices should produce 0 indices"

/-! ## Path to Polygon Tests -/

test "pathToPolygon extracts 4 points from rectangle path" := do
  let path := Path.rectangle (Rect.mk' 0 0 100 100)
  let points := pathToPolygon path
  -- moveTo + 3 lineTo + closePath = 4 points
  ensure (points.size == 4) s!"Expected 4 points, got {points.size}"

test "pathToPolygonWithClosed detects rectangle as closed" := do
  let path := Path.rectangle (Rect.mk' 0 0 100 100)
  let (_, isClosed) := pathToPolygonWithClosed path
  ensure isClosed "Rectangle path should be detected as closed"

test "pathToPolygonWithClosed detects rect command as closed" := do
  let path := Path.empty.rect (Rect.mk' 0 0 100 100)
  let (_, isClosed) := pathToPolygonWithClosed path
  ensure isClosed "Rect command should be detected as closed"

test "pathToPolygonWithClosed detects open path" := do
  let path := Path.empty
    |>.moveTo ⟨0, 0⟩
    |>.lineTo ⟨100, 0⟩
    |>.lineTo ⟨100, 100⟩
  let (_, isClosed) := pathToPolygonWithClosed path
  ensure (!isClosed) "Path without closePath should be detected as open"

test "pathToPolygon extracts correct points from hexagon" := do
  let path := Path.polygon ⟨100, 100⟩ 50 6
  let points := pathToPolygon path
  ensure (points.size == 6) s!"Expected 6 points for hexagon, got {points.size}"

/-! ## Bezier Flattening Tests -/

test "flattenCubicBezier on straight line produces few points" := do
  -- A "bezier" that is actually a straight line
  let p0 := Point.mk' 0 0
  let p1 := Point.mk' 33 0
  let p2 := Point.mk' 66 0
  let p3 := Point.mk' 100 0
  let result := flattenCubicBezier p0 p1 p2 p3 0.5
  -- Should produce just the endpoint for a straight line
  ensure (result.size <= 2) s!"Straight bezier should produce few points, got {result.size}"

test "flattenCubicBezier on curve produces multiple points" := do
  -- An actual curve
  let p0 := Point.mk' 0 0
  let p1 := Point.mk' 0 100
  let p2 := Point.mk' 100 100
  let p3 := Point.mk' 100 0
  let result := flattenCubicBezier p0 p1 p2 p3 1.0
  -- Should produce multiple points for a significant curve
  ensure (result.size >= 2) s!"Curved bezier should produce multiple points, got {result.size}"

test "flattenCubicBezier ends at p3" := do
  let p0 := Point.mk' 0 0
  let p1 := Point.mk' 50 100
  let p2 := Point.mk' 100 100
  let p3 := Point.mk' 150 50
  let result := flattenCubicBezier p0 p1 p2 p3 0.5
  ensure (result.size > 0) "Should produce at least one point"
  let lastPt := result[result.size - 1]!
  shouldBeNear lastPt.x 150.0
  shouldBeNear lastPt.y 50.0

/-! ## Gradient Sampling Tests -/

test "interpolateGradientStops at t=0 returns first color" := do
  let stops := #[
    { position := 0.0, color := Color.red : GradientStop },
    { position := 1.0, color := Color.blue }
  ]
  let result := interpolateGradientStops stops 0.0
  shouldBeNear result.r 1.0
  shouldBeNear result.g 0.0
  shouldBeNear result.b 0.0

test "interpolateGradientStops at t=1 returns last color" := do
  let stops := #[
    { position := 0.0, color := Color.red : GradientStop },
    { position := 1.0, color := Color.blue }
  ]
  let result := interpolateGradientStops stops 1.0
  shouldBeNear result.r 0.0
  shouldBeNear result.g 0.0
  shouldBeNear result.b 1.0

test "interpolateGradientStops at t=0.5 interpolates colors" := do
  let stops := #[
    { position := 0.0, color := Color.red : GradientStop },
    { position := 1.0, color := Color.blue }
  ]
  let result := interpolateGradientStops stops 0.5
  shouldBeNear result.r 0.5
  shouldBeNear result.g 0.0
  shouldBeNear result.b 0.5

test "interpolateGradientStops with 3 stops" := do
  let stops := #[
    { position := 0.0, color := Color.red : GradientStop },
    { position := 0.5, color := Color.green },
    { position := 1.0, color := Color.blue }
  ]
  -- At 0.25: between red and green
  let result := interpolateGradientStops stops 0.25
  shouldBeNear result.r 0.5  -- halfway from red(1) to green(0)
  shouldBeNear result.g 0.5  -- halfway from red(0) to green(1)

test "sampleLinearGradient samples correctly along horizontal" := do
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

test "sampleRadialGradient at center returns first stop" := do
  let center := Point.mk' 100 100
  let radius := 50.0
  let stops := #[
    { position := 0.0, color := Color.red : GradientStop },
    { position := 1.0, color := Color.blue }
  ]
  let result := sampleRadialGradient center radius stops center
  shouldBeNear result.r 1.0
  shouldBeNear result.b 0.0

test "sampleRadialGradient at edge returns last stop" := do
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

/-! ## NDC Conversion Tests -/

test "pixelToNDC converts top-left (0,0) to (-1, 1)" := do
  let result := pixelToNDC 0 0 100 100
  shouldBeNear result.x (-1.0)
  shouldBeNear result.y 1.0

test "pixelToNDC converts bottom-right to (1, -1)" := do
  let result := pixelToNDC 100 100 100 100
  shouldBeNear result.x 1.0
  shouldBeNear result.y (-1.0)

test "pixelToNDC converts center to (0, 0)" := do
  let result := pixelToNDC 50 50 100 100
  shouldBeNear result.x 0.0
  shouldBeNear result.y 0.0

/-! ## Stroke Tessellation Tests -/

test "expandPolylineToStroke produces left and right edges" := do
  let points := #[Point.mk' 0 0, Point.mk' 100 0, Point.mk' 100 100]
  let (left, right) := expandPolylineToStroke points 5.0 .butt .miter 10.0
  ensure (left.size > 0) "Should produce left edge points"
  ensure (right.size > 0) "Should produce right edge points"
  ensure (left.size == right.size) s!"Left ({left.size}) and right ({right.size}) should have same size"

test "expandPolylineToStroke produces one edge pair per input point" := do
  let points := #[Point.mk' 0 0, Point.mk' 100 0, Point.mk' 200 0]
  let (left, right) := expandPolylineToStroke points 5.0 .butt .miter 10.0
  -- 3 input points should produce 3 left and 3 right points
  ensure (left.size == 3) s!"Expected 3 left points, got {left.size}"
  ensure (right.size == 3) s!"Expected 3 right points, got {right.size}"

test "strokeEdgesToTriangles produces valid triangle mesh" := do
  let left := #[Point.mk' 0 5, Point.mk' 100 5, Point.mk' 200 5]
  let right := #[Point.mk' 0 (-5), Point.mk' 100 (-5), Point.mk' 200 (-5)]
  let result := strokeEdgesToTriangles left right Color.red
  -- 3 pairs → 2 quads → 4 triangles → 12 indices
  ensure (result.indices.size == 12) s!"Expected 12 indices, got {result.indices.size}"
  -- 6 vertices (3 left + 3 right interleaved)
  ensure (result.vertices.size == 36) s!"Expected 36 floats (6 vertices), got {result.vertices.size}"

test "tessellateStroke handles closed paths" := do
  let path := Path.rectangle (Rect.mk' 0 0 100 100)
  let style := { StrokeStyle.default with lineWidth := 4.0 }
  let result := tessellateStroke path style
  ensure (result.vertices.size > 0) "Should produce vertices for closed path"
  ensure (result.indices.size > 0) "Should produce indices for closed path"

/-! ## Convex Path Tessellation Tests -/

test "tessellateConvexPath produces correct output for triangle" := do
  let path := Path.triangle ⟨50, 0⟩ ⟨0, 100⟩ ⟨100, 100⟩
  let result := tessellateConvexPath path Color.green
  -- Triangle: 3 vertices × 6 floats = 18
  ensure (result.vertices.size == 18) s!"Expected 18 floats, got {result.vertices.size}"
  -- 1 triangle = 3 indices
  ensure (result.indices.size == 3) s!"Expected 3 indices, got {result.indices.size}"

test "tessellateConvexPath produces correct output for hexagon" := do
  let path := Path.hexagon ⟨100, 100⟩ 50
  let result := tessellateConvexPath path Color.blue
  -- Hexagon: 6 vertices × 6 floats = 36
  ensure (result.vertices.size == 36) s!"Expected 36 floats, got {result.vertices.size}"
  -- 4 triangles (fan from first vertex) = 12 indices
  ensure (result.indices.size == 12) s!"Expected 12 indices, got {result.indices.size}"

#generate_tests

end Afferent.Tests.TessellationTests

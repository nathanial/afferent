/-
  Afferent Tessellation Tests
  Unit tests for geometry generation without GPU/Metal.
-/
import Afferent.Tests.Framework
import Afferent.Core.Types
import Afferent.Core.Path
import Afferent.Core.Paint
import Afferent.Render.Tessellation
import Linalg.Core


namespace Afferent.Tests.TessellationTests

open Crucible
open Afferent
open Afferent.Tests
open Afferent.Tessellation
open Linalg

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

test "tessellateConvexPathFillNDC slices multi-stop linear gradients" := do
  let rect := Rect.mk' 0 0 10 100
  let path := Path.rectangle rect
  let start := Point.mk' 0 0
  let finish := Point.mk' 0 100
  let stops : Array GradientStop := #[
    { position := 0.0, color := Color.red },
    { position := 0.5, color := Color.green },
    { position := 1.0, color := Color.blue }
  ]
  let style := FillStyle.linearGradient start finish stops
  let result := tessellateConvexPathFillNDC path style 100 100
  -- Sliced gradients should produce more than 4 vertices / 2 triangles.
  ensure (result.vertices.size > 24) s!"Expected >24 floats, got {result.vertices.size}"
  ensure (result.indices.size > 6) s!"Expected >6 indices, got {result.indices.size}"

/-! ## Gradient Edge Case Tests -/

test "interpolateGradientStops with empty stops returns black" := do
  let stops : Array GradientStop := #[]
  let result := interpolateGradientStops stops 0.5
  shouldBeNear result.r 0.0
  shouldBeNear result.g 0.0
  shouldBeNear result.b 0.0

test "interpolateGradientStops with single stop returns that color" := do
  let stops := #[{ position := 0.5, color := Color.green : GradientStop }]
  let result := interpolateGradientStops stops 0.0
  shouldBeNear result.r 0.0
  shouldBeNear result.g 1.0
  shouldBeNear result.b 0.0
  -- Also check at t=1.0
  let result2 := interpolateGradientStops stops 1.0
  shouldBeNear result2.g 1.0

test "interpolateGradientStops clamps t below 0" := do
  let stops := #[
    { position := 0.0, color := Color.red : GradientStop },
    { position := 1.0, color := Color.blue }
  ]
  let result := interpolateGradientStops stops (-0.5)
  -- Should return first stop color
  shouldBeNear result.r 1.0
  shouldBeNear result.b 0.0

test "interpolateGradientStops clamps t above 1" := do
  let stops := #[
    { position := 0.0, color := Color.red : GradientStop },
    { position := 1.0, color := Color.blue }
  ]
  let result := interpolateGradientStops stops 1.5
  -- Should return last stop color
  shouldBeNear result.r 0.0
  shouldBeNear result.b 1.0

test "sampleLinearGradient outside gradient bounds clamps" := do
  let start := Point.mk' 0 0
  let finish := Point.mk' 100 0
  let stops := #[
    { position := 0.0, color := Color.white : GradientStop },
    { position := 1.0, color := Color.black }
  ]
  -- Sample before start (x = -50)
  let before := sampleLinearGradient start finish stops ⟨-50, 0⟩
  shouldBeNear before.r 1.0  -- White
  -- Sample after end (x = 150)
  let after := sampleLinearGradient start finish stops ⟨150, 0⟩
  shouldBeNear after.r 0.0  -- Black

test "sampleRadialGradient outside radius clamps to last stop" := do
  let center := Point.mk' 50 50
  let radius := 25.0
  let stops := #[
    { position := 0.0, color := Color.yellow : GradientStop },
    { position := 1.0, color := Color.purple }
  ]
  -- Point far outside radius
  let farPoint := Point.mk' 200 200
  let result := sampleRadialGradient center radius stops farPoint
  -- Should be clamped to last stop (purple)
  shouldBeNear result.r 0.5
  shouldBeNear result.g 0.0
  shouldBeNear result.b 0.5

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

/-! ## Stroke Segment Tests -/

test "tessellateStrokeSegments emits line segment for simple line" := do
  let path := Path.empty
    |>.moveTo ⟨0, 0⟩
    |>.lineTo ⟨100, 0⟩
  let result := tessellateStrokeSegments path StrokeStyle.default
  ensure (result.lineCount == 1) s!"Expected 1 line segment, got {result.lineCount}"
  ensure (result.lineSegments.size == strokeSegmentStride)
    s!"Expected {strokeSegmentStride} floats, got {result.lineSegments.size}"
  ensure (result.curveCount == 0) s!"Expected 0 curve segments, got {result.curveCount}"

test "tessellateStrokeSegments emits cubic segment for bezier path" := do
  let path := Path.empty
    |>.moveTo ⟨0, 0⟩
    |>.bezierCurveTo ⟨50, 100⟩ ⟨150, 100⟩ ⟨200, 0⟩
  let result := tessellateStrokeSegments path StrokeStyle.default
  ensure (result.curveCount == 1) s!"Expected 1 curve segment, got {result.curveCount}"
  ensure (result.curveSegments.size == strokeSegmentStride)
    s!"Expected {strokeSegmentStride} floats, got {result.curveSegments.size}"

test "tessellateStrokeSegments marks closed paths as adjacent" := do
  let path := Path.rectangle (Rect.mk' 0 0 100 100)
  let result := tessellateStrokeSegments path StrokeStyle.default
  ensure (result.lineCount == 4) s!"Expected 4 segments, got {result.lineCount}"
  let hasPrev := result.lineSegments[14]!
  let hasNext := result.lineSegments[15]!
  shouldBeNear hasPrev 1.0
  shouldBeNear hasNext 1.0

test "triangulatePolygon handles convex and concave polygons" := do
  let square := #[
    Point.mk' 0 0,
    Point.mk' 100 0,
    Point.mk' 100 100,
    Point.mk' 0 100
  ]
  let arrow := #[
    Point.mk' 50 0,
    Point.mk' 100 50,
    Point.mk' 75 50,
    Point.mk' 75 100,
    Point.mk' 25 100,
    Point.mk' 25 50,
    Point.mk' 0 50
  ]
  -- Both should produce valid triangulations
  let squareIndices := triangulatePolygon square
  let arrowIndices := triangulatePolygon arrow
  ensure (squareIndices.size == 6) s!"Square: expected 6 indices, got {squareIndices.size}"
  ensure (arrowIndices.size == 15) s!"Arrow: expected 15 indices, got {arrowIndices.size}"

test "tessellatePath handles holes via earcut" := do
  let outer := Rect.mk' 0 0 100 100
  let inner := Rect.mk' 25 25 50 50
  let path := Path.empty
    |>.rect outer
    |>.rect inner
  let result := tessellatePath path Color.red
  ensure (result.vertices.size == 8 * 6) s!"Expected 48 floats, got {result.vertices.size}"
  ensure (result.indices.size == 24) s!"Expected 24 indices, got {result.indices.size}"

/-! ## arcTo Geometry Tests -/

test "computeArcTo returns some for valid corner" := do
  let current := Point.mk' 0 50
  let p1 := Point.mk' 50 50  -- corner
  let p2 := Point.mk' 50 100 -- direction
  let radius := 10.0
  let result := computeArcTo current p1 p2 radius
  ensure result.isSome "computeArcTo should return some for valid corner"
  match result with
  | some (t1, beziers, t2) =>
    ensure (beziers.size > 0) "Should produce bezier segments"
    -- Tangent points should be near p1 (within radius distance)
    let d1 := Float.sqrt ((t1.x - p1.x) * (t1.x - p1.x) + (t1.y - p1.y) * (t1.y - p1.y))
    let d2 := Float.sqrt ((t2.x - p1.x) * (t2.x - p1.x) + (t2.y - p1.y) * (t2.y - p1.y))
    ensure (d1 <= radius + 1.0) s!"First tangent too far from corner: {d1}"
    ensure (d2 <= radius + 1.0) s!"Second tangent too far from corner: {d2}"
  | none => pure ()  -- Already handled by isSome check

test "computeArcTo returns none for zero radius" := do
  let current := Point.mk' 0 0
  let p1 := Point.mk' 50 50
  let p2 := Point.mk' 100 50
  let result := computeArcTo current p1 p2 0.0
  ensure result.isNone "computeArcTo should return none for zero radius"

test "computeArcTo returns none for negative radius" := do
  let current := Point.mk' 0 0
  let p1 := Point.mk' 50 50
  let p2 := Point.mk' 100 50
  let result := computeArcTo current p1 p2 (-10.0)
  ensure result.isNone "computeArcTo should return none for negative radius"

test "computeArcTo returns none for collinear points" := do
  let current := Point.mk' 0 0
  let p1 := Point.mk' 50 0   -- same line
  let p2 := Point.mk' 100 0  -- same line
  let result := computeArcTo current p1 p2 10.0
  ensure result.isNone "computeArcTo should return none for collinear points"

test "computeArcTo handles right angle corner" := do
  let current := Point.mk' 0 0
  let p1 := Point.mk' 100 0   -- corner at right
  let p2 := Point.mk' 100 100 -- going down
  let radius := 20.0
  let result := computeArcTo current p1 p2 radius
  ensure result.isSome "computeArcTo should handle right angle corner"
  match result with
  | some (t1, beziers, t2) =>
    -- For 90° corner, tangent points should be radius distance from corner
    let d1 := Float.sqrt ((t1.x - p1.x) * (t1.x - p1.x) + (t1.y - p1.y) * (t1.y - p1.y))
    let d2 := Float.sqrt ((t2.x - p1.x) * (t2.x - p1.x) + (t2.y - p1.y) * (t2.y - p1.y))
    ensure ((d1 - radius).abs < 1.0) s!"First tangent distance should be ~{radius}, got {d1}"
    ensure ((d2 - radius).abs < 1.0) s!"Second tangent distance should be ~{radius}, got {d2}"
    ensure (beziers.size >= 1) "Should produce at least 1 bezier segment"
  | none => pure ()  -- Already handled by isSome check

/-! ## tessellatePath Tests (uses triangulatePolygon) -/

test "tessellatePath handles concave polygon" := do
  -- Create a concave arrow path
  let path := Path.empty
    |>.moveTo ⟨50, 0⟩
    |>.lineTo ⟨100, 50⟩
    |>.lineTo ⟨75, 50⟩
    |>.lineTo ⟨75, 100⟩
    |>.lineTo ⟨25, 100⟩
    |>.lineTo ⟨25, 50⟩
    |>.lineTo ⟨0, 50⟩
    |>.closePath
  let result := tessellatePath path Color.red
  -- Should produce valid triangulation
  ensure (result.vertices.size > 0) "Should produce vertices"
  ensure (result.indices.size > 0) "Should produce indices"
  -- 7 vertices × 6 floats = 42
  ensure (result.vertices.size == 42) s!"Expected 42 floats (7 vertices), got {result.vertices.size}"
  -- 5 triangles × 3 indices = 15
  ensure (result.indices.size == 15) s!"Expected 15 indices, got {result.indices.size}"

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

/-! ## Bezier-Curved Shape Tessellation Tests -/

test "heart tessellation produces valid triangles" := do
  let heartPath := Path.heart ⟨100, 100⟩ 50
  let points := pathToPolygon heartPath 0.5
  let indices := triangulatePolygon points
  -- Basic validity checks
  ensure (points.size >= 3) s!"Heart should have at least 3 points, got {points.size}"
  ensure (indices.size >= 3) s!"Should produce at least 1 triangle, got {indices.size}"
  ensure (indices.size % 3 == 0) s!"Index count should be multiple of 3, got {indices.size}"
  -- Verify all indices are in bounds
  for idx in indices do
    ensure (idx.toNat < points.size) s!"Index {idx} out of bounds (points.size={points.size})"
  -- Expected triangle count: n-2 triangles for n vertices
  let expectedTriangles := points.size - 2
  ensure (indices.size / 3 == expectedTriangles) s!"Expected {expectedTriangles} triangles, got {indices.size / 3}"

test "circle tessellation produces valid triangles" := do
  let circlePath := Path.circle ⟨100, 100⟩ 50
  let points := pathToPolygon circlePath 0.5
  let indices := triangulatePolygon points
  -- Basic validity checks
  ensure (points.size >= 3) s!"Circle should have at least 3 points, got {points.size}"
  ensure (indices.size >= 3) s!"Should produce at least 1 triangle, got {indices.size}"
  ensure (indices.size % 3 == 0) s!"Index count should be multiple of 3, got {indices.size}"
  -- Verify all indices are in bounds
  for idx in indices do
    ensure (idx.toNat < points.size) s!"Index {idx} out of bounds (points.size={points.size})"
  -- Expected triangle count
  let expectedTriangles := points.size - 2
  ensure (indices.size / 3 == expectedTriangles) s!"Expected {expectedTriangles} triangles, got {indices.size / 3}"

test "ellipse tessellation produces valid triangles" := do
  let ellipsePath := Path.ellipse ⟨100, 100⟩ 60 40
  let points := pathToPolygon ellipsePath 0.5
  let indices := triangulatePolygon points
  -- Basic validity checks
  ensure (points.size >= 3) s!"Ellipse should have at least 3 points, got {points.size}"
  ensure (indices.size >= 3) s!"Should produce at least 1 triangle, got {indices.size}"
  ensure (indices.size % 3 == 0) s!"Index count should be multiple of 3, got {indices.size}"
  -- Verify all indices are in bounds
  for idx in indices do
    ensure (idx.toNat < points.size) s!"Index {idx} out of bounds (points.size={points.size})"
  -- Expected triangle count
  let expectedTriangles := points.size - 2
  ensure (indices.size / 3 == expectedTriangles) s!"Expected {expectedTriangles} triangles, got {indices.size / 3}"

test "pie slice tessellation produces valid triangles" := do
  let piePath := Path.pie ⟨100, 100⟩ 50 0 Float.halfPi
  let points := pathToPolygon piePath 0.5
  let indices := triangulatePolygon points
  -- Basic validity checks
  ensure (points.size >= 3) s!"Pie should have at least 3 points, got {points.size}"
  ensure (indices.size >= 3) s!"Should produce at least 1 triangle, got {indices.size}"
  ensure (indices.size % 3 == 0) s!"Index count should be multiple of 3, got {indices.size}"
  -- Verify all indices are in bounds
  for idx in indices do
    ensure (idx.toNat < points.size) s!"Index {idx} out of bounds (points.size={points.size})"

test "270-degree arc tessellation produces valid triangles" := do
  -- This is the "45+scale" test case - a 270-degree arc creates a non-convex chord
  let arcPath := Path.arcPath ⟨0, 0⟩ 35 0 (Float.pi * 1.5) false
  let points := pathToPolygon arcPath 0.5
  let indices := triangulatePolygon points
  -- Basic validity checks
  ensure (points.size >= 3) s!"Arc should have at least 3 points, got {points.size}"
  ensure (indices.size >= 3) s!"Should produce at least 1 triangle, got {indices.size}"
  ensure (indices.size % 3 == 0) s!"Index count should be multiple of 3, got {indices.size}"
  -- Verify all indices are in bounds
  for idx in indices do
    ensure (idx.toNat < points.size) s!"Index {idx} out of bounds (points.size={points.size})"
  -- Expected triangle count
  let expectedTriangles := points.size - 2
  ensure (indices.size / 3 == expectedTriangles) s!"Expected {expectedTriangles} triangles, got {indices.size / 3}"

test "star shape tessellation produces valid triangles" := do
  -- Stars are non-convex with alternating inner/outer vertices
  let starPath := Path.star ⟨100, 100⟩ 50 25 5
  let points := pathToPolygon starPath 0.5
  let indices := triangulatePolygon points
  -- Basic validity checks - star with 5 points has 10 vertices
  ensure (points.size == 10) s!"5-pointed star should have 10 vertices, got {points.size}"
  ensure (indices.size >= 3) s!"Should produce at least 1 triangle, got {indices.size}"
  ensure (indices.size % 3 == 0) s!"Index count should be multiple of 3, got {indices.size}"
  -- Verify all indices are in bounds
  for idx in indices do
    ensure (idx.toNat < points.size) s!"Index {idx} out of bounds (points.size={points.size})"
  -- Expected triangle count: 10-2 = 8 triangles = 24 indices
  ensure (indices.size == 24) s!"Expected 24 indices for star, got {indices.size}"

test "semicircle tessellation produces valid triangles" := do
  let semiPath := Path.semicircle ⟨100, 100⟩ 50
  let points := pathToPolygon semiPath 0.5
  let indices := triangulatePolygon points
  -- Basic validity checks
  ensure (points.size >= 3) s!"Semicircle should have at least 3 points, got {points.size}"
  ensure (indices.size >= 3) s!"Should produce at least 1 triangle, got {indices.size}"
  ensure (indices.size % 3 == 0) s!"Index count should be multiple of 3, got {indices.size}"
  -- Verify all indices are in bounds
  for idx in indices do
    ensure (idx.toNat < points.size) s!"Index {idx} out of bounds (points.size={points.size})"

test "rounded rectangle tessellation produces valid triangles" := do
  let rrPath := Path.roundedRect (Rect.mk' 0 0 100 80) 15
  let points := pathToPolygon rrPath 0.5
  let indices := triangulatePolygon points
  -- Basic validity checks
  ensure (points.size >= 8) s!"Rounded rect should have at least 8 points, got {points.size}"
  ensure (indices.size >= 3) s!"Should produce at least 1 triangle, got {indices.size}"
  ensure (indices.size % 3 == 0) s!"Index count should be multiple of 3, got {indices.size}"
  -- Verify all indices are in bounds
  for idx in indices do
    ensure (idx.toNat < points.size) s!"Index {idx} out of bounds (points.size={points.size})"
  -- Expected triangle count
  let expectedTriangles := points.size - 2
  ensure (indices.size / 3 == expectedTriangles) s!"Expected {expectedTriangles} triangles, got {indices.size / 3}"



end Afferent.Tests.TessellationTests

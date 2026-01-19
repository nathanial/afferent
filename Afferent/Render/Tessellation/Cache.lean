/-
  Afferent Tessellation Cache
  Types and functions for pre-tessellated polygon caching and batching.
-/
import Afferent.Core.Types
import Afferent.Render.Tessellation.Types

namespace Afferent

namespace Tessellation

/-- Pre-tessellated polygon in LOCAL coordinates (not NDC, no colors).
    Used to cache tessellation results that can be reused each frame. -/
structure TessellatedPolygon where
  /-- Vertex positions: [x, y, x, y, ...] in normalized 0-1 coords. -/
  positions : Array Float
  /-- Triangle indices for rendering. -/
  indices : Array UInt32
  /-- Number of vertices (positions.size / 2). -/
  vertexCount : Nat
  /-- Centroid X coordinate in normalized coords. -/
  centroidX : Float
  /-- Centroid Y coordinate in normalized coords. -/
  centroidY : Float
  /-- Bounding box: (minX, minY, maxX, maxY) -/
  bounds : Float × Float × Float × Float
deriving Repr, Inhabited

namespace TessellatedPolygon

/-- Create an empty tessellated polygon. -/
def empty : TessellatedPolygon :=
  { positions := #[]
    indices := #[]
    vertexCount := 0
    centroidX := 0.0
    centroidY := 0.0
    bounds := (0.0, 0.0, 0.0, 0.0) }

/-- Check if the tessellated polygon is empty. -/
def isEmpty (poly : TessellatedPolygon) : Bool := poly.vertexCount == 0

end TessellatedPolygon

/-- Batch accumulator with transformed, colored vertices in screen coordinates.
    Accumulates geometry from multiple TessellatedPolygons for a single draw call.
    NDC conversion is done at execute time when screen dimensions are available. -/
structure TessellatedBatch where
  /-- Vertex data: [x, y, r, g, b, a, ...] in screen coordinates (pixels). -/
  vertices : Array Float
  /-- Triangle indices. -/
  indices : Array UInt32
  /-- Current vertex count (vertices.size / 6). -/
  vertexCount : Nat
deriving Repr, Inhabited

namespace TessellatedBatch

/-- Create an empty batch. -/
def empty : TessellatedBatch :=
  { vertices := #[]
    indices := #[]
    vertexCount := 0 }

/-- Create a batch with pre-allocated capacity.
    avgVerticesPerPolygon is the expected average vertex count per polygon. -/
def withCapacity (polygonCount : Nat) (avgVerticesPerPolygon : Nat := 50) : TessellatedBatch :=
  let totalVertices := polygonCount * avgVerticesPerPolygon
  let totalIndices := polygonCount * (avgVerticesPerPolygon - 2) * 3  -- triangles
  { vertices := Array.mkEmpty (totalVertices * 6)  -- 6 floats per vertex
    indices := Array.mkEmpty totalIndices
    vertexCount := 0 }

/-- Add a pre-tessellated polygon to the batch with transformation and color.
    Outputs screen coordinates (not NDC) - conversion happens at execute time.
    - poly: Pre-tessellated polygon in normalized 0-1 coordinates
    - rectX, rectY: Content rect offset in screen pixels
    - panX, panY: Pan offset in screen pixels
    - zoom: Zoom factor
    - centerX, centerY: Screen center for zoom origin (relative to content rect)
    - rectWidth, rectHeight: Content rectangle dimensions
    - color: Fill color (RGBA) -/
def addPolygon (batch : TessellatedBatch) (poly : TessellatedPolygon)
    (rectX rectY panX panY zoom centerX centerY rectWidth rectHeight : Float)
    (color : Color) : TessellatedBatch :=
  if poly.isEmpty then batch
  else Id.run do
    let baseIndex := batch.vertexCount.toUInt32
    let mut vertices := batch.vertices
    let mut indices := batch.indices

    -- Transform each vertex from normalized coords to screen coords
    let posCount := poly.positions.size / 2
    for i in [:posCount] do
      let normX := poly.positions[i * 2]!
      let normY := poly.positions[i * 2 + 1]!
      -- Transform: normalized -> base screen -> zoomed -> panned -> offset by rect position
      let baseX := normX * rectWidth
      let baseY := normY * rectHeight
      let localX := centerX + (baseX - centerX) * zoom + panX
      let localY := centerY + (baseY - centerY) * zoom + panY
      -- Add rect offset to get final screen position
      let screenX := rectX + localX
      let screenY := rectY + localY
      -- Push vertex with color (screen coordinates, not NDC)
      vertices := vertices.push screenX |>.push screenY
        |>.push color.r |>.push color.g |>.push color.b |>.push color.a

    -- Remap indices
    for idx in poly.indices do
      indices := indices.push (idx + baseIndex)

    { vertices, indices, vertexCount := batch.vertexCount + posCount }

/-- Check if the batch is empty. -/
def isEmpty (batch : TessellatedBatch) : Bool := batch.vertexCount == 0

/-- Get the number of indices for draw call. -/
def indexCount (batch : TessellatedBatch) : Nat := batch.indices.size

end TessellatedBatch

/-- Line data accumulator for batched stroke rendering.
    Accumulates line segments from multiple polygons for a single draw call. -/
structure StrokeBatch where
  /-- Line data: [x1, y1, x2, y2, r, g, b, a, padding] per line. -/
  data : Array Float
  /-- Number of line segments. -/
  lineCount : Nat
deriving Repr, Inhabited

namespace StrokeBatch

/-- Create an empty stroke batch. -/
def empty : StrokeBatch :=
  { data := #[], lineCount := 0 }

/-- Create a stroke batch with pre-allocated capacity. -/
def withCapacity (polygonCount : Nat) (avgEdgesPerPolygon : Nat := 50) : StrokeBatch :=
  let totalLines := polygonCount * avgEdgesPerPolygon
  { data := Array.mkEmpty (totalLines * 9)  -- 9 floats per line
    lineCount := 0 }

/-- Add a polygon's border to the stroke batch.
    - poly: Pre-tessellated polygon (we use positions for edges)
    - rectX, rectY: Content rect offset in screen pixels
    - panX, panY: Pan offset in screen pixels
    - zoom: Zoom factor
    - centerX, centerY: Screen center for zoom origin (relative to content rect)
    - rectWidth, rectHeight: Content rectangle dimensions
    - color: Stroke color (RGBA) -/
def addPolygonBorder (batch : StrokeBatch) (poly : TessellatedPolygon)
    (rectX rectY panX panY zoom centerX centerY rectWidth rectHeight : Float)
    (color : Color) : StrokeBatch :=
  if poly.vertexCount < 3 then batch
  else Id.run do
    let mut data := batch.data
    let mut lineCount := batch.lineCount

    -- Transform function for normalized -> screen coords (with rect offset)
    let transform (normX normY : Float) : Float × Float :=
      let baseX := normX * rectWidth
      let baseY := normY * rectHeight
      let localX := centerX + (baseX - centerX) * zoom + panX
      let localY := centerY + (baseY - centerY) * zoom + panY
      -- Add rect offset to get final screen position
      (rectX + localX, rectY + localY)

    -- Add edges (connect consecutive vertices, close the polygon)
    let numVerts := poly.vertexCount
    for i in [:numVerts] do
      let nextI := if i + 1 < numVerts then i + 1 else 0
      let x1 := poly.positions[i * 2]!
      let y1 := poly.positions[i * 2 + 1]!
      let x2 := poly.positions[nextI * 2]!
      let y2 := poly.positions[nextI * 2 + 1]!
      let (sx1, sy1) := transform x1 y1
      let (sx2, sy2) := transform x2 y2
      -- Push line data: x1, y1, x2, y2, r, g, b, a, padding
      data := data.push sx1 |>.push sy1 |>.push sx2 |>.push sy2
        |>.push color.r |>.push color.g |>.push color.b |>.push color.a
        |>.push 0.0
      lineCount := lineCount + 1

    { data, lineCount }

/-- Check if the batch is empty. -/
def isEmpty (batch : StrokeBatch) : Bool := batch.lineCount == 0

end StrokeBatch

end Tessellation

end Afferent

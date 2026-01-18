/-
  Afferent Tessellation Batch Accumulation
-/
import Afferent.Core.Types
import Afferent.Core.Paint
import Afferent.Core.Transform
import Afferent.Render.Tessellation.Types
import Afferent.Render.Tessellation.Fill

namespace Afferent

/-! ## Batch Accumulation -/

/-- Accumulates tessellated geometry for a single draw call.
    Use this to batch many shapes into one draw call for better performance. -/
structure Batch where
  /-- Accumulated vertex data (vertexSize2D floats per vertex: x, y, r, g, b, a). -/
  vertices : Array Float
  /-- Accumulated triangle indices. -/
  indices : Array UInt32
  /-- Current vertex count (vertices.size / vertexSize2D), used for index remapping. -/
  vertexCount : Nat
deriving Inhabited

namespace Batch

/-- Create an empty batch. -/
def empty : Batch := { vertices := #[], indices := #[], vertexCount := 0 }

/-- Create a batch with pre-allocated capacity for estimated shape count.
    Assumes ~30 floats and ~10 indices per shape on average. -/
def withCapacity (shapeCount : Nat) : Batch :=
  { vertices := Array.mkEmpty (shapeCount * 30)
    indices := Array.mkEmpty (shapeCount * 10)
    vertexCount := 0 }

/-- Add a tessellation result to the batch.
    Indices are automatically remapped to account for existing vertices.
    Uses in-place push loop to avoid intermediate array allocation from .map -/
def add (batch : Batch) (result : TessellationResult) : Batch :=
  if result.vertices.size == 0 then batch
  else Id.run do
    let offset := batch.vertexCount.toUInt32
    -- Push indices one by one with offset applied inline (no intermediate array)
    let mut indices := batch.indices
    for idx in result.indices do
      indices := indices.push (idx + offset)
    { vertices := batch.vertices ++ result.vertices
      indices := indices
      vertexCount := batch.vertexCount + result.vertices.size / Tessellation.vertexSize2D }

/-- Combine two batches.
    Uses in-place push loop to avoid intermediate array allocation from .map -/
def append (b1 b2 : Batch) : Batch :=
  if b2.vertices.size == 0 then b1
  else if b1.vertices.size == 0 then b2
  else Id.run do
    let offset := b1.vertexCount.toUInt32
    -- Push indices one by one with offset applied inline (no intermediate array)
    let mut indices := b1.indices
    for idx in b2.indices do
      indices := indices.push (idx + offset)
    { vertices := b1.vertices ++ b2.vertices
      indices := indices
      vertexCount := b1.vertexCount + b2.vertexCount }

/-- FAST PATH: Add a transformed rectangle directly to the batch.
    No intermediate TessellationResult allocation - writes directly to batch arrays.
    This is the hot path for batched rectangle rendering.
    Note: Gradients are sampled at ORIGINAL positions (before transform) since gradient
    coordinates are defined in the original coordinate space. -/
def addTransformedRect (batch : Batch) (rect : Rect) (transform : Transform)
    (style : FillStyle) (screenWidth screenHeight : Float) : Batch :=
  -- Sample colors at ORIGINAL positions (before transform)
  -- This is correct because gradient coordinates are in original space
  let tlColor := Tessellation.sampleFillStyle style rect.topLeft
  let trColor := Tessellation.sampleFillStyle style rect.topRight
  let blColor := Tessellation.sampleFillStyle style rect.bottomLeft
  let brColor := Tessellation.sampleFillStyle style rect.bottomRight

  -- Transform corners for rendering positions
  let tl := transform.apply rect.topLeft
  let tr := transform.apply rect.topRight
  let bl := transform.apply rect.bottomLeft
  let br := transform.apply rect.bottomRight

  -- Convert to NDC
  let tlNDC := Tessellation.pixelToNDC tl.x tl.y screenWidth screenHeight
  let trNDC := Tessellation.pixelToNDC tr.x tr.y screenWidth screenHeight
  let blNDC := Tessellation.pixelToNDC bl.x bl.y screenWidth screenHeight
  let brNDC := Tessellation.pixelToNDC br.x br.y screenWidth screenHeight

  -- Current vertex index for this rect
  let baseIdx := batch.vertexCount.toUInt32

  -- Push vertices directly (no intermediate array)
  let vertices := batch.vertices
    |>.push tlNDC.x |>.push tlNDC.y |>.push tlColor.r |>.push tlColor.g |>.push tlColor.b |>.push tlColor.a
    |>.push trNDC.x |>.push trNDC.y |>.push trColor.r |>.push trColor.g |>.push trColor.b |>.push trColor.a
    |>.push brNDC.x |>.push brNDC.y |>.push brColor.r |>.push brColor.g |>.push brColor.b |>.push brColor.a
    |>.push blNDC.x |>.push blNDC.y |>.push blColor.r |>.push blColor.g |>.push blColor.b |>.push blColor.a

  -- Push indices directly (two triangles: 0,1,2 and 0,2,3 offset by baseIdx)
  let indices := batch.indices
    |>.push baseIdx |>.push (baseIdx + 1) |>.push (baseIdx + 2)
    |>.push baseIdx |>.push (baseIdx + 2) |>.push (baseIdx + 3)

  { vertices, indices, vertexCount := batch.vertexCount + 4 }

/-- FASTEST PATH: Add a rectangle with pre-computed position, rotation, size, and color.
    Computes transform inline - no Canvas state, no Transform struct allocation.
    x, y: center position; angle: rotation in radians; halfSize: half the side length -/
def addRectDirect (batch : Batch) (x y angle halfSize : Float) (color : Color)
    (screenWidth screenHeight : Float) : Batch :=
  -- Inline rotation matrix computation (avoid Transform allocation)
  let cosA := Float.cos angle
  let sinA := Float.sin angle

  -- Compute 4 corners relative to center, then rotate and translate
  -- Corner offsets: (-h,-h), (h,-h), (h,h), (-h,h) where h = halfSize
  let h := halfSize

  -- Top-left corner (relative: -h, -h)
  let tlX := x + (-h) * cosA - (-h) * sinA
  let tlY := y + (-h) * sinA + (-h) * cosA

  -- Top-right corner (relative: h, -h)
  let trX := x + h * cosA - (-h) * sinA
  let trY := y + h * sinA + (-h) * cosA

  -- Bottom-right corner (relative: h, h)
  let brX := x + h * cosA - h * sinA
  let brY := y + h * sinA + h * cosA

  -- Bottom-left corner (relative: -h, h)
  let blX := x + (-h) * cosA - h * sinA
  let blY := y + (-h) * sinA + h * cosA

  -- Convert to NDC inline
  let toNdcX := fun px => (px / screenWidth) * 2.0 - 1.0
  let toNdcY := fun py => 1.0 - (py / screenHeight) * 2.0

  let tlNdcX := toNdcX tlX; let tlNdcY := toNdcY tlY
  let trNdcX := toNdcX trX; let trNdcY := toNdcY trY
  let brNdcX := toNdcX brX; let brNdcY := toNdcY brY
  let blNdcX := toNdcX blX; let blNdcY := toNdcY blY

  let baseIdx := batch.vertexCount.toUInt32

  let vertices := batch.vertices
    |>.push tlNdcX |>.push tlNdcY |>.push color.r |>.push color.g |>.push color.b |>.push color.a
    |>.push trNdcX |>.push trNdcY |>.push color.r |>.push color.g |>.push color.b |>.push color.a
    |>.push brNdcX |>.push brNdcY |>.push color.r |>.push color.g |>.push color.b |>.push color.a
    |>.push blNdcX |>.push blNdcY |>.push color.r |>.push color.g |>.push color.b |>.push color.a

  let indices := batch.indices
    |>.push baseIdx |>.push (baseIdx + 1) |>.push (baseIdx + 2)
    |>.push baseIdx |>.push (baseIdx + 2) |>.push (baseIdx + 3)

  { vertices, indices, vertexCount := batch.vertexCount + 4 }

/-- FAST PATH: Add an axis-aligned rectangle directly to the batch.
    No rotation computation - optimized for UI elements like terminal cells.
    x, y: top-left corner position; width, height: rectangle dimensions -/
def addAxisAlignedRect (batch : Batch) (x y width height : Float) (color : Color)
    (screenWidth screenHeight : Float) : Batch :=
  -- Compute corner positions
  let right := x + width
  let bottom := y + height

  -- Convert to NDC inline
  let toNdcX := fun px => (px / screenWidth) * 2.0 - 1.0
  let toNdcY := fun py => 1.0 - (py / screenHeight) * 2.0

  let tlNdcX := toNdcX x;     let tlNdcY := toNdcY y
  let trNdcX := toNdcX right; let trNdcY := toNdcY y
  let brNdcX := toNdcX right; let brNdcY := toNdcY bottom
  let blNdcX := toNdcX x;     let blNdcY := toNdcY bottom

  let baseIdx := batch.vertexCount.toUInt32

  -- 4 vertices × 6 floats each (x, y, r, g, b, a)
  let vertices := batch.vertices
    |>.push tlNdcX |>.push tlNdcY |>.push color.r |>.push color.g |>.push color.b |>.push color.a
    |>.push trNdcX |>.push trNdcY |>.push color.r |>.push color.g |>.push color.b |>.push color.a
    |>.push brNdcX |>.push brNdcY |>.push color.r |>.push color.g |>.push color.b |>.push color.a
    |>.push blNdcX |>.push blNdcY |>.push color.r |>.push color.g |>.push color.b |>.push color.a

  -- Two triangles: (TL,TR,BR) and (TL,BR,BL)
  let indices := batch.indices
    |>.push baseIdx |>.push (baseIdx + 1) |>.push (baseIdx + 2)
    |>.push baseIdx |>.push (baseIdx + 2) |>.push (baseIdx + 3)

  { vertices, indices, vertexCount := batch.vertexCount + 4 }

/-- Check if the batch is empty. -/
def isEmpty (batch : Batch) : Bool := batch.vertices.size == 0

/-- Get the number of indices (for draw call). -/
def indexCount (batch : Batch) : Nat := batch.indices.size

/-- Get the number of vertices. -/
def vertexCount' (batch : Batch) : Nat := batch.vertexCount

end Batch

end Afferent

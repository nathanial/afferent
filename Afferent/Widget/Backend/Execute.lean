/-
  Afferent Widget Backend Command Execution
-/
import Afferent.Canvas.Context
import Afferent.Core.Path
import Afferent.Core.Transform
import Afferent.Text.Font
import Afferent.Text.Measurer
import Afferent.Arbor
import Afferent.Arbor.Core.Path
import Afferent.Widget.Backend.Convert
import Std.Data.HashMap

namespace Afferent.Widget

open Afferent
open Afferent.Arbor

/-- Execute a single RenderCommand using CanvasM.
    Requires a FontRegistry to resolve FontIds to Font handles. -/
def executeCommand (reg : FontRegistry) (cmd : Afferent.Arbor.RenderCommand) : CanvasM Unit := do
  match cmd with
  | .fillRect rect color cornerRadius =>
    let afferentRect := toAfferentRect rect
    if cornerRadius > 0 then
      CanvasM.setFillColor (toAfferentColor color)
      CanvasM.fillRoundedRect afferentRect cornerRadius
    else
      CanvasM.setFillColor (toAfferentColor color)
      CanvasM.fillRect afferentRect

  | .fillRectStyle rect style cornerRadius =>
    let afferentRect := toAfferentRect rect
    CanvasM.save
    CanvasM.setFillStyle style
    if cornerRadius > 0 then
      CanvasM.fillRoundedRect afferentRect cornerRadius
    else
      CanvasM.fillRect afferentRect
    CanvasM.restore

  | .strokeRect rect color lineWidth cornerRadius =>
    let afferentRect := toAfferentRect rect
    CanvasM.setStrokeColor (toAfferentColor color)
    CanvasM.setLineWidth lineWidth
    if cornerRadius > 0 then
      CanvasM.strokeRoundedRect afferentRect cornerRadius
    else
      CanvasM.strokeRect afferentRect

  | .fillCircle center radius color =>
    -- Draw a single filled circle via the batch function
    let canvas ← CanvasM.getCanvas
    let (canvasWidth, canvasHeight) ← canvas.ctx.getCurrentSize
    let data := #[center.x, center.y, radius, 0.0, color.r, color.g, color.b, color.a, 0.0]
    canvas.ctx.renderer.drawBatch 1 data 1 0.0 0.0 canvasWidth canvasHeight

  | .strokeCircle center radius color lineWidth =>
    -- Draw a stroked circle using path (no stroked circle batch yet)
    let twoPi := 6.283185307179586  -- 2 * pi
    let path := Afferent.Path.empty.arc ⟨center.x, center.y⟩ radius 0 twoPi false
    CanvasM.setStrokeColor (toAfferentColor color)
    CanvasM.setLineWidth lineWidth
    CanvasM.strokePath path

  | .strokeLine p1 p2 color lineWidth =>
    -- Draw a single line via the batch function (batch size = 1)
    let canvas ← CanvasM.getCanvas
    let (canvasWidth, canvasHeight) ← canvas.ctx.getCurrentSize
    let data := #[p1.x, p1.y, p2.x, p2.y, color.r, color.g, color.b, color.a, 0.0]
    canvas.ctx.renderer.drawLineBatch data 1 lineWidth canvasWidth canvasHeight

  | .strokeLineBatch data count lineWidth =>
    if count == 0 then
      pure ()
    else
      let canvas ← CanvasM.getCanvas
      let (canvasWidth, canvasHeight) ← canvas.ctx.getCurrentSize
      canvas.ctx.renderer.drawLineBatch data count.toUInt32 lineWidth canvasWidth canvasHeight

  | .fillCircleBatch data count =>
    if count == 0 then
      pure ()
    else
      let canvas ← CanvasM.getCanvas
      let (canvasWidth, canvasHeight) ← canvas.ctx.getCurrentSize
      -- Convert from [cx, cy, radius, r, g, b, a] (7 floats) to [x, y, w, h, r, g, b, a, cornerRadius] (9 floats)
      let mut batchData : Array Float := Array.mkEmpty (count * 9)
      for i in [:count] do
        let base := i * 7
        let cx := data[base]!
        let cy := data[base + 1]!
        let radius := data[base + 2]!
        let r := data[base + 3]!
        let g := data[base + 4]!
        let b := data[base + 5]!
        let a := data[base + 6]!
        let diameter := radius * 2.0
        batchData := batchData.push (cx - radius) |>.push (cy - radius)
                               |>.push diameter |>.push diameter
                               |>.push r |>.push g |>.push b |>.push a |>.push 0.0
      canvas.ctx.renderer.drawBatch 1 batchData count.toUInt32 0.0 0.0 canvasWidth canvasHeight

  | .fillText text x y fontId color =>
    match reg.get fontId with
    | some font =>
      CanvasM.fillTextColor text ⟨x, y⟩ font (toAfferentColor color)
    | none =>
      -- Font not found, skip rendering
      pure ()

  | .fillTextBlock text rect fontId color align valign =>
    match reg.get fontId with
    | some font =>
      -- Measure text to calculate alignment
      let (textWidth, textHeight) ← CanvasM.measureText text font
      let x := match align with
        | .left => rect.origin.x
        | .center => rect.origin.x + (rect.size.width - textWidth) / 2
        | .right => rect.origin.x + rect.size.width - textWidth
      let y := match valign with
        | .top => rect.origin.y + font.ascender
        | .middle => rect.origin.y + (rect.size.height - textHeight) / 2 + font.ascender
        | .bottom => rect.origin.y + rect.size.height - font.descender
      CanvasM.fillTextColor text ⟨x, y⟩ font (toAfferentColor color)
    | none =>
      pure ()

  | .fillPolygon points color =>
    if points.size >= 3 then
      let path := polygonToPath points
      CanvasM.setFillColor (toAfferentColor color)
      CanvasM.fillPath path
    else
      pure ()

  | .strokePolygon points color lineWidth =>
    if points.size >= 3 then
      let path := polygonToPath points
      CanvasM.setStrokeColor (toAfferentColor color)
      CanvasM.setLineWidth lineWidth
      CanvasM.strokePath path
    else
      pure ()

  | .fillPath path color =>
    let afferentPath := toAfferentPath path
    CanvasM.setFillColor (toAfferentColor color)
    CanvasM.fillPath afferentPath

  | .fillPathStyle path style =>
    let afferentPath := toAfferentPath path
    CanvasM.save
    CanvasM.setFillStyle style
    CanvasM.fillPath afferentPath
    CanvasM.restore

  | .strokePath path color lineWidth =>
    let afferentPath := toAfferentPath path
    CanvasM.setStrokeColor (toAfferentColor color)
    CanvasM.setLineWidth lineWidth
    CanvasM.strokePath afferentPath

  | .fillPolygonInstanced pathHash vertices indices instances centerX centerY =>
    if instances.size == 0 then
      pure ()
    else
      -- Get or create cached mesh
      let canvas ← CanvasM.getCanvas
      let (mesh, canvas) ← do
        match canvas.meshCache.get? pathHash with
        | some mesh => pure (mesh, canvas)
        | none =>
          -- Create new cached mesh
          let mesh ← FFI.MeshCache.create canvas.ctx.renderer vertices indices centerX centerY
          let cache := canvas.meshCache.insert pathHash mesh
          pure (mesh, { canvas with meshCache := cache })

      -- Ensure instance buffer has enough capacity (8 floats per instance)
      let requiredFloats := instances.size * 8
      let (buf, cap, canvas) ←
        match canvas.meshInstanceBuffer with
        | some buf =>
          if canvas.meshInstanceBufferCapacity >= requiredFloats then
            pure (buf, canvas.meshInstanceBufferCapacity, canvas)
          else
            FFI.FloatBuffer.destroy buf
            let newBuf ← FFI.FloatBuffer.create requiredFloats.toUSize
            pure (newBuf, requiredFloats, { canvas with meshInstanceBuffer := some newBuf, meshInstanceBufferCapacity := requiredFloats })
        | none =>
          let newBuf ← FFI.FloatBuffer.create requiredFloats.toUSize
          pure (newBuf, requiredFloats, { canvas with meshInstanceBuffer := some newBuf, meshInstanceBufferCapacity := requiredFloats })

      -- Write instance data to buffer
      let mut idx : USize := 0
      for inst in instances do
        buf.setVec8 idx inst.x inst.y inst.rotation inst.scale inst.r inst.g inst.b inst.a
        idx := idx + 8

      CanvasM.setCanvas canvas

      -- Draw all instances
      let (canvasWidth, canvasHeight) ← canvas.ctx.getCurrentSize
      FFI.MeshCache.drawInstancedBuffer canvas.ctx.renderer mesh buf instances.size.toUInt32 canvasWidth canvasHeight

  | .strokeArcInstanced instances segments =>
    if instances.size == 0 then
      pure ()
    else
      let canvas ← CanvasM.getCanvas
      let (canvasWidth, canvasHeight) ← canvas.ctx.getCurrentSize
      -- Pack arc instances into Float array: 10 floats per instance
      let data := instances.foldl (init := #[]) fun acc inst =>
        acc.push inst.centerX |>.push inst.centerY
           |>.push inst.startAngle |>.push inst.sweepAngle
           |>.push inst.radius |>.push inst.strokeWidth
           |>.push inst.r |>.push inst.g |>.push inst.b |>.push inst.a
      canvas.ctx.renderer.drawArcInstanced data instances.size.toUInt32 segments.toUInt32 canvasWidth canvasHeight

  | .drawFragment fragmentHash _primitiveType params _instanceCount =>
    -- Get pipeline cache from canvas
    let canvas ← CanvasM.getCanvas
    let cache ← canvas.fragmentCache.get

    -- Get or compile the fragment pipeline using global registry
    let (maybePipeline, newCache) ← Shader.getOrCompileGlobal cache canvas.ctx.renderer fragmentHash

    -- Update cache if changed
    canvas.fragmentCache.set newCache

    -- Draw if we have a pipeline
    match maybePipeline with
    | some pipeline =>
      let (canvasWidth, canvasHeight) ← canvas.ctx.getCurrentSize
      FFI.Fragment.draw canvas.ctx.renderer pipeline params canvasWidth canvasHeight
    | none =>
      -- Pipeline compilation failed or fragment not registered
      pure ()

  | .fillTessellatedBatch vertices indices vertexCount =>
    if vertexCount == 0 || indices.size == 0 then
      pure ()
    else
      -- Get screen dimensions for NDC conversion
      let canvas ← CanvasM.getCanvas
      let (screenWidth, screenHeight) ← canvas.ctx.getCurrentSize

      -- Convert screen coordinates to NDC
      -- vertices layout: [x, y, r, g, b, a, ...] (6 floats per vertex)
      let ndcVertices := Id.run do
        let mut ndc : Array Float := Array.mkEmpty vertices.size
        for i in [:vertexCount] do
          let base := i * 6
          let x := vertices[base]!
          let y := vertices[base + 1]!
          let r := vertices[base + 2]!
          let g := vertices[base + 3]!
          let b := vertices[base + 4]!
          let a := vertices[base + 5]!
          -- Convert pixel coords to NDC
          let ndcX := (x / screenWidth) * 2.0 - 1.0
          let ndcY := 1.0 - (y / screenHeight) * 2.0
          ndc := ndc.push ndcX |>.push ndcY |>.push r |>.push g |>.push b |>.push a
        ndc

      -- Create vertex and index buffers from NDC data
      let vertexBuffer ← FFI.Buffer.createVertex canvas.ctx.renderer ndcVertices
      let indexBuffer ← FFI.Buffer.createIndex canvas.ctx.renderer indices
      -- Draw the tessellated triangles
      canvas.ctx.renderer.drawTriangles vertexBuffer indexBuffer indices.size.toUInt32
      -- Clean up buffers
      FFI.Buffer.destroy indexBuffer
      FFI.Buffer.destroy vertexBuffer

  | .pushClip rect =>
    let afferentRect := toAfferentRect rect
    CanvasM.clip afferentRect

  | .popClip =>
    CanvasM.unclip

  | .pushTranslate dx dy =>
    CanvasM.save
    CanvasM.translate dx dy

  | .pushRotate angle =>
    CanvasM.save
    CanvasM.rotate angle

  | .pushScale sx sy =>
    CanvasM.save
    CanvasM.scale sx sy

  | .popTransform =>
    CanvasM.restore

  | .save =>
    CanvasM.save

  | .restore =>
    CanvasM.restore

end Afferent.Widget

/-
  Afferent FFI Fragment
  FFI bindings for shader fragment compilation and rendering.
-/
import Afferent.FFI.Renderer
import Afferent.FFI.FloatBuffer

namespace Afferent.FFI

/-- Opaque handle to a compiled fragment pipeline. -/
opaque FragmentPipelinePointed : NonemptyType
def FragmentPipeline : Type := FragmentPipelinePointed.type

instance : Nonempty FragmentPipeline := FragmentPipelinePointed.property

/-- Compile a shader fragment into a GPU pipeline.
    Returns `some pipeline` on success, `none` on compilation failure.

    Parameters:
    - `renderer`: The GPU renderer
    - `name`: Unique name for this fragment
    - `paramsStruct`: Metal struct definition for parameters
    - `functionCode`: Fragment function body (computes primitive from index)
    - `primitiveType`: Type of primitive (0 = circle)
    - `instanceCount`: Number of primitives per draw
    - `paramsFloatCount`: Number of floats in parameters struct -/
@[extern "lean_afferent_fragment_compile"]
opaque Fragment.compile (renderer : @& Renderer)
    (name : @& String) (paramsStruct : @& String) (functionCode : @& String)
    (primitiveType : UInt32) (instanceCount : UInt32) (paramsFloatCount : UInt32)
    : IO (Option FragmentPipeline)

/-- Destroy a compiled fragment pipeline. -/
@[extern "lean_afferent_fragment_destroy"]
opaque Fragment.destroy (pipeline : @& FragmentPipeline) : IO Unit

/-- Draw using a compiled fragment pipeline.

    Parameters:
    - `renderer`: The GPU renderer
    - `pipeline`: Compiled fragment pipeline
    - `params`: Float array containing parameter data
    - `canvasWidth`, `canvasHeight`: Viewport dimensions -/
@[extern "lean_afferent_fragment_draw"]
opaque Fragment.draw (renderer : @& Renderer) (pipeline : @& FragmentPipeline)
    (params : @& Array Float) (canvasWidth canvasHeight : Float) : IO Unit

/-- Draw using a compiled fragment pipeline with FloatBuffer for parameters.
    More efficient than array version for repeated calls.

    Parameters:
    - `renderer`: The GPU renderer
    - `pipeline`: Compiled fragment pipeline
    - `paramsBuffer`: FloatBuffer containing parameter data
    - `canvasWidth`, `canvasHeight`: Viewport dimensions -/
@[extern "lean_afferent_fragment_draw_buffer"]
opaque Fragment.drawBuffer (renderer : @& Renderer) (pipeline : @& FragmentPipeline)
    (paramsBuffer : @& FloatBuffer) (canvasWidth canvasHeight : Float) : IO Unit

end Afferent.FFI

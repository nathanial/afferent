// fragment_compiler.h - Runtime shader fragment compilation
#ifndef AFFERENT_FRAGMENT_COMPILER_H
#define AFFERENT_FRAGMENT_COMPILER_H

#import <Metal/Metal.h>

// Fragment primitive types (must match Lean FragmentPrimitive)
typedef enum {
    AFFERENT_FRAGMENT_CIRCLE = 0,
    AFFERENT_FRAGMENT_RECT = 1,
    AFFERENT_FRAGMENT_ARC = 2,
    AFFERENT_FRAGMENT_QUAD = 3
} AfferentFragmentPrimitiveType;

// Compiled fragment pipeline handle
typedef struct AfferentFragmentPipeline {
    __strong id<MTLRenderPipelineState> pipelineState;
    uint64_t fragmentHash;
    uint32_t primitiveType;
    uint32_t instanceCount;
    uint32_t paramsFloatCount;
} AfferentFragmentPipeline;

typedef AfferentFragmentPipeline* AfferentFragmentPipelineRef;

// Compile a fragment shader at runtime
// Returns NULL on failure (error logged via NSLog)
AfferentFragmentPipelineRef afferent_fragment_compile(
    id<MTLDevice> device,
    const char* fragmentName,
    const char* paramsStructCode,
    const char* fragmentCode,
    uint32_t primitiveType,
    uint32_t instanceCount,
    uint32_t paramsFloatCount
);

// Destroy a compiled fragment pipeline
void afferent_fragment_destroy(AfferentFragmentPipelineRef pipeline);

// Draw using a compiled fragment pipeline
// batchCount = number of param structs in paramsBuffer (for batched rendering)
// Total instances drawn = batchCount * pipeline->instanceCount
void afferent_fragment_draw(
    id<MTLRenderCommandEncoder> encoder,
    AfferentFragmentPipelineRef pipeline,
    id<MTLBuffer> paramsBuffer,
    uint32_t batchCount,
    float viewportWidth,
    float viewportHeight
);

#endif // AFFERENT_FRAGMENT_COMPILER_H

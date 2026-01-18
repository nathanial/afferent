// fragment_compiler.m - Runtime shader fragment compilation
#import <Metal/Metal.h>
#import <Foundation/Foundation.h>
#include "fragment_compiler.h"
#include "render.h"

// Template shader for circle-generating fragments
// Uses placeholders that get replaced with user code
// Supports batching: params buffer contains array of N param structs,
// totalInstances = N * primitivesPerInstance
static const char* fragment_circle_template =
    "#include <metal_stdlib>\n"
    "using namespace metal;\n"
    "\n"
    "// Result type for circle-generating fragments\n"
    "struct CircleResult {\n"
    "    float2 center;\n"
    "    float radius;\n"
    "    float4 color;\n"
    "};\n"
    "\n"
    "// === USER PARAMS STRUCT ===\n"
    "%s\n"  // PARAMS_STRUCT
    "\n"
    "// === USER FRAGMENT FUNCTION ===\n"
    "static inline CircleResult %s(uint idx, constant %s& p) {\n"  // FRAGMENT_NAME, PARAMS_TYPE
    "    %s\n"  // FRAGMENT_BODY
    "}\n"
    "\n"
    "// Uniforms for fragment shader\n"
    "struct FragmentCircleUniforms {\n"
    "    float2 viewport;\n"
    "    uint primitivesPerInstance;  // Circles per params struct (e.g., 16 for helix)\n"
    "    uint totalInstances;         // Total circles to draw (batchCount * primitivesPerInstance)\n"
    "};\n"
    "\n"
    "// Vertex output\n"
    "struct FragmentCircleVertexOut {\n"
    "    float4 position [[position]];\n"
    "    float4 color;\n"
    "    float2 uv;\n"
    "};\n"
    "\n"
    "// Unit quad vertices (for generating circle quads)\n"
    "constant float2 unitQuad[4] = {\n"
    "    float2(0, 0),\n"
    "    float2(1, 0),\n"
    "    float2(0, 1),\n"
    "    float2(1, 1)\n"
    "};\n"
    "\n"
    "vertex FragmentCircleVertexOut fragment_circle_vertex(\n"
    "    uint vid [[vertex_id]],\n"
    "    uint iid [[instance_id]],\n"
    "    constant %s* params [[buffer(0)]],\n"  // PARAMS_TYPE - array of param structs
    "    constant FragmentCircleUniforms& uniforms [[buffer(1)]]\n"
    ") {\n"
    "    // Compute which params struct this instance uses and its local index\n"
    "    uint paramIndex = iid / uniforms.primitivesPerInstance;\n"
    "    uint localIdx = iid %% uniforms.primitivesPerInstance;\n"
    "\n"
    "    // Call user's fragment function to compute circle properties\n"
    "    CircleResult c = %s(localIdx, params[paramIndex]);\n"  // FRAGMENT_NAME
    "\n"
    "    // Generate quad vertices for this circle\n"
    "    float2 uv = unitQuad[vid];\n"
    "    float diameter = c.radius * 2.0;\n"
    "    float2 pos = c.center - c.radius + uv * diameter;\n"
    "\n"
    "    // Convert to NDC\n"
    "    float2 ndc;\n"
    "    ndc.x = (pos.x / uniforms.viewport.x) * 2.0 - 1.0;\n"
    "    ndc.y = 1.0 - (pos.y / uniforms.viewport.y) * 2.0;\n"
    "\n"
    "    FragmentCircleVertexOut out;\n"
    "    out.position = float4(ndc, 0.0, 1.0);\n"
    "    out.color = c.color;\n"
    "    out.uv = uv;\n"
    "    return out;\n"
    "}\n"
    "\n"
    "fragment float4 fragment_circle_fragment(FragmentCircleVertexOut in [[stage_in]]) {\n"
    "    // Render smooth circle with anti-aliasing\n"
    "    float2 local = in.uv * 2.0 - 1.0;\n"
    "    float dist = length(local);\n"
    "    float alpha = 1.0 - smoothstep(0.95, 1.0, dist);\n"
    "    if (alpha < 0.01) discard_fragment();\n"
    "    return float4(in.color.rgb, in.color.a * alpha);\n"
    "}\n";

// Extract struct name from struct definition code
// e.g., "struct HelixParams { ... };" -> "HelixParams"
static NSString* extractStructName(const char* paramsStructCode) {
    NSString* code = [NSString stringWithUTF8String:paramsStructCode];

    // Look for "struct <name>" pattern
    NSRegularExpression* regex = [NSRegularExpression
        regularExpressionWithPattern:@"struct\\s+(\\w+)"
        options:0
        error:nil];

    NSTextCheckingResult* match = [regex firstMatchInString:code
        options:0
        range:NSMakeRange(0, code.length)];

    if (match && match.numberOfRanges > 1) {
        NSRange nameRange = [match rangeAtIndex:1];
        return [code substringWithRange:nameRange];
    }

    // Fallback: use the fragment name with "Params" suffix
    return @"FragmentParams";
}

AfferentFragmentPipelineRef afferent_fragment_compile(
    id<MTLDevice> device,
    const char* fragmentName,
    const char* paramsStructCode,
    const char* fragmentCode,
    uint32_t primitiveType,
    uint32_t instanceCount,
    uint32_t paramsFloatCount
) {
    if (!device || !fragmentName || !paramsStructCode || !fragmentCode) {
        NSLog(@"[FragmentCompiler] Invalid parameters");
        return NULL;
    }

    // Only circle primitives supported for now
    if (primitiveType != AFFERENT_FRAGMENT_CIRCLE) {
        NSLog(@"[FragmentCompiler] Only circle primitives are currently supported");
        return NULL;
    }

    // Extract param type name from struct definition
    NSString* paramsType = extractStructName(paramsStructCode);

    // Build shader source by substituting placeholders
    // Template has 6 format specifiers: paramsStruct, name, type, body, type (params ptr), name (call)
    NSString* shaderSource = [NSString stringWithFormat:
        [NSString stringWithUTF8String:fragment_circle_template],
        paramsStructCode,        // %s - PARAMS_STRUCT
        fragmentName,            // %s - FRAGMENT_NAME (function definition)
        [paramsType UTF8String], // %s - PARAMS_TYPE (in function signature)
        fragmentCode,            // %s - FRAGMENT_BODY
        [paramsType UTF8String], // %s - PARAMS_TYPE (params pointer type in vertex)
        fragmentName             // %s - FRAGMENT_NAME (call in vertex shader)
    ];

    // Compile shader
    NSError* compileError = nil;
    MTLCompileOptions* options = [[MTLCompileOptions alloc] init];
    options.fastMathEnabled = YES;

    id<MTLLibrary> library = [device newLibraryWithSource:shaderSource
                                                  options:options
                                                    error:&compileError];

    if (compileError || !library) {
        NSLog(@"[FragmentCompiler] Shader compilation failed for '%s': %@",
              fragmentName, compileError.localizedDescription);
        NSLog(@"[FragmentCompiler] Generated shader source:\n%@", shaderSource);
        return NULL;
    }

    // Get shader functions
    id<MTLFunction> vertexFunction = [library newFunctionWithName:@"fragment_circle_vertex"];
    id<MTLFunction> fragmentFunction = [library newFunctionWithName:@"fragment_circle_fragment"];

    if (!vertexFunction || !fragmentFunction) {
        NSLog(@"[FragmentCompiler] Failed to find shader functions for '%s'", fragmentName);
        return NULL;
    }

    // Create render pipeline descriptor
    MTLRenderPipelineDescriptor* pipelineDesc = [[MTLRenderPipelineDescriptor alloc] init];
    pipelineDesc.label = [NSString stringWithFormat:@"Fragment_%s", fragmentName];
    pipelineDesc.vertexFunction = vertexFunction;
    pipelineDesc.fragmentFunction = fragmentFunction;
    pipelineDesc.colorAttachments[0].pixelFormat = MTLPixelFormatBGRA8Unorm;
    pipelineDesc.rasterSampleCount = AFFERENT_MSAA_SAMPLE_COUNT;

    // Enable alpha blending
    MTLRenderPipelineColorAttachmentDescriptor* colorAttachment = pipelineDesc.colorAttachments[0];
    colorAttachment.blendingEnabled = YES;
    colorAttachment.sourceRGBBlendFactor = MTLBlendFactorSourceAlpha;
    colorAttachment.destinationRGBBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
    colorAttachment.rgbBlendOperation = MTLBlendOperationAdd;
    colorAttachment.sourceAlphaBlendFactor = MTLBlendFactorOne;
    colorAttachment.destinationAlphaBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
    colorAttachment.alphaBlendOperation = MTLBlendOperationAdd;

    // Create pipeline state
    NSError* pipelineError = nil;
    id<MTLRenderPipelineState> pipelineState = [device
        newRenderPipelineStateWithDescriptor:pipelineDesc
        error:&pipelineError];

    if (pipelineError || !pipelineState) {
        NSLog(@"[FragmentCompiler] Pipeline creation failed for '%s': %@",
              fragmentName, pipelineError.localizedDescription);
        return NULL;
    }

    // Allocate and populate pipeline handle
    AfferentFragmentPipelineRef pipeline = (AfferentFragmentPipelineRef)malloc(sizeof(AfferentFragmentPipeline));
    if (!pipeline) {
        NSLog(@"[FragmentCompiler] Failed to allocate pipeline for '%s'", fragmentName);
        return NULL;
    }

    pipeline->pipelineState = pipelineState;
    pipeline->fragmentHash = 0;  // Will be set by caller
    pipeline->primitiveType = primitiveType;
    pipeline->instanceCount = instanceCount;
    pipeline->paramsFloatCount = paramsFloatCount;

    NSLog(@"[FragmentCompiler] Successfully compiled fragment '%s' with %u instances",
          fragmentName, instanceCount);

    return pipeline;
}

void afferent_fragment_destroy(AfferentFragmentPipelineRef pipeline) {
    if (pipeline) {
        pipeline->pipelineState = nil;
        free(pipeline);
    }
}

void afferent_fragment_draw(
    id<MTLRenderCommandEncoder> encoder,
    AfferentFragmentPipelineRef pipeline,
    id<MTLBuffer> paramsBuffer,
    uint32_t batchCount,
    float viewportWidth,
    float viewportHeight
) {
    if (!encoder || !pipeline || !pipeline->pipelineState || batchCount == 0) {
        return;
    }

    // Total instances = batchCount * primitivesPerInstance
    uint32_t totalInstances = batchCount * pipeline->instanceCount;

    // Uniforms struct matching shader definition
    struct {
        float viewport[2];
        uint32_t primitivesPerInstance;
        uint32_t totalInstances;
    } uniforms = {
        { viewportWidth, viewportHeight },
        pipeline->instanceCount,
        totalInstances
    };

    // Set pipeline state
    [encoder setRenderPipelineState:pipeline->pipelineState];

    // Set buffers
    [encoder setVertexBuffer:paramsBuffer offset:0 atIndex:0];
    [encoder setVertexBytes:&uniforms length:sizeof(uniforms) atIndex:1];

    // Draw instanced quads (4 vertices per quad, triangle strip)
    // Each instance is one circle, draw all circles from all batched params
    [encoder drawPrimitives:MTLPrimitiveTypeTriangleStrip
                vertexStart:0
                vertexCount:4
              instanceCount:totalInstances];
}

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
    "// strokeWidth = 0 renders filled circle, >0 renders ring/stroke\n"
    "struct CircleResult {\n"
    "    float2 center;\n"
    "    float radius;\n"
    "    float strokeWidth;\n"
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
    "    float strokeRatio;  // strokeWidth / radius (0 = filled)\n"
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
    "    // Generate quad vertices for this circle/ring\n"
    "    // For rings, the quad must include the stroke extending beyond radius\n"
    "    float2 uv = unitQuad[vid];\n"
    "    float outerRadius = c.radius + c.strokeWidth * 0.5;\n"
    "    float diameter = outerRadius * 2.0;\n"
    "    float2 pos = c.center - outerRadius + uv * diameter;\n"
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
    "    out.strokeRatio = (c.radius > 0.0) ? c.strokeWidth / c.radius : 0.0;\n"
    "    return out;\n"
    "}\n"
    "\n"
    "fragment float4 fragment_circle_fragment(FragmentCircleVertexOut in [[stage_in]]) {\n"
    "    // Render smooth circle or ring with anti-aliasing\n"
    "    float2 local = in.uv * 2.0 - 1.0;\n"
    "    float dist = length(local);\n"
    "    float alpha;\n"
    "    if (in.strokeRatio > 0.0) {\n"
    "        // Ring mode: strokeRatio = strokeWidth / radius\n"
    "        // UV space is scaled to outerRadius, so outer edge is at dist=1\n"
    "        // Inner edge is at dist = 1 - strokeRatio / (1 + strokeRatio * 0.5)\n"
    "        float normalizedStroke = in.strokeRatio / (1.0 + in.strokeRatio * 0.5);\n"
    "        float innerEdge = 1.0 - normalizedStroke;\n"
    "        float outerAlpha = 1.0 - smoothstep(0.95, 1.0, dist);\n"
    "        float innerAlpha = smoothstep(innerEdge - 0.05, innerEdge, dist);\n"
    "        alpha = outerAlpha * innerAlpha;\n"
    "    } else {\n"
    "        // Filled circle mode\n"
    "        alpha = 1.0 - smoothstep(0.95, 1.0, dist);\n"
    "    }\n"
    "    if (alpha < 0.01) discard_fragment();\n"
    "    return float4(in.color.rgb, in.color.a * alpha);\n"
    "}\n";

// Template shader for rectangle-generating fragments
static const char* fragment_rect_template =
    "#include <metal_stdlib>\n"
    "using namespace metal;\n"
    "\n"
    "// Result type for rectangle-generating fragments\n"
    "struct RectResult {\n"
    "    float2 position;    // Top-left corner\n"
    "    float2 size;        // Width, height\n"
    "    float cornerRadius; // 0 = sharp corners\n"
    "    float4 color;\n"
    "};\n"
    "\n"
    "// === USER PARAMS STRUCT ===\n"
    "%s\n"  // PARAMS_STRUCT
    "\n"
    "// === USER FRAGMENT FUNCTION ===\n"
    "static inline RectResult %s(uint idx, constant %s& p) {\n"  // FRAGMENT_NAME, PARAMS_TYPE
    "    %s\n"  // FRAGMENT_BODY
    "}\n"
    "\n"
    "// Uniforms for fragment shader\n"
    "struct FragmentRectUniforms {\n"
    "    float2 viewport;\n"
    "    uint primitivesPerInstance;\n"
    "    uint totalInstances;\n"
    "};\n"
    "\n"
    "// Vertex output\n"
    "struct FragmentRectVertexOut {\n"
    "    float4 position [[position]];\n"
    "    float4 color;\n"
    "    float2 uv;\n"
    "    float2 rectSize;\n"
    "    float cornerRadius;\n"
    "};\n"
    "\n"
    "// Unit quad vertices\n"
    "constant float2 unitQuad[4] = {\n"
    "    float2(0, 0),\n"
    "    float2(1, 0),\n"
    "    float2(0, 1),\n"
    "    float2(1, 1)\n"
    "};\n"
    "\n"
    "vertex FragmentRectVertexOut fragment_rect_vertex(\n"
    "    uint vid [[vertex_id]],\n"
    "    uint iid [[instance_id]],\n"
    "    constant %s* params [[buffer(0)]],\n"  // PARAMS_TYPE
    "    constant FragmentRectUniforms& uniforms [[buffer(1)]]\n"
    ") {\n"
    "    uint paramIndex = iid / uniforms.primitivesPerInstance;\n"
    "    uint localIdx = iid %% uniforms.primitivesPerInstance;\n"
    "\n"
    "    RectResult r = %s(localIdx, params[paramIndex]);\n"  // FRAGMENT_NAME
    "\n"
    "    float2 uv = unitQuad[vid];\n"
    "    float2 pos = r.position + uv * r.size;\n"
    "\n"
    "    float2 ndc;\n"
    "    ndc.x = (pos.x / uniforms.viewport.x) * 2.0 - 1.0;\n"
    "    ndc.y = 1.0 - (pos.y / uniforms.viewport.y) * 2.0;\n"
    "\n"
    "    FragmentRectVertexOut out;\n"
    "    out.position = float4(ndc, 0.0, 1.0);\n"
    "    out.color = r.color;\n"
    "    out.uv = uv;\n"
    "    out.rectSize = r.size;\n"
    "    out.cornerRadius = r.cornerRadius;\n"
    "    return out;\n"
    "}\n"
    "\n"
    "fragment float4 fragment_rect_fragment(FragmentRectVertexOut in [[stage_in]]) {\n"
    "    if (in.cornerRadius <= 0.0) {\n"
    "        // Sharp rectangle - no distance field needed\n"
    "        return in.color;\n"
    "    }\n"
    "    // Rounded rectangle using SDF\n"
    "    float2 halfSize = in.rectSize * 0.5;\n"
    "    float2 localPos = (in.uv - 0.5) * in.rectSize;\n"
    "    float2 q = abs(localPos) - halfSize + in.cornerRadius;\n"
    "    float dist = length(max(q, 0.0)) + min(max(q.x, q.y), 0.0) - in.cornerRadius;\n"
    "    float alpha = 1.0 - smoothstep(-1.0, 0.0, dist);\n"
    "    if (alpha < 0.01) discard_fragment();\n"
    "    return float4(in.color.rgb, in.color.a * alpha);\n"
    "}\n";

// Template shader for quad fragments with per-pixel shading
// The fragment shader receives interpolated UVs and evaluates user's pixel DSL code
static const char* fragment_quad_template =
    "#include <metal_stdlib>\n"
    "using namespace metal;\n"
    "\n"
    "// Result type for quad vertex shader (computes quad bounds)\n"
    "struct QuadVertexResult {\n"
    "    float2 position;  // Top-left corner\n"
    "    float2 size;      // Width, height\n"
    "};\n"
    "\n"
    "// === USER PARAMS STRUCT ===\n"
    "%s\n"  // PARAMS_STRUCT
    "\n"
    "// === USER VERTEX FUNCTION (computes quad bounds) ===\n"
    "static inline QuadVertexResult %s_vertex(uint idx, constant %s& p) {\n"  // FRAGMENT_NAME, PARAMS_TYPE
    "    %s\n"  // VERTEX_BODY
    "}\n"
    "\n"
    "// Uniforms for quad shader\n"
    "struct FragmentQuadUniforms {\n"
    "    float2 viewport;\n"
    "    uint primitivesPerInstance;\n"
    "    uint totalInstances;\n"
    "};\n"
    "\n"
    "// Vertex output - passes UV and params to fragment shader\n"
    "struct FragmentQuadVertexOut {\n"
    "    float4 position [[position]];\n"
    "    float2 uv;  // 0-1 normalized within quad\n"
    "    uint paramIndex;  // Which param struct to use\n"
    "};\n"
    "\n"
    "// Unit quad vertices\n"
    "constant float2 unitQuad[4] = {\n"
    "    float2(0, 0),\n"
    "    float2(1, 0),\n"
    "    float2(0, 1),\n"
    "    float2(1, 1)\n"
    "};\n"
    "\n"
    "vertex FragmentQuadVertexOut fragment_quad_vertex(\n"
    "    uint vid [[vertex_id]],\n"
    "    uint iid [[instance_id]],\n"
    "    constant %s* params [[buffer(0)]],\n"  // PARAMS_TYPE
    "    constant FragmentQuadUniforms& uniforms [[buffer(1)]]\n"
    ") {\n"
    "    uint paramIndex = iid / uniforms.primitivesPerInstance;\n"
    "    uint localIdx = iid %% uniforms.primitivesPerInstance;\n"
    "\n"
    "    // Call user's vertex function to compute quad bounds\n"
    "    QuadVertexResult q = %s_vertex(localIdx, params[paramIndex]);\n"  // FRAGMENT_NAME
    "\n"
    "    // Generate quad vertex\n"
    "    float2 uv = unitQuad[vid];\n"
    "    float2 pos = q.position + uv * q.size;\n"
    "\n"
    "    // Convert to NDC\n"
    "    float2 ndc;\n"
    "    ndc.x = (pos.x / uniforms.viewport.x) * 2.0 - 1.0;\n"
    "    ndc.y = 1.0 - (pos.y / uniforms.viewport.y) * 2.0;\n"
    "\n"
    "    FragmentQuadVertexOut out;\n"
    "    out.position = float4(ndc, 0.0, 1.0);\n"
    "    out.uv = uv;\n"
    "    out.paramIndex = paramIndex;\n"
    "    return out;\n"
    "}\n"
    "\n"
    "// === USER PIXEL FUNCTION (computes per-pixel color) ===\n"
    "static inline float4 %s_pixel(FragmentQuadVertexOut in, constant %s& p) {\n"  // FRAGMENT_NAME, PARAMS_TYPE
    "    %s\n"  // PIXEL_BODY
    "}\n"
    "\n"
    "fragment float4 fragment_quad_fragment(\n"
    "    FragmentQuadVertexOut in [[stage_in]],\n"
    "    constant %s* params [[buffer(0)]]\n"  // PARAMS_TYPE
    ") {\n"
    "    float4 color = %s_pixel(in, params[in.paramIndex]);\n"  // FRAGMENT_NAME
    "    if (color.a < 0.01) discard_fragment();\n"
    "    return color;\n"
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

    // Extract param type name from struct definition
    NSString* paramsType = extractStructName(paramsStructCode);

    // Select template based on primitive type
    const char* shaderTemplate = NULL;
    NSString* vertexFuncName = nil;
    NSString* fragmentFuncName = nil;
    BOOL isQuadShader = NO;

    switch (primitiveType) {
        case AFFERENT_FRAGMENT_CIRCLE:
            shaderTemplate = fragment_circle_template;
            vertexFuncName = @"fragment_circle_vertex";
            fragmentFuncName = @"fragment_circle_fragment";
            break;
        case AFFERENT_FRAGMENT_RECT:
            shaderTemplate = fragment_rect_template;
            vertexFuncName = @"fragment_rect_vertex";
            fragmentFuncName = @"fragment_rect_fragment";
            break;
        case AFFERENT_FRAGMENT_QUAD:
            shaderTemplate = fragment_quad_template;
            vertexFuncName = @"fragment_quad_vertex";
            fragmentFuncName = @"fragment_quad_fragment";
            isQuadShader = YES;
            break;
        default:
            NSLog(@"[FragmentCompiler] Unsupported primitive type: %u", primitiveType);
            return NULL;
    }

    // Build shader source by substituting placeholders
    NSString* shaderSource = nil;

    if (isQuadShader) {
        // Quad shader has vertex and pixel code separated by "|||"
        NSString* fullCode = [NSString stringWithUTF8String:fragmentCode];
        NSArray* parts = [fullCode componentsSeparatedByString:@"|||"];
        if (parts.count != 2) {
            NSLog(@"[FragmentCompiler] Quad shader code must have vertex|||pixel format");
            return NULL;
        }
        NSString* vertexBody = parts[0];
        NSString* pixelBody = parts[1];

        // Quad template has 11 format specifiers:
        // paramsStruct, name_vertex, type, vertexBody, type (vertex params ptr), name_vertex (call),
        // name_pixel, type, pixelBody, type (fragment params ptr), name_pixel (call)
        shaderSource = [NSString stringWithFormat:
            [NSString stringWithUTF8String:shaderTemplate],
            paramsStructCode,            // %s - PARAMS_STRUCT
            fragmentName,                // %s - FRAGMENT_NAME (vertex function def)
            [paramsType UTF8String],     // %s - PARAMS_TYPE (vertex function signature)
            [vertexBody UTF8String],     // %s - VERTEX_BODY
            [paramsType UTF8String],     // %s - PARAMS_TYPE (vertex params pointer)
            fragmentName,                // %s - FRAGMENT_NAME (vertex call)
            fragmentName,                // %s - FRAGMENT_NAME (pixel function def)
            [paramsType UTF8String],     // %s - PARAMS_TYPE (pixel function signature)
            [pixelBody UTF8String],      // %s - PIXEL_BODY
            [paramsType UTF8String],     // %s - PARAMS_TYPE (fragment params pointer)
            fragmentName                 // %s - FRAGMENT_NAME (pixel call)
        ];
    } else {
        // Circle/Rect templates have 6 format specifiers:
        // paramsStruct, name, type, body, type (params ptr), name (call)
        shaderSource = [NSString stringWithFormat:
            [NSString stringWithUTF8String:shaderTemplate],
            paramsStructCode,        // %s - PARAMS_STRUCT
            fragmentName,            // %s - FRAGMENT_NAME (function definition)
            [paramsType UTF8String], // %s - PARAMS_TYPE (in function signature)
            fragmentCode,            // %s - FRAGMENT_BODY
            [paramsType UTF8String], // %s - PARAMS_TYPE (params pointer type in vertex)
            fragmentName             // %s - FRAGMENT_NAME (call in vertex shader)
        ];
    }

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
    id<MTLFunction> vertexFunction = [library newFunctionWithName:vertexFuncName];
    id<MTLFunction> fragmentFunction = [library newFunctionWithName:fragmentFuncName];

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
    // Quad fragment shaders read params in the fragment stage (buffer(0)).
    // Binding for all fragment types is safe and keeps quad pipelines working.
    [encoder setFragmentBuffer:paramsBuffer offset:0 atIndex:0];

    // Draw instanced quads (4 vertices per quad, triangle strip)
    // Each instance is one circle, draw all circles from all batched params
    [encoder drawPrimitives:MTLPrimitiveTypeTriangleStrip
                vertexStart:0
                vertexCount:4
              instanceCount:totalInstances];
}

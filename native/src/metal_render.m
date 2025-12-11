#import <Metal/Metal.h>
#import <QuartzCore/CAMetalLayer.h>
#include "afferent.h"

// External declarations from window.m
extern id<MTLDevice> afferent_window_get_device(AfferentWindowRef window);
extern CAMetalLayer* afferent_window_get_metal_layer(AfferentWindowRef window);

// Shader source embedded in code
static NSString *shaderSource = @R"(
#include <metal_stdlib>
using namespace metal;

struct VertexIn {
    float2 position [[attribute(0)]];
    float4 color [[attribute(1)]];
};

struct VertexOut {
    float4 position [[position]];
    float4 color;
};

vertex VertexOut vertex_main(VertexIn in [[stage_in]]) {
    VertexOut out;
    // Position is already in NDC (-1 to 1)
    out.position = float4(in.position, 0.0, 1.0);
    out.color = in.color;
    return out;
}

fragment float4 fragment_main(VertexOut in [[stage_in]]) {
    return in.color;
}
)";

// Internal renderer structure
struct AfferentRenderer {
    AfferentWindowRef window;
    id<MTLDevice> device;
    id<MTLCommandQueue> commandQueue;
    id<MTLRenderPipelineState> pipelineState;
    id<MTLCommandBuffer> currentCommandBuffer;
    id<MTLRenderCommandEncoder> currentEncoder;
    id<CAMetalDrawable> currentDrawable;
    MTLClearColor clearColor;
};

// Internal buffer structure
struct AfferentBuffer {
    id<MTLBuffer> mtlBuffer;
    uint32_t count;
};

AfferentResult afferent_renderer_create(
    AfferentWindowRef window,
    AfferentRendererRef* out_renderer
) {
    @autoreleasepool {
        struct AfferentRenderer *renderer = malloc(sizeof(struct AfferentRenderer));
        memset(renderer, 0, sizeof(struct AfferentRenderer));

        renderer->window = window;
        renderer->device = afferent_window_get_device(window);
        renderer->clearColor = MTLClearColorMake(0.0, 0.0, 0.0, 1.0);

        if (!renderer->device) {
            NSLog(@"No Metal device available");
            free(renderer);
            return AFFERENT_ERROR_DEVICE_FAILED;
        }

        // Create command queue
        renderer->commandQueue = [renderer->device newCommandQueue];
        if (!renderer->commandQueue) {
            NSLog(@"Failed to create command queue");
            free(renderer);
            return AFFERENT_ERROR_INIT_FAILED;
        }

        // Compile shaders
        NSError *error = nil;
        id<MTLLibrary> library = [renderer->device newLibraryWithSource:shaderSource
                                                                options:nil
                                                                  error:&error];
        if (!library) {
            NSLog(@"Shader compilation failed: %@", error);
            free(renderer);
            return AFFERENT_ERROR_PIPELINE_FAILED;
        }

        id<MTLFunction> vertexFunction = [library newFunctionWithName:@"vertex_main"];
        id<MTLFunction> fragmentFunction = [library newFunctionWithName:@"fragment_main"];

        if (!vertexFunction || !fragmentFunction) {
            NSLog(@"Failed to find shader functions");
            free(renderer);
            return AFFERENT_ERROR_PIPELINE_FAILED;
        }

        // Create vertex descriptor
        MTLVertexDescriptor *vertexDescriptor = [[MTLVertexDescriptor alloc] init];

        // Position: 2 floats at offset 0
        vertexDescriptor.attributes[0].format = MTLVertexFormatFloat2;
        vertexDescriptor.attributes[0].offset = offsetof(AfferentVertex, position);
        vertexDescriptor.attributes[0].bufferIndex = 0;

        // Color: 4 floats at offset 8 (after position)
        vertexDescriptor.attributes[1].format = MTLVertexFormatFloat4;
        vertexDescriptor.attributes[1].offset = offsetof(AfferentVertex, color);
        vertexDescriptor.attributes[1].bufferIndex = 0;

        // Layout
        vertexDescriptor.layouts[0].stride = sizeof(AfferentVertex);
        vertexDescriptor.layouts[0].stepFunction = MTLVertexStepFunctionPerVertex;

        // Create pipeline state
        MTLRenderPipelineDescriptor *pipelineDesc = [[MTLRenderPipelineDescriptor alloc] init];
        pipelineDesc.vertexFunction = vertexFunction;
        pipelineDesc.fragmentFunction = fragmentFunction;
        pipelineDesc.vertexDescriptor = vertexDescriptor;
        pipelineDesc.colorAttachments[0].pixelFormat = MTLPixelFormatBGRA8Unorm;

        // Enable blending for transparency
        pipelineDesc.colorAttachments[0].blendingEnabled = YES;
        pipelineDesc.colorAttachments[0].sourceRGBBlendFactor = MTLBlendFactorSourceAlpha;
        pipelineDesc.colorAttachments[0].destinationRGBBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
        pipelineDesc.colorAttachments[0].sourceAlphaBlendFactor = MTLBlendFactorOne;
        pipelineDesc.colorAttachments[0].destinationAlphaBlendFactor = MTLBlendFactorOneMinusSourceAlpha;

        renderer->pipelineState = [renderer->device newRenderPipelineStateWithDescriptor:pipelineDesc
                                                                                   error:&error];
        if (!renderer->pipelineState) {
            NSLog(@"Pipeline creation failed: %@", error);
            free(renderer);
            return AFFERENT_ERROR_PIPELINE_FAILED;
        }

        *out_renderer = renderer;
        return AFFERENT_OK;
    }
}

void afferent_renderer_destroy(AfferentRendererRef renderer) {
    if (renderer) {
        free(renderer);
    }
}

AfferentResult afferent_renderer_begin_frame(AfferentRendererRef renderer, float r, float g, float b, float a) {
    @autoreleasepool {
        CAMetalLayer *metalLayer = afferent_window_get_metal_layer(renderer->window);
        if (!metalLayer) {
            return AFFERENT_ERROR_INIT_FAILED;
        }

        renderer->currentDrawable = [metalLayer nextDrawable];
        if (!renderer->currentDrawable) {
            return AFFERENT_ERROR_INIT_FAILED;
        }

        renderer->currentCommandBuffer = [renderer->commandQueue commandBuffer];
        if (!renderer->currentCommandBuffer) {
            return AFFERENT_ERROR_INIT_FAILED;
        }

        MTLRenderPassDescriptor *passDesc = [MTLRenderPassDescriptor renderPassDescriptor];
        passDesc.colorAttachments[0].texture = renderer->currentDrawable.texture;
        passDesc.colorAttachments[0].loadAction = MTLLoadActionClear;
        passDesc.colorAttachments[0].storeAction = MTLStoreActionStore;
        passDesc.colorAttachments[0].clearColor = MTLClearColorMake(r, g, b, a);

        renderer->currentEncoder = [renderer->currentCommandBuffer renderCommandEncoderWithDescriptor:passDesc];
        if (!renderer->currentEncoder) {
            return AFFERENT_ERROR_INIT_FAILED;
        }

        [renderer->currentEncoder setRenderPipelineState:renderer->pipelineState];

        return AFFERENT_OK;
    }
}

AfferentResult afferent_renderer_end_frame(AfferentRendererRef renderer) {
    @autoreleasepool {
        if (renderer->currentEncoder) {
            [renderer->currentEncoder endEncoding];
            renderer->currentEncoder = nil;
        }

        if (renderer->currentCommandBuffer && renderer->currentDrawable) {
            [renderer->currentCommandBuffer presentDrawable:renderer->currentDrawable];
            [renderer->currentCommandBuffer commit];
        }

        renderer->currentCommandBuffer = nil;
        renderer->currentDrawable = nil;

        return AFFERENT_OK;
    }
}

AfferentResult afferent_buffer_create_vertex(
    AfferentRendererRef renderer,
    const AfferentVertex* vertices,
    uint32_t vertex_count,
    AfferentBufferRef* out_buffer
) {
    @autoreleasepool {
        struct AfferentBuffer *buffer = malloc(sizeof(struct AfferentBuffer));
        buffer->count = vertex_count;
        buffer->mtlBuffer = [renderer->device newBufferWithBytes:vertices
                                                          length:vertex_count * sizeof(AfferentVertex)
                                                         options:MTLResourceStorageModeShared];
        if (!buffer->mtlBuffer) {
            free(buffer);
            return AFFERENT_ERROR_BUFFER_FAILED;
        }
        *out_buffer = buffer;
        return AFFERENT_OK;
    }
}

AfferentResult afferent_buffer_create_index(
    AfferentRendererRef renderer,
    const uint32_t* indices,
    uint32_t index_count,
    AfferentBufferRef* out_buffer
) {
    @autoreleasepool {
        struct AfferentBuffer *buffer = malloc(sizeof(struct AfferentBuffer));
        buffer->count = index_count;
        buffer->mtlBuffer = [renderer->device newBufferWithBytes:indices
                                                          length:index_count * sizeof(uint32_t)
                                                         options:MTLResourceStorageModeShared];
        if (!buffer->mtlBuffer) {
            free(buffer);
            return AFFERENT_ERROR_BUFFER_FAILED;
        }
        *out_buffer = buffer;
        return AFFERENT_OK;
    }
}

void afferent_buffer_destroy(AfferentBufferRef buffer) {
    if (buffer) {
        free(buffer);
    }
}

void afferent_renderer_draw_triangles(
    AfferentRendererRef renderer,
    AfferentBufferRef vertex_buffer,
    AfferentBufferRef index_buffer,
    uint32_t index_count
) {
    if (!renderer->currentEncoder || !vertex_buffer || !index_buffer) {
        return;
    }

    [renderer->currentEncoder setVertexBuffer:vertex_buffer->mtlBuffer offset:0 atIndex:0];

    [renderer->currentEncoder drawIndexedPrimitives:MTLPrimitiveTypeTriangle
                                         indexCount:index_count
                                          indexType:MTLIndexTypeUInt32
                                        indexBuffer:index_buffer->mtlBuffer
                                  indexBufferOffset:0];
}

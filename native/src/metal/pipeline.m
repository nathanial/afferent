// pipeline.m - Pipeline creation and MSAA/depth texture setup
#import "render.h"
#import "shaders.h"

// Helper function to create or recreate MSAA texture if needed
void ensureMSAATexture(AfferentRendererRef renderer, NSUInteger width, NSUInteger height) {
    if (renderer->msaaTexture &&
        renderer->msaaWidth == width &&
        renderer->msaaHeight == height) {
        return;  // Already have correct size
    }

    // Create new MSAA texture
    MTLTextureDescriptor *msaaDesc = [MTLTextureDescriptor texture2DDescriptorWithPixelFormat:MTLPixelFormatBGRA8Unorm
                                                                                        width:width
                                                                                       height:height
                                                                                    mipmapped:NO];
    msaaDesc.textureType = MTLTextureType2DMultisample;
    msaaDesc.sampleCount = 4;
    msaaDesc.usage = MTLTextureUsageRenderTarget;
    msaaDesc.storageMode = MTLStorageModePrivate;  // GPU-only, no CPU access needed

    renderer->msaaTexture = [renderer->device newTextureWithDescriptor:msaaDesc];
    renderer->msaaWidth = width;
    renderer->msaaHeight = height;
}

// Helper function to create or recreate depth textures if needed
void ensureDepthTexture(AfferentRendererRef renderer, NSUInteger width, NSUInteger height, bool msaa) {
    if (renderer->depthWidth == width && renderer->depthHeight == height) {
        // Check if we have the right textures already
        if (msaa && renderer->msaaDepthTexture) return;
        if (!msaa && renderer->depthTexture) return;
    }

    // Create depth texture descriptor
    MTLTextureDescriptor *depthDesc = [MTLTextureDescriptor texture2DDescriptorWithPixelFormat:MTLPixelFormatDepth32Float
                                                                                          width:width
                                                                                         height:height
                                                                                      mipmapped:NO];
    depthDesc.usage = MTLTextureUsageRenderTarget;
    depthDesc.storageMode = MTLStorageModePrivate;

    if (msaa) {
        depthDesc.textureType = MTLTextureType2DMultisample;
        depthDesc.sampleCount = 4;
        renderer->msaaDepthTexture = [renderer->device newTextureWithDescriptor:depthDesc];
    } else {
        depthDesc.textureType = MTLTextureType2D;
        depthDesc.sampleCount = 1;
        renderer->depthTexture = [renderer->device newTextureWithDescriptor:depthDesc];
    }

    renderer->depthWidth = width;
    renderer->depthHeight = height;
}

// Create all pipelines for the renderer
AfferentResult create_pipelines(struct AfferentRenderer* renderer) {
    NSError *error = nil;

    // Compile basic shader
    id<MTLLibrary> library = [renderer->device newLibraryWithSource:shaderSource
                                                            options:nil
                                                              error:&error];
    if (!library) {
        NSLog(@"Shader compilation failed: %@", error);
        return AFFERENT_ERROR_PIPELINE_FAILED;
    }

    id<MTLFunction> vertexFunction = [library newFunctionWithName:@"vertex_main"];
    id<MTLFunction> fragmentFunction = [library newFunctionWithName:@"fragment_main"];

    if (!vertexFunction || !fragmentFunction) {
        NSLog(@"Failed to find shader functions");
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

    // Create pipeline states (MSAA + non-MSAA)
    MTLRenderPipelineDescriptor *pipelineDesc = [[MTLRenderPipelineDescriptor alloc] init];
    pipelineDesc.vertexFunction = vertexFunction;
    pipelineDesc.fragmentFunction = fragmentFunction;
    pipelineDesc.vertexDescriptor = vertexDescriptor;
    pipelineDesc.colorAttachments[0].pixelFormat = MTLPixelFormatBGRA8Unorm;
    pipelineDesc.rasterSampleCount = 4;  // Enable 4x MSAA by default

    // Enable blending for transparency
    pipelineDesc.colorAttachments[0].blendingEnabled = YES;
    pipelineDesc.colorAttachments[0].sourceRGBBlendFactor = MTLBlendFactorSourceAlpha;
    pipelineDesc.colorAttachments[0].destinationRGBBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
    pipelineDesc.colorAttachments[0].sourceAlphaBlendFactor = MTLBlendFactorOne;
    pipelineDesc.colorAttachments[0].destinationAlphaBlendFactor = MTLBlendFactorOneMinusSourceAlpha;

    renderer->pipelineStateMSAA = [renderer->device newRenderPipelineStateWithDescriptor:pipelineDesc
                                                                                   error:&error];
    if (!renderer->pipelineStateMSAA) {
        NSLog(@"Pipeline creation failed (MSAA): %@", error);
        return AFFERENT_ERROR_PIPELINE_FAILED;
    }

    pipelineDesc.rasterSampleCount = 1;
    renderer->pipelineStateNoMSAA = [renderer->device newRenderPipelineStateWithDescriptor:pipelineDesc
                                                                                     error:&error];
    if (!renderer->pipelineStateNoMSAA) {
        NSLog(@"Pipeline creation failed (no MSAA): %@", error);
        return AFFERENT_ERROR_PIPELINE_FAILED;
    }

    renderer->pipelineState = renderer->pipelineStateMSAA;
    renderer->msaaEnabled = true;

    // Create text rendering pipeline
    id<MTLLibrary> textLibrary = [renderer->device newLibraryWithSource:textShaderSource
                                                                options:nil
                                                                  error:&error];
    if (!textLibrary) {
        NSLog(@"Text shader compilation failed: %@", error);
        return AFFERENT_ERROR_PIPELINE_FAILED;
    }

    id<MTLFunction> textVertexFunction = [textLibrary newFunctionWithName:@"text_vertex_main"];
    id<MTLFunction> textFragmentFunction = [textLibrary newFunctionWithName:@"text_fragment_main"];

    if (!textVertexFunction || !textFragmentFunction) {
        NSLog(@"Failed to find text shader functions");
        return AFFERENT_ERROR_PIPELINE_FAILED;
    }

    // Create text vertex descriptor
    MTLVertexDescriptor *textVertexDescriptor = [[MTLVertexDescriptor alloc] init];

    // Position: 2 floats at offset 0
    textVertexDescriptor.attributes[0].format = MTLVertexFormatFloat2;
    textVertexDescriptor.attributes[0].offset = offsetof(TextVertex, position);
    textVertexDescriptor.attributes[0].bufferIndex = 0;

    // TexCoord: 2 floats at offset 8
    textVertexDescriptor.attributes[1].format = MTLVertexFormatFloat2;
    textVertexDescriptor.attributes[1].offset = offsetof(TextVertex, texCoord);
    textVertexDescriptor.attributes[1].bufferIndex = 0;

    // Color: 4 floats at offset 16
    textVertexDescriptor.attributes[2].format = MTLVertexFormatFloat4;
    textVertexDescriptor.attributes[2].offset = offsetof(TextVertex, color);
    textVertexDescriptor.attributes[2].bufferIndex = 0;

    // Layout
    textVertexDescriptor.layouts[0].stride = sizeof(TextVertex);
    textVertexDescriptor.layouts[0].stepFunction = MTLVertexStepFunctionPerVertex;

    // Create text pipeline states (MSAA + non-MSAA)
    MTLRenderPipelineDescriptor *textPipelineDesc = [[MTLRenderPipelineDescriptor alloc] init];
    textPipelineDesc.vertexFunction = textVertexFunction;
    textPipelineDesc.fragmentFunction = textFragmentFunction;
    textPipelineDesc.vertexDescriptor = textVertexDescriptor;
    textPipelineDesc.colorAttachments[0].pixelFormat = MTLPixelFormatBGRA8Unorm;
    textPipelineDesc.rasterSampleCount = 4;  // Match MSAA by default

    // Enable blending for text
    textPipelineDesc.colorAttachments[0].blendingEnabled = YES;
    textPipelineDesc.colorAttachments[0].sourceRGBBlendFactor = MTLBlendFactorSourceAlpha;
    textPipelineDesc.colorAttachments[0].destinationRGBBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
    textPipelineDesc.colorAttachments[0].sourceAlphaBlendFactor = MTLBlendFactorOne;
    textPipelineDesc.colorAttachments[0].destinationAlphaBlendFactor = MTLBlendFactorOneMinusSourceAlpha;

    renderer->textPipelineStateMSAA = [renderer->device newRenderPipelineStateWithDescriptor:textPipelineDesc
                                                                                       error:&error];
    if (!renderer->textPipelineStateMSAA) {
        NSLog(@"Text pipeline creation failed (MSAA): %@", error);
        return AFFERENT_ERROR_PIPELINE_FAILED;
    }

    textPipelineDesc.rasterSampleCount = 1;
    renderer->textPipelineStateNoMSAA = [renderer->device newRenderPipelineStateWithDescriptor:textPipelineDesc
                                                                                         error:&error];
    if (!renderer->textPipelineStateNoMSAA) {
        NSLog(@"Text pipeline creation failed (no MSAA): %@", error);
        return AFFERENT_ERROR_PIPELINE_FAILED;
    }

    renderer->textPipelineState = renderer->textPipelineStateMSAA;

    // Create text sampler
    MTLSamplerDescriptor *samplerDesc = [[MTLSamplerDescriptor alloc] init];
    samplerDesc.minFilter = MTLSamplerMinMagFilterLinear;
    samplerDesc.magFilter = MTLSamplerMinMagFilterLinear;
    samplerDesc.sAddressMode = MTLSamplerAddressModeClampToEdge;
    samplerDesc.tAddressMode = MTLSamplerAddressModeClampToEdge;
    renderer->textSampler = [renderer->device newSamplerStateWithDescriptor:samplerDesc];

    // Create sprite sampler (for textured sprite rendering)
    MTLSamplerDescriptor *spriteSamplerDesc = [[MTLSamplerDescriptor alloc] init];
    spriteSamplerDesc.minFilter = MTLSamplerMinMagFilterLinear;
    spriteSamplerDesc.magFilter = MTLSamplerMinMagFilterLinear;
    spriteSamplerDesc.mipFilter = MTLSamplerMipFilterLinear;
    spriteSamplerDesc.sAddressMode = MTLSamplerAddressModeClampToEdge;
    spriteSamplerDesc.tAddressMode = MTLSamplerAddressModeClampToEdge;
    renderer->spriteSampler = [renderer->device newSamplerStateWithDescriptor:spriteSamplerDesc];

    // Create instanced rendering pipeline (for GPU-accelerated rectangle batches)
    id<MTLLibrary> instancedLibrary = [renderer->device newLibraryWithSource:instancedShaderSource
                                                                     options:nil
                                                                       error:&error];
    if (!instancedLibrary) {
        NSLog(@"Instanced shader compilation failed: %@", error);
        return AFFERENT_ERROR_PIPELINE_FAILED;
    }

    id<MTLFunction> instancedVertexFunction = [instancedLibrary newFunctionWithName:@"instanced_vertex_main"];
    id<MTLFunction> instancedFragmentFunction = [instancedLibrary newFunctionWithName:@"instanced_fragment_main"];

    if (!instancedVertexFunction || !instancedFragmentFunction) {
        NSLog(@"Failed to find instanced shader functions");
        return AFFERENT_ERROR_PIPELINE_FAILED;
    }

    // Instanced pipeline - no vertex descriptor needed (we use vertex_id and instance_id)
    MTLRenderPipelineDescriptor *instancedPipelineDesc = [[MTLRenderPipelineDescriptor alloc] init];
    instancedPipelineDesc.vertexFunction = instancedVertexFunction;
    instancedPipelineDesc.fragmentFunction = instancedFragmentFunction;
    instancedPipelineDesc.colorAttachments[0].pixelFormat = MTLPixelFormatBGRA8Unorm;
    instancedPipelineDesc.rasterSampleCount = 4;  // Match MSAA

    // Enable blending
    instancedPipelineDesc.colorAttachments[0].blendingEnabled = YES;
    instancedPipelineDesc.colorAttachments[0].sourceRGBBlendFactor = MTLBlendFactorSourceAlpha;
    instancedPipelineDesc.colorAttachments[0].destinationRGBBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
    instancedPipelineDesc.colorAttachments[0].sourceAlphaBlendFactor = MTLBlendFactorOne;
    instancedPipelineDesc.colorAttachments[0].destinationAlphaBlendFactor = MTLBlendFactorOneMinusSourceAlpha;

    renderer->instancedPipelineState = [renderer->device newRenderPipelineStateWithDescriptor:instancedPipelineDesc
                                                                                        error:&error];
    if (!renderer->instancedPipelineState) {
        NSLog(@"Instanced pipeline creation failed: %@", error);
        return AFFERENT_ERROR_PIPELINE_FAILED;
    }

    // Create triangle pipeline (same library, different vertex function)
    id<MTLFunction> triangleVertexFunction = [instancedLibrary newFunctionWithName:@"instanced_triangle_vertex"];
    if (!triangleVertexFunction) {
        NSLog(@"Failed to find triangle vertex function");
        return AFFERENT_ERROR_PIPELINE_FAILED;
    }

    MTLRenderPipelineDescriptor *trianglePipelineDesc = [[MTLRenderPipelineDescriptor alloc] init];
    trianglePipelineDesc.vertexFunction = triangleVertexFunction;
    trianglePipelineDesc.fragmentFunction = instancedFragmentFunction;  // Same fragment shader
    trianglePipelineDesc.colorAttachments[0].pixelFormat = MTLPixelFormatBGRA8Unorm;
    trianglePipelineDesc.rasterSampleCount = 4;
    trianglePipelineDesc.colorAttachments[0].blendingEnabled = YES;
    trianglePipelineDesc.colorAttachments[0].sourceRGBBlendFactor = MTLBlendFactorSourceAlpha;
    trianglePipelineDesc.colorAttachments[0].destinationRGBBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
    trianglePipelineDesc.colorAttachments[0].sourceAlphaBlendFactor = MTLBlendFactorOne;
    trianglePipelineDesc.colorAttachments[0].destinationAlphaBlendFactor = MTLBlendFactorOneMinusSourceAlpha;

    renderer->trianglePipelineState = [renderer->device newRenderPipelineStateWithDescriptor:trianglePipelineDesc
                                                                                       error:&error];
    if (!renderer->trianglePipelineState) {
        NSLog(@"Triangle pipeline creation failed: %@", error);
        return AFFERENT_ERROR_PIPELINE_FAILED;
    }

    // Create circle pipeline (different vertex and fragment functions)
    id<MTLFunction> circleVertexFunction = [instancedLibrary newFunctionWithName:@"instanced_circle_vertex"];
    id<MTLFunction> circleFragmentFunction = [instancedLibrary newFunctionWithName:@"instanced_circle_fragment"];
    if (!circleVertexFunction || !circleFragmentFunction) {
        NSLog(@"Failed to find circle shader functions");
        return AFFERENT_ERROR_PIPELINE_FAILED;
    }

    MTLRenderPipelineDescriptor *circlePipelineDesc = [[MTLRenderPipelineDescriptor alloc] init];
    circlePipelineDesc.vertexFunction = circleVertexFunction;
    circlePipelineDesc.fragmentFunction = circleFragmentFunction;
    circlePipelineDesc.colorAttachments[0].pixelFormat = MTLPixelFormatBGRA8Unorm;
    circlePipelineDesc.rasterSampleCount = 4;
    circlePipelineDesc.colorAttachments[0].blendingEnabled = YES;
    circlePipelineDesc.colorAttachments[0].sourceRGBBlendFactor = MTLBlendFactorSourceAlpha;
    circlePipelineDesc.colorAttachments[0].destinationRGBBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
    circlePipelineDesc.colorAttachments[0].sourceAlphaBlendFactor = MTLBlendFactorOne;
    circlePipelineDesc.colorAttachments[0].destinationAlphaBlendFactor = MTLBlendFactorOneMinusSourceAlpha;

    renderer->circlePipelineState = [renderer->device newRenderPipelineStateWithDescriptor:circlePipelineDesc
                                                                                     error:&error];
    if (!renderer->circlePipelineState) {
        NSLog(@"Circle pipeline creation failed: %@", error);
        return AFFERENT_ERROR_PIPELINE_FAILED;
    }

    // ====================================================================
    // Create animated pipelines (GPU-side animation for maximum performance)
    // ====================================================================
    id<MTLLibrary> animatedLibrary = [renderer->device newLibraryWithSource:animatedShaderSource
                                                                    options:nil
                                                                      error:&error];
    if (!animatedLibrary) {
        NSLog(@"Animated shader compilation failed: %@", error);
        return AFFERENT_ERROR_PIPELINE_FAILED;
    }

    // Animated rect pipeline
    id<MTLFunction> animRectVertexFunc = [animatedLibrary newFunctionWithName:@"animated_rect_vertex"];
    id<MTLFunction> animRectFragmentFunc = [animatedLibrary newFunctionWithName:@"animated_rect_fragment"];
    if (!animRectVertexFunc || !animRectFragmentFunc) {
        NSLog(@"Failed to find animated rect shader functions");
        return AFFERENT_ERROR_PIPELINE_FAILED;
    }

    MTLRenderPipelineDescriptor *animRectPipelineDesc = [[MTLRenderPipelineDescriptor alloc] init];
    animRectPipelineDesc.vertexFunction = animRectVertexFunc;
    animRectPipelineDesc.fragmentFunction = animRectFragmentFunc;
    animRectPipelineDesc.colorAttachments[0].pixelFormat = MTLPixelFormatBGRA8Unorm;
    animRectPipelineDesc.rasterSampleCount = 4;
    animRectPipelineDesc.colorAttachments[0].blendingEnabled = YES;
    animRectPipelineDesc.colorAttachments[0].sourceRGBBlendFactor = MTLBlendFactorSourceAlpha;
    animRectPipelineDesc.colorAttachments[0].destinationRGBBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
    animRectPipelineDesc.colorAttachments[0].sourceAlphaBlendFactor = MTLBlendFactorOne;
    animRectPipelineDesc.colorAttachments[0].destinationAlphaBlendFactor = MTLBlendFactorOneMinusSourceAlpha;

    renderer->animatedRectPipelineState = [renderer->device newRenderPipelineStateWithDescriptor:animRectPipelineDesc
                                                                                           error:&error];
    if (!renderer->animatedRectPipelineState) {
        NSLog(@"Animated rect pipeline creation failed: %@", error);
        return AFFERENT_ERROR_PIPELINE_FAILED;
    }

    // Animated triangle pipeline
    id<MTLFunction> animTriVertexFunc = [animatedLibrary newFunctionWithName:@"animated_triangle_vertex"];
    if (!animTriVertexFunc) {
        NSLog(@"Failed to find animated triangle vertex function");
        return AFFERENT_ERROR_PIPELINE_FAILED;
    }

    MTLRenderPipelineDescriptor *animTriPipelineDesc = [[MTLRenderPipelineDescriptor alloc] init];
    animTriPipelineDesc.vertexFunction = animTriVertexFunc;
    animTriPipelineDesc.fragmentFunction = animRectFragmentFunc;  // Same fragment shader
    animTriPipelineDesc.colorAttachments[0].pixelFormat = MTLPixelFormatBGRA8Unorm;
    animTriPipelineDesc.rasterSampleCount = 4;
    animTriPipelineDesc.colorAttachments[0].blendingEnabled = YES;
    animTriPipelineDesc.colorAttachments[0].sourceRGBBlendFactor = MTLBlendFactorSourceAlpha;
    animTriPipelineDesc.colorAttachments[0].destinationRGBBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
    animTriPipelineDesc.colorAttachments[0].sourceAlphaBlendFactor = MTLBlendFactorOne;
    animTriPipelineDesc.colorAttachments[0].destinationAlphaBlendFactor = MTLBlendFactorOneMinusSourceAlpha;

    renderer->animatedTrianglePipelineState = [renderer->device newRenderPipelineStateWithDescriptor:animTriPipelineDesc
                                                                                               error:&error];
    if (!renderer->animatedTrianglePipelineState) {
        NSLog(@"Animated triangle pipeline creation failed: %@", error);
        return AFFERENT_ERROR_PIPELINE_FAILED;
    }

    // Animated circle pipeline
    id<MTLFunction> animCircleVertexFunc = [animatedLibrary newFunctionWithName:@"animated_circle_vertex"];
    id<MTLFunction> animCircleFragmentFunc = [animatedLibrary newFunctionWithName:@"animated_circle_fragment"];
    if (!animCircleVertexFunc || !animCircleFragmentFunc) {
        NSLog(@"Failed to find animated circle shader functions");
        return AFFERENT_ERROR_PIPELINE_FAILED;
    }

    MTLRenderPipelineDescriptor *animCirclePipelineDesc = [[MTLRenderPipelineDescriptor alloc] init];
    animCirclePipelineDesc.vertexFunction = animCircleVertexFunc;
    animCirclePipelineDesc.fragmentFunction = animCircleFragmentFunc;
    animCirclePipelineDesc.colorAttachments[0].pixelFormat = MTLPixelFormatBGRA8Unorm;
    animCirclePipelineDesc.rasterSampleCount = 4;
    animCirclePipelineDesc.colorAttachments[0].blendingEnabled = YES;
    animCirclePipelineDesc.colorAttachments[0].sourceRGBBlendFactor = MTLBlendFactorSourceAlpha;
    animCirclePipelineDesc.colorAttachments[0].destinationRGBBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
    animCirclePipelineDesc.colorAttachments[0].sourceAlphaBlendFactor = MTLBlendFactorOne;
    animCirclePipelineDesc.colorAttachments[0].destinationAlphaBlendFactor = MTLBlendFactorOneMinusSourceAlpha;

    renderer->animatedCirclePipelineState = [renderer->device newRenderPipelineStateWithDescriptor:animCirclePipelineDesc
                                                                                             error:&error];
    if (!renderer->animatedCirclePipelineState) {
        NSLog(@"Animated circle pipeline creation failed: %@", error);
        return AFFERENT_ERROR_PIPELINE_FAILED;
    }

    // ====================================================================
    // Create orbital pipeline (particles orbiting around a center point)
    // ====================================================================
    id<MTLLibrary> orbitalLibrary = [renderer->device newLibraryWithSource:orbitalShaderSource
                                                                   options:nil
                                                                     error:&error];
    if (!orbitalLibrary) {
        NSLog(@"Orbital shader compilation failed: %@", error);
        return AFFERENT_ERROR_PIPELINE_FAILED;
    }

    id<MTLFunction> orbitalVertexFunc = [orbitalLibrary newFunctionWithName:@"orbital_rect_vertex"];
    id<MTLFunction> orbitalFragmentFunc = [orbitalLibrary newFunctionWithName:@"orbital_rect_fragment"];
    if (!orbitalVertexFunc || !orbitalFragmentFunc) {
        NSLog(@"Failed to find orbital shader functions");
        return AFFERENT_ERROR_PIPELINE_FAILED;
    }

    MTLRenderPipelineDescriptor *orbitalPipelineDesc = [[MTLRenderPipelineDescriptor alloc] init];
    orbitalPipelineDesc.vertexFunction = orbitalVertexFunc;
    orbitalPipelineDesc.fragmentFunction = orbitalFragmentFunc;
    orbitalPipelineDesc.colorAttachments[0].pixelFormat = MTLPixelFormatBGRA8Unorm;
    orbitalPipelineDesc.rasterSampleCount = 4;
    orbitalPipelineDesc.colorAttachments[0].blendingEnabled = YES;
    orbitalPipelineDesc.colorAttachments[0].sourceRGBBlendFactor = MTLBlendFactorSourceAlpha;
    orbitalPipelineDesc.colorAttachments[0].destinationRGBBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
    orbitalPipelineDesc.colorAttachments[0].sourceAlphaBlendFactor = MTLBlendFactorOne;
    orbitalPipelineDesc.colorAttachments[0].destinationAlphaBlendFactor = MTLBlendFactorOneMinusSourceAlpha;

    renderer->orbitalPipelineState = [renderer->device newRenderPipelineStateWithDescriptor:orbitalPipelineDesc
                                                                                      error:&error];
    if (!renderer->orbitalPipelineState) {
        NSLog(@"Orbital pipeline creation failed: %@", error);
        return AFFERENT_ERROR_PIPELINE_FAILED;
    }

    // Create dynamic circle pipeline
    id<MTLLibrary> dynamicCircleLibrary = [renderer->device newLibraryWithSource:dynamicCircleShaderSource
                                                                         options:nil
                                                                           error:&error];
    if (!dynamicCircleLibrary) {
        NSLog(@"Dynamic circle shader compilation failed: %@", error);
        return AFFERENT_ERROR_PIPELINE_FAILED;
    }

    id<MTLFunction> dynamicCircleVertexFunc = [dynamicCircleLibrary newFunctionWithName:@"dynamic_circle_vertex"];
    id<MTLFunction> dynamicCircleFragmentFunc = [dynamicCircleLibrary newFunctionWithName:@"dynamic_circle_fragment"];
    if (!dynamicCircleVertexFunc || !dynamicCircleFragmentFunc) {
        NSLog(@"Failed to find dynamic circle shader functions");
        return AFFERENT_ERROR_PIPELINE_FAILED;
    }

    MTLRenderPipelineDescriptor *dynamicCirclePipelineDesc = [[MTLRenderPipelineDescriptor alloc] init];
    dynamicCirclePipelineDesc.vertexFunction = dynamicCircleVertexFunc;
    dynamicCirclePipelineDesc.fragmentFunction = dynamicCircleFragmentFunc;
    dynamicCirclePipelineDesc.colorAttachments[0].pixelFormat = MTLPixelFormatBGRA8Unorm;
    dynamicCirclePipelineDesc.rasterSampleCount = 4;
    dynamicCirclePipelineDesc.colorAttachments[0].blendingEnabled = YES;
    dynamicCirclePipelineDesc.colorAttachments[0].sourceRGBBlendFactor = MTLBlendFactorSourceAlpha;
    dynamicCirclePipelineDesc.colorAttachments[0].destinationRGBBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
    dynamicCirclePipelineDesc.colorAttachments[0].sourceAlphaBlendFactor = MTLBlendFactorOne;
    dynamicCirclePipelineDesc.colorAttachments[0].destinationAlphaBlendFactor = MTLBlendFactorOneMinusSourceAlpha;

    renderer->dynamicCirclePipelineState = [renderer->device newRenderPipelineStateWithDescriptor:dynamicCirclePipelineDesc
                                                                                            error:&error];
    if (!renderer->dynamicCirclePipelineState) {
        NSLog(@"Dynamic circle pipeline creation failed: %@", error);
        return AFFERENT_ERROR_PIPELINE_FAILED;
    }

    // Create dynamic rect pipeline
    id<MTLLibrary> dynamicRectLibrary = [renderer->device newLibraryWithSource:dynamicRectShaderSource
                                                                       options:nil
                                                                         error:&error];
    if (!dynamicRectLibrary) {
        NSLog(@"Dynamic rect shader compilation failed: %@", error);
        return AFFERENT_ERROR_PIPELINE_FAILED;
    }

    id<MTLFunction> dynamicRectVertexFunc = [dynamicRectLibrary newFunctionWithName:@"dynamic_rect_vertex"];
    id<MTLFunction> dynamicRectFragmentFunc = [dynamicRectLibrary newFunctionWithName:@"dynamic_rect_fragment"];
    if (!dynamicRectVertexFunc || !dynamicRectFragmentFunc) {
        NSLog(@"Failed to find dynamic rect shader functions");
        return AFFERENT_ERROR_PIPELINE_FAILED;
    }

    MTLRenderPipelineDescriptor *dynamicRectPipelineDesc = [[MTLRenderPipelineDescriptor alloc] init];
    dynamicRectPipelineDesc.vertexFunction = dynamicRectVertexFunc;
    dynamicRectPipelineDesc.fragmentFunction = dynamicRectFragmentFunc;
    dynamicRectPipelineDesc.colorAttachments[0].pixelFormat = MTLPixelFormatBGRA8Unorm;
    dynamicRectPipelineDesc.rasterSampleCount = 4;
    dynamicRectPipelineDesc.colorAttachments[0].blendingEnabled = YES;
    dynamicRectPipelineDesc.colorAttachments[0].sourceRGBBlendFactor = MTLBlendFactorSourceAlpha;
    dynamicRectPipelineDesc.colorAttachments[0].destinationRGBBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
    dynamicRectPipelineDesc.colorAttachments[0].sourceAlphaBlendFactor = MTLBlendFactorOne;
    dynamicRectPipelineDesc.colorAttachments[0].destinationAlphaBlendFactor = MTLBlendFactorOneMinusSourceAlpha;

    renderer->dynamicRectPipelineState = [renderer->device newRenderPipelineStateWithDescriptor:dynamicRectPipelineDesc
                                                                                          error:&error];
    if (!renderer->dynamicRectPipelineState) {
        NSLog(@"Dynamic rect pipeline creation failed: %@", error);
        return AFFERENT_ERROR_PIPELINE_FAILED;
    }

    // Create dynamic triangle pipeline
    id<MTLLibrary> dynamicTriangleLibrary = [renderer->device newLibraryWithSource:dynamicTriangleShaderSource
                                                                           options:nil
                                                                             error:&error];
    if (!dynamicTriangleLibrary) {
        NSLog(@"Dynamic triangle shader compilation failed: %@", error);
        return AFFERENT_ERROR_PIPELINE_FAILED;
    }

    id<MTLFunction> dynamicTriangleVertexFunc = [dynamicTriangleLibrary newFunctionWithName:@"dynamic_triangle_vertex"];
    id<MTLFunction> dynamicTriangleFragmentFunc = [dynamicTriangleLibrary newFunctionWithName:@"dynamic_triangle_fragment"];
    if (!dynamicTriangleVertexFunc || !dynamicTriangleFragmentFunc) {
        NSLog(@"Failed to find dynamic triangle shader functions");
        return AFFERENT_ERROR_PIPELINE_FAILED;
    }

    MTLRenderPipelineDescriptor *dynamicTrianglePipelineDesc = [[MTLRenderPipelineDescriptor alloc] init];
    dynamicTrianglePipelineDesc.vertexFunction = dynamicTriangleVertexFunc;
    dynamicTrianglePipelineDesc.fragmentFunction = dynamicTriangleFragmentFunc;
    dynamicTrianglePipelineDesc.colorAttachments[0].pixelFormat = MTLPixelFormatBGRA8Unorm;
    dynamicTrianglePipelineDesc.rasterSampleCount = 4;
    dynamicTrianglePipelineDesc.colorAttachments[0].blendingEnabled = YES;
    dynamicTrianglePipelineDesc.colorAttachments[0].sourceRGBBlendFactor = MTLBlendFactorSourceAlpha;
    dynamicTrianglePipelineDesc.colorAttachments[0].destinationRGBBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
    dynamicTrianglePipelineDesc.colorAttachments[0].sourceAlphaBlendFactor = MTLBlendFactorOne;
    dynamicTrianglePipelineDesc.colorAttachments[0].destinationAlphaBlendFactor = MTLBlendFactorOneMinusSourceAlpha;

    renderer->dynamicTrianglePipelineState = [renderer->device newRenderPipelineStateWithDescriptor:dynamicTrianglePipelineDesc
                                                                                              error:&error];
    if (!renderer->dynamicTrianglePipelineState) {
        NSLog(@"Dynamic triangle pipeline creation failed: %@", error);
        return AFFERENT_ERROR_PIPELINE_FAILED;
    }

    // Create sprite pipeline (textured quads)
    id<MTLLibrary> spriteLibrary = [renderer->device newLibraryWithSource:spriteShaderSource
                                                                  options:nil
                                                                    error:&error];
    if (!spriteLibrary) {
        NSLog(@"Sprite shader compilation failed: %@", error);
        return AFFERENT_ERROR_PIPELINE_FAILED;
    }

    id<MTLFunction> spriteVertexFunc = [spriteLibrary newFunctionWithName:@"sprite_vertex"];
    id<MTLFunction> spriteFragmentFunc = [spriteLibrary newFunctionWithName:@"sprite_fragment"];
    if (!spriteVertexFunc || !spriteFragmentFunc) {
        NSLog(@"Failed to find sprite shader functions");
        return AFFERENT_ERROR_PIPELINE_FAILED;
    }

    MTLRenderPipelineDescriptor *spritePipelineDesc = [[MTLRenderPipelineDescriptor alloc] init];
    spritePipelineDesc.vertexFunction = spriteVertexFunc;
    spritePipelineDesc.fragmentFunction = spriteFragmentFunc;
    spritePipelineDesc.colorAttachments[0].pixelFormat = MTLPixelFormatBGRA8Unorm;
    spritePipelineDesc.rasterSampleCount = 4;
    spritePipelineDesc.colorAttachments[0].blendingEnabled = YES;
    spritePipelineDesc.colorAttachments[0].sourceRGBBlendFactor = MTLBlendFactorSourceAlpha;
    spritePipelineDesc.colorAttachments[0].destinationRGBBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
    spritePipelineDesc.colorAttachments[0].sourceAlphaBlendFactor = MTLBlendFactorOne;
    spritePipelineDesc.colorAttachments[0].destinationAlphaBlendFactor = MTLBlendFactorOneMinusSourceAlpha;

    renderer->spritePipelineStateMSAA = [renderer->device newRenderPipelineStateWithDescriptor:spritePipelineDesc
                                                                                         error:&error];
    if (!renderer->spritePipelineStateMSAA) {
        NSLog(@"Sprite pipeline creation failed (MSAA): %@", error);
        return AFFERENT_ERROR_PIPELINE_FAILED;
    }

    spritePipelineDesc.rasterSampleCount = 1;
    renderer->spritePipelineStateNoMSAA = [renderer->device newRenderPipelineStateWithDescriptor:spritePipelineDesc
                                                                                           error:&error];
    if (!renderer->spritePipelineStateNoMSAA) {
        NSLog(@"Sprite pipeline creation failed (no MSAA): %@", error);
        return AFFERENT_ERROR_PIPELINE_FAILED;
    }

    renderer->spritePipelineState = renderer->spritePipelineStateMSAA;

    // ====================================================================
    // Create depth stencil state for 3D rendering
    // ====================================================================
    MTLDepthStencilDescriptor *depthStateDesc = [[MTLDepthStencilDescriptor alloc] init];
    depthStateDesc.depthCompareFunction = MTLCompareFunctionLess;
    depthStateDesc.depthWriteEnabled = YES;
    renderer->depthState = [renderer->device newDepthStencilStateWithDescriptor:depthStateDesc];

    // Create depth stencil state with depth testing disabled (for 2D after 3D)
    MTLDepthStencilDescriptor *depthDisabledDesc = [[MTLDepthStencilDescriptor alloc] init];
    depthDisabledDesc.depthCompareFunction = MTLCompareFunctionAlways;
    depthDisabledDesc.depthWriteEnabled = NO;
    renderer->depthStateDisabled = [renderer->device newDepthStencilStateWithDescriptor:depthDisabledDesc];

    renderer->depthStateOcean = nil;

    // ====================================================================
    // Create 3D rendering pipeline
    // ====================================================================
    id<MTLLibrary> library3D = [renderer->device newLibraryWithSource:shader3DSource
                                                              options:nil
                                                                error:&error];
    if (!library3D) {
        NSLog(@"3D shader compilation failed: %@", error);
        return AFFERENT_ERROR_PIPELINE_FAILED;
    }

    id<MTLFunction> vertex3DFunction = [library3D newFunctionWithName:@"vertex_main_3d"];
    id<MTLFunction> vertexOceanFunction = [library3D newFunctionWithName:@"vertex_ocean_projected_waves"];
    id<MTLFunction> fragment3DFunction = [library3D newFunctionWithName:@"fragment_main_3d"];

    if (!vertex3DFunction || !vertexOceanFunction || !fragment3DFunction) {
        NSLog(@"Failed to find 3D shader functions");
        return AFFERENT_ERROR_PIPELINE_FAILED;
    }

    // Create 3D vertex descriptor
    MTLVertexDescriptor *vertex3DDescriptor = [[MTLVertexDescriptor alloc] init];

    // Position: 3 floats at offset 0
    vertex3DDescriptor.attributes[0].format = MTLVertexFormatFloat3;
    vertex3DDescriptor.attributes[0].offset = 0;
    vertex3DDescriptor.attributes[0].bufferIndex = 0;

    // Normal: 3 floats at offset 12
    vertex3DDescriptor.attributes[1].format = MTLVertexFormatFloat3;
    vertex3DDescriptor.attributes[1].offset = 12;
    vertex3DDescriptor.attributes[1].bufferIndex = 0;

    // Color: 4 floats at offset 24
    vertex3DDescriptor.attributes[2].format = MTLVertexFormatFloat4;
    vertex3DDescriptor.attributes[2].offset = 24;
    vertex3DDescriptor.attributes[2].bufferIndex = 0;

    // Layout: 40 bytes per vertex (3+3+4 floats)
    vertex3DDescriptor.layouts[0].stride = 40;
    vertex3DDescriptor.layouts[0].stepFunction = MTLVertexStepFunctionPerVertex;

    MTLRenderPipelineDescriptor *pipeline3DDesc = [[MTLRenderPipelineDescriptor alloc] init];
    pipeline3DDesc.vertexFunction = vertex3DFunction;
    pipeline3DDesc.fragmentFunction = fragment3DFunction;
    pipeline3DDesc.vertexDescriptor = vertex3DDescriptor;
    pipeline3DDesc.colorAttachments[0].pixelFormat = MTLPixelFormatBGRA8Unorm;
    pipeline3DDesc.depthAttachmentPixelFormat = MTLPixelFormatDepth32Float;

    // Enable blending for transparency
    pipeline3DDesc.colorAttachments[0].blendingEnabled = YES;
    pipeline3DDesc.colorAttachments[0].sourceRGBBlendFactor = MTLBlendFactorSourceAlpha;
    pipeline3DDesc.colorAttachments[0].destinationRGBBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
    pipeline3DDesc.colorAttachments[0].sourceAlphaBlendFactor = MTLBlendFactorOne;
    pipeline3DDesc.colorAttachments[0].destinationAlphaBlendFactor = MTLBlendFactorOneMinusSourceAlpha;

    pipeline3DDesc.rasterSampleCount = 4;  // MSAA
    renderer->pipeline3DMSAA = [renderer->device newRenderPipelineStateWithDescriptor:pipeline3DDesc
                                                                                error:&error];
    if (!renderer->pipeline3DMSAA) {
        NSLog(@"3D pipeline creation failed (MSAA): %@", error);
        return AFFERENT_ERROR_PIPELINE_FAILED;
    }

    pipeline3DDesc.rasterSampleCount = 1;  // No MSAA
    renderer->pipeline3DNoMSAA = [renderer->device newRenderPipelineStateWithDescriptor:pipeline3DDesc
                                                                                  error:&error];
    if (!renderer->pipeline3DNoMSAA) {
        NSLog(@"3D pipeline creation failed (no MSAA): %@", error);
        return AFFERENT_ERROR_PIPELINE_FAILED;
    }

    renderer->pipeline3D = renderer->pipeline3DMSAA;

    // ====================================================================
    // Create projected-grid ocean pipeline (procedural vertices via vertex_id)
    // ====================================================================
    MTLRenderPipelineDescriptor *pipelineOceanDesc = [[MTLRenderPipelineDescriptor alloc] init];
    pipelineOceanDesc.vertexFunction = vertexOceanFunction;
    pipelineOceanDesc.fragmentFunction = fragment3DFunction;
    pipelineOceanDesc.vertexDescriptor = nil;
    pipelineOceanDesc.colorAttachments[0].pixelFormat = MTLPixelFormatBGRA8Unorm;
    pipelineOceanDesc.depthAttachmentPixelFormat = MTLPixelFormatDepth32Float;

    pipelineOceanDesc.colorAttachments[0].blendingEnabled = YES;
    pipelineOceanDesc.colorAttachments[0].sourceRGBBlendFactor = MTLBlendFactorSourceAlpha;
    pipelineOceanDesc.colorAttachments[0].destinationRGBBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
    pipelineOceanDesc.colorAttachments[0].sourceAlphaBlendFactor = MTLBlendFactorOne;
    pipelineOceanDesc.colorAttachments[0].destinationAlphaBlendFactor = MTLBlendFactorOneMinusSourceAlpha;

    pipelineOceanDesc.rasterSampleCount = 4;  // MSAA
    renderer->pipeline3DOceanMSAA = [renderer->device newRenderPipelineStateWithDescriptor:pipelineOceanDesc
                                                                                     error:&error];
    if (!renderer->pipeline3DOceanMSAA) {
        NSLog(@"Ocean pipeline creation failed (MSAA): %@", error);
        return AFFERENT_ERROR_PIPELINE_FAILED;
    }

    pipelineOceanDesc.rasterSampleCount = 1;  // No MSAA
    renderer->pipeline3DOceanNoMSAA = [renderer->device newRenderPipelineStateWithDescriptor:pipelineOceanDesc
                                                                                       error:&error];
    if (!renderer->pipeline3DOceanNoMSAA) {
        NSLog(@"Ocean pipeline creation failed (no MSAA): %@", error);
        return AFFERENT_ERROR_PIPELINE_FAILED;
    }

    renderer->pipeline3DOcean = renderer->pipeline3DOceanMSAA;

    // ====================================================================
    // Create textured 3D rendering pipeline (for loaded assets)
    // ====================================================================
    id<MTLLibrary> library3DTextured = [renderer->device newLibraryWithSource:shader3DTexturedSource
                                                                       options:nil
                                                                         error:&error];
    if (!library3DTextured) {
        NSLog(@"Textured 3D shader compilation failed: %@", error);
        return AFFERENT_ERROR_PIPELINE_FAILED;
    }

    id<MTLFunction> vertex3DTexturedFunction = [library3DTextured newFunctionWithName:@"vertex_main_3d_textured"];
    id<MTLFunction> fragment3DTexturedFunction = [library3DTextured newFunctionWithName:@"fragment_main_3d_textured"];

    if (!vertex3DTexturedFunction || !fragment3DTexturedFunction) {
        NSLog(@"Failed to find textured 3D shader functions");
        return AFFERENT_ERROR_PIPELINE_FAILED;
    }

    // Create textured 3D vertex descriptor
    // 12 floats per vertex: position(3) + normal(3) + uv(2) + color(4) = 48 bytes
    MTLVertexDescriptor *vertex3DTexturedDescriptor = [[MTLVertexDescriptor alloc] init];

    // Position: 3 floats at offset 0
    vertex3DTexturedDescriptor.attributes[0].format = MTLVertexFormatFloat3;
    vertex3DTexturedDescriptor.attributes[0].offset = 0;
    vertex3DTexturedDescriptor.attributes[0].bufferIndex = 0;

    // Normal: 3 floats at offset 12
    vertex3DTexturedDescriptor.attributes[1].format = MTLVertexFormatFloat3;
    vertex3DTexturedDescriptor.attributes[1].offset = 12;
    vertex3DTexturedDescriptor.attributes[1].bufferIndex = 0;

    // UV: 2 floats at offset 24
    vertex3DTexturedDescriptor.attributes[2].format = MTLVertexFormatFloat2;
    vertex3DTexturedDescriptor.attributes[2].offset = 24;
    vertex3DTexturedDescriptor.attributes[2].bufferIndex = 0;

    // Color: 4 floats at offset 32
    vertex3DTexturedDescriptor.attributes[3].format = MTLVertexFormatFloat4;
    vertex3DTexturedDescriptor.attributes[3].offset = 32;
    vertex3DTexturedDescriptor.attributes[3].bufferIndex = 0;

    // Layout: 48 bytes per vertex (3+3+2+4 floats = 12 floats)
    vertex3DTexturedDescriptor.layouts[0].stride = 48;
    vertex3DTexturedDescriptor.layouts[0].stepFunction = MTLVertexStepFunctionPerVertex;

    MTLRenderPipelineDescriptor *pipeline3DTexturedDesc = [[MTLRenderPipelineDescriptor alloc] init];
    pipeline3DTexturedDesc.vertexFunction = vertex3DTexturedFunction;
    pipeline3DTexturedDesc.fragmentFunction = fragment3DTexturedFunction;
    pipeline3DTexturedDesc.vertexDescriptor = vertex3DTexturedDescriptor;
    pipeline3DTexturedDesc.colorAttachments[0].pixelFormat = MTLPixelFormatBGRA8Unorm;
    pipeline3DTexturedDesc.depthAttachmentPixelFormat = MTLPixelFormatDepth32Float;

    // Enable blending for transparency
    pipeline3DTexturedDesc.colorAttachments[0].blendingEnabled = YES;
    pipeline3DTexturedDesc.colorAttachments[0].sourceRGBBlendFactor = MTLBlendFactorSourceAlpha;
    pipeline3DTexturedDesc.colorAttachments[0].destinationRGBBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
    pipeline3DTexturedDesc.colorAttachments[0].sourceAlphaBlendFactor = MTLBlendFactorOne;
    pipeline3DTexturedDesc.colorAttachments[0].destinationAlphaBlendFactor = MTLBlendFactorOneMinusSourceAlpha;

    pipeline3DTexturedDesc.rasterSampleCount = 4;  // MSAA
    renderer->pipeline3DTexturedMSAA = [renderer->device newRenderPipelineStateWithDescriptor:pipeline3DTexturedDesc
                                                                                         error:&error];
    if (!renderer->pipeline3DTexturedMSAA) {
        NSLog(@"Textured 3D pipeline creation failed (MSAA): %@", error);
        return AFFERENT_ERROR_PIPELINE_FAILED;
    }

    pipeline3DTexturedDesc.rasterSampleCount = 1;  // No MSAA
    renderer->pipeline3DTexturedNoMSAA = [renderer->device newRenderPipelineStateWithDescriptor:pipeline3DTexturedDesc
                                                                                           error:&error];
    if (!renderer->pipeline3DTexturedNoMSAA) {
        NSLog(@"Textured 3D pipeline creation failed (no MSAA): %@", error);
        return AFFERENT_ERROR_PIPELINE_FAILED;
    }

    renderer->pipeline3DTextured = renderer->pipeline3DTexturedMSAA;

    // Create textured mesh sampler
    MTLSamplerDescriptor *texturedMeshSamplerDesc = [[MTLSamplerDescriptor alloc] init];
    texturedMeshSamplerDesc.minFilter = MTLSamplerMinMagFilterLinear;
    texturedMeshSamplerDesc.magFilter = MTLSamplerMinMagFilterLinear;
    texturedMeshSamplerDesc.mipFilter = MTLSamplerMipFilterNotMipmapped;
    texturedMeshSamplerDesc.sAddressMode = MTLSamplerAddressModeRepeat;
    texturedMeshSamplerDesc.tAddressMode = MTLSamplerAddressModeRepeat;
    renderer->texturedMeshSampler = [renderer->device newSamplerStateWithDescriptor:texturedMeshSamplerDesc];

    return AFFERENT_OK;
}

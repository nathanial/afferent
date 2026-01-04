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

    // Create stroke rendering pipeline (screen-space extrusion)
    id<MTLLibrary> strokeLibrary = [renderer->device newLibraryWithSource:strokeShaderSource
                                                                  options:nil
                                                                    error:&error];
    if (!strokeLibrary) {
        NSLog(@"Stroke shader compilation failed: %@", error);
        return AFFERENT_ERROR_PIPELINE_FAILED;
    }

    id<MTLFunction> strokeVertexFunction = [strokeLibrary newFunctionWithName:@"stroke_vertex_main"];
    id<MTLFunction> strokeFragmentFunction = [strokeLibrary newFunctionWithName:@"stroke_fragment_main"];

    if (!strokeVertexFunction || !strokeFragmentFunction) {
        NSLog(@"Failed to find stroke shader functions");
        return AFFERENT_ERROR_PIPELINE_FAILED;
    }

    MTLVertexDescriptor *strokeVertexDescriptor = [[MTLVertexDescriptor alloc] init];

    // Position: 2 floats at offset 0
    strokeVertexDescriptor.attributes[0].format = MTLVertexFormatFloat2;
    strokeVertexDescriptor.attributes[0].offset = offsetof(AfferentStrokeVertex, position);
    strokeVertexDescriptor.attributes[0].bufferIndex = 0;

    // Normal: 2 floats after position
    strokeVertexDescriptor.attributes[1].format = MTLVertexFormatFloat2;
    strokeVertexDescriptor.attributes[1].offset = offsetof(AfferentStrokeVertex, normal);
    strokeVertexDescriptor.attributes[1].bufferIndex = 0;

    // Side: 1 float after normal
    strokeVertexDescriptor.attributes[2].format = MTLVertexFormatFloat;
    strokeVertexDescriptor.attributes[2].offset = offsetof(AfferentStrokeVertex, side);
    strokeVertexDescriptor.attributes[2].bufferIndex = 0;

    strokeVertexDescriptor.layouts[0].stride = sizeof(AfferentStrokeVertex);
    strokeVertexDescriptor.layouts[0].stepFunction = MTLVertexStepFunctionPerVertex;

    MTLRenderPipelineDescriptor *strokePipelineDesc = [[MTLRenderPipelineDescriptor alloc] init];
    strokePipelineDesc.vertexFunction = strokeVertexFunction;
    strokePipelineDesc.fragmentFunction = strokeFragmentFunction;
    strokePipelineDesc.vertexDescriptor = strokeVertexDescriptor;
    strokePipelineDesc.colorAttachments[0].pixelFormat = MTLPixelFormatBGRA8Unorm;
    strokePipelineDesc.rasterSampleCount = 4;

    // Enable blending for transparency
    strokePipelineDesc.colorAttachments[0].blendingEnabled = YES;
    strokePipelineDesc.colorAttachments[0].sourceRGBBlendFactor = MTLBlendFactorSourceAlpha;
    strokePipelineDesc.colorAttachments[0].destinationRGBBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
    strokePipelineDesc.colorAttachments[0].sourceAlphaBlendFactor = MTLBlendFactorOne;
    strokePipelineDesc.colorAttachments[0].destinationAlphaBlendFactor = MTLBlendFactorOneMinusSourceAlpha;

    renderer->strokePipelineStateMSAA = [renderer->device newRenderPipelineStateWithDescriptor:strokePipelineDesc
                                                                                          error:&error];
    if (!renderer->strokePipelineStateMSAA) {
        NSLog(@"Stroke pipeline creation failed (MSAA): %@", error);
        return AFFERENT_ERROR_PIPELINE_FAILED;
    }

    strokePipelineDesc.rasterSampleCount = 1;
    renderer->strokePipelineStateNoMSAA = [renderer->device newRenderPipelineStateWithDescriptor:strokePipelineDesc
                                                                                            error:&error];
    if (!renderer->strokePipelineStateNoMSAA) {
        NSLog(@"Stroke pipeline creation failed (no MSAA): %@", error);
        return AFFERENT_ERROR_PIPELINE_FAILED;
    }

    renderer->strokePipelineState = renderer->strokePipelineStateMSAA;

    // Create stroke path rendering pipeline (segment-based GPU extrusion)
    id<MTLLibrary> strokePathLibrary = [renderer->device newLibraryWithSource:strokePathShaderSource
                                                                       options:nil
                                                                         error:&error];
    if (!strokePathLibrary) {
        NSLog(@"Stroke path shader compilation failed: %@", error);
        return AFFERENT_ERROR_PIPELINE_FAILED;
    }

    id<MTLFunction> strokePathVertexFunction = [strokePathLibrary newFunctionWithName:@"stroke_path_vertex_main"];
    id<MTLFunction> strokePathFragmentFunction = [strokePathLibrary newFunctionWithName:@"stroke_path_fragment_main"];

    if (!strokePathVertexFunction || !strokePathFragmentFunction) {
        NSLog(@"Failed to find stroke path shader functions");
        return AFFERENT_ERROR_PIPELINE_FAILED;
    }

    MTLRenderPipelineDescriptor *strokePathPipelineDesc = [[MTLRenderPipelineDescriptor alloc] init];
    strokePathPipelineDesc.vertexFunction = strokePathVertexFunction;
    strokePathPipelineDesc.fragmentFunction = strokePathFragmentFunction;
    strokePathPipelineDesc.colorAttachments[0].pixelFormat = MTLPixelFormatBGRA8Unorm;
    strokePathPipelineDesc.rasterSampleCount = 4;

    // Enable blending for transparency
    strokePathPipelineDesc.colorAttachments[0].blendingEnabled = YES;
    strokePathPipelineDesc.colorAttachments[0].sourceRGBBlendFactor = MTLBlendFactorSourceAlpha;
    strokePathPipelineDesc.colorAttachments[0].destinationRGBBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
    strokePathPipelineDesc.colorAttachments[0].sourceAlphaBlendFactor = MTLBlendFactorOne;
    strokePathPipelineDesc.colorAttachments[0].destinationAlphaBlendFactor = MTLBlendFactorOneMinusSourceAlpha;

    renderer->strokePathPipelineStateMSAA = [renderer->device newRenderPipelineStateWithDescriptor:strokePathPipelineDesc
                                                                                            error:&error];
    if (!renderer->strokePathPipelineStateMSAA) {
        NSLog(@"Stroke path pipeline creation failed (MSAA): %@", error);
        return AFFERENT_ERROR_PIPELINE_FAILED;
    }

    strokePathPipelineDesc.rasterSampleCount = 1;
    renderer->strokePathPipelineStateNoMSAA = [renderer->device newRenderPipelineStateWithDescriptor:strokePathPipelineDesc
                                                                                              error:&error];
    if (!renderer->strokePathPipelineStateNoMSAA) {
        NSLog(@"Stroke path pipeline creation failed (no MSAA): %@", error);
        return AFFERENT_ERROR_PIPELINE_FAILED;
    }

    renderer->strokePathPipelineState = renderer->strokePathPipelineStateMSAA;

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

    // Create sprite pipeline (textured quads)
    id<MTLLibrary> spriteLibrary = [renderer->device newLibraryWithSource:spriteShaderSource
                                                                  options:nil
                                                                    error:&error];
    if (!spriteLibrary) {
        NSLog(@"Sprite shader compilation failed: %@", error);
        return AFFERENT_ERROR_PIPELINE_FAILED;
    }

    id<MTLFunction> spriteVertexFunc = [spriteLibrary newFunctionWithName:@"sprite_vertex_layout0"];
    id<MTLFunction> texturedSpriteVertexFunc = [spriteLibrary newFunctionWithName:@"sprite_vertex_layout1"];
    id<MTLFunction> spriteFragmentFunc = [spriteLibrary newFunctionWithName:@"sprite_fragment"];
    if (!spriteVertexFunc || !texturedSpriteVertexFunc || !spriteFragmentFunc) {
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

    MTLRenderPipelineDescriptor *texturedSpritePipelineDesc = [[MTLRenderPipelineDescriptor alloc] init];
    texturedSpritePipelineDesc.vertexFunction = texturedSpriteVertexFunc;
    texturedSpritePipelineDesc.fragmentFunction = spriteFragmentFunc;
    texturedSpritePipelineDesc.colorAttachments[0].pixelFormat = MTLPixelFormatBGRA8Unorm;
    texturedSpritePipelineDesc.rasterSampleCount = 4;
    texturedSpritePipelineDesc.colorAttachments[0].blendingEnabled = YES;
    texturedSpritePipelineDesc.colorAttachments[0].sourceRGBBlendFactor = MTLBlendFactorSourceAlpha;
    texturedSpritePipelineDesc.colorAttachments[0].destinationRGBBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
    texturedSpritePipelineDesc.colorAttachments[0].sourceAlphaBlendFactor = MTLBlendFactorOne;
    texturedSpritePipelineDesc.colorAttachments[0].destinationAlphaBlendFactor = MTLBlendFactorOneMinusSourceAlpha;

    renderer->texturedSpritePipelineStateMSAA = [renderer->device newRenderPipelineStateWithDescriptor:texturedSpritePipelineDesc
                                                                                                 error:&error];
    if (!renderer->texturedSpritePipelineStateMSAA) {
        NSLog(@"Textured sprite pipeline creation failed (MSAA): %@", error);
        return AFFERENT_ERROR_PIPELINE_FAILED;
    }

    texturedSpritePipelineDesc.rasterSampleCount = 1;
    renderer->texturedSpritePipelineStateNoMSAA = [renderer->device newRenderPipelineStateWithDescriptor:texturedSpritePipelineDesc
                                                                                                   error:&error];
    if (!renderer->texturedSpritePipelineStateNoMSAA) {
        NSLog(@"Textured sprite pipeline creation failed (no MSAA): %@", error);
        return AFFERENT_ERROR_PIPELINE_FAILED;
    }

    renderer->texturedSpritePipelineState = renderer->texturedSpritePipelineStateMSAA;

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
    id<MTLFunction> vertex3DTexturedFunction = [library3D newFunctionWithName:@"vertex_main_3d_textured"];
    id<MTLFunction> fragment3DFunction = [library3D newFunctionWithName:@"fragment_main_3d"];

    if (!vertex3DFunction || !vertexOceanFunction || !vertex3DTexturedFunction || !fragment3DFunction) {
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
    pipeline3DTexturedDesc.fragmentFunction = fragment3DFunction;
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

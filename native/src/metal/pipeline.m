// pipeline.m - Pipeline creation and depth texture setup
#import "render.h"
#import "shaders.h"

static void apply_alpha_blend(MTLRenderPipelineColorAttachmentDescriptor *color) {
    color.blendingEnabled = YES;
    color.sourceRGBBlendFactor = MTLBlendFactorSourceAlpha;
    color.destinationRGBBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
    color.sourceAlphaBlendFactor = MTLBlendFactorOne;
    color.destinationAlphaBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
}

static id<MTLRenderPipelineState> build_pipeline(
    id<MTLDevice> device,
    MTLRenderPipelineDescriptor *desc,
    const char *label,
    NSError **error
) {
    desc.rasterSampleCount = AFFERENT_MSAA_SAMPLE_COUNT;
    id<MTLRenderPipelineState> pipeline = [device newRenderPipelineStateWithDescriptor:desc error:error];
    if (!pipeline) {
        NSLog(@"%s pipeline creation failed: %@", label, *error);
    }
    return pipeline;
}

// Helper function to create or recreate depth textures if needed
void ensureDepthTexture(AfferentRendererRef renderer, NSUInteger width, NSUInteger height) {
    if (renderer->depthWidth == width && renderer->depthHeight == height) {
        if (renderer->depthTexture) return;
    }

    // Create depth texture descriptor
    MTLTextureDescriptor *depthDesc = [MTLTextureDescriptor texture2DDescriptorWithPixelFormat:MTLPixelFormatDepth32Float
                                                                                          width:width
                                                                                         height:height
                                                                                      mipmapped:NO];
    depthDesc.usage = MTLTextureUsageRenderTarget;
    depthDesc.storageMode = MTLStorageModePrivate;
    depthDesc.textureType = MTLTextureType2DMultisample;
    depthDesc.sampleCount = AFFERENT_MSAA_SAMPLE_COUNT;
    renderer->depthTexture = [renderer->device newTextureWithDescriptor:depthDesc];

    renderer->depthWidth = width;
    renderer->depthHeight = height;
}

// Helper function to create or recreate MSAA color textures if needed
void ensureMSAATexture(AfferentRendererRef renderer, NSUInteger width, NSUInteger height) {
    if (renderer->msaaWidth == width && renderer->msaaHeight == height) {
        if (renderer->msaaColorTexture) return;
    }

    MTLTextureDescriptor *colorDesc = [MTLTextureDescriptor texture2DDescriptorWithPixelFormat:MTLPixelFormatBGRA8Unorm
                                                                                         width:width
                                                                                        height:height
                                                                                     mipmapped:NO];
    colorDesc.usage = MTLTextureUsageRenderTarget;
    colorDesc.storageMode = MTLStorageModePrivate;
    colorDesc.textureType = MTLTextureType2DMultisample;
    colorDesc.sampleCount = AFFERENT_MSAA_SAMPLE_COUNT;
    renderer->msaaColorTexture = [renderer->device newTextureWithDescriptor:colorDesc];

    renderer->msaaWidth = width;
    renderer->msaaHeight = height;
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

    // Create pipeline state
    MTLRenderPipelineDescriptor *pipelineDesc = [[MTLRenderPipelineDescriptor alloc] init];
    pipelineDesc.vertexFunction = vertexFunction;
    pipelineDesc.fragmentFunction = fragmentFunction;
    pipelineDesc.vertexDescriptor = vertexDescriptor;
    pipelineDesc.colorAttachments[0].pixelFormat = MTLPixelFormatBGRA8Unorm;
    apply_alpha_blend(pipelineDesc.colorAttachments[0]);

    renderer->pipelineState = build_pipeline(
        renderer->device,
        pipelineDesc,
        "Basic",
        &error
    );
    if (!renderer->pipelineState) return AFFERENT_ERROR_PIPELINE_FAILED;

    // Create screen-coords triangle pipeline (GPU-side NDC conversion)
    id<MTLFunction> screenCoordsVertexFunc = [library newFunctionWithName:@"vertex_screen_coords"];
    if (!screenCoordsVertexFunc) {
        NSLog(@"Failed to find vertex_screen_coords function");
        return AFFERENT_ERROR_PIPELINE_FAILED;
    }

    MTLRenderPipelineDescriptor *screenCoordsPipelineDesc = [[MTLRenderPipelineDescriptor alloc] init];
    screenCoordsPipelineDesc.vertexFunction = screenCoordsVertexFunc;
    screenCoordsPipelineDesc.fragmentFunction = fragmentFunction;  // Reuse fragment_main
    screenCoordsPipelineDesc.colorAttachments[0].pixelFormat = MTLPixelFormatBGRA8Unorm;
    apply_alpha_blend(screenCoordsPipelineDesc.colorAttachments[0]);

    renderer->screenCoordsPipelineState = build_pipeline(
        renderer->device,
        screenCoordsPipelineDesc,
        "ScreenCoords",
        &error
    );
    if (!renderer->screenCoordsPipelineState) return AFFERENT_ERROR_PIPELINE_FAILED;

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
    apply_alpha_blend(strokePipelineDesc.colorAttachments[0]);

    renderer->strokePipelineState = build_pipeline(
        renderer->device,
        strokePipelineDesc,
        "Stroke",
        &error
    );
    if (!renderer->strokePipelineState) return AFFERENT_ERROR_PIPELINE_FAILED;

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
    apply_alpha_blend(strokePathPipelineDesc.colorAttachments[0]);

    renderer->strokePathPipelineState = build_pipeline(
        renderer->device,
        strokePathPipelineDesc,
        "Stroke path",
        &error
    );
    if (!renderer->strokePathPipelineState) return AFFERENT_ERROR_PIPELINE_FAILED;

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

    // Create text pipeline state
    MTLRenderPipelineDescriptor *textPipelineDesc = [[MTLRenderPipelineDescriptor alloc] init];
    textPipelineDesc.vertexFunction = textVertexFunction;
    textPipelineDesc.fragmentFunction = textFragmentFunction;
    textPipelineDesc.vertexDescriptor = textVertexDescriptor;
    textPipelineDesc.colorAttachments[0].pixelFormat = MTLPixelFormatBGRA8Unorm;
    apply_alpha_blend(textPipelineDesc.colorAttachments[0]);

    renderer->textPipelineState = build_pipeline(
        renderer->device,
        textPipelineDesc,
        "Text",
        &error
    );
    if (!renderer->textPipelineState) return AFFERENT_ERROR_PIPELINE_FAILED;

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

    // Create instanced rendering pipeline (for GPU-accelerated shapes)
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
    apply_alpha_blend(instancedPipelineDesc.colorAttachments[0]);

    renderer->instancedPipelineState = build_pipeline(
        renderer->device,
        instancedPipelineDesc,
        "Instanced",
        &error
    );
    if (!renderer->instancedPipelineState) {
        return AFFERENT_ERROR_PIPELINE_FAILED;
    }

    // Create batched shapes pipeline (rects, circles, stroke rects)
    id<MTLFunction> batchedVertexFunc = [instancedLibrary newFunctionWithName:@"batched_vertex"];
    id<MTLFunction> batchedFragmentFunc = [instancedLibrary newFunctionWithName:@"batched_fragment"];
    if (!batchedVertexFunc || !batchedFragmentFunc) {
        NSLog(@"Failed to find batched shader functions");
        return AFFERENT_ERROR_PIPELINE_FAILED;
    }

    MTLRenderPipelineDescriptor *batchedPipelineDesc = [[MTLRenderPipelineDescriptor alloc] init];
    batchedPipelineDesc.vertexFunction = batchedVertexFunc;
    batchedPipelineDesc.fragmentFunction = batchedFragmentFunc;
    batchedPipelineDesc.colorAttachments[0].pixelFormat = MTLPixelFormatBGRA8Unorm;
    apply_alpha_blend(batchedPipelineDesc.colorAttachments[0]);

    renderer->batchedPipelineState = build_pipeline(
        renderer->device,
        batchedPipelineDesc,
        "Batched",
        &error
    );
    if (!renderer->batchedPipelineState) return AFFERENT_ERROR_PIPELINE_FAILED;

    // Create mesh instanced pipeline (for complex polygons like gears)
    id<MTLFunction> meshInstancedVertexFunc = [instancedLibrary newFunctionWithName:@"mesh_instanced_vertex"];
    id<MTLFunction> meshInstancedFragmentFunc = [instancedLibrary newFunctionWithName:@"mesh_instanced_fragment"];
    if (!meshInstancedVertexFunc || !meshInstancedFragmentFunc) {
        NSLog(@"Failed to find mesh instanced shader functions");
        return AFFERENT_ERROR_PIPELINE_FAILED;
    }

    MTLRenderPipelineDescriptor *meshInstancedPipelineDesc = [[MTLRenderPipelineDescriptor alloc] init];
    meshInstancedPipelineDesc.vertexFunction = meshInstancedVertexFunc;
    meshInstancedPipelineDesc.fragmentFunction = meshInstancedFragmentFunc;
    meshInstancedPipelineDesc.colorAttachments[0].pixelFormat = MTLPixelFormatBGRA8Unorm;
    apply_alpha_blend(meshInstancedPipelineDesc.colorAttachments[0]);

    renderer->meshInstancedPipelineState = build_pipeline(
        renderer->device,
        meshInstancedPipelineDesc,
        "MeshInstanced",
        &error
    );
    if (!renderer->meshInstancedPipelineState) return AFFERENT_ERROR_PIPELINE_FAILED;

    // Create arc instanced pipeline (for GPU-generated arc strokes)
    id<MTLFunction> arcInstancedVertexFunc = [instancedLibrary newFunctionWithName:@"arc_instanced_vertex"];
    id<MTLFunction> arcInstancedFragmentFunc = [instancedLibrary newFunctionWithName:@"arc_instanced_fragment"];
    if (!arcInstancedVertexFunc || !arcInstancedFragmentFunc) {
        NSLog(@"Failed to find arc instanced shader functions");
        return AFFERENT_ERROR_PIPELINE_FAILED;
    }

    MTLRenderPipelineDescriptor *arcInstancedPipelineDesc = [[MTLRenderPipelineDescriptor alloc] init];
    arcInstancedPipelineDesc.vertexFunction = arcInstancedVertexFunc;
    arcInstancedPipelineDesc.fragmentFunction = arcInstancedFragmentFunc;
    arcInstancedPipelineDesc.colorAttachments[0].pixelFormat = MTLPixelFormatBGRA8Unorm;
    apply_alpha_blend(arcInstancedPipelineDesc.colorAttachments[0]);

    renderer->arcInstancedPipelineState = build_pipeline(
        renderer->device,
        arcInstancedPipelineDesc,
        "ArcInstanced",
        &error
    );
    if (!renderer->arcInstancedPipelineState) return AFFERENT_ERROR_PIPELINE_FAILED;

    // Create sprite pipeline (textured quads)
    id<MTLLibrary> spriteLibrary = [renderer->device newLibraryWithSource:spriteShaderSource
                                                                  options:nil
                                                                    error:&error];
    if (!spriteLibrary) {
        NSLog(@"Sprite shader compilation failed: %@", error);
        return AFFERENT_ERROR_PIPELINE_FAILED;
    }

    id<MTLFunction> spriteVertexFunc = [spriteLibrary newFunctionWithName:@"sprite_vertex_layout0"];
    id<MTLFunction> spriteFragmentFunc = [spriteLibrary newFunctionWithName:@"sprite_fragment"];
    if (!spriteVertexFunc || !spriteFragmentFunc) {
        NSLog(@"Failed to find sprite shader functions");
        return AFFERENT_ERROR_PIPELINE_FAILED;
    }

    MTLRenderPipelineDescriptor *spritePipelineDesc = [[MTLRenderPipelineDescriptor alloc] init];
    spritePipelineDesc.vertexFunction = spriteVertexFunc;
    spritePipelineDesc.fragmentFunction = spriteFragmentFunc;
    spritePipelineDesc.colorAttachments[0].pixelFormat = MTLPixelFormatBGRA8Unorm;
    apply_alpha_blend(spritePipelineDesc.colorAttachments[0]);

    renderer->spritePipelineState = build_pipeline(
        renderer->device,
        spritePipelineDesc,
        "Sprite",
        &error
    );
    if (!renderer->spritePipelineState) return AFFERENT_ERROR_PIPELINE_FAILED;

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

    apply_alpha_blend(pipeline3DDesc.colorAttachments[0]);

    renderer->pipeline3D = build_pipeline(
        renderer->device,
        pipeline3DDesc,
        "3D",
        &error
    );
    if (!renderer->pipeline3D) return AFFERENT_ERROR_PIPELINE_FAILED;

    // ====================================================================
    // Create projected-grid ocean pipeline (procedural vertices via vertex_id)
    // ====================================================================
    MTLRenderPipelineDescriptor *pipelineOceanDesc = [[MTLRenderPipelineDescriptor alloc] init];
    pipelineOceanDesc.vertexFunction = vertexOceanFunction;
    pipelineOceanDesc.fragmentFunction = fragment3DFunction;
    pipelineOceanDesc.vertexDescriptor = nil;
    pipelineOceanDesc.colorAttachments[0].pixelFormat = MTLPixelFormatBGRA8Unorm;
    pipelineOceanDesc.depthAttachmentPixelFormat = MTLPixelFormatDepth32Float;

    apply_alpha_blend(pipelineOceanDesc.colorAttachments[0]);

    renderer->pipeline3DOcean = build_pipeline(
        renderer->device,
        pipelineOceanDesc,
        "Ocean",
        &error
    );
    if (!renderer->pipeline3DOcean) return AFFERENT_ERROR_PIPELINE_FAILED;

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

    apply_alpha_blend(pipeline3DTexturedDesc.colorAttachments[0]);

    renderer->pipeline3DTextured = build_pipeline(
        renderer->device,
        pipeline3DTexturedDesc,
        "Textured 3D",
        &error
    );
    if (!renderer->pipeline3DTextured) return AFFERENT_ERROR_PIPELINE_FAILED;

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

// shaders.m - Metal shader sources (embedded from Lean at compile time)
#import "shaders.h"
#import <Foundation/Foundation.h>

// Global shader source strings (set from Lean via FFI)
NSString *shaderSource = nil;
NSString *textShaderSource = nil;
NSString *instancedShaderSource = nil;
NSString *animatedShaderSource = nil;
NSString *orbitalShaderSource = nil;
NSString *dynamicCircleShaderSource = nil;
NSString *dynamicRectShaderSource = nil;
NSString *dynamicTriangleShaderSource = nil;
NSString *spriteShaderSource = nil;
NSString *shader3DSource = nil;
NSString *shader3DTexturedSource = nil;
NSString *texturedRectShaderSource = nil;

// Set a shader source by name (called from Lean FFI)
void afferent_set_shader_source(const char* name, const char* source) {
    NSString *sourceStr = [NSString stringWithUTF8String:source];

    if (strcmp(name, "basic") == 0) {
        shaderSource = sourceStr;
    } else if (strcmp(name, "text") == 0) {
        textShaderSource = sourceStr;
    } else if (strcmp(name, "instanced") == 0) {
        instancedShaderSource = sourceStr;
    } else if (strcmp(name, "animated") == 0) {
        animatedShaderSource = sourceStr;
    } else if (strcmp(name, "orbital") == 0) {
        orbitalShaderSource = sourceStr;
    } else if (strcmp(name, "dynamic_circle") == 0) {
        dynamicCircleShaderSource = sourceStr;
    } else if (strcmp(name, "dynamic_rect") == 0) {
        dynamicRectShaderSource = sourceStr;
    } else if (strcmp(name, "dynamic_triangle") == 0) {
        dynamicTriangleShaderSource = sourceStr;
    } else if (strcmp(name, "sprite") == 0) {
        spriteShaderSource = sourceStr;
    } else if (strcmp(name, "mesh3d") == 0) {
        shader3DSource = sourceStr;
    } else if (strcmp(name, "mesh3d_textured") == 0) {
        shader3DTexturedSource = sourceStr;
    } else if (strcmp(name, "textured_rect") == 0) {
        texturedRectShaderSource = sourceStr;
    }
}

BOOL afferent_init_shaders(void) {
    // Verify all shaders were set from Lean
    if (shaderSource && textShaderSource && instancedShaderSource &&
        animatedShaderSource && orbitalShaderSource && dynamicCircleShaderSource &&
        dynamicRectShaderSource && dynamicTriangleShaderSource && spriteShaderSource &&
        shader3DSource && shader3DTexturedSource && texturedRectShaderSource) {
        return YES;
    }

    NSLog(@"Error: Shaders not initialized. Call FFI.initShaders before creating Renderer.");
    return NO;
}

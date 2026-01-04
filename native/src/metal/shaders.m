// shaders.m - Metal shader sources (embedded from Lean at compile time)
#import "shaders.h"
#import <Foundation/Foundation.h>

// Global shader source strings (set from Lean via FFI)
NSString *shaderSource = nil;
NSString *textShaderSource = nil;
NSString *instancedShaderSource = nil;
NSString *spriteShaderSource = nil;
NSString *strokeShaderSource = nil;
NSString *strokePathShaderSource = nil;
NSString *shader3DSource = nil;

// Set a shader source by name (called from Lean FFI)
void afferent_set_shader_source(const char* name, const char* source) {
    NSString *sourceStr = [NSString stringWithUTF8String:source];

    if (strcmp(name, "basic") == 0) {
        shaderSource = sourceStr;
    } else if (strcmp(name, "text") == 0) {
        textShaderSource = sourceStr;
    } else if (strcmp(name, "instanced") == 0) {
        instancedShaderSource = sourceStr;
    } else if (strcmp(name, "sprite") == 0) {
        spriteShaderSource = sourceStr;
    } else if (strcmp(name, "stroke") == 0) {
        strokeShaderSource = sourceStr;
    } else if (strcmp(name, "stroke_path") == 0) {
        strokePathShaderSource = sourceStr;
    } else if (strcmp(name, "mesh3d") == 0) {
        shader3DSource = sourceStr;
    }
}

BOOL afferent_init_shaders(void) {
    // Verify all shaders were set from Lean
    if (shaderSource && textShaderSource && instancedShaderSource &&
        spriteShaderSource && strokeShaderSource && strokePathShaderSource &&
        shader3DSource) {
        return YES;
    }

    NSLog(@"Error: Shaders not initialized. Call FFI.initShaders before creating Renderer.");
    return NO;
}

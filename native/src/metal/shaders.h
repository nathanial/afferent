// shaders.h - Metal shader loading declarations
#ifndef AFFERENT_METAL_SHADERS_H
#define AFFERENT_METAL_SHADERS_H

#import <Foundation/Foundation.h>

// Set a shader source by name (called from Lean FFI with embedded shaders)
// Must be called for all shaders before afferent_init_shaders
void afferent_set_shader_source(const char* name, const char* source);

// Verify all shaders were initialized
// Returns YES if all shaders are set, NO otherwise
BOOL afferent_init_shaders(void);

// Basic colored vertex shader
extern NSString *shaderSource;

// Text rendering shader
extern NSString *textShaderSource;

// Instanced shapes shader (rects, triangles, circles)
extern NSString *instancedShaderSource;

// Sprite/texture shader
extern NSString *spriteShaderSource;

// Screen-space stroke shader
extern NSString *strokeShaderSource;

// Screen-space stroke path shader (segment-based)
extern NSString *strokePathShaderSource;

// 3D mesh shader with lighting and fog
extern NSString *shader3DSource;

#endif // AFFERENT_METAL_SHADERS_H

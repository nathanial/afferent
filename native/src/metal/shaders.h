// shaders.h - Metal shader loading declarations
#ifndef AFFERENT_METAL_SHADERS_H
#define AFFERENT_METAL_SHADERS_H

#import <Foundation/Foundation.h>

// Load a shader from the shaders directory by name (without .metal extension)
// Returns the shader source as an NSString, or nil on error
NSString* afferent_load_shader(const char* name);

// Initialize all shaders (loads from files)
// Returns YES on success, NO on failure
BOOL afferent_init_shaders(void);

// Basic colored vertex shader
extern NSString *shaderSource;

// Text rendering shader
extern NSString *textShaderSource;

// Instanced shapes shader (rects, triangles, circles)
extern NSString *instancedShaderSource;

// GPU-side animated shapes shader
extern NSString *animatedShaderSource;

// Orbital particles shader
extern NSString *orbitalShaderSource;

// Dynamic circle shader
extern NSString *dynamicCircleShaderSource;

// Dynamic rect shader
extern NSString *dynamicRectShaderSource;

// Dynamic triangle shader
extern NSString *dynamicTriangleShaderSource;

// Sprite/texture shader
extern NSString *spriteShaderSource;

// 3D mesh shader with lighting and fog
extern NSString *shader3DSource;

// 3D textured mesh shader (for loaded assets with diffuse textures)
extern NSString *shader3DTexturedSource;

// Textured rectangle shader (for map tiles)
extern NSString *texturedRectShaderSource;

#endif // AFFERENT_METAL_SHADERS_H

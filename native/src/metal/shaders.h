// shaders.h - Metal shader string declarations
#ifndef AFFERENT_METAL_SHADERS_H
#define AFFERENT_METAL_SHADERS_H

#import <Foundation/Foundation.h>

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

#endif // AFFERENT_METAL_SHADERS_H

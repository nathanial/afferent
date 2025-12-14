// types.h - C data structures matching shader layouts
#ifndef AFFERENT_METAL_TYPES_H
#define AFFERENT_METAL_TYPES_H

#include <stdint.h>

// Text vertex structure (different layout than AfferentVertex)
typedef struct {
    float position[2];
    float texCoord[2];
    float color[4];
} TextVertex;

// Instance data structure (matches shader) - 32 bytes packed
typedef struct __attribute__((packed)) {
    float pos[2];       // Center position in NDC (8 bytes)
    float angle;        // Rotation angle in radians (4 bytes)
    float halfSize;     // Half side length in NDC (4 bytes)
    float color[4];     // RGBA (16 bytes)
} InstanceData;  // Total: 32 bytes

// Animated instance data structure (matches shader) - 24 bytes
typedef struct {
    float pixelPos[2];      // Position in pixel coordinates (8 bytes)
    float hueBase;          // Base hue 0-1 (4 bytes)
    float halfSizePixels;   // Half size in pixels (4 bytes)
    float phaseOffset;      // Per-particle phase offset (4 bytes)
    float spinSpeed;        // Spin speed multiplier (4 bytes)
} AnimatedInstanceData;  // Total: 24 bytes

// Animation uniforms structure (matches shader)
typedef struct {
    float time;
    float canvasWidth;
    float canvasHeight;
    float padding;
} AnimationUniforms;

// Orbital instance data structure (matches shader) - 32 bytes
typedef struct {
    float phase;           // Initial angle offset (4 bytes)
    float baseRadius;      // Base orbit radius in pixels (4 bytes)
    float orbitSpeed;      // Orbit angular speed (4 bytes)
    float phaseX3;         // Phase for radius wobble (4 bytes)
    float phase2;          // Phase for spin rotation (4 bytes)
    float hueBase;         // Base color hue 0-1 (4 bytes)
    float halfSizePixels;  // Half size in pixels (4 bytes)
    float padding;         // Align to 32 bytes (4 bytes)
} OrbitalInstanceData;  // Total: 32 bytes

// Orbital uniforms structure (matches shader)
typedef struct {
    float time;
    float centerX;
    float centerY;
    float canvasWidth;
    float canvasHeight;
    float radiusWobble;
    float padding1;
    float padding2;
} OrbitalUniforms;

// Dynamic circle data structure (matches shader) - 16 bytes
typedef struct {
    float pixelX;           // Position X in pixels (4 bytes)
    float pixelY;           // Position Y in pixels (4 bytes)
    float hueBase;          // Base color hue 0-1 (4 bytes)
    float radiusPixels;     // Radius in pixels (4 bytes)
} DynamicCircleData;  // Total: 16 bytes

// Dynamic circle uniforms structure (matches shader)
typedef struct {
    float time;
    float canvasWidth;
    float canvasHeight;
    float hueSpeed;
} DynamicCircleUniforms;

// Dynamic rect data structure (matches shader) - 20 bytes
typedef struct {
    float pixelX;           // Position X in pixels
    float pixelY;           // Position Y in pixels
    float hueBase;          // Base color hue 0-1
    float halfSizePixels;   // Half size in pixels
    float rotation;         // Rotation angle in radians
} DynamicRectData;  // Total: 20 bytes

// Dynamic rect uniforms structure (matches shader)
typedef struct {
    float time;
    float canvasWidth;
    float canvasHeight;
    float hueSpeed;
} DynamicRectUniforms;

// Dynamic triangle data structure (matches shader) - 20 bytes
typedef struct {
    float pixelX;           // Position X in pixels
    float pixelY;           // Position Y in pixels
    float hueBase;          // Base color hue 0-1
    float halfSizePixels;   // Half size in pixels
    float rotation;         // Rotation angle in radians
} DynamicTriangleData;  // Total: 20 bytes

// Dynamic triangle uniforms structure (matches shader)
typedef struct {
    float time;
    float canvasWidth;
    float canvasHeight;
    float hueSpeed;
} DynamicTriangleUniforms;

// Sprite instance data structure (matches shader) - 20 bytes
typedef struct {
    float pixelX;           // Position X in pixels
    float pixelY;           // Position Y in pixels
    float rotation;         // Rotation angle in radians
    float halfSizePixels;   // Half size in pixels
    float alpha;            // Alpha transparency 0-1
} SpriteInstanceData;  // Total: 20 bytes

// Sprite uniforms structure (matches shader)
typedef struct {
    float canvasWidth;
    float canvasHeight;
} SpriteUniforms;

// 3D scene uniforms structure (matches shader)
typedef struct {
    float modelViewProj[16];  // MVP matrix (64 bytes)
    float modelMatrix[16];    // Model matrix (64 bytes)
    float lightDir[3];        // Light direction (12 bytes)
    float ambient;            // Ambient factor (4 bytes)
    float cameraPos[3];       // Camera position for fog (12 bytes)
    float fogStart;           // Fog start distance (4 bytes)
    float fogColor[3];        // Fog color RGB (12 bytes)
    float fogEnd;             // Fog end distance (4 bytes)
} Scene3DUniforms;  // Total: 176 bytes

// Ocean projected-grid uniforms
typedef struct {
    Scene3DUniforms scene;
    float params0[4];  // (time, fovY, aspect, maxDistance)
    float params1[4];  // (snapSize, overscanNdc, horizonMargin, yaw)
    float params2[4];  // (pitch, gridSize, nearExtent, mode)
    float waveA[4][4]; // (dirX, dirZ, k, omegaSpeed)
    float waveB[4][4]; // (amplitude, ak, 0, 0)
} OceanProjectedUniforms;

#endif // AFFERENT_METAL_TYPES_H

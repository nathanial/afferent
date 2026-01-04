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
    float pos[2];       // Center position (world or NDC) (8 bytes)
    float angle;        // Rotation angle in radians (4 bytes)
    float halfSize;     // Half side length (world or pixels) (4 bytes)
    float color[4];     // RGBA (16 bytes)
} InstanceData;  // Total: 32 bytes

// Instanced uniforms structure (matches instanced shader)
// transform0 = [a, b, c, d], transform1 = [tx, ty, 0, 0]
// sizeMode: 0 = world, 1 = screen (pixel size)
// colorMode: 0 = RGBA, 1 = HSV (time-based)
typedef struct {
    float transform0[4];
    float transform1[4];
    float viewport[2];
    float time;
    float hueSpeed;
    uint32_t sizeMode;
    uint32_t colorMode;
    float padding0;
    float padding1;
} InstancedUniforms;  // Total: 64 bytes

// Stroke uniforms structure (matches stroke shader)
typedef struct {
    float viewport[2];  // Canvas width/height in pixels (8 bytes)
    float halfWidth;    // Half line width in pixels (4 bytes)
    float padding;      // Alignment padding (4 bytes)
    float color[4];     // RGBA (16 bytes)
} StrokeUniforms;  // Total: 32 bytes

// Stroke segment structure (packed floats, 18 floats = 72 bytes)
typedef struct __attribute__((packed)) {
    float p0[2];
    float p1[2];
    float c1[2];
    float c2[2];
    float prevDir[2];
    float nextDir[2];
    float startDist;
    float length;
    float hasPrev;
    float hasNext;
    float kind;
    float padding;
} StrokeSegment;

// Stroke path vertex uniforms (matches stroke_path shader)
typedef struct {
    float viewport[2];
    float halfWidth;
    float miterLimit;
    uint32_t lineCap;
    uint32_t lineJoin;
    uint32_t segmentSubdivisions;
    uint32_t padding0;
    float transform0[4];  // [a, b, c, d]
    float transform1[4];  // [tx, ty, 0, 0]
} StrokePathVertexUniforms;  // Total: 64 bytes

// Stroke path fragment uniforms (matches stroke_path shader)
typedef struct {
    float color[4];
    float dashSegments[8];
    uint32_t dashCount;
    float dashOffset;
    uint32_t lineCap;
    float halfWidth;
    float padding0;
    float padding1;
    float padding2;
    float padding3;
} StrokePathFragmentUniforms;  // Total: 80 bytes

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

// Sprite instance data structure (matches shader) - 20 bytes
typedef struct {
    float pixelX;           // Position X in pixels
    float pixelY;           // Position Y in pixels
    float rotation;         // Rotation angle in radians
    float halfSizePixels;   // Half size in pixels
    float alpha;            // Alpha transparency 0-1
} SpriteInstanceData;  // Total: 20 bytes

// Sprite uniforms structure (matches shader)
// layout: 0 = sprite (5 floats), 1 = textured (10 floats)
// uvRect is used when layout == 0
typedef struct {
    float viewport[2];
    uint32_t layout;
    uint32_t padding0;
    float uvRect[4];
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

// 3D Textured scene uniforms structure (matches shader)
// Same as Scene3DUniforms with texture tiling options
typedef struct {
    float modelViewProj[16];  // MVP matrix (64 bytes)
    float modelMatrix[16];    // Model matrix (64 bytes)
    float lightDir[3];        // Light direction (12 bytes)
    float ambient;            // Ambient factor (4 bytes)
    float cameraPos[3];       // Camera position for fog (12 bytes)
    float fogStart;           // Fog start distance (4 bytes)
    float fogColor[3];        // Fog color RGB (12 bytes)
    float fogEnd;             // Fog end distance (4 bytes)
    float uvScale[2];         // UV tiling scale (8 bytes) - default (1,1)
    float uvOffset[2];        // UV offset (8 bytes) - default (0,0)
} Scene3DTexturedUniforms;  // Total: 192 bytes

#endif // AFFERENT_METAL_TYPES_H

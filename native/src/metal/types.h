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
// shapeType: 0 = rect, 1 = triangle, 2 = circle
typedef struct {
    float transform0[4];
    float transform1[4];
    float viewport[2];
    float time;
    float hueSpeed;
    uint32_t sizeMode;
    uint32_t colorMode;
    uint32_t shapeType;
    uint32_t padding0;
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
    float viewport[2];
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
    float uvScale[2];         // UV tiling scale (8 bytes)
    float uvOffset[2];        // UV offset (8 bytes)
    uint32_t useTexture;      // 1 = sample diffuse texture, 0 = ignore (4 bytes)
    uint32_t padding0;        // Alignment padding (4 bytes)
    float padding1[2];        // Alignment padding (8 bytes)
} Scene3DUniforms;  // Total: 208 bytes

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

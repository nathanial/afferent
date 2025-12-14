#import <Metal/Metal.h>
#import <QuartzCore/CAMetalLayer.h>
#include "afferent.h"

// External declarations from window.m
extern id<MTLDevice> afferent_window_get_device(AfferentWindowRef window);
extern CAMetalLayer* afferent_window_get_metal_layer(AfferentWindowRef window);

// External declarations from text_render.c for atlas dirty tracking
extern int afferent_font_atlas_dirty(AfferentFontRef font);
extern void afferent_font_atlas_clear_dirty(AfferentFontRef font);

// Shader source embedded in code - basic colored vertices
static NSString *shaderSource = @R"(
#include <metal_stdlib>
using namespace metal;

struct VertexIn {
    float2 position [[attribute(0)]];
    float4 color [[attribute(1)]];
};

struct VertexOut {
    float4 position [[position]];
    float4 color;
};

vertex VertexOut vertex_main(VertexIn in [[stage_in]]) {
    VertexOut out;
    // Position is already in NDC (-1 to 1)
    out.position = float4(in.position, 0.0, 1.0);
    out.color = in.color;
    return out;
}

fragment float4 fragment_main(VertexOut in [[stage_in]]) {
    return in.color;
}
)";

// Text shader source - textured quads with alpha from texture
static NSString *textShaderSource = @R"(
#include <metal_stdlib>
using namespace metal;

struct TextVertexIn {
    float2 position [[attribute(0)]];
    float2 texCoord [[attribute(1)]];
    float4 color [[attribute(2)]];
};

struct TextVertexOut {
    float4 position [[position]];
    float2 texCoord;
    float4 color;
};

vertex TextVertexOut text_vertex_main(TextVertexIn in [[stage_in]]) {
    TextVertexOut out;
    out.position = float4(in.position, 0.0, 1.0);
    out.texCoord = in.texCoord;
    out.color = in.color;
    return out;
}

fragment float4 text_fragment_main(TextVertexOut in [[stage_in]],
                                   texture2d<float> tex [[texture(0)]],
                                   sampler smp [[sampler(0)]]) {
    float alpha = tex.sample(smp, in.texCoord).r;  // Single channel (grayscale) atlas
    return float4(in.color.rgb, in.color.a * alpha);
}
)";

// Instanced rectangle shader - GPU-side transforms for massive parallelism
static NSString *instancedShaderSource = @R"(
#include <metal_stdlib>
using namespace metal;

// Instance data: position(2) + angle(1) + halfSize(1) + color(4) = 8 floats
// Use packed layout to match the flat array from Lean
struct InstanceData {
    packed_float2 pos;       // Center position in NDC (8 bytes)
    float angle;             // Rotation angle in radians (4 bytes)
    float halfSize;          // Half side length in NDC (4 bytes)
    packed_float4 color;     // RGBA (16 bytes)
};  // Total: 32 bytes, no padding

struct InstancedVertexOut {
    float4 position [[position]];
    float4 color;
};

vertex InstancedVertexOut instanced_vertex_main(
    uint vid [[vertex_id]],
    uint iid [[instance_id]],
    constant InstanceData* instances [[buffer(0)]]
) {
    // Unit quad vertices for triangle strip: forms a quad with vertices 0,1,2,3
    // Order: bottom-left, bottom-right, top-left, top-right (Z pattern for strip)
    float2 unitQuad[4] = {
        float2(-1, -1),  // 0: bottom-left
        float2( 1, -1),  // 1: bottom-right
        float2(-1,  1),  // 2: top-left
        float2( 1,  1)   // 3: top-right
    };

    InstanceData inst = instances[iid];
    float2 v = unitQuad[vid];

    // Compute sin/cos on GPU (massively parallel!)
    float sinA = sin(inst.angle);
    float cosA = cos(inst.angle);

    // Rotate: v' = (v.x * cos - v.y * sin, v.x * sin + v.y * cos)
    float2 rotated = float2(
        v.x * cosA - v.y * sinA,
        v.x * sinA + v.y * cosA
    );

    // Scale and translate
    float2 finalPos = inst.pos + rotated * inst.halfSize;

    InstancedVertexOut out;
    out.position = float4(finalPos, 0.0, 1.0);
    out.color = inst.color;
    return out;
}

fragment float4 instanced_fragment_main(InstancedVertexOut in [[stage_in]]) {
    return in.color;
}

// === TRIANGLE SHADER ===
// Draws spinning triangles using 3 vertices per instance

vertex InstancedVertexOut instanced_triangle_vertex(
    uint vid [[vertex_id]],
    uint iid [[instance_id]],
    constant InstanceData* instances [[buffer(0)]]
) {
    // Equilateral triangle vertices (pointing up)
    float2 unitTriangle[3] = {
        float2( 0.0,  1.15),   // top
        float2(-1.0, -0.58),   // bottom-left
        float2( 1.0, -0.58)    // bottom-right
    };

    InstanceData inst = instances[iid];
    float2 v = unitTriangle[vid];

    float sinA = sin(inst.angle);
    float cosA = cos(inst.angle);

    float2 rotated = float2(
        v.x * cosA - v.y * sinA,
        v.x * sinA + v.y * cosA
    );

    float2 finalPos = inst.pos + rotated * inst.halfSize;

    InstancedVertexOut out;
    out.position = float4(finalPos, 0.0, 1.0);
    out.color = inst.color;
    return out;
}

// === CIRCLE SHADER ===
// Draws filled circles using fragment shader distance check

struct CircleVertexOut {
    float4 position [[position]];
    float4 color;
    float2 uv;  // -1 to 1, for distance calculation
};

vertex CircleVertexOut instanced_circle_vertex(
    uint vid [[vertex_id]],
    uint iid [[instance_id]],
    constant InstanceData* instances [[buffer(0)]]
) {
    // Quad vertices (no rotation needed for circles)
    float2 unitQuad[4] = {
        float2(-1, -1),
        float2( 1, -1),
        float2(-1,  1),
        float2( 1,  1)
    };

    InstanceData inst = instances[iid];
    float2 v = unitQuad[vid];

    // Circles don't rotate, but we can use angle for something else (like pulsing)
    float2 finalPos = inst.pos + v * inst.halfSize;

    CircleVertexOut out;
    out.position = float4(finalPos, 0.0, 1.0);
    out.color = inst.color;
    out.uv = v;  // Pass UV for fragment shader
    return out;
}

fragment float4 instanced_circle_fragment(CircleVertexOut in [[stage_in]]) {
    // Distance from center (0,0) in UV space
    float dist = length(in.uv);
    // Smooth edge with anti-aliasing
    float alpha = 1.0 - smoothstep(0.9, 1.0, dist);
    if (alpha < 0.01) discard_fragment();
    return float4(in.color.rgb, in.color.a * alpha);
}
)";

// ============================================================================
// ANIMATED SHADERS - GPU-side animation for maximum performance
// Static instance data uploaded once, only time uniform sent per frame
// ============================================================================
static NSString *animatedShaderSource = @R"(
#include <metal_stdlib>
using namespace metal;

// Per-particle static data (uploaded once at startup)
struct AnimatedInstanceData {
    packed_float2 pixelPos;    // Position in pixel coordinates (8 bytes)
    float hueBase;             // Base hue 0-1 (4 bytes)
    float halfSizePixels;      // Half size in pixels (4 bytes)
    float phaseOffset;         // Per-particle phase offset (4 bytes)
    float spinSpeed;           // Spin speed multiplier (4 bytes)
};  // Total: 24 bytes

// Uniforms updated once per frame
struct AnimationUniforms {
    float time;
    float canvasWidth;
    float canvasHeight;
    float padding;  // Align to 16 bytes
};

struct AnimatedVertexOut {
    float4 position [[position]];
    float4 color;
};

// HSV to RGB conversion (optimized for GPU)
// Using smooth interpolation formula
float3 hsv_to_rgb_fast(float h) {
    // Simplified HSV->RGB with S=0.9, V=1.0
    float3 rgb = clamp(abs(fmod(h * 6.0 + float3(0.0, 4.0, 2.0), 6.0) - 3.0) - 1.0, 0.0, 1.0);
    return 1.0 - 0.9 * (1.0 - rgb);  // Apply saturation
}

vertex AnimatedVertexOut animated_rect_vertex(
    uint vid [[vertex_id]],
    uint iid [[instance_id]],
    constant AnimatedInstanceData* instances [[buffer(0)]],
    constant AnimationUniforms& uniforms [[buffer(1)]]
) {
    // Unit quad vertices for triangle strip
    float2 unitQuad[4] = {
        float2(-1, -1),
        float2( 1, -1),
        float2(-1,  1),
        float2( 1,  1)
    };

    AnimatedInstanceData inst = instances[iid];
    float2 v = unitQuad[vid];

    // Compute angle from time (GPU-side!)
    float angle = uniforms.time * inst.spinSpeed + inst.phaseOffset;

    // Compute HSV -> RGB (GPU-side!)
    float hue = fract(uniforms.time * 0.3 + inst.hueBase);
    float3 rgb = hsv_to_rgb_fast(hue);

    // Convert pixel -> NDC (GPU-side!)
    float2 ndcPos = float2(
        (inst.pixelPos.x / uniforms.canvasWidth) * 2.0 - 1.0,
        1.0 - (inst.pixelPos.y / uniforms.canvasHeight) * 2.0
    );
    float ndcHalfSize = inst.halfSizePixels / uniforms.canvasWidth * 2.0;

    // Rotate
    float sinA = sin(angle);
    float cosA = cos(angle);
    float2 rotated = float2(
        v.x * cosA - v.y * sinA,
        v.x * sinA + v.y * cosA
    );

    // Scale and translate
    float2 finalPos = ndcPos + rotated * ndcHalfSize;

    AnimatedVertexOut out;
    out.position = float4(finalPos, 0.0, 1.0);
    out.color = float4(rgb, 1.0);
    return out;
}

fragment float4 animated_rect_fragment(AnimatedVertexOut in [[stage_in]]) {
    return in.color;
}

// Triangle variant - same animation, different geometry
vertex AnimatedVertexOut animated_triangle_vertex(
    uint vid [[vertex_id]],
    uint iid [[instance_id]],
    constant AnimatedInstanceData* instances [[buffer(0)]],
    constant AnimationUniforms& uniforms [[buffer(1)]]
) {
    // Equilateral triangle vertices
    float2 unitTriangle[3] = {
        float2( 0.0,  1.15),
        float2(-1.0, -0.58),
        float2( 1.0, -0.58)
    };

    AnimatedInstanceData inst = instances[iid];
    float2 v = unitTriangle[vid];

    float angle = uniforms.time * inst.spinSpeed + inst.phaseOffset;
    float hue = fract(uniforms.time * 0.3 + inst.hueBase);
    float3 rgb = hsv_to_rgb_fast(hue);

    float2 ndcPos = float2(
        (inst.pixelPos.x / uniforms.canvasWidth) * 2.0 - 1.0,
        1.0 - (inst.pixelPos.y / uniforms.canvasHeight) * 2.0
    );
    float ndcHalfSize = inst.halfSizePixels / uniforms.canvasWidth * 2.0;

    float sinA = sin(angle);
    float cosA = cos(angle);
    float2 rotated = float2(v.x * cosA - v.y * sinA, v.x * sinA + v.y * cosA);
    float2 finalPos = ndcPos + rotated * ndcHalfSize;

    AnimatedVertexOut out;
    out.position = float4(finalPos, 0.0, 1.0);
    out.color = float4(rgb, 1.0);
    return out;
}

// Circle variant - animated with HSV color cycling
struct AnimatedCircleVertexOut {
    float4 position [[position]];
    float4 color;
    float2 uv;
};

vertex AnimatedCircleVertexOut animated_circle_vertex(
    uint vid [[vertex_id]],
    uint iid [[instance_id]],
    constant AnimatedInstanceData* instances [[buffer(0)]],
    constant AnimationUniforms& uniforms [[buffer(1)]]
) {
    float2 unitQuad[4] = {
        float2(-1, -1),
        float2( 1, -1),
        float2(-1,  1),
        float2( 1,  1)
    };

    AnimatedInstanceData inst = instances[iid];
    float2 v = unitQuad[vid];

    // Circles don't rotate, but we animate color
    float hue = fract(uniforms.time * 0.3 + inst.hueBase);
    float3 rgb = hsv_to_rgb_fast(hue);

    float2 ndcPos = float2(
        (inst.pixelPos.x / uniforms.canvasWidth) * 2.0 - 1.0,
        1.0 - (inst.pixelPos.y / uniforms.canvasHeight) * 2.0
    );
    float ndcHalfSize = inst.halfSizePixels / uniforms.canvasWidth * 2.0;
    float2 finalPos = ndcPos + v * ndcHalfSize;

    AnimatedCircleVertexOut out;
    out.position = float4(finalPos, 0.0, 1.0);
    out.color = float4(rgb, 1.0);
    out.uv = v;
    return out;
}

fragment float4 animated_circle_fragment(AnimatedCircleVertexOut in [[stage_in]]) {
    float dist = length(in.uv);
    float alpha = 1.0 - smoothstep(0.9, 1.0, dist);
    if (alpha < 0.01) discard_fragment();
    return float4(in.color.rgb, in.color.a * alpha);
}
)";

// ============================================================================
// ORBITAL SHADER - Particles orbiting around a center point
// Position computed on GPU from orbital parameters
// ============================================================================
static NSString *orbitalShaderSource = @R"(
#include <metal_stdlib>
using namespace metal;

// Per-particle static orbital data (uploaded once at startup)
struct OrbitalInstanceData {
    float phase;           // Initial angle offset (4 bytes)
    float baseRadius;      // Base orbit radius in pixels (4 bytes)
    float orbitSpeed;      // Orbit angular speed (4 bytes)
    float phaseX3;         // Phase for radius wobble (4 bytes)
    float phase2;          // Phase for spin rotation (4 bytes)
    float hueBase;         // Base color hue 0-1 (4 bytes)
    float halfSizePixels;  // Half size in pixels (4 bytes)
    float padding;         // Align to 32 bytes (4 bytes)
};  // Total: 32 bytes

// Uniforms updated once per frame
struct OrbitalUniforms {
    float time;
    float centerX;         // Orbit center in pixels
    float centerY;
    float canvasWidth;
    float canvasHeight;
    float radiusWobble;    // Amount of radius wobble (default 30.0)
    float padding1;
    float padding2;
};

struct OrbitalVertexOut {
    float4 position [[position]];
    float4 color;
};

// HSV to RGB conversion (same as animated shader)
float3 orbital_hsv_to_rgb(float h) {
    float3 rgb = clamp(abs(fmod(h * 6.0 + float3(0.0, 4.0, 2.0), 6.0) - 3.0) - 1.0, 0.0, 1.0);
    return 1.0 - 0.9 * (1.0 - rgb);
}

vertex OrbitalVertexOut orbital_rect_vertex(
    uint vid [[vertex_id]],
    uint iid [[instance_id]],
    constant OrbitalInstanceData* instances [[buffer(0)]],
    constant OrbitalUniforms& uniforms [[buffer(1)]]
) {
    float2 unitQuad[4] = {
        float2(-1, -1),
        float2( 1, -1),
        float2(-1,  1),
        float2( 1,  1)
    };

    OrbitalInstanceData inst = instances[iid];
    float2 v = unitQuad[vid];

    // Compute orbital position (GPU-side!)
    float orbitAngle = uniforms.time * inst.orbitSpeed + inst.phase;
    float orbitRadius = inst.baseRadius + uniforms.radiusWobble * sin(uniforms.time * 0.5 + inst.phaseX3);
    float pixelX = uniforms.centerX + orbitRadius * cos(orbitAngle);
    float pixelY = uniforms.centerY + orbitRadius * sin(orbitAngle);

    // Compute spin angle (GPU-side!)
    float spinAngle = uniforms.time * 3.0 + inst.phase2;

    // Compute HSV -> RGB (GPU-side!)
    float hue = fract(uniforms.time * 0.3 + inst.hueBase);
    float3 rgb = orbital_hsv_to_rgb(hue);

    // Convert pixel -> NDC (GPU-side!)
    float2 ndcPos = float2(
        (pixelX / uniforms.canvasWidth) * 2.0 - 1.0,
        1.0 - (pixelY / uniforms.canvasHeight) * 2.0
    );
    float ndcHalfSize = inst.halfSizePixels / uniforms.canvasWidth * 2.0;

    // Rotate
    float sinA = sin(spinAngle);
    float cosA = cos(spinAngle);
    float2 rotated = float2(
        v.x * cosA - v.y * sinA,
        v.x * sinA + v.y * cosA
    );

    // Scale and translate
    float2 finalPos = ndcPos + rotated * ndcHalfSize;

    OrbitalVertexOut out;
    out.position = float4(finalPos, 0.0, 1.0);
    out.color = float4(rgb, 1.0);
    return out;
}

fragment float4 orbital_rect_fragment(OrbitalVertexOut in [[stage_in]]) {
    return in.color;
}
)";

// ============================================================================
// DYNAMIC CIRCLE SHADER - Circles with CPU-updated positions, GPU color/NDC
// Positions updated each frame, but HSV->RGB and pixel->NDC done on GPU
// ============================================================================
static NSString *dynamicCircleShaderSource = @R"(
#include <metal_stdlib>
using namespace metal;

// Per-particle dynamic data (updated each frame from CPU)
// Only 4 floats per particle instead of 8 - half the bandwidth!
struct DynamicCircleData {
    float pixelX;           // Position X in pixels (4 bytes)
    float pixelY;           // Position Y in pixels (4 bytes)
    float hueBase;          // Base color hue 0-1 (4 bytes)
    float radiusPixels;     // Radius in pixels (4 bytes)
};  // Total: 16 bytes

// Uniforms updated once per frame
struct DynamicCircleUniforms {
    float time;
    float canvasWidth;
    float canvasHeight;
    float hueSpeed;         // How fast hue cycles (default 0.2)
};

struct DynamicCircleVertexOut {
    float4 position [[position]];
    float4 color;
    float2 uv;              // For circle SDF
};

// HSV to RGB conversion
float3 dynamic_hsv_to_rgb(float h) {
    float3 rgb = clamp(abs(fmod(h * 6.0 + float3(0.0, 4.0, 2.0), 6.0) - 3.0) - 1.0, 0.0, 1.0);
    return 1.0 - 0.9 * (1.0 - rgb);
}

vertex DynamicCircleVertexOut dynamic_circle_vertex(
    uint vid [[vertex_id]],
    uint iid [[instance_id]],
    constant DynamicCircleData* instances [[buffer(0)]],
    constant DynamicCircleUniforms& uniforms [[buffer(1)]]
) {
    // Unit quad for circle bounding box
    float2 unitQuad[4] = {
        float2(-1, -1),
        float2( 1, -1),
        float2(-1,  1),
        float2( 1,  1)
    };

    DynamicCircleData inst = instances[iid];
    float2 v = unitQuad[vid];

    // Compute HSV -> RGB (GPU-side!)
    float hue = fract(uniforms.time * uniforms.hueSpeed + inst.hueBase);
    float3 rgb = dynamic_hsv_to_rgb(hue);

    // Convert pixel -> NDC (GPU-side!)
    float2 ndcPos = float2(
        (inst.pixelX / uniforms.canvasWidth) * 2.0 - 1.0,
        1.0 - (inst.pixelY / uniforms.canvasHeight) * 2.0
    );
    float ndcRadius = inst.radiusPixels / uniforms.canvasWidth * 2.0;

    // Scale and translate (no rotation for circles)
    float2 finalPos = ndcPos + v * ndcRadius;

    DynamicCircleVertexOut out;
    out.position = float4(finalPos, 0.0, 1.0);
    out.color = float4(rgb, 1.0);
    out.uv = v;  // Pass unit coords for SDF
    return out;
}

fragment float4 dynamic_circle_fragment(DynamicCircleVertexOut in [[stage_in]]) {
    // Smooth circle using SDF
    float dist = length(in.uv);
    float alpha = 1.0 - smoothstep(0.95, 1.0, dist);
    if (alpha < 0.01) discard_fragment();
    return float4(in.color.rgb, in.color.a * alpha);
}
)";

// ============================================================================
// DYNAMIC RECT SHADER - Rects with CPU-updated positions/rotation, GPU color/NDC
// 5 floats per instance: [pixelX, pixelY, hueBase, halfSizePixels, rotation]
// ============================================================================
static NSString *dynamicRectShaderSource = @R"(
#include <metal_stdlib>
using namespace metal;

struct DynamicRectData {
    float pixelX;           // Position X in pixels
    float pixelY;           // Position Y in pixels
    float hueBase;          // Base color hue 0-1
    float halfSizePixels;   // Half size in pixels
    float rotation;         // Rotation angle in radians
};  // 20 bytes

struct DynamicRectUniforms {
    float time;
    float canvasWidth;
    float canvasHeight;
    float hueSpeed;
};

struct DynamicRectVertexOut {
    float4 position [[position]];
    float4 color;
};

float3 dynamic_rect_hsv_to_rgb(float h) {
    float3 rgb = clamp(abs(fmod(h * 6.0 + float3(0.0, 4.0, 2.0), 6.0) - 3.0) - 1.0, 0.0, 1.0);
    return 1.0 - 0.9 * (1.0 - rgb);
}

vertex DynamicRectVertexOut dynamic_rect_vertex(
    uint vid [[vertex_id]],
    uint iid [[instance_id]],
    constant DynamicRectData* instances [[buffer(0)]],
    constant DynamicRectUniforms& uniforms [[buffer(1)]]
) {
    float2 unitQuad[4] = {
        float2(-1, -1),
        float2( 1, -1),
        float2(-1,  1),
        float2( 1,  1)
    };

    DynamicRectData inst = instances[iid];
    float2 v = unitQuad[vid];

    // Compute HSV -> RGB (GPU-side!)
    float hue = fract(uniforms.time * uniforms.hueSpeed + inst.hueBase);
    float3 rgb = dynamic_rect_hsv_to_rgb(hue);

    // Convert pixel -> NDC (GPU-side!)
    float2 ndcPos = float2(
        (inst.pixelX / uniforms.canvasWidth) * 2.0 - 1.0,
        1.0 - (inst.pixelY / uniforms.canvasHeight) * 2.0
    );
    float ndcHalfSize = inst.halfSizePixels / uniforms.canvasWidth * 2.0;

    // Apply rotation (from CPU)
    float sinA = sin(inst.rotation);
    float cosA = cos(inst.rotation);
    float2 rotated = float2(v.x * cosA - v.y * sinA, v.x * sinA + v.y * cosA);

    float2 finalPos = ndcPos + rotated * ndcHalfSize;

    DynamicRectVertexOut out;
    out.position = float4(finalPos, 0.0, 1.0);
    out.color = float4(rgb, 1.0);
    return out;
}

fragment float4 dynamic_rect_fragment(DynamicRectVertexOut in [[stage_in]]) {
    return in.color;
}
)";

// ============================================================================
// DYNAMIC TRIANGLE SHADER - Triangles with CPU-updated positions/rotation, GPU color/NDC
// 5 floats per instance: [pixelX, pixelY, hueBase, halfSizePixels, rotation]
// ============================================================================
static NSString *dynamicTriangleShaderSource = @R"(
#include <metal_stdlib>
using namespace metal;

struct DynamicTriangleData {
    float pixelX;           // Position X in pixels
    float pixelY;           // Position Y in pixels
    float hueBase;          // Base color hue 0-1
    float halfSizePixels;   // Half size in pixels
    float rotation;         // Rotation angle in radians
};  // 20 bytes

struct DynamicTriangleUniforms {
    float time;
    float canvasWidth;
    float canvasHeight;
    float hueSpeed;
};

struct DynamicTriangleVertexOut {
    float4 position [[position]];
    float4 color;
};

float3 dynamic_triangle_hsv_to_rgb(float h) {
    float3 rgb = clamp(abs(fmod(h * 6.0 + float3(0.0, 4.0, 2.0), 6.0) - 3.0) - 1.0, 0.0, 1.0);
    return 1.0 - 0.9 * (1.0 - rgb);
}

vertex DynamicTriangleVertexOut dynamic_triangle_vertex(
    uint vid [[vertex_id]],
    uint iid [[instance_id]],
    constant DynamicTriangleData* instances [[buffer(0)]],
    constant DynamicTriangleUniforms& uniforms [[buffer(1)]]
) {
    // Unit equilateral triangle (pointing up)
    float2 unitTriangle[3] = {
        float2( 0.0,  1.0),      // Top
        float2(-0.866, -0.5),   // Bottom left
        float2( 0.866, -0.5)    // Bottom right
    };

    DynamicTriangleData inst = instances[iid];
    float2 v = unitTriangle[vid];

    // Compute HSV -> RGB (GPU-side!)
    float hue = fract(uniforms.time * uniforms.hueSpeed + inst.hueBase);
    float3 rgb = dynamic_triangle_hsv_to_rgb(hue);

    // Convert pixel -> NDC (GPU-side!)
    float2 ndcPos = float2(
        (inst.pixelX / uniforms.canvasWidth) * 2.0 - 1.0,
        1.0 - (inst.pixelY / uniforms.canvasHeight) * 2.0
    );
    float ndcHalfSize = inst.halfSizePixels / uniforms.canvasWidth * 2.0;

    // Apply rotation (from CPU)
    float sinA = sin(inst.rotation);
    float cosA = cos(inst.rotation);
    float2 rotated = float2(v.x * cosA - v.y * sinA, v.x * sinA + v.y * cosA);

    float2 finalPos = ndcPos + rotated * ndcHalfSize;

    DynamicTriangleVertexOut out;
    out.position = float4(finalPos, 0.0, 1.0);
    out.color = float4(rgb, 1.0);
    return out;
}

fragment float4 dynamic_triangle_fragment(DynamicTriangleVertexOut in [[stage_in]]) {
    return in.color;
}
)";

// ============================================================================
// SPRITE SHADER - Textured quad with rotation and alpha
// Data format: [pixelX, pixelY, rotation, halfSize, alpha] × count (5 floats)
// ============================================================================

static NSString *spriteShaderSource = @R"(
#include <metal_stdlib>
using namespace metal;

struct SpriteInstanceData {
    float pixelX;           // Position X in pixels
    float pixelY;           // Position Y in pixels
    float rotation;         // Rotation angle in radians
    float halfSizePixels;   // Half size in pixels
    float alpha;            // Alpha transparency 0-1
};  // 20 bytes

struct SpriteUniforms {
    float canvasWidth;
    float canvasHeight;
};

struct SpriteVertexOut {
    float4 position [[position]];
    float2 uv;
    float alpha;
};

vertex SpriteVertexOut sprite_vertex(
    uint vid [[vertex_id]],
    uint iid [[instance_id]],
    constant SpriteInstanceData* instances [[buffer(0)]],
    constant SpriteUniforms& uniforms [[buffer(1)]]
) {
    // Unit quad positions and UVs (triangle strip order)
    float2 positions[4] = {
        float2(-1.0, -1.0),  // Bottom-left
        float2( 1.0, -1.0),  // Bottom-right
        float2(-1.0,  1.0),  // Top-left
        float2( 1.0,  1.0)   // Top-right
    };
    float2 uvs[4] = {
        float2(0.0, 1.0),    // Bottom-left
        float2(1.0, 1.0),    // Bottom-right
        float2(0.0, 0.0),    // Top-left
        float2(1.0, 0.0)     // Top-right
    };

    SpriteInstanceData inst = instances[iid];
    float2 v = positions[vid];
    float2 uv = uvs[vid];

    // Convert pixel -> NDC
    float2 ndcPos = float2(
        (inst.pixelX / uniforms.canvasWidth) * 2.0 - 1.0,
        1.0 - (inst.pixelY / uniforms.canvasHeight) * 2.0
    );
    float ndcHalfSize = inst.halfSizePixels / uniforms.canvasWidth * 2.0;

    // Apply rotation
    float sinA = sin(inst.rotation);
    float cosA = cos(inst.rotation);
    float2 rotated = float2(v.x * cosA - v.y * sinA, v.x * sinA + v.y * cosA);

    float2 finalPos = ndcPos + rotated * ndcHalfSize;

    SpriteVertexOut out;
    out.position = float4(finalPos, 0.0, 1.0);
    out.uv = uv;
    out.alpha = inst.alpha;
    return out;
}

fragment float4 sprite_fragment(
    SpriteVertexOut in [[stage_in]],
    texture2d<float> tex [[texture(0)]],
    sampler samp [[sampler(0)]]
) {
    float4 color = tex.sample(samp, in.uv);
    color.a *= in.alpha;
    // Premultiplied alpha discard for transparency
    if (color.a < 0.01) discard_fragment();
    return color;
}
)";

// ============================================================================
// 3D SHADER - Perspective projection with basic lighting
// Vertices: position[3], normal[3], color[4] (10 floats per vertex)
// ============================================================================
static NSString *shader3DSource = @R"(
#include <metal_stdlib>
using namespace metal;

// 3D Vertex input (matches AfferentVertex3D layout)
struct Vertex3DIn {
    float3 position [[attribute(0)]];
    float3 normal [[attribute(1)]];
    float4 color [[attribute(2)]];
};

// 3D Vertex output
struct Vertex3DOut {
    float4 position [[position]];
    float3 worldNormal;
    float3 worldPos;    // World position for fog calculation
    float2 oceanBaseXZ; // Undisplaced ocean XZ (world), for stable seam clipping
    float4 color;
};

// Scene uniforms for 3D rendering
// NOTE: Using packed_float3 to match C struct layout (12 bytes, no padding)
struct Scene3DUniforms {
    float4x4 modelViewProj;   // Combined MVP matrix
    float4x4 modelMatrix;     // Model matrix for normal transformation
    packed_float3 lightDir;   // Light direction (12 bytes, packed to match C)
    float ambient;            // Ambient light factor
    packed_float3 cameraPos;  // Camera position for fog distance
    float fogStart;           // Distance where fog begins
    packed_float3 fogColor;   // Fog color (RGB)
    float fogEnd;             // Distance where fog is fully opaque
};

vertex Vertex3DOut vertex_main_3d(
    Vertex3DIn in [[stage_in]],
    constant Scene3DUniforms& uniforms [[buffer(1)]]
) {
    Vertex3DOut out;
    out.position = uniforms.modelViewProj * float4(in.position, 1.0);
    // Transform normal to world space (using upper-left 3x3 of model matrix)
    out.worldNormal = (uniforms.modelMatrix * float4(in.normal, 0.0)).xyz;
    // Pass world position for fog calculation
    out.worldPos = (uniforms.modelMatrix * float4(in.position, 1.0)).xyz;
    out.oceanBaseXZ = float2(0.0, 0.0);
    out.color = in.color;
    return out;
}

// Projected-grid ocean uniforms: scene + parameters + 4 Gerstner waves.
// params0: (time, fovY, aspect, maxDistance)
// params1: (snapSize, overscanNdc, horizonMargin, yaw)
// params2: (pitch, gridSize, 0, 0)
// waveA[i]: (dirX, dirZ, k, omegaSpeed)
// waveB[i]: (amplitude, ak, 0, 0)
struct OceanProjectedUniforms {
    Scene3DUniforms scene;
    float4 params0;
    float4 params1;
    float4 params2;
    float4 waveA[4];
    float4 waveB[4];
};

static inline void ocean_gerstner(
    float2 xz,
    constant OceanProjectedUniforms& u,
    thread float3& displacedPos,
    thread float3& normalOut
) {
    float dx = 0.0;
    float dy = 0.0;
    float dz = 0.0;
    float sx = 0.0;
    float sz = 0.0;
    float sxx = 0.0;
    float szz = 0.0;
    float sxz = 0.0;

    for (uint i = 0; i < 4; i++) {
        float2 dir = u.waveA[i].xy;
        float k = u.waveA[i].z;
        float omegaSpeed = u.waveA[i].w;
        float amplitude = u.waveB[i].x;
        float ak = u.waveB[i].y;

        float phase = k * (dir.x * xz.x + dir.y * xz.y) - omegaSpeed * u.params0.x;
        float c = cos(phase);
        float s = sin(phase);

        dx += amplitude * dir.x * c;
        dy += amplitude * s;
        dz += amplitude * dir.y * c;

        sx += ak * dir.x * c;
        sz += ak * dir.y * c;
        sxx += ak * dir.x * dir.x * s;
        szz += ak * dir.y * dir.y * s;
        sxz += ak * dir.x * dir.y * s;
    }

    displacedPos = float3(xz.x + dx, dy, xz.y + dz);

    float3 dPdx = float3(1.0 - sxx, sx, -sxz);
    float3 dPdz = float3(-sxz, sz, 1.0 - szz);
    normalOut = normalize(cross(dPdz, dPdx));
}

vertex Vertex3DOut vertex_ocean_projected_waves(
    uint vid [[vertex_id]],
    constant OceanProjectedUniforms& u [[buffer(1)]]
) {
    Vertex3DOut out;

    float time = u.params0.x;
    (void)time;
    float fovY = u.params0.y;
    float aspect = u.params0.z;
    float maxDistance = u.params0.w;
    float snapSize = u.params1.x;
    float overscanNdc = u.params1.y;
    float horizonMargin = u.params1.z;
    float yaw = u.params1.w;
    float pitch = u.params2.x;
    uint gridSize = (uint)u.params2.y;
    float nearExtent = u.params2.z;
    uint gridSizeMinus1 = (gridSize > 0) ? (gridSize - 1) : 0;
    uint row = (gridSize > 0) ? (vid / gridSize) : 0;
    uint col = (gridSize > 0) ? (vid - row * gridSize) : 0;
    float u01 = (gridSizeMinus1 > 0) ? ((float)col / (float)gridSizeMinus1) : 0.0;
    float v01 = (gridSizeMinus1 > 0) ? ((float)row / (float)gridSizeMinus1) : 0.0;

    // Camera basis (matches Lean FPSCamera).
    float cosPitch = cos(pitch);
    float sinPitch = sin(pitch);
    float cosYaw = cos(yaw);
    float sinYaw = sin(yaw);
    float3 fwd = float3(cosPitch * sinYaw, sinPitch, -cosPitch * cosYaw);
    float3 right = normalize(cross(fwd, float3(0.0, 1.0, 0.0)));
    float3 up = normalize(cross(right, fwd));

    float3 camPos = float3(u.scene.cameraPos);

    // Grid snapping (world XZ).
    float originX = camPos.x;
    float originZ = camPos.z;
    if (snapSize > 0.00001) {
        originX = floor(originX / snapSize) * snapSize;
        originZ = floor(originZ / snapSize) * snapSize;
    }

    float tanHalfFovY = tan(fovY * 0.5);
    float tanHalfFovX = tanHalfFovY * aspect;

    // Conservative wave bounds for overscan:
    // - Vertical displacement is bounded by sum(amplitude).
    // - Horizontal displacement in our Gerstner implementation is also bounded by sum(amplitude).
    float maxWaveAmp = 0.0;
    for (uint i = 0; i < 4; i++) {
        maxWaveAmp += u.waveB[i].x;
    }

    float eps = 0.00001;
    float baseX = originX;
    float baseZ = originZ;

    (void)nearExtent;

    // Projected grid only: generate the ocean surface by intersecting view rays with the ocean plane.
    // Horizon cutoff in NDC (same logic as CPU path).
    float horizonSy = (abs(up.y) < eps) ? 0.0 : (-fwd.y) / up.y;
    float horizonNdcY = horizonSy / tanHalfFovY;

    // Aggressive adaptive overscan near the surface.
    // The projected-grid is view-frustum aligned, so when the camera is close to the surface and pitched down,
    // wave displacement can expose the mesh boundary unless we overscan significantly (especially at the bottom).
    float camHeight = max(camPos.y, 0.05);
    float ampOverHeight = (camHeight > eps) ? (maxWaveAmp / camHeight) : 0.0;
    float pitchDown = clamp(-pitch, 0.0, 1.2); // pitch is negative when looking down

    float extraAllNdc = clamp(ampOverHeight * 0.45, 0.0, 4.0);
    float overscanEff = overscanNdc + extraAllNdc;

    float extraBottomNdc = clamp(ampOverHeight * (2.8 + 3.0 * pitchDown), 0.0, 30.0);
    float extraSideNdc = clamp(ampOverHeight * (1.2 + 1.5 * pitchDown), 0.0, 12.0);
    float extraTopNdc = clamp(ampOverHeight * (0.8 + 0.8 * pitchDown), 0.0, 8.0);

    float ndcBottom = -1.0 - overscanEff - extraBottomNdc;
    float ndcTop0 = horizonNdcY - horizonMargin;
    float ndcTop = clamp(ndcTop0, ndcBottom, 1.0 + overscanEff + extraTopNdc);
    float ndcLeft = -1.0 - overscanEff - extraSideNdc;
    float ndcRight = 1.0 + overscanEff + extraSideNdc;

    float ndcX = mix(ndcLeft, ndcRight, u01);
    float ndcY = mix(ndcTop, ndcBottom, v01);

    float sx = ndcX * tanHalfFovX;
    float sy = ndcY * tanHalfFovY;
    float3 dir = right * sx + up * sy + fwd;

    float tHit = (abs(dir.y) < eps) ? maxDistance : (-camPos.y) / dir.y;
    tHit = (tHit < 0.0) ? maxDistance : ((tHit > maxDistance) ? maxDistance : tHit);

    float baseProjX = originX + dir.x * tHit;
    float baseProjZ = originZ + dir.z * tHit;

    float ndcAbsMaxX = 1.0 + overscanEff + extraSideNdc;
    float ndcAbsMaxY = max(abs(ndcBottom), abs(ndcTop));
    float edge01X = abs(ndcX) / max(ndcAbsMaxX, eps);
    float edge01Y = abs(ndcY) / max(ndcAbsMaxY, eps);
    float edgeWeightX = smoothstep(0.75, 1.0, edge01X);
    float edgeWeightY = smoothstep(0.75, 1.0, edge01Y);
    float edgeWeight = max(edgeWeightX, edgeWeightY);
    float ampHeightFactor = clamp(ampOverHeight, 0.0, 10.0);
    float expandScale = 2.0 + 3.0 * pitchDown + 0.35 * ampHeightFactor;
    float expandMeters = (maxWaveAmp * expandScale + 2.0) * edgeWeight;
    if (expandMeters > 0.0) {
        float2 v = float2(baseProjX - originX, baseProjZ - originZ);
        float lenV = length(v);
        float2 dirXZ = (lenV > eps) ? (v / lenV) : normalize(float2(dir.x, dir.z));
        baseProjX += dirXZ.x * expandMeters;
        baseProjZ += dirXZ.y * expandMeters;
    }

    baseX = baseProjX;
    baseZ = baseProjZ;

    float3 displacedPos;
    float3 localNormal;
    ocean_gerstner(float2(baseX, baseZ), u, displacedPos, localNormal);

    out.position = u.scene.modelViewProj * float4(displacedPos, 1.0);
    out.worldPos = (u.scene.modelMatrix * float4(displacedPos, 1.0)).xyz;
    out.worldNormal = (u.scene.modelMatrix * float4(localNormal, 0.0)).xyz;
    float3 baseWorld = (u.scene.modelMatrix * float4(baseX, 0.0, baseZ, 1.0)).xyz;
    out.oceanBaseXZ = baseWorld.xz;

    // Color based on wave height (matches CPU color mapping).
    float heightFactor = clamp((displacedPos.y + 2.0) / 4.0, 0.0, 1.0);
    float3 water = float3(
        0.15 + heightFactor * 0.35,
        0.25 + heightFactor * 0.30,
        0.30 + heightFactor * 0.30
    );
    out.color = float4(water, 1.0);
    return out;
}

fragment float4 fragment_main_3d(
    Vertex3DOut in [[stage_in]],
    constant Scene3DUniforms& uniforms [[buffer(0)]]
) {
    float3 N = normalize(in.worldNormal);
    float3 L = normalize(uniforms.lightDir);
    float diffuse = max(0.0, dot(N, L));
    float3 litColor = in.color.rgb * (uniforms.ambient + (1.0 - uniforms.ambient) * diffuse);

    // Linear fog based on distance from camera
    // When fogEnd <= fogStart, fog is disabled (fogFactor = 1.0 means no fog)
    float dist = length(in.worldPos - float3(uniforms.cameraPos));
    float fogRange = uniforms.fogEnd - uniforms.fogStart;
    float fogFactor = (fogRange > 0.0) ? clamp((uniforms.fogEnd - dist) / fogRange, 0.0, 1.0) : 1.0;
    float3 finalColor = mix(float3(uniforms.fogColor), litColor, fogFactor);

    return float4(finalColor, in.color.a);
}

fragment float4 fragment_ocean_3d(
    Vertex3DOut in [[stage_in]],
    constant Scene3DUniforms& scene [[buffer(0)]],
    constant OceanProjectedUniforms& u [[buffer(1)]]
) {
    // Seam handling between the two-pass ocean draw (local patch + projected grid):
    // Instead of hard clipping (which can leave cracks), we cross-fade in a small radial band.
    float snapSize = u.params1.x;
    float nearExtent = u.params2.z;
    float mode = u.params2.w;

    float maxWaveAmp = 0.0;
    for (uint i = 0; i < 4; i++) {
        maxWaveAmp += u.waveB[i].x;
    }
    float seamWidth = max(2.0, maxWaveAmp * 2.0);
    float donutRadius = nearExtent + seamWidth;
    float blendWidth = max(6.0, maxWaveAmp * 6.0);

    float originX = scene.cameraPos[0];
    float originZ = scene.cameraPos[2];
    if (snapSize > 0.00001) {
        originX = floor(originX / snapSize) * snapSize;
        originZ = floor(originZ / snapSize) * snapSize;
    }

    // Use undisplaced XZ so waves can't "push" fragments across the boundary and cause shimmering seams.
    float2 d = float2(in.oceanBaseXZ.x - originX, in.oceanBaseXZ.y - originZ);
    float r = length(d);
    float alpha = 1.0;
    if (donutRadius > 0.0 && blendWidth > 0.0) {
        float r0 = donutRadius - blendWidth;
        float r1 = donutRadius + blendWidth;
        float t = (r1 > r0) ? clamp((r - r0) / (r1 - r0), 0.0, 1.0) : 0.5;
        alpha = (mode > 0.5) ? (1.0 - t) : t;
        if (alpha < 0.001) discard_fragment();
    }

    float3 N = normalize(in.worldNormal);
    float3 L = normalize(scene.lightDir);
    float diffuse = max(0.0, dot(N, L));
    float3 litColor = in.color.rgb * (scene.ambient + (1.0 - scene.ambient) * diffuse);

    float dist = length(in.worldPos - float3(scene.cameraPos));
    float fogRange = scene.fogEnd - scene.fogStart;
    float fogFactor = (fogRange > 0.0) ? clamp((scene.fogEnd - dist) / fogRange, 0.0, 1.0) : 1.0;
    float3 finalColor = mix(float3(scene.fogColor), litColor, fogFactor);

    return float4(finalColor, alpha);
}
)";

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

typedef struct {
    Scene3DUniforms scene;
    float params0[4];  // (time, fovY, aspect, maxDistance)
    float params1[4];  // (snapSize, overscanNdc, horizonMargin, yaw)
    float params2[4];  // (pitch, gridSize, 0, 0)
    float waveA[4][4]; // (dirX, dirZ, k, omegaSpeed)
    float waveB[4][4]; // (amplitude, ak, 0, 0)
} OceanProjectedUniforms;

// Internal renderer structure
struct AfferentRenderer {
    AfferentWindowRef window;
    id<MTLDevice> device;
    id<MTLCommandQueue> commandQueue;
    bool msaaEnabled;                                  // Per-frame MSAA toggle
    float drawableScaleOverride;                       // 0 = native scale, >0 overrides
    // Active pipeline pointers (match current render pass sample count)
    id<MTLRenderPipelineState> pipelineState;
    id<MTLRenderPipelineState> textPipelineState;      // For text rendering
    id<MTLRenderPipelineState> spritePipelineState;    // For sprite/texture rendering
    // MSAA / non-MSAA variants for pipelines used in sprite benchmark
    id<MTLRenderPipelineState> pipelineStateMSAA;
    id<MTLRenderPipelineState> pipelineStateNoMSAA;
    id<MTLRenderPipelineState> textPipelineStateMSAA;
    id<MTLRenderPipelineState> textPipelineStateNoMSAA;
    id<MTLRenderPipelineState> spritePipelineStateMSAA;
    id<MTLRenderPipelineState> spritePipelineStateNoMSAA;
    id<MTLRenderPipelineState> instancedPipelineState; // For instanced rect rendering
    id<MTLRenderPipelineState> trianglePipelineState;  // For instanced triangle rendering
    id<MTLRenderPipelineState> circlePipelineState;    // For instanced circle rendering
    // Animated pipelines (GPU-side animation)
    id<MTLRenderPipelineState> animatedRectPipelineState;
    id<MTLRenderPipelineState> animatedTrianglePipelineState;
    id<MTLRenderPipelineState> animatedCirclePipelineState;
    id<MTLRenderPipelineState> orbitalPipelineState;   // For orbital particle rendering
    id<MTLRenderPipelineState> dynamicCirclePipelineState;  // For dynamic position circles
    id<MTLRenderPipelineState> dynamicRectPipelineState;    // For dynamic position rects
    id<MTLRenderPipelineState> dynamicTrianglePipelineState; // For dynamic position triangles
    id<MTLSamplerState> textSampler;                   // For text texture sampling
    id<MTLSamplerState> spriteSampler;                 // For sprite texture sampling
    id<MTLCommandBuffer> currentCommandBuffer;
    id<MTLRenderCommandEncoder> currentEncoder;
    id<CAMetalDrawable> currentDrawable;
    id<MTLTexture> msaaTexture;  // 4x MSAA render target
    NSUInteger msaaWidth;        // Track size for recreation
    NSUInteger msaaHeight;
    // 3D rendering support
    id<MTLTexture> depthTexture;           // Depth buffer (non-MSAA)
    id<MTLTexture> msaaDepthTexture;       // Depth buffer (MSAA)
    id<MTLDepthStencilState> depthState;   // Depth test state (enabled)
    id<MTLDepthStencilState> depthStateDisabled; // Depth test disabled for 2D after 3D
    id<MTLDepthStencilState> depthStateOcean;    // Ocean depth state (test on, no writes)
    id<MTLRenderPipelineState> pipeline3D;       // Active 3D rendering pipeline
    id<MTLRenderPipelineState> pipeline3DMSAA;   // 3D pipeline (4x MSAA)
    id<MTLRenderPipelineState> pipeline3DNoMSAA; // 3D pipeline (no MSAA)
    id<MTLRenderPipelineState> pipeline3DOcean;       // Active ocean projected-grid pipeline
    id<MTLRenderPipelineState> pipeline3DOceanMSAA;   // Ocean pipeline (4x MSAA)
    id<MTLRenderPipelineState> pipeline3DOceanNoMSAA; // Ocean pipeline (no MSAA)
    id<MTLBuffer> oceanIndexBuffer;
    uint32_t oceanIndexCount;
    uint32_t oceanGridSize;
    NSUInteger depthWidth;                 // Track depth texture size
    NSUInteger depthHeight;
    MTLClearColor clearColor;
    float screenWidth;   // Current screen dimensions for text rendering
    float screenHeight;
    // Persistent buffers for animated rendering (uploaded once, reused every frame)
    id<MTLBuffer> animatedRectBuffer;
    id<MTLBuffer> animatedTriangleBuffer;
    id<MTLBuffer> animatedCircleBuffer;
    id<MTLBuffer> orbitalBuffer;
    uint32_t animatedRectCount;
    uint32_t animatedTriangleCount;
    uint32_t animatedCircleCount;
    uint32_t orbitalCount;
    // Orbital center (stored at upload time)
    float orbitalCenterX;
    float orbitalCenterY;
};

// Internal buffer structure
struct AfferentBuffer {
    id<MTLBuffer> mtlBuffer;
    uint32_t count;
};

// ============================================================================
// Buffer Pool - Reuse MTLBuffers across frames to avoid allocation overhead
// ============================================================================

#define BUFFER_POOL_SIZE 64
#define MAX_BUFFER_SIZE (1024 * 1024)  // 1MB max per pooled buffer
#define WRAPPER_POOL_SIZE 256  // Pool for AfferentBuffer wrapper structs

typedef struct {
    id<MTLBuffer> buffer;
    size_t capacity;
    bool in_use;
} PooledBuffer;

typedef struct {
    PooledBuffer vertex_pool[BUFFER_POOL_SIZE];
    PooledBuffer index_pool[BUFFER_POOL_SIZE];
    int vertex_pool_count;
    int index_pool_count;
    // Wrapper struct pool to avoid malloc/free per draw call
    struct AfferentBuffer* wrapper_pool[WRAPPER_POOL_SIZE];
    int wrapper_pool_count;
    int wrapper_pool_used;
    // Text rendering buffer pools (separate from shape buffers)
    PooledBuffer text_vertex_pool[BUFFER_POOL_SIZE];
    PooledBuffer text_index_pool[BUFFER_POOL_SIZE];
    int text_vertex_pool_count;
    int text_index_pool_count;
} BufferPool;

static BufferPool g_buffer_pool = {0};

// Staging buffer for text vertex conversion (reused across frames)
static TextVertex* g_text_vertex_staging = NULL;
static size_t g_text_vertex_staging_capacity = 0;

// Get a wrapper struct from the pool (or allocate if pool is empty)
static struct AfferentBuffer* pool_acquire_wrapper(void) {
    if (g_buffer_pool.wrapper_pool_used < g_buffer_pool.wrapper_pool_count) {
        return g_buffer_pool.wrapper_pool[g_buffer_pool.wrapper_pool_used++];
    }
    // Pool exhausted, allocate new and try to add to pool
    struct AfferentBuffer* wrapper = malloc(sizeof(struct AfferentBuffer));
    if (g_buffer_pool.wrapper_pool_count < WRAPPER_POOL_SIZE) {
        g_buffer_pool.wrapper_pool[g_buffer_pool.wrapper_pool_count++] = wrapper;
        g_buffer_pool.wrapper_pool_used++;
    }
    return wrapper;
}

// Find or create a buffer of at least the required size
static id<MTLBuffer> pool_acquire_buffer(id<MTLDevice> device, PooledBuffer* pool, int* count, size_t required_size, bool is_vertex) {
    // First, try to find an existing buffer that's large enough and not in use
    for (int i = 0; i < *count; i++) {
        if (!pool[i].in_use && pool[i].capacity >= required_size) {
            pool[i].in_use = true;
            return pool[i].buffer;
        }
    }

    // No suitable buffer found - create a new one
    // Round up to power of 2 for better reuse
    size_t capacity = 4096;  // Minimum 4KB
    while (capacity < required_size && capacity < MAX_BUFFER_SIZE) {
        capacity *= 2;
    }
    if (capacity < required_size) {
        capacity = required_size;  // For very large buffers
    }

    id<MTLBuffer> newBuffer = [device newBufferWithLength:capacity
                                                  options:MTLResourceStorageModeShared];
    if (!newBuffer) {
        return nil;
    }

    // Add to pool if there's room
    if (*count < BUFFER_POOL_SIZE) {
        pool[*count].buffer = newBuffer;
        pool[*count].capacity = capacity;
        pool[*count].in_use = true;
        (*count)++;
    }
    // If pool is full, just return the buffer (it won't be pooled)

    return newBuffer;
}

// Mark all buffers as available for reuse (call at frame start)
static void pool_reset_frame(void) {
    for (int i = 0; i < g_buffer_pool.vertex_pool_count; i++) {
        g_buffer_pool.vertex_pool[i].in_use = false;
    }
    for (int i = 0; i < g_buffer_pool.index_pool_count; i++) {
        g_buffer_pool.index_pool[i].in_use = false;
    }
    // Reset text buffer pools
    for (int i = 0; i < g_buffer_pool.text_vertex_pool_count; i++) {
        g_buffer_pool.text_vertex_pool[i].in_use = false;
    }
    for (int i = 0; i < g_buffer_pool.text_index_pool_count; i++) {
        g_buffer_pool.text_index_pool[i].in_use = false;
    }
    // Reset wrapper pool (structs stay allocated, just reset usage counter)
    g_buffer_pool.wrapper_pool_used = 0;
}

AfferentResult afferent_renderer_create(
    AfferentWindowRef window,
    AfferentRendererRef* out_renderer
) {
    @autoreleasepool {
        struct AfferentRenderer *renderer = malloc(sizeof(struct AfferentRenderer));
        memset(renderer, 0, sizeof(struct AfferentRenderer));

        renderer->window = window;
        renderer->device = afferent_window_get_device(window);
        renderer->clearColor = MTLClearColorMake(0.0, 0.0, 0.0, 1.0);

        if (!renderer->device) {
            NSLog(@"No Metal device available");
            free(renderer);
            return AFFERENT_ERROR_DEVICE_FAILED;
        }

        // Create command queue
        renderer->commandQueue = [renderer->device newCommandQueue];
        if (!renderer->commandQueue) {
            NSLog(@"Failed to create command queue");
            free(renderer);
            return AFFERENT_ERROR_INIT_FAILED;
        }

        // Compile shaders
        NSError *error = nil;
        id<MTLLibrary> library = [renderer->device newLibraryWithSource:shaderSource
                                                                options:nil
                                                                  error:&error];
        if (!library) {
            NSLog(@"Shader compilation failed: %@", error);
            free(renderer);
            return AFFERENT_ERROR_PIPELINE_FAILED;
        }

        id<MTLFunction> vertexFunction = [library newFunctionWithName:@"vertex_main"];
        id<MTLFunction> fragmentFunction = [library newFunctionWithName:@"fragment_main"];

        if (!vertexFunction || !fragmentFunction) {
            NSLog(@"Failed to find shader functions");
            free(renderer);
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
            free(renderer);
            return AFFERENT_ERROR_PIPELINE_FAILED;
        }

        pipelineDesc.rasterSampleCount = 1;
        renderer->pipelineStateNoMSAA = [renderer->device newRenderPipelineStateWithDescriptor:pipelineDesc
                                                                                         error:&error];
        if (!renderer->pipelineStateNoMSAA) {
            NSLog(@"Pipeline creation failed (no MSAA): %@", error);
            free(renderer);
            return AFFERENT_ERROR_PIPELINE_FAILED;
        }

        renderer->pipelineState = renderer->pipelineStateMSAA;
        renderer->msaaEnabled = true;

        // Create text rendering pipeline
        id<MTLLibrary> textLibrary = [renderer->device newLibraryWithSource:textShaderSource
                                                                    options:nil
                                                                      error:&error];
        if (!textLibrary) {
            NSLog(@"Text shader compilation failed: %@", error);
            free(renderer);
            return AFFERENT_ERROR_PIPELINE_FAILED;
        }

        id<MTLFunction> textVertexFunction = [textLibrary newFunctionWithName:@"text_vertex_main"];
        id<MTLFunction> textFragmentFunction = [textLibrary newFunctionWithName:@"text_fragment_main"];

        if (!textVertexFunction || !textFragmentFunction) {
            NSLog(@"Failed to find text shader functions");
            free(renderer);
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
            free(renderer);
            return AFFERENT_ERROR_PIPELINE_FAILED;
        }

        textPipelineDesc.rasterSampleCount = 1;
        renderer->textPipelineStateNoMSAA = [renderer->device newRenderPipelineStateWithDescriptor:textPipelineDesc
                                                                                             error:&error];
        if (!renderer->textPipelineStateNoMSAA) {
            NSLog(@"Text pipeline creation failed (no MSAA): %@", error);
            free(renderer);
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
            free(renderer);
            return AFFERENT_ERROR_PIPELINE_FAILED;
        }

        id<MTLFunction> instancedVertexFunction = [instancedLibrary newFunctionWithName:@"instanced_vertex_main"];
        id<MTLFunction> instancedFragmentFunction = [instancedLibrary newFunctionWithName:@"instanced_fragment_main"];

        if (!instancedVertexFunction || !instancedFragmentFunction) {
            NSLog(@"Failed to find instanced shader functions");
            free(renderer);
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
            free(renderer);
            return AFFERENT_ERROR_PIPELINE_FAILED;
        }

        // Create triangle pipeline (same library, different vertex function)
        id<MTLFunction> triangleVertexFunction = [instancedLibrary newFunctionWithName:@"instanced_triangle_vertex"];
        if (!triangleVertexFunction) {
            NSLog(@"Failed to find triangle vertex function");
            free(renderer);
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
            free(renderer);
            return AFFERENT_ERROR_PIPELINE_FAILED;
        }

        // Create circle pipeline (different vertex and fragment functions)
        id<MTLFunction> circleVertexFunction = [instancedLibrary newFunctionWithName:@"instanced_circle_vertex"];
        id<MTLFunction> circleFragmentFunction = [instancedLibrary newFunctionWithName:@"instanced_circle_fragment"];
        if (!circleVertexFunction || !circleFragmentFunction) {
            NSLog(@"Failed to find circle shader functions");
            free(renderer);
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
            free(renderer);
            return AFFERENT_ERROR_PIPELINE_FAILED;
        }

        // ====================================================================
        // Create animated pipelines (GPU-side animation for maximum performance)
        // ====================================================================
        id<MTLLibrary> animatedLibrary = [renderer->device newLibraryWithSource:animatedShaderSource
                                                                        options:nil
                                                                          error:&error];
        if (!animatedLibrary) {
            NSLog(@"Animated shader compilation failed: %@", error);
            free(renderer);
            return AFFERENT_ERROR_PIPELINE_FAILED;
        }

        // Animated rect pipeline
        id<MTLFunction> animRectVertexFunc = [animatedLibrary newFunctionWithName:@"animated_rect_vertex"];
        id<MTLFunction> animRectFragmentFunc = [animatedLibrary newFunctionWithName:@"animated_rect_fragment"];
        if (!animRectVertexFunc || !animRectFragmentFunc) {
            NSLog(@"Failed to find animated rect shader functions");
            free(renderer);
            return AFFERENT_ERROR_PIPELINE_FAILED;
        }

        MTLRenderPipelineDescriptor *animRectPipelineDesc = [[MTLRenderPipelineDescriptor alloc] init];
        animRectPipelineDesc.vertexFunction = animRectVertexFunc;
        animRectPipelineDesc.fragmentFunction = animRectFragmentFunc;
        animRectPipelineDesc.colorAttachments[0].pixelFormat = MTLPixelFormatBGRA8Unorm;
        animRectPipelineDesc.rasterSampleCount = 4;
        animRectPipelineDesc.colorAttachments[0].blendingEnabled = YES;
        animRectPipelineDesc.colorAttachments[0].sourceRGBBlendFactor = MTLBlendFactorSourceAlpha;
        animRectPipelineDesc.colorAttachments[0].destinationRGBBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
        animRectPipelineDesc.colorAttachments[0].sourceAlphaBlendFactor = MTLBlendFactorOne;
        animRectPipelineDesc.colorAttachments[0].destinationAlphaBlendFactor = MTLBlendFactorOneMinusSourceAlpha;

        renderer->animatedRectPipelineState = [renderer->device newRenderPipelineStateWithDescriptor:animRectPipelineDesc
                                                                                               error:&error];
        if (!renderer->animatedRectPipelineState) {
            NSLog(@"Animated rect pipeline creation failed: %@", error);
            free(renderer);
            return AFFERENT_ERROR_PIPELINE_FAILED;
        }

        // Animated triangle pipeline
        id<MTLFunction> animTriVertexFunc = [animatedLibrary newFunctionWithName:@"animated_triangle_vertex"];
        if (!animTriVertexFunc) {
            NSLog(@"Failed to find animated triangle vertex function");
            free(renderer);
            return AFFERENT_ERROR_PIPELINE_FAILED;
        }

        MTLRenderPipelineDescriptor *animTriPipelineDesc = [[MTLRenderPipelineDescriptor alloc] init];
        animTriPipelineDesc.vertexFunction = animTriVertexFunc;
        animTriPipelineDesc.fragmentFunction = animRectFragmentFunc;  // Same fragment shader
        animTriPipelineDesc.colorAttachments[0].pixelFormat = MTLPixelFormatBGRA8Unorm;
        animTriPipelineDesc.rasterSampleCount = 4;
        animTriPipelineDesc.colorAttachments[0].blendingEnabled = YES;
        animTriPipelineDesc.colorAttachments[0].sourceRGBBlendFactor = MTLBlendFactorSourceAlpha;
        animTriPipelineDesc.colorAttachments[0].destinationRGBBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
        animTriPipelineDesc.colorAttachments[0].sourceAlphaBlendFactor = MTLBlendFactorOne;
        animTriPipelineDesc.colorAttachments[0].destinationAlphaBlendFactor = MTLBlendFactorOneMinusSourceAlpha;

        renderer->animatedTrianglePipelineState = [renderer->device newRenderPipelineStateWithDescriptor:animTriPipelineDesc
                                                                                                   error:&error];
        if (!renderer->animatedTrianglePipelineState) {
            NSLog(@"Animated triangle pipeline creation failed: %@", error);
            free(renderer);
            return AFFERENT_ERROR_PIPELINE_FAILED;
        }

        // Animated circle pipeline
        id<MTLFunction> animCircleVertexFunc = [animatedLibrary newFunctionWithName:@"animated_circle_vertex"];
        id<MTLFunction> animCircleFragmentFunc = [animatedLibrary newFunctionWithName:@"animated_circle_fragment"];
        if (!animCircleVertexFunc || !animCircleFragmentFunc) {
            NSLog(@"Failed to find animated circle shader functions");
            free(renderer);
            return AFFERENT_ERROR_PIPELINE_FAILED;
        }

        MTLRenderPipelineDescriptor *animCirclePipelineDesc = [[MTLRenderPipelineDescriptor alloc] init];
        animCirclePipelineDesc.vertexFunction = animCircleVertexFunc;
        animCirclePipelineDesc.fragmentFunction = animCircleFragmentFunc;
        animCirclePipelineDesc.colorAttachments[0].pixelFormat = MTLPixelFormatBGRA8Unorm;
        animCirclePipelineDesc.rasterSampleCount = 4;
        animCirclePipelineDesc.colorAttachments[0].blendingEnabled = YES;
        animCirclePipelineDesc.colorAttachments[0].sourceRGBBlendFactor = MTLBlendFactorSourceAlpha;
        animCirclePipelineDesc.colorAttachments[0].destinationRGBBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
        animCirclePipelineDesc.colorAttachments[0].sourceAlphaBlendFactor = MTLBlendFactorOne;
        animCirclePipelineDesc.colorAttachments[0].destinationAlphaBlendFactor = MTLBlendFactorOneMinusSourceAlpha;

        renderer->animatedCirclePipelineState = [renderer->device newRenderPipelineStateWithDescriptor:animCirclePipelineDesc
                                                                                                 error:&error];
        if (!renderer->animatedCirclePipelineState) {
            NSLog(@"Animated circle pipeline creation failed: %@", error);
            free(renderer);
            return AFFERENT_ERROR_PIPELINE_FAILED;
        }

        // ====================================================================
        // Create orbital pipeline (particles orbiting around a center point)
        // ====================================================================
        id<MTLLibrary> orbitalLibrary = [renderer->device newLibraryWithSource:orbitalShaderSource
                                                                       options:nil
                                                                         error:&error];
        if (!orbitalLibrary) {
            NSLog(@"Orbital shader compilation failed: %@", error);
            free(renderer);
            return AFFERENT_ERROR_PIPELINE_FAILED;
        }

        id<MTLFunction> orbitalVertexFunc = [orbitalLibrary newFunctionWithName:@"orbital_rect_vertex"];
        id<MTLFunction> orbitalFragmentFunc = [orbitalLibrary newFunctionWithName:@"orbital_rect_fragment"];
        if (!orbitalVertexFunc || !orbitalFragmentFunc) {
            NSLog(@"Failed to find orbital shader functions");
            free(renderer);
            return AFFERENT_ERROR_PIPELINE_FAILED;
        }

        MTLRenderPipelineDescriptor *orbitalPipelineDesc = [[MTLRenderPipelineDescriptor alloc] init];
        orbitalPipelineDesc.vertexFunction = orbitalVertexFunc;
        orbitalPipelineDesc.fragmentFunction = orbitalFragmentFunc;
        orbitalPipelineDesc.colorAttachments[0].pixelFormat = MTLPixelFormatBGRA8Unorm;
        orbitalPipelineDesc.rasterSampleCount = 4;
        orbitalPipelineDesc.colorAttachments[0].blendingEnabled = YES;
        orbitalPipelineDesc.colorAttachments[0].sourceRGBBlendFactor = MTLBlendFactorSourceAlpha;
        orbitalPipelineDesc.colorAttachments[0].destinationRGBBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
        orbitalPipelineDesc.colorAttachments[0].sourceAlphaBlendFactor = MTLBlendFactorOne;
        orbitalPipelineDesc.colorAttachments[0].destinationAlphaBlendFactor = MTLBlendFactorOneMinusSourceAlpha;

        renderer->orbitalPipelineState = [renderer->device newRenderPipelineStateWithDescriptor:orbitalPipelineDesc
                                                                                          error:&error];
        if (!renderer->orbitalPipelineState) {
            NSLog(@"Orbital pipeline creation failed: %@", error);
            free(renderer);
            return AFFERENT_ERROR_PIPELINE_FAILED;
        }

        // Create dynamic circle pipeline
        id<MTLLibrary> dynamicCircleLibrary = [renderer->device newLibraryWithSource:dynamicCircleShaderSource
                                                                             options:nil
                                                                               error:&error];
        if (!dynamicCircleLibrary) {
            NSLog(@"Dynamic circle shader compilation failed: %@", error);
            free(renderer);
            return AFFERENT_ERROR_PIPELINE_FAILED;
        }

        id<MTLFunction> dynamicCircleVertexFunc = [dynamicCircleLibrary newFunctionWithName:@"dynamic_circle_vertex"];
        id<MTLFunction> dynamicCircleFragmentFunc = [dynamicCircleLibrary newFunctionWithName:@"dynamic_circle_fragment"];
        if (!dynamicCircleVertexFunc || !dynamicCircleFragmentFunc) {
            NSLog(@"Failed to find dynamic circle shader functions");
            free(renderer);
            return AFFERENT_ERROR_PIPELINE_FAILED;
        }

        MTLRenderPipelineDescriptor *dynamicCirclePipelineDesc = [[MTLRenderPipelineDescriptor alloc] init];
        dynamicCirclePipelineDesc.vertexFunction = dynamicCircleVertexFunc;
        dynamicCirclePipelineDesc.fragmentFunction = dynamicCircleFragmentFunc;
        dynamicCirclePipelineDesc.colorAttachments[0].pixelFormat = MTLPixelFormatBGRA8Unorm;
        dynamicCirclePipelineDesc.rasterSampleCount = 4;
        dynamicCirclePipelineDesc.colorAttachments[0].blendingEnabled = YES;
        dynamicCirclePipelineDesc.colorAttachments[0].sourceRGBBlendFactor = MTLBlendFactorSourceAlpha;
        dynamicCirclePipelineDesc.colorAttachments[0].destinationRGBBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
        dynamicCirclePipelineDesc.colorAttachments[0].sourceAlphaBlendFactor = MTLBlendFactorOne;
        dynamicCirclePipelineDesc.colorAttachments[0].destinationAlphaBlendFactor = MTLBlendFactorOneMinusSourceAlpha;

        renderer->dynamicCirclePipelineState = [renderer->device newRenderPipelineStateWithDescriptor:dynamicCirclePipelineDesc
                                                                                                error:&error];
        if (!renderer->dynamicCirclePipelineState) {
            NSLog(@"Dynamic circle pipeline creation failed: %@", error);
            free(renderer);
            return AFFERENT_ERROR_PIPELINE_FAILED;
        }

        // Create dynamic rect pipeline
        id<MTLLibrary> dynamicRectLibrary = [renderer->device newLibraryWithSource:dynamicRectShaderSource
                                                                           options:nil
                                                                             error:&error];
        if (!dynamicRectLibrary) {
            NSLog(@"Dynamic rect shader compilation failed: %@", error);
            free(renderer);
            return AFFERENT_ERROR_PIPELINE_FAILED;
        }

        id<MTLFunction> dynamicRectVertexFunc = [dynamicRectLibrary newFunctionWithName:@"dynamic_rect_vertex"];
        id<MTLFunction> dynamicRectFragmentFunc = [dynamicRectLibrary newFunctionWithName:@"dynamic_rect_fragment"];
        if (!dynamicRectVertexFunc || !dynamicRectFragmentFunc) {
            NSLog(@"Failed to find dynamic rect shader functions");
            free(renderer);
            return AFFERENT_ERROR_PIPELINE_FAILED;
        }

        MTLRenderPipelineDescriptor *dynamicRectPipelineDesc = [[MTLRenderPipelineDescriptor alloc] init];
        dynamicRectPipelineDesc.vertexFunction = dynamicRectVertexFunc;
        dynamicRectPipelineDesc.fragmentFunction = dynamicRectFragmentFunc;
        dynamicRectPipelineDesc.colorAttachments[0].pixelFormat = MTLPixelFormatBGRA8Unorm;
        dynamicRectPipelineDesc.rasterSampleCount = 4;
        dynamicRectPipelineDesc.colorAttachments[0].blendingEnabled = YES;
        dynamicRectPipelineDesc.colorAttachments[0].sourceRGBBlendFactor = MTLBlendFactorSourceAlpha;
        dynamicRectPipelineDesc.colorAttachments[0].destinationRGBBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
        dynamicRectPipelineDesc.colorAttachments[0].sourceAlphaBlendFactor = MTLBlendFactorOne;
        dynamicRectPipelineDesc.colorAttachments[0].destinationAlphaBlendFactor = MTLBlendFactorOneMinusSourceAlpha;

        renderer->dynamicRectPipelineState = [renderer->device newRenderPipelineStateWithDescriptor:dynamicRectPipelineDesc
                                                                                              error:&error];
        if (!renderer->dynamicRectPipelineState) {
            NSLog(@"Dynamic rect pipeline creation failed: %@", error);
            free(renderer);
            return AFFERENT_ERROR_PIPELINE_FAILED;
        }

        // Create dynamic triangle pipeline
        id<MTLLibrary> dynamicTriangleLibrary = [renderer->device newLibraryWithSource:dynamicTriangleShaderSource
                                                                               options:nil
                                                                                 error:&error];
        if (!dynamicTriangleLibrary) {
            NSLog(@"Dynamic triangle shader compilation failed: %@", error);
            free(renderer);
            return AFFERENT_ERROR_PIPELINE_FAILED;
        }

        id<MTLFunction> dynamicTriangleVertexFunc = [dynamicTriangleLibrary newFunctionWithName:@"dynamic_triangle_vertex"];
        id<MTLFunction> dynamicTriangleFragmentFunc = [dynamicTriangleLibrary newFunctionWithName:@"dynamic_triangle_fragment"];
        if (!dynamicTriangleVertexFunc || !dynamicTriangleFragmentFunc) {
            NSLog(@"Failed to find dynamic triangle shader functions");
            free(renderer);
            return AFFERENT_ERROR_PIPELINE_FAILED;
        }

        MTLRenderPipelineDescriptor *dynamicTrianglePipelineDesc = [[MTLRenderPipelineDescriptor alloc] init];
        dynamicTrianglePipelineDesc.vertexFunction = dynamicTriangleVertexFunc;
        dynamicTrianglePipelineDesc.fragmentFunction = dynamicTriangleFragmentFunc;
        dynamicTrianglePipelineDesc.colorAttachments[0].pixelFormat = MTLPixelFormatBGRA8Unorm;
        dynamicTrianglePipelineDesc.rasterSampleCount = 4;
        dynamicTrianglePipelineDesc.colorAttachments[0].blendingEnabled = YES;
        dynamicTrianglePipelineDesc.colorAttachments[0].sourceRGBBlendFactor = MTLBlendFactorSourceAlpha;
        dynamicTrianglePipelineDesc.colorAttachments[0].destinationRGBBlendFactor = MTLBlendFactorOneMinusSourceAlpha;
        dynamicTrianglePipelineDesc.colorAttachments[0].sourceAlphaBlendFactor = MTLBlendFactorOne;
        dynamicTrianglePipelineDesc.colorAttachments[0].destinationAlphaBlendFactor = MTLBlendFactorOneMinusSourceAlpha;

        renderer->dynamicTrianglePipelineState = [renderer->device newRenderPipelineStateWithDescriptor:dynamicTrianglePipelineDesc
                                                                                                  error:&error];
        if (!renderer->dynamicTrianglePipelineState) {
            NSLog(@"Dynamic triangle pipeline creation failed: %@", error);
            free(renderer);
            return AFFERENT_ERROR_PIPELINE_FAILED;
        }

        // Create sprite pipeline (textured quads)
        id<MTLLibrary> spriteLibrary = [renderer->device newLibraryWithSource:spriteShaderSource
                                                                      options:nil
                                                                        error:&error];
        if (!spriteLibrary) {
            NSLog(@"Sprite shader compilation failed: %@", error);
            free(renderer);
            return AFFERENT_ERROR_PIPELINE_FAILED;
        }

        id<MTLFunction> spriteVertexFunc = [spriteLibrary newFunctionWithName:@"sprite_vertex"];
        id<MTLFunction> spriteFragmentFunc = [spriteLibrary newFunctionWithName:@"sprite_fragment"];
        if (!spriteVertexFunc || !spriteFragmentFunc) {
            NSLog(@"Failed to find sprite shader functions");
            free(renderer);
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
            free(renderer);
            return AFFERENT_ERROR_PIPELINE_FAILED;
        }

        spritePipelineDesc.rasterSampleCount = 1;
        renderer->spritePipelineStateNoMSAA = [renderer->device newRenderPipelineStateWithDescriptor:spritePipelineDesc
                                                                                               error:&error];
        if (!renderer->spritePipelineStateNoMSAA) {
            NSLog(@"Sprite pipeline creation failed (no MSAA): %@", error);
            free(renderer);
            return AFFERENT_ERROR_PIPELINE_FAILED;
        }

        renderer->spritePipelineState = renderer->spritePipelineStateMSAA;

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
            free(renderer);
            return AFFERENT_ERROR_PIPELINE_FAILED;
        }

        id<MTLFunction> vertex3DFunction = [library3D newFunctionWithName:@"vertex_main_3d"];
        id<MTLFunction> vertexOceanFunction = [library3D newFunctionWithName:@"vertex_ocean_projected_waves"];
        id<MTLFunction> fragment3DFunction = [library3D newFunctionWithName:@"fragment_main_3d"];

        if (!vertex3DFunction || !vertexOceanFunction || !fragment3DFunction) {
            NSLog(@"Failed to find 3D shader functions");
            free(renderer);
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
            free(renderer);
            return AFFERENT_ERROR_PIPELINE_FAILED;
        }

        pipeline3DDesc.rasterSampleCount = 1;  // No MSAA
        renderer->pipeline3DNoMSAA = [renderer->device newRenderPipelineStateWithDescriptor:pipeline3DDesc
                                                                                      error:&error];
        if (!renderer->pipeline3DNoMSAA) {
            NSLog(@"3D pipeline creation failed (no MSAA): %@", error);
            free(renderer);
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
            free(renderer);
            return AFFERENT_ERROR_PIPELINE_FAILED;
        }

        pipelineOceanDesc.rasterSampleCount = 1;  // No MSAA
        renderer->pipeline3DOceanNoMSAA = [renderer->device newRenderPipelineStateWithDescriptor:pipelineOceanDesc
                                                                                           error:&error];
        if (!renderer->pipeline3DOceanNoMSAA) {
            NSLog(@"Ocean pipeline creation failed (no MSAA): %@", error);
            free(renderer);
            return AFFERENT_ERROR_PIPELINE_FAILED;
        }

        renderer->pipeline3DOcean = renderer->pipeline3DOceanMSAA;
        renderer->oceanIndexBuffer = nil;
        renderer->oceanIndexCount = 0;
        renderer->oceanGridSize = 0;

        // Initialize depth texture pointers
        renderer->depthTexture = nil;
        renderer->msaaDepthTexture = nil;
        renderer->depthWidth = 0;
        renderer->depthHeight = 0;

        // Initialize animated buffer pointers to nil
        renderer->animatedRectBuffer = nil;
        renderer->animatedTriangleBuffer = nil;
        renderer->animatedCircleBuffer = nil;
        renderer->orbitalBuffer = nil;
        renderer->animatedRectCount = 0;
        renderer->animatedTriangleCount = 0;
        renderer->animatedCircleCount = 0;
        renderer->orbitalCount = 0;
        renderer->orbitalCenterX = 0;
        renderer->orbitalCenterY = 0;

        *out_renderer = renderer;
        return AFFERENT_OK;
    }
}

void afferent_renderer_destroy(AfferentRendererRef renderer) {
    if (renderer) {
        free(renderer);
    }
}

// Toggle MSAA for subsequent frames. This only switches the active pipelines and
// beginFrame render pass configuration; it doesn't rebuild resources.
void afferent_renderer_set_msaa_enabled(AfferentRendererRef renderer, bool enabled) {
    if (!renderer) return;
    renderer->msaaEnabled = enabled;
    renderer->pipelineState = enabled ? renderer->pipelineStateMSAA : renderer->pipelineStateNoMSAA;
    renderer->textPipelineState = enabled ? renderer->textPipelineStateMSAA : renderer->textPipelineStateNoMSAA;
    renderer->spritePipelineState = enabled ? renderer->spritePipelineStateMSAA : renderer->spritePipelineStateNoMSAA;
    renderer->pipeline3D = enabled ? renderer->pipeline3DMSAA : renderer->pipeline3DNoMSAA;
    renderer->pipeline3DOcean = enabled ? renderer->pipeline3DOceanMSAA : renderer->pipeline3DOceanNoMSAA;
}

// Enable a drawable scale override (typically 1.0 to disable Retina).
// Pass scale <= 0 to restore native backing scale.
void afferent_renderer_set_drawable_scale(AfferentRendererRef renderer, float scale) {
    if (!renderer) return;
    renderer->drawableScaleOverride = scale;
    CAMetalLayer *metalLayer = afferent_window_get_metal_layer(renderer->window);
    if (!metalLayer) return;
    CGSize boundsSize = metalLayer.bounds.size;
    if (scale > 0.0f) {
        metalLayer.drawableSize = CGSizeMake(boundsSize.width * scale, boundsSize.height * scale);
    } else {
        CGFloat nativeScale = metalLayer.contentsScale;
        if (nativeScale <= 0.0) nativeScale = 1.0;
        metalLayer.drawableSize = CGSizeMake(boundsSize.width * nativeScale, boundsSize.height * nativeScale);
    }
}

// Helper function to create or recreate MSAA texture if needed
static void ensureMSAATexture(AfferentRendererRef renderer, NSUInteger width, NSUInteger height) {
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
static void ensureDepthTexture(AfferentRendererRef renderer, NSUInteger width, NSUInteger height, bool msaa) {
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

AfferentResult afferent_renderer_begin_frame(AfferentRendererRef renderer, float r, float g, float b, float a) {
    @autoreleasepool {
        // Reset buffer pool at frame start - all buffers become available for reuse
        pool_reset_frame();

        CAMetalLayer *metalLayer = afferent_window_get_metal_layer(renderer->window);
        if (!metalLayer) {
            return AFFERENT_ERROR_INIT_FAILED;
        }

        // Re-apply drawable scale override each frame (handles window resizes)
        if (renderer->drawableScaleOverride > 0.0f) {
            CGSize boundsSize = metalLayer.bounds.size;
            float s = renderer->drawableScaleOverride;
            metalLayer.drawableSize = CGSizeMake(boundsSize.width * s, boundsSize.height * s);
        }

        renderer->currentDrawable = [metalLayer nextDrawable];
        if (!renderer->currentDrawable) {
            return AFFERENT_ERROR_INIT_FAILED;
        }

        renderer->currentCommandBuffer = [renderer->commandQueue commandBuffer];
        if (!renderer->currentCommandBuffer) {
            return AFFERENT_ERROR_INIT_FAILED;
        }

        id<MTLTexture> drawableTexture = renderer->currentDrawable.texture;

        // Store screen dimensions for text rendering
        renderer->screenWidth = drawableTexture.width;
        renderer->screenHeight = drawableTexture.height;

        MTLRenderPassDescriptor *passDesc = [MTLRenderPassDescriptor renderPassDescriptor];
        passDesc.colorAttachments[0].loadAction = MTLLoadActionClear;
        passDesc.colorAttachments[0].clearColor = MTLClearColorMake(r, g, b, a);

        if (renderer->msaaEnabled) {
            // Ensure MSAA texture matches drawable size
            ensureMSAATexture(renderer, drawableTexture.width, drawableTexture.height);
            // Ensure MSAA depth texture
            ensureDepthTexture(renderer, drawableTexture.width, drawableTexture.height, true);
            // Render to MSAA texture and resolve to drawable
            passDesc.colorAttachments[0].texture = renderer->msaaTexture;
            passDesc.colorAttachments[0].resolveTexture = drawableTexture;
            passDesc.colorAttachments[0].storeAction = MTLStoreActionMultisampleResolve;
            // Attach depth buffer for 3D rendering
            passDesc.depthAttachment.texture = renderer->msaaDepthTexture;
            passDesc.depthAttachment.loadAction = MTLLoadActionClear;
            passDesc.depthAttachment.storeAction = MTLStoreActionDontCare;
            passDesc.depthAttachment.clearDepth = 1.0;
        } else {
            // Ensure non-MSAA depth texture
            ensureDepthTexture(renderer, drawableTexture.width, drawableTexture.height, false);
            // Render directly to drawable without MSAA
            passDesc.colorAttachments[0].texture = drawableTexture;
            passDesc.colorAttachments[0].resolveTexture = nil;
            passDesc.colorAttachments[0].storeAction = MTLStoreActionStore;
            // Attach depth buffer for 3D rendering
            passDesc.depthAttachment.texture = renderer->depthTexture;
            passDesc.depthAttachment.loadAction = MTLLoadActionClear;
            passDesc.depthAttachment.storeAction = MTLStoreActionDontCare;
            passDesc.depthAttachment.clearDepth = 1.0;
        }

        renderer->currentEncoder = [renderer->currentCommandBuffer renderCommandEncoderWithDescriptor:passDesc];
        if (!renderer->currentEncoder) {
            return AFFERENT_ERROR_INIT_FAILED;
        }

        [renderer->currentEncoder setRenderPipelineState:renderer->pipelineState];

        return AFFERENT_OK;
    }
}

AfferentResult afferent_renderer_end_frame(AfferentRendererRef renderer) {
    @autoreleasepool {
        if (renderer->currentEncoder) {
            [renderer->currentEncoder endEncoding];
            renderer->currentEncoder = nil;
        }

        if (renderer->currentCommandBuffer && renderer->currentDrawable) {
            [renderer->currentCommandBuffer presentDrawable:renderer->currentDrawable];
            [renderer->currentCommandBuffer commit];
        }

        renderer->currentCommandBuffer = nil;
        renderer->currentDrawable = nil;

        return AFFERENT_OK;
    }
}

AfferentResult afferent_buffer_create_vertex(
    AfferentRendererRef renderer,
    const AfferentVertex* vertices,
    uint32_t vertex_count,
    AfferentBufferRef* out_buffer
) {
    @autoreleasepool {
        size_t required_size = vertex_count * sizeof(AfferentVertex);

        // Get a buffer from the pool (or create a new one)
        id<MTLBuffer> mtlBuffer = pool_acquire_buffer(
            renderer->device,
            g_buffer_pool.vertex_pool,
            &g_buffer_pool.vertex_pool_count,
            required_size,
            true
        );

        if (!mtlBuffer) {
            return AFFERENT_ERROR_BUFFER_FAILED;
        }

        // Copy vertex data into the pooled buffer
        memcpy(mtlBuffer.contents, vertices, required_size);

        // Get wrapper struct from pool (avoids malloc per draw call)
        struct AfferentBuffer *buffer = pool_acquire_wrapper();
        buffer->count = vertex_count;
        buffer->mtlBuffer = mtlBuffer;
        *out_buffer = buffer;
        return AFFERENT_OK;
    }
}

AfferentResult afferent_buffer_create_index(
    AfferentRendererRef renderer,
    const uint32_t* indices,
    uint32_t index_count,
    AfferentBufferRef* out_buffer
) {
    @autoreleasepool {
        size_t required_size = index_count * sizeof(uint32_t);

        // Get a buffer from the pool (or create a new one)
        id<MTLBuffer> mtlBuffer = pool_acquire_buffer(
            renderer->device,
            g_buffer_pool.index_pool,
            &g_buffer_pool.index_pool_count,
            required_size,
            false
        );

        if (!mtlBuffer) {
            return AFFERENT_ERROR_BUFFER_FAILED;
        }

        // Copy index data into the pooled buffer
        memcpy(mtlBuffer.contents, indices, required_size);

        // Get wrapper struct from pool (avoids malloc per draw call)
        struct AfferentBuffer *buffer = pool_acquire_wrapper();
        buffer->count = index_count;
        buffer->mtlBuffer = mtlBuffer;
        *out_buffer = buffer;
        return AFFERENT_OK;
    }
}

void afferent_buffer_destroy(AfferentBufferRef buffer) {
    // Note: We don't destroy anything here anymore.
    // MTLBuffers stay in the pool for reuse, and wrapper structs
    // are pooled and recycled at frame boundaries.
    // This function is kept for API compatibility but is now a no-op.
    (void)buffer;
}

void afferent_renderer_draw_triangles(
    AfferentRendererRef renderer,
    AfferentBufferRef vertex_buffer,
    AfferentBufferRef index_buffer,
    uint32_t index_count
) {
    if (!renderer->currentEncoder || !vertex_buffer || !index_buffer) {
        return;
    }

    // Ensure we're using the basic pipeline (not text pipeline)
    [renderer->currentEncoder setRenderPipelineState:renderer->pipelineState];

    [renderer->currentEncoder setVertexBuffer:vertex_buffer->mtlBuffer offset:0 atIndex:0];

    [renderer->currentEncoder drawIndexedPrimitives:MTLPrimitiveTypeTriangle
                                         indexCount:index_count
                                          indexType:MTLIndexTypeUInt32
                                        indexBuffer:index_buffer->mtlBuffer
                                  indexBufferOffset:0];
}

// Draw instanced rectangles - GPU computes transforms
// instance_data: array of 9 floats per instance (pos.x, pos.y, sin, cos, halfSize, r, g, b, a)
void afferent_renderer_draw_instanced_rects(
    AfferentRendererRef renderer,
    const float* instance_data,
    uint32_t instance_count
) {
    if (!renderer || !renderer->currentEncoder || !instance_data || instance_count == 0) {
        return;
    }

    @autoreleasepool {
        // Create buffer for instance data
        size_t data_size = instance_count * sizeof(InstanceData);
        id<MTLBuffer> instanceBuffer = pool_acquire_buffer(
            renderer->device,
            g_buffer_pool.vertex_pool,
            &g_buffer_pool.vertex_pool_count,
            data_size,
            true
        );

        if (!instanceBuffer) {
            NSLog(@"Failed to create instance buffer");
            return;
        }

        // Copy instance data
        memcpy(instanceBuffer.contents, instance_data, data_size);

        // Switch to instanced pipeline
        [renderer->currentEncoder setRenderPipelineState:renderer->instancedPipelineState];

        // Bind instance buffer
        [renderer->currentEncoder setVertexBuffer:instanceBuffer offset:0 atIndex:0];

        // Draw: 4 vertices per quad (triangle strip would be 4, but we use 2 triangles = 6 indices)
        // Actually we use drawPrimitives with triangle strip for simplicity
        [renderer->currentEncoder drawPrimitives:MTLPrimitiveTypeTriangleStrip
                                     vertexStart:0
                                     vertexCount:4
                                   instanceCount:instance_count];

        // Switch back to basic pipeline
        [renderer->currentEncoder setRenderPipelineState:renderer->pipelineState];
    }
}

// Draw instanced triangles - GPU computes transforms
void afferent_renderer_draw_instanced_triangles(
    AfferentRendererRef renderer,
    const float* instance_data,
    uint32_t instance_count
) {
    if (!renderer || !renderer->currentEncoder || !instance_data || instance_count == 0) {
        return;
    }

    @autoreleasepool {
        size_t data_size = instance_count * sizeof(InstanceData);
        id<MTLBuffer> instanceBuffer = pool_acquire_buffer(
            renderer->device,
            g_buffer_pool.vertex_pool,
            &g_buffer_pool.vertex_pool_count,
            data_size,
            true
        );

        if (!instanceBuffer) {
            NSLog(@"Failed to create triangle instance buffer");
            return;
        }

        memcpy(instanceBuffer.contents, instance_data, data_size);

        [renderer->currentEncoder setRenderPipelineState:renderer->trianglePipelineState];
        [renderer->currentEncoder setVertexBuffer:instanceBuffer offset:0 atIndex:0];

        // Draw: 3 vertices per triangle
        [renderer->currentEncoder drawPrimitives:MTLPrimitiveTypeTriangle
                                     vertexStart:0
                                     vertexCount:3
                                   instanceCount:instance_count];

        [renderer->currentEncoder setRenderPipelineState:renderer->pipelineState];
    }
}

// Draw instanced circles - smooth circles via fragment shader
void afferent_renderer_draw_instanced_circles(
    AfferentRendererRef renderer,
    const float* instance_data,
    uint32_t instance_count
) {
    if (!renderer || !renderer->currentEncoder || !instance_data || instance_count == 0) {
        return;
    }

    @autoreleasepool {
        size_t data_size = instance_count * sizeof(InstanceData);
        id<MTLBuffer> instanceBuffer = pool_acquire_buffer(
            renderer->device,
            g_buffer_pool.vertex_pool,
            &g_buffer_pool.vertex_pool_count,
            data_size,
            true
        );

        if (!instanceBuffer) {
            NSLog(@"Failed to create circle instance buffer");
            return;
        }

        memcpy(instanceBuffer.contents, instance_data, data_size);

        [renderer->currentEncoder setRenderPipelineState:renderer->circlePipelineState];
        [renderer->currentEncoder setVertexBuffer:instanceBuffer offset:0 atIndex:0];

        // Draw: 4 vertices per quad (triangle strip)
        [renderer->currentEncoder drawPrimitives:MTLPrimitiveTypeTriangleStrip
                                     vertexStart:0
                                     vertexCount:4
                                   instanceCount:instance_count];

        [renderer->currentEncoder setRenderPipelineState:renderer->pipelineState];
    }
}

void afferent_renderer_set_scissor(
    AfferentRendererRef renderer,
    uint32_t x,
    uint32_t y,
    uint32_t width,
    uint32_t height
) {
    if (!renderer || !renderer->currentEncoder) {
        return;
    }

    // Clamp scissor to render target bounds
    NSUInteger maxW = (NSUInteger)renderer->screenWidth;
    NSUInteger maxH = (NSUInteger)renderer->screenHeight;

    NSUInteger sx = (NSUInteger)x;
    NSUInteger sy = (NSUInteger)y;
    NSUInteger sw = (NSUInteger)width;
    NSUInteger sh = (NSUInteger)height;

    // Ensure scissor doesn't exceed render target
    if (sx + sw > maxW) sw = maxW - sx;
    if (sy + sh > maxH) sh = maxH - sy;

    MTLScissorRect scissor;
    scissor.x = sx;
    scissor.y = sy;
    scissor.width = sw;
    scissor.height = sh;

    [renderer->currentEncoder setScissorRect:scissor];
}

void afferent_renderer_reset_scissor(AfferentRendererRef renderer) {
    if (!renderer || !renderer->currentEncoder) {
        return;
    }

    // Reset to full drawable size
    MTLScissorRect scissor;
    scissor.x = 0;
    scissor.y = 0;
    scissor.width = (NSUInteger)renderer->screenWidth;
    scissor.height = (NSUInteger)renderer->screenHeight;
    [renderer->currentEncoder setScissorRect:scissor];
}

// External functions from text_render.c
extern uint8_t* afferent_font_get_atlas_data(AfferentFontRef font);
extern uint32_t afferent_font_get_atlas_width(AfferentFontRef font);
extern uint32_t afferent_font_get_atlas_height(AfferentFontRef font);
extern void* afferent_font_get_metal_texture(AfferentFontRef font);
extern void afferent_font_set_metal_texture(AfferentFontRef font, void* texture);
extern int afferent_text_generate_vertices(
    AfferentFontRef font,
    const char* text,
    float x, float y,
    float r, float g, float b, float a,
    float screen_width, float screen_height,
    const float* transform,
    float** out_vertices,
    uint32_t** out_indices,
    uint32_t* out_vertex_count,
    uint32_t* out_index_count
);

// Create or update font atlas texture
static id<MTLTexture> ensureFontTexture(AfferentRendererRef renderer, AfferentFontRef font) {
    void* stored_texture = afferent_font_get_metal_texture(font);
    id<MTLTexture> texture = (__bridge id<MTLTexture>)stored_texture;

    if (!texture) {
        // Create texture from atlas data
        uint8_t* atlas_data = afferent_font_get_atlas_data(font);
        uint32_t atlas_width = afferent_font_get_atlas_width(font);
        uint32_t atlas_height = afferent_font_get_atlas_height(font);

        MTLTextureDescriptor *desc = [MTLTextureDescriptor texture2DDescriptorWithPixelFormat:MTLPixelFormatR8Unorm
                                                                                        width:atlas_width
                                                                                       height:atlas_height
                                                                                    mipmapped:NO];
        desc.usage = MTLTextureUsageShaderRead;

        texture = [renderer->device newTextureWithDescriptor:desc];

        MTLRegion region = MTLRegionMake2D(0, 0, atlas_width, atlas_height);
        [texture replaceRegion:region mipmapLevel:0 withBytes:atlas_data bytesPerRow:atlas_width];

        // Use __bridge_retained to transfer ownership to the C struct
        // This prevents ARC from releasing the texture when the function returns
        afferent_font_set_metal_texture(font, (__bridge_retained void*)texture);
    }

    return texture;
}

// Update the font texture with new glyph data (only if atlas has changed)
static void updateFontTexture(AfferentRendererRef renderer, AfferentFontRef font) {
    // Only upload if new glyphs were added to the atlas
    if (!afferent_font_atlas_dirty(font)) {
        return;
    }

    id<MTLTexture> texture = (__bridge id<MTLTexture>)afferent_font_get_metal_texture(font);
    if (texture) {
        uint8_t* atlas_data = afferent_font_get_atlas_data(font);
        uint32_t atlas_width = afferent_font_get_atlas_width(font);
        uint32_t atlas_height = afferent_font_get_atlas_height(font);

        MTLRegion region = MTLRegionMake2D(0, 0, atlas_width, atlas_height);
        [texture replaceRegion:region mipmapLevel:0 withBytes:atlas_data bytesPerRow:atlas_width];

        // Clear dirty flag after successful upload
        afferent_font_atlas_clear_dirty(font);
    }
}

// Render text using the text pipeline
AfferentResult afferent_text_render(
    AfferentRendererRef renderer,
    AfferentFontRef font,
    const char* text,
    float x,
    float y,
    float r,
    float g,
    float b,
    float a,
    const float* transform,
    float canvas_width,
    float canvas_height
) {
    @autoreleasepool {
        if (!renderer || !renderer->currentEncoder || !font || !text || text[0] == '\0') {
            return AFFERENT_OK;  // Nothing to render
        }

        // Generate vertex data
        float* vertices = NULL;
        uint32_t* indices = NULL;
        uint32_t vertex_count = 0;
        uint32_t index_count = 0;

        // Use the canvas dimensions (not current drawable size) for NDC conversion
        // This ensures text scales consistently with shapes when the window is resized
        int success = afferent_text_generate_vertices(
            font, text, x, y, r, g, b, a,
            canvas_width, canvas_height,
            transform,
            &vertices, &indices, &vertex_count, &index_count
        );

        if (!success || vertex_count == 0) {
            free(vertices);
            free(indices);
            return AFFERENT_OK;
        }

        // Ensure font texture is created and up to date
        id<MTLTexture> fontTexture = ensureFontTexture(renderer, font);
        updateFontTexture(renderer, font);

        // Ensure staging buffer is large enough (grows as needed, never shrinks)
        if (vertex_count > g_text_vertex_staging_capacity) {
            free(g_text_vertex_staging);
            g_text_vertex_staging_capacity = vertex_count + 64;  // Add some headroom
            g_text_vertex_staging = malloc(g_text_vertex_staging_capacity * sizeof(TextVertex));
        }

        // Convert float vertex data to TextVertex format using staging buffer
        TextVertex* textVertices = g_text_vertex_staging;
        for (uint32_t i = 0; i < vertex_count; i++) {
            size_t base = i * 8;  // 8 floats per vertex
            textVertices[i].position[0] = vertices[base + 0];
            textVertices[i].position[1] = vertices[base + 1];
            textVertices[i].texCoord[0] = vertices[base + 2];
            textVertices[i].texCoord[1] = vertices[base + 3];
            textVertices[i].color[0] = vertices[base + 4];
            textVertices[i].color[1] = vertices[base + 5];
            textVertices[i].color[2] = vertices[base + 6];
            textVertices[i].color[3] = vertices[base + 7];
        }

        // Use pooled Metal buffers instead of creating fresh ones each call
        size_t vertex_buffer_size = vertex_count * sizeof(TextVertex);
        size_t index_buffer_size = index_count * sizeof(uint32_t);

        id<MTLBuffer> vertexBuffer = pool_acquire_buffer(
            renderer->device,
            g_buffer_pool.text_vertex_pool,
            &g_buffer_pool.text_vertex_pool_count,
            vertex_buffer_size,
            true
        );
        id<MTLBuffer> indexBuffer = pool_acquire_buffer(
            renderer->device,
            g_buffer_pool.text_index_pool,
            &g_buffer_pool.text_index_pool_count,
            index_buffer_size,
            false
        );

        // Copy data into pooled buffers
        if (vertexBuffer) {
            memcpy(vertexBuffer.contents, textVertices, vertex_buffer_size);
        }
        if (indexBuffer) {
            memcpy(indexBuffer.contents, indices, index_buffer_size);
        }

        // Free the vertex/index data generated by afferent_text_generate_vertices
        // (staging buffer is kept for reuse)
        free(vertices);
        free(indices);

        if (!vertexBuffer || !indexBuffer) {
            return AFFERENT_ERROR_TEXT_FAILED;
        }

        // Switch to text pipeline and disable depth testing for 2D text
        [renderer->currentEncoder setRenderPipelineState:renderer->textPipelineState];
        [renderer->currentEncoder setDepthStencilState:renderer->depthStateDisabled];

        // Set texture and sampler
        [renderer->currentEncoder setFragmentTexture:fontTexture atIndex:0];
        [renderer->currentEncoder setFragmentSamplerState:renderer->textSampler atIndex:0];

        // Draw text quads
        [renderer->currentEncoder setVertexBuffer:vertexBuffer offset:0 atIndex:0];
        [renderer->currentEncoder drawIndexedPrimitives:MTLPrimitiveTypeTriangle
                                             indexCount:index_count
                                              indexType:MTLIndexTypeUInt32
                                            indexBuffer:indexBuffer
                                      indexBufferOffset:0];

        // Switch back to basic pipeline for subsequent drawing
        [renderer->currentEncoder setRenderPipelineState:renderer->pipelineState];

        return AFFERENT_OK;
    }
}

// Helper to get renderer screen dimensions (for Lean FFI)
float afferent_renderer_get_screen_width(AfferentRendererRef renderer) {
    return renderer ? renderer->screenWidth : 0;
}

float afferent_renderer_get_screen_height(AfferentRendererRef renderer) {
    return renderer ? renderer->screenHeight : 0;
}

// Release a retained Metal texture (called from text_render.c when font is destroyed)
void afferent_release_metal_texture(void* texture_ptr) {
    if (texture_ptr) {
        // Transfer ownership back to ARC so it can release the texture
        id<MTLTexture> texture = (__bridge_transfer id<MTLTexture>)texture_ptr;
        (void)texture;  // Let ARC release it
    }
}

// ============================================================================
// ANIMATED RENDERING - GPU-side animation for maximum performance
// Static data uploaded once, only time uniform sent per frame
// ============================================================================

// Upload static instance data for animated rects (called once at startup)
// data: [pixelX, pixelY, hueBase, halfSizePixels, phaseOffset, spinSpeed] × count
void afferent_renderer_upload_animated_rects(
    AfferentRendererRef renderer,
    const float* data,
    uint32_t count
) {
    if (!renderer || !data || count == 0) return;

    @autoreleasepool {
        size_t size = count * sizeof(AnimatedInstanceData);
        renderer->animatedRectBuffer = [renderer->device newBufferWithBytes:data
                                                                     length:size
                                                                    options:MTLResourceStorageModeShared];
        renderer->animatedRectCount = count;
    }
}

// Upload static instance data for animated triangles
void afferent_renderer_upload_animated_triangles(
    AfferentRendererRef renderer,
    const float* data,
    uint32_t count
) {
    if (!renderer || !data || count == 0) return;

    @autoreleasepool {
        size_t size = count * sizeof(AnimatedInstanceData);
        renderer->animatedTriangleBuffer = [renderer->device newBufferWithBytes:data
                                                                         length:size
                                                                        options:MTLResourceStorageModeShared];
        renderer->animatedTriangleCount = count;
    }
}

// Upload static instance data for animated circles
void afferent_renderer_upload_animated_circles(
    AfferentRendererRef renderer,
    const float* data,
    uint32_t count
) {
    if (!renderer || !data || count == 0) return;

    @autoreleasepool {
        size_t size = count * sizeof(AnimatedInstanceData);
        renderer->animatedCircleBuffer = [renderer->device newBufferWithBytes:data
                                                                       length:size
                                                                      options:MTLResourceStorageModeShared];
        renderer->animatedCircleCount = count;
    }
}

// Draw animated rects (called every frame - only sends time uniform!)
void afferent_renderer_draw_animated_rects(
    AfferentRendererRef renderer,
    float time
) {
    if (!renderer || !renderer->currentEncoder || !renderer->animatedRectBuffer || renderer->animatedRectCount == 0) {
        return;
    }

    @autoreleasepool {
        // Prepare uniforms (just 16 bytes!)
        AnimationUniforms uniforms = {
            .time = time,
            .canvasWidth = renderer->screenWidth,
            .canvasHeight = renderer->screenHeight,
            .padding = 0
        };

        [renderer->currentEncoder setRenderPipelineState:renderer->animatedRectPipelineState];
        [renderer->currentEncoder setVertexBuffer:renderer->animatedRectBuffer offset:0 atIndex:0];
        [renderer->currentEncoder setVertexBytes:&uniforms length:sizeof(uniforms) atIndex:1];
        [renderer->currentEncoder drawPrimitives:MTLPrimitiveTypeTriangleStrip
                                     vertexStart:0
                                     vertexCount:4
                                   instanceCount:renderer->animatedRectCount];
        [renderer->currentEncoder setRenderPipelineState:renderer->pipelineState];
    }
}

// Draw animated triangles (called every frame)
void afferent_renderer_draw_animated_triangles(
    AfferentRendererRef renderer,
    float time
) {
    if (!renderer || !renderer->currentEncoder || !renderer->animatedTriangleBuffer || renderer->animatedTriangleCount == 0) {
        return;
    }

    @autoreleasepool {
        AnimationUniforms uniforms = {
            .time = time,
            .canvasWidth = renderer->screenWidth,
            .canvasHeight = renderer->screenHeight,
            .padding = 0
        };

        [renderer->currentEncoder setRenderPipelineState:renderer->animatedTrianglePipelineState];
        [renderer->currentEncoder setVertexBuffer:renderer->animatedTriangleBuffer offset:0 atIndex:0];
        [renderer->currentEncoder setVertexBytes:&uniforms length:sizeof(uniforms) atIndex:1];
        [renderer->currentEncoder drawPrimitives:MTLPrimitiveTypeTriangle
                                     vertexStart:0
                                     vertexCount:3
                                   instanceCount:renderer->animatedTriangleCount];
        [renderer->currentEncoder setRenderPipelineState:renderer->pipelineState];
    }
}

// Draw animated circles (called every frame)
void afferent_renderer_draw_animated_circles(
    AfferentRendererRef renderer,
    float time
) {
    if (!renderer || !renderer->currentEncoder || !renderer->animatedCircleBuffer || renderer->animatedCircleCount == 0) {
        return;
    }

    @autoreleasepool {
        AnimationUniforms uniforms = {
            .time = time,
            .canvasWidth = renderer->screenWidth,
            .canvasHeight = renderer->screenHeight,
            .padding = 0
        };

        [renderer->currentEncoder setRenderPipelineState:renderer->animatedCirclePipelineState];
        [renderer->currentEncoder setVertexBuffer:renderer->animatedCircleBuffer offset:0 atIndex:0];
        [renderer->currentEncoder setVertexBytes:&uniforms length:sizeof(uniforms) atIndex:1];
        [renderer->currentEncoder drawPrimitives:MTLPrimitiveTypeTriangleStrip
                                     vertexStart:0
                                     vertexCount:4
                                   instanceCount:renderer->animatedCircleCount];
        [renderer->currentEncoder setRenderPipelineState:renderer->pipelineState];
    }
}

// ============================================================================
// ORBITAL RENDERING - Particles orbiting around a center point
// Position computed on GPU from orbital parameters
// ============================================================================

// Upload static orbital instance data (called once at startup)
// data: [phase, baseRadius, orbitSpeed, phaseX3, phase2, hueBase, halfSizePixels, padding] × count
void afferent_renderer_upload_orbital_particles(
    AfferentRendererRef renderer,
    const float* data,
    uint32_t count,
    float centerX,
    float centerY
) {
    if (!renderer || !data || count == 0) return;

    @autoreleasepool {
        size_t size = count * sizeof(OrbitalInstanceData);
        renderer->orbitalBuffer = [renderer->device newBufferWithBytes:data
                                                                length:size
                                                               options:MTLResourceStorageModeShared];
        renderer->orbitalCount = count;
        renderer->orbitalCenterX = centerX;
        renderer->orbitalCenterY = centerY;
    }
}

// Draw orbital particles (called every frame - only sends time and uniforms!)
void afferent_renderer_draw_orbital_particles(
    AfferentRendererRef renderer,
    float time
) {
    if (!renderer || !renderer->currentEncoder || !renderer->orbitalBuffer || renderer->orbitalCount == 0) {
        return;
    }

    @autoreleasepool {
        OrbitalUniforms uniforms = {
            .time = time,
            .centerX = renderer->orbitalCenterX,
            .centerY = renderer->orbitalCenterY,
            .canvasWidth = renderer->screenWidth,
            .canvasHeight = renderer->screenHeight,
            .radiusWobble = 30.0f,
            .padding1 = 0,
            .padding2 = 0
        };

        [renderer->currentEncoder setRenderPipelineState:renderer->orbitalPipelineState];
        [renderer->currentEncoder setVertexBuffer:renderer->orbitalBuffer offset:0 atIndex:0];
        [renderer->currentEncoder setVertexBytes:&uniforms length:sizeof(uniforms) atIndex:1];
        [renderer->currentEncoder drawPrimitives:MTLPrimitiveTypeTriangleStrip
                                     vertexStart:0
                                     vertexCount:4
                                   instanceCount:renderer->orbitalCount];
        [renderer->currentEncoder setRenderPipelineState:renderer->pipelineState];
    }
}

// Draw dynamic circles (positions updated each frame, GPU does color + NDC)
// data: [pixelX, pixelY, hueBase, radiusPixels] × count (4 floats per circle)
void afferent_renderer_draw_dynamic_circles(
    AfferentRendererRef renderer,
    const float* data,
    uint32_t count,
    float time,
    float canvasWidth,
    float canvasHeight
) {
    if (!renderer || !renderer->currentEncoder || !data || count == 0) {
        return;
    }

    @autoreleasepool {
        size_t dataSize = count * sizeof(DynamicCircleData);
        id<MTLBuffer> circleBuffer = pool_acquire_buffer(
            renderer->device,
            g_buffer_pool.vertex_pool,
            &g_buffer_pool.vertex_pool_count,
            dataSize,
            true
        );

        if (!circleBuffer) {
            return;
        }

        memcpy(circleBuffer.contents, data, dataSize);

        DynamicCircleUniforms uniforms = {
            .time = time,
            .canvasWidth = canvasWidth,
            .canvasHeight = canvasHeight,
            .hueSpeed = 0.2f
        };

        [renderer->currentEncoder setRenderPipelineState:renderer->dynamicCirclePipelineState];
        [renderer->currentEncoder setVertexBuffer:circleBuffer offset:0 atIndex:0];
        [renderer->currentEncoder setVertexBytes:&uniforms length:sizeof(uniforms) atIndex:1];
        [renderer->currentEncoder drawPrimitives:MTLPrimitiveTypeTriangleStrip
                                     vertexStart:0
                                     vertexCount:4
                                   instanceCount:count];
        [renderer->currentEncoder setRenderPipelineState:renderer->pipelineState];
    }
}

// Draw dynamic rects (positions/rotation updated each frame, GPU does color + NDC)
// data: [pixelX, pixelY, hueBase, halfSizePixels, rotation] × count (5 floats per rect)
void afferent_renderer_draw_dynamic_rects(
    AfferentRendererRef renderer,
    const float* data,
    uint32_t count,
    float time,
    float canvasWidth,
    float canvasHeight
) {
    if (!renderer || !renderer->currentEncoder || !data || count == 0) {
        return;
    }

    @autoreleasepool {
        // Create temporary buffer for this frame's rect data
        size_t dataSize = count * sizeof(DynamicRectData);
        id<MTLBuffer> rectBuffer = [renderer->device newBufferWithBytes:data
                                                                 length:dataSize
                                                                options:MTLResourceStorageModeShared];

        DynamicRectUniforms uniforms = {
            .time = time,
            .canvasWidth = canvasWidth,
            .canvasHeight = canvasHeight,
            .hueSpeed = 0.2f
        };

        [renderer->currentEncoder setRenderPipelineState:renderer->dynamicRectPipelineState];
        [renderer->currentEncoder setVertexBuffer:rectBuffer offset:0 atIndex:0];
        [renderer->currentEncoder setVertexBytes:&uniforms length:sizeof(uniforms) atIndex:1];
        [renderer->currentEncoder drawPrimitives:MTLPrimitiveTypeTriangleStrip
                                     vertexStart:0
                                     vertexCount:4
                                   instanceCount:count];
        [renderer->currentEncoder setRenderPipelineState:renderer->pipelineState];
    }
}

// Draw dynamic triangles (positions/rotation updated each frame, GPU does color + NDC)
// data: [pixelX, pixelY, hueBase, halfSizePixels, rotation] × count (5 floats per triangle)
void afferent_renderer_draw_dynamic_triangles(
    AfferentRendererRef renderer,
    const float* data,
    uint32_t count,
    float time,
    float canvasWidth,
    float canvasHeight
) {
    if (!renderer || !renderer->currentEncoder || !data || count == 0) {
        return;
    }

    @autoreleasepool {
        // Create temporary buffer for this frame's triangle data
        size_t dataSize = count * sizeof(DynamicTriangleData);
        id<MTLBuffer> triangleBuffer = [renderer->device newBufferWithBytes:data
                                                                     length:dataSize
                                                                    options:MTLResourceStorageModeShared];

        DynamicTriangleUniforms uniforms = {
            .time = time,
            .canvasWidth = canvasWidth,
            .canvasHeight = canvasHeight,
            .hueSpeed = 0.2f
        };

        [renderer->currentEncoder setRenderPipelineState:renderer->dynamicTrianglePipelineState];
        [renderer->currentEncoder setVertexBuffer:triangleBuffer offset:0 atIndex:0];
        [renderer->currentEncoder setVertexBytes:&uniforms length:sizeof(uniforms) atIndex:1];
        [renderer->currentEncoder drawPrimitives:MTLPrimitiveTypeTriangle
                                     vertexStart:0
                                     vertexCount:3
                                   instanceCount:count];
        [renderer->currentEncoder setRenderPipelineState:renderer->pipelineState];
    }
}

// ============================================================================
// SPRITE/TEXTURE RENDERING - Textured quads with transparency
// ============================================================================

// External declarations from texture.c
extern const uint8_t* afferent_texture_get_data(AfferentTextureRef texture);
extern void afferent_texture_get_size(AfferentTextureRef texture, uint32_t* width, uint32_t* height);
extern void* afferent_texture_get_metal_texture(AfferentTextureRef texture);
extern void afferent_texture_set_metal_texture(AfferentTextureRef texture, void* metal_tex);

// Create a Metal texture from raw RGBA pixel data
static id<MTLTexture> createMetalTexture(id<MTLDevice> device, const uint8_t* data, uint32_t width, uint32_t height) {
    MTLTextureDescriptor *desc = [MTLTextureDescriptor texture2DDescriptorWithPixelFormat:MTLPixelFormatRGBA8Unorm
                                                                                    width:width
                                                                                   height:height
                                                                                mipmapped:YES];
    // Keep this conservative: shader-read is required; render-target helps some drivers/tools with mip generation paths.
    desc.usage = MTLTextureUsageShaderRead | MTLTextureUsageRenderTarget;
    desc.storageMode = MTLStorageModeManaged;

    id<MTLTexture> texture = [device newTextureWithDescriptor:desc];
    if (!texture) return nil;

    MTLRegion region = MTLRegionMake2D(0, 0, width, height);
    [texture replaceRegion:region
               mipmapLevel:0
                 withBytes:data
               bytesPerRow:width * 4];

    // Generate mip chain on CPU once (avoids needing a blit encoder mid-frame).
    // This matters a lot when drawing many minified sprites from a large source texture.
    const uint8_t* prev = data;
    uint8_t* prevOwned = NULL;
    uint32_t prevW = width;
    uint32_t prevH = height;

    uint32_t mipCount = (uint32_t)texture.mipmapLevelCount;
    for (uint32_t level = 1; level < mipCount; level++) {
        uint32_t nextW = prevW > 1 ? (prevW / 2) : 1;
        uint32_t nextH = prevH > 1 ? (prevH / 2) : 1;

        size_t nextSize = (size_t)nextW * (size_t)nextH * 4;
        uint8_t* next = (uint8_t*)malloc(nextSize);
        if (!next) {
            break;
        }

        for (uint32_t y = 0; y < nextH; y++) {
            uint32_t sy0 = (2 * y);
            uint32_t sy1 = (sy0 + 1 < prevH) ? (sy0 + 1) : (prevH - 1);
            for (uint32_t x = 0; x < nextW; x++) {
                uint32_t sx0 = (2 * x);
                uint32_t sx1 = (sx0 + 1 < prevW) ? (sx0 + 1) : (prevW - 1);

                const uint8_t* p00 = prev + ((size_t)sy0 * (size_t)prevW + (size_t)sx0) * 4;
                const uint8_t* p10 = prev + ((size_t)sy0 * (size_t)prevW + (size_t)sx1) * 4;
                const uint8_t* p01 = prev + ((size_t)sy1 * (size_t)prevW + (size_t)sx0) * 4;
                const uint8_t* p11 = prev + ((size_t)sy1 * (size_t)prevW + (size_t)sx1) * 4;

                uint32_t r = (uint32_t)p00[0] + (uint32_t)p10[0] + (uint32_t)p01[0] + (uint32_t)p11[0];
                uint32_t g = (uint32_t)p00[1] + (uint32_t)p10[1] + (uint32_t)p01[1] + (uint32_t)p11[1];
                uint32_t b = (uint32_t)p00[2] + (uint32_t)p10[2] + (uint32_t)p01[2] + (uint32_t)p11[2];
                uint32_t a = (uint32_t)p00[3] + (uint32_t)p10[3] + (uint32_t)p01[3] + (uint32_t)p11[3];

                uint8_t* dst = next + ((size_t)y * (size_t)nextW + (size_t)x) * 4;
                dst[0] = (uint8_t)(r >> 2);
                dst[1] = (uint8_t)(g >> 2);
                dst[2] = (uint8_t)(b >> 2);
                dst[3] = (uint8_t)(a >> 2);
            }
        }

        MTLRegion mipRegion = MTLRegionMake2D(0, 0, nextW, nextH);
        [texture replaceRegion:mipRegion
                   mipmapLevel:level
                     withBytes:next
                   bytesPerRow:nextW * 4];

        if (prevOwned) {
            free(prevOwned);
        }
        prev = next;
        prevOwned = next;
        prevW = nextW;
        prevH = nextH;
    }

    if (prevOwned) {
        free(prevOwned);
    }

    return texture;
}

// Draw textured sprites (positions/rotation updated each frame)
// data: [pixelX, pixelY, rotation, halfSizePixels, alpha] × count (5 floats per sprite)
void afferent_renderer_draw_sprites(
    AfferentRendererRef renderer,
    AfferentTextureRef texture,
    const float* data,
    uint32_t count,
    float canvasWidth,
    float canvasHeight
) {
    if (!renderer || !renderer->currentEncoder || !texture || !data || count == 0) {
        return;
    }

    @autoreleasepool {
        // Get or create Metal texture for this sprite
        id<MTLTexture> metalTex = (__bridge id<MTLTexture>)afferent_texture_get_metal_texture(texture);

        if (!metalTex) {
            // Create Metal texture from pixel data
            const uint8_t* pixelData = afferent_texture_get_data(texture);
            uint32_t width, height;
            afferent_texture_get_size(texture, &width, &height);

            if (!pixelData || width == 0 || height == 0) {
                return;
            }

            metalTex = createMetalTexture(renderer->device, pixelData, width, height);
            if (!metalTex) {
                return;
            }

            // Store for future use (transfer ownership via __bridge_retained)
            afferent_texture_set_metal_texture(texture, (__bridge_retained void*)metalTex);
        }

        // Acquire pooled buffer for this frame's sprite data
        size_t dataSize = count * sizeof(SpriteInstanceData);
        id<MTLBuffer> spriteBuffer = pool_acquire_buffer(
            renderer->device,
            g_buffer_pool.vertex_pool,
            &g_buffer_pool.vertex_pool_count,
            dataSize,
            true
        );

        if (!spriteBuffer) {
            NSLog(@"Failed to acquire sprite instance buffer");
            return;
        }

        memcpy(spriteBuffer.contents, data, dataSize);

        SpriteUniforms uniforms = {
            .canvasWidth = canvasWidth,
            .canvasHeight = canvasHeight
        };

        [renderer->currentEncoder setRenderPipelineState:renderer->spritePipelineState];
        [renderer->currentEncoder setVertexBuffer:spriteBuffer offset:0 atIndex:0];
        [renderer->currentEncoder setVertexBytes:&uniforms length:sizeof(uniforms) atIndex:1];
        [renderer->currentEncoder setFragmentTexture:metalTex atIndex:0];
        [renderer->currentEncoder setFragmentSamplerState:renderer->spriteSampler atIndex:0];
        [renderer->currentEncoder drawPrimitives:MTLPrimitiveTypeTriangleStrip
                                     vertexStart:0
                                     vertexCount:4
                                   instanceCount:count];
        [renderer->currentEncoder setRenderPipelineState:renderer->pipelineState];
    }
}

// Draw sprites from FloatBuffer that already contains SpriteInstanceData layout
void afferent_renderer_draw_sprites_instance_buffer(
    AfferentRendererRef renderer,
    AfferentTextureRef texture,
    const float* data,
    uint32_t count,
    float canvasWidth,
    float canvasHeight
) {
    // Same layout as afferent_renderer_draw_sprites, so forward directly
    afferent_renderer_draw_sprites(renderer, texture, data, count, canvasWidth, canvasHeight);
}

// Release Metal texture associated with an AfferentTexture (called when texture is destroyed)
void afferent_release_sprite_metal_texture(AfferentTextureRef texture) {
    if (!texture) return;

    void* metalTexPtr = afferent_texture_get_metal_texture(texture);
    if (metalTexPtr) {
        // Release the Metal texture (transfer back ownership with __bridge_transfer)
        id<MTLTexture> metalTex = (__bridge_transfer id<MTLTexture>)metalTexPtr;
        metalTex = nil;  // ARC will release
        afferent_texture_set_metal_texture(texture, NULL);
    }
}

// Draw sprites from FloatBuffer using physics layout.
// Buffer layout: [x, y, vx, vy, rotation] per sprite (5 floats).
// Converted on CPU into SpriteInstanceData with uniform halfSize and alpha=1.0.
void afferent_renderer_draw_sprites_buffer(
    AfferentRendererRef renderer,
    AfferentTextureRef texture,
    const float* data,
    uint32_t count,
    float halfSize,
    float canvasWidth,
    float canvasHeight
) {
    if (!renderer || !renderer->currentEncoder || !texture || !data || count == 0) {
        return;
    }

    @autoreleasepool {
        // Get or create Metal texture
        id<MTLTexture> metalTex = (__bridge id<MTLTexture>)afferent_texture_get_metal_texture(texture);

        if (!metalTex) {
            const uint8_t* pixelData = afferent_texture_get_data(texture);
            uint32_t width, height;
            afferent_texture_get_size(texture, &width, &height);

            if (!pixelData || width == 0 || height == 0) {
                return;
            }

            metalTex = createMetalTexture(renderer->device, pixelData, width, height);
            if (!metalTex) {
                return;
            }

            afferent_texture_set_metal_texture(texture, (__bridge_retained void*)metalTex);
        }

        // Convert physics layout [x, y, vx, vy, rotation] -> SpriteInstanceData
        size_t instanceSize = count * sizeof(SpriteInstanceData);
        id<MTLBuffer> spriteBuffer = pool_acquire_buffer(
            renderer->device,
            g_buffer_pool.vertex_pool,
            &g_buffer_pool.vertex_pool_count,
            instanceSize,
            true
        );

        if (!spriteBuffer) {
            NSLog(@"Failed to acquire sprite buffer");
            return;
        }

        SpriteInstanceData* instances = (SpriteInstanceData*)spriteBuffer.contents;
        for (uint32_t i = 0; i < count; i++) {
            const float* src = data + i * 5;
            instances[i].pixelX = src[0];
            instances[i].pixelY = src[1];
            instances[i].rotation = src[4];
            instances[i].halfSizePixels = halfSize;
            instances[i].alpha = 1.0f;
        }

        SpriteUniforms uniforms = {
            .canvasWidth = canvasWidth,
            .canvasHeight = canvasHeight
        };

        [renderer->currentEncoder setRenderPipelineState:renderer->spritePipelineState];
        [renderer->currentEncoder setVertexBuffer:spriteBuffer offset:0 atIndex:0];
        [renderer->currentEncoder setVertexBytes:&uniforms length:sizeof(uniforms) atIndex:1];
        [renderer->currentEncoder setFragmentTexture:metalTex atIndex:0];
        [renderer->currentEncoder setFragmentSamplerState:renderer->spriteSampler atIndex:0];
        [renderer->currentEncoder drawPrimitives:MTLPrimitiveTypeTriangleStrip
                                     vertexStart:0
                                     vertexCount:4
                                   instanceCount:count];
        [renderer->currentEncoder setRenderPipelineState:renderer->pipelineState];
    }
}

// ============================================================================
// 3D Mesh Rendering
// ============================================================================
static void ensure_ocean_index_buffer(AfferentRendererRef renderer, uint32_t gridSize) {
    if (!renderer || gridSize < 2) return;
    if (renderer->oceanIndexBuffer && renderer->oceanGridSize == gridSize) return;

    uint32_t quadsPerRow = gridSize - 1;
    uint32_t quadCount = quadsPerRow * quadsPerRow;
    uint32_t indexCount = quadCount * 6;
    size_t indexSize = (size_t)indexCount * sizeof(uint32_t);

    uint32_t* indices = (uint32_t*)malloc(indexSize);
    if (!indices) {
        NSLog(@"Failed to allocate ocean index buffer");
        return;
    }

    uint32_t w = gridSize;
    uint32_t idx = 0;
    for (uint32_t row = 0; row < gridSize - 1; row++) {
        for (uint32_t col = 0; col < gridSize - 1; col++) {
            uint32_t topLeft = row * w + col;
            uint32_t topRight = topLeft + 1;
            uint32_t bottomLeft = (row + 1) * w + col;
            uint32_t bottomRight = bottomLeft + 1;

            indices[idx++] = topLeft;
            indices[idx++] = bottomLeft;
            indices[idx++] = topRight;

            indices[idx++] = topRight;
            indices[idx++] = bottomLeft;
            indices[idx++] = bottomRight;
        }
    }

    id<MTLBuffer> indexBuffer = [renderer->device newBufferWithBytes:indices
                                                              length:indexSize
                                                             options:MTLResourceStorageModeShared];
    free(indices);
    if (!indexBuffer) {
        NSLog(@"Failed to create ocean index MTLBuffer");
        return;
    }

    renderer->oceanIndexBuffer = indexBuffer;
    renderer->oceanIndexCount = indexCount;
    renderer->oceanGridSize = gridSize;
}

void afferent_renderer_draw_ocean_projected_grid_with_fog(
    AfferentRendererRef renderer,
    uint32_t grid_size,
    const float* mvp_matrix,
    const float* model_matrix,
    const float* light_dir,
    float ambient,
    const float* camera_pos,
    const float* fog_color,
    float fog_start,
    float fog_end,
    float time,
    float fovY,
    float aspect,
    float maxDistance,
    float snapSize,
    float overscanNdc,
    float horizonMargin,
    float yaw,
    float pitch,
    const float* wave_params,
    uint32_t wave_param_count
) {
    if (!renderer || !renderer->currentEncoder || !mvp_matrix || !model_matrix ||
        !light_dir || !camera_pos || !fog_color || grid_size < 2) {
        return;
    }

    ensure_ocean_index_buffer(renderer, grid_size);
    if (!renderer->oceanIndexBuffer || renderer->oceanIndexCount == 0) return;

    @autoreleasepool {
        OceanProjectedUniforms uniforms;
        memset(&uniforms, 0, sizeof(uniforms));

        memcpy(uniforms.scene.modelViewProj, mvp_matrix, 64);
        memcpy(uniforms.scene.modelMatrix, model_matrix, 64);
        memcpy(uniforms.scene.lightDir, light_dir, 12);
        uniforms.scene.ambient = ambient;
        memcpy(uniforms.scene.cameraPos, camera_pos, 12);
        uniforms.scene.fogStart = fog_start;
        memcpy(uniforms.scene.fogColor, fog_color, 12);
        uniforms.scene.fogEnd = fog_end;

        uniforms.params0[0] = time;
        uniforms.params0[1] = fovY;
        uniforms.params0[2] = aspect;
        uniforms.params0[3] = maxDistance;

        uniforms.params1[0] = snapSize;
        uniforms.params1[1] = overscanNdc;
        uniforms.params1[2] = horizonMargin;
        uniforms.params1[3] = yaw;

        uniforms.params2[0] = pitch;
        uniforms.params2[1] = (float)grid_size;
        // Reserved (was used for local patch). Keep 0 so shader stays in projected-grid-only mode.
        uniforms.params2[2] = 0.0f;
        uniforms.params2[3] = 0.0f;

        if (wave_params && wave_param_count >= 32) {
            for (uint32_t i = 0; i < 4; i++) {
                for (uint32_t j = 0; j < 4; j++) {
                    uniforms.waveA[i][j] = wave_params[i * 4 + j];
                    uniforms.waveB[i][j] = wave_params[16 + i * 4 + j];
                }
            }
        }

        [renderer->currentEncoder setRenderPipelineState:renderer->pipeline3DOcean];
        [renderer->currentEncoder setDepthStencilState:renderer->depthState];
        [renderer->currentEncoder setFragmentBytes:&uniforms.scene length:sizeof(uniforms.scene) atIndex:0];
        [renderer->currentEncoder setVertexBytes:&uniforms length:sizeof(uniforms) atIndex:1];
        [renderer->currentEncoder drawIndexedPrimitives:MTLPrimitiveTypeTriangle
                                             indexCount:renderer->oceanIndexCount
                                              indexType:MTLIndexTypeUInt32
                                            indexBuffer:renderer->oceanIndexBuffer
                                      indexBufferOffset:0];

        [renderer->currentEncoder setRenderPipelineState:renderer->pipelineState];
    }
}

void afferent_renderer_draw_mesh_3d(
    AfferentRendererRef renderer,
    const AfferentVertex3D* vertices,
    uint32_t vertex_count,
    const uint32_t* indices,
    uint32_t index_count,
    const float* mvp_matrix,
    const float* model_matrix,
    const float* light_dir,
    float ambient
) {
    if (!renderer || !renderer->currentEncoder || !vertices || !indices ||
        vertex_count == 0 || index_count == 0) {
        return;
    }

    @autoreleasepool {
        // Acquire temporary vertex buffer (pooled)
        size_t vertex_size = vertex_count * sizeof(AfferentVertex3D);
        id<MTLBuffer> vertexBuffer = pool_acquire_buffer(
            renderer->device,
            g_buffer_pool.vertex_pool,
            &g_buffer_pool.vertex_pool_count,
            vertex_size,
            true
        );
        if (!vertexBuffer) {
            NSLog(@"Failed to create 3D vertex buffer");
            return;
        }
        memcpy(vertexBuffer.contents, vertices, vertex_size);

        // Acquire temporary index buffer (pooled)
        size_t index_size = index_count * sizeof(uint32_t);
        id<MTLBuffer> indexBuffer = pool_acquire_buffer(
            renderer->device,
            g_buffer_pool.index_pool,
            &g_buffer_pool.index_pool_count,
            index_size,
            false
        );
        if (!indexBuffer) {
            NSLog(@"Failed to create 3D index buffer");
            return;
        }
        memcpy(indexBuffer.contents, indices, index_size);

        // Set up uniforms
        Scene3DUniforms uniforms;
        memcpy(uniforms.modelViewProj, mvp_matrix, 64);
        memcpy(uniforms.modelMatrix, model_matrix, 64);
        memcpy(uniforms.lightDir, light_dir, 12);
        uniforms.ambient = ambient;
        // Default: no fog (start=end=0 disables fog in shader)
        uniforms.cameraPos[0] = 0.0f;
        uniforms.cameraPos[1] = 0.0f;
        uniforms.cameraPos[2] = 0.0f;
        uniforms.fogStart = 0.0f;
        uniforms.fogEnd = 0.0f;
        uniforms.fogColor[0] = 0.5f;
        uniforms.fogColor[1] = 0.5f;
        uniforms.fogColor[2] = 0.5f;

        // Configure encoder for 3D rendering
        [renderer->currentEncoder setRenderPipelineState:renderer->pipeline3D];
        [renderer->currentEncoder setDepthStencilState:renderer->depthState];
        [renderer->currentEncoder setVertexBuffer:vertexBuffer offset:0 atIndex:0];
        [renderer->currentEncoder setVertexBytes:&uniforms length:sizeof(uniforms) atIndex:1];
        [renderer->currentEncoder setFragmentBytes:&uniforms length:sizeof(uniforms) atIndex:0];

        // Draw indexed triangles
        [renderer->currentEncoder drawIndexedPrimitives:MTLPrimitiveTypeTriangle
                                             indexCount:index_count
                                              indexType:MTLIndexTypeUInt32
                                            indexBuffer:indexBuffer
                                      indexBufferOffset:0];

        // Restore default pipeline
        [renderer->currentEncoder setRenderPipelineState:renderer->pipelineState];
    }
}

// 3D Mesh Rendering with fog parameters
void afferent_renderer_draw_mesh_3d_with_fog(
    AfferentRendererRef renderer,
    const AfferentVertex3D* vertices,
    uint32_t vertex_count,
    const uint32_t* indices,
    uint32_t index_count,
    const float* mvp_matrix,
    const float* model_matrix,
    const float* light_dir,
    float ambient,
    const float* camera_pos,
    const float* fog_color,
    float fog_start,
    float fog_end
) {
    if (!renderer || !renderer->currentEncoder || !vertices || !indices ||
        vertex_count == 0 || index_count == 0) {
        return;
    }

    @autoreleasepool {
        // Acquire temporary vertex buffer (pooled)
        size_t vertex_size = vertex_count * sizeof(AfferentVertex3D);
        id<MTLBuffer> vertexBuffer = pool_acquire_buffer(
            renderer->device,
            g_buffer_pool.vertex_pool,
            &g_buffer_pool.vertex_pool_count,
            vertex_size,
            true
        );
        if (!vertexBuffer) {
            NSLog(@"Failed to create 3D vertex buffer (fog)");
            return;
        }
        memcpy(vertexBuffer.contents, vertices, vertex_size);

        // Acquire temporary index buffer (pooled)
        size_t index_size = index_count * sizeof(uint32_t);
        id<MTLBuffer> indexBuffer = pool_acquire_buffer(
            renderer->device,
            g_buffer_pool.index_pool,
            &g_buffer_pool.index_pool_count,
            index_size,
            false
        );
        if (!indexBuffer) {
            NSLog(@"Failed to create 3D index buffer (fog)");
            return;
        }
        memcpy(indexBuffer.contents, indices, index_size);

        // Set up uniforms with fog parameters
        Scene3DUniforms uniforms;
        memcpy(uniforms.modelViewProj, mvp_matrix, 64);
        memcpy(uniforms.modelMatrix, model_matrix, 64);
        memcpy(uniforms.lightDir, light_dir, 12);
        uniforms.ambient = ambient;
        memcpy(uniforms.cameraPos, camera_pos, 12);
        uniforms.fogStart = fog_start;
        memcpy(uniforms.fogColor, fog_color, 12);
        uniforms.fogEnd = fog_end;

        // Configure encoder for 3D rendering
        [renderer->currentEncoder setRenderPipelineState:renderer->pipeline3D];
        [renderer->currentEncoder setDepthStencilState:renderer->depthState];
        [renderer->currentEncoder setVertexBuffer:vertexBuffer offset:0 atIndex:0];
        [renderer->currentEncoder setVertexBytes:&uniforms length:sizeof(uniforms) atIndex:1];
        [renderer->currentEncoder setFragmentBytes:&uniforms length:sizeof(uniforms) atIndex:0];

        // Draw indexed triangles
        [renderer->currentEncoder drawIndexedPrimitives:MTLPrimitiveTypeTriangle
                                             indexCount:index_count
                                              indexType:MTLIndexTypeUInt32
                                            indexBuffer:indexBuffer
                                      indexBufferOffset:0];

        // Restore default pipeline
        [renderer->currentEncoder setRenderPipelineState:renderer->pipelineState];
    }
}

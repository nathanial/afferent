// shaders.m - Metal shader string definitions
#import "shaders.h"

// Shader source embedded in code - basic colored vertices
NSString *shaderSource = @R"(
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
NSString *textShaderSource = @R"(
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
NSString *instancedShaderSource = @R"(
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
NSString *animatedShaderSource = @R"(
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
NSString *orbitalShaderSource = @R"(
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
NSString *dynamicCircleShaderSource = @R"(
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
NSString *dynamicRectShaderSource = @R"(
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
NSString *dynamicTriangleShaderSource = @R"(
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

NSString *spriteShaderSource = @R"(
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
NSString *shader3DSource = @R"(
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

    // Make overscan sensitive to wave direction relative to camera.
    // If waves are aligned with camera forward, horizontal displacement tends to pull/push geometry along
    // the view direction, which most strongly reveals gaps near the bottom/foreground when looking down.
    float2 fwdXZ0 = float2(fwd.x, fwd.z);
    float2 rightXZ0 = float2(right.x, right.z);
    float fwdXZLen = length(fwdXZ0);
    float rightXZLen = length(rightXZ0);
    float2 fwdXZ = (fwdXZLen > eps) ? (fwdXZ0 / fwdXZLen) : float2(0.0, -1.0);
    float2 rightXZ = (rightXZLen > eps) ? (rightXZ0 / rightXZLen) : float2(1.0, 0.0);

    float forwardDisp = 0.0;
    float sideDisp = 0.0;
    for (uint i = 0; i < 4; i++) {
        float2 wdir = float2(u.waveA[i].x, u.waveA[i].y);
        float amplitude = u.waveB[i].x;
        forwardDisp += amplitude * abs(dot(wdir, fwdXZ));
        sideDisp += amplitude * abs(dot(wdir, rightXZ));
    }
    float forwardAlign = (maxWaveAmp > eps) ? clamp(forwardDisp / maxWaveAmp, 0.0, 1.0) : 0.0;
    float sideAlign = (maxWaveAmp > eps) ? clamp(sideDisp / maxWaveAmp, 0.0, 1.0) : 0.0;

    float extraAllNdc = clamp(ampOverHeight * 0.45, 0.0, 4.0);
    extraAllNdc *= (1.0 + 0.35 * forwardAlign);
    float overscanEff = overscanNdc + extraAllNdc;

    float extraBottomNdc = clamp(ampOverHeight * (2.8 + 3.0 * pitchDown), 0.0, 30.0);
    float extraSideNdc = clamp(ampOverHeight * (1.2 + 1.5 * pitchDown), 0.0, 12.0);
    float extraTopNdc = clamp(ampOverHeight * (0.8 + 0.8 * pitchDown), 0.0, 8.0);
    extraBottomNdc *= (1.0 + 2.5 * forwardAlign);
    extraSideNdc *= (1.0 + 1.5 * sideAlign);

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
    float expandScale = 2.0 + 3.0 * pitchDown + 0.35 * ampHeightFactor + 1.5 * forwardAlign + 0.75 * sideAlign;
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

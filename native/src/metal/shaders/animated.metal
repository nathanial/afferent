// animated.metal - GPU-side animation for maximum performance
// Static instance data uploaded once, only time uniform sent per frame
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

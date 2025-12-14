// dynamic_rect.metal - Rects with CPU-updated positions/rotation, GPU color/NDC
// 5 floats per instance: [pixelX, pixelY, hueBase, halfSizePixels, rotation]
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

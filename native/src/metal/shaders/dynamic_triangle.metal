// dynamic_triangle.metal - Triangles with CPU-updated positions/rotation, GPU color/NDC
// 5 floats per instance: [pixelX, pixelY, hueBase, halfSizePixels, rotation]
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

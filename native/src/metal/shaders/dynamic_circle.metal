// dynamic_circle.metal - Circles with CPU-updated positions, GPU color/NDC
// Positions updated each frame, but HSV->RGB and pixel->NDC done on GPU
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

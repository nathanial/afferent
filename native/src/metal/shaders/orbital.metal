// orbital.metal - Particles orbiting around a center point
// Position computed on GPU from orbital parameters
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

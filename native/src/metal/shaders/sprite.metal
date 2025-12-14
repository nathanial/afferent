// sprite.metal - Textured quad with rotation and alpha
// Data format: [pixelX, pixelY, rotation, halfSize, alpha] x count (5 floats)
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

// sprite.metal - Textured instanced quad (sprites)
// Layout: [pixelX, pixelY, rotation, halfSize, alpha] × count (5 floats)
#include <metal_stdlib>
using namespace metal;

// Unit quad positions and UVs (triangle strip order)
constant float2 kSpritePositions[4] = {
    float2(-1.0, -1.0),  // Bottom-left
    float2( 1.0, -1.0),  // Bottom-right
    float2(-1.0,  1.0),  // Top-left
    float2( 1.0,  1.0)   // Top-right
};
constant float2 kSpriteUVs[4] = {
    float2(0.0, 1.0),    // Bottom-left
    float2(1.0, 1.0),    // Bottom-right
    float2(0.0, 0.0),    // Top-left
    float2(1.0, 0.0)     // Top-right
};

struct SpriteUniforms {
    float2 viewport;
};

struct SpriteInstanceData {
    float pixelX;
    float pixelY;
    float rotation;
    float halfSizePixels;
    float alpha;
};

struct SpriteVertexOut {
    float4 position [[position]];
    float2 uv;
    float alpha;
};

static inline float2 rotate_local(float2 v, float angle) {
    float sinA = sin(angle);
    float cosA = cos(angle);
    return float2(
        v.x * cosA - v.y * sinA,
        v.x * sinA + v.y * cosA
    );
}

// Layout: fixed sprite path (scalar halfSize).
vertex SpriteVertexOut sprite_vertex_layout0(
    uint vid [[vertex_id]],
    uint iid [[instance_id]],
    const device SpriteInstanceData* instances [[buffer(0)]],
    constant SpriteUniforms& uniforms [[buffer(1)]]
) {
    SpriteInstanceData inst = instances[iid];
    float2 v = kSpritePositions[vid];

    float2 rotated = rotate_local(v, inst.rotation);
    float2 ndcPos = float2(
        (inst.pixelX / uniforms.viewport.x) * 2.0 - 1.0,
        1.0 - (inst.pixelY / uniforms.viewport.y) * 2.0
    );
    float2 ndcHalfSize = float2(
        inst.halfSizePixels / uniforms.viewport.x * 2.0,
        inst.halfSizePixels / uniforms.viewport.y * 2.0
    );
    float2 finalPos = ndcPos + rotated * ndcHalfSize;

    SpriteVertexOut out;
    out.position = float4(finalPos, 0.0, 1.0);
    out.uv = kSpriteUVs[vid];
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

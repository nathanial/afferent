// sprite.metal - Textured instanced quad (sprites + sub-rects)
// Layout 0 (sprites): [pixelX, pixelY, rotation, halfSize, alpha] × count (5 floats)
// Layout 1 (textured): [pixelX, pixelY, rotation, halfSizeX, halfSizeY, u0, v0, u1, v1, alpha] × count (10 floats)
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
    uint layout;       // 0 = sprite, 1 = textured (uv rect per instance)
    uint useMatrix;    // 0 = pixel -> NDC, 1 = use transform matrix
    float4 uvRect;     // u0, v0, u1, v1 (used for layout 0)
    float4 transform0; // [a, b, c, d]
    float4 transform1; // [tx, ty, 0, 0]
};

struct SpriteInstanceData {
    float pixelX;
    float pixelY;
    float rotation;
    float halfSizePixels;
    float alpha;
};

struct TexturedSpriteInstanceData {
    float pixelX;
    float pixelY;
    float rotation;
    float halfSizeX;
    float halfSizeY;
    float u0;
    float v0;
    float u1;
    float v1;
    float alpha;
};

struct SpriteVertexOut {
    float4 position [[position]];
    float2 uv;
    float alpha;
};

static inline float2 apply_affine(float2 p, constant SpriteUniforms& u) {
    return float2(
        u.transform0.x * p.x + u.transform0.z * p.y + u.transform1.x,
        u.transform0.y * p.x + u.transform0.w * p.y + u.transform1.y
    );
}

static inline float2 apply_linear(float2 v, constant SpriteUniforms& u) {
    return float2(
        u.transform0.x * v.x + u.transform0.z * v.y,
        u.transform0.y * v.x + u.transform0.w * v.y
    );
}

static inline float2 rotate_local(float2 v, float angle) {
    float sinA = sin(angle);
    float cosA = cos(angle);
    return float2(
        v.x * cosA - v.y * sinA,
        v.x * sinA + v.y * cosA
    );
}

// Layout 0: specialized sprite path (fixed layout, scalar halfSize).
vertex SpriteVertexOut sprite_vertex_layout0(
    uint vid [[vertex_id]],
    uint iid [[instance_id]],
    const device SpriteInstanceData* instances [[buffer(0)]],
    constant SpriteUniforms& uniforms [[buffer(1)]]
) {
    SpriteInstanceData inst = instances[iid];
    float2 v = kSpritePositions[vid];

    float2 rotated = rotate_local(v, inst.rotation);
    float2 finalPos;
    if (uniforms.useMatrix == 0) {
        // Convert pixel -> NDC
        float2 ndcPos = float2(
            (inst.pixelX / uniforms.viewport.x) * 2.0 - 1.0,
            1.0 - (inst.pixelY / uniforms.viewport.y) * 2.0
        );
        float2 ndcHalfSize = float2(
            inst.halfSizePixels / uniforms.viewport.x * 2.0,
            inst.halfSizePixels / uniforms.viewport.y * 2.0
        );
        finalPos = ndcPos + rotated * ndcHalfSize;
    } else {
        float2 base = apply_affine(float2(inst.pixelX, inst.pixelY), uniforms);
        float2 offset = rotated * inst.halfSizePixels;
        finalPos = base + apply_linear(offset, uniforms);
    }

    SpriteVertexOut out;
    out.position = float4(finalPos, 0.0, 1.0);
    out.uv = kSpriteUVs[vid];
    out.alpha = inst.alpha;
    return out;
}

// Layout 1: textured quads with per-instance size and UV rect.
vertex SpriteVertexOut sprite_vertex_layout1(
    uint vid [[vertex_id]],
    uint iid [[instance_id]],
    const device TexturedSpriteInstanceData* instances [[buffer(0)]],
    constant SpriteUniforms& uniforms [[buffer(1)]]
) {
    TexturedSpriteInstanceData inst = instances[iid];
    float2 v = kSpritePositions[vid];
    float2 uvBasis = kSpriteUVs[vid];

    // Apply rotation with non-uniform size.
    float2 local = float2(v.x * inst.halfSizeX, v.y * inst.halfSizeY);
    float2 rotated = rotate_local(local, inst.rotation);
    float2 finalPos;
    if (uniforms.useMatrix == 0) {
        // Convert pixel -> NDC
        float2 ndcPos = float2(
            (inst.pixelX / uniforms.viewport.x) * 2.0 - 1.0,
            1.0 - (inst.pixelY / uniforms.viewport.y) * 2.0
        );
        float2 ndcOffset = float2(
            rotated.x * 2.0 / uniforms.viewport.x,
            rotated.y * 2.0 / uniforms.viewport.y
        );
        finalPos = ndcPos + ndcOffset;
    } else {
        float2 base = apply_affine(float2(inst.pixelX, inst.pixelY), uniforms);
        finalPos = base + apply_linear(rotated, uniforms);
    }

    float2 uv = float2(
        mix(inst.u0, inst.u1, uvBasis.x),
        mix(inst.v0, inst.v1, uvBasis.y)
    );

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

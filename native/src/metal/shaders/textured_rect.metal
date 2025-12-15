// textured_rect.metal - Textured rectangle with source/dest rectangles
// Used for map tile rendering with cropping and scaling
#include <metal_stdlib>
using namespace metal;

struct TexturedRectUniforms {
    // Source rectangle in texture pixels
    float srcX;
    float srcY;
    float srcW;
    float srcH;
    // Destination rectangle in screen pixels
    float dstX;
    float dstY;
    float dstW;
    float dstH;
    // Texture dimensions (for UV conversion)
    float texWidth;
    float texHeight;
    // Canvas dimensions (for NDC conversion)
    float canvasWidth;
    float canvasHeight;
    // Alpha
    float alpha;
};

struct TexturedRectVertexOut {
    float4 position [[position]];
    float2 uv;
    float alpha;
};

vertex TexturedRectVertexOut textured_rect_vertex(
    uint vid [[vertex_id]],
    constant TexturedRectUniforms& uniforms [[buffer(0)]]
) {
    // Unit quad positions (triangle strip order)
    // 0: top-left, 1: top-right, 2: bottom-left, 3: bottom-right
    float2 positions[4] = {
        float2(0.0, 0.0),  // Top-left
        float2(1.0, 0.0),  // Top-right
        float2(0.0, 1.0),  // Bottom-left
        float2(1.0, 1.0)   // Bottom-right
    };

    float2 p = positions[vid];

    // Convert destination rect to NDC
    // Screen coords: origin at top-left, Y increases downward
    // NDC: origin at center, Y increases upward
    float x = uniforms.dstX + p.x * uniforms.dstW;
    float y = uniforms.dstY + p.y * uniforms.dstH;

    float ndcX = (x / uniforms.canvasWidth) * 2.0 - 1.0;
    float ndcY = 1.0 - (y / uniforms.canvasHeight) * 2.0;

    // Convert source rect to UV coordinates (0-1 range)
    // Texture origin is top-left, UV (0,0) is top-left
    float u = (uniforms.srcX + p.x * uniforms.srcW) / uniforms.texWidth;
    float v = (uniforms.srcY + p.y * uniforms.srcH) / uniforms.texHeight;

    TexturedRectVertexOut out;
    out.position = float4(ndcX, ndcY, 0.0, 1.0);
    out.uv = float2(u, v);
    out.alpha = uniforms.alpha;
    return out;
}

fragment float4 textured_rect_fragment(
    TexturedRectVertexOut in [[stage_in]],
    texture2d<float> tex [[texture(0)]],
    sampler samp [[sampler(0)]]
) {
    float4 color = tex.sample(samp, in.uv);
    color.a *= in.alpha;
    // Premultiplied alpha discard for transparency
    if (color.a < 0.01) discard_fragment();
    return color;
}

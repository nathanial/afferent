// stroke.metal - Screen-space stroke extrusion shader
#include <metal_stdlib>
using namespace metal;

struct StrokeVertexIn {
    float2 position [[attribute(0)]];
    float2 normal [[attribute(1)]];
    float side [[attribute(2)]];
};

struct StrokeUniforms {
    float2 viewport;
    float halfWidth;
    float padding;
    float4 color;
};

struct StrokeVertexOut {
    float4 position [[position]];
    float4 color;
};

vertex StrokeVertexOut stroke_vertex_main(
    StrokeVertexIn in [[stage_in]],
    constant StrokeUniforms &u [[buffer(1)]]
) {
    StrokeVertexOut out;

    float2 ndcPos = float2(
        (in.position.x / u.viewport.x) * 2.0 - 1.0,
        1.0 - (in.position.y / u.viewport.y) * 2.0
    );

    float2 normal = float2(in.normal.x, -in.normal.y);
    float2 screenNormal = normalize(normal);
    float2 offset = screenNormal * in.side * u.halfWidth * 2.0 / u.viewport;

    out.position = float4(ndcPos + offset, 0.0, 1.0);
    out.color = u.color;
    return out;
}

fragment float4 stroke_fragment_main(
    StrokeVertexOut in [[stage_in]],
    constant StrokeUniforms &u [[buffer(1)]]
) {
    return in.color;
}

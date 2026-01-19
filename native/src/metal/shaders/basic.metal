// basic.metal - Basic colored vertices shader
#include <metal_stdlib>
using namespace metal;

struct VertexIn {
    float2 position [[attribute(0)]];
    float4 color [[attribute(1)]];
};

struct VertexOut {
    float4 position [[position]];
    float4 color;
};

vertex VertexOut vertex_main(VertexIn in [[stage_in]]) {
    VertexOut out;
    // Position is already in NDC (-1 to 1)
    out.position = float4(in.position, 0.0, 1.0);
    out.color = in.color;
    return out;
}

fragment float4 fragment_main(VertexOut in [[stage_in]]) {
    return in.color;
}

// =============================================================================
// SCREEN COORDINATES TRIANGLE SHADER
// Converts screen coordinates to NDC in the vertex shader (GPU-parallel)
// Input: [x, y, r, g, b, a] per vertex in screen/pixel coordinates
// =============================================================================

struct ScreenCoordsUniforms {
    float2 viewport;  // canvas width, height
};

vertex VertexOut vertex_screen_coords(
    uint vid [[vertex_id]],
    constant float* vertices [[buffer(0)]],
    constant ScreenCoordsUniforms& uniforms [[buffer(1)]]
) {
    // 6 floats per vertex: x, y, r, g, b, a
    uint base = vid * 6;
    float x = vertices[base];
    float y = vertices[base + 1];
    float r = vertices[base + 2];
    float g = vertices[base + 3];
    float b = vertices[base + 4];
    float a = vertices[base + 5];

    // Screen to NDC conversion (GPU-parallel for all vertices)
    float ndcX = (x / uniforms.viewport.x) * 2.0 - 1.0;
    float ndcY = 1.0 - (y / uniforms.viewport.y) * 2.0;

    VertexOut out;
    out.position = float4(ndcX, ndcY, 0.0, 1.0);
    out.color = float4(r, g, b, a);
    return out;
}

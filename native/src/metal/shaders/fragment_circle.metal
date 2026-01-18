// fragment_circle.metal - Template shader for circle-generating fragments
// This shader is compiled at runtime with user code substituted.
// Placeholders:
//   {PARAMS_STRUCT}   - User's parameter struct definition
//   {PARAMS_TYPE}     - Name of the user's parameter struct
//   {FRAGMENT_NAME}   - Name of the fragment function
//   {FRAGMENT_BODY}   - Body of the user's fragment function
#include <metal_stdlib>
using namespace metal;

// Result type for circle-generating fragments
struct CircleResult {
    float2 center;
    float radius;
    float4 color;
};

// === USER PARAMS STRUCT (inserted at compile time) ===
{PARAMS_STRUCT}

// === USER FRAGMENT FUNCTION (inserted at compile time) ===
static inline CircleResult {FRAGMENT_NAME}(uint idx, constant {PARAMS_TYPE}& p) {
    {FRAGMENT_BODY}
}

// Uniforms for fragment shader
struct FragmentCircleUniforms {
    float2 viewport;
    uint instanceCount;
    uint padding;
};

// Vertex output
struct FragmentCircleVertexOut {
    float4 position [[position]];
    float4 color;
    float2 uv;
};

// Unit quad vertices (for generating circle quads)
constant float2 unitQuad[4] = {
    float2(0, 0),
    float2(1, 0),
    float2(0, 1),
    float2(1, 1)
};

vertex FragmentCircleVertexOut fragment_circle_vertex(
    uint vid [[vertex_id]],
    uint iid [[instance_id]],
    constant void* params [[buffer(0)]],
    constant FragmentCircleUniforms& uniforms [[buffer(1)]]
) {
    // Call user's fragment function to compute circle properties
    CircleResult c = {FRAGMENT_NAME}(iid, *((constant {PARAMS_TYPE}*)params));

    // Generate quad vertices for this circle
    float2 uv = unitQuad[vid];
    float diameter = c.radius * 2.0;
    float2 pos = c.center - c.radius + uv * diameter;

    // Convert to NDC
    float2 ndc;
    ndc.x = (pos.x / uniforms.viewport.x) * 2.0 - 1.0;
    ndc.y = 1.0 - (pos.y / uniforms.viewport.y) * 2.0;

    FragmentCircleVertexOut out;
    out.position = float4(ndc, 0.0, 1.0);
    out.color = c.color;
    out.uv = uv;
    return out;
}

fragment float4 fragment_circle_fragment(FragmentCircleVertexOut in [[stage_in]]) {
    // Render smooth circle with anti-aliasing
    float2 local = in.uv * 2.0 - 1.0;
    float dist = length(local);
    float alpha = 1.0 - smoothstep(0.95, 1.0, dist);
    if (alpha < 0.01) discard_fragment();
    return float4(in.color.rgb, in.color.a * alpha);
}

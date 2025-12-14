// instanced.metal - Instanced shapes shader (rects, triangles, circles)
// GPU-side transforms for massive parallelism
#include <metal_stdlib>
using namespace metal;

// Instance data: position(2) + angle(1) + halfSize(1) + color(4) = 8 floats
// Use packed layout to match the flat array from Lean
struct InstanceData {
    packed_float2 pos;       // Center position in NDC (8 bytes)
    float angle;             // Rotation angle in radians (4 bytes)
    float halfSize;          // Half side length in NDC (4 bytes)
    packed_float4 color;     // RGBA (16 bytes)
};  // Total: 32 bytes, no padding

struct InstancedVertexOut {
    float4 position [[position]];
    float4 color;
};

vertex InstancedVertexOut instanced_vertex_main(
    uint vid [[vertex_id]],
    uint iid [[instance_id]],
    constant InstanceData* instances [[buffer(0)]]
) {
    // Unit quad vertices for triangle strip: forms a quad with vertices 0,1,2,3
    // Order: bottom-left, bottom-right, top-left, top-right (Z pattern for strip)
    float2 unitQuad[4] = {
        float2(-1, -1),  // 0: bottom-left
        float2( 1, -1),  // 1: bottom-right
        float2(-1,  1),  // 2: top-left
        float2( 1,  1)   // 3: top-right
    };

    InstanceData inst = instances[iid];
    float2 v = unitQuad[vid];

    // Compute sin/cos on GPU (massively parallel!)
    float sinA = sin(inst.angle);
    float cosA = cos(inst.angle);

    // Rotate: v' = (v.x * cos - v.y * sin, v.x * sin + v.y * cos)
    float2 rotated = float2(
        v.x * cosA - v.y * sinA,
        v.x * sinA + v.y * cosA
    );

    // Scale and translate
    float2 finalPos = inst.pos + rotated * inst.halfSize;

    InstancedVertexOut out;
    out.position = float4(finalPos, 0.0, 1.0);
    out.color = inst.color;
    return out;
}

fragment float4 instanced_fragment_main(InstancedVertexOut in [[stage_in]]) {
    return in.color;
}

// === TRIANGLE SHADER ===
// Draws spinning triangles using 3 vertices per instance

vertex InstancedVertexOut instanced_triangle_vertex(
    uint vid [[vertex_id]],
    uint iid [[instance_id]],
    constant InstanceData* instances [[buffer(0)]]
) {
    // Equilateral triangle vertices (pointing up)
    float2 unitTriangle[3] = {
        float2( 0.0,  1.15),   // top
        float2(-1.0, -0.58),   // bottom-left
        float2( 1.0, -0.58)    // bottom-right
    };

    InstanceData inst = instances[iid];
    float2 v = unitTriangle[vid];

    float sinA = sin(inst.angle);
    float cosA = cos(inst.angle);

    float2 rotated = float2(
        v.x * cosA - v.y * sinA,
        v.x * sinA + v.y * cosA
    );

    float2 finalPos = inst.pos + rotated * inst.halfSize;

    InstancedVertexOut out;
    out.position = float4(finalPos, 0.0, 1.0);
    out.color = inst.color;
    return out;
}

// === CIRCLE SHADER ===
// Draws filled circles using fragment shader distance check

struct CircleVertexOut {
    float4 position [[position]];
    float4 color;
    float2 uv;  // -1 to 1, for distance calculation
};

vertex CircleVertexOut instanced_circle_vertex(
    uint vid [[vertex_id]],
    uint iid [[instance_id]],
    constant InstanceData* instances [[buffer(0)]]
) {
    // Quad vertices (no rotation needed for circles)
    float2 unitQuad[4] = {
        float2(-1, -1),
        float2( 1, -1),
        float2(-1,  1),
        float2( 1,  1)
    };

    InstanceData inst = instances[iid];
    float2 v = unitQuad[vid];

    // Circles don't rotate, but we can use angle for something else (like pulsing)
    float2 finalPos = inst.pos + v * inst.halfSize;

    CircleVertexOut out;
    out.position = float4(finalPos, 0.0, 1.0);
    out.color = inst.color;
    out.uv = v;  // Pass UV for fragment shader
    return out;
}

fragment float4 instanced_circle_fragment(CircleVertexOut in [[stage_in]]) {
    // Distance from center (0,0) in UV space
    float dist = length(in.uv);
    // Smooth edge with anti-aliasing
    float alpha = 1.0 - smoothstep(0.9, 1.0, dist);
    if (alpha < 0.01) discard_fragment();
    return float4(in.color.rgb, in.color.a * alpha);
}

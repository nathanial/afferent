// instanced.metal - Instanced shapes shader (rects, triangles, circles)
// GPU-side transforms for massive parallelism
#include <metal_stdlib>
using namespace metal;

// Instance data: position(2) + angle(1) + halfSize(1) + color(4) = 8 floats
// Position/size are interpreted via InstancedUniforms (world vs screen).
// Use packed layout to match the flat array from Lean.
struct InstanceData {
    packed_float2 pos;       // Center position (world or NDC)
    float angle;             // Rotation angle in radians (4 bytes)
    float halfSize;          // Half side length (world or pixels)
    packed_float4 color;     // RGBA (16 bytes)
};  // Total: 32 bytes, no padding

// Instanced uniform data (affine transform + viewport)
// transform0 = [a, b, c, d] (column-major 2x2)
// transform1 = [tx, ty, 0, 0]
// sizeMode: 0 = world (offset transformed by matrix), 1 = screen (pixel size)
// colorMode: 0 = RGBA, 1 = HSV(time-based)
struct InstancedUniforms {
    float4 transform0;
    float4 transform1;
    float2 viewport;
    float time;
    float hueSpeed;
    uint sizeMode;
    uint colorMode;
    float padding0;
    float padding1;
};

struct InstancedVertexOut {
    float4 position [[position]];
    float4 color;
};

constant uint SIZE_MODE_WORLD = 0;
constant uint SIZE_MODE_SCREEN = 1;
constant uint COLOR_MODE_RGBA = 0;
constant uint COLOR_MODE_HSV = 1;

static inline float2 apply_affine(float2 p, constant InstancedUniforms& u) {
    return float2(
        u.transform0.x * p.x + u.transform0.z * p.y + u.transform1.x,
        u.transform0.y * p.x + u.transform0.w * p.y + u.transform1.y
    );
}

static inline float2 apply_linear(float2 v, constant InstancedUniforms& u) {
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

static inline float2 compute_instanced_pos(float2 local, InstanceData inst, constant InstancedUniforms& u) {
    float2 rotated = rotate_local(local, inst.angle);
    float2 offset = rotated * inst.halfSize;
    float2 base = apply_affine(inst.pos, u);
    float2 clipOffset;
    if (u.sizeMode == SIZE_MODE_SCREEN) {
        float sx = (u.viewport.x > 0.0) ? (2.0 / u.viewport.x) : 0.0;
        float sy = (u.viewport.y > 0.0) ? (-2.0 / u.viewport.y) : 0.0;
        clipOffset = float2(offset.x * sx, offset.y * sy);
    } else {
        clipOffset = apply_linear(offset, u);
    }
    return base + clipOffset;
}

static inline float3 hsv_to_rgb(float h) {
    float3 rgb = clamp(abs(fmod(h * 6.0 + float3(0.0, 4.0, 2.0), 6.0) - 3.0) - 1.0, 0.0, 1.0);
    return 1.0 - 0.9 * (1.0 - rgb);
}

static inline float4 compute_instanced_color(InstanceData inst, constant InstancedUniforms& u) {
    if (u.colorMode == COLOR_MODE_HSV) {
        float hue = fract(u.time * u.hueSpeed + inst.color.x);
        float3 rgb = hsv_to_rgb(hue);
        return float4(rgb, inst.color.w);
    }
    return inst.color;
}

vertex InstancedVertexOut instanced_vertex_main(
    uint vid [[vertex_id]],
    uint iid [[instance_id]],
    constant InstanceData* instances [[buffer(0)]],
    constant InstancedUniforms& uniforms [[buffer(1)]]
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
    float2 finalPos = compute_instanced_pos(v, inst, uniforms);

    InstancedVertexOut out;
    out.position = float4(finalPos, 0.0, 1.0);
    out.color = compute_instanced_color(inst, uniforms);
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
    constant InstanceData* instances [[buffer(0)]],
    constant InstancedUniforms& uniforms [[buffer(1)]]
) {
    // Equilateral triangle vertices (pointing up)
    float2 unitTriangle[3] = {
        float2( 0.0,  1.15),   // top
        float2(-1.0, -0.58),   // bottom-left
        float2( 1.0, -0.58)    // bottom-right
    };

    InstanceData inst = instances[iid];
    float2 v = unitTriangle[vid];
    float2 finalPos = compute_instanced_pos(v, inst, uniforms);

    InstancedVertexOut out;
    out.position = float4(finalPos, 0.0, 1.0);
    out.color = compute_instanced_color(inst, uniforms);
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
    constant InstanceData* instances [[buffer(0)]],
    constant InstancedUniforms& uniforms [[buffer(1)]]
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
    float2 finalPos = compute_instanced_pos(v, inst, uniforms);

    CircleVertexOut out;
    out.position = float4(finalPos, 0.0, 1.0);
    out.color = compute_instanced_color(inst, uniforms);
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

// =============================================================================
// BATCHED AXIS-ALIGNED RECT SHADER
// Optimized for charts (heatmaps, scatter plots, bar charts) where each rect
// has variable dimensions: [x, y, width, height, r, g, b, a] per instance
// =============================================================================

// Instance data for batched rects: 8 floats per rect
struct BatchedRectInstance {
    packed_float2 pos;        // Top-left corner (x, y) in pixels
    packed_float2 size;       // Width, height in pixels
    packed_float4 color;      // RGBA
};  // 32 bytes total

// Uniforms for batched rects
struct BatchedRectUniforms {
    float2 viewport;          // Canvas width, height for NDC conversion
    float cornerRadius;       // Uniform corner radius (0 for sharp corners)
    float padding;
};

struct BatchedRectVertexOut {
    float4 position [[position]];
    float4 color;
    float2 uv;                // 0-1 UV within the rect
    float2 rectSize;          // Rect size in pixels (for corner radius)
    float cornerRadius;       // Pass corner radius to fragment
};

vertex BatchedRectVertexOut batched_rect_vertex(
    uint vid [[vertex_id]],
    uint iid [[instance_id]],
    constant BatchedRectInstance* instances [[buffer(0)]],
    constant BatchedRectUniforms& uniforms [[buffer(1)]]
) {
    // Quad vertices for triangle strip: 0=TL, 1=TR, 2=BL, 3=BR
    float2 unitQuad[4] = {
        float2(0, 0),   // 0: top-left
        float2(1, 0),   // 1: top-right
        float2(0, 1),   // 2: bottom-left
        float2(1, 1)    // 3: bottom-right
    };

    BatchedRectInstance inst = instances[iid];
    float2 uv = unitQuad[vid];

    // Compute pixel position
    float2 pixelPos = inst.pos + uv * inst.size;

    // Convert to NDC: x: 0..width -> -1..1, y: 0..height -> 1..-1 (Y flipped)
    float2 ndc;
    ndc.x = (pixelPos.x / uniforms.viewport.x) * 2.0 - 1.0;
    ndc.y = 1.0 - (pixelPos.y / uniforms.viewport.y) * 2.0;

    BatchedRectVertexOut out;
    out.position = float4(ndc, 0.0, 1.0);
    out.color = inst.color;
    out.uv = uv;
    out.rectSize = inst.size;
    out.cornerRadius = uniforms.cornerRadius;
    return out;
}

fragment float4 batched_rect_fragment(BatchedRectVertexOut in [[stage_in]]) {
    if (in.cornerRadius <= 0.0) {
        // No corner radius - simple solid rect
        return in.color;
    }

    // Rounded corners using SDF
    float2 halfSize = in.rectSize * 0.5;
    float2 center = float2(0.5, 0.5);
    float2 localPos = (in.uv - center) * in.rectSize;  // Position relative to center

    // Clamp corner radius to half the smaller dimension
    float r = min(in.cornerRadius, min(halfSize.x, halfSize.y));

    // Distance to rounded rect edge
    float2 q = abs(localPos) - (halfSize - r);
    float dist = min(max(q.x, q.y), 0.0) + length(max(q, 0.0)) - r;

    // Anti-aliased edge (1 pixel transition)
    float alpha = 1.0 - smoothstep(-1.0, 0.0, dist);
    if (alpha < 0.01) discard_fragment();

    return float4(in.color.rgb, in.color.a * alpha);
}

// =============================================================================
// BATCHED CIRCLE SHADER (for scatter plots, bubble charts)
// Instance data: [centerX, centerY, radius, r, g, b, a] = 7 floats per circle
// Padded to 8 floats for alignment: [centerX, centerY, radius, padding, r, g, b, a]
// =============================================================================

struct BatchedCircleInstance {
    packed_float2 center;     // Center position in pixels
    float radius;             // Radius in pixels
    float padding;            // Alignment padding
    packed_float4 color;      // RGBA
};  // 32 bytes total

struct BatchedCircleUniforms {
    float2 viewport;          // Canvas width, height for NDC conversion
};

struct BatchedCircleVertexOut {
    float4 position [[position]];
    float4 color;
    float2 uv;                // -1 to 1, for distance calculation
};

vertex BatchedCircleVertexOut batched_circle_vertex(
    uint vid [[vertex_id]],
    uint iid [[instance_id]],
    constant BatchedCircleInstance* instances [[buffer(0)]],
    constant BatchedCircleUniforms& uniforms [[buffer(1)]]
) {
    // Quad vertices covering the circle bounding box
    float2 unitQuad[4] = {
        float2(-1, -1),  // 0: bottom-left
        float2( 1, -1),  // 1: bottom-right
        float2(-1,  1),  // 2: top-left
        float2( 1,  1)   // 3: top-right
    };

    BatchedCircleInstance inst = instances[iid];
    float2 uv = unitQuad[vid];

    // Compute pixel position (quad covers bounding box of circle)
    float2 pixelPos = inst.center + uv * inst.radius;

    // Convert to NDC
    float2 ndc;
    ndc.x = (pixelPos.x / uniforms.viewport.x) * 2.0 - 1.0;
    ndc.y = 1.0 - (pixelPos.y / uniforms.viewport.y) * 2.0;

    BatchedCircleVertexOut out;
    out.position = float4(ndc, 0.0, 1.0);
    out.color = inst.color;
    out.uv = uv;
    return out;
}

fragment float4 batched_circle_fragment(BatchedCircleVertexOut in [[stage_in]]) {
    float dist = length(in.uv);
    // Smooth edge with anti-aliasing
    float alpha = 1.0 - smoothstep(0.95, 1.0, dist);
    if (alpha < 0.01) discard_fragment();
    return float4(in.color.rgb, in.color.a * alpha);
}

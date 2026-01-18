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
    uint shapeType;
    uint padding0;
};

struct InstancedVertexOut {
    float4 position [[position]];
    float4 color;
    float2 uv;
    float shapeType;
};

constant uint SIZE_MODE_WORLD = 0;
constant uint SIZE_MODE_SCREEN = 1;
constant uint COLOR_MODE_RGBA = 0;
constant uint COLOR_MODE_HSV = 1;
constant uint SHAPE_RECT = 0;
constant uint SHAPE_TRIANGLE = 1;
constant uint SHAPE_CIRCLE = 2;

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
    InstanceData inst = instances[iid];
    float2 v;
    if (uniforms.shapeType == SHAPE_TRIANGLE) {
        float2 unitTriangle[3] = {
            float2( 0.0,  1.15),   // top
            float2(-1.0, -0.58),   // bottom-left
            float2( 1.0, -0.58)    // bottom-right
        };
        v = unitTriangle[vid];
    } else {
        float2 unitQuad[4] = {
            float2(-1, -1),
            float2( 1, -1),
            float2(-1,  1),
            float2( 1,  1)
        };
        v = unitQuad[vid];
    }
    float2 finalPos = compute_instanced_pos(v, inst, uniforms);

    InstancedVertexOut out;
    out.position = float4(finalPos, 0.0, 1.0);
    out.color = compute_instanced_color(inst, uniforms);
    out.uv = v;
    out.shapeType = (float)uniforms.shapeType;
    return out;
}

fragment float4 instanced_fragment_main(InstancedVertexOut in [[stage_in]]) {
    if (in.shapeType > 1.5) {
        float dist = length(in.uv);
        float alpha = 1.0 - smoothstep(0.9, 1.0, dist);
        if (alpha < 0.01) discard_fragment();
        return float4(in.color.rgb, in.color.a * alpha);
    }
    return in.color;
}
// =============================================================================
// BATCHED SHAPES (rect, circle, stroke rect)
// Instance data: [x, y, width, height, r, g, b, a, cornerRadius] per instance
// =============================================================================

struct BatchedInstance {
    packed_float2 pos;
    packed_float2 size;
    packed_float4 color;
    float cornerRadius;
};

struct BatchedUniforms {
    float2 viewport;
    float lineWidth;
    float cornerRadius;
    uint shapeType;
    uint padding;
};

struct BatchedVertexOut {
    float4 position [[position]];
    float4 color;
    float2 uv;
    float2 size;
    float4 params;  // lineWidth, cornerRadius, shapeType, unused
};

vertex BatchedVertexOut batched_vertex(
    uint vid [[vertex_id]],
    uint iid [[instance_id]],
    constant BatchedInstance* instances [[buffer(0)]],
    constant BatchedUniforms& uniforms [[buffer(1)]]
) {
    float2 unitQuad[4] = {
        float2(0, 0),
        float2(1, 0),
        float2(0, 1),
        float2(1, 1)
    };

    BatchedInstance inst = instances[iid];
    float2 uv = unitQuad[vid];
    float2 pixelPos;

    // shapeType 3 = line: pos = p1, size = p2 (endpoints)
    if (uniforms.shapeType == 3) {
        float2 p1 = inst.pos;
        float2 p2 = inst.size;  // Reinterpret size as second endpoint
        float2 dir = p2 - p1;
        float len = length(dir);
        if (len < 0.001) {
            dir = float2(1.0, 0.0);
        } else {
            dir = dir / len;
        }
        float2 perp = float2(-dir.y, dir.x);
        float halfWidth = uniforms.lineWidth * 0.5;

        // Generate quad vertices: vid 0,1 at p1, vid 2,3 at p2
        // uv.x selects endpoint (0=p1, 1=p2), uv.y selects side (-1 or +1)
        float2 basePoint = (vid < 2) ? p1 : p2;
        float side = ((vid == 0) || (vid == 2)) ? -1.0 : 1.0;
        pixelPos = basePoint + perp * halfWidth * side;
    } else {
        pixelPos = inst.pos + uv * inst.size;
    }

    float2 ndc;
    ndc.x = (pixelPos.x / uniforms.viewport.x) * 2.0 - 1.0;
    ndc.y = 1.0 - (pixelPos.y / uniforms.viewport.y) * 2.0;

    BatchedVertexOut out;
    out.position = float4(ndc, 0.0, 1.0);
    out.color = inst.color;
    out.uv = uv;
    out.size = inst.size;
    out.params = float4(uniforms.lineWidth, inst.cornerRadius, (float)uniforms.shapeType, 0.0);
    return out;
}

fragment float4 batched_fragment(BatchedVertexOut in [[stage_in]]) {
    float shapeType = in.params.z;

    // shapeType 3 = line: solid color, geometry already expanded by vertex shader
    if (shapeType > 2.5 && shapeType < 3.5) {
        return in.color;
    }

    // shapeType 4 = strokeCircle: ring/annulus shape
    if (shapeType > 3.5 && shapeType < 4.5) {
        float2 local = in.uv * 2.0 - 1.0;
        float dist = length(local);
        float lineWidth = in.params.x;
        // Convert lineWidth from pixels to normalized units (stroke inside the circle)
        float diameter = in.size.x;  // width and height should be equal for circles
        float normalizedWidth = (lineWidth * 2.0) / diameter;
        float innerEdge = 1.0 - normalizedWidth;
        float innerAlpha = smoothstep(innerEdge - 0.02, innerEdge + 0.02, dist);
        float outerAlpha = 1.0 - smoothstep(0.96, 1.0, dist);
        float alpha = innerAlpha * outerAlpha;
        if (alpha < 0.01) discard_fragment();
        return float4(in.color.rgb, in.color.a * alpha);
    }

    // shapeType 1 = filled circle
    if (shapeType > 0.5 && shapeType < 1.5) {
        float2 local = in.uv * 2.0 - 1.0;
        float dist = length(local);
        float alpha = 1.0 - smoothstep(0.95, 1.0, dist);
        if (alpha < 0.01) discard_fragment();
        return float4(in.color.rgb, in.color.a * alpha);
    }

    float lineWidth = in.params.x;
    float cornerRadius = in.params.y;
    if (cornerRadius <= 0.0 && lineWidth <= 0.0) {
        return in.color;
    }

    float2 halfSize = in.size * 0.5;
    float2 localPos = (in.uv - 0.5) * in.size;
    float r = min(cornerRadius, min(halfSize.x, halfSize.y));
    float2 q = abs(localPos) - (halfSize - r);
    float dist = min(max(q.x, q.y), 0.0) + length(max(q, 0.0)) - r;

    float alpha;
    if (shapeType > 1.5) {
        float halfWidth = lineWidth * 0.5;
        float innerAlpha = smoothstep(-halfWidth - 1.0, -halfWidth, dist);
        float outerAlpha = 1.0 - smoothstep(halfWidth - 1.0, halfWidth, dist);
        alpha = innerAlpha * outerAlpha;
    } else {
        alpha = 1.0 - smoothstep(-1.0, 0.0, dist);
    }

    if (alpha < 0.01) discard_fragment();
    return float4(in.color.rgb, in.color.a * alpha);
}

// =============================================================================
// INSTANCED MESH RENDERING (for complex polygons like gears)
// Renders a pre-tessellated mesh (vertices + indices) with per-instance transforms.
// - Mesh vertices are in local space (centered on origin)
// - Each instance: position(2) + rotation(1) + scale(1) + color(4) = 8 floats
// - Single draw call for all instances with indexed triangles
// =============================================================================

struct MeshInstance {
    packed_float2 position;  // Screen-space center position
    float rotation;          // Rotation in radians
    float scale;             // Uniform scale factor
    packed_float4 color;     // RGBA color
};

struct MeshUniforms {
    float2 viewport;         // Canvas width, height
    float2 meshCenter;       // Mesh centroid (rotation pivot)
    uint padding0;
    uint padding1;
};

struct MeshVertexOut {
    float4 position [[position]];
    float4 color;
};

vertex MeshVertexOut mesh_instanced_vertex(
    uint vid [[vertex_id]],
    uint iid [[instance_id]],
    constant packed_float2* meshVertices [[buffer(0)]],
    constant MeshInstance* instances [[buffer(1)]],
    constant MeshUniforms& uniforms [[buffer(2)]]
) {
    // Get base mesh vertex and instance data
    float2 localPos = meshVertices[vid];
    MeshInstance inst = instances[iid];

    // Transform: local → rotated around centroid → scaled → translated
    float2 centered = localPos - uniforms.meshCenter;

    float sinA = sin(inst.rotation);
    float cosA = cos(inst.rotation);
    float2 rotated = float2(
        centered.x * cosA - centered.y * sinA,
        centered.x * sinA + centered.y * cosA
    );

    float2 scaled = rotated * inst.scale;
    float2 translated = scaled + inst.position;

    // Convert to NDC
    float2 ndc;
    ndc.x = (translated.x / uniforms.viewport.x) * 2.0 - 1.0;
    ndc.y = 1.0 - (translated.y / uniforms.viewport.y) * 2.0;

    MeshVertexOut out;
    out.position = float4(ndc, 0.0, 1.0);
    out.color = inst.color;
    return out;
}

fragment float4 mesh_instanced_fragment(MeshVertexOut in [[stage_in]]) {
    return in.color;
}

// =============================================================================
// INSTANCED ARC STROKE RENDERING
// GPU-side arc geometry generation with per-instance parameters.
// Each arc is rendered as a triangle strip forming a thick arc stroke.
// Instance data: center(2) + angles(2) + radius(1) + strokeWidth(1) + color(4) = 10 floats
// =============================================================================

struct ArcInstance {
    packed_float2 center;     // Arc center position
    float startAngle;         // Start angle in radians
    float sweepAngle;         // Sweep angle in radians
    float radius;             // Arc radius
    float strokeWidth;        // Stroke thickness
    packed_float4 color;      // RGBA
};

struct ArcUniforms {
    float2 viewport;          // Canvas width, height
    uint segments;            // Subdivisions per arc (e.g., 16)
    uint padding;
};

struct ArcVertexOut {
    float4 position [[position]];
    float4 color;
};

vertex ArcVertexOut arc_instanced_vertex(
    uint vid [[vertex_id]],
    uint iid [[instance_id]],
    constant ArcInstance* instances [[buffer(0)]],
    constant ArcUniforms& uniforms [[buffer(1)]]
) {
    ArcInstance inst = instances[iid];
    uint segments = uniforms.segments;

    // Each arc needs (segments+1)*2 vertices for triangle strip
    // vertexInArc tells us which vertex within the arc we're processing
    uint vertexInArc = vid;
    uint segmentIdx = vertexInArc / 2;
    bool isOuter = (vertexInArc % 2) == 0;

    // Calculate angle for this segment
    float t = float(segmentIdx) / float(segments);
    float angle = inst.startAngle + t * inst.sweepAngle;

    // Inner/outer radius for stroke thickness
    float halfStroke = inst.strokeWidth * 0.5;
    float r = isOuter ? (inst.radius + halfStroke) : (inst.radius - halfStroke);

    // Calculate position
    float2 pos = inst.center + float2(cos(angle), sin(angle)) * r;

    // Convert to NDC
    float2 ndc;
    ndc.x = (pos.x / uniforms.viewport.x) * 2.0 - 1.0;
    ndc.y = 1.0 - (pos.y / uniforms.viewport.y) * 2.0;

    ArcVertexOut out;
    out.position = float4(ndc, 0.0, 1.0);
    out.color = inst.color;
    return out;
}

fragment float4 arc_instanced_fragment(ArcVertexOut in [[stage_in]]) {
    return in.color;
}

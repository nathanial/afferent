// stroke_path.metal - GPU stroke extrusion from parametric segments
#include <metal_stdlib>
using namespace metal;

struct StrokeSegment {
    packed_float2 p0;
    packed_float2 p1;
    packed_float2 c1;
    packed_float2 c2;
    packed_float2 prevDir;
    packed_float2 nextDir;
    float startDist;
    float length;
    float hasPrev;
    float hasNext;
    float kind;
    float padding;
};

struct StrokePathVertexUniforms {
    float2 viewport;
    float halfWidth;
    float miterLimit;
    uint lineCap;
    uint lineJoin;
    uint segmentSubdivisions;
    uint padding0;
    float4 transform0; // [a, b, c, d]
    float4 transform1; // [tx, ty, 0, 0]
};

struct StrokePathFragmentUniforms {
    float4 color;
    float dashSegments[8];
    uint dashCount;
    float dashOffset;
    uint lineCap;
    float halfWidth;
    float padding0;
    float padding1;
    float padding2;
    float padding3;
};

struct StrokePathVertexOut {
    float4 position [[position]];
    float pathDist;
    float perpDist;
    float segmentStart;
    float segmentEnd;
    float hasPrev;
    float hasNext;
};

constant uint LINECAP_BUTT = 0;
constant uint LINECAP_ROUND = 1;
constant uint LINECAP_SQUARE = 2;

constant uint LINEJOIN_MITER = 0;
constant uint LINEJOIN_ROUND = 1;
constant uint LINEJOIN_BEVEL = 2;

static inline float2 normalize_safe(float2 v) {
    float len = length(v);
    if (len < 1e-4) {
        return float2(0.0, 0.0);
    }
    return v / len;
}

static inline float2 perp(float2 v) {
    return float2(-v.y, v.x);
}

static inline float2 apply_affine(float2 p, float4 t0, float4 t1) {
    return float2(
        t0.x * p.x + t0.z * p.y + t1.x,
        t0.y * p.x + t0.w * p.y + t1.y
    );
}

static inline float2 apply_linear(float2 v, float4 t0) {
    return float2(
        t0.x * v.x + t0.z * v.y,
        t0.y * v.x + t0.w * v.y
    );
}

static inline float2 cubic_point(float2 p0, float2 c1, float2 c2, float2 p1, float t) {
    float u = 1.0 - t;
    float tt = t * t;
    float uu = u * u;
    float uuu = uu * u;
    float ttt = tt * t;
    return p0 * uuu + c1 * (3.0 * uu * t) + c2 * (3.0 * u * tt) + p1 * ttt;
}

static inline float2 cubic_tangent(float2 p0, float2 c1, float2 c2, float2 p1, float t) {
    float u = 1.0 - t;
    float tt = t * t;
    float uu = u * u;
    float2 term1 = (c1 - p0) * (3.0 * uu);
    float2 term2 = (c2 - c1) * (6.0 * u * t);
    float2 term3 = (p1 - c2) * (3.0 * tt);
    return term1 + term2 + term3;
}

static inline void compute_miter(float2 n1, float2 n2, float miterLimit,
                                 thread float2 &outDir, thread float &outScale) {
    float2 m = n1 + n2;
    float len = length(m);
    if (len < 1e-4) {
        outDir = n1;
        outScale = 1.0;
        return;
    }
    m /= len;
    float dotVal = dot(m, n1);
    float scale = (fabs(dotVal) > 1e-4) ? (1.0 / dotVal) : 1.0;
    if (scale > miterLimit) {
        scale = miterLimit;
    }
    outDir = m;
    outScale = scale;
}

vertex StrokePathVertexOut stroke_path_vertex_main(
    uint vertex_id [[vertex_id]],
    uint instance_id [[instance_id]],
    const device StrokeSegment *segments [[buffer(0)]],
    constant StrokePathVertexUniforms &u [[buffer(1)]]
) {
    StrokePathVertexOut out;
    StrokeSegment seg = segments[instance_id];

    float4 t0 = u.transform0;
    float4 t1 = u.transform1;

    float2 p0 = float2(seg.p0);
    float2 p1 = float2(seg.p1);
    float2 c1 = float2(seg.c1);
    float2 c2 = float2(seg.c2);

    float2 prevDir = normalize_safe(apply_linear(float2(seg.prevDir), t0));
    float2 nextDir = normalize_safe(apply_linear(float2(seg.nextDir), t0));

    uint subdivisions = max(u.segmentSubdivisions, 1u);
    uint sampleIndex = vertex_id / 2;
    float side = (vertex_id % 2 == 0) ? 1.0 : -1.0;
    float t = float(sampleIndex) / float(subdivisions);

    float2 pos;
    float2 dir;

    if (seg.kind < 0.5) {
        pos = mix(p0, p1, t);
        dir = p1 - p0;
    } else {
        pos = cubic_point(p0, c1, c2, p1, t);
        dir = cubic_tangent(p0, c1, c2, p1, t);
    }

    pos = apply_affine(pos, t0, t1);
    dir = normalize_safe(apply_linear(dir, t0));

    if (length(dir) < 1e-4) {
        dir = float2(1.0, 0.0);
    }

    float2 normal = perp(dir);
    float2 offsetDir = normal;
    float miterScale = 1.0;

    bool isStart = (sampleIndex == 0);
    bool isEnd = (sampleIndex == subdivisions);

    if (u.lineJoin == LINEJOIN_MITER) {
        if (isStart && seg.hasPrev > 0.5) {
            float2 n1 = perp(prevDir);
            float2 n2 = normal;
            compute_miter(n1, n2, u.miterLimit, offsetDir, miterScale);
        } else if (isEnd && seg.hasNext > 0.5) {
            float2 n1 = normal;
            float2 n2 = perp(nextDir);
            compute_miter(n1, n2, u.miterLimit, offsetDir, miterScale);
        }
    }

    float pathDist = seg.startDist + seg.length * t;

    if (isStart && seg.hasPrev < 0.5 && u.lineCap != LINECAP_BUTT) {
        pos -= dir * u.halfWidth;
        pathDist = seg.startDist - u.halfWidth;
    } else if (isEnd && seg.hasNext < 0.5 && u.lineCap != LINECAP_BUTT) {
        pos += dir * u.halfWidth;
        pathDist = seg.startDist + seg.length + u.halfWidth;
    }

    float2 offset = offsetDir * side * u.halfWidth * miterScale;
    float2 ndcPos = float2(
        (pos.x / u.viewport.x) * 2.0 - 1.0,
        1.0 - (pos.y / u.viewport.y) * 2.0
    );
    float2 ndcOffset = float2(
        offset.x * 2.0 / u.viewport.x,
        -offset.y * 2.0 / u.viewport.y
    );

    out.position = float4(ndcPos + ndcOffset, 0.0, 1.0);
    out.pathDist = pathDist;
    out.perpDist = side * u.halfWidth;
    out.segmentStart = seg.startDist;
    out.segmentEnd = seg.startDist + seg.length;
    out.hasPrev = seg.hasPrev;
    out.hasNext = seg.hasNext;
    return out;
}

fragment float4 stroke_path_fragment_main(
    StrokePathVertexOut in [[stage_in]],
    constant StrokePathFragmentUniforms &u [[buffer(1)]]
) {
    float halfWidth = u.halfWidth;
    float perp = fabs(in.perpDist);

    // Handle start/end caps outside the path range
    if (in.hasPrev < 0.5 && in.pathDist < in.segmentStart) {
        if (u.lineCap == LINECAP_BUTT) {
            discard_fragment();
        }
        if (u.lineCap == LINECAP_ROUND) {
            float along = in.segmentStart - in.pathDist;
            if (along * along + perp * perp > halfWidth * halfWidth) {
                discard_fragment();
            }
        }
        return u.color;
    }

    if (in.hasNext < 0.5 && in.pathDist > in.segmentEnd) {
        if (u.lineCap == LINECAP_BUTT) {
            discard_fragment();
        }
        if (u.lineCap == LINECAP_ROUND) {
            float along = in.pathDist - in.segmentEnd;
            if (along * along + perp * perp > halfWidth * halfWidth) {
                discard_fragment();
            }
        }
        return u.color;
    }

    // Dash pattern
    if (u.dashCount > 0) {
        float cycleLen = 0.0;
        for (uint i = 0; i < 8; ++i) {
            if (i >= u.dashCount) {
                break;
            }
            cycleLen += u.dashSegments[i];
        }

        if (cycleLen > 1e-4) {
            float pos = fmod(in.pathDist + u.dashOffset, cycleLen);
            if (pos < 0.0) {
                pos += cycleLen;
            }

            bool draw = true;
            float accum = 0.0;
            float segStart = 0.0;
            float segEnd = 0.0;

            for (uint i = 0; i < 8; ++i) {
                if (i >= u.dashCount) {
                    break;
                }
                float segLen = u.dashSegments[i];
                segStart = accum;
                segEnd = accum + segLen;
                if (pos >= segStart && pos <= segEnd) {
                    draw = ((i % 2) == 0);
                    break;
                }
                accum = segEnd;
            }

            if (!draw) {
                if (u.lineCap == LINECAP_ROUND) {
                    float distToStart = pos - segStart;
                    float distToEnd = segEnd - pos;
                    float along = min(distToStart, distToEnd);
                    if (along < halfWidth) {
                        if (along * along + perp * perp <= halfWidth * halfWidth) {
                            return u.color;
                        }
                    }
                }
                discard_fragment();
            }
        }
    }

    return u.color;
}

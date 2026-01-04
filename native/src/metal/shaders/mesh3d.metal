// mesh3d.metal - 3D shader with perspective projection, lighting, fog, and ocean waves
#include <metal_stdlib>
using namespace metal;

// 3D Vertex input (matches AfferentVertex3D layout)
struct Vertex3DIn {
    float3 position [[attribute(0)]];
    float3 normal [[attribute(1)]];
    float4 color [[attribute(2)]];
};

// 3D Textured Vertex input (matches AfferentVertex3DTextured layout)
// 12 floats per vertex: position(3) + normal(3) + uv(2) + color(4)
struct Vertex3DTexturedIn {
    float3 position [[attribute(0)]];
    float3 normal [[attribute(1)]];
    float2 uv [[attribute(2)]];
    float4 color [[attribute(3)]];
};

// 3D Vertex output (shared for textured + untextured)
struct Vertex3DOut {
    float4 position [[position]];
    float3 worldNormal;
    float3 worldPos;    // World position for fog calculation
    float2 oceanBaseXZ; // Undisplaced ocean XZ (world), for stable seam clipping
    float2 uv;
    float4 color;
};

// Scene uniforms for 3D rendering
// NOTE: Using packed_float3 to match C struct layout (12 bytes, no padding)
struct Scene3DUniforms {
    float4x4 modelViewProj;   // Combined MVP matrix
    float4x4 modelMatrix;     // Model matrix for normal transformation
    packed_float3 lightDir;   // Light direction (12 bytes, packed to match C)
    float ambient;            // Ambient light factor
    packed_float3 cameraPos;  // Camera position for fog distance
    float fogStart;           // Distance where fog begins
    packed_float3 fogColor;   // Fog color (RGB)
    float fogEnd;             // Distance where fog is fully opaque
    float2 uvScale;           // UV tiling scale (default 1,1)
    float2 uvOffset;          // UV offset (default 0,0)
    uint useTexture;          // 1 = sample diffuse texture, 0 = ignore
    uint padding0;
    float2 padding1;
};

vertex Vertex3DOut vertex_main_3d(
    Vertex3DIn in [[stage_in]],
    constant Scene3DUniforms& uniforms [[buffer(1)]]
) {
    Vertex3DOut out;
    out.position = uniforms.modelViewProj * float4(in.position, 1.0);
    // Transform normal to world space (using upper-left 3x3 of model matrix)
    out.worldNormal = (uniforms.modelMatrix * float4(in.normal, 0.0)).xyz;
    // Pass world position for fog calculation
    out.worldPos = (uniforms.modelMatrix * float4(in.position, 1.0)).xyz;
    out.oceanBaseXZ = float2(0.0, 0.0);
    out.uv = float2(0.0, 0.0);
    out.color = in.color;
    return out;
}

vertex Vertex3DOut vertex_main_3d_textured(
    Vertex3DTexturedIn in [[stage_in]],
    constant Scene3DUniforms& uniforms [[buffer(1)]]
) {
    Vertex3DOut out;
    out.position = uniforms.modelViewProj * float4(in.position, 1.0);
    out.worldNormal = (uniforms.modelMatrix * float4(in.normal, 0.0)).xyz;
    out.worldPos = (uniforms.modelMatrix * float4(in.position, 1.0)).xyz;
    out.oceanBaseXZ = float2(0.0, 0.0);
    out.uv = in.uv * uniforms.uvScale + uniforms.uvOffset;
    out.color = in.color;
    return out;
}

// Projected-grid ocean uniforms: scene + parameters + 4 Gerstner waves.
// params0: (time, fovY, aspect, maxDistance)
// params1: (snapSize, overscanNdc, horizonMargin, yaw)
// params2: (pitch, gridSize, 0, 0)
// waveA[i]: (dirX, dirZ, k, omegaSpeed)
// waveB[i]: (amplitude, ak, 0, 0)
struct OceanProjectedUniforms {
    Scene3DUniforms scene;
    float4 params0;
    float4 params1;
    float4 params2;
    float4 waveA[4];
    float4 waveB[4];
};

static inline void ocean_gerstner(
    float2 xz,
    constant OceanProjectedUniforms& u,
    thread float3& displacedPos,
    thread float3& normalOut
) {
    float dx = 0.0;
    float dy = 0.0;
    float dz = 0.0;
    float sx = 0.0;
    float sz = 0.0;
    float sxx = 0.0;
    float szz = 0.0;
    float sxz = 0.0;

    for (uint i = 0; i < 4; i++) {
        float2 dir = u.waveA[i].xy;
        float k = u.waveA[i].z;
        float omegaSpeed = u.waveA[i].w;
        float amplitude = u.waveB[i].x;
        float ak = u.waveB[i].y;

        float phase = k * (dir.x * xz.x + dir.y * xz.y) - omegaSpeed * u.params0.x;
        float c = cos(phase);
        float s = sin(phase);

        dx += amplitude * dir.x * c;
        dy += amplitude * s;
        dz += amplitude * dir.y * c;

        sx += ak * dir.x * c;
        sz += ak * dir.y * c;
        sxx += ak * dir.x * dir.x * s;
        szz += ak * dir.y * dir.y * s;
        sxz += ak * dir.x * dir.y * s;
    }

    displacedPos = float3(xz.x + dx, dy, xz.y + dz);

    float3 dPdx = float3(1.0 - sxx, sx, -sxz);
    float3 dPdz = float3(-sxz, sz, 1.0 - szz);
    normalOut = normalize(cross(dPdz, dPdx));
}

vertex Vertex3DOut vertex_ocean_projected_waves(
    uint vid [[vertex_id]],
    constant OceanProjectedUniforms& u [[buffer(1)]]
) {
    Vertex3DOut out;

    float time = u.params0.x;
    (void)time;
    float fovY = u.params0.y;
    float aspect = u.params0.z;
    float maxDistance = u.params0.w;
    float snapSize = u.params1.x;
    float overscanNdc = u.params1.y;
    float horizonMargin = u.params1.z;
    float yaw = u.params1.w;
    float pitch = u.params2.x;
    uint gridSize = (uint)u.params2.y;
    float nearExtent = u.params2.z;
    uint gridSizeMinus1 = (gridSize > 0) ? (gridSize - 1) : 0;
    uint row = (gridSize > 0) ? (vid / gridSize) : 0;
    uint col = (gridSize > 0) ? (vid - row * gridSize) : 0;
    float u01 = (gridSizeMinus1 > 0) ? ((float)col / (float)gridSizeMinus1) : 0.0;
    float v01 = (gridSizeMinus1 > 0) ? ((float)row / (float)gridSizeMinus1) : 0.0;

    // Camera basis (matches Lean FPSCamera).
    float cosPitch = cos(pitch);
    float sinPitch = sin(pitch);
    float cosYaw = cos(yaw);
    float sinYaw = sin(yaw);
    float3 fwd = float3(cosPitch * sinYaw, sinPitch, -cosPitch * cosYaw);
    float3 right = normalize(cross(fwd, float3(0.0, 1.0, 0.0)));
    float3 up = normalize(cross(right, fwd));

    float3 camPos = float3(u.scene.cameraPos);

    // Grid snapping (world XZ).
    float originX = camPos.x;
    float originZ = camPos.z;
    if (snapSize > 0.00001) {
        originX = floor(originX / snapSize) * snapSize;
        originZ = floor(originZ / snapSize) * snapSize;
    }

    float tanHalfFovY = tan(fovY * 0.5);
    float tanHalfFovX = tanHalfFovY * aspect;

    // Conservative wave bounds for overscan:
    // - Vertical displacement is bounded by sum(amplitude).
    // - Horizontal displacement in our Gerstner implementation is also bounded by sum(amplitude).
    float maxWaveAmp = 0.0;
    for (uint i = 0; i < 4; i++) {
        maxWaveAmp += u.waveB[i].x;
    }

    float eps = 0.00001;
    float baseX = originX;
    float baseZ = originZ;

    (void)nearExtent;

    // Projected grid only: generate the ocean surface by intersecting view rays with the ocean plane.
    // Horizon cutoff in NDC (same logic as CPU path).
    float horizonSy = (abs(up.y) < eps) ? 0.0 : (-fwd.y) / up.y;
    float horizonNdcY = horizonSy / tanHalfFovY;

    // Aggressive adaptive overscan near the surface.
    // The projected-grid is view-frustum aligned, so when the camera is close to the surface and pitched down,
    // wave displacement can expose the mesh boundary unless we overscan significantly (especially at the bottom).
    float camHeight = max(camPos.y, 0.05);
    float ampOverHeight = (camHeight > eps) ? (maxWaveAmp / camHeight) : 0.0;
    float pitchDown = clamp(-pitch, 0.0, 1.2); // pitch is negative when looking down

    // Make overscan sensitive to wave direction relative to camera.
    // If waves are aligned with camera forward, horizontal displacement tends to pull/push geometry along
    // the view direction, which most strongly reveals gaps near the bottom/foreground when looking down.
    float2 fwdXZ0 = float2(fwd.x, fwd.z);
    float2 rightXZ0 = float2(right.x, right.z);
    float fwdXZLen = length(fwdXZ0);
    float rightXZLen = length(rightXZ0);
    float2 fwdXZ = (fwdXZLen > eps) ? (fwdXZ0 / fwdXZLen) : float2(0.0, -1.0);
    float2 rightXZ = (rightXZLen > eps) ? (rightXZ0 / rightXZLen) : float2(1.0, 0.0);

    float forwardDisp = 0.0;
    float sideDisp = 0.0;
    for (uint i = 0; i < 4; i++) {
        float2 wdir = float2(u.waveA[i].x, u.waveA[i].y);
        float amplitude = u.waveB[i].x;
        forwardDisp += amplitude * abs(dot(wdir, fwdXZ));
        sideDisp += amplitude * abs(dot(wdir, rightXZ));
    }
    float forwardAlign = (maxWaveAmp > eps) ? clamp(forwardDisp / maxWaveAmp, 0.0, 1.0) : 0.0;
    float sideAlign = (maxWaveAmp > eps) ? clamp(sideDisp / maxWaveAmp, 0.0, 1.0) : 0.0;

    float extraAllNdc = clamp(ampOverHeight * 0.45, 0.0, 4.0);
    extraAllNdc *= (1.0 + 0.35 * forwardAlign);
    float overscanEff = overscanNdc + extraAllNdc;

    float extraBottomNdc = clamp(ampOverHeight * (2.8 + 3.0 * pitchDown), 0.0, 30.0);
    float extraSideNdc = clamp(ampOverHeight * (1.2 + 1.5 * pitchDown), 0.0, 12.0);
    float extraTopNdc = clamp(ampOverHeight * (0.8 + 0.8 * pitchDown), 0.0, 8.0);
    extraBottomNdc *= (1.0 + 2.5 * forwardAlign);
    extraSideNdc *= (1.0 + 1.5 * sideAlign);

    float ndcBottom = -1.0 - overscanEff - extraBottomNdc;
    float ndcTop0 = horizonNdcY - horizonMargin;
    float ndcTop = clamp(ndcTop0, ndcBottom, 1.0 + overscanEff + extraTopNdc);
    float ndcLeft = -1.0 - overscanEff - extraSideNdc;
    float ndcRight = 1.0 + overscanEff + extraSideNdc;

    float ndcX = mix(ndcLeft, ndcRight, u01);
    float ndcY = mix(ndcTop, ndcBottom, v01);

    float sx = ndcX * tanHalfFovX;
    float sy = ndcY * tanHalfFovY;
    float3 dir = right * sx + up * sy + fwd;

    float tHit = (abs(dir.y) < eps) ? maxDistance : (-camPos.y) / dir.y;
    tHit = (tHit < 0.0) ? maxDistance : ((tHit > maxDistance) ? maxDistance : tHit);

    float baseProjX = originX + dir.x * tHit;
    float baseProjZ = originZ + dir.z * tHit;

    float ndcAbsMaxX = 1.0 + overscanEff + extraSideNdc;
    float ndcAbsMaxY = max(abs(ndcBottom), abs(ndcTop));
    float edge01X = abs(ndcX) / max(ndcAbsMaxX, eps);
    float edge01Y = abs(ndcY) / max(ndcAbsMaxY, eps);
    float edgeWeightX = smoothstep(0.75, 1.0, edge01X);
    float edgeWeightY = smoothstep(0.75, 1.0, edge01Y);
    float edgeWeight = max(edgeWeightX, edgeWeightY);
    float ampHeightFactor = clamp(ampOverHeight, 0.0, 10.0);
    float expandScale = 2.0 + 3.0 * pitchDown + 0.35 * ampHeightFactor + 1.5 * forwardAlign + 0.75 * sideAlign;
    float expandMeters = (maxWaveAmp * expandScale + 2.0) * edgeWeight;
    if (expandMeters > 0.0) {
        float2 v = float2(baseProjX - originX, baseProjZ - originZ);
        float lenV = length(v);
        float2 dirXZ = (lenV > eps) ? (v / lenV) : normalize(float2(dir.x, dir.z));
        baseProjX += dirXZ.x * expandMeters;
        baseProjZ += dirXZ.y * expandMeters;
    }

    baseX = baseProjX;
    baseZ = baseProjZ;

    float3 displacedPos;
    float3 localNormal;
    ocean_gerstner(float2(baseX, baseZ), u, displacedPos, localNormal);

    out.position = u.scene.modelViewProj * float4(displacedPos, 1.0);
    out.worldPos = (u.scene.modelMatrix * float4(displacedPos, 1.0)).xyz;
    out.worldNormal = (u.scene.modelMatrix * float4(localNormal, 0.0)).xyz;
    float3 baseWorld = (u.scene.modelMatrix * float4(baseX, 0.0, baseZ, 1.0)).xyz;
    out.oceanBaseXZ = baseWorld.xz;

    // Color based on wave height (matches CPU color mapping).
    float heightFactor = clamp((displacedPos.y + 2.0) / 4.0, 0.0, 1.0);
    float3 water = float3(
        0.15 + heightFactor * 0.35,
        0.25 + heightFactor * 0.30,
        0.30 + heightFactor * 0.30
    );
    out.color = float4(water, 1.0);
    return out;
}

fragment float4 fragment_main_3d(
    Vertex3DOut in [[stage_in]],
    constant Scene3DUniforms& uniforms [[buffer(0)]],
    texture2d<float> diffuseTexture [[texture(0)]],
    sampler texSampler [[sampler(0)]]
) {
    float4 baseColor = in.color;
    if (uniforms.useTexture != 0) {
        float4 texColor = diffuseTexture.sample(texSampler, in.uv);
        baseColor *= texColor;
    }

    float3 N = normalize(in.worldNormal);
    float3 L = normalize(uniforms.lightDir);
    float diffuse = max(0.0, dot(N, L));
    float3 litColor = baseColor.rgb * (uniforms.ambient + (1.0 - uniforms.ambient) * diffuse);

    // Linear fog based on distance from camera
    // When fogEnd <= fogStart, fog is disabled (fogFactor = 1.0 means no fog)
    float dist = length(in.worldPos - float3(uniforms.cameraPos));
    float fogRange = uniforms.fogEnd - uniforms.fogStart;
    float fogFactor = (fogRange > 0.0) ? clamp((uniforms.fogEnd - dist) / fogRange, 0.0, 1.0) : 1.0;
    float3 finalColor = mix(float3(uniforms.fogColor), litColor, fogFactor);

    return float4(finalColor, baseColor.a);
}

fragment float4 fragment_ocean_3d(
    Vertex3DOut in [[stage_in]],
    constant Scene3DUniforms& scene [[buffer(0)]],
    constant OceanProjectedUniforms& u [[buffer(1)]]
) {
    // Seam handling between the two-pass ocean draw (local patch + projected grid):
    // Instead of hard clipping (which can leave cracks), we cross-fade in a small radial band.
    float snapSize = u.params1.x;
    float nearExtent = u.params2.z;
    float mode = u.params2.w;

    float maxWaveAmp = 0.0;
    for (uint i = 0; i < 4; i++) {
        maxWaveAmp += u.waveB[i].x;
    }
    float seamWidth = max(2.0, maxWaveAmp * 2.0);
    float donutRadius = nearExtent + seamWidth;
    float blendWidth = max(6.0, maxWaveAmp * 6.0);

    float originX = scene.cameraPos[0];
    float originZ = scene.cameraPos[2];
    if (snapSize > 0.00001) {
        originX = floor(originX / snapSize) * snapSize;
        originZ = floor(originZ / snapSize) * snapSize;
    }

    // Use undisplaced XZ so waves can't "push" fragments across the boundary and cause shimmering seams.
    float2 d = float2(in.oceanBaseXZ.x - originX, in.oceanBaseXZ.y - originZ);
    float r = length(d);
    float alpha = 1.0;
    if (donutRadius > 0.0 && blendWidth > 0.0) {
        float r0 = donutRadius - blendWidth;
        float r1 = donutRadius + blendWidth;
        float t = (r1 > r0) ? clamp((r - r0) / (r1 - r0), 0.0, 1.0) : 0.5;
        alpha = (mode > 0.5) ? (1.0 - t) : t;
        if (alpha < 0.001) discard_fragment();
    }

    float3 N = normalize(in.worldNormal);
    float3 L = normalize(scene.lightDir);
    float diffuse = max(0.0, dot(N, L));
    float3 litColor = in.color.rgb * (scene.ambient + (1.0 - scene.ambient) * diffuse);

    float dist = length(in.worldPos - float3(scene.cameraPos));
    float fogRange = scene.fogEnd - scene.fogStart;
    float fogFactor = (fogRange > 0.0) ? clamp((scene.fogEnd - dist) / fogRange, 0.0, 1.0) : 1.0;
    float3 finalColor = mix(float3(scene.fogColor), litColor, fogFactor);

    return float4(finalColor, alpha);
}

// mesh3d_textured.metal - Textured 3D shader with diffuse texture, lighting, and fog
// For loading and rendering 3D assets (FBX, OBJ) with textures
#include <metal_stdlib>
using namespace metal;

// 3D Textured Vertex input (matches AfferentVertex3DTextured layout)
// 12 floats per vertex: position(3) + normal(3) + uv(2) + color(4)
struct Vertex3DTexturedIn {
    float3 position [[attribute(0)]];
    float3 normal [[attribute(1)]];
    float2 uv [[attribute(2)]];
    float4 color [[attribute(3)]];
};

// 3D Textured Vertex output
struct Vertex3DTexturedOut {
    float4 position [[position]];
    float3 worldNormal;
    float3 worldPos;
    float2 uv;
    float4 color;
};

// Scene uniforms for textured 3D rendering
struct Scene3DTexturedUniforms {
    float4x4 modelViewProj;   // Combined MVP matrix
    float4x4 modelMatrix;     // Model matrix for normal transformation
    packed_float3 lightDir;   // Light direction (12 bytes, packed)
    float ambient;            // Ambient light factor
    packed_float3 cameraPos;  // Camera position for fog distance
    float fogStart;           // Distance where fog begins
    packed_float3 fogColor;   // Fog color (RGB)
    float fogEnd;             // Distance where fog is fully opaque
    float2 uvScale;           // UV tiling scale (default 1,1)
    float2 uvOffset;          // UV offset (default 0,0)
};

vertex Vertex3DTexturedOut vertex_main_3d_textured(
    Vertex3DTexturedIn in [[stage_in]],
    constant Scene3DTexturedUniforms& uniforms [[buffer(1)]]
) {
    Vertex3DTexturedOut out;
    out.position = uniforms.modelViewProj * float4(in.position, 1.0);
    // Transform normal to world space
    out.worldNormal = (uniforms.modelMatrix * float4(in.normal, 0.0)).xyz;
    // Pass world position for fog calculation
    out.worldPos = (uniforms.modelMatrix * float4(in.position, 1.0)).xyz;
    // Apply UV scale and offset
    out.uv = in.uv * uniforms.uvScale + uniforms.uvOffset;
    out.color = in.color;
    return out;
}

fragment float4 fragment_main_3d_textured(
    Vertex3DTexturedOut in [[stage_in]],
    constant Scene3DTexturedUniforms& uniforms [[buffer(0)]],
    texture2d<float> diffuseTexture [[texture(0)]],
    sampler texSampler [[sampler(0)]]
) {
    // Sample diffuse texture
    float4 texColor = diffuseTexture.sample(texSampler, in.uv);

    // Combine texture with vertex color (allows tinting)
    float4 baseColor = texColor * in.color;

    // Lighting calculation
    float3 N = normalize(in.worldNormal);
    float3 L = normalize(uniforms.lightDir);
    float diffuse = max(0.0, dot(N, L));
    float3 litColor = baseColor.rgb * (uniforms.ambient + (1.0 - uniforms.ambient) * diffuse);

    // Linear fog based on distance from camera
    float dist = length(in.worldPos - float3(uniforms.cameraPos));
    float fogRange = uniforms.fogEnd - uniforms.fogStart;
    float fogFactor = (fogRange > 0.0) ? clamp((uniforms.fogEnd - dist) / fogRange, 0.0, 1.0) : 1.0;
    float3 finalColor = mix(float3(uniforms.fogColor), litColor, fogFactor);

    return float4(finalColor, baseColor.a);
}

/*
PBR Extension Notes:
To upgrade this shader for full PBR rendering:

1. Add more texture inputs:
   texture2d<float> normalMap [[texture(1)]],
   texture2d<float> metallicMap [[texture(2)]],
   texture2d<float> roughnessMap [[texture(3)]],
   texture2d<float> aoMap [[texture(4)]]

2. Sample normal map and transform to world space:
   float3 tangentNormal = normalMap.sample(texSampler, in.uv).xyz * 2.0 - 1.0;
   // Requires tangent/bitangent in vertex data

3. Sample metallic/roughness:
   float metallic = metallicMap.sample(texSampler, in.uv).r;
   float roughness = roughnessMap.sample(texSampler, in.uv).r;

4. Implement Cook-Torrance BRDF:
   - F0 = mix(0.04, baseColor.rgb, metallic)
   - Normal Distribution Function (GGX)
   - Geometry Function (Smith)
   - Fresnel (Schlick)

5. Combine specular and diffuse contributions
*/

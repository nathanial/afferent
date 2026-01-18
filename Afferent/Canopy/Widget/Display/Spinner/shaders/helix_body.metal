const float PI = 3.14159265359;
const float TWO_PI = PI * 2.0;
const float PI_4 = PI * 0.25;
uint pair = idx / 2u;
bool strand2 = (idx % 2u) == 1u;
float y = (float(pair) / 8.0 - 0.5) * p.size * 0.7;
float phase = p.time * TWO_PI + float(pair) * PI_4;
float sinP = sin(phase);
float cosP = cos(phase);
if (strand2) { sinP = -sinP; cosP = -cosP; }
float depth = (cosP + 1.0) * 0.5;
// Animate hue based on time and circle index (inline HSV to RGB)
float hue = fract(p.time * 0.3 + float(idx) * 0.04);
float4 K = float4(1.0, 2.0/3.0, 1.0/3.0, 3.0);
float3 hp = abs(fract(float3(hue, hue, hue) + K.xyz) * 6.0 - K.www);
float3 rgb = mix(K.xxx, clamp(hp - K.xxx, 0.0, 1.0), 0.8);
CircleResult result;
result.center = p.center + float2(p.size * 0.3 * sinP, y);
result.radius = p.size * 0.05 * (0.6 + 0.4 * depth);
result.color = float4(rgb, p.color.a * (0.4 + 0.6 * depth));
return result;

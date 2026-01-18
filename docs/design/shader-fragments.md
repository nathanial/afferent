# Shader Fragments: Custom GPU Code for Widgets

## Executive Summary

This document explores a system that allows widget authors to write GPU shader code without managing full Metal pipelines. Widgets provide small "shader fragments" - functions that compute per-primitive properties. Afferent composes these into complete shaders, handles compilation, and manages execution.

**Goal**: Enable Lean widget authors to move computation to GPU with minimal Metal knowledge.

---

## 1. The Problem

### Current State

Widgets that need custom GPU behavior have two options:

1. **Use existing primitives** - Limited to what's built in. Complex patterns like helix require CPU-side computation (slow for many instances).

2. **Full FFI + native code** - Write Metal shaders, C wrappers, FFI bindings. High barrier, requires native toolchain.

### The Gap

There's no middle ground. A widget author who wants a custom animated pattern must either:
- Accept CPU overhead (1000s of trig calls per frame)
- Become a Metal/C developer

### Desired State

Widget authors write a small function in a shader language that describes their pattern. Afferent handles everything else.

---

## 2. Core Concept

### Shader Fragments

A **shader fragment** is a pure function that computes output properties from input parameters. It doesn't know about vertices, pipelines, or render passes - just math.

```metal
// Fragment: Computes circle properties for a helix pattern
// Inputs: instance params + primitive index
// Outputs: position, radius, alpha
fragment_circle helix(uint idx, HelixParams p) {
    uint pair = idx / 2;
    bool strand2 = (idx % 2) == 1;

    float y = (float(pair) / 8.0 - 0.5) * p.size * 0.7;
    float phase = p.time + float(pair) * M_PI_4;

    float sinP = sin(phase);
    float cosP = cos(phase);
    if (strand2) { sinP = -sinP; cosP = -cosP; }

    float x = p.center.x + p.size * 0.3 * sinP;
    float depth = (cosP + 1.0) * 0.5;

    return circle(
        float2(x, p.center.y + y),           // position
        p.size * 0.05 * (0.6 + 0.4 * depth), // radius
        p.color * float4(1, 1, 1, 0.4 + 0.6 * depth) // color with alpha
    );
}
```

### Fragment Types

Different fragment types for different primitive patterns:

| Fragment Type | Outputs | Use Case |
|---------------|---------|----------|
| `fragment_circle` | position, radius, color | Particle systems, spinners |
| `fragment_rect` | position, size, color, cornerRadius | Dynamic layouts |
| `fragment_line` | start, end, color, width | Graphs, connections |
| `fragment_vertex` | position, color | Custom geometry |
| `fragment_arc` | center, angles, radius, strokeWidth, color | Curved elements |

### Composition Model

Afferent provides **template shaders** with holes for fragments:

```metal
// Template: instanced_circles.metal (provided by Afferent)
struct CircleInstance { /* from fragment outputs */ };

vertex VertexOut instanced_circle_vertex(
    uint vid [[vertex_id]],
    uint iid [[instance_id]],
    constant Params& params [[buffer(0)]]
) {
    // === FRAGMENT CALL ===
    Circle c = FRAGMENT_FUNCTION(iid, params);
    // === END FRAGMENT ===

    // Standard circle vertex generation
    float angle = float(vid) / float(CIRCLE_SEGMENTS) * 2.0 * M_PI;
    float2 pos = c.position + float2(cos(angle), sin(angle)) * c.radius;
    // ... convert to NDC, output
}
```

At compile time (or runtime), `FRAGMENT_FUNCTION` is replaced with the widget's fragment.

---

## 3. Lean Integration

### Defining Fragments

Fragments are defined in Lean as string literals with metadata:

```lean
/-- Helix spinner shader fragment -/
def helixFragment : ShaderFragment := {
  name := "helix"
  fragmentType := .circle
  primitiveCount := 16  -- generates 16 circles per instance
  params := #[
    ("center", .float2),
    ("size", .float),
    ("time", .float),
    ("color", .float4)
  ]
  source := "
    uint pair = idx / 2;
    bool strand2 = (idx % 2) == 1;

    float y = (float(pair) / 8.0 - 0.5) * p.size * 0.7;
    float phase = p.time + float(pair) * M_PI_4;

    float sinP = sin(phase);
    float cosP = cos(phase);
    if (strand2) { sinP = -sinP; cosP = -cosP; }

    float x = p.center.x + p.size * 0.3 * sinP;
    float depth = (cosP + 1.0) * 0.5;

    return circle(
        float2(x, p.center.y + y),
        p.size * 0.05 * (0.6 + 0.4 * depth),
        p.color * float4(1, 1, 1, 0.4 + 0.6 * depth)
    );
  "
}
```

### Using Fragments in Widgets

```lean
def helixSpec (t : Float) (color : Color) (dims : Dimensions) : CustomSpec := {
  measure := fun _ _ => (dims.size, dims.size)
  collect := fun layout =>
    let rect := layout.contentRect
    let cx := rect.x + dims.size / 2
    let cy := rect.y + dims.size / 2

    RenderM.build do
      -- Single emit, 8 floats, GPU does all the work
      RenderM.fragmentDraw helixFragment {
        center := (cx, cy)
        size := dims.size
        time := t * Float.twoPi
        color := (color.r, color.g, color.b, color.a)
      }
  draw := none
}
```

### RenderCommand Extension

```lean
inductive RenderCommand where
  -- ... existing commands ...

  /-- Draw using a shader fragment.
      The fragment generates `primitiveCount` primitives per instance.
      params is packed float data matching the fragment's param declaration. -/
  | fragmentDraw (fragment : ShaderFragment) (params : Array Float) (instanceCount : Nat := 1)
```

---

## 4. Compilation Pipeline

### Build-Time Compilation (Preferred)

Fragments are compiled to Metal libraries at `lake build` time:

```
┌─────────────────┐     ┌──────────────────┐     ┌─────────────────┐
│ ShaderFragment  │────▶│ Fragment Compiler │────▶│ .metallib file  │
│ (Lean source)   │     │ (lake build hook) │     │ (binary shader) │
└─────────────────┘     └──────────────────┘     └─────────────────┘
```

1. Lake build script extracts fragment source from Lean
2. Composes with appropriate template shader
3. Compiles with `xcrun metal` / `xcrun metallib`
4. Embeds resulting `.metallib` in the executable

**Advantages**: No runtime compilation, faster startup, catch errors at build time.

### Runtime Compilation (Fallback)

For development/REPL, compile fragments on first use:

```swift
func getOrCompilePipeline(fragment: ShaderFragment) -> MTLRenderPipelineState {
    if let cached = pipelineCache[fragment.name] {
        return cached
    }

    let source = composeShader(template: circleTemplate, fragment: fragment.source)
    let library = device.makeLibrary(source: source, options: nil)
    let pipeline = createPipeline(library: library)

    pipelineCache[fragment.name] = pipeline
    return pipeline
}
```

**Advantages**: Faster iteration during development.

### Hybrid Approach

- Build-time: Compile all fragments found in the codebase
- Runtime: Fall back to JIT for dynamically-created fragments
- Cache: Both share a pipeline cache keyed by fragment hash

---

## 5. Fragment Language

### Design Goals

1. **Familiar** - C-like syntax, similar to Metal/GLSL
2. **Safe** - No pointers, no unbounded loops, no resource access
3. **Minimal** - Only what's needed for computing primitive properties

### Built-in Types

```
// Scalars
float, int, uint, bool

// Vectors
float2, float3, float4
int2, int3, int4

// Matrices (if needed)
float2x2, float3x3, float4x4
```

### Built-in Functions

```
// Trigonometry
sin, cos, tan, asin, acos, atan, atan2

// Math
abs, floor, ceil, round, fract
min, max, clamp, mix, step, smoothstep
sqrt, pow, exp, log

// Vector
length, distance, dot, cross, normalize
reflect, refract

// Utility
fmod, sign
```

### Output Constructors

Each fragment type has a constructor for its output:

```metal
// For fragment_circle
circle(float2 position, float radius, float4 color)

// For fragment_rect
rect(float2 position, float2 size, float4 color, float cornerRadius)

// For fragment_line
line(float2 start, float2 end, float4 color, float width)

// For fragment_arc
arc(float2 center, float startAngle, float sweepAngle,
    float radius, float strokeWidth, float4 color)
```

### Constraints

- No loops (unroll at compile time if needed)
- No recursion
- No texture sampling (separate feature if needed)
- No side effects
- Bounded execution time

---

## 6. Parameter Passing

### Lean → GPU Data Flow

```
┌────────────────┐     ┌─────────────────┐     ┌──────────────────┐
│ Lean struct    │────▶│ Pack to Array   │────▶│ GPU buffer       │
│ {center, size} │     │ Float           │     │ (uniform/SSBO)   │
└────────────────┘     └─────────────────┘     └──────────────────┘
```

### Automatic Packing

The `fragmentDraw` helper automatically packs Lean values:

```lean
structure HelixParams where
  center : Float × Float
  size : Float
  time : Float
  color : Float × Float × Float × Float

def packParams (p : HelixParams) : Array Float :=
  #[p.center.1, p.center.2, p.size, p.time,
    p.color.1, p.color.2, p.color.3, p.color.4]
```

### GPU-Side Unpacking

The template shader unpacks into a struct:

```metal
struct HelixParams {
    float2 center;
    float size;
    float time;
    float4 color;
};

// In vertex shader
HelixParams p;
p.center = float2(params[0], params[1]);
p.size = params[2];
p.time = params[3];
p.color = float4(params[4], params[5], params[6], params[7]);
```

This unpacking code is generated from the fragment's param declaration.

---

## 7. Integration with Render Pipeline

### Batching

Multiple instances using the same fragment batch naturally:

```lean
-- 1000 helix spinners, each emits one fragmentDraw
-- Backend collects all with same fragment into one draw call
-- GPU renders 1000 instances × 16 circles = 16000 circles in one call
```

### Clipping

Fragments operate in local coordinates. The template shader applies viewport transform and scissor test:

```metal
vertex VertexOut fragment_vertex(...) {
    Circle c = FRAGMENT_FUNCTION(iid, params);

    // Apply instance transform (translation from widget position)
    c.position += instanceTransforms[iid];

    // Standard NDC conversion
    out.position = toNDC(c.position, viewport);

    // Scissor is handled by Metal pipeline state
    return out;
}
```

### Z-Ordering

Fragment draws are ordered like other commands - by their position in the command stream. The coalescing pass groups fragments by type while preserving relative order with other command types.

---

## 8. Implementation Phases

### Phase 1: Core Infrastructure (MVP)

**Goal**: One working fragment type (circles) with build-time compilation.

1. Define `ShaderFragment` Lean structure
2. Create circle template shader with fragment slot
3. Build script to extract and compile fragments
4. FFI to load and execute compiled shaders
5. `RenderCommand.fragmentDraw` variant
6. Backend execution path

**Deliverable**: Helix spinner using fragment system.

### Phase 2: Developer Experience

**Goal**: Make fragments easy to write and debug.

1. Runtime compilation fallback for development
2. Error messages that map back to fragment source
3. Fragment validation (type checking params)
4. Hot reload support

### Phase 3: Additional Fragment Types

**Goal**: Cover common use cases.

1. `fragment_rect` - Dynamic rectangles
2. `fragment_line` - Line graphs, connections
3. `fragment_arc` - Curved elements (could replace current arc instancing)
4. `fragment_vertex` - Arbitrary geometry

### Phase 4: Advanced Features

**Goal**: Power user capabilities.

1. Fragment composition (call other fragments)
2. Texture sampling in fragments
3. Compute shader fragments (for non-rendering computation)
4. Animation system integration (time uniforms)

---

## 9. Example: Wave Spinner

Demonstrating reuse of the fragment system:

```lean
def waveFragment : ShaderFragment := {
  name := "wave"
  fragmentType := .circle
  primitiveCount := 7
  params := #[
    ("center", .float2),
    ("size", .float),
    ("time", .float),
    ("color", .float4)
  ]
  source := "
    float spacing = p.size * 0.12;
    float amplitude = p.size * 0.15;
    float radius = p.size * 0.055;

    float xOffset = (float(idx) - 3.0) * spacing;
    float phase = p.time * 2.0 - float(idx) * M_PI / 3.0;
    float yOffset = amplitude * sin(phase);

    return circle(
        p.center + float2(xOffset, yOffset),
        radius,
        p.color
    );
  "
}

def waveSpec (t : Float) (color : Color) (dims : Dimensions) : CustomSpec := {
  measure := fun _ _ => (dims.size, dims.size)
  collect := fun layout =>
    let cx := layout.contentRect.x + dims.size / 2
    let cy := layout.contentRect.y + dims.size / 2
    RenderM.build do
      RenderM.fragmentDraw waveFragment {
        center := (cx, cy)
        size := dims.size
        time := t * Float.twoPi
        color := (color.r, color.g, color.b, color.a)
      }
  draw := none
}
```

---

## 10. Open Questions

### Language Design

1. **Should fragments be Metal directly or a DSL?**
   - Metal: Familiar to graphics programmers, full power
   - DSL: Safer, portable (could target WebGPU later), simpler
   - Recommendation: Start with restricted Metal subset, consider DSL later

2. **How to handle errors in fragment code?**
   - Build-time: Metal compiler errors, need to map back to Lean source
   - Runtime: Shader compilation failure, need graceful fallback

3. **Should fragments support local variables?**
   - Yes, but no mutable state that persists across primitives
   - Each primitive evaluation is independent

### Performance

4. **How many pipeline states can we have?**
   - Each unique fragment = one pipeline state
   - Metal handles many pipelines fine, but switching has cost
   - Batch same-fragment draws together

5. **Parameter buffer management?**
   - Per-frame upload of all fragment params
   - Use buffer pooling like existing batch rendering

### Integration

6. **How do fragments interact with transforms?**
   - Fragments compute in local widget coordinates
   - Template applies widget-to-screen transform
   - Consistent with existing CustomSpec behavior

7. **Can fragments read from textures?**
   - Phase 4 feature
   - Would need texture binding declarations in fragment metadata

---

## 11. Alternatives Considered

### Why not a visual shader editor?

- Overkill for a code-first framework
- Adds significant tooling complexity
- Fragments serve the same purpose with less overhead

### Why not WebGPU/WGSL?

- Afferent is macOS-focused (Metal)
- WGSL support could be added later via the DSL path
- Direct Metal gives best performance and debugging

### Why not just add more built-in primitives?

- Doesn't scale - every pattern needs core changes
- Fragments let the ecosystem grow without core bloat
- Built-in primitives are still the fast path for common cases

---

## 12. Conclusion

Shader fragments provide a middle ground between "use built-in primitives" and "write full native code". They enable:

- **Widget authors**: Write GPU code in Lean files
- **Core maintainers**: Keep core small, let ecosystem extend
- **Performance**: Move arbitrary computation to GPU
- **Future portability**: Fragment abstraction could target multiple backends

The phased implementation approach lets us validate the design with a real use case (helix spinner) before building out the full system.

### Recommended Next Step

Implement Phase 1 (MVP) with the helix spinner as the proving ground. This will surface design issues early while delivering immediate value.

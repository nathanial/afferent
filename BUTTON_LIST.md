# Button Variants

Catalog of button variants for Afferent's Canopy widget library. Organized into four tiers, from standard UI essentials to experimental shader-driven designs.

Rendering approach key:
- **RC** = Render commands (fillRect, strokeRect, fillText — no shader needed)
- **Shader** = Shader DSL fragment (CircleShader, RectShader, or QuadShader)
- **Tess** = Pre-tessellated geometry (polygons, Bezier curves, triangle fans)
- **Composite** = Combines multiple existing widgets/primitives

---

## Tier 1: Standard

Buttons every serious UI framework provides. These round out the existing primary/secondary/outline/ghost set.

| # | Name | Description | Approach |
|---|------|-------------|----------|
| 1 | **Icon** | Square button containing only an icon glyph, no label text. Compact for toolbars and action bars. | RC |
| 2 | **IconLabel** | Icon placed before (or after) the label text. The most common real-world button layout. | RC |
| 3 | **FAB** | Floating Action Button. Circular, elevated with shadow, typically holds a single icon. Canonical mobile pattern. | RC |
| 4 | **MiniFAB** | Smaller FAB variant for secondary floating actions. | RC |
| 5 | **ExtendedFAB** | Pill-shaped FAB with icon and label, wider than standard FAB. | RC |
| 6 | **Toggle** | Button that stays pressed/active to represent on/off state. Visual indicator for active state. | RC |
| 7 | **ToggleGroup** | Row of mutually exclusive toggle buttons — only one active at a time. Segmented control pattern. | Composite |
| 8 | **Split** | Primary action button with a dropdown arrow section separated by a divider. Click left for default action, right for menu. | Composite |
| 9 | **Dropdown** | Button that opens a dropdown menu beneath it. Shows chevron/caret indicator. | Composite |
| 10 | **Loading** | Button that replaces its label with a spinner while an async operation runs. Prevents double-submission. | Composite |
| 11 | **Danger** | Destructive action button. Red-toned, optionally with a warning icon. Used for delete/remove actions. | RC |
| 12 | **Success** | Confirmation/positive action button. Green-toned. Used for save/confirm/approve actions. | RC |
| 13 | **Pill** | Fully rounded corners (cornerRadius = height/2). Softer, friendlier appearance than standard rounded rect. | RC |
| 14 | **Link** | Styled as inline text with underline on hover. No background, no border. Blends into paragraph text. | RC |
| 15 | **Compact** | Reduced padding and font size for dense UIs. Same variants as standard but tighter. | RC |

---

## Tier 2: Animated

Standard shapes enhanced with time-driven or state-driven animation. Each uses the existing rectangular/pill form factor but adds motion.

| # | Name | Description | Approach |
|---|------|-------------|----------|
| 16 | **Ripple** | Material Design-style ink ripple expanding from the click point outward, then fading. | Shader (QuadShader) |
| 17 | **Pulse** | Gentle scale pulse on idle — the button breathes in and out. Draws attention without being aggressive. | Shader |
| 18 | **GlowOnHover** | Soft outer glow bloom that fades in on hover. Color matches the button's variant. | Shader (QuadShader) |
| 19 | **BorderTrace** | On hover, the border draws itself around the perimeter like a racing light. Starts from click point or top-left. | Shader |
| 20 | **ShimmerLoading** | A diagonal highlight band sweeps across the button surface on loop. Skeleton-screen loading feel. | Shader (QuadShader) |
| 21 | **Bounce** | Button compresses down on press and springs back with elastic overshoot on release. Tactile feel. | RC (animated transform) |
| 22 | **Jelly** | Soft squish deformation on press — wider and shorter — then wobbles back. Playful elastic physics. | Shader |
| 23 | **Typewriter** | Label text types in character by character on hover, with a blinking cursor. Reveals the label progressively. | RC (animated text) |
| 24 | **SlideReveal** | Background color slides in from one edge on hover (like a curtain wipe), slides out on unhover. | Shader (QuadShader) |
| 25 | **Heartbeat** | Double-pulse rhythm (like ECG) on idle. Urgent/attention-grabbing for CTAs. | Shader |

---

## Tier 3: Shader-Driven

Full GPU fragment effects. These go beyond what CSS/standard 2D rendering can do, leveraging QuadShader for per-pixel computation.

| # | Name | Description | Approach |
|---|------|-------------|----------|
| 26 | **Gradient** | Animated linear gradient that slowly shifts angle and color stops over time. Smooth, modern feel. | Shader (QuadShader) |
| 27 | **Aurora** | Layered sine waves of translucent color bands drifting across the button, like the northern lights. Multiple overlapping hues. | Shader (QuadShader) |
| 28 | **Glass** | Frosted glass / glassmorphism. Semi-transparent with blur effect and subtle refraction. Content behind the button is visible but diffused. | Shader (QuadShader) |
| 29 | **Neon** | Dark background with bright neon-colored border that glows and flickers subtly. Cyberpunk aesthetic. Optional text glow. | Shader (QuadShader) |
| 30 | **Holographic** | Rainbow iridescent shimmer that shifts based on time, simulating a holographic foil surface. Prismatic color bands. | Shader (QuadShader) |
| 31 | **Plasma** | Classic plasma effect — overlapping sinusoidal color fields creating organic, lava-lamp-like motion. | Shader (QuadShader) |
| 32 | **Fire** | Flames licking upward from the bottom edge. Procedural noise-based fire with orange/yellow/red palette. | Shader (QuadShader) |
| 33 | **Electric** | Crackling energy arcs dancing across the surface. Lightning-bolt procedural lines with bright white/blue. Intensifies on hover. | Shader (QuadShader) |
| 34 | **Frost** | Ice crystal patterns growing inward from the edges on hover. Fractal branching with cool blue/white tones. | Shader (QuadShader) |
| 35 | **Ember** | Dark surface with glowing ember particles drifting upward. Warm orange pinpoints of light with soft trails. | Shader (QuadShader) |
| 36 | **Nebula** | Deep space nebula clouds — layered procedural noise in purples, blues, and pinks. Stars twinkle as bright pixels. | Shader (QuadShader) |
| 37 | **Ocean** | Stylized water surface with caustic light patterns rippling across the button. Cool blue-green palette. | Shader (QuadShader) |
| 38 | **Matrix** | Digital rain of characters falling down the button surface. Green-on-black terminal aesthetic. | Shader (QuadShader) |
| 39 | **Prism** | White light enters one side and fans out into a rainbow spectrum across the surface. Pink Floyd homage. | Shader (QuadShader) |
| 40 | **Terrain** | Topographic contour lines rendered procedurally. Lines shift and morph slowly. Cartographic/technical aesthetic. | Shader (QuadShader) |
| 41 | **Circuit** | Procedural circuit board trace pattern. Glowing traces with node points. Data pulses travel along the traces on click. | Shader (QuadShader) |
| 42 | **Warp** | Starfield warp-speed effect — dots streaking outward from center. Accelerates on hover. | Shader (QuadShader) |
| 43 | **Voronoi** | Animated Voronoi cell pattern with colored regions and visible cell edges. Cells shift and reconfigure over time. | Shader (QuadShader) |
| 44 | **Radar** | Spinning radar sweep line over a dark surface with glowing blips. Tactical/military aesthetic. | Shader (QuadShader) |

---

## Tier 4: Whimsical & Experimental

Unconventional interactive concepts that push beyond traditional button paradigms.

| # | Name | Description | Approach |
|---|------|-------------|----------|
| 45 | **Magnetic** | Button subtly tilts/shifts toward the cursor as it approaches, as if attracted. 3D perspective transform based on mouse position relative to button center. | RC (animated transform) |
| 46 | **Shatter** | On click, the button surface fractures into triangular shards that fly outward, then reassemble. Dramatic confirmation effect. | Tess + Shader |
| 47 | **Confetti** | On click, a burst of colorful confetti particles erupts from the button. Celebration/success feedback. | Shader (CircleShader, instanced) |
| 48 | **Melt** | On press, the button appears to melt downward like hot wax, then resolidifies on release. | Shader (QuadShader) |
| 49 | **Sketch** | Hand-drawn aesthetic — wobbly borders that subtly redraw each frame, as if sketched with a shaky pen. Cross-hatched fill. | Shader (QuadShader) |
| 50 | **Retro** | Pixel-art style with chunked corners, dithered gradients, and 8-bit color palette. Optional scanlines. | Shader (QuadShader) |
| 51 | **Origami** | Appears as a folded paper shape. On hover, it unfolds to reveal the label. Simulated paper creases with light/shadow. | Shader (QuadShader) |
| 52 | **Bubble** | Transparent soap bubble surface with iridescent thin-film interference colors. Pops on click with a satisfying burst, then reforms. | Shader (QuadShader) |
| 53 | **Moss** | Organic growth — green/natural texture that spreads to fill the button on hover, recedes on unhover. Living surface. | Shader (QuadShader) |
| 54 | **Sandstorm** | Surface of swirling sand particles. On click, they scatter and reform. Desert/archaeological aesthetic. | Shader (QuadShader) |
| 55 | **Portal** | Swirling vortex background pulling inward toward center. Intensifies on hover. Sci-fi dimensional rift. | Shader (QuadShader) |
| 56 | **Chalkboard** | Matte dark green surface with chalk-white text. Chalk dust particles fall on click. Eraser smudge on hover. | Shader (QuadShader) |
| 57 | **Stained Glass** | Colorful geometric segments with dark leading lines, like a cathedral window. Light shifts through it over time. | Shader (QuadShader) |
| 58 | **Clockwork** | Visible gears and mechanisms turning inside a transparent button face. Steampunk aesthetic with brass tones. | Tess + Shader |
| 59 | **Liquid Metal** | Mercury/T-1000 surface that deforms and ripples on hover. Highly reflective, chrome appearance. | Shader (QuadShader) |
| 60 | **Constellation** | Dark field with stars connected by faint lines forming a constellation pattern. Stars brighten on hover. | Shader (CircleShader, instanced) |

---

## Implementation Notes

- Start with Tier 1 (standard) to fill out the practical button library.
- Tier 2 buttons can reuse existing button infrastructure, adding animation via `useElapsedTime` and `useAnimationFrame`.
- Tier 3/4 buttons follow the spinner pattern: each variant gets its own file under `Afferent/Canopy/Widget/Input/Button/`, with a `ButtonVariant` enum and dispatch in a shared `Component.lean`.
- QuadShader (per-pixel fragment) is the workhorse for Tier 3 — it allows arbitrary per-pixel computation within the button's bounding rect.
- Several Tier 4 variants combine shader rendering with tessellated geometry (Shatter, Clockwork) or instanced particles (Confetti, Constellation).
- All variants should support the existing interactive states (hovered, focused, pressed, disabled) and integrate with the theme system for base colors.

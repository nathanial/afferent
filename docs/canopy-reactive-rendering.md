# Canopy Reactive Rendering Patterns

This document explains how the Canopy reactive widget system triggers re-renders, and the tradeoffs between different rendering patterns.

## The Monad Stack

Canopy uses a three-level monad stack:

```
WidgetM α = StateT WidgetMState ReactiveM α
                    ↓
ReactiveM α = ReaderT ReactiveEvents SpiderM α
                       ↓
SpiderM α ≈ IO (managed by Reactive.Host.Spider)
```

- **WidgetM** - Accumulates widget renders, provides component hooks
- **ReactiveM** - Carries event context (clicks, hovers, animation frames)
- **SpiderM** - Executes IO, runs the reactive network

## How Re-Rendering Works

### The Two-Phase Render Cycle

Rendering is a **two-phase process** driven by the main loop:

**Phase 1: Push Events (propagation)**
```
Main loop: state.inputs.fireAnimationFrame(env.dt)
    ↓
Spider propagation queue fires all subscribers (height-ordered)
    ↓
Dynamics update to new values
```

**Phase 2: Pull Values (render)**
```
Main loop: state.appState.render()  -- called AFTER firing events
    ↓
runWidget executes all collected emit closures
    ↓
Dynamic.sample pulls current (already-updated) values
    ↓
WidgetBuilder tree constructed and rendered
```

### Key Insight: Temporal Ordering

The main loop guarantees that **all events propagate before rendering**:

```lean
-- From Demos/DemoRegistry.lean
update := fun env state => do
  state.inputs.fireAnimationFrame env.dt    -- (1) FIRST: fire events
  let widget ← state.appState.render        -- (2) THEN: render samples
```

This ordering is what makes `sample` safe inside `emit` closures.

### The Complete Call Stack

```
Runner.lean main loop (every frame)
  │
  ├─→ fireAnimationFrame(dt)           -- Push: triggers propagation
  │     └─→ Spider.PropagationQueue    -- Height-ordered event firing
  │           └─→ Event subscribers    -- Update Dynamics
  │
  └─→ appState.render()                -- Pull: executes emit closures
        └─→ runWidget                  -- Collects and runs children
              └─→ emit closure         -- IO WidgetBuilder
                    └─→ sample         -- Reads current Dynamic value
```

## Two Rendering Patterns

### Pattern 1: `emit` + `sample`

```lean
emit do
  let hovered ← isHovered.sample
  let anim ← animProgress.sample
  let state : WidgetState := { hovered, pressed := false, focused := false }
  pure (animatedSwitchVisual name label theme anim state)
```

**How it works:**
- `emit` collects an `IO WidgetBuilder` closure into `WidgetMState.children`
- The closure is NOT executed immediately
- When `render()` is called, all collected closures execute
- `sample` pulls current values from Dynamics at render time

**Characteristics:**
- Simple, readable code
- Rebuilds widget every frame, regardless of whether state changed
- Frame-driven: tied to the render loop frequency

### Pattern 2: `dynWidget`

```lean
let renderState ← Dynamic.zipWithM (·, ·) isHovered animProgress
let _ ← dynWidget renderState fun (hovered, anim) => do
  let state : WidgetState := { hovered, pressed := false, focused := false }
  emit do pure (animatedSwitchVisual name label theme anim state)
```

**How it works:**
- `dynWidget` subscribes to the Dynamic's `updated` event
- Widget subtree only rebuilds when the Dynamic actually changes
- More aligned with pure FRP push-based semantics

**Characteristics:**
- Change-driven: only rebuilds on actual state changes
- Requires combining multiple Dynamics with `zipWithM`
- More efficient for widgets that are often idle

## Comparison: When to Use Each

### Performance Characteristics

| Widget State | `emit` + `sample` | `dynWidget` |
|--------------|-------------------|-------------|
| Idle (no interaction) | Rebuilds every frame | No rebuilds |
| Animating | Rebuilds every frame | Rebuilds every frame |
| Hover state change | Rebuilds every frame | Rebuilds on change |

### Decision Guide

| Pattern | Best For |
|---------|----------|
| `emit` + `sample` | Continuously animated widgets (particles, live graphs), rapid prototyping |
| `dynWidget` | Discrete-state widgets (buttons, checkboxes), production UIs with many widgets |

### Scaling Considerations

With `emit` + `sample`:
- N widgets = N rebuilds per frame, always
- Cost is constant regardless of interaction

With `dynWidget`:
- Idle widgets have zero rebuild cost
- Only actively changing widgets rebuild
- Better for UIs with many widgets where most are idle

## Example: Refactoring Switch to Use `dynWidget`

### Before (emit + sample)

```lean
def switch (label : Option String) (theme : Theme) (initialOn : Bool := false)
    : WidgetM SwitchResult := do
  let name ← registerComponentW "switch"
  let isHovered ← useHover name
  let clicks ← useClick name
  let animFrames ← useAnimationFrame

  let isOn ← Reactive.foldDyn (fun _ on => !on) initialOn clicks
  let onToggle := isOn.updated

  let initialAnim := if initialOn then 1.0 else 0.0
  let animProgress ← SpiderM.fixDynM fun animBehavior => do
    let updateEvent ← Event.attachWithM
      (fun (anim, on) dt =>
        let animSpeed := 8.0
        let rawFactor := animSpeed * dt
        let lerpFactor := if rawFactor > 1.0 then 1.0 else rawFactor
        let target := if on then 1.0 else 0.0
        let diff := target - anim
        if diff.abs < 0.01 then target else anim + diff * lerpFactor)
      (Reactive.Behavior.zipWith Prod.mk animBehavior isOn.current)
      animFrames
    Reactive.holdDyn initialAnim updateEvent

  emit do
    let hovered ← isHovered.sample
    let anim ← animProgress.sample
    let state : WidgetState := { hovered, pressed := false, focused := false }
    pure (animatedSwitchVisual name label theme anim state)

  pure { onToggle, isOn, animProgress }
```

### After (dynWidget)

```lean
def switch (label : Option String) (theme : Theme) (initialOn : Bool := false)
    : WidgetM SwitchResult := do
  let name ← registerComponentW "switch"
  let isHovered ← useHover name
  let clicks ← useClick name
  let animFrames ← useAnimationFrame

  let isOn ← Reactive.foldDyn (fun _ on => !on) initialOn clicks
  let onToggle := isOn.updated

  let initialAnim := if initialOn then 1.0 else 0.0
  let animProgress ← SpiderM.fixDynM fun animBehavior => do
    let updateEvent ← Event.attachWithM
      (fun (anim, on) dt =>
        let animSpeed := 8.0
        let rawFactor := animSpeed * dt
        let lerpFactor := if rawFactor > 1.0 then 1.0 else rawFactor
        let target := if on then 1.0 else 0.0
        let diff := target - anim
        if diff.abs < 0.01 then target else anim + diff * lerpFactor)
      (Reactive.Behavior.zipWith Prod.mk animBehavior isOn.current)
      animFrames
    Reactive.holdDyn initialAnim updateEvent

  -- Combine dynamics for efficient change detection
  let renderState ← Dynamic.zipWithM (fun h a => (h, a)) isHovered animProgress

  let _ ← dynWidget renderState fun (hovered, anim) => do
    let state : WidgetState := { hovered, pressed := false, focused := false }
    emit do pure (animatedSwitchVisual name label theme anim state)

  pure { onToggle, isOn, animProgress }
```

## Optimization: Stop Animation Updates When Idle

The switch animation updates `animProgress` every frame via `useAnimationFrame`. Even with `dynWidget`, this causes rebuilds every frame during animation.

To make the switch truly idle-efficient, stop firing updates once animation reaches its target. Use `Event.mapMaybeM` which filters and transforms in one step:

```lean
-- First attach the behavior to get combined values
let combined ← Event.attachWithM
  (fun (anim, on) dt => (anim, on, dt))
  (Reactive.Behavior.zipWith Prod.mk animBehavior isOn.current)
  animFrames

-- Then filter out updates when animation is complete
let updateEvent ← Event.mapMaybeM
  (fun (anim, on, dt) =>
    let animSpeed := 8.0
    let rawFactor := animSpeed * dt
    let lerpFactor := if rawFactor > 1.0 then 1.0 else rawFactor
    let target := if on then 1.0 else 0.0
    let diff := target - anim
    -- Only fire event if animation is still moving
    if diff.abs < 0.001 then
      none  -- Skip update, animation complete
    else
      some (anim + diff * lerpFactor))
  combined
```

With this optimization:
- During animation: `dynWidget` rebuilds each frame (necessary)
- When idle: No updates fire, `dynWidget` doesn't rebuild

## Summary

The Canopy reactive system uses a **frame-synchronized hybrid model**:

- **Push**: Events fire and propagate through the FRP network
- **Pull**: `sample` reads state after propagation completes
- **Synchronization**: Main loop ensures push happens before pull

Both `emit` + `sample` and `dynWidget` are valid patterns. Choose based on:

1. **Widget behavior**: Continuous animation vs discrete state changes
2. **Scale**: Few widgets vs many widgets
3. **Code simplicity**: Prototyping vs production

For production UIs with many widgets, prefer `dynWidget` to minimize unnecessary rebuilds.

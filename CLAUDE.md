# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Afferent is a Lean 4 2D/3D graphics and UI framework targeting macOS with Metal GPU rendering. It provides:
- HTML5 Canvas-style 2D API for shapes, paths, gradients, and text
- 3D rendering with perspective projection, lighting, fog, and procedural effects
- Declarative widget system with Elm-style architecture
- CSS-style layout engine (Flexbox and Grid)
- High-performance rendering via GPU instancing and zero-copy buffers

## Build Commands

**IMPORTANT:** Do not use `lake` directly. Use the provided shell scripts which set `LEAN_CC=/usr/bin/clang` for proper macOS framework linking (the bundled lld linker doesn't handle macOS frameworks).

```bash
# Build the project
./build.sh

# Build a specific target
./build.sh afferent
./build.sh hello_triangle
./build.sh spinning_cubes

# Build and run
./run.sh                  # Runs afferent (default)
./run.sh hello_triangle   # Runs the example

# Run tests
./test.sh
```

## Architecture

```
┌──────────────────────────────────────┐
│   Lean 4 Application                 │  Main.lean, Demos/, Examples/
│   (demos, examples, user code)       │
├──────────────────────────────────────┤
│   Canopy (Reactive Widgets)          │  Afferent/Canopy/*.lean
│   FRP-powered WidgetM, hooks, events │  (uses reactive library)
├──────────────────────────────────────┤
│   Arbor (Widget Builders)            │  Afferent/Arbor/*.lean
│   declarative widget construction    │  (uses trellis for layout)
├──────────────────────────────────────┤
│   Widget System                      │  Afferent/Widget/*.lean
│   declarative UI, events, Elm arch   │
├──────────────────────────────────────┤
│   Layout Engine                      │  Afferent/Layout/*.lean
│   CSS Flexbox, Grid, constraints     │
├──────────────────────────────────────┤
│   Canvas API                         │  Afferent/Canvas/*.lean
│   2D drawing, state, transforms      │
├──────────────────────────────────────┤
│   3D Rendering                       │  Afferent/Render/*.lean
│   Matrix4, FPSCamera, Mesh, Dynamic  │
├──────────────────────────────────────┤
│   Core Types                         │  Afferent/Core/*.lean
│   Point, Color, Rect, Path, Paint    │
├──────────────────────────────────────┤
│   Text Rendering                     │  Afferent/Text/Font.lean
│   FreeType fonts, measurement        │
├──────────────────────────────────────┤
│   FFI Layer                          │  Afferent/FFI/*.lean
│   Window, Renderer, Texture, 3D      │
├──────────────────────────────────────┤
│   Native Code                        │  native/src/
│   Metal pipeline, FreeType           │
└──────────────────────────────────────┘
```

## Key Patterns

### Arbor (Declarative Widgets)

Build widget trees using `Arbor.row`, `Arbor.column`, `Arbor.text`, `Arbor.box`, `Arbor.spacer`:

```lean
let widget := Arbor.column (gap := 8) (style := {}) #[
  Arbor.text "Hello" { color := Color.white, ... },
  Arbor.row (gap := 4) (style := {}) #[ ... ]
]
```

### FFI (Opaque Handles)

Use the NonemptyType pattern for native handles:

```lean
opaque WindowPointed : NonemptyType
def Window : Type := WindowPointed.type

@[extern "lean_afferent_window_create"]
opaque Window.create (width height : UInt32) (title : @& String) : IO Window
```

## Dependencies

- **collimator** - Profunctor optics library for Lean 4 (state management)
- **reactive** - Reflex-style FRP library (Event, Behavior, Dynamic)
- **trellis** - CSS-style layout engine (Flexbox, Grid)
- **tincture** - Color library (Color type, named colors, HSV/HSL)
- **FreeType** - Font rendering (Homebrew: `brew install freetype`)
- **Assimptor** - Assimp 3D model loading wrapper (see `../assimptor`)
- **Metal/Cocoa/QuartzCore** - macOS frameworks for GPU rendering

## Canopy (Reactive Widget System)

Canopy (`Afferent.Canopy.*`) provides a Reflex-DOM style reactive widget system. It builds on the `reactive` library's FRP primitives to enable declarative, composable UI components.

### Monad Stack

```
WidgetM α = StateT WidgetMState ReactiveM α   -- Accumulates widget renders
ReactiveM α = ReaderT ReactiveEvents SpiderM α -- Carries event context
SpiderM α ≈ IO α                               -- Reactive runtime
```

- **WidgetM** - Build widget trees, emit renders, use hooks
- **ReactiveM** - Access event streams, create subscriptions
- **SpiderM** - Execute IO, run reactive network

### Core FRP Types (from `reactive` library)

```lean
-- Discrete occurrences (push-based)
Event Spider α       -- Fires values to subscribers

-- Time-varying values (pull-based)
Behavior Spider α    -- Sampable at any time

-- Behavior + change notification
Dynamic Spider α     -- .current : Behavior, .updated : Event, .sample : IO α
```

### Key Combinators

```lean
-- Create from initial value + update event
holdDyn : α → Event Spider α → m (Dynamic Spider α)

-- Fold over events (like Redux reducer)
foldDyn : (α → β → β) → β → Event Spider α → m (Dynamic Spider β)

-- Transform events
Event.map : (α → β) → Event Spider α → Event Spider β
Event.mapM : (α → m β) → Event Spider α → m (Event Spider β)
Event.filter : (α → Bool) → Event Spider α → Event Spider α

-- Merge events (leftmost wins on simultaneous)
Event.leftmost : List (Event Spider α) → m (Event Spider α)

-- Execute IO effects when event fires
performEvent_ : Event Spider (IO Unit) → m Unit
```

### IMPORTANT: Never Use `sample` in Widgets

**Do NOT use `sample` or `Dynamic.sample` in Canopy widgets.** Sampling breaks the reactive data flow by pulling values imperatively instead of pushing updates through the FRP network.

Instead, use `dynWidget` to rebuild widget subtrees when dynamics change:

```lean
-- BAD: Sampling breaks reactivity
emit do
  let value ← someDynamic.sample  -- DON'T DO THIS
  pure (someVisual value)

-- GOOD: dynWidget rebuilds when the dynamic changes
let _ ← dynWidget someDynamic fun value => do
  emit do pure (someVisual value)
```

The `dynWidget` combinator properly subscribes to the Dynamic's update event and rebuilds the widget subtree whenever the value changes.

### Component Hooks

Like React hooks, these access event streams from context:

```lean
-- Hover state for a named widget
useHover : String → ReactiveM (Dynamic Spider Bool)

-- Click event for a named widget
useClick : String → ReactiveM (Event Spider Unit)

-- Click with position data (for sliders)
useClickData : String → ReactiveM (Event Spider ClickData)

-- Shared elapsed time (use for continuous animations - all widgets share ONE Dynamic)
useElapsedTime : ReactiveM (Dynamic Spider Float)

-- Animation frames with delta time (use for physics, hover delays, NOT continuous animation)
useAnimationFrame : ReactiveM (Event Spider Float)

-- Keyboard events
useKeyboard : ReactiveM (Event Spider KeyData)

-- All clicks (for focus management)
useAllClicks : ReactiveM (Event Spider ClickData)
```

### Container Combinators

Build widget hierarchies declaratively:

```lean
-- Layout containers
column' (gap : Float) (style : BoxStyle) (children : WidgetM α) : WidgetM α
row' (gap : Float) (style : BoxStyle) (children : WidgetM α) : WidgetM α
flexRow' (props : FlexContainer) (style : BoxStyle) (children : WidgetM α) : WidgetM α
flexColumn' (props : FlexContainer) (style : BoxStyle) (children : WidgetM α) : WidgetM α

-- Panel containers
titledPanel' (title : String) (variant : PanelVariant) (theme : Theme) (children : WidgetM α) : WidgetM α
elevatedPanel' (theme : Theme) (padding : Float) (children : WidgetM α) : WidgetM α
outlinedPanel' (theme : Theme) (padding : Float) (children : WidgetM α) : WidgetM α
filledPanel' (theme : Theme) (padding : Float) (children : WidgetM α) : WidgetM α
```

### Available Widgets

All widgets return reactive state (Events, Dynamics) for wiring:

```lean
-- Buttons (returns click event)
button : String → Theme → ButtonVariant → WidgetM (Event Spider Unit)

-- Toggle controls (returns checked state)
checkbox : String → Theme → Bool → WidgetM (Dynamic Spider Bool)
switch : Option String → Theme → Bool → WidgetM (Dynamic Spider Bool)

-- Selection
radioGroup : Array RadioOption → Theme → String → WidgetM (Dynamic Spider String)
dropdown : Array String → Theme → Nat → WidgetM DropdownResult

-- Input
textInput : Theme → String → String → WidgetM TextInputResult
textArea : Theme → String → TextAreaConfig → Font → WidgetM TextAreaResult
slider : Option String → Theme → Float → WidgetM (Dynamic Spider Float)

-- Feedback
progressBar : Theme → Float → ProgressVariant → Option String → Bool → WidgetM Unit
progressBarIndeterminate : Theme → ProgressVariant → Option String → WidgetM Unit

-- Overlays
modal : String → Theme → WidgetM α → WidgetM ModalResult
toastManager : Theme → WidgetM ToastManager

-- Navigation
tabView : Array TabDef → Theme → Nat → WidgetM (Dynamic Spider Nat)

-- Text
heading1' heading2' heading3' bodyText' caption' : String → Theme → WidgetM Unit
```

### Dynamic Rendering

```lean
-- Conditional rendering
when' : Dynamic Spider Bool → WidgetM Unit → WidgetM Unit

-- Rebuild subtree when dynamic changes (like Reflex's dyn)
dynWidget : Dynamic Spider α → (α → WidgetM β) → WidgetM (Dynamic Spider β)
```

Use `dynWidget` to create widgets that update reactively when their input Dynamic changes.

### Cross-Tree Wiring

For wiring between components in different parts of the tree, create trigger events before building the widget tree:

```lean
-- Pre-create shared trigger (outside runWidget)
let (clickTrigger, fireClick) ← newTriggerEvent (t := Spider) (a := Unit)
let clickCount ← foldDyn (fun _ n => n + 1) 0 clickTrigger

let (_, render) ← runWidget do
  -- Display updates reactively when clickCount changes
  let _ ← dynWidget clickCount fun count => do
    caption' s!"Clicks: {count}" theme

  -- Button (fires trigger)
  let click ← button "Click Me" theme .primary
  performEvent_ (← Event.mapM (fun _ => fireClick ()) click)
```

### Example: Complete Reactive Widget

```lean
def clickCounterPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Click Counter" .outlined theme do
    caption' "Button displays its own click count:" theme
    -- Register widget for event handling
    let name ← registerComponentW "counter-button"
    let isHovered ← useHover name
    let onClick ← useClick name
    -- Count clicks using foldDyn
    let clickCount ← foldDyn (fun _ n => n + 1) 0 onClick
    -- Combine dynamics and rebuild button when either changes
    let buttonState ← Dynamic.zipWithM (fun count hovered => (count, hovered)) clickCount isHovered
    let _ ← dynWidget buttonState fun (count, hovered) => do
      let state := { hovered, pressed := false, focused := false }
      let label := if count == 0 then "Click me!" else s!"Clicked {count} times"
      emit do pure (buttonVisual name label theme .primary state)
```

### Example: Dependent Dropdowns

```lean
def dependentDropdownsPanel (theme : Theme) : WidgetM Unit :=
  titledPanel' "Dependent Dropdowns" .outlined theme do
    caption' "Second dropdown depends on first:" theme
    let categories := #["Fruits", "Vegetables", "Dairy"]
    let itemsForCategory (idx : Nat) : Array String :=
      match idx with
      | 0 => #["Apple", "Banana", "Cherry"]
      | 1 => #["Carrot", "Broccoli", "Spinach"]
      | 2 => #["Milk", "Cheese", "Yogurt"]
      | _ => #[]
    row' (gap := 16) (style := {}) do
      column' (gap := 4) (style := {}) do
        caption' "Category:" theme
        let catResult ← dropdown categories theme 0
        -- dynWidget rebuilds second dropdown when category changes
        let _ ← dynWidget catResult.selection fun catIdx =>
          dropdown (itemsForCategory catIdx) theme 0
        pure ()
```

## FFI Notes

### Returning Float Tuples

When returning `Float × Float × Float` from C to Lean, use nested `Prod` structures:

```c
// Float × Float × Float = Prod Float (Prod Float Float)
lean_object* inner = lean_alloc_ctor(0, 2, 0);
lean_ctor_set(inner, 0, lean_box_float(val2));
lean_ctor_set(inner, 1, lean_box_float(val3));

lean_object* outer = lean_alloc_ctor(0, 2, 0);
lean_ctor_set(outer, 0, lean_box_float(val1));
lean_ctor_set(outer, 1, inner);
```

### External Classes

Native handles use `lean_alloc_external` with registered classes:

```c
static lean_external_class* g_font_class = NULL;
// In init: g_font_class = lean_register_external_class(finalizer, NULL);
// Usage: lean_alloc_external(g_font_class, native_ptr);
```

### Struct Layout

When adding Lean `structure`s that cross FFI:
- Structures with only scalar fields use **unboxed-scalar** layout
- Use `lean_alloc_ctor(tag, 0, <bytes>)` and `lean_ctor_set_float/uint16/uint8`
- Check generated C in `.lake/build/ir/` for exact offsets

## Performance Patterns

### FloatBuffer
C-allocated mutable arrays that avoid Lean's copy-on-write:
```lean
let buf ← FloatBuffer.create 10000
buf.setVec5 index x y size rotation alpha
Render.Dynamic.drawSpritesFromBuffer renderer texture buf count size screenWidth screenHeight
```

### Instanced Streaming
Stream large instanced batches without Array allocation:
```lean
let buf ← FloatBuffer.create (count * 8)
Render.Dynamic.drawInstancedAnimated renderer 0 particles buf halfSize t spinSpeed
```

### Instanced Rendering
Draw millions of shapes via GPU instancing (shapeType: 0=rect, 1=triangle, 2=circle):
```lean
FFI.Renderer.drawInstancedShapesBuffer renderer 2 instanceBuffer count a b c d tx ty
  screenWidth screenHeight sizeMode t hueSpeed colorMode
```

## Testing

Run tests with `./test.sh`. Tests are in `Afferent/Tests/`. See `Demos/` for usage examples of all features.

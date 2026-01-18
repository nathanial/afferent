/-
  Canopy Spinner Widget - Reactive Component
-/
import Reactive
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Reactive.Component
import Afferent.Canopy.Widget.Display.Spinner.Core
import Afferent.Canopy.Widget.Display.Spinner.CircleDots
import Afferent.Canopy.Widget.Display.Spinner.Ring
import Afferent.Canopy.Widget.Display.Spinner.BouncingDots
import Afferent.Canopy.Widget.Display.Spinner.Bars
import Afferent.Canopy.Widget.Display.Spinner.DualRing
import Afferent.Canopy.Widget.Display.Spinner.Orbit
import Afferent.Canopy.Widget.Display.Spinner.Pulse
import Afferent.Canopy.Widget.Display.Spinner.Helix
import Afferent.Canopy.Widget.Display.Spinner.Wave
import Afferent.Canopy.Widget.Display.Spinner.Spiral
import Afferent.Canopy.Widget.Display.Spinner.Clock
import Afferent.Canopy.Widget.Display.Spinner.Pendulum
import Afferent.Canopy.Widget.Display.Spinner.Ripple
import Afferent.Canopy.Widget.Display.Spinner.Heartbeat
import Afferent.Canopy.Widget.Display.Spinner.Gears

namespace Afferent.Canopy

open Afferent.Arbor hiding Event

namespace Spinner

/-- Get the appropriate spec for a spinner variant. -/
def variantSpec (variant : SpinnerVariant) (t : Float) (color : Color)
    (dims : Dimensions) : CustomSpec :=
  match variant with
  | .circleDots => circleDotsSpec t color dims
  | .ring => ringSpec t color dims
  | .bouncingDots => bouncingDotsSpec t color dims
  | .bars => barsSpec t color dims
  | .dualRing => dualRingSpec t color dims
  | .orbit => orbitSpec t color dims
  | .pulse => pulseSpec t color dims
  | .helix => helixSpec t color dims
  | .wave => waveSpec t color dims
  | .spiral => spiralSpec t color dims
  | .clock => clockSpec t color dims
  | .pendulum => pendulumSpec t color dims
  | .ripple => rippleSpec t color dims
  | .heartbeat => heartbeatSpec t color dims
  | .gears => gearsSpec t color dims

end Spinner

/-- Build a spinner (WidgetBuilder version).
    - `name`: Widget name for identification
    - `t`: Animation progress (0.0 to 1.0)
    - `config`: Spinner configuration
    - `theme`: Theme for styling
-/
def spinnerVisual (name : String) (t : Float) (config : Spinner.Config)
    (theme : Theme) : WidgetBuilder := do
  let color := Spinner.getColor config theme
  let baseSpec := Spinner.variantSpec config.variant t color config.dims
  let spec := { baseSpec with skipCache := true }

  let wid ← freshId
  let props : Trellis.FlexContainer := { Trellis.FlexContainer.column 0 with alignItems := .center }

  let spinnerWidget ← custom spec {
    minWidth := some config.dims.size
    minHeight := some config.dims.size
  }

  pure (.flex wid (some name) props {} #[spinnerWidget])

/-! ## Reactive Spinner Component (FRP-based) -/

open Reactive Reactive.Host
open Afferent.Canopy.Reactive

/-- Float modulo for animation cycling. -/
private def floatMod (x y : Float) : Float :=
  x - y * (x / y).floor

/-- Create a spinner component using WidgetM.
    Emits an animated spinner that cycles continuously.
    - `theme`: Theme for styling
    - `config`: Spinner configuration (variant, color, speed, dimensions)
-/
def spinner (theme : Theme) (config : Spinner.Config := {}) : WidgetM Unit := do
  let name ← registerComponentW "spinner" (isInteractive := false)

  -- Generate a random time offset so multiple spinners don't animate in sync
  let cycleDuration : Float := 2.0 / config.speed
  let randomSeed ← (IO.rand 0 10000 : IO Nat)
  let timeOffset : Float := randomSeed.toFloat / 10000.0 * cycleDuration

  -- Use shared elapsed time (all widgets share ONE Dynamic, no per-widget foldDyn)
  let elapsedTime ← useElapsedTime

  -- Variants using sin/cos need raw time for seamless animation (they handle any angle).
  -- Other variants use wrapped progress [0,1) for their animation cycles.
  let useRawTime := match config.variant with
    | .ring | .dualRing => true          -- Arc rendering with angles
    | .helix | .wave | .circleDots       -- Shader DSL with sin/cos
    | .pendulum | .orbit | .bouncingDots
    | .ripple | .pulse | .bars => true
    | _ => false
  let _ ← dynWidget elapsedTime fun t => do
    let offsetT := t + timeOffset
    let animTime := if useRawTime then offsetT * config.speed else floatMod offsetT cycleDuration / cycleDuration
    emit do pure (spinnerVisual name animTime config theme)

/-- Convenience function: Create a default ring spinner. -/
def spinnerRing (theme : Theme) (color : Option Color := none)
    (size : Float := 40.0) : WidgetM Unit :=
  spinner theme { variant := .ring, color, dims := { size } }

/-- Convenience function: Create a circle dots spinner. -/
def spinnerCircleDots (theme : Theme) (color : Option Color := none)
    (size : Float := 40.0) : WidgetM Unit :=
  spinner theme { variant := .circleDots, color, dims := { size } }

/-- Convenience function: Create a bouncing dots spinner. -/
def spinnerBouncingDots (theme : Theme) (color : Option Color := none)
    (size : Float := 40.0) : WidgetM Unit :=
  spinner theme { variant := .bouncingDots, color, dims := { size } }

end Afferent.Canopy

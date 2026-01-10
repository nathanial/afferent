/-
  Canopy ProgressBar Widget
  Horizontal progress indicator with determinate and indeterminate modes.
-/
import Reactive
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Reactive.Component

namespace Afferent.Canopy

open Afferent.Arbor hiding Event

/-- Progress bar variant for different visual styles. -/
inductive ProgressVariant where
  | primary
  | secondary
  | success
  | warning
  | error
deriving Repr, BEq, Inhabited

namespace ProgressBar

/-- Dimensions for progress bar rendering. -/
structure Dimensions where
  width : Float := 200.0
  height : Float := 8.0
  cornerRadius : Float := 4.0
deriving Repr, Inhabited

/-- Default progress bar dimensions. -/
def defaultDimensions : Dimensions := {}

/-- Get the fill color for a variant. -/
def variantColor (variant : ProgressVariant) (theme : Theme) : Color :=
  match variant with
  | .primary => theme.primary.background
  | .secondary => theme.secondary.background
  | .success => Color.rgba 0.2 0.8 0.3 1.0  -- Green
  | .warning => Color.rgba 1.0 0.7 0.0 1.0  -- Orange
  | .error => Color.rgba 0.9 0.2 0.2 1.0    -- Red

/-- Custom spec for determinate progress bar.
    `value` is 0.0 to 1.0, representing completion. -/
def determinateSpec (value : Float) (variant : ProgressVariant)
    (theme : Theme) (dims : Dimensions := defaultDimensions) : CustomSpec := {
  measure := fun _ _ => (dims.width, dims.height)
  collect := fun layout =>
    let rect := layout.contentRect
    let cmds : Array RenderCommand := #[]

    -- Clamp value to valid range
    let v := if value < 0.0 then 0.0 else if value > 1.0 then 1.0 else value

    -- Background track
    let trackRect := Arbor.Rect.mk' rect.x rect.y dims.width dims.height
    let trackBg := Color.gray 0.25
    let cmds := cmds.push (.fillRect trackRect trackBg dims.cornerRadius)

    -- Filled portion
    let filledWidth := dims.width * v
    if filledWidth > 0 then
      let filledRect := Arbor.Rect.mk' rect.x rect.y filledWidth dims.height
      let fillColor := variantColor variant theme
      cmds.push (.fillRect filledRect fillColor dims.cornerRadius)
    else
      cmds
  draw := none
}

/-- Custom spec for indeterminate progress bar with animation.
    `animationProgress` is 0.0 to 1.0 for the animation cycle. -/
def indeterminateSpec (animationProgress : Float) (variant : ProgressVariant)
    (theme : Theme) (dims : Dimensions := defaultDimensions) : CustomSpec := {
  measure := fun _ _ => (dims.width, dims.height)
  collect := fun layout =>
    let rect := layout.contentRect
    let cmds : Array RenderCommand := #[]

    -- Background track
    let trackRect := Arbor.Rect.mk' rect.x rect.y dims.width dims.height
    let trackBg := Color.gray 0.25
    let cmds := cmds.push (.fillRect trackRect trackBg dims.cornerRadius)

    -- Animated segment (slides back and forth)
    let segmentWidth := dims.width * 0.3
    -- Use sine wave for smooth back-and-forth motion
    let t := animationProgress * 2.0 * Path.pi
    let normalizedPos := (Float.sin t + 1.0) / 2.0  -- 0.0 to 1.0
    let segmentX := rect.x + normalizedPos * (dims.width - segmentWidth)

    let segmentRect := Arbor.Rect.mk' segmentX rect.y segmentWidth dims.height
    let fillColor := variantColor variant theme
    cmds.push (.fillRect segmentRect fillColor dims.cornerRadius)
  draw := none
}

end ProgressBar

/-- Build a determinate progress bar (WidgetBuilder version).
    - `name`: Widget name for identification
    - `value`: Progress value (0.0 to 1.0)
    - `variant`: Color variant
    - `theme`: Theme for styling
    - `label`: Optional label text
    - `showPercentage`: Whether to show percentage text
-/
def progressBarVisual (name : String) (value : Float)
    (variant : ProgressVariant := .primary) (theme : Theme)
    (label : Option String := none) (showPercentage : Bool := false)
    (dims : ProgressBar.Dimensions := ProgressBar.defaultDimensions) : WidgetBuilder := do
  let progressTrack : WidgetBuilder := do
    custom (ProgressBar.determinateSpec value variant theme dims) {
      minWidth := some dims.width
      minHeight := some dims.height
    }

  let wid ← freshId
  let props : Trellis.FlexContainer := { Trellis.FlexContainer.column 4 with alignItems := .flexStart }

  match label, showPercentage with
  | some text, true =>
    let labelWidget ← text' text theme.font theme.text .left
    let track ← progressTrack
    let pct := (value * 100).toUInt32
    let pctWidget ← text' s!"{pct}%" theme.smallFont theme.textMuted .left
    let trackRow ← do
      let rowId ← freshId
      let rowProps : Trellis.FlexContainer := { Trellis.FlexContainer.row 8 with alignItems := .center }
      pure (.flex rowId none rowProps {} #[track, pctWidget])
    pure (.flex wid (some name) props {} #[labelWidget, trackRow])
  | some text, false =>
    let labelWidget ← text' text theme.font theme.text .left
    let track ← progressTrack
    pure (.flex wid (some name) props {} #[labelWidget, track])
  | none, true =>
    let track ← progressTrack
    let pct := (value * 100).toUInt32
    let pctWidget ← text' s!"{pct}%" theme.smallFont theme.textMuted .left
    let rowProps : Trellis.FlexContainer := { Trellis.FlexContainer.row 8 with alignItems := .center }
    pure (.flex wid (some name) rowProps {} #[track, pctWidget])
  | none, false =>
    let track ← progressTrack
    pure (.flex wid (some name) props {} #[track])

/-- Build an indeterminate progress bar (WidgetBuilder version).
    - `name`: Widget name for identification
    - `animationProgress`: Animation cycle progress (0.0 to 1.0)
    - `variant`: Color variant
    - `theme`: Theme for styling
    - `label`: Optional label text
-/
def progressBarIndeterminateVisual (name : String) (animationProgress : Float)
    (variant : ProgressVariant := .primary) (theme : Theme)
    (label : Option String := none)
    (dims : ProgressBar.Dimensions := ProgressBar.defaultDimensions) : WidgetBuilder := do
  let progressTrack : WidgetBuilder := do
    custom (ProgressBar.indeterminateSpec animationProgress variant theme dims) {
      minWidth := some dims.width
      minHeight := some dims.height
    }

  let wid ← freshId
  let props : Trellis.FlexContainer := { Trellis.FlexContainer.column 4 with alignItems := .flexStart }

  match label with
  | some text =>
    let labelWidget ← text' text theme.font theme.text .left
    let track ← progressTrack
    pure (.flex wid (some name) props {} #[labelWidget, track])
  | none =>
    let track ← progressTrack
    pure (.flex wid (some name) props {} #[track])

/-! ## Reactive ProgressBar Components (FRP-based)

These use WidgetM for declarative composition with automatic animation.
-/

open Reactive Reactive.Host
open Afferent.Canopy.Reactive

/-- Float modulo for animation cycling. -/
private def floatMod (x y : Float) : Float :=
  x - y * (x / y).floor

/-- ProgressBar result - just state access since progress bars are typically display-only. -/
structure ProgressBarResult where
  value : Reactive.Dynamic Spider Float

/-- Create a determinate progress bar component using WidgetM.
    Displays a static progress value.
    - `theme`: Theme for styling
    - `initialValue`: Initial progress value (0.0 to 1.0)
    - `variant`: Color variant
    - `label`: Optional label text
    - `showPercentage`: Whether to show percentage text
-/
def ProgressBar.reactive (theme : Theme) (initialValue : Float := 0.0)
    (variant : ProgressVariant := .primary)
    (label : Option String := none) (showPercentage : Bool := false)
    : WidgetM ProgressBarResult := do
  let name ← registerComponentW "progress-bar" (isInteractive := false)

  -- Create a constant dynamic
  let value ← Dynamic.pureM initialValue

  emit do
    pure (progressBarVisual name initialValue variant theme label showPercentage)

  pure { value }

/-- IndeterminateProgressBar result - includes animation control. -/
structure IndeterminateProgressBarResult where
  animationProgress : Reactive.Dynamic Spider Float

/-- Create an indeterminate progress bar component using WidgetM.
    Emits an animated progress bar that cycles continuously.
    - `theme`: Theme for styling
    - `variant`: Color variant
    - `label`: Optional label text
-/
def ProgressBar.indeterminate (theme : Theme)
    (variant : ProgressVariant := .primary)
    (label : Option String := none)
    : WidgetM IndeterminateProgressBarResult := do
  let name ← registerComponentW "progress-bar-indeterminate" (isInteractive := false)

  -- Subscribe to animation frames for continuous animation
  let animFrame ← useAnimationFrame

  -- Accumulate time for animation (cycle every 2 seconds)
  let cycleDuration : Float := 2.0
  let animationTime ← Reactive.foldDyn (fun dt acc => floatMod (acc + dt) cycleDuration) 0.0 animFrame
  let animationProgress ← Dynamic.mapM (· / cycleDuration) animationTime

  emit do
    let progress ← animationProgress.sample
    pure (progressBarIndeterminateVisual name progress variant theme label)

  pure { animationProgress }

/-- Create a progress bar that updates based on an external event stream.
    Useful for showing download progress, file processing, etc.
    - `theme`: Theme for styling
    - `valueUpdates`: Event stream of progress values
    - `initialValue`: Initial progress value
    - `variant`: Color variant
    - `label`: Optional label text
    - `showPercentage`: Whether to show percentage text
-/
def ProgressBar.withEvents (theme : Theme) (valueUpdates : Reactive.Event Spider Float)
    (initialValue : Float := 0.0)
    (variant : ProgressVariant := .primary)
    (label : Option String := none) (showPercentage : Bool := true)
    : WidgetM ProgressBarResult := do
  let name ← registerComponentW "progress-bar" (isInteractive := false)

  let value ← Reactive.holdDyn initialValue valueUpdates

  emit do
    let v ← value.sample
    pure (progressBarVisual name v variant theme label showPercentage)

  pure { value }

end Afferent.Canopy

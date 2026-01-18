/-
  Canopy Spinner Widget - Core Types and Configuration
-/
import Afferent.Canopy.Core
import Afferent.Canopy.Theme

namespace Afferent.Canopy

open Afferent.Arbor hiding Event

/-- Spinner variant for different visual styles. -/
inductive SpinnerVariant where
  -- Standard spinners
  | circleDots      -- Classic dots arranged in circle, fading sequentially
  | ring            -- Rotating arc segment (macOS/iOS style)
  | bouncingDots    -- Three dots bouncing horizontally
  | bars            -- Vertical bars pulsing in sequence
  | dualRing        -- Two concentric rotating rings (opposite directions)
  -- Creative spinners
  | orbit           -- Dots orbiting a center point at different speeds
  | pulse           -- Expanding/contracting concentric rings
  | helix           -- DNA-like double helix rotating
  | wave            -- Dots following sine wave pattern
  | spiral          -- Drawing spiral that resets
  | clock           -- Clock hands rotating at different speeds
  | pendulum        -- Swinging pendulum with trail
  | ripple          -- Concentric circles expanding outward
  | heartbeat       -- Pulsing heart-like shape with ECG timing
  | gears           -- Two interlocking gears rotating
deriving Repr, BEq, Inhabited

namespace Spinner

/-- Dimensions for spinner rendering. -/
structure Dimensions where
  size : Float := 40.0        -- Overall size (width = height)
  strokeWidth : Float := 3.0  -- Line thickness for stroked elements
deriving Repr, Inhabited

/-- Configuration for spinner widget. -/
structure Config where
  variant : SpinnerVariant := .ring
  color : Option Color := none  -- Uses theme.primary.background if none
  speed : Float := 1.0          -- Animation speed multiplier (1.0 = normal)
  dims : Dimensions := {}
deriving Repr, Inhabited

/-- Default spinner dimensions. -/
def defaultDimensions : Dimensions := {}

/-- Get the color for rendering, defaulting to theme primary. -/
def getColor (config : Config) (theme : Theme) : Color :=
  config.color.getD theme.primary.background

end Spinner

end Afferent.Canopy

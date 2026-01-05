-- This module serves as the root of the `Afferent` library.
-- Import modules here that should be built as part of the library.

-- Embedded shader sources
import Afferent.Shaders

-- Core types
import Afferent.Core.Types
import Afferent.Core.Path
import Afferent.Core.Transform
import Afferent.Core.Paint

-- Rendering
import Afferent.Render.Tessellation
import Afferent.Render.Earcut
import Afferent.Render.Dynamic
import Afferent.Render.Mesh
import Afferent.Render.FPSCamera

-- Linear algebra (from Linalg library)
import Linalg

-- Canvas API
import Afferent.Canvas.State
import Afferent.Canvas.Context

-- Text
import Afferent.Text.Font
import Afferent.Text.Measurer

-- FFI (modular: Types, Window, Renderer, Text, FloatBuffer, Texture)
import Afferent.FFI

-- Layout
import Afferent.Layout

-- Widget system
import Afferent.Widget

-- Arbor / Canopy (now bundled under Afferent)
import Afferent.Arbor
import Afferent.Canopy

-- App runtime helpers
import Afferent.App.UIRunner

-- Re-export useful Linalg types
namespace Afferent
export Linalg (Vec2 Vec3 Vec4 Mat4 Quat)
-- Note: Linalg.Easing is available as a namespace (e.g., Linalg.Easing.quadInOut)
end Afferent

-- This module serves as the root of the `Afferent` library.
-- Import modules here that should be built as part of the library.

-- Core types
import Afferent.Core.Types
import Afferent.Core.Path
import Afferent.Core.Transform
import Afferent.Core.Paint

-- Rendering
import Afferent.Render.Tessellation
import Afferent.Render.Dynamic
import Afferent.Render.Matrix4
import Afferent.Render.Mesh

-- Canvas API
import Afferent.Canvas.State
import Afferent.Canvas.Context

-- Text
import Afferent.Text.Font

-- FFI (modular: Types, Window, Renderer, Text, FloatBuffer, Texture)
import Afferent.FFI

-- Layout
import Afferent.Layout

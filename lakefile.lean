import Lake
open Lake DSL
open System (FilePath)

package afferent where
  version := v!"0.1.0"

require crucible from git "https://github.com/nathanial/crucible" @ "v0.0.9"
require shader from git "https://github.com/nathanial/shader" @ "v0.1.0"
require tincture from git "https://github.com/nathanial/tincture" @ "v0.0.1"
require trellis from git "https://github.com/nathanial/trellis" @ "v0.0.14"
require assimptor from git "https://github.com/nathanial/assimptor" @ "v0.0.2"
require staple from git "https://github.com/nathanial/staple" @ "v0.0.2"
require linalg from git "https://github.com/nathanial/linalg" @ "v0.0.3"
require reactive from git "https://github.com/nathanial/reactive" @ "v0.2.2"
require raster from git "https://github.com/nathanial/raster" @ "v0.0.5"
require oracle from git "https://github.com/nathanial/oracle" @ "v0.2.0"

-- Common link arguments for all executables
-- Includes both Homebrew paths for Apple Silicon (/opt/homebrew) and Intel (/usr/local)
def commonLinkArgs : Array String := #[
  "-framework", "Metal",
  "-framework", "Cocoa",
  "-framework", "QuartzCore",
  "-framework", "Foundation",
  "-framework", "Security",
  "-framework", "SystemConfiguration",
  "-lobjc",
  "-L/opt/homebrew/lib",    -- Apple Silicon Homebrew
  "-L/usr/local/lib",        -- Intel Homebrew fallback
  "-lfreetype",
  "-lassimp",
  "-lz",
  "-lc++",
  "-lcurl"                   -- Required by Wisp (HTTP client via Oracle)
]

-- Native library compilation
@[default_target]
lean_lib Afferent where
  roots := #[`Afferent]

-- Test executable
@[test_driver]
lean_exe afferent_tests where
  root := `AfferentTests
  moreLinkArgs := commonLinkArgs

-- Native code targets
-- Metal-specific native code (macOS only)
target window_o pkg : FilePath := do
  let oFile := pkg.buildDir / "native" / "window.o"
  let srcFile := pkg.dir / "native" / "src" / "metal" / "window.m"
  let includeDir := pkg.dir / "native" / "include"
  buildO oFile (← inputTextFile srcFile) #[
    "-I", includeDir.toString,
    "-fobjc-arc",
    "-fPIC",
    "-O2"
  ] #[] "clang"

target metal_render_o pkg : FilePath := do
  let oFile := pkg.buildDir / "native" / "metal_render.o"
  let srcFile := pkg.dir / "native" / "src" / "metal" / "render.m"
  let includeDir := pkg.dir / "native" / "include"
  buildO oFile (← inputTextFile srcFile) #[
    "-I", includeDir.toString,
    "-fobjc-arc",
    "-fPIC",
    "-O2"
  ] #[] "clang"

target draw_2d_o pkg : FilePath := do
  let oFile := pkg.buildDir / "native" / "draw_2d.o"
  let srcFile := pkg.dir / "native" / "src" / "metal" / "draw_2d.m"
  let includeDir := pkg.dir / "native" / "include"
  buildO oFile (← inputTextFile srcFile) #[
    "-I", includeDir.toString,
    "-fobjc-arc",
    "-fPIC",
    "-O2"
  ] #[] "clang"

target fragment_compiler_o pkg : FilePath := do
  let oFile := pkg.buildDir / "native" / "fragment_compiler.o"
  let srcFile := pkg.dir / "native" / "src" / "metal" / "fragment_compiler.m"
  let includeDir := pkg.dir / "native" / "include"
  let metalDir := pkg.dir / "native" / "src" / "metal"
  buildO oFile (← inputTextFile srcFile) #[
    "-I", includeDir.toString,
    "-I", metalDir.toString,
    "-fobjc-arc",
    "-fPIC",
    "-O2"
  ] #[] "clang"

-- Cross-platform native code
target text_render_o pkg : FilePath := do
  let oFile := pkg.buildDir / "native" / "text_render.o"
  let srcFile := pkg.dir / "native" / "src" / "common" / "text_render.c"
  let includeDir := pkg.dir / "native" / "include"
  buildO oFile (← inputTextFile srcFile) #[
    "-I", includeDir.toString,
    "-I/opt/homebrew/include/freetype2",
    "-I/usr/local/include/freetype2",
    "-fPIC",
    "-O2"
  ] #[] "cc"

target lean_bridge_init_o pkg : FilePath := do
  let oFile := pkg.buildDir / "native" / "lean_bridge_init.o"
  let srcFile := pkg.dir / "native" / "src" / "lean_bridge" / "init.c"
  let includeDir := pkg.dir / "native" / "include"
  let bridgeDir := pkg.dir / "native" / "src" / "lean_bridge"
  let leanIncludeDir ← getLeanIncludeDir
  buildO oFile (← inputTextFile srcFile) #[
    "-I", leanIncludeDir.toString,
    "-I", includeDir.toString,
    "-I", bridgeDir.toString,
    "-fPIC",
    "-O2"
  ] #[] "cc"

target lean_bridge_window_o pkg : FilePath := do
  let oFile := pkg.buildDir / "native" / "lean_bridge_window.o"
  let srcFile := pkg.dir / "native" / "src" / "lean_bridge" / "window.c"
  let includeDir := pkg.dir / "native" / "include"
  let bridgeDir := pkg.dir / "native" / "src" / "lean_bridge"
  let leanIncludeDir ← getLeanIncludeDir
  buildO oFile (← inputTextFile srcFile) #[
    "-I", leanIncludeDir.toString,
    "-I", includeDir.toString,
    "-I", bridgeDir.toString,
    "-fPIC",
    "-O2"
  ] #[] "cc"

target lean_bridge_renderer_o pkg : FilePath := do
  let oFile := pkg.buildDir / "native" / "lean_bridge_renderer.o"
  let srcFile := pkg.dir / "native" / "src" / "lean_bridge" / "renderer.c"
  let includeDir := pkg.dir / "native" / "include"
  let bridgeDir := pkg.dir / "native" / "src" / "lean_bridge"
  let leanIncludeDir ← getLeanIncludeDir
  buildO oFile (← inputTextFile srcFile) #[
    "-I", leanIncludeDir.toString,
    "-I", includeDir.toString,
    "-I", bridgeDir.toString,
    "-fPIC",
    "-O2"
  ] #[] "cc"

target lean_bridge_buffers_o pkg : FilePath := do
  let oFile := pkg.buildDir / "native" / "lean_bridge_buffers.o"
  let srcFile := pkg.dir / "native" / "src" / "lean_bridge" / "buffers.c"
  let includeDir := pkg.dir / "native" / "include"
  let bridgeDir := pkg.dir / "native" / "src" / "lean_bridge"
  let leanIncludeDir ← getLeanIncludeDir
  buildO oFile (← inputTextFile srcFile) #[
    "-I", leanIncludeDir.toString,
    "-I", includeDir.toString,
    "-I", bridgeDir.toString,
    "-fPIC",
    "-O2"
  ] #[] "cc"

target lean_bridge_text_o pkg : FilePath := do
  let oFile := pkg.buildDir / "native" / "lean_bridge_text.o"
  let srcFile := pkg.dir / "native" / "src" / "lean_bridge" / "text.c"
  let includeDir := pkg.dir / "native" / "include"
  let bridgeDir := pkg.dir / "native" / "src" / "lean_bridge"
  let leanIncludeDir ← getLeanIncludeDir
  buildO oFile (← inputTextFile srcFile) #[
    "-I", leanIncludeDir.toString,
    "-I", includeDir.toString,
    "-I", bridgeDir.toString,
    "-fPIC",
    "-O2"
  ] #[] "cc"

target lean_bridge_float_buffer_o pkg : FilePath := do
  let oFile := pkg.buildDir / "native" / "lean_bridge_float_buffer.o"
  let srcFile := pkg.dir / "native" / "src" / "lean_bridge" / "float_buffer.c"
  let includeDir := pkg.dir / "native" / "include"
  let bridgeDir := pkg.dir / "native" / "src" / "lean_bridge"
  let leanIncludeDir ← getLeanIncludeDir
  buildO oFile (← inputTextFile srcFile) #[
    "-I", leanIncludeDir.toString,
    "-I", includeDir.toString,
    "-I", bridgeDir.toString,
    "-fPIC",
    "-O2"
  ] #[] "cc"

target lean_bridge_texture_o pkg : FilePath := do
  let oFile := pkg.buildDir / "native" / "lean_bridge_texture.o"
  let srcFile := pkg.dir / "native" / "src" / "lean_bridge" / "texture.c"
  let includeDir := pkg.dir / "native" / "include"
  let bridgeDir := pkg.dir / "native" / "src" / "lean_bridge"
  let leanIncludeDir ← getLeanIncludeDir
  buildO oFile (← inputTextFile srcFile) #[
    "-I", leanIncludeDir.toString,
    "-I", includeDir.toString,
    "-I", bridgeDir.toString,
    "-fPIC",
    "-O2"
  ] #[] "cc"

target lean_bridge_mesh_o pkg : FilePath := do
  let oFile := pkg.buildDir / "native" / "lean_bridge_mesh.o"
  let srcFile := pkg.dir / "native" / "src" / "lean_bridge" / "mesh.c"
  let includeDir := pkg.dir / "native" / "include"
  let bridgeDir := pkg.dir / "native" / "src" / "lean_bridge"
  let leanIncludeDir ← getLeanIncludeDir
  buildO oFile (← inputTextFile srcFile) #[
    "-I", leanIncludeDir.toString,
    "-I", includeDir.toString,
    "-I", bridgeDir.toString,
    "-fPIC",
    "-O2"
  ] #[] "cc"

target lean_bridge_batch_o pkg : FilePath := do
  let oFile := pkg.buildDir / "native" / "lean_bridge_batch.o"
  let srcFile := pkg.dir / "native" / "src" / "lean_bridge" / "batch.c"
  let includeDir := pkg.dir / "native" / "include"
  let bridgeDir := pkg.dir / "native" / "src" / "lean_bridge"
  let leanIncludeDir ← getLeanIncludeDir
  buildO oFile (← inputTextFile srcFile) #[
    "-I", leanIncludeDir.toString,
    "-I", includeDir.toString,
    "-I", bridgeDir.toString,
    "-fPIC",
    "-O2"
  ] #[] "cc"

target lean_bridge_fragment_o pkg : FilePath := do
  let oFile := pkg.buildDir / "native" / "lean_bridge_fragment.o"
  let srcFile := pkg.dir / "native" / "src" / "lean_bridge" / "fragment.m"
  let includeDir := pkg.dir / "native" / "include"
  let bridgeDir := pkg.dir / "native" / "src" / "lean_bridge"
  let metalDir := pkg.dir / "native" / "src" / "metal"
  let leanIncludeDir ← getLeanIncludeDir
  buildO oFile (← inputTextFile srcFile) #[
    "-I", leanIncludeDir.toString,
    "-I", includeDir.toString,
    "-I", bridgeDir.toString,
    "-I", metalDir.toString,
    "-fPIC",
    "-O2",
    "-fobjc-arc"
  ] #[] "clang"

target float_buffer_o pkg : FilePath := do
  let oFile := pkg.buildDir / "native" / "float_buffer.o"
  let srcFile := pkg.dir / "native" / "src" / "common" / "float_buffer.c"
  let includeDir := pkg.dir / "native" / "include"
  buildO oFile (← inputTextFile srcFile) #[
    "-I", includeDir.toString,
    "-fPIC",
    "-O2"
  ] #[] "cc"

target texture_o pkg : FilePath := do
  let oFile := pkg.buildDir / "native" / "texture.o"
  let srcFile := pkg.dir / "native" / "src" / "texture.c"
  let includeDir := pkg.dir / "native" / "include"
  buildO oFile (← inputTextFile srcFile) #[
    "-I", includeDir.toString,
    "-fPIC",
    "-O2"
  ] #[] "cc"

extern_lib libafferent_native pkg := do
  let name := nameToStaticLib "afferent_native"
  let windowO ← window_o.fetch
  let metalO ← metal_render_o.fetch
  let draw2dO ← draw_2d_o.fetch
  let fragmentCompilerO ← fragment_compiler_o.fetch
  let textO ← text_render_o.fetch
  let bridgeInitO ← lean_bridge_init_o.fetch
  let bridgeWindowO ← lean_bridge_window_o.fetch
  let bridgeRendererO ← lean_bridge_renderer_o.fetch
  let bridgeBuffersO ← lean_bridge_buffers_o.fetch
  let bridgeTextO ← lean_bridge_text_o.fetch
  let bridgeFloatBufferO ← lean_bridge_float_buffer_o.fetch
  let bridgeTextureO ← lean_bridge_texture_o.fetch
  let bridgeMeshO ← lean_bridge_mesh_o.fetch
  let bridgeBatchO ← lean_bridge_batch_o.fetch
  let bridgeFragmentO ← lean_bridge_fragment_o.fetch
  let floatBufferO ← float_buffer_o.fetch
  let textureO ← texture_o.fetch
  buildStaticLib (pkg.staticLibDir / name) #[
    windowO,
    metalO,
    draw2dO,
    fragmentCompilerO,
    textO,
    bridgeInitO,
    bridgeWindowO,
    bridgeRendererO,
    bridgeBuffersO,
    bridgeTextO,
    bridgeFloatBufferO,
    bridgeTextureO,
    bridgeMeshO,
    bridgeBatchO,
    bridgeFragmentO,
    floatBufferO,
    textureO
  ]

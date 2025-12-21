import Lake
open Lake DSL
open System (FilePath)

package afferent where
  version := v!"0.1.0"

require collimator from git "https://github.com/nathanial/collimator" @ "master"
require crucible from git "https://github.com/nathanial/crucible" @ "master"
require wisp from git "https://github.com/nathanial/wisp" @ "master"
require cellar from git "https://github.com/nathanial/cellar" @ "master"
require tincture from git "https://github.com/nathanial/tincture" @ "master"
require trellis from ".." / "trellis"
require arbor from ".." / "arbor"

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
  "-L/opt/homebrew/opt/openssl@3/lib", -- Homebrew OpenSSL (keg-only)
  "-L/usr/local/opt/openssl@3/lib",    -- Intel Homebrew OpenSSL (keg-only)
  "-lssl",
  "-lcrypto",
  "-lfreetype",
  "-Lthird_party/assimp/build/lib",
  "-lassimp",
  "-lz",
  "-lcurl",  -- Required by wisp
  "-lc++"
]

-- Native library compilation
@[default_target]
lean_lib Afferent where
  roots := #[`Afferent]

-- Demo library
lean_lib Demos where
  roots := #[`Demos]

lean_exe afferent where
  root := `Main
  -- Link against Metal and Cocoa frameworks on macOS
  -- NOTE: Build with LEAN_CC=/usr/bin/clang to use system linker
  moreLinkArgs := commonLinkArgs

-- Example executable
lean_exe hello_triangle where
  root := `Examples.HelloTriangle
  moreLinkArgs := commonLinkArgs

-- 3D Spinning Cubes demo
lean_exe spinning_cubes where
  root := `Examples.SpinningCubes
  moreLinkArgs := commonLinkArgs

-- Headless map tile fetch/decode diagnostic
lean_exe map_tile_fetch_test where
  root := `Examples.MapTileFetchTest
  moreLinkArgs := commonLinkArgs

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

target lean_bridge_o pkg : FilePath := do
  let oFile := pkg.buildDir / "native" / "lean_bridge.o"
  let srcFile := pkg.dir / "native" / "src" / "lean_bridge.c"
  let includeDir := pkg.dir / "native" / "include"
  let leanIncludeDir ← getLeanIncludeDir
  buildO oFile (← inputTextFile srcFile) #[
    "-I", leanIncludeDir.toString,
    "-I", includeDir.toString,
    "-fPIC",
    "-O2"
  ] #[] "cc"

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
  let srcDir := pkg.dir / "native" / "src"
  buildO oFile (← inputTextFile srcFile) #[
    "-I", includeDir.toString,
    "-I", srcDir.toString,  -- For stb_image.h
    "-fPIC",
    "-O2"
  ] #[] "cc"

-- Assimp loader (C++ code for 3D model loading)
target assimp_loader_o pkg : FilePath := do
  let oFile := pkg.buildDir / "native" / "assimp_loader.o"
  let srcFile := pkg.dir / "native" / "src" / "common" / "assimp_loader.cpp"
  let includeDir := pkg.dir / "native" / "include"
  let assimpIncludeDir := pkg.dir / "third_party" / "assimp" / "include"
  let assimpBuildIncludeDir := pkg.dir / "third_party" / "assimp" / "build" / "include"
  buildO oFile (← inputTextFile srcFile) #[
    "-I", includeDir.toString,
    "-I", assimpIncludeDir.toString,
    "-I", assimpBuildIncludeDir.toString,
    "-std=c++17",
    "-fPIC",
    "-O2"
  ] #[] "clang++"

extern_lib libafferent_native pkg := do
  let name := nameToStaticLib "afferent_native"
  let windowO ← window_o.fetch
  let metalO ← metal_render_o.fetch
  let textO ← text_render_o.fetch
  let bridgeO ← lean_bridge_o.fetch
  let floatBufferO ← float_buffer_o.fetch
  let textureO ← texture_o.fetch
  let assimpLoaderO ← assimp_loader_o.fetch
  buildStaticLib (pkg.staticLibDir / name) #[windowO, metalO, textO, bridgeO, floatBufferO, textureO, assimpLoaderO]

import Lake
open Lake DSL
open System (FilePath)

package afferent where
  version := v!"0.1.0"

require collimator from git "https://github.com/nathanial/collimator" @ "master"

-- Native library compilation
@[default_target]
lean_lib Afferent where
  roots := #[`Afferent]

lean_exe afferent where
  root := `Main
  -- Link against Metal and Cocoa frameworks on macOS
  -- NOTE: Build with LEAN_CC=/usr/bin/clang to use system linker
  moreLinkArgs := #[
    "-L/Users/nathanialhartman/.elan/toolchains/leanprover--lean4---v4.25.2/lib",
    "-L/Users/nathanialhartman/.elan/toolchains/leanprover--lean4---v4.25.2/lib/libc",
    "-framework", "Metal",
    "-framework", "Cocoa",
    "-framework", "QuartzCore",
    "-framework", "Foundation",
    "-lobjc",
    "-L/opt/homebrew/lib",
    "-lfreetype"
  ]

-- Example executable
lean_exe hello_triangle where
  root := `Examples.HelloTriangle
  moreLinkArgs := #[
    "-L/Users/nathanialhartman/.elan/toolchains/leanprover--lean4---v4.25.2/lib",
    "-L/Users/nathanialhartman/.elan/toolchains/leanprover--lean4---v4.25.2/lib/libc",
    "-framework", "Metal",
    "-framework", "Cocoa",
    "-framework", "QuartzCore",
    "-framework", "Foundation",
    "-lobjc",
    "-L/opt/homebrew/lib",
    "-lfreetype"
  ]

-- Test executable
lean_exe afferent_tests where
  root := `AfferentTests
  moreLinkArgs := #["-L/opt/homebrew/lib"]

-- Native code targets
target window_o pkg : FilePath := do
  let oFile := pkg.buildDir / "native" / "window.o"
  let srcFile := pkg.dir / "native" / "src" / "window.m"
  let includeDir := pkg.dir / "native" / "include"
  buildO oFile (← inputTextFile srcFile) #[
    "-I", includeDir.toString,
    "-fobjc-arc",
    "-fPIC",
    "-O2"
  ] #[] "clang"

target metal_render_o pkg : FilePath := do
  let oFile := pkg.buildDir / "native" / "metal_render.o"
  let srcFile := pkg.dir / "native" / "src" / "metal_render.m"
  let includeDir := pkg.dir / "native" / "include"
  buildO oFile (← inputTextFile srcFile) #[
    "-I", includeDir.toString,
    "-fobjc-arc",
    "-fPIC",
    "-O2"
  ] #[] "clang"

target text_render_o pkg : FilePath := do
  let oFile := pkg.buildDir / "native" / "text_render.o"
  let srcFile := pkg.dir / "native" / "src" / "text_render.c"
  let includeDir := pkg.dir / "native" / "include"
  buildO oFile (← inputTextFile srcFile) #[
    "-I", includeDir.toString,
    "-I/opt/homebrew/include/freetype2",
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
  let srcFile := pkg.dir / "native" / "src" / "float_buffer.c"
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

extern_lib libafferent_native pkg := do
  let name := nameToStaticLib "afferent_native"
  let windowO ← window_o.fetch
  let metalO ← metal_render_o.fetch
  let textO ← text_render_o.fetch
  let bridgeO ← lean_bridge_o.fetch
  let floatBufferO ← float_buffer_o.fetch
  let textureO ← texture_o.fetch
  buildStaticLib (pkg.staticLibDir / name) #[windowO, metalO, textO, bridgeO, floatBufferO, textureO]

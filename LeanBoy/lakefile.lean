import Lake
open Lake DSL

package "LeanBoy" where
  version := v!"0.1.0"

require batteries from git
  "https://github.com/leanprover-community/batteries" @ "v4.24.0"

lean_lib LeanBoy

@[default_target]
lean_exe leanboy where
  root := `Main
  moreLinkArgs := #["-L/opt/homebrew/lib", "-lSDL2"]

extern_lib sdlShim pkg := do
  let cFile := pkg.dir / "ffi" / "sdl_frontend.c"
  let oFile := pkg.buildDir / "ffi" / "sdl_frontend.o"
  let aFile := pkg.buildDir / "ffi" / "libsdlShim.a"
  let cJob ← inputTextFile cFile
  let lean ← getLeanInstall
  let oJob ← buildO oFile cJob
    #[s!"-I{lean.includeDir}", "-I/opt/homebrew/include", "-I/opt/homebrew/include/SDL2"]
  buildStaticLib aFile #[oJob]

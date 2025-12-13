/-
  Afferent FFI Initialization
  Module initialization that registers external classes.
-/

namespace Afferent.FFI

-- Module initialization (registers external classes for all handle types)
@[extern "afferent_initialize"]
opaque init : IO Unit

end Afferent.FFI

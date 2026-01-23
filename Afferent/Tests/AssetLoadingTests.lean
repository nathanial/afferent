/-
  Afferent Asset Loading Tests
  Validates Assimp-based model importing invariants.
-/
import Afferent.Tests.Framework
import Assimptor

namespace Afferent.Tests.AssetLoadingTests

open Crucible
open Afferent.Tests

private def validateSubmeshRanges (indicesSize : Nat) (textureCount : Nat)
    (submeshes : Array Assimptor.SubMesh) : IO Unit := do
  for sm in submeshes do
    let off : Nat := sm.indexOffset.toNat
    let cnt : Nat := sm.indexCount.toNat
    ensure (off <= indicesSize) s!"submesh indexOffset out of range: {off} > {indicesSize}"
    ensure (off + cnt <= indicesSize) s!"submesh range out of range: {off}+{cnt} > {indicesSize}"
    -- Triangulation is requested in the importer.
    ensure (cnt % 3 == 0) s!"submesh indexCount not multiple of 3: {cnt}"
    let isNoTexture : Bool := sm.textureIndex.toNat == UInt32.size - 1
    if !isNoTexture then
      ensure (sm.textureIndex.toNat < textureCount)
        s!"submesh textureIndex out of range: {sm.textureIndex} (textures={textureCount})"

testSuite "Asset Loading Tests"

test "loadAsset rejects missing file" := do
  let ok ←
    try
      let _ ← Assimptor.loadAsset "assets/does-not-exist.fbx" "assets"
      pure false
    catch _ =>
      pure true
  ensure ok "expected loadAsset to throw on missing file"

end Afferent.Tests.AssetLoadingTests

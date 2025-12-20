/-
  Afferent Asset Loading Tests
  Validates Assimp-based model importing invariants.
-/
import Afferent.Tests.Framework
import Afferent.FFI.Asset
import Afferent.FFI.Init
import Demos.Seascape

namespace Afferent.Tests.AssetLoadingTests

open Crucible
open Afferent.Tests

private def validateSubmeshRanges (indicesSize : Nat) (textureCount : Nat)
    (submeshes : Array Afferent.FFI.SubMesh) : IO Unit := do
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
      let _ ← Afferent.FFI.loadAsset "assets/does-not-exist.fbx" "assets"
      pure false
    catch _ =>
      pure true
  ensure ok "expected loadAsset to throw on missing file"

test "Demos.loadFrigate caches without crashing" := do
  -- This exercises the Seascape caching path (IO.Ref.set with a cached Texture).
  Afferent.FFI.init
  let _ ← Demos.loadFrigate
  let _ ← Demos.loadFrigate

test "loadAsset loads fictional frigate" := do
  let asset ← Afferent.FFI.loadAsset
    "assets/fictional-frigate/source/frigateUn1.fbx"
    "assets/fictional-frigate/textures"

  ensure (asset.vertices.size > 0) "expected non-empty vertex array"
  ensure (asset.indices.size > 0) "expected non-empty index array"
  ensure (asset.subMeshes.size > 0) "expected at least one submesh"
  ensure (asset.vertices.size % 12 == 0) s!"vertex float count not multiple of 12: {asset.vertices.size}"
  ensure (asset.indices.size % 3 == 0) s!"index count not multiple of 3: {asset.indices.size}"

  validateSubmeshRanges asset.indices.size asset.texturePaths.size asset.subMeshes

#generate_tests

end Afferent.Tests.AssetLoadingTests

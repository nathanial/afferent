/-
  Map Tile Coordinates and Web Mercator Projection
  Ported from heavenly-host to Afferent
-/
namespace Afferent.Map

/-- Pi constant -/
def pi : Float := 3.14159265358979323846

/-- Convert Int to Float -/
def intToFloat (n : Int) : Float := Float.ofInt n

/-- Convert Nat to Int -/
def natToInt (n : Nat) : Int := n

/-- Max of two integers -/
def intMax (a b : Int) : Int := if a > b then a else b

/-- Min of two integers -/
def intMin (a b : Int) : Int := if a < b then a else b

/-- Tile coordinates (x, y at zoom level z) -/
structure TileCoord where
  x : Int
  y : Int
  z : Int  -- zoom level (0-19 for OSM)
  deriving Repr, BEq, Hashable, Inhabited

/-- Geographic coordinates -/
structure LatLon where
  lat : Float  -- -90 to 90
  lon : Float  -- -180 to 180
  deriving Repr, BEq, Inhabited

/-- Convert latitude/longitude to tile coordinates at given zoom level.
    Uses Web Mercator projection (EPSG:3857). -/
def latLonToTile (pos : LatLon) (zoom : Int) : TileCoord :=
  let n := Float.pow 2.0 (intToFloat zoom)
  let x := natToInt ((pos.lon + 180.0) / 360.0 * n).floor.toUInt64.toNat
  let latRad := pos.lat * pi / 180.0
  let y := natToInt ((1.0 - Float.log (Float.tan latRad + 1.0 / Float.cos latRad) / pi) / 2.0 * n).floor.toUInt64.toNat
  { x := x, y := y, z := zoom }

/-- Convert tile coordinates to latitude/longitude (northwest corner of tile). -/
def tileToLatLon (tile : TileCoord) : LatLon :=
  let n := Float.pow 2.0 (intToFloat tile.z)
  let lon := (intToFloat tile.x) / n * 360.0 - 180.0
  let latRad := Float.atan (Float.sinh (pi * (1.0 - 2.0 * (intToFloat tile.y) / n)))
  let lat := latRad * 180.0 / pi
  { lat := lat, lon := lon }

/-- Generate CartoDB Dark @2x tile URL (512px retina tiles).
    Uses subdomains a-d for load balancing. -/
def tileUrl (tile : TileCoord) : String :=
  -- Rotate through subdomains based on tile coordinates
  let subdomains := #["a", "b", "c", "d"]
  let idx := ((tile.x.toNat + tile.y.toNat) % 4)
  let subdomain := subdomains[idx]!
  s!"https://{subdomain}.basemaps.cartocdn.com/dark_all/{tile.z}/{tile.x}/{tile.y}@2x.png"

/-- Number of tiles at a given zoom level (per axis). -/
def tilesAtZoom (zoom : Int) : Int :=
  natToInt (Float.pow 2.0 (intToFloat zoom)).toUInt64.toNat

/-- Clamp zoom level to valid range (0-19). -/
def clampZoom (z : Int) : Int :=
  intMin 19 (intMax 0 z)

namespace TileCoord

/-- Get parent tile at zoom z-1 (integer division gives correct quadrant) -/
def parentTile (coord : TileCoord) : TileCoord :=
  { x := coord.x / 2, y := coord.y / 2, z := coord.z - 1 }

/-- Get the 4 child tiles at zoom z+1 that this tile covers -/
def childTiles (coord : TileCoord) : Array TileCoord :=
  let x2 := coord.x * 2
  let y2 := coord.y * 2
  #[ { x := x2,     y := y2,     z := coord.z + 1 }
   , { x := x2 + 1, y := y2,     z := coord.z + 1 }
   , { x := x2,     y := y2 + 1, z := coord.z + 1 }
   , { x := x2 + 1, y := y2 + 1, z := coord.z + 1 } ]

end TileCoord

end Afferent.Map

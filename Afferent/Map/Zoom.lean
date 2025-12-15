/-
  Zoom-to-Point (Zoom to Cursor)

  This module provides pure functions for zoom-to-point operations.
  When zooming, the geographic point under the cursor stays fixed.

  The key formula: newCenterTile = oldCenterTile * k + cursorOffset * (k - 1)
  where k = 2^(newZoom - oldZoom) is the zoom ratio.

  Ported from heavenly-host to Afferent
-/

import Afferent.Map.Viewport

namespace Afferent.Map.Zoom

open Afferent.Map

/-- Max of two floats -/
def floatMax (a b : Float) : Float := if a > b then a else b

/-- Min of two floats -/
def floatMin (a b : Float) : Float := if a < b then a else b

/-- Clamp float to range -/
def floatClamp (value min max : Float) : Float :=
  floatMin max (floatMax min value)

/-- Convert screen coordinates to fractional tile position -/
def screenToTile (vp : MapViewport) (sx sy : Float) : (Float × Float) :=
  let (centerTileX, centerTileY) := vp.centerTilePos
  let cx := (intToFloat vp.screenWidth) / 2.0
  let cy := (intToFloat vp.screenHeight) / 2.0
  (centerTileX + (sx - cx) / (intToFloat vp.tileSize),
   centerTileY + (sy - cy) / (intToFloat vp.tileSize))

/-- Convert fractional tile position to geographic coordinates (inverse Mercator).
    Returns (latitude, longitude). -/
def tileToGeo (tileX tileY : Float) (zoom : Int) : (Float × Float) :=
  let n := Float.pow 2.0 (intToFloat zoom)
  let lon := tileX / n * 360.0 - 180.0
  let latRad := Float.atan (Float.sinh (pi * (1.0 - 2.0 * tileY / n)))
  let lat := latRad * 180.0 / pi
  (lat, lon)

/-- Convert screen coordinates to geographic coordinates.
    Returns (latitude, longitude). -/
def screenToGeo (vp : MapViewport) (sx sy : Float) : (Float × Float) :=
  let (tileX, tileY) := screenToTile vp sx sy
  tileToGeo tileX tileY vp.zoom

/-- Compute new center after zooming at cursor position.
    Returns (newLat, newLon).

    The formula ensures that the cursor's screen position maps to the same
    geographic point before and after the zoom (the fixed point property). -/
def zoomToPointCenter (vp : MapViewport) (cursorX cursorY : Float)
                       (newZoom : Int) : (Float × Float) :=
  let oldN := Float.pow 2.0 (intToFloat vp.zoom)
  let newN := Float.pow 2.0 (intToFloat newZoom)
  let k := newN / oldN  -- zoom ratio

  -- Screen center
  let cx := (intToFloat vp.screenWidth) / 2.0
  let cy := (intToFloat vp.screenHeight) / 2.0

  -- Offset from screen center to cursor in tile units (at old zoom)
  let dxTile := (cursorX - cx) / (intToFloat vp.tileSize)
  let dyTile := (cursorY - cy) / (intToFloat vp.tileSize)

  -- Old center in tile coords
  let (oldCenterTileX, oldCenterTileY) := vp.centerTilePos

  -- New center in tile coords (at NEW zoom level)
  -- This formula ensures: cursorTileX * k - newCenterTileX = dxTile
  -- i.e., the cursor's screen offset from center stays the same
  let newCenterTileX := oldCenterTileX * k + dxTile * (k - 1.0)
  let newCenterTileY := oldCenterTileY * k + dyTile * (k - 1.0)

  -- Convert new center tile coords to geographic
  tileToGeo newCenterTileX newCenterTileY newZoom

/-- Create new viewport after zoom-to-point.
    The geographic point under the cursor remains fixed after zooming. -/
def zoomToPoint (vp : MapViewport) (cursorX cursorY : Float)
                (newZoom : Int) : MapViewport :=
  let clampedZoom := clampZoom newZoom
  let (newLat, newLon) := zoomToPointCenter vp cursorX cursorY clampedZoom

  -- Clamp latitude to valid Mercator range
  let clampedLat := floatClamp newLat (-85.0) 85.0

  -- Wrap longitude to [-180, 180]
  let wrappedLon := if newLon > 180.0 then newLon - 360.0
                    else if newLon < -180.0 then newLon + 360.0
                    else newLon

  { vp with
    centerLat := clampedLat
    centerLon := wrappedLon
    zoom := clampedZoom
  }

/-- Convert lat/lon to fractional tile position at fractional zoom level -/
def geoToTileFrac (lat lon : Float) (zoom : Float) : (Float × Float) :=
  let n := Float.pow 2.0 zoom
  let tileX := (lon + 180.0) / 360.0 * n
  let latRad := lat * pi / 180.0
  let tileY := (1.0 - Float.log (Float.tan latRad + 1.0 / Float.cos latRad) / pi) / 2.0 * n
  (tileX, tileY)

/-- Convert fractional tile position to lat/lon at fractional zoom level -/
def tileToGeoFrac (tileX tileY : Float) (zoom : Float) : (Float × Float) :=
  let n := Float.pow 2.0 zoom
  let lon := tileX / n * 360.0 - 180.0
  let latRad := Float.atan (Float.sinh (pi * (1.0 - 2.0 * tileY / n)))
  let lat := latRad * 180.0 / pi
  (lat, lon)

/-- Compute center position to keep anchor point fixed at anchor screen position.
    Given an anchor geographic point and where it should appear on screen,
    computes the map center that achieves this at the given (fractional) zoom. -/
def centerForAnchor (anchorLat anchorLon : Float)
    (anchorScreenX anchorScreenY : Float)
    (screenWidth screenHeight tileSize : Int)
    (displayZoom : Float) : (Float × Float) :=
  -- Convert anchor point to tile coordinates at current display zoom
  let (anchorTileX, anchorTileY) := geoToTileFrac anchorLat anchorLon displayZoom
  -- Compute offset from screen center to anchor in tile units
  let cx := (intToFloat screenWidth) / 2.0
  let cy := (intToFloat screenHeight) / 2.0
  let dxTile := (anchorScreenX - cx) / (intToFloat tileSize)
  let dyTile := (anchorScreenY - cy) / (intToFloat tileSize)
  -- Center tile position: anchor - offset
  let centerTileX := anchorTileX - dxTile
  let centerTileY := anchorTileY - dyTile
  -- Convert back to geographic coordinates
  tileToGeoFrac centerTileX centerTileY displayZoom

end Afferent.Map.Zoom

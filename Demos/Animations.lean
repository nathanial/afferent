/-
  Animations Demo - Psychedelic animated shapes ("Disco Party")
-/
import Afferent

open Afferent CanvasM Linalg

namespace Demos

/-- HSV to RGB color conversion (h in [0,1], s,v in [0,1]) -/
def hsvToRgb (h s v : Float) : Color :=
  let h6 := h * 6.0
  let i := h6.floor
  let f := h6 - i
  let p := v * (1.0 - s)
  let q := v * (1.0 - s * f)
  let t := v * (1.0 - s * (1.0 - f))
  let mod := (i.toUInt32 % 6).toNat
  match mod with
  | 0 => Color.rgb v t p
  | 1 => Color.rgb q v p
  | 2 => Color.rgb p v t
  | 3 => Color.rgb p q v
  | 4 => Color.rgb t p v
  | _ => Color.rgb v p q

/-- Render psychedelic animation cell using CanvasM -/
def renderAnimationsM (t : Float) : CanvasM Unit := do
  -- Spinning star cluster
  save
  translate 150 150
  for i in [:7] do
    save
    let angle := t * 2.0 + i.toFloat * (Float.twoPi / 7.0)
    let dist := 60 + 20 * Float.sin (t * 3.0 + i.toFloat)
    translate (dist * Float.cos angle) (dist * Float.sin angle)
    rotate (t * 4.0 + i.toFloat)
    let hue := (t * 0.5 + i.toFloat / 7.0) - (t * 0.5 + i.toFloat / 7.0).floor
    setFillColor (hsvToRgb hue 1.0 1.0)
    fillPath (Path.star ⟨0, 0⟩ (20 + 10 * Float.sin (t * 5.0)) 10 5)
    restore
  restore

  -- Pulsing rainbow circles
  save
  translate 400 150
  for i in [:12] do
    let angle := i.toFloat * (Float.pi / 6.0)
    let pulse := 0.5 + 0.5 * Float.sin (t * 4.0 + i.toFloat * 0.5)
    let radius := 20 + 30 * pulse
    let x := 80 * Float.cos (angle + t)
    let y := 80 * Float.sin (angle + t)
    let hue := (i.toFloat / 12.0 + t * 0.3) - (i.toFloat / 12.0 + t * 0.3).floor
    setFillColor (hsvToRgb hue 1.0 1.0)
    fillCircle ⟨x, y⟩ radius
  restore

  -- Wiggling lines (sine wave with moving phase)
  save
  translate 650 100
  for row in [:5] do
    let rowOffset := row.toFloat * 0.5
    setLineWidth (2 + row.toFloat)
    let hue := (row.toFloat / 5.0 + t * 0.2) - (row.toFloat / 5.0 + t * 0.2).floor
    setStrokeColor (hsvToRgb hue 1.0 1.0)
    let mut path := Path.empty
    path := path.moveTo ⟨0, row.toFloat * 50⟩
    for i in [:20] do
      let x := i.toFloat * 15
      let y := row.toFloat * 50 + 20 * Float.sin (t * 6.0 + x * 0.05 + rowOffset)
      path := path.lineTo ⟨x, y⟩
    strokePath path
  restore

  -- Morphing polygon (changing number of sides smoothly via rotation)
  save
  translate 150 300
  rotate (t * 1.5)
  let sides := 3 + ((t * 0.5).floor.toUInt32 % 6).toNat
  let hue := (t * 0.4) - (t * 0.4).floor
  setFillColor (hsvToRgb hue 0.8 0.9)
  fillPath (Path.polygon ⟨0, 0⟩ (40 + 20 * Float.sin t) sides)
  restore

  -- Orbiting hearts with trail effect
  save
  translate 400 320
  for i in [:8] do
    let trailT := t - i.toFloat * 0.05
    let angle := trailT * 2.0
    let x := 60 * Float.cos angle
    let y := 40 * Float.sin angle
    let alpha := 1.0 - i.toFloat * 0.12
    let hue := (trailT * 0.3) - (trailT * 0.3).floor
    let color := hsvToRgb hue 1.0 1.0
    setFillColor (Color.rgba color.r color.g color.b alpha)
    save
    translate x y
    scale (0.3 + 0.1 * Float.sin (t * 3.0)) (0.3 + 0.1 * Float.sin (t * 3.0))
    fillPath (Path.heart ⟨0, 0⟩ 80)
    restore
  restore

  -- Bouncing rectangles with color cycling
  save
  translate 650 280
  for i in [:6] do
    let phase := i.toFloat * 0.8
    let bounce := Float.abs (Float.sin (t * 3.0 + phase)) * 60
    let x := i.toFloat * 45
    let rotation := t * 2.0 + phase
    save
    translate x (-bounce)
    rotate rotation
    let hue := (t * 0.5 + i.toFloat / 6.0) - (t * 0.5 + i.toFloat / 6.0).floor
    setFillColor (hsvToRgb hue 0.9 1.0)
    fillRectXYWH (-15) (-15) 30 30
    restore
  restore

end Demos

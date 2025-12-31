/-
  Afferent FPS Camera
  First-person-shooter style camera with position and look direction.
-/
import Linalg

namespace Afferent.Render

open Linalg

/-- FPS camera state with position and orientation. -/
structure FPSCamera where
  x : Float := 0.0
  y : Float := 0.0
  z : Float := 12.0
  yaw : Float := 0.0      -- Horizontal rotation (radians), 0 = looking toward -Z
  pitch : Float := 0.0    -- Vertical rotation (radians), clamped to ±89°
  moveSpeed : Float := 5.0
  lookSensitivity : Float := 0.003
  deriving Repr

namespace FPSCamera

/-- A sensible default FPS camera (uses the field defaults above). -/
def default : FPSCamera := {}

end FPSCamera

instance : Inhabited FPSCamera := ⟨FPSCamera.default⟩

namespace FPSCamera

/-- Update camera from input. Returns new camera state.
    dt: delta time in seconds
    forward/back/left/right/up/down: movement key states
    mouseDeltaX/Y: mouse movement since last frame -/
def update (cam : FPSCamera) (dt : Float)
    (forward back left right up down : Bool)
    (mouseDeltaX mouseDeltaY : Float) : FPSCamera :=
  -- Update look direction from mouse
  let yaw := cam.yaw + mouseDeltaX * cam.lookSensitivity
  let pitch := Float.clamp (cam.pitch - mouseDeltaY * cam.lookSensitivity) (-Float.halfPi * 0.99) (Float.halfPi * 0.99)

  -- Calculate forward and right vectors (in XZ plane for movement)
  let fwdX := Float.sin yaw
  let fwdZ := -Float.cos yaw
  let rightX := Float.cos yaw
  let rightZ := Float.sin yaw

  -- Calculate movement delta
  let speed := cam.moveSpeed * dt
  let dx := Id.run do
    let mut d := 0.0
    if forward then d := d + fwdX * speed
    if back then d := d - fwdX * speed
    if right then d := d + rightX * speed
    if left then d := d - rightX * speed
    return d
  let dy := Id.run do
    let mut d := 0.0
    if up then d := d + speed
    if down then d := d - speed
    return d
  let dz := Id.run do
    let mut d := 0.0
    if forward then d := d + fwdZ * speed
    if back then d := d - fwdZ * speed
    if right then d := d + rightZ * speed
    if left then d := d - rightZ * speed
    return d

  { cam with x := cam.x + dx, y := cam.y + dy, z := cam.z + dz, yaw, pitch }

/-- Build view matrix from camera state. -/
def viewMatrix (cam : FPSCamera) : Mat4 :=
  -- Camera looks in direction based on yaw and pitch
  let cosPitch := Float.cos cam.pitch
  let sinPitch := Float.sin cam.pitch
  let cosYaw := Float.cos cam.yaw
  let sinYaw := Float.sin cam.yaw

  -- Forward direction (where camera looks)
  let fwdX := cosPitch * sinYaw
  let fwdY := sinPitch
  let fwdZ := -cosPitch * cosYaw

  -- Target point
  let targetX := cam.x + fwdX
  let targetY := cam.y + fwdY
  let targetZ := cam.z + fwdZ

  Mat4.lookAt ⟨cam.x, cam.y, cam.z⟩ ⟨targetX, targetY, targetZ⟩ Vec3.unitY

end FPSCamera

end Afferent.Render

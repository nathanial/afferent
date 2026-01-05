# Scale-to-fit for Trellis/Arbor

Goal: Add a first-class content scaling policy for layout containers (flex/grid/box) so content can scale down (and optionally up) to fit its container, while keeping hit-testing correct and scaling text normally.

Decisions
- Allow upscale: yes, but default is off.
- Modes: implement 3 modes now (contain, cover, stretch). These map cleanly to common UI/image semantics.
- Hit area default: scaled content only. For cover/stretch this effectively becomes the container area. Provide an override to use container hit area if needed.
- Text scales normally via transforms (no font re-measure). This is the intended behavior.

API proposal (Trellis BoxStyle)
- Add a new optional field:
  - contentScale : Option ContentScale
- ContentScale:
  - mode : ContentScaleMode = contain | cover | stretch
  - allowUpscale : Bool := false
  - anchor : ScaleAnchor := center | topLeft | top | topRight | left | right | bottomLeft | bottom | bottomRight
  - hitArea : ScaleHitArea := scaled | container

High-level behavior
- The container (flex/grid cell/box) lays out normally.
- The child is measured for its intrinsic size.
- A scale (and offset) is computed to fit the child inside the container using the selected mode.
- Rendering applies a translate + scale transform around the child render commands.
- Hit testing maps pointer coords into the child’s unscaled coordinate space using the inverse transform.

Layout changes (Trellis)
1) Measure pass
   - If contentScale is set on a container, measure its child with relaxed constraints to obtain an intrinsic size (w,h).
   - Parent layout sizing uses the container’s normal rules; the intrinsic size should not grow the parent unexpectedly.

2) Store scale metadata
   - Extend Trellis.ComputedLayout (or add a parallel metadata map keyed by WidgetId) to store:
     - contentScaleX, contentScaleY
     - contentOffsetX, contentOffsetY (relative to container’s content rect)
     - contentIntrinsicSize (w,h)
   - This is a deliberate, invasive change so Arbor and hit testing can use the transform.

3) Scale computation
   - Let availW/H be the container’s contentRect size.
   - Let intrinsicW/H be measured child size (min 1.0).
   - contain:
     - scale = min(availW/intrinsicW, availH/intrinsicH)
     - if !allowUpscale, scale = min(scale, 1)
     - scaleX = scaleY = scale
   - cover:
     - scale = max(availW/intrinsicW, availH/intrinsicH)
     - if !allowUpscale, scale = min(scale, 1)
     - scaleX = scaleY = scale
   - stretch:
     - scaleX = availW/intrinsicW; scaleY = availH/intrinsicH
     - if !allowUpscale, clamp each axis to <= 1
   - offset is computed from anchor and scaled size:
     - scaledW = intrinsicW * scaleX
     - scaledH = intrinsicH * scaleY
     - center anchor: ((availW - scaledW)/2, (availH - scaledH)/2)
     - topLeft: (0, 0), top: ((availW - scaledW)/2, 0), etc.

Rendering changes (Arbor)
- In collectCommands for a widget with contentScale:
  - pushClip(containerRect)
  - pushTranslate(offsetX, offsetY)
  - pushScale(scaleX, scaleY)
  - collect child commands
  - popTransform (scale)
  - popTransform (translate)
  - popClip
- This scales text and geometry uniformly, as intended.

Hit testing changes (Arbor)
- When descending into a scaled container:
  - If hitArea == scaled:
    - Compute scaled rect: (offsetX, offsetY, scaledW, scaledH)
    - Require pointer to be inside this rect (still within container clip).
  - If hitArea == container:
    - Use container bounds for “inside” check.
  - Map pointer to child-local coords:
    - localX = (x - offsetX) / scaleX
    - localY = (y - offsetY) / scaleY
  - Use local coords for child hit testing and path construction.

Arbor/Trellis integration
- The new contentScale metadata must be available to:
  - Render command collection
  - Hit test traversal (hitTestPath / hitTestResults)
- This means either:
  - expand Trellis.LayoutResult nodes to carry contentScale metadata, or
  - add a parallel map in Arbor keyed by widget id, produced during measure/layout.
- Either path is intentionally invasive to avoid hidden behavior.

Testing plan
- Layout/scale correctness:
  - contain vs cover vs stretch sizes
  - allowUpscale on/off
  - anchor offsets (center, corners)
- Render verification:
  - golden tests for scale/offset (if available)
- Hit tests:
  - pointer on scaled content hits
  - pointer outside scaled content (contain) does not hit when hitArea=scaled
  - pointer inside container hits when hitArea=container
- Integration sanity:
  - Counter widget in Overview should scale down inside grid cell.

Rollout steps
1) Add new ContentScale types and BoxStyle field in Trellis.
2) Update measure/layout to compute and store contentScale metadata.
3) Update Arbor render command collection to apply transforms.
4) Update Arbor hit testing to invert transforms.
5) Add tests for layout + hit test.
6) Apply to Counter cell in Overview as a demo.

Notes
- This is a structural change across Trellis layout, Arbor rendering, and Arbor hit testing. It is intentionally invasive to avoid ad hoc scaling hacks and to keep behavior consistent.

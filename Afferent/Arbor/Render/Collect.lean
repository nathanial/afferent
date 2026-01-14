/-
  Arbor Render Command Collector
  Convert widget trees + layouts into RenderCommand arrays.
  This is the key abstraction that makes rendering backend-independent.
-/
import Afferent.Arbor.Widget.Core
import Afferent.Arbor.Render.Cache
import Trellis

namespace Afferent.Arbor

/-- Render command collector state. -/
structure CollectState where
  commands : Array RenderCommand := #[]
  /-- Deferred absolute-positioned widgets to render after normal flow. -/
  deferredAbsolute : Array (Widget × Trellis.LayoutResult) := #[]
deriving Inhabited

/-- Collector monad for accumulating render commands. -/
abbrev CollectM := StateM CollectState

namespace CollectM

/-- Emit a single render command. -/
def emit (cmd : RenderCommand) : CollectM Unit := do
  modify fun s => { s with commands := s.commands.push cmd }

/-- Emit multiple render commands. -/
def emitAll (cmds : Array RenderCommand) : CollectM Unit := do
  modify fun s => { s with commands := s.commands ++ cmds }

/-- Defer an absolute-positioned widget to render after normal flow. -/
def deferAbsolute (w : Widget) (layouts : Trellis.LayoutResult) : CollectM Unit := do
  modify fun s => { s with deferredAbsolute := s.deferredAbsolute.push (w, layouts) }

/-- Run the collector and return the commands. -/
def execute {α : Type} (m : CollectM α) : Array RenderCommand :=
  (StateT.run m {}).2.commands

end CollectM

def isAbsoluteWidgetForRender (w : Widget) : Bool :=
  match w.style? with
  | some style => style.position == .absolute
  | none => false

/-- Separate children into flow (normal) and absolute-positioned. -/
def partitionChildren (children : Array Widget) : (Array Widget × Array Widget) := Id.run do
  let mut flow : Array Widget := #[]
  let mut abs : Array Widget := #[]
  for child in children do
    if isAbsoluteWidgetForRender child then
      abs := abs.push child
    else
      flow := flow.push child
  (flow, abs)

/-- Collect box background and border render commands based on BoxStyle. -/
def collectBoxStyle (rect : Trellis.LayoutRect) (style : BoxStyle) : CollectM Unit := do
  let r : Rect := ⟨⟨rect.x, rect.y⟩, ⟨rect.width, rect.height⟩⟩

  -- Background
  if let some bg := style.backgroundColor then
    CollectM.emit (.fillRect r bg style.cornerRadius)

  -- Border
  if let some bc := style.borderColor then
    if style.borderWidth > 0 then
      CollectM.emit (.strokeRect r bc style.borderWidth style.cornerRadius)

/-- Collect render commands for wrapped text with alignment.
    Text is vertically centered within the content rect.
    Baseline = top + verticalOffset + ascender (where verticalOffset centers the text block). -/
def collectWrappedText (contentRect : Trellis.LayoutRect) (font : FontId)
    (color : Color) (align : TextAlign) (textLayout : TextLayout) : CollectM Unit := do
  let lineHeight := textLayout.lineHeight
  let ascender := textLayout.ascender
  -- Vertical centering: offset to center the text block within the content rect
  let verticalOffset := (contentRect.height - textLayout.totalHeight) / 2
  -- First baseline: top of content + vertical offset + ascender
  let mut y := contentRect.y + verticalOffset + ascender

  for line in textLayout.lines do
    -- Calculate x based on alignment
    let x := match align with
      | .left => contentRect.x
      | .center => contentRect.x + (contentRect.width - line.width) / 2
      | .right => contentRect.x + contentRect.width - line.width

    CollectM.emit (.fillText line.text x y font color)
    y := y + lineHeight

/-- Collect render commands for single-line text (no wrapping).
    Text is vertically centered within the content rect. -/
def collectSingleLineText (contentRect : Trellis.LayoutRect) (text : String)
    (font : FontId) (color : Color) (align : TextAlign) (textWidth : Float)
    (lineHeight : Float) : CollectM Unit := do
  -- Calculate x based on alignment
  let x := match align with
    | .left => contentRect.x
    | .center => contentRect.x + (contentRect.width - textWidth) / 2
    | .right => contentRect.x + contentRect.width - textWidth

  -- Vertical centering with estimated ascender (0.8 * lineHeight)
  let ascender := lineHeight * 0.8
  let verticalOffset := (contentRect.height - lineHeight) / 2
  CollectM.emit (.fillText text x (contentRect.y + verticalOffset + ascender) font color)

/-- Compute the bounding box of children from their layouts. -/
def computeChildrenBounds (children : Array Widget) (layouts : Trellis.LayoutResult)
    : Trellis.LayoutRect :=
  let (minX, minY, maxX, maxY) := children.foldl (init := (1000000.0, 1000000.0, -1000000.0, -1000000.0))
    fun (minX, minY, maxX, maxY) child =>
      match layouts.get child.id with
      | some computed =>
        let r := computed.borderRect
        (min minX r.x, min minY r.y, max maxX (r.x + r.width), max maxY (r.y + r.height))
      | none => (minX, minY, maxX, maxY)
  if minX == 1000000.0 then
    { x := 0, y := 0, width := 0, height := 0 }
  else
    { x := minX, y := minY, width := maxX - minX, height := maxY - minY }

mutual
/-- Collect render commands for scaled children.
    Applies clip, translate, and scale transforms before rendering children.

    The transform maps from children's intrinsic bounding box to the
    container's available space with proper scaling and centering.

    Children are laid out with relaxed constraints during measure, so they
    have their intrinsic size. Trellis positions them within the container,
    potentially extending beyond contentRect if larger than available space.

    Transform sequence:
    1. Compute children's actual bounding box from layouts
    2. Translate to final position (contentRect origin + anchor offset)
    3. Scale by computed factors
    4. Translate children's bounding box to origin
-/
partial def collectScaledChildren (contentRect : Trellis.LayoutRect)
    (m : Trellis.ScaleMetadata)
    (children : Array Widget)
    (layouts : Trellis.LayoutResult) : CollectM Unit := do
  -- Clip to content area
  let clipRect : Rect := ⟨⟨contentRect.x, contentRect.y⟩, ⟨contentRect.width, contentRect.height⟩⟩
  CollectM.emit (.pushClip clipRect)
  CollectM.emit .save

  -- Compute the actual bounding box of children from their layouts.
  -- This may differ from contentRect if children are larger (e.g., centered overflow)
  let childBounds := computeChildrenBounds children layouts

  -- Apply transforms: translate to final position, scale, translate children to origin
  -- T1 * S * T2 where:
  -- T2 = translate(-childBounds.x, -childBounds.y): move children so their bounding box is at origin
  -- S = scale(scaleX, scaleY): scale the content
  -- T1 = translate(contentRect.x + offsetX, contentRect.y + offsetY): position at anchor
  CollectM.emit (.pushTranslate (contentRect.x + m.offsetX) (contentRect.y + m.offsetY))
  CollectM.emit (.pushScale m.scaleX m.scaleY)
  CollectM.emit (.pushTranslate (-childBounds.x) (-childBounds.y))

  -- Render children (they have absolute coordinates that we're transforming)
  -- Defer absolute children to render after all normal flow content
  let (flowChildren, absChildren) := partitionChildren children
  for child in flowChildren do
    collectWidget child layouts
  for child in absChildren do
    CollectM.deferAbsolute child layouts

  -- Pop transforms in reverse order
  CollectM.emit .popTransform  -- translate to origin
  CollectM.emit .popTransform  -- scale
  CollectM.emit .popTransform  -- translate to final position
  CollectM.emit .restore
  CollectM.emit .popClip

/-- Collect render commands for a widget tree using computed layout positions.
    The widget should have been measured (text layouts computed) before calling this.
    Returns an array of RenderCommands that can be executed by any backend. -/
partial def collectWidget (w : Widget) (layouts : Trellis.LayoutResult) : CollectM Unit := do
  let some computed := layouts.get w.id | return
  let borderRect := computed.borderRect
  let contentRect := computed.contentRect

  match w with
  | .rect _ _ style =>
    collectBoxStyle borderRect style

  | .text _ _ content font color align _ textLayoutOpt =>
    match textLayoutOpt with
    | some textLayout =>
      collectWrappedText contentRect font color align textLayout
    | none =>
      -- Fallback to single-line rendering with estimated dimensions
      -- (this path shouldn't normally be hit if measureWidget was called)
      collectSingleLineText contentRect content font color align contentRect.width 16.0

  | .spacer _ _ _ _ =>
    -- Spacers don't render anything
    pure ()

  | .custom _ _ style spec =>
    collectBoxStyle borderRect style
    CollectM.emitAll (spec.collect computed)

  | .flex _ _ _ style children =>
    collectBoxStyle borderRect style
    match computed.scaleMetadata with
    | some m =>
      -- Apply content scale transforms
      collectScaledChildren contentRect m children layouts
    | none =>
      -- Render flow children inline, defer absolute children
      let (flowChildren, absChildren) := partitionChildren children
      for child in flowChildren do
        collectWidget child layouts
      for child in absChildren do
        CollectM.deferAbsolute child layouts

  | .grid _ _ _ style children =>
    collectBoxStyle borderRect style
    match computed.scaleMetadata with
    | some m =>
      -- Apply content scale transforms
      collectScaledChildren contentRect m children layouts
    | none =>
      -- Render flow children inline, defer absolute children
      let (flowChildren, absChildren) := partitionChildren children
      for child in flowChildren do
        collectWidget child layouts
      for child in absChildren do
        CollectM.deferAbsolute child layouts

  | .scroll _ _ style scrollState contentWidth contentHeight scrollbarConfig child =>
    -- Render background
    collectBoxStyle borderRect style

    -- Set up clipping to content area
    let clipRect : Rect := ⟨⟨contentRect.x, contentRect.y⟩, ⟨contentRect.width, contentRect.height⟩⟩
    CollectM.emit (.pushClip clipRect)

    -- Save state and apply scroll offset
    CollectM.emit .save
    CollectM.emit (.pushTranslate (-scrollState.offsetX) (-scrollState.offsetY))

    -- Render child
    collectWidget child layouts

    -- Restore state
    CollectM.emit .popTransform
    CollectM.emit .restore
    CollectM.emit .popClip

    -- Render scrollbars (after content, so they overlay)
    let viewportW := contentRect.width
    let viewportH := contentRect.height
    let thickness := scrollbarConfig.thickness
    let minThumb := scrollbarConfig.minThumbLength
    let radius := scrollbarConfig.cornerRadius

    -- Vertical scrollbar
    if scrollbarConfig.showVertical && contentHeight > viewportH then
      -- Calculate scrollable range
      let maxScrollY := contentHeight - viewportH
      let scrollRatio := if maxScrollY > 0 then scrollState.offsetY / maxScrollY else 0

      -- Calculate thumb size (proportional to viewport/content ratio)
      let thumbRatio := viewportH / contentHeight
      let thumbHeight := max minThumb (viewportH * thumbRatio)
      let trackHeight := viewportH
      let thumbTravel := trackHeight - thumbHeight
      let thumbY := thumbTravel * scrollRatio

      -- Track rect (right edge of content area)
      let trackX := contentRect.x + viewportW - thickness
      let trackRect : Rect := ⟨⟨trackX, contentRect.y⟩, ⟨thickness, trackHeight⟩⟩
      CollectM.emit (.fillRect trackRect scrollbarConfig.trackColor radius)

      -- Thumb rect
      let thumbRect : Rect := ⟨⟨trackX, contentRect.y + thumbY⟩, ⟨thickness, thumbHeight⟩⟩
      CollectM.emit (.fillRect thumbRect scrollbarConfig.thumbColor radius)

    -- Horizontal scrollbar
    if scrollbarConfig.showHorizontal && contentWidth > viewportW then
      -- Calculate scrollable range
      let maxScrollX := contentWidth - viewportW
      let scrollRatio := if maxScrollX > 0 then scrollState.offsetX / maxScrollX else 0

      -- Calculate thumb size (proportional to viewport/content ratio)
      let thumbRatio := viewportW / contentWidth
      let thumbWidth := max minThumb (viewportW * thumbRatio)
      let trackWidth := viewportW
      let thumbTravel := trackWidth - thumbWidth
      let thumbX := thumbTravel * scrollRatio

      -- Track rect (bottom edge of content area)
      let trackY := contentRect.y + viewportH - thickness
      let trackRect : Rect := ⟨⟨contentRect.x, trackY⟩, ⟨trackWidth, thickness⟩⟩
      CollectM.emit (.fillRect trackRect scrollbarConfig.trackColor radius)

      -- Thumb rect
      let thumbRect : Rect := ⟨⟨contentRect.x + thumbX, trackY⟩, ⟨thumbWidth, thickness⟩⟩
      CollectM.emit (.fillRect thumbRect scrollbarConfig.thumbColor radius)

end  -- mutual

/-- Render all deferred absolute-positioned widgets.
    Called after the main tree traversal to ensure they render on top. -/
partial def renderDeferredAbsolute : CollectM Unit := do
  let state ← get
  -- Clear the deferred list before processing (in case rendering adds more)
  set { state with deferredAbsolute := #[] }
  for (widget, layouts) in state.deferredAbsolute do
    collectWidget widget layouts
  -- Check if any new absolute elements were deferred during rendering
  let newState ← get
  if newState.deferredAbsolute.size > 0 then
    renderDeferredAbsolute

/-- Collect render commands for a widget tree.
    This is the main entry point for converting a widget tree to render commands.
    Absolute-positioned elements are rendered after all normal flow content. -/
def collectCommands (w : Widget) (layouts : Trellis.LayoutResult) : Array RenderCommand :=
  CollectM.execute do
    collectWidget w layouts
    renderDeferredAbsolute

/-- Collect render commands with an initial save/restore wrapper. -/
def collectCommandsWithSave (w : Widget) (layouts : Trellis.LayoutResult) : Array RenderCommand :=
  CollectM.execute do
    CollectM.emit .save
    collectWidget w layouts
    renderDeferredAbsolute
    CollectM.emit .restore

/-- Collect debug border commands for all layout cells.
    Draws a colored stroke rect around each widget's border rect.
    Useful for debugging layout issues. -/
partial def collectDebugBorders (w : Widget) (layouts : Trellis.LayoutResult)
    (color : Color := ⟨0.5, 1.0, 0.5, 0.5⟩) (lineWidth : Float := 1.0) : CollectM Unit := do
  let some computed := layouts.get w.id | return
  let r := computed.borderRect
  let rect : Rect := ⟨⟨r.x, r.y⟩, ⟨r.width, r.height⟩⟩
  CollectM.emit (.strokeRect rect color lineWidth 0)

  -- Recurse into children
  for child in w.children do
    collectDebugBorders child layouts color lineWidth

/-- Collect both regular widget commands and debug borders.
    Returns commands that render the widget with debug borders overlaid. -/
def collectCommandsWithDebug (w : Widget) (layouts : Trellis.LayoutResult)
    (borderColor : Color := ⟨0.5, 1.0, 0.5, 0.5⟩) : Array RenderCommand :=
  CollectM.execute do
    collectWidget w layouts
    renderDeferredAbsolute
    collectDebugBorders w layouts borderColor

/-! ## Cached Collection

These functions provide render command caching at the widget level.
Cache is keyed by path-based identity + layout hash. Each widget gets a unique
path based on its position in the tree (e.g., "0.2.1" for the 2nd child of the
3rd child of the 1st child of root). This provides automatic caching for all
CustomSpec widgets without requiring explicit names.

When data changes, dynWidget rebuilds a subtree, and the paths within that
subtree naturally change, causing cache misses for the updated widgets. -/

/-- Cached collector state with access to the render cache. -/
structure CachedCollectState where
  commands : Array RenderCommand := #[]
  /-- Deferred absolute-positioned widgets with their paths for cache key generation. -/
  deferredAbsolute : Array (Widget × Trellis.LayoutResult × String) := #[]
  cacheHits : Nat := 0
  cacheMisses : Nat := 0
deriving Inhabited

/-- Cached collector monad with IO for cache access. -/
abbrev CachedCollectM := StateT CachedCollectState IO

namespace CachedCollectM

def emit (cmd : RenderCommand) : CachedCollectM Unit := do
  modify fun s => { s with commands := s.commands.push cmd }

def emitAll (cmds : Array RenderCommand) : CachedCollectM Unit := do
  modify fun s => { s with commands := s.commands ++ cmds }

def deferAbsolute (w : Widget) (layouts : Trellis.LayoutResult) (path : String) : CachedCollectM Unit := do
  modify fun s => { s with deferredAbsolute := s.deferredAbsolute.push (w, layouts, path) }

def recordCacheHit : CachedCollectM Unit := do
  modify fun s => { s with cacheHits := s.cacheHits + 1 }

def recordCacheMiss : CachedCollectM Unit := do
  modify fun s => { s with cacheMisses := s.cacheMisses + 1 }

end CachedCollectM

/-- Build a child path by appending an index to the parent path. -/
def childPath (parentPath : String) (index : Nat) : String :=
  if parentPath.isEmpty then s!"{index}" else s!"{parentPath}.{index}"

/-- Collect box background and border render commands (cached version). -/
def collectBoxStyleCached (rect : Trellis.LayoutRect) (style : BoxStyle) : CachedCollectM Unit := do
  let r : Rect := ⟨⟨rect.x, rect.y⟩, ⟨rect.width, rect.height⟩⟩
  if let some bg := style.backgroundColor then
    CachedCollectM.emit (.fillRect r bg style.cornerRadius)
  if let some bc := style.borderColor then
    if style.borderWidth > 0 then
      CachedCollectM.emit (.strokeRect r bc style.borderWidth style.cornerRadius)

/-- Collect wrapped text (cached version). -/
def collectWrappedTextCached (contentRect : Trellis.LayoutRect) (font : FontId)
    (color : Color) (align : TextAlign) (textLayout : TextLayout) : CachedCollectM Unit := do
  let lineHeight := textLayout.lineHeight
  let ascender := textLayout.ascender
  let verticalOffset := (contentRect.height - textLayout.totalHeight) / 2
  let mut y := contentRect.y + verticalOffset + ascender
  for line in textLayout.lines do
    let x := match align with
      | .left => contentRect.x
      | .center => contentRect.x + (contentRect.width - line.width) / 2
      | .right => contentRect.x + contentRect.width - line.width
    CachedCollectM.emit (.fillText line.text x y font color)
    y := y + lineHeight

/-- Collect single-line text (cached version). -/
def collectSingleLineTextCached (contentRect : Trellis.LayoutRect) (text : String)
    (font : FontId) (color : Color) (align : TextAlign) (textWidth : Float)
    (lineHeight : Float) : CachedCollectM Unit := do
  let x := match align with
    | .left => contentRect.x
    | .center => contentRect.x + (contentRect.width - textWidth) / 2
    | .right => contentRect.x + contentRect.width - textWidth
  let ascender := lineHeight * 0.8
  let verticalOffset := (contentRect.height - lineHeight) / 2
  CachedCollectM.emit (.fillText text x (contentRect.y + verticalOffset + ascender) font color)

mutual
/-- Collect scaled children (cached version with path tracking). -/
partial def collectScaledChildrenCached (cache : IO.Ref RenderCache)
    (contentRect : Trellis.LayoutRect) (m : Trellis.ScaleMetadata)
    (children : Array Widget) (layouts : Trellis.LayoutResult)
    (path : String) : CachedCollectM Unit := do
  let clipRect : Rect := ⟨⟨contentRect.x, contentRect.y⟩, ⟨contentRect.width, contentRect.height⟩⟩
  CachedCollectM.emit (.pushClip clipRect)
  CachedCollectM.emit .save
  let childBounds := computeChildrenBounds children layouts
  CachedCollectM.emit (.pushTranslate (contentRect.x + m.offsetX) (contentRect.y + m.offsetY))
  CachedCollectM.emit (.pushScale m.scaleX m.scaleY)
  CachedCollectM.emit (.pushTranslate (-childBounds.x) (-childBounds.y))
  let (flowChildren, absChildren) := partitionChildren children
  -- Track child indices for path generation
  let mut flowIdx := 0
  for child in flowChildren do
    collectWidgetCached cache child layouts (childPath path flowIdx)
    flowIdx := flowIdx + 1
  let mut absIdx := flowIdx
  for child in absChildren do
    CachedCollectM.deferAbsolute child layouts (childPath path absIdx)
    absIdx := absIdx + 1
  CachedCollectM.emit .popTransform
  CachedCollectM.emit .popTransform
  CachedCollectM.emit .popTransform
  CachedCollectM.emit .restore
  CachedCollectM.emit .popClip

/-- Collect render commands for a widget tree with caching support.
    All CustomSpec widgets are automatically cached using path-based identity.
    The path represents the widget's position in the tree (e.g., "0.2.1"). -/
partial def collectWidgetCached (cache : IO.Ref RenderCache)
    (w : Widget) (layouts : Trellis.LayoutResult) (path : String) : CachedCollectM Unit := do
  let some computed := layouts.get w.id | return
  let borderRect := computed.borderRect
  let contentRect := computed.contentRect

  match w with
  | .rect _ _ style =>
    collectBoxStyleCached borderRect style

  | .text _ _ content font color align _ textLayoutOpt =>
    match textLayoutOpt with
    | some textLayout =>
      collectWrappedTextCached contentRect font color align textLayout
    | none =>
      collectSingleLineTextCached contentRect content font color align contentRect.width 16.0

  | .spacer _ _ _ _ =>
    pure ()

  | .custom _ name style spec =>
    collectBoxStyleCached borderRect style
    let layoutHash := hashLayoutRect contentRect

    -- Use widget name if provided, otherwise use path + generation.
    -- Named widgets use stable names (for expensive widgets like charts).
    -- Unnamed widgets include generation so cache is invalidated when dynWidget rebuilds.
    let cacheKey := match name with
      | some widgetName => widgetName
      | none => s!"@{path}:{spec.generation}"

    let renderCache ← cache.get
    match renderCache.find? cacheKey with
    | some cached =>
      if cached.layoutHash == layoutHash then
        -- Cache hit! Use cached commands
        CachedCollectM.emitAll cached.commands
        CachedCollectM.recordCacheHit
      else
        -- Layout changed, recompute and update cache
        let cmds := spec.collect computed
        cache.modify fun rc => rc.insert cacheKey ⟨cmds, layoutHash⟩
        CachedCollectM.emitAll cmds
        CachedCollectM.recordCacheMiss
    | none =>
      -- First time seeing this widget, compute and cache
      let cmds := spec.collect computed
      cache.modify fun rc => rc.insert cacheKey ⟨cmds, layoutHash⟩
      CachedCollectM.emitAll cmds
      CachedCollectM.recordCacheMiss

  | .flex _ _ _ style children =>
    collectBoxStyleCached borderRect style
    match computed.scaleMetadata with
    | some m =>
      collectScaledChildrenCached cache contentRect m children layouts path
    | none =>
      let (flowChildren, absChildren) := partitionChildren children
      let mut flowIdx := 0
      for child in flowChildren do
        collectWidgetCached cache child layouts (childPath path flowIdx)
        flowIdx := flowIdx + 1
      let mut absIdx := flowIdx
      for child in absChildren do
        CachedCollectM.deferAbsolute child layouts (childPath path absIdx)
        absIdx := absIdx + 1

  | .grid _ _ _ style children =>
    collectBoxStyleCached borderRect style
    match computed.scaleMetadata with
    | some m =>
      collectScaledChildrenCached cache contentRect m children layouts path
    | none =>
      let (flowChildren, absChildren) := partitionChildren children
      let mut flowIdx := 0
      for child in flowChildren do
        collectWidgetCached cache child layouts (childPath path flowIdx)
        flowIdx := flowIdx + 1
      let mut absIdx := flowIdx
      for child in absChildren do
        CachedCollectM.deferAbsolute child layouts (childPath path absIdx)
        absIdx := absIdx + 1

  | .scroll _ _ style scrollState contentWidth contentHeight scrollbarConfig child =>
    collectBoxStyleCached borderRect style
    let clipRect : Rect := ⟨⟨contentRect.x, contentRect.y⟩, ⟨contentRect.width, contentRect.height⟩⟩
    CachedCollectM.emit (.pushClip clipRect)
    CachedCollectM.emit .save
    CachedCollectM.emit (.pushTranslate (-scrollState.offsetX) (-scrollState.offsetY))
    collectWidgetCached cache child layouts (childPath path 0)
    CachedCollectM.emit .popTransform
    CachedCollectM.emit .restore
    CachedCollectM.emit .popClip

    -- Render scrollbars
    let viewportW := contentRect.width
    let viewportH := contentRect.height
    let thickness := scrollbarConfig.thickness
    let minThumb := scrollbarConfig.minThumbLength
    let radius := scrollbarConfig.cornerRadius

    if scrollbarConfig.showVertical && contentHeight > viewportH then
      let maxScrollY := contentHeight - viewportH
      let scrollRatio := if maxScrollY > 0 then scrollState.offsetY / maxScrollY else 0
      let thumbRatio := viewportH / contentHeight
      let thumbHeight := max minThumb (viewportH * thumbRatio)
      let trackHeight := viewportH
      let thumbTravel := trackHeight - thumbHeight
      let thumbY := thumbTravel * scrollRatio
      let trackX := contentRect.x + viewportW - thickness
      let trackRect : Rect := ⟨⟨trackX, contentRect.y⟩, ⟨thickness, trackHeight⟩⟩
      CachedCollectM.emit (.fillRect trackRect scrollbarConfig.trackColor radius)
      let thumbRect : Rect := ⟨⟨trackX, contentRect.y + thumbY⟩, ⟨thickness, thumbHeight⟩⟩
      CachedCollectM.emit (.fillRect thumbRect scrollbarConfig.thumbColor radius)

    if scrollbarConfig.showHorizontal && contentWidth > viewportW then
      let maxScrollX := contentWidth - viewportW
      let scrollRatio := if maxScrollX > 0 then scrollState.offsetX / maxScrollX else 0
      let thumbRatio := viewportW / contentWidth
      let thumbWidth := max minThumb (viewportW * thumbRatio)
      let trackWidth := viewportW
      let thumbTravel := trackWidth - thumbWidth
      let thumbX := thumbTravel * scrollRatio
      let trackY := contentRect.y + viewportH - thickness
      let trackRect : Rect := ⟨⟨contentRect.x, trackY⟩, ⟨trackWidth, thickness⟩⟩
      CachedCollectM.emit (.fillRect trackRect scrollbarConfig.trackColor radius)
      let thumbRect : Rect := ⟨⟨contentRect.x + thumbX, trackY⟩, ⟨thumbWidth, thickness⟩⟩
      CachedCollectM.emit (.fillRect thumbRect scrollbarConfig.thumbColor radius)

end  -- mutual

/-- Render all deferred absolute-positioned widgets (cached version). -/
partial def renderDeferredAbsoluteCached (cache : IO.Ref RenderCache) : CachedCollectM Unit := do
  let state ← get
  set { state with deferredAbsolute := #[] }
  for (widget, layouts, widgetPath) in state.deferredAbsolute do
    collectWidgetCached cache widget layouts widgetPath
  let newState ← get
  if newState.deferredAbsolute.size > 0 then
    renderDeferredAbsoluteCached cache

/-- Collect render commands with caching.
    This is the main entry point for cached render command collection.
    All CustomSpec widgets are automatically cached using path-based identity. -/
def collectCommandsCached (cache : IO.Ref RenderCache) (w : Widget)
    (layouts : Trellis.LayoutResult) : IO (Array RenderCommand) := do
  let ((), state) ← StateT.run (do
    collectWidgetCached cache w layouts ""  -- Start with empty path at root
    renderDeferredAbsoluteCached cache) {}
  pure state.commands

/-- Collect render commands with caching and return statistics.
    Returns (commands, cacheHits, cacheMisses). -/
def collectCommandsCachedWithStats (cache : IO.Ref RenderCache) (w : Widget)
    (layouts : Trellis.LayoutResult) : IO (Array RenderCommand × Nat × Nat) := do
  let ((), state) ← StateT.run (do
    collectWidgetCached cache w layouts ""  -- Start with empty path at root
    renderDeferredAbsoluteCached cache) {}
  pure (state.commands, state.cacheHits, state.cacheMisses)

end Afferent.Arbor

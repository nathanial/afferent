/-
  Canopy Pagination Widget
  Page navigation controls for navigating through paginated data.
-/
import Reactive
import Afferent.Canopy.Core
import Afferent.Canopy.Theme
import Afferent.Canopy.Widget.Display.Label
import Afferent.Canopy.Reactive.Component

namespace Afferent.Canopy

open Afferent.Arbor hiding Event
open Reactive Reactive.Host
open Afferent.Canopy.Reactive
open Trellis

/-- Configuration for pagination appearance and behavior. -/
structure PaginationConfig where
  /-- Maximum page buttons to show (including first/last and ellipses). -/
  maxVisiblePages : Nat := 7
  /-- Show first and last page buttons when ellipsis is present. -/
  showFirstLast : Bool := true
  /-- Size of each page button (square). -/
  buttonSize : Float := 32.0
  /-- Gap between buttons. -/
  gap : Float := 4.0
  /-- Corner radius for buttons. -/
  cornerRadius : Float := 6.0
deriving Repr, Inhabited

namespace PaginationConfig

def default : PaginationConfig := {}

end PaginationConfig

/-- Represents a button in the pagination bar. -/
inductive PageButton where
  /-- A clickable page number (0-indexed). -/
  | page (n : Nat)
  /-- A non-clickable ellipsis "..." -/
  | ellipsis
  /-- Previous page button. -/
  | prev
  /-- Next page button. -/
  | next
deriving Repr, BEq, Inhabited

namespace Pagination

/-- Calculate which page buttons to show based on current page and total pages.
    Returns an array of PageButton values.

    For `maxVisiblePages=7` and `totalPages=10`:
    - Current page 0: `[prev] [0] [1] [2] [3] [4] [...] [9] [next]`
    - Current page 5: `[prev] [0] [...] [4] [5] [6] [...] [9] [next]`
    - Current page 9: `[prev] [0] [...] [5] [6] [7] [8] [9] [next]`
-/
def calculatePageButtons (current totalPages : Nat) (config : PaginationConfig) : Array PageButton := Id.run do
  if totalPages == 0 then
    return #[]
  if totalPages == 1 then
    return #[.prev, .page 0, .next]

  -- Always include prev/next
  let mut buttons : Array PageButton := #[.prev]

  -- If total pages fits within maxVisible (excluding prev/next), show all
  let innerMax := config.maxVisiblePages
  if totalPages <= innerMax then
    for i in [:totalPages] do
      buttons := buttons.push (.page i)
  else if config.showFirstLast then
    -- Complex case: need ellipsis
    -- Reserve slots: first, last, and at least some middle pages
    -- Format: [0] [...] [mid pages] [...] [last]
    -- We have innerMax slots for page buttons + ellipses

    let lastPage := totalPages - 1

    -- Always show first page
    buttons := buttons.push (.page 0)

    -- Calculate how many middle pages we can show
    -- We need: 1 (first) + potentially 1 (left ellipsis) + middle + potentially 1 (right ellipsis) + 1 (last) <= innerMax
    -- Minimum middle = innerMax - 4 (first, two ellipses, last), but at least 1
    let middleCount := if innerMax > 4 then innerMax - 4 else 1

    -- Determine the range of middle pages centered on current
    let halfMiddle := middleCount / 2
    let mut startMiddle := if current > halfMiddle + 1 then current - halfMiddle else 1
    let mut endMiddle := startMiddle + middleCount - 1

    -- Adjust if we're near the end
    if endMiddle >= lastPage then
      endMiddle := lastPage - 1
      startMiddle := if endMiddle >= middleCount then endMiddle - middleCount + 1 else 1

    -- Adjust if we're near the start
    if startMiddle <= 1 then
      startMiddle := 1
      endMiddle := if startMiddle + middleCount - 1 < lastPage then startMiddle + middleCount - 1 else lastPage - 1

    -- Left ellipsis if there's a gap after first page
    if startMiddle > 1 then
      buttons := buttons.push .ellipsis

    -- Middle pages
    for i in [startMiddle:endMiddle + 1] do
      buttons := buttons.push (.page i)

    -- Right ellipsis if there's a gap before last page
    if endMiddle < lastPage - 1 then
      buttons := buttons.push .ellipsis

    -- Always show last page
    buttons := buttons.push (.page lastPage)
  else
    -- No first/last anchors, just show pages around current
    let halfVisible := innerMax / 2
    let mut startPage := if current > halfVisible then current - halfVisible else 0
    let mut endPage := startPage + innerMax - 1
    if endPage >= totalPages then
      endPage := totalPages - 1
      startPage := if endPage >= innerMax - 1 then endPage - innerMax + 1 else 0
    for i in [startPage:endPage + 1] do
      buttons := buttons.push (.page i)

  buttons := buttons.push .next
  return buttons

/-- Get the display text for a page button. -/
def buttonText : PageButton → String
  | .page n => toString (n + 1)  -- Display 1-indexed
  | .ellipsis => "..."
  | .prev => "<"
  | .next => ">"

/-- Check if a page button is clickable. -/
def isClickable (btn : PageButton) (current totalPages : Nat) : Bool :=
  match btn with
  | .page n => n != current
  | .ellipsis => false
  | .prev => current > 0
  | .next => current < totalPages - 1

end Pagination

/-- Build a single pagination button visual. -/
def paginationButtonVisual (name : String) (btn : PageButton) (current totalPages : Nat)
    (hovered : Bool) (theme : Theme) (config : PaginationConfig := {}) : WidgetBuilder := do
  let isEnabled := Pagination.isClickable btn current totalPages
  let isCurrentPage := match btn with
    | .page n => n == current
    | _ => false
  let isEllipsis := btn == .ellipsis

  let bgColor :=
    if isCurrentPage then theme.primary.background
    else if !isEnabled then theme.input.backgroundDisabled
    else if hovered then theme.secondary.backgroundHover
    else theme.secondary.background

  let textColor :=
    if isCurrentPage then theme.primary.foreground
    else if !isEnabled then theme.textMuted
    else if isEllipsis then theme.textMuted
    else theme.text

  let style : BoxStyle := {
    backgroundColor := some bgColor
    borderColor := some theme.input.border
    borderWidth := 1
    cornerRadius := config.cornerRadius
    width := .length config.buttonSize
    height := .length config.buttonSize
  }

  let wid ← freshId
  let props : FlexContainer := {
    direction := .row
    alignItems := .center
    justifyContent := .center
  }
  let label ← text' (Pagination.buttonText btn) theme.font textColor .center
  pure (.flex wid (some name) props style #[label])

/-- Build the complete pagination bar visual. -/
def paginationVisual (containerName : String) (buttonNameFn : Nat → String)
    (buttons : Array PageButton) (current totalPages : Nat) (hoveredIdx : Option Nat)
    (theme : Theme) (config : PaginationConfig := {}) : WidgetBuilder := do
  let mut children : Array Widget := #[]
  for i in [:buttons.size] do
    let btn := buttons.getD i .ellipsis
    let isHovered := hoveredIdx == some i
    let btnWidget ← paginationButtonVisual (buttonNameFn i) btn current totalPages isHovered theme config
    children := children.push btnWidget

  let outerStyle : BoxStyle := {}
  let outerWid ← freshId
  let outerProps : FlexContainer := { direction := .row, gap := config.gap, alignItems := .center }
  pure (.flex outerWid (some containerName) outerProps outerStyle children)

/-! ## Reactive Pagination Component -/

/-- Pagination result - events and dynamics. -/
structure PaginationResult where
  /-- Event that fires when page changes (with new page index). -/
  onPageChange : Reactive.Event Spider Nat
  /-- Current page index (0-based). -/
  currentPage : Reactive.Dynamic Spider Nat

/-- Create a reactive pagination component using WidgetM.
    - `totalPages`: Total number of pages
    - `initialPage`: Initial page index (0-based)
    - `config`: Optional configuration
-/
def pagination (totalPages : Nat) (initialPage : Nat := 0)
    (config : PaginationConfig := {}) : WidgetM PaginationResult := do
  let theme ← getThemeW
  if totalPages == 0 then
    -- Empty pagination - no pages to navigate
    let ctx ← SpiderM.getTimelineCtx
    let neverEvent ← SpiderM.liftIO (Reactive.Event.never ctx)
    let constDyn ← Dynamic.pureM (0 : Nat)
    return { onPageChange := neverEvent, currentPage := constDyn }

  let containerName ← registerComponentW "pagination" (isInteractive := false)

  -- Clamp initial page
  let clampedInitial := if initialPage >= totalPages then totalPages - 1 else initialPage

  -- Register names for each button position (we register for max possible buttons)
  let maxButtons := config.maxVisiblePages + 2  -- +2 for prev/next
  let mut buttonNames : Array String := #[]
  for _ in [:maxButtons] do
    let name ← registerComponentW "pagination-btn"
    buttonNames := buttonNames.push name
  let buttonNameFn (i : Nat) : String := buttonNames.getD i ""

  -- Click detection
  let allClicks ← useAllClicks

  -- Find which button was clicked and what action it represents
  let findClickedAction (data : ClickData) (currentPage : Nat) : Option Nat := Id.run do
    let buttons := Pagination.calculatePageButtons currentPage totalPages config
    for i in [:buttons.size] do
      let name := buttonNameFn i
      if hitWidget data name then
        let btn := buttons.getD i .ellipsis
        match btn with
        | .page n => if n != currentPage then return some n else return none
        | .prev => if currentPage > 0 then return some (currentPage - 1) else return none
        | .next => if currentPage < totalPages - 1 then return some (currentPage + 1) else return none
        | .ellipsis => return none
    return none

  -- Use foldDyn to track current page, updating on clicks
  let currentPageDyn ← Reactive.foldDyn
    (fun (click : ClickData) (page : Nat) =>
      match findClickedAction click page with
      | some newPage => newPage
      | none => page)
    clampedInitial
    allClicks

  let pageChanges ← Dynamic.changesM currentPageDyn
  let onPageChange ← Event.mapMaybeM
    (fun (old, new) => if old != new then some new else none)
    pageChanges

  -- Hover tracking for buttons
  let hoverChanges ← StateT.lift (hoverIndexEvent buttonNames)
  let hoveredButton ← Reactive.holdDyn none hoverChanges

  -- Refs for caching
  let totalPagesRef := totalPages
  let configRef := config

  -- Render with dynWidget
  let renderState ← Dynamic.zipWithM (fun p h => (p, h)) currentPageDyn hoveredButton
  let _ ← dynWidget renderState fun (currentPage, hoveredIdx) => do
    let buttons := Pagination.calculatePageButtons currentPage totalPagesRef configRef
    emit do pure (paginationVisual containerName buttonNameFn buttons currentPage totalPagesRef hoveredIdx theme configRef)

  pure { onPageChange, currentPage := currentPageDyn }

end Afferent.Canopy

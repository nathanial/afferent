# Canopy Widget Roadmap

A complete desktop widget framework for building rich GUI applications in Lean 4.

## Currently Implemented

- [x] **Button** - Primary, secondary, outline, ghost variants
- [x] **Checkbox** - Toggle with label
- [x] **TextInput** - Single-line text entry with cursor
- [x] **Label** - Heading1/2/3, body text, caption
- [x] **Panel** - Elevated, outlined, filled containers
- [x] **Theme** - Dark/light theming with color schemes

## Input Widgets

- [ ] **RadioButton** - Single selection from group
- [ ] **RadioGroup** - Container managing radio button exclusivity
- [ ] **Switch/Toggle** - iOS-style on/off toggle
- [ ] **Slider** - Horizontal/vertical value slider with optional labels
- [x] **RangeSlider** - Dual-handle for min/max range selection
- [x] **Stepper** - Increment/decrement numeric input (+/- buttons)
- [ ] **TextArea** - Multi-line text input with scrolling
- [x] **PasswordInput** - Masked text input
- [ ] **SearchInput** - Text input with search icon and clear button
- [ ] **ColorPicker** - Color selection (hue/saturation/brightness)
- [x] **DatePicker** - Calendar-based date selection
- [ ] **TimePicker** - Hour/minute/second selection
- [ ] **FilePicker** - File/folder browser dialog

## Selection Widgets

- [ ] **Dropdown/Select** - Single selection from dropdown list
- [ ] **ComboBox** - Dropdown with text input for filtering
- [ ] **MultiSelect** - Multiple selection with chips/tags
- [ ] **ListBox** - Scrollable list with single/multi selection
- [ ] **TreeView** - Hierarchical expandable/collapsible tree
- [ ] **Menu** - Context menu / popup menu
- [ ] **MenuBar** - Application menu bar

## Display Widgets

- [ ] **Badge** - Small status indicator (counts, notifications)
- [ ] **Chip/Tag** - Compact labeled element (removable)
- [ ] **Avatar** - User profile image (circular, with fallback initials)
- [ ] **Icon** - Scalable vector icons
- [ ] **Tooltip** - Hover information popup
- [ ] **ProgressBar** - Determinate/indeterminate progress
- [ ] **Spinner/Loader** - Loading indicator animation
- [ ] **Separator/Divider** - Horizontal/vertical line separator
- [ ] **Breadcrumb** - Navigation path indicator

## Layout Widgets

- [ ] **Card** - Styled container with optional header/footer
- [ ] **Accordion** - Collapsible content sections
- [ ] **TabView** - Tabbed content panels
- [x] **SplitPane** - Resizable split container (horizontal/vertical)
- [ ] **ScrollView** - Scrollable content area with scrollbars
- [ ] **Sidebar** - Collapsible side navigation panel
- [ ] **Toolbar** - Horizontal bar with action buttons
- [ ] **StatusBar** - Bottom status information bar
- [ ] **Modal/Dialog** - Overlay dialog with backdrop
- [ ] **Popover** - Anchored floating content
- [ ] **Drawer** - Slide-in panel from edge

## Data Display Widgets

- [ ] **Table** - Sortable, filterable data table with columns
- [x] **DataGrid** - Editable table with cell editing
- [x] **VirtualList** - Efficiently rendered long lists
- [ ] **Calendar** - Month/week/day calendar view
- [ ] **Timeline** - Chronological event display
- [ ] **Chart** - Basic charts (bar, line, pie)

## Feedback Widgets

- [ ] **Toast/Snackbar** - Temporary notification message
- [ ] **Alert** - Inline warning/error/success/info message
- [ ] **ConfirmDialog** - Yes/No confirmation dialog
- [ ] **Skeleton** - Loading placeholder animation

## Navigation Widgets

- [ ] **Link** - Clickable text link
- [ ] **NavList** - Vertical navigation list
- [ ] **Pagination** - Page navigation controls
- [ ] **Stepper** (wizard) - Multi-step form progress

## Form Widgets

- [ ] **Form** - Form container with validation state
- [ ] **FormField** - Label + input + error message wrapper
- [ ] **ValidationMessage** - Error/warning text display

## Implementation Priority

### Phase 1: Core Input (Foundation)
1. RadioButton / RadioGroup
2. Switch/Toggle
3. Slider
4. TextArea
5. Dropdown/Select

### Phase 2: Layout & Navigation
1. TabView
2. Modal/Dialog
3. ScrollView (enhanced)
4. Tooltip
5. Menu / MenuBar

### Phase 3: Data & Feedback
1. Table
2. Toast/Snackbar
3. ProgressBar
4. ListBox
5. TreeView

### Phase 4: Advanced
1. VirtualList
2. SplitPane (done)
3. DatePicker (done)
4. ColorPicker
5. DataGrid (done)
6. RangeSlider (done)

## Design Principles

1. **Themeable** - All widgets respect Theme colors and sizing
2. **Accessible** - Keyboard navigation, focus management
3. **Composable** - Widgets can be nested and combined
4. **Stateless rendering** - Visual widgets are pure functions of state
5. **Event-driven** - Widgets emit messages, parent handles state

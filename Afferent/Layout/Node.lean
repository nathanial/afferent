/-
  Afferent Layout Node
  Unified layout tree structure supporting flex and grid containers.
-/
import Afferent.Layout.Types
import Afferent.Layout.Flex
import Afferent.Layout.Grid

namespace Afferent.Layout

/-- The type of container a node represents. -/
inductive ContainerKind where
  | flex (props : FlexContainer)   -- Flexbox container
  | grid (props : GridContainer)   -- Grid container
  | none                           -- Not a container (leaf node)
deriving Repr, BEq

namespace ContainerKind

def isFlex : ContainerKind → Bool
  | .flex _ => true
  | _ => false

def isGrid : ContainerKind → Bool
  | .grid _ => true
  | _ => false

def isNone : ContainerKind → Bool
  | .none => true
  | _ => false

end ContainerKind

/-- Item properties based on parent container type. -/
inductive ItemKind where
  | flexChild (props : FlexItem)   -- Child of a flex container
  | gridChild (props : GridItem)   -- Child of a grid container
  | none                           -- No special item properties
deriving Repr, BEq

namespace ItemKind

def isFlex : ItemKind → Bool
  | .flexChild _ => true
  | _ => false

def isGrid : ItemKind → Bool
  | .gridChild _ => true
  | _ => false

/-- Get flex item properties if this is a flex child. -/
def flexItem? : ItemKind → Option FlexItem
  | .flexChild props => some props
  | _ => Option.none

/-- Get grid item properties if this is a grid child. -/
def gridItem? : ItemKind → Option GridItem
  | .gridChild props => some props
  | _ => Option.none

end ItemKind

/-- Intrinsic content size for leaf nodes. -/
structure ContentSize where
  width : Length
  height : Length
deriving Repr, BEq, Inhabited

namespace ContentSize

def zero : ContentSize := ⟨0, 0⟩

def mk' (w h : Length) : ContentSize := ⟨w, h⟩

end ContentSize

/-- A node in the layout tree. -/
inductive LayoutNode where
  | mk (id : Nat)
       (box : BoxConstraints)
       (container : ContainerKind)
       (item : ItemKind)
       (content : Option ContentSize)
       (children : Array LayoutNode)
deriving Repr

instance : Inhabited LayoutNode :=
  ⟨LayoutNode.mk 0 {} .none .none Option.none #[]⟩

namespace LayoutNode

/-- Get the unique identifier of this node. -/
def id : LayoutNode → Nat
  | mk id .. => id

/-- Get box constraints. -/
def box : LayoutNode → BoxConstraints
  | mk _ box .. => box

/-- Get container kind. -/
def container : LayoutNode → ContainerKind
  | mk _ _ container .. => container

/-- Get item properties. -/
def item : LayoutNode → ItemKind
  | mk _ _ _ item .. => item

/-- Get content size for leaf nodes. -/
def content : LayoutNode → Option ContentSize
  | mk _ _ _ _ content _ => content

/-- Get children array. -/
def children : LayoutNode → Array LayoutNode
  | mk _ _ _ _ _ children => children

/-- Check if this is a leaf node (no children). -/
def isLeaf (n : LayoutNode) : Bool := n.children.isEmpty

/-- Check if this is a flex container. -/
def isFlex (n : LayoutNode) : Bool := n.container.isFlex

/-- Check if this is a grid container. -/
def isGrid (n : LayoutNode) : Bool := n.container.isGrid

/-- Get flex container properties if this is a flex container. -/
def flexContainer? : LayoutNode → Option FlexContainer
  | mk _ _ (.flex props) .. => some props
  | _ => none

/-- Get grid container properties if this is a grid container. -/
def gridContainer? : LayoutNode → Option GridContainer
  | mk _ _ (.grid props) .. => some props
  | _ => none

/-- Get flex item properties if this is a flex child. -/
def flexItem? (n : LayoutNode) : Option FlexItem := n.item.flexItem?

/-- Get grid item properties if this is a grid child. -/
def gridItem? (n : LayoutNode) : Option GridItem := n.item.gridItem?

/-! ## Builder Functions -/

/-- Create a leaf node with intrinsic content size. -/
def leaf (id : Nat) (content : ContentSize)
    (box : BoxConstraints := {})
    (item : ItemKind := .none) : LayoutNode :=
  mk id box .none item (some content) #[]

/-- Create a leaf node with width and height. -/
def leaf' (id : Nat) (width height : Length)
    (box : BoxConstraints := {})
    (item : ItemKind := .none) : LayoutNode :=
  leaf id ⟨width, height⟩ box item

/-- Create a flex container node. -/
def flexBox (id : Nat) (props : FlexContainer)
    (children : Array LayoutNode)
    (box : BoxConstraints := {})
    (item : ItemKind := .none) : LayoutNode :=
  mk id box (.flex props) item none children

/-- Create a flex row container. -/
def row (id : Nat) (children : Array LayoutNode)
    (gap : Length := 0)
    (box : BoxConstraints := {})
    (item : ItemKind := .none) : LayoutNode :=
  flexBox id (FlexContainer.row gap) children box item

/-- Create a flex column container. -/
def column (id : Nat) (children : Array LayoutNode)
    (gap : Length := 0)
    (box : BoxConstraints := {})
    (item : ItemKind := .none) : LayoutNode :=
  flexBox id (FlexContainer.column gap) children box item

/-- Create a grid container node. -/
def gridBox (id : Nat) (props : GridContainer)
    (children : Array LayoutNode)
    (box : BoxConstraints := {})
    (item : ItemKind := .none) : LayoutNode :=
  mk id box (.grid props) item none children

/-- Create a simple grid with n columns. -/
def grid (id : Nat) (columns : Nat) (children : Array LayoutNode)
    (gap : Length := 0)
    (box : BoxConstraints := {})
    (item : ItemKind := .none) : LayoutNode :=
  gridBox id (GridContainer.columns columns gap) children box item

/-! ## Modification Functions -/

/-- Set box constraints on a node. -/
def withBox (n : LayoutNode) (box : BoxConstraints) : LayoutNode :=
  match n with
  | mk id _ container item content children =>
    mk id box container item content children

/-- Set item kind on a node. -/
def withItem (n : LayoutNode) (item : ItemKind) : LayoutNode :=
  match n with
  | mk id box container _ content children =>
    mk id box container item content children

/-- Add a child to a container node. -/
def addChild (n : LayoutNode) (child : LayoutNode) : LayoutNode :=
  match n with
  | mk id box container item content children =>
    mk id box container item content (children.push child)

/-- Set children of a container node. -/
def withChildren (n : LayoutNode) (children : Array LayoutNode) : LayoutNode :=
  match n with
  | mk id box container item content _ =>
    mk id box container item content children

/-- Map a function over children. -/
def mapChildren (n : LayoutNode) (f : LayoutNode → LayoutNode) : LayoutNode :=
  n.withChildren (n.children.map f)

/-- Count total nodes in tree. -/
partial def nodeCount (n : LayoutNode) : Nat :=
  1 + n.children.foldl (fun acc child => acc + child.nodeCount) 0

/-- Get all node IDs in tree. -/
partial def allIds (n : LayoutNode) : Array Nat :=
  #[n.id] ++ n.children.flatMap allIds

end LayoutNode

end Afferent.Layout

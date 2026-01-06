/-
  Afferent.Arbor.Style.CSS - CSS-like DSL for BoxStyle

  Provides a compile-time `css!` macro that transforms CSS-like syntax
  into BoxStyle values.

  Example:
  ```lean
  css! {
    background-color: red;
    border: 1 solid white;
    padding: 10;
    min-width: 80;
    flex-grow: 1;
  }
  ```

  Produces a BoxStyle with corresponding fields set.
-/
import Lean
import Afferent.Arbor.Widget.Core
import Trellis
import Tincture

namespace Afferent.Arbor.CSS

open Lean

/-! ## Syntax Categories -/

/-- CSS value: numbers, identifiers, hex colors, color functions -/
declare_syntax_cat cssValue

/-- Bare number: 10, 1.5 -/
syntax num : cssValue

/-- Number with unit: 10px, 50pct (use pct instead of % for Lean syntax) -/
syntax num ident : cssValue

/-- Bare float number: 1.5, 0.75 -/
syntax scientific : cssValue

/-- Float with unit: 1.5px, 0.5pct -/
syntax scientific ident : cssValue

/-- Identifier (for colors, keywords): red, auto, center -/
syntax ident : cssValue

/-- Hex color: #fff, #ffffff -/
syntax "#" ident : cssValue

/-- rgb(r, g, b) color function -/
syntax "rgb(" num "," num "," num ")" : cssValue

/-- rgba(r, g, b, a) color function with integer alpha (0-255) -/
syntax "rgba(" num "," num "," num "," num ")" : cssValue

/-- rgba(r, g, b, a) color function with float alpha (0.0-1.0) -/
syntax "rgba(" num "," num "," num "," scientific ")" : cssValue

/-- gray(level) shorthand for grayscale -/
syntax "gray(" num ")" : cssValue

/-- hsv(h, s, v) color function - all values 0.0-1.0 -/
syntax "hsv(" scientific "," scientific "," scientific ")" : cssValue

/-- hsva(h, s, v, a) color function - all values 0.0-1.0 -/
syntax "hsva(" scientific "," scientific "," scientific "," scientific ")" : cssValue

/-- hsl(h, s, l) color function - all values 0.0-1.0 -/
syntax "hsl(" scientific "," scientific "," scientific ")" : cssValue

/-- hsla(h, s, l, a) color function - all values 0.0-1.0 -/
syntax "hsla(" scientific "," scientific "," scientific "," scientific ")" : cssValue

/-- CSS property: `property-name: value1 value2 ...;` -/
syntax cssProperty := ident ("-" ident)* ":" cssValue+ ";"

/-- Main css! syntax -/
syntax "css!" "{" cssProperty* "}" : term

/-! ## Helper Types -/

/-- Accumulated field values during macro expansion -/
private structure CSSFields where
  backgroundColor : Option (TSyntax `term) := none
  borderColor : Option (TSyntax `term) := none
  borderWidth : Option (TSyntax `term) := none
  cornerRadius : Option (TSyntax `term) := none
  padding : Option (TSyntax `term) := none
  margin : Option (TSyntax `term) := none
  width : Option (TSyntax `term) := none
  height : Option (TSyntax `term) := none
  minWidth : Option (TSyntax `term) := none
  maxWidth : Option (TSyntax `term) := none
  minHeight : Option (TSyntax `term) := none
  maxHeight : Option (TSyntax `term) := none
  flexItem : Option (TSyntax `term) := none

/-! ## Helper Functions -/

/-- Get the string value of a property name from parts -/
private def propertyNameToString (parts : Array Name) : String :=
  parts.foldl (init := "") fun acc part =>
    if acc.isEmpty then part.toString
    else acc ++ "-" ++ part.toString

/-- Extract all property name parts from syntax -/
private def extractPropertyParts (firstPart : TSyntax `ident)
    (restParts : TSyntaxArray `ident) : Array Name :=
  #[firstPart.getId] ++ restParts.map (·.getId)

/-- Parse a CSS color value -/
private def parseColor (stx : TSyntax `cssValue) : MacroM (TSyntax `term) := do
  match stx with
  -- rgb(r, g, b) - values 0-255
  | `(cssValue| rgb( $r:num , $g:num , $b:num )) =>
    `(Tincture.Color.fromRgb8 $(r) $(g) $(b))
  -- rgba(r, g, b, a) - all values 0-255
  | `(cssValue| rgba( $r:num , $g:num , $b:num , $a:num )) =>
    `(Tincture.Color.fromRgb8 $(r) $(g) $(b) $(a))
  -- rgba(r, g, b, a) - r,g,b 0-255, alpha 0.0-1.0
  | `(cssValue| rgba( $r:num , $g:num , $b:num , $a:scientific )) =>
    `(Tincture.Color.fromRgb8 $(r) $(g) $(b) (Float.toUInt8 ($(a) * 255.0)))
  -- gray(level) - level 0-100 (percentage)
  | `(cssValue| gray( $level:num )) =>
    `(Tincture.Color.gray (($(level) : Float) / 100.0))
  -- hsv(h, s, v) - all values 0.0-1.0
  | `(cssValue| hsv( $h:scientific , $s:scientific , $v:scientific )) =>
    `(Tincture.Color.hsv $(h) $(s) $(v))
  -- hsva(h, s, v, a) - all values 0.0-1.0
  | `(cssValue| hsva( $h:scientific , $s:scientific , $v:scientific , $a:scientific )) =>
    `(Tincture.Color.hsva $(h) $(s) $(v) $(a))
  -- hsl(h, s, l) - all values 0.0-1.0
  | `(cssValue| hsl( $h:scientific , $s:scientific , $l:scientific )) =>
    `(Tincture.Color.hsl $(h) $(s) $(l))
  -- hsla(h, s, l, a) - all values 0.0-1.0
  | `(cssValue| hsla( $h:scientific , $s:scientific , $l:scientific , $a:scientific )) =>
    `(Tincture.Color.hsla $(h) $(s) $(l) $(a))
  -- Hex color
  | `(cssValue| # $hex:ident) =>
    let hexStr := "#" ++ hex.getId.toString
    `(Tincture.Color.fromHex $(quote hexStr) |>.getD Tincture.Color.black)
  -- Named colors
  | `(cssValue| $name:ident) =>
    let colorName := name.getId.toString
    match colorName with
    | "red" => `(Tincture.Named.red)
    | "green" => `(Tincture.Named.green)
    | "blue" => `(Tincture.Named.blue)
    | "yellow" => `(Tincture.Named.yellow)
    | "cyan" => `(Tincture.Named.cyan)
    | "magenta" => `(Tincture.Named.magenta)
    | "orange" => `(Tincture.Named.orange)
    | "white" => `(Tincture.Named.white)
    | "black" => `(Tincture.Named.black)
    | "gray" | "grey" => `(Tincture.Named.gray)
    | "pink" => `(Tincture.Named.pink)
    | "purple" => `(Tincture.Named.purple)
    | "brown" => `(Tincture.Named.brown)
    | "lime" => `(Tincture.Named.lime)
    | "navy" => `(Tincture.Named.navy)
    | "teal" => `(Tincture.Named.teal)
    | "olive" => `(Tincture.Named.olive)
    | "maroon" => `(Tincture.Named.maroon)
    | "aqua" => `(Tincture.Named.aqua)
    | "silver" => `(Tincture.Named.silver)
    | "coral" => `(Tincture.Named.coral)
    | "crimson" => `(Tincture.Named.crimson)
    | "gold" => `(Tincture.Named.gold)
    | "salmon" => `(Tincture.Named.salmon)
    | "tomato" => `(Tincture.Named.tomato)
    | "transparent" => `(Tincture.Color.transparent)
    | _ => `(Tincture.Color.fromName $(quote colorName) |>.getD Tincture.Color.black)
  | _ => Macro.throwError s!"Invalid color value: {stx}"

/-- Parse a CSS length value (returns Float) -/
private def parseLength (stx : TSyntax `cssValue) : MacroM (TSyntax `term) := do
  match stx with
  | `(cssValue| $n:num $u:ident) =>
    let unit := u.getId.toString
    match unit with
    | "px" => `(($(n) : Float))
    | "pct" =>
      -- For percentages, divide by 100
      `(($(n) : Float) / 100.0)
    | _ => Macro.throwError s!"Unknown length unit: {unit}"
  | `(cssValue| $n:scientific $u:ident) =>
    let unit := u.getId.toString
    match unit with
    | "px" => `(($(n) : Float))
    | "pct" => `(($(n) : Float) / 100.0)
    | _ => Macro.throwError s!"Unknown length unit: {unit}"
  | `(cssValue| $n:num) =>
    `(($(n) : Float))
  | `(cssValue| $n:scientific) =>
    `(($(n) : Float))
  | _ => Macro.throwError s!"Invalid length value: {stx}"

/-- Parse a CSS dimension value (returns Trellis.Dimension) -/
private def parseDimension (stx : TSyntax `cssValue) : MacroM (TSyntax `term) := do
  match stx with
  | `(cssValue| auto) => `(Trellis.Dimension.auto)
  | `(cssValue| $n:num $u:ident) =>
    let unit := u.getId.toString
    match unit with
    | "px" => `(Trellis.Dimension.length $(n))
    | "pct" =>
      -- For percentages, divide by 100
      `(Trellis.Dimension.percent (($(n) : Float) / 100.0))
    | _ => Macro.throwError s!"Unknown dimension unit: {unit}"
  | `(cssValue| $n:scientific $u:ident) =>
    let unit := u.getId.toString
    match unit with
    | "px" => `(Trellis.Dimension.length $(n))
    | "pct" => `(Trellis.Dimension.percent (($(n) : Float) / 100.0))
    | _ => Macro.throwError s!"Unknown dimension unit: {unit}"
  | `(cssValue| $n:num) => `(Trellis.Dimension.length $(n))
  | `(cssValue| $n:scientific) => `(Trellis.Dimension.length $(n))
  | `(cssValue| $id:ident) =>
    let name := id.getId.toString
    match name with
    | "auto" => `(Trellis.Dimension.auto)
    | _ => Macro.throwError s!"Invalid dimension value: {name}"
  | _ => Macro.throwError s!"Invalid dimension value: {stx}"

/-- Build the final BoxStyle from accumulated fields -/
private def buildBoxStyle (fields : CSSFields) : MacroM (TSyntax `term) := do
  let mut stx ← `(Afferent.Arbor.BoxStyle.default)

  if let some v := fields.backgroundColor then
    stx ← `({ $stx with backgroundColor := some $v })
  if let some v := fields.borderColor then
    stx ← `({ $stx with borderColor := some $v })
  if let some v := fields.borderWidth then
    stx ← `({ $stx with borderWidth := $v })
  if let some v := fields.cornerRadius then
    stx ← `({ $stx with cornerRadius := $v })
  if let some v := fields.padding then
    stx ← `({ $stx with padding := $v })
  if let some v := fields.margin then
    stx ← `({ $stx with margin := $v })
  if let some v := fields.width then
    stx ← `({ $stx with width := $v })
  if let some v := fields.height then
    stx ← `({ $stx with height := $v })
  if let some v := fields.minWidth then
    stx ← `({ $stx with minWidth := some $v })
  if let some v := fields.maxWidth then
    stx ← `({ $stx with maxWidth := some $v })
  if let some v := fields.minHeight then
    stx ← `({ $stx with minHeight := some $v })
  if let some v := fields.maxHeight then
    stx ← `({ $stx with maxHeight := some $v })
  if let some v := fields.flexItem then
    stx ← `({ $stx with flexItem := some $v })

  pure stx

/-! ## Main Macro -/

macro_rules
  | `(css! { $props:cssProperty* }) => do
    let mut fields : CSSFields := {}

    for prop in props do
      match prop with
      | `(cssProperty| $first:ident $[- $rest:ident]* : $values:cssValue* ;) =>
        let propName := propertyNameToString (extractPropertyParts first rest)
        let vals : Array (TSyntax `cssValue) := values

        match propName with
        -- Background
        | "background-color" | "background" =>
          let colorTerm ← parseColor vals[0]!
          fields := { fields with backgroundColor := some colorTerm }

        -- Border color
        | "border-color" =>
          let colorTerm ← parseColor vals[0]!
          fields := { fields with borderColor := some colorTerm }

        -- Border width
        | "border-width" =>
          let widthTerm ← parseLength vals[0]!
          fields := { fields with borderWidth := some widthTerm }

        -- Border shorthand: width style color OR just width
        | "border" =>
          if vals.size >= 3 then
            let widthTerm ← parseLength vals[0]!
            let colorTerm ← parseColor vals[2]!
            fields := { fields with borderWidth := some widthTerm, borderColor := some colorTerm }
          else if vals.size >= 1 then
            let widthTerm ← parseLength vals[0]!
            fields := { fields with borderWidth := some widthTerm }

        -- Corner radius
        | "border-radius" | "corner-radius" =>
          let radiusTerm ← parseLength vals[0]!
          fields := { fields with cornerRadius := some radiusTerm }

        -- Dimensions
        | "width" =>
          let dimTerm ← parseDimension vals[0]!
          fields := { fields with width := some dimTerm }

        | "height" =>
          let dimTerm ← parseDimension vals[0]!
          fields := { fields with height := some dimTerm }

        | "min-width" =>
          let lenTerm ← parseLength vals[0]!
          fields := { fields with minWidth := some lenTerm }

        | "max-width" =>
          let lenTerm ← parseLength vals[0]!
          fields := { fields with maxWidth := some lenTerm }

        | "min-height" =>
          let lenTerm ← parseLength vals[0]!
          fields := { fields with minHeight := some lenTerm }

        | "max-height" =>
          let lenTerm ← parseLength vals[0]!
          fields := { fields with maxHeight := some lenTerm }

        -- Padding
        | "padding" =>
          if vals.size == 1 then
            let pTerm ← parseLength vals[0]!
            let padTerm ← `(Trellis.EdgeInsets.uniform $pTerm)
            fields := { fields with padding := some padTerm }
          else if vals.size == 2 then
            let vTerm ← parseLength vals[0]!
            let hTerm ← parseLength vals[1]!
            let padTerm ← `(Trellis.EdgeInsets.symmetric $hTerm $vTerm)
            fields := { fields with padding := some padTerm }
          else if vals.size == 4 then
            let tTerm ← parseLength vals[0]!
            let rTerm ← parseLength vals[1]!
            let bTerm ← parseLength vals[2]!
            let lTerm ← parseLength vals[3]!
            let padTerm ← `({ top := $tTerm, right := $rTerm, bottom := $bTerm, left := $lTerm : Trellis.EdgeInsets })
            fields := { fields with padding := some padTerm }

        -- Margin
        | "margin" =>
          if vals.size == 1 then
            let mTerm ← parseLength vals[0]!
            let marginTerm ← `(Trellis.EdgeInsets.uniform $mTerm)
            fields := { fields with margin := some marginTerm }
          else if vals.size == 2 then
            let vTerm ← parseLength vals[0]!
            let hTerm ← parseLength vals[1]!
            let marginTerm ← `(Trellis.EdgeInsets.symmetric $hTerm $vTerm)
            fields := { fields with margin := some marginTerm }

        -- Flex item properties
        | "flex-grow" =>
          let growTerm ← parseLength vals[0]!
          let flexTerm ← `(Trellis.FlexItem.growing $growTerm)
          fields := { fields with flexItem := some flexTerm }

        -- Flex shorthand: grow [shrink] [basis]
        | "flex" =>
          if vals.size >= 1 then
            let growTerm ← parseLength vals[0]!
            if vals.size >= 2 then
              let shrinkTerm ← parseLength vals[1]!
              if vals.size >= 3 then
                let basisTerm ← parseDimension vals[2]!
                let flexTerm ← `({ grow := $growTerm, shrink := $shrinkTerm, basis := $basisTerm : Trellis.FlexItem })
                fields := { fields with flexItem := some flexTerm }
              else
                let flexTerm ← `({ grow := $growTerm, shrink := $shrinkTerm : Trellis.FlexItem })
                fields := { fields with flexItem := some flexTerm }
            else
              let flexTerm ← `(Trellis.FlexItem.growing $growTerm)
              fields := { fields with flexItem := some flexTerm }

        | other =>
          Macro.throwError s!"Unknown CSS property: {other}"

      | _ => Macro.throwError "Invalid CSS property syntax"

    buildBoxStyle fields

end Afferent.Arbor.CSS

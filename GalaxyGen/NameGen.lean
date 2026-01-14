/-
  GalaxyGen.NameGen
  Simple syllable-based name generation.
-/

import Std
import GalaxyGen.Rng

namespace GalaxyGen

namespace NameGen

private def syllables : Array String := #[
  "al", "an", "ar", "bel", "cor", "den", "el", "far", "gan", "hel",
  "ian", "jor", "kel", "lor", "mir", "nel", "or", "pra", "quil", "ran",
  "sin", "tor", "ul", "vor", "wen", "xer", "yor", "zen"
]

private def suffixes : Array String := #[
  "ia", "on", "ara", "us", "ix", "um", "prime", "major", "minor"
]

/-- Capitalize the first character of a string. -/
def capitalize (s : String) : String :=
  match s.toList with
  | [] => s
  | c :: cs => String.ofList (c.toUpper :: cs)

private def join (parts : Array String) : String :=
  parts.foldl (fun acc part => acc ++ part) ""

/-- Convert a small number into a roman numeral.
    Falls back to decimal for values outside the table. -/
def romanNumeral (n : Nat) : String :=
  match n with
  | 0 => "0"
  | 1 => "I"
  | 2 => "II"
  | 3 => "III"
  | 4 => "IV"
  | 5 => "V"
  | 6 => "VI"
  | 7 => "VII"
  | 8 => "VIII"
  | 9 => "IX"
  | 10 => "X"
  | 11 => "XI"
  | 12 => "XII"
  | 13 => "XIII"
  | 14 => "XIV"
  | 15 => "XV"
  | 16 => "XVI"
  | 17 => "XVII"
  | 18 => "XVIII"
  | 19 => "XIX"
  | 20 => "XX"
  | _ => toString n

/-- Generate a new star/system name. -/
def starName : Gen String := do
  let syllableCount ← Gen.nextNatRange 2 3
  let mut parts : Array String := #[]
  for _ in [0:syllableCount] do
    let part ← Gen.choose syllables
    parts := parts.push part
  let base := capitalize (join parts)
  let addSuffix ← Gen.nextBoolWeighted 0.25
  if addSuffix then
    let suffix ← Gen.choose suffixes
    return base ++ suffix
  return base

/-- Generate a planet name based on the parent star. -/
def planetName (star : String) (index : Nat) : Gen String := do
  let numeral := romanNumeral index
  let useHyphen ← Gen.nextBoolWeighted 0.2
  if useHyphen then
    return s!"{star}-{numeral}"
  return s!"{star} {numeral}"

end NameGen

end GalaxyGen

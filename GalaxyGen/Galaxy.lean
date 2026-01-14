/-
  GalaxyGen.Galaxy
  Galaxy data structures (systems + hyperlanes).
-/

import Std
import GalaxyGen.System

namespace GalaxyGen

/-- A connection between two star systems. -/
structure Hyperlane where
  a : Nat
  b : Nat
  deriving Repr, BEq, DecidableEq, Hashable

namespace Hyperlane

/-- Create an ordered hyperlane to keep pairs stable. -/
def ordered (a b : Nat) : Hyperlane :=
  if a <= b then
    { a := a, b := b }
  else
    { a := b, b := a }

instance : ToString Hyperlane := ⟨fun lane => s!"{lane.a}-{lane.b}"⟩

end Hyperlane

/-- Galaxy container. -/
structure Galaxy where
  systems : Array StarSystem
  hyperlanes : Array Hyperlane
  deriving Repr, BEq

end GalaxyGen

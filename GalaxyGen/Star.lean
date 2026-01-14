/-
  GalaxyGen.Star
  Star classifications and properties.
-/

namespace GalaxyGen

/-- Stellar classification (OBAFGKM). -/
inductive StarClass
  | o
  | b
  | a
  | f
  | g
  | k
  | m
  deriving Repr, BEq, DecidableEq

instance : Inhabited StarClass := ⟨.g⟩

namespace StarClass

/-- Short label for a star class. -/
def label : StarClass → String
  | .o => "O"
  | .b => "B"
  | .a => "A"
  | .f => "F"
  | .g => "G"
  | .k => "K"
  | .m => "M"

instance : ToString StarClass := ⟨label⟩

/-- Weights for selecting star classes. -/
def weights : Array (StarClass × Float) := #[(.o, 0.00003), (.b, 0.0013), (.a, 0.006),
  (.f, 0.03), (.g, 0.076), (.k, 0.12), (.m, 0.76667)]

/-- Approximate mass range in solar masses. -/
def massRange : StarClass → (Float × Float)
  | .o => (16.0, 60.0)
  | .b => (2.1, 16.0)
  | .a => (1.4, 2.1)
  | .f => (1.0, 1.4)
  | .g => (0.8, 1.0)
  | .k => (0.6, 0.8)
  | .m => (0.08, 0.6)

/-- Approximate radius range in solar radii. -/
def radiusRange : StarClass → (Float × Float)
  | .o => (6.0, 15.0)
  | .b => (2.0, 6.0)
  | .a => (1.4, 2.5)
  | .f => (1.1, 1.4)
  | .g => (0.9, 1.1)
  | .k => (0.7, 0.9)
  | .m => (0.1, 0.7)

/-- Approximate luminosity range in solar luminosities. -/
def luminosityRange : StarClass → (Float × Float)
  | .o => (30000.0, 1000000.0)
  | .b => (25.0, 30000.0)
  | .a => (5.0, 25.0)
  | .f => (1.5, 5.0)
  | .g => (0.6, 1.5)
  | .k => (0.08, 0.6)
  | .m => (0.01, 0.08)

end StarClass

/-- Star properties used for generation. -/
structure Star where
  name : String
  kind : StarClass
  mass : Float
  radius : Float
  luminosity : Float
  deriving Repr, BEq, Inhabited

namespace Star

/-- Simple habitable zone estimation (AU). -/
def habitableZone (star : Star) : (Float × Float) :=
  let base := Float.sqrt star.luminosity
  (0.95 * base, 1.37 * base)

end Star

end GalaxyGen

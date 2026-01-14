/-
  GalaxyGen.Planet
  Planet types and properties.
-/

import GalaxyGen.Orbit

namespace GalaxyGen

/-- Basic planet classifications. -/
inductive PlanetClass
  | terrestrial
  | ocean
  | desert
  | ice
  | gasGiant
  | barren
  deriving Repr, BEq, DecidableEq

instance : Inhabited PlanetClass := ⟨.terrestrial⟩

namespace PlanetClass

/-- Short label for a planet class. -/
def label : PlanetClass → String
  | .terrestrial => "Terrestrial"
  | .ocean => "Ocean"
  | .desert => "Desert"
  | .ice => "Ice"
  | .gasGiant => "Gas Giant"
  | .barren => "Barren"

instance : ToString PlanetClass := ⟨label⟩

/-- Habitable classes for quick filtering. -/
def isHabitable : PlanetClass → Bool
  | .terrestrial => true
  | .ocean => true
  | _ => false

/-- Approximate size range in Earth radii. -/
def sizeRange : PlanetClass → (Float × Float)
  | .terrestrial => (0.7, 2.0)
  | .ocean => (0.8, 2.2)
  | .desert => (0.6, 1.8)
  | .ice => (0.4, 1.6)
  | .gasGiant => (3.0, 12.0)
  | .barren => (0.3, 1.2)

end PlanetClass

/-- Planet properties used by the generator. -/
structure Planet where
  id : Nat
  name : String
  kind : PlanetClass
  orbit : Orbit
  size : Float
  habitable : Bool
  deriving Repr, BEq, Inhabited

end GalaxyGen

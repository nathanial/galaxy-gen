/-
  GalaxyGen.Orbit
  Basic orbital parameters.
-/

namespace GalaxyGen

/-- Orbital parameters (units are in AU and radians where applicable). -/
structure Orbit where
  semiMajorAxis : Float
  eccentricity : Float
  inclination : Float
  phase : Float
  deriving Repr, BEq, Inhabited

namespace Orbit

/-- Approximate orbital period in Earth years using Kepler's third law. -/
def period (orbit : Orbit) (starMass : Float := 1.0) : Float :=
  let a3 := orbit.semiMajorAxis * orbit.semiMajorAxis * orbit.semiMajorAxis
  Float.sqrt (a3 / starMass)

end Orbit

end GalaxyGen

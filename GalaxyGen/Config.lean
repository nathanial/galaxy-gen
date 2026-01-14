/-
  GalaxyGen.Config
  Parameters for procedural galaxy generation.
-/

import GalaxyGen.Shape

namespace GalaxyGen

/-- Parameters for procedural galaxy generation. -/
structure GalaxyConfig where
  seed : UInt64 := 1
  systemCount : Nat := 200
  radius : Float := 1000.0
  thickness : Float := 60.0
  minSpacing : Float := 30.0
  placementAttempts : Nat := 25
  densityAttempts : Nat := 50
  minPlanets : Nat := 2
  maxPlanets : Nat := 9
  minOrbitAU : Float := 0.3
  orbitStepRange : (Float × Float) := (0.25, 0.75)
  hyperlaneNeighbors : Nat := 3
  shape : GalaxyShapeConfig := {}
  deriving Repr, Inhabited

namespace GalaxyConfig

/-- Update the seed. -/
def withSeed (seed : UInt64) (config : GalaxyConfig) : GalaxyConfig :=
  { config with seed := seed }

/-- Update the system count. -/
def withSystemCount (count : Nat) (config : GalaxyConfig) : GalaxyConfig :=
  { config with systemCount := count }

/-- Update the planet count range. -/
def withPlanetRange (minPlanets maxPlanets : Nat) (config : GalaxyConfig) : GalaxyConfig :=
  { config with minPlanets := minPlanets, maxPlanets := maxPlanets }

/-- Update the hyperlane neighbor count. -/
def withHyperlaneNeighbors (neighbors : Nat) (config : GalaxyConfig) : GalaxyConfig :=
  { config with hyperlaneNeighbors := neighbors }

def withShape (shape : GalaxyShapeConfig) (config : GalaxyConfig) : GalaxyConfig :=
  { config with shape := shape }

end GalaxyConfig

end GalaxyGen

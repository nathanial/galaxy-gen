/-
  GalaxyGen.Generate
  Procedural galaxy generation entry points.
-/

import Std.Data.HashSet
import Linalg
import GalaxyGen.Config
import GalaxyGen.Rng
import GalaxyGen.NameGen
import GalaxyGen.Orbit
import GalaxyGen.Star
import GalaxyGen.Planet
import GalaxyGen.System
import GalaxyGen.Galaxy

namespace GalaxyGen

open Linalg

private def randomPointInDisc (radius thickness : Float) : Gen Vec3 := do
  let angle ← Gen.nextFloatRange 0.0 Float.twoPi
  let u ← Gen.nextFloat
  let r := Float.sqrt u * radius
  let x := r * Float.cos angle
  let y := r * Float.sin angle
  let z ← Gen.nextFloatRange (-thickness / 2.0) (thickness / 2.0)
  return Vec3.mk x y z

private def isFarEnough (positions : Array Vec3) (candidate : Vec3) (minDistSq : Float) : Bool :=
  positions.all (fun pos => Vec3.distanceSquared pos candidate >= minDistSq)

private def findPosition (positions : Array Vec3) (config : GalaxyConfig) (attempts : Nat) : Gen Vec3 := do
  let candidate ← randomPointInDisc config.radius config.thickness
  if isFarEnough positions candidate (config.minSpacing * config.minSpacing) then
    return candidate
  match attempts with
  | 0 => return candidate
  | n + 1 => findPosition positions config n

private def placeSystems (config : GalaxyConfig) : Gen (Array Vec3) := do
  let mut positions : Array Vec3 := #[]
  for _ in [0:config.systemCount] do
    let pos ← findPosition positions config config.placementAttempts
    positions := positions.push pos
  return positions

private def generateStar (name : String) : Gen Star := do
  let starClass ← Gen.chooseWeighted StarClass.weights
  let (minMass, maxMass) := StarClass.massRange starClass
  let (minRadius, maxRadius) := StarClass.radiusRange starClass
  let (minLum, maxLum) := StarClass.luminosityRange starClass
  let mass ← Gen.nextFloatRange minMass maxMass
  let radius ← Gen.nextFloatRange minRadius maxRadius
  let luminosity ← Gen.nextFloatRange minLum maxLum
  return { name := name, kind := starClass, mass := mass, radius := radius, luminosity := luminosity }

private def generateOrbit (semiMajorAxis : Float) : Gen Orbit := do
  let eccentricity ← Gen.nextFloatRange 0.0 0.25
  let inclination ← Gen.nextFloatRange (-0.1) 0.1
  let phase ← Gen.nextFloatRange 0.0 Float.twoPi
  return {
    semiMajorAxis := semiMajorAxis,
    eccentricity := eccentricity,
    inclination := inclination,
    phase := phase
  }

private def innerWeights : Array (PlanetClass × Float) := #[(.barren, 0.4), (.desert, 0.35),
  (.terrestrial, 0.15), (.gasGiant, 0.1)]

private def habitableWeights : Array (PlanetClass × Float) := #[(.terrestrial, 0.45), (.ocean, 0.3),
  (.desert, 0.15), (.barren, 0.1)]

private def outerWeights : Array (PlanetClass × Float) := #[(.ice, 0.5), (.gasGiant, 0.35),
  (.barren, 0.15)]

private def farWeights : Array (PlanetClass × Float) := #[(.gasGiant, 0.7), (.ice, 0.2), (.barren, 0.1)]

private def choosePlanetClass (orbit : Orbit) (zoneInner zoneOuter : Float) : Gen PlanetClass := do
  let r := orbit.semiMajorAxis
  if r < zoneInner then
    Gen.chooseWeighted innerWeights
  else if r <= zoneOuter then
    Gen.chooseWeighted habitableWeights
  else if r <= zoneOuter * 2.5 then
    Gen.chooseWeighted outerWeights
  else
    Gen.chooseWeighted farWeights

private def generatePlanets (star : Star) (count : Nat) (config : GalaxyConfig) : Gen (Array Planet) := do
  let (zoneInner, zoneOuter) := Star.habitableZone star
  let mut planets : Array Planet := #[]
  let mut current := config.minOrbitAU
  let (stepMin, stepMax) := config.orbitStepRange
  for i in [0:count] do
    let step ← Gen.nextFloatRange stepMin stepMax
    current := current + step
    let orbit ← generateOrbit current
    let planetClass ← choosePlanetClass orbit zoneInner zoneOuter
    let (sizeMin, sizeMax) := PlanetClass.sizeRange planetClass
    let size ← Gen.nextFloatRange sizeMin sizeMax
    let habitable := PlanetClass.isHabitable planetClass &&
      orbit.semiMajorAxis >= zoneInner &&
      orbit.semiMajorAxis <= zoneOuter
    let name ← NameGen.planetName star.name (i + 1)
    let planet : Planet := {
      id := i,
      name := name,
      kind := planetClass,
      orbit := orbit,
      size := size,
      habitable := habitable
    }
    planets := planets.push planet
  return planets

private def generateSystem (id : Nat) (position : Vec3) (config : GalaxyConfig) : Gen StarSystem := do
  let name ← NameGen.starName
  let star ← generateStar name
  let planetCount ← Gen.nextNatRange config.minPlanets config.maxPlanets
  let planets ← generatePlanets star planetCount config
  return {
    id := id,
    name := name,
    position := position,
    star := star,
    planets := planets
  }

private def nearestNeighbors (systems : Array StarSystem) (index : Nat) (k : Nat) : Array Nat :=
  if k == 0 then
    #[]
  else
    Id.run do
      let origin := systems[index]!.position
      let mut distances : Array (Nat × Float) := #[]
      for j in [0:systems.size] do
        if j != index then
          let dist := Vec3.distanceSquared origin systems[j]!.position
          distances := distances.push (j, dist)
      let sorted := distances.qsort (fun a b => a.2 < b.2)
      let picked := sorted.toList.take k
      return picked.toArray.map (fun item => item.1)

private def generateHyperlanes (systems : Array StarSystem) (neighbors : Nat) : Array Hyperlane := Id.run do
  if systems.size <= 1 || neighbors == 0 then
    return #[]
  let neighborCount := Nat.min neighbors (systems.size - 1)
  let mut edges : Std.HashSet Hyperlane := {}
  for i in [0:systems.size] do
    let nearest := nearestNeighbors systems i neighborCount
    for j in nearest do
      if i != j then
        edges := edges.insert (Hyperlane.ordered i j)
  return edges.toList.toArray

/-- Generate a galaxy using the generator monad. -/
def generateGalaxyM (config : GalaxyConfig) : Gen Galaxy := do
  let positions ← placeSystems config
  let mut systems : Array StarSystem := #[]
  for i in [0:positions.size] do
    let system ← generateSystem i positions[i]! config
    systems := systems.push system
  let hyperlanes := generateHyperlanes systems config.hyperlaneNeighbors
  return { systems := systems, hyperlanes := hyperlanes }

/-- Generate a galaxy from a config using the embedded seed. -/
def generateGalaxy (config : GalaxyConfig) : Galaxy :=
  Gen.evalWithSeed (generateGalaxyM config) config.seed

/-- Generate a galaxy from a config using a custom seed. -/
def generateGalaxyWithSeed (config : GalaxyConfig) (seed : UInt64) : Galaxy :=
  Gen.evalWithSeed (generateGalaxyM config) seed

end GalaxyGen

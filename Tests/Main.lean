/-
  GalaxyGen tests
-/

import GalaxyGen
import Crucible

open Crucible
open scoped Crucible
open GalaxyGen

namespace Tests

testSuite "GalaxyGen"

test "roman numerals" := do
  shouldBe (GalaxyGen.NameGen.romanNumeral 1) "I"
  shouldBe (GalaxyGen.NameGen.romanNumeral 4) "IV"
  shouldBe (GalaxyGen.NameGen.romanNumeral 9) "IX"
  shouldBe (GalaxyGen.NameGen.romanNumeral 12) "XII"

test "deterministic generation" := do
  let config :=
    ({} : GalaxyConfig)
      |> GalaxyConfig.withSeed 42
      |> GalaxyConfig.withSystemCount 32
      |> GalaxyConfig.withHyperlaneNeighbors 3
  let g1 := generateGalaxy config
  let g2 := generateGalaxy config
  shouldBe (g1 == g2) true

test "system and planet counts" := do
  let config :=
    ({} : GalaxyConfig)
      |> GalaxyConfig.withSeed 7
      |> GalaxyConfig.withSystemCount 20
      |> GalaxyConfig.withPlanetRange 1 6
  let galaxy := generateGalaxy config
  shouldBe galaxy.systems.size config.systemCount
  for sys in galaxy.systems do
    if sys.planets.size < config.minPlanets || sys.planets.size > config.maxPlanets then
      throw (IO.userError s!"system {sys.id} planet count out of range")

test "hyperlane endpoints" := do
  let config :=
    ({} : GalaxyConfig)
      |> GalaxyConfig.withSeed 99
      |> GalaxyConfig.withSystemCount 25
      |> GalaxyConfig.withHyperlaneNeighbors 2
  let galaxy := generateGalaxy config
  for lane in galaxy.hyperlanes do
    let ok :=
      lane.a < config.systemCount &&
      lane.b < config.systemCount &&
      lane.a != lane.b
    if !ok then
      throw (IO.userError s!"invalid hyperlane {lane}")

test "habitable class consistency" := do
  let config :=
    ({} : GalaxyConfig)
      |> GalaxyConfig.withSeed 123
      |> GalaxyConfig.withSystemCount 15
  let galaxy := generateGalaxy config
  for sys in galaxy.systems do
    for planet in sys.planets do
      if planet.habitable && !PlanetClass.isHabitable planet.kind then
        throw (IO.userError s!"invalid habitable flag for {planet.name}")

#generate_tests

end Tests

def main (args : List String) : IO UInt32 := do
  IO.println "GalaxyGen Tests"
  runAllSuitesFiltered args

/-
  GalaxyGen tests
-/

import GalaxyGen
import Crucible
import Totem

open Crucible
open scoped Crucible
open Linalg
open GalaxyGen

namespace Tests

testSuite "GalaxyGen"

test "roman numerals" := do
  shouldBe (GalaxyGen.NameGen.romanNumeral 1) "I"
  shouldBe (GalaxyGen.NameGen.romanNumeral 4) "IV"
  shouldBe (GalaxyGen.NameGen.romanNumeral 9) "IX"
  shouldBe (GalaxyGen.NameGen.romanNumeral 12) "XII"

test "density bounds" := do
  let shape : GalaxyShapeConfig := {}
  let outside := GalaxyShape.densityAt shape 100.0 50.0 (Vec3.mk 200.0 0.0 0.0)
  let inside := GalaxyShape.densityAt shape 100.0 50.0 (Vec3.mk 10.0 0.0 0.0)
  shouldBe outside 0.0
  let inRange := decide (inside >= 0.0) && decide (inside <= 1.0)
  shouldBe inRange true

test "ring density peaks away from center" := do
  let shape : GalaxyShapeConfig := {
    kind := .ring,
    ringInner := 0.4,
    ringOuter := 0.6,
    coreDensity := 0.0
  }
  let center := GalaxyShape.densityAt shape 100.0 50.0 (Vec3.mk 0.0 0.0 0.0)
  let onRing := GalaxyShape.densityAt shape 100.0 50.0 (Vec3.mk 50.0 0.0 0.0)
  shouldBe (decide (onRing > center)) true

test "ring region tags" := do
  let shape := Presets.ringworld
  let tags := Regions.tagsAt shape 100.0 50.0 (Vec3.mk 60.0 0.0 0.0)
  let hasRing := tags.any (fun tag =>
    match tag with
    | .ring => true
    | _ => false
  )
  shouldBe hasRing true

test "toml config parsing" := do
  let input := "seed = 123\nsystemCount = 12\nradius = 800.0\n\n" ++
    "[shape]\npreset = \"ring\"\nringInner = 0.5\nringOuter = 0.7\n"
  match Totem.parse input with
  | .error e => throw (IO.userError s!"parse error: {e}")
  | .ok table =>
      match GalaxyGen.Toml.configFromTable table with
      | .error e => throw (IO.userError s!"extract error: {e}")
      | .ok cfg =>
          shouldBe cfg.seed 123
          shouldBe cfg.systemCount 12
          shouldBe cfg.shape.kind .ring
          shouldBe cfg.shape.ringInner 0.5
          shouldBe cfg.shape.ringOuter 0.7

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

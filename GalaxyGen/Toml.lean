/-
  GalaxyGen.Toml
  Load galaxy configuration from TOML.
-/

import Totem
import GalaxyGen.Config
import GalaxyGen.Presets
import GalaxyGen.Shape

namespace GalaxyGen

namespace Toml

open Totem

private def kindFromString (value : String) : Option GalaxyShapeKind :=
  match value.toLower with
  | "disc" => some .disc
  | "spiral" => some .spiral
  | "barred" => some .barredSpiral
  | "barred_spiral" => some .barredSpiral
  | "barredspiral" => some .barredSpiral
  | "ring" => some .ring
  | "elliptical" => some .elliptical
  | "elliptic" => some .elliptical
  | "irregular" => some .irregular
  | _ => none

private def kindName : GalaxyShapeKind → String
  | .disc => "disc"
  | .spiral => "spiral"
  | .barredSpiral => "barred_spiral"
  | .ring => "ring"
  | .elliptical => "elliptical"
  | .irregular => "irregular"

private def getOrDefault [FromConfig α] (t : Table) (path : String) (default : α)
    : ExtractResult α :=
  t.getAsOrDefault path default

/-- Parse a GalaxyShapeConfig from a TOML table. -/
def shapeFromTable (t : Table) : ExtractResult GalaxyShapeConfig := do
  let presetName ← t.getAsOption "shape.preset"
  let base ← match presetName with
    | some name =>
        match Presets.shapeByName name with
        | some preset => pure preset
        | none => throw (.typeConversion "shape.preset" "known preset" name)
    | none => pure {}

  let kindStr ← getOrDefault t "shape.kind" (kindName base.kind)
  let kind ← match kindFromString kindStr with
    | some k => pure k
    | none => throw (.typeConversion "shape.kind" "GalaxyShapeKind" kindStr)

  let armCount ← getOrDefault t "shape.armCount" base.armCount
  let armTightness ← getOrDefault t "shape.armTightness" base.armTightness
  let armWidth ← getOrDefault t "shape.armWidth" base.armWidth
  let armJitter ← getOrDefault t "shape.armJitter" base.armJitter
  let armCrossLinks ← getOrDefault t "shape.armCrossLinks" base.armCrossLinks
  let barLength ← getOrDefault t "shape.barLength" base.barLength
  let barWidth ← getOrDefault t "shape.barWidth" base.barWidth
  let ringInner ← getOrDefault t "shape.ringInner" base.ringInner
  let ringOuter ← getOrDefault t "shape.ringOuter" base.ringOuter
  let ellipticity ← getOrDefault t "shape.ellipticity" base.ellipticity
  let coreRadius ← getOrDefault t "shape.coreRadius" base.coreRadius
  let coreDensity ← getOrDefault t "shape.coreDensity" base.coreDensity
  let radialFalloff ← getOrDefault t "shape.radialFalloff" base.radialFalloff
  let noiseScale ← getOrDefault t "shape.noiseScale" base.noiseScale
  let noiseStrength ← getOrDefault t "shape.noiseStrength" base.noiseStrength
  let nebulaScale ← getOrDefault t "shape.nebulaScale" base.nebulaScale
  let nebulaThreshold ← getOrDefault t "shape.nebulaThreshold" base.nebulaThreshold
  let voidScale ← getOrDefault t "shape.voidScale" base.voidScale
  let voidThreshold ← getOrDefault t "shape.voidThreshold" base.voidThreshold

  return {
    base with
    kind := kind,
    armCount := armCount,
    armTightness := armTightness,
    armWidth := armWidth,
    armJitter := armJitter,
    armCrossLinks := armCrossLinks,
    barLength := barLength,
    barWidth := barWidth,
    ringInner := ringInner,
    ringOuter := ringOuter,
    ellipticity := ellipticity,
    coreRadius := coreRadius,
    coreDensity := coreDensity,
    radialFalloff := radialFalloff,
    noiseScale := noiseScale,
    noiseStrength := noiseStrength,
    nebulaScale := nebulaScale,
    nebulaThreshold := nebulaThreshold,
    voidScale := voidScale,
    voidThreshold := voidThreshold
  }

/-- Parse a GalaxyConfig from a TOML table. -/
def configFromTable (t : Table) : ExtractResult GalaxyConfig := do
  let base : GalaxyConfig := {}
  let seedNat ← getOrDefault t "seed" base.seed.toNat
  let seed := UInt64.ofNat seedNat
  let systemCount ← getOrDefault t "systemCount" base.systemCount
  let radius ← getOrDefault t "radius" base.radius
  let thickness ← getOrDefault t "thickness" base.thickness
  let minSpacing ← getOrDefault t "minSpacing" base.minSpacing
  let placementAttempts ← getOrDefault t "placementAttempts" base.placementAttempts
  let densityAttempts ← getOrDefault t "densityAttempts" base.densityAttempts
  let minPlanets ← getOrDefault t "minPlanets" base.minPlanets
  let maxPlanets ← getOrDefault t "maxPlanets" base.maxPlanets
  let minOrbitAU ← getOrDefault t "minOrbitAU" base.minOrbitAU
  let hyperlaneNeighbors ← getOrDefault t "hyperlaneNeighbors" base.hyperlaneNeighbors
  let shape ← shapeFromTable t

  let stepMin ← getOrDefault t "orbitStepMin" base.orbitStepRange.1
  let stepMax ← getOrDefault t "orbitStepMax" base.orbitStepRange.2
  let stepArray : Option (Array Float) ← t.getAsOption "orbitStepRange"
  let (orbitMin, orbitMax) := match stepArray with
    | some arr =>
        if arr.size >= 2 then
          (arr[0]!, arr[1]!)
        else
          (stepMin, stepMax)
    | none => (stepMin, stepMax)

  return {
    base with
    seed := seed,
    systemCount := systemCount,
    radius := radius,
    thickness := thickness,
    minSpacing := minSpacing,
    placementAttempts := placementAttempts,
    densityAttempts := densityAttempts,
    minPlanets := minPlanets,
    maxPlanets := maxPlanets,
    minOrbitAU := minOrbitAU,
    orbitStepRange := (orbitMin, orbitMax),
    hyperlaneNeighbors := hyperlaneNeighbors,
    shape := shape
  }

/-- Load a GalaxyConfig from a TOML file. -/
def loadConfig (path : System.FilePath) : IO (Except String GalaxyConfig) := do
  match ← Totem.loadFile path with
  | .error e => return .error e
  | .ok table =>
      match configFromTable table with
      | .ok cfg => return .ok cfg
      | .error e => return .error (toString e)

/-- Load a GalaxyConfig from a TOML file with environment variables. -/
def loadConfigWithEnv (path : System.FilePath) : IO (Except String GalaxyConfig) := do
  match ← Totem.loadFileWithEnv path with
  | .error e => return .error e
  | .ok table =>
      match configFromTable table with
      | .ok cfg => return .ok cfg
      | .error e => return .error (toString e)

end Toml

end GalaxyGen

/-
  GalaxyGen.Presets
  Handy presets for common galaxy shapes.
-/

import GalaxyGen.Shape

namespace GalaxyGen

namespace Presets

/-- Four-armed spiral galaxy preset. -/
def spiral4 : GalaxyShapeConfig := {
  kind := .spiral,
  armCount := 4,
  armTightness := 4.2,
  armWidth := 0.32,
  armJitter := 0.14,
  coreRadius := 0.18,
  coreDensity := 0.65,
  radialFalloff := 1.2
}

/-- Barred spiral preset. -/
def barred : GalaxyShapeConfig := {
  kind := .barredSpiral,
  armCount := 4,
  armTightness := 4.0,
  armWidth := 0.33,
  barLength := 0.5,
  barWidth := 0.15,
  coreRadius := 0.16,
  coreDensity := 0.55,
  radialFalloff := 1.15
}

/-- Ring galaxy preset. -/
def ringworld : GalaxyShapeConfig := {
  kind := .ring,
  ringInner := 0.55,
  ringOuter := 0.78,
  coreDensity := 0.0,
  radialFalloff := 1.0
}

/-- Elliptical galaxy preset. -/
def elliptical : GalaxyShapeConfig := {
  kind := .elliptical,
  ellipticity := 0.45,
  coreRadius := 0.22,
  coreDensity := 0.8,
  radialFalloff := 1.4
}

/-- Flat disc preset. -/
def disc : GalaxyShapeConfig := {
  kind := .disc,
  coreRadius := 0.12,
  coreDensity := 0.5
}

/-- Irregular galaxy preset. -/
def irregular : GalaxyShapeConfig := {
  kind := .irregular,
  noiseStrength := 0.45,
  voidThreshold := 0.7,
  radialFalloff := 0.9
}

/-- Resolve a preset by name (case-insensitive). -/
def shapeByName (name : String) : Option GalaxyShapeConfig :=
  let key := name.toLower
  if key == "spiral4" || key == "spiral" then
    some spiral4
  else if key == "barred" || key == "barred_spiral" || key == "barredspiral" then
    some barred
  else if key == "ring" || key == "ringworld" then
    some ringworld
  else if key == "elliptical" || key == "elliptic" then
    some elliptical
  else if key == "disc" || key == "disk" then
    some disc
  else if key == "irregular" then
    some irregular
  else
    none

end Presets

end GalaxyGen

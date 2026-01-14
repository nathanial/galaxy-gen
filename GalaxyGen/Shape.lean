/-
  GalaxyGen.Shape
  Density functions for galaxy shapes.
-/

import Linalg

namespace GalaxyGen

/-- High-level galaxy shape category. -/
inductive GalaxyShapeKind
  | disc
  | spiral
  | barredSpiral
  | ring
  | elliptical
  | irregular
  deriving Repr, BEq, DecidableEq

instance : Inhabited GalaxyShapeKind := ⟨.spiral⟩

/-- Parameters that control the density profile of a galaxy. -/
structure GalaxyShapeConfig where
  kind : GalaxyShapeKind := .spiral
  armCount : Nat := 4
  armTightness : Float := 4.0
  armWidth : Float := 0.35
  armJitter : Float := 0.12
  armCrossLinks : Nat := 1
  barLength : Float := 0.4
  barWidth : Float := 0.12
  ringInner : Float := 0.55
  ringOuter : Float := 0.78
  ellipticity : Float := 0.3
  coreRadius : Float := 0.12
  coreDensity : Float := 0.6
  radialFalloff : Float := 1.25
  noiseScale : Float := 2.0
  noiseStrength : Float := 0.25
  nebulaScale : Float := 1.4
  nebulaThreshold : Float := 0.6
  voidScale : Float := 1.0
  voidThreshold : Float := 0.85
  deriving Repr, Inhabited

namespace GalaxyShape

open Linalg

private def clamp01 (x : Float) : Float :=
  Float.clamp x 0.0 1.0

private def radialDistance (radius : Float) (pos : Vec3) : Float :=
  let r2 := pos.x * pos.x + pos.y * pos.y
  Float.sqrt r2 / radius

private def verticalFalloff (thickness z : Float) : Float :=
  if thickness <= 0.0 then
    0.0
  else
    let half := thickness / 2.0
    clamp01 (1.0 - (Float.abs' z / half))

private def baseFalloff (shape : GalaxyShapeConfig) (rN : Float) : Float :=
  let t := clamp01 (1.0 - rN)
  Float.pow t shape.radialFalloff

private def coreBoost (shape : GalaxyShapeConfig) (rN : Float) : Float :=
  if shape.coreRadius <= 0.0 then
    0.0
  else
    let t := clamp01 (1.0 - rN / shape.coreRadius)
    shape.coreDensity * t

private def noiseValue (shape : GalaxyShapeConfig) (radius : Float) (pos : Vec3) : Float :=
  let nx := pos.x / radius * shape.noiseScale
  let ny := pos.y / radius * shape.noiseScale
  let n := Noise.fbmSimplex2D nx ny
  clamp01 (1.0 + n * shape.noiseStrength)

private def armDensity (shape : GalaxyShapeConfig) (theta rN : Float) (jitter : Float) : Float :=
  let armCount := Nat.max 1 shape.armCount
  let arms := armCount.toFloat
  let phase := theta * arms + rN * shape.armTightness * Float.twoPi + jitter
  let signal := (Float.cos phase + 1.0) / 2.0
  let width := Float.clamp shape.armWidth 0.05 1.0
  let exponent := 1.0 / width
  Float.pow signal exponent

private def ringDensity (shape : GalaxyShapeConfig) (rN : Float) : Float :=
  let inner := Float.clamp shape.ringInner 0.0 1.0
  let outer := Float.clamp shape.ringOuter inner 1.0
  let center := (inner + outer) / 2.0
  let halfWidth := Float.max 0.01 ((outer - inner) / 2.0)
  let d := Float.abs' (rN - center)
  clamp01 (1.0 - d / halfWidth)

private def barDensity (shape : GalaxyShapeConfig) (radius : Float) (pos : Vec3) (rN : Float) : Float :=
  let halfWidth := Float.max 0.01 (radius * shape.barWidth)
  let length := Float.max 0.01 (shape.barLength)
  let axis := clamp01 (1.0 - Float.abs' pos.y / halfWidth)
  let radial := clamp01 (1.0 - (rN / length))
  axis * radial

/-- Spiral arm index for a position. -/
def armIndexAt (shape : GalaxyShapeConfig) (radius : Float) (pos : Vec3) : Nat :=
  let arms := Nat.max 1 shape.armCount
  let rN := radialDistance radius pos
  let theta := Float.atan2 pos.y pos.x
  let t := (theta + Float.twoPi) / Float.twoPi
  let spiral := t + rN * shape.armTightness
  let idx := Float.floor (spiral * arms.toFloat)
  (idx.toUInt64.toNat) % arms

/-- Strength of spiral arm influence at a position. -/
def armWeightAt (shape : GalaxyShapeConfig) (radius : Float) (pos : Vec3) : Float :=
  let theta := Float.atan2 pos.y pos.x
  let rN := radialDistance radius pos
  let noise := noiseValue shape radius pos
  let jitter := (noise - 1.0) * shape.armJitter
  armDensity shape theta rN jitter

/-- Strength of bar influence at a position. -/
def barWeightAt (shape : GalaxyShapeConfig) (radius : Float) (pos : Vec3) : Float :=
  let rN := radialDistance radius pos
  barDensity shape radius pos rN

/-- Field used to place nebulae (0..1). -/
def nebulaField (shape : GalaxyShapeConfig) (radius : Float) (pos : Vec3) : Float :=
  let nx := pos.x / radius * shape.nebulaScale
  let ny := pos.y / radius * shape.nebulaScale
  let v := Noise.fbmWorley2D nx ny
  clamp01 (1.0 - v)

/-- Field used to place voids (0..1). -/
def voidField (shape : GalaxyShapeConfig) (radius : Float) (pos : Vec3) : Float :=
  let nx := pos.x / radius * shape.voidScale
  let ny := pos.y / radius * shape.voidScale
  let v := Noise.fbmWorley2D nx ny
  clamp01 v

/-- Evaluate the density [0, 1] for a position in the galaxy. -/
def densityAt (shape : GalaxyShapeConfig) (radius thickness : Float) (pos : Vec3) : Float :=
  let rN := radialDistance radius pos
  if rN > 1.0 then
    0.0
  else
    let zFalloff := verticalFalloff thickness pos.z
    if zFalloff <= 0.0 then
      0.0
    else
      let theta := Float.atan2 pos.y pos.x
      let base := clamp01 (baseFalloff shape rN + coreBoost shape rN)
      let noise := noiseValue shape radius pos
      let jitter := (noise - 1.0) * shape.armJitter
      let voidMask := if voidField shape radius pos > shape.voidThreshold then 0.0 else 1.0
      let shapeDensity :=
        match shape.kind with
        | .disc => base
        | .elliptical =>
            let stretch := Float.clamp shape.ellipticity 0.0 0.8
            let rx := pos.x / (radius * (1.0 - stretch))
            let ry := pos.y / (radius * (1.0 + stretch))
            let rE := Float.sqrt (rx * rx + ry * ry)
            baseFalloff shape rE
        | .ring =>
            base * ringDensity shape rN
        | .spiral =>
            base * (0.35 + 0.65 * armDensity shape theta rN jitter)
        | .barredSpiral =>
            let arms := base * (0.3 + 0.7 * armDensity shape theta rN jitter)
            let bar := barDensity shape radius pos rN
            clamp01 (arms + bar * 0.75)
        | .irregular =>
            base * noise
      clamp01 (shapeDensity * zFalloff * noise * voidMask)

end GalaxyShape

end GalaxyGen

/-
  GalaxyGen.Regions
  Region tags derived from the density field for gameplay hooks.
-/

import Linalg
import GalaxyGen.Shape

namespace GalaxyGen

/-- Tags describing large-scale galaxy regions. -/
inductive RegionTag
  | core
  | inner
  | outer
  | arm (index : Nat)
  | bar
  | ring
  | nebula
  | void
  deriving Repr, BEq, DecidableEq

instance : Inhabited RegionTag := ⟨.outer⟩

namespace Regions

open Linalg

private def radialDistance (radius : Float) (pos : Vec3) : Float :=
  let r2 := pos.x * pos.x + pos.y * pos.y
  Float.sqrt r2 / radius

/-- Classify region tags for a position. -/
def tagsAt (shape : GalaxyShapeConfig) (radius _thickness : Float) (pos : Vec3) : Array RegionTag := Id.run do
  let mut tags : Array RegionTag := #[]
  let rN := radialDistance radius pos

  if rN <= shape.coreRadius then
    tags := tags.push .core

  if rN <= 0.4 then
    tags := tags.push .inner
  else
    tags := tags.push .outer

  match shape.kind with
  | .ring =>
      if rN >= shape.ringInner && rN <= shape.ringOuter then
        tags := tags.push .ring
  | .spiral =>
      let armWeight := GalaxyShape.armWeightAt shape radius pos
      if armWeight > 0.6 then
        tags := tags.push (.arm (GalaxyShape.armIndexAt shape radius pos))
  | .barredSpiral =>
      let armWeight := GalaxyShape.armWeightAt shape radius pos
      if armWeight > 0.55 then
        tags := tags.push (.arm (GalaxyShape.armIndexAt shape radius pos))
      if GalaxyShape.barWeightAt shape radius pos > 0.5 then
        tags := tags.push .bar
  | _ =>
      pure ()

  let nebula := GalaxyShape.nebulaField shape radius pos
  if nebula >= shape.nebulaThreshold then
    tags := tags.push .nebula

  let voidField := GalaxyShape.voidField shape radius pos
  if voidField > shape.voidThreshold then
    tags := tags.push .void

  return tags

end Regions

end GalaxyGen

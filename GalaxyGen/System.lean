/-
  GalaxyGen.System
  Star system composition.
-/

import Linalg
import GalaxyGen.Star
import GalaxyGen.Planet
import GalaxyGen.Regions

namespace GalaxyGen

/-- A generated star system. -/
structure StarSystem where
  id : Nat
  name : String
  position : Linalg.Vec3
  star : Star
  planets : Array Planet
  tags : Array RegionTag
  deriving Repr, BEq, Inhabited

end GalaxyGen

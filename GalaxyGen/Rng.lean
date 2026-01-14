/-
  GalaxyGen.Rng
  Thin wrapper around Linalg's RNG with a StateM helper.
-/

import Std
import Linalg

namespace GalaxyGen

/-- RNG type alias (Linalg's xorshift RNG). -/
abbrev Rng := Linalg.Random.RNG

namespace Rng

/-- Create a new RNG from a seed. -/
def seed (s : UInt64) : Rng := Linalg.Random.RNG.seed s

/-- Generate the next random UInt64. -/
def nextUInt64 (rng : Rng) : UInt64 × Rng := Linalg.Random.RNG.nextUInt64 rng

/-- Generate a random Float in [0, 1). -/
def nextFloat (rng : Rng) : Float × Rng := Linalg.Random.RNG.nextFloat rng

/-- Generate a random Float in [lo, hi). -/
def nextFloatRange (rng : Rng) (lo hi : Float) : Float × Rng :=
  Linalg.Random.RNG.nextFloatRange rng lo hi

/-- Generate a random Nat in [0, bound). -/
def nextNat (rng : Rng) (bound : Nat) : Nat × Rng :=
  if bound == 0 then
    (0, rng)
  else
    let (u, rng') := nextUInt64 rng
    (u.toNat % bound, rng')

/-- Generate a random Nat in [lo, hi] (inclusive). -/
def nextNatRange (rng : Rng) (lo hi : Nat) : Nat × Rng :=
  if hi <= lo then
    (lo, rng)
  else
    let (n, rng') := nextNat rng (hi - lo + 1)
    (lo + n, rng')

/-- Generate a random Int in [lo, hi] (inclusive). -/
def nextIntRange (rng : Rng) (lo hi : Int) : Int × Rng :=
  if hi <= lo then
    (lo, rng)
  else
    let rangeNat := (hi - lo + 1).toNat
    let (n, rng') := nextNat rng rangeNat
    (lo + (Int.ofNat n), rng')

/-- Generate a random boolean. -/
def nextBool (rng : Rng) : Bool × Rng :=
  let (u, rng') := nextUInt64 rng
  (u &&& 1 == 1, rng')

/-- Generate a random boolean with a probability of true. -/
def nextBoolWeighted (rng : Rng) (p : Float) : Bool × Rng :=
  let (f, rng') := nextFloat rng
  (f < p, rng')

end Rng

/-- Generator monad for deterministic generation. -/
abbrev Gen := StateM Rng

namespace Gen

/-- Run a generator with a seed. -/
def runWithSeed (g : Gen α) (seed : UInt64) : α × Rng :=
  g.run (Rng.seed seed)

/-- Evaluate a generator with a seed. -/
def evalWithSeed (g : Gen α) (seed : UInt64) : α :=
  (g.run (Rng.seed seed)).1

/-- Generate the next random UInt64. -/
def nextUInt64 : Gen UInt64 := do
  let rng ← get
  let (v, rng') := Rng.nextUInt64 rng
  set rng'
  pure v

/-- Generate a random Float in [0, 1). -/
def nextFloat : Gen Float := do
  let rng ← get
  let (v, rng') := Rng.nextFloat rng
  set rng'
  pure v

/-- Generate a random Float in [lo, hi). -/
def nextFloatRange (lo hi : Float) : Gen Float := do
  let rng ← get
  let (v, rng') := Rng.nextFloatRange rng lo hi
  set rng'
  pure v

/-- Generate a random Nat in [0, bound). -/
def nextNat (bound : Nat) : Gen Nat := do
  let rng ← get
  let (n, rng') := Rng.nextNat rng bound
  set rng'
  pure n

/-- Generate a random Nat in [lo, hi] (inclusive). -/
def nextNatRange (lo hi : Nat) : Gen Nat := do
  let rng ← get
  let (n, rng') := Rng.nextNatRange rng lo hi
  set rng'
  pure n

/-- Generate a random Int in [lo, hi] (inclusive). -/
def nextIntRange (lo hi : Int) : Gen Int := do
  let rng ← get
  let (n, rng') := Rng.nextIntRange rng lo hi
  set rng'
  pure n

/-- Generate a random boolean. -/
def nextBool : Gen Bool := do
  let rng ← get
  let (b, rng') := Rng.nextBool rng
  set rng'
  pure b

/-- Generate a random boolean with a probability of true. -/
def nextBoolWeighted (p : Float) : Gen Bool := do
  let rng ← get
  let (b, rng') := Rng.nextBoolWeighted rng p
  set rng'
  pure b

/-- Choose a random element from a non-empty array.
    Returns default if the array is empty. -/
def choose [Inhabited α] (items : Array α) : Gen α := do
  if items.isEmpty then
    return default
  let idx ← nextNat items.size
  return items[idx]!

/-- Choose a weighted element from an array of (value, weight).
    Returns default if the array is empty or weights are non-positive. -/
def chooseWeighted [Inhabited α] (items : Array (α × Float)) : Gen α := do
  if items.isEmpty then
    return default
  let total := items.foldl (fun acc item =>
    let w := if item.2 < 0.0 then 0.0 else item.2
    acc + w
  ) 0.0
  if total <= 0.0 then
    return items[0]!.1
  let pick ← nextFloatRange 0.0 total
  let mut acc := 0.0
  for item in items do
    let w := if item.2 < 0.0 then 0.0 else item.2
    acc := acc + w
    if pick <= acc then
      return item.1
  return items[items.size - 1]!.1

end Gen

end GalaxyGen

# GalaxyGen

Lean 4 library for generating Stellaris-like galaxies: star systems, planets, and hyperlanes with deterministic seeds.

## Features

- Seeded generation for reproducible galaxies
- Star classification with simple physical ranges
- Planet orbits, classes, and habitable zone checks
- Hyperlane graph from nearest-neighbor connectivity
- Syllable-based name generator for stars and planets

## Installation

Add to your `lakefile.lean`:

```lean
require galaxyGen from git "https://github.com/nathanial/galaxy-gen" @ "main"
```

## Usage

```lean
import GalaxyGen

open GalaxyGen

let config : GalaxyConfig := { seed := 42, systemCount := 64, hyperlaneNeighbors := 3 }
let galaxy := generateGalaxy config
IO.println s!"systems: {galaxy.systems.size}, lanes: {galaxy.hyperlanes.size}"
```

## Tests

```bash
lake test
```

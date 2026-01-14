import Lake
open Lake DSL

package galaxyGen where
  version := v!"0.1.0"

require linalg from git "https://github.com/nathanial/linalg" @ "v0.0.2"
require crucible from git "https://github.com/nathanial/crucible" @ "v0.0.7"
require totem from "../totem"

@[default_target]
lean_lib GalaxyGen where
  roots := #[`GalaxyGen]

lean_lib Tests where
  roots := #[`Tests]

@[test_driver]
lean_exe galaxy_gen_tests where
  root := `Tests.Main

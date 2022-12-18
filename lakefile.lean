import Lake
open Lake DSL

package «lean-binding-generator» {
  -- add package configuration options here
}

lean_lib LeanBindingGenerator {
  -- add library configuration options here
}

@[default_target]
lean_exe «lean-binding-generator» {
  root := `Main
}

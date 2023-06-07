{ name = "node-os"
, dependencies =
  [ "arrays"
  , "bifunctors"
  , "console"
  , "control"
  , "datetime"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "foreign"
  , "foreign-object"
  , "functions"
  , "maybe"
  , "node-buffer"
  , "nullable"
  , "partial"
  , "posix-types"
  , "prelude"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, license = "MIT"
}

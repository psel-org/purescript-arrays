{ name = "arrays"
, dependencies =
      [ "bifunctors"
      , "control"
      , "foldable-traversable"
      , "maybe"
      , "nonempty"
      , "partial"
      , "prelude"
      , "st"
      , "tailrec"
      , "tuples"
      , "unfoldable"
      , "unsafe-coerce"
      ]
    # [ "console", "assert", "const", "effect", "orders" ]
, packages =
    https://raw.githubusercontent.com/psel-org/package-sets/main/src/el-0.14.5-20211116/packages.dhall
, backend = "psel"
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}

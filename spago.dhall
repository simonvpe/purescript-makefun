{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "makefun"
, dependencies =
    [ "aff"
    , "ansi"
    , "console"
    , "effect"
    , "filterable"
    , "node-child-process"
    , "node-fs"
    , "node-path"
    , "psci-support"
    , "yargs"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}

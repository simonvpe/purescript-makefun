{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "makefun"
, dependencies =
    [ "aff"
    , "console"
    , "effect"
    , "node-child-process"
    , "node-path"
    , "psci-support"
    , "yargs"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}

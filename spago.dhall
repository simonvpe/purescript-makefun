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
    , "crypto"
    , "effect"
    , "filterable"
    , "formatters"
    , "js-date"
    , "node-child-process"
    , "node-fs"
    , "node-path"
    , "parsing"
    , "psci-support"
    , "yargs"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}

{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "bucketchain-simple-api"
, license = "MIT"
, repository =
    "https://github.com/Bucketchain/purescript-bucketchain-simple-api"
, dependencies =
  [ "aff"
  , "arrays"
  , "bucketchain"
  , "control"
  , "effect"
  , "either"
  , "exceptions"
  , "foreign"
  , "foreign-object"
  , "freet"
  , "lists"
  , "maybe"
  , "media-types"
  , "nullable"
  , "parallel"
  , "prelude"
  , "simple-json"
  , "strings"
  , "tailrec"
  , "transformers"
  , "typelevel-prelude"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}

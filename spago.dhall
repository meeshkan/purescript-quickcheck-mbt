{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "quickcheck-mbt"
, repository = "https://github.com/meeshkan/purescript-quickcheck-mbt"
, license = "Apache-2.0"
, dependencies = [ "quickcheck" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}

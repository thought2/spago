let Registry = ./Registry.dhall
let TargetType = ./TargetType.dhall
let Map = Registry.Prelude.Map.Type

-- a compilation target, i.e. "how to build a bunch of files from the project"
let Target = {
      , description : Text
      -- A mapping between package names and SemVer ranges for them.
      , dependencies : Map Text Text
      -- Source globs for the local project to include in the compilation alongside its dependencies.
      , sources : List Text
      , targetType : TargetType
      -- The current version of this package
      , version : Text
      }

in Target

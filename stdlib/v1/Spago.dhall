let Registry = ./Registry.dhall
let Config = ./Config.dhall
let Index = ./Index.dhall
let Map = Registry.Prelude.Map.Type
let Package = ./Package.dhall
let Target = ./Target.dhall
let TargetType = ./TargetType.dhall
let AppTarget = ./AppTarget.dhall
let LibraryTarget = ./LibraryTarget.dhall
let PackageExtension = ./PackageExtension.dhall

-- we can provide some defaults (through the `::` operator, that we'll see later) so that users don't have to deal
-- with all the settings at once if they have a simple project
let configDefaults =
      {
      -- , packages = Index.RegistryPackages ([] : Map Text Package)
      , output = "output"
      , backend = None Text
      , license = "AGPL"
      }

let targetDefaults =
      {
      , description = ""
      , dependencies = (toMap {=} : Map Text Text)
      , sources = [] : List Text
      , version = "0.0.0"
      }

let TestTarget = AppTarget // { default = AppTarget.default // { entrypoint = "Test.Main" } }

-- let's make some helpers to easily declare overriden packages, we'll use these later
let mkPackageFromGitHub = \(githubOwner : Text) -> \(githubRepo : Text) -> \(ref : Text) ->
      Package.External (PackageExtension.RemotePkg
        { ref
        , repo = Registry.Repo.GitHub
            { subdir = None Text
            , githubOwner
            , githubRepo
            }
        })
let mkLocalPackage = \(location : Registry.Prelude.Location.Type) ->
      Package.External (PackageExtension.LocalPkg location)

let mkApp = \(app : AppTarget.Type) ->
      TargetType.AppTarget app

let mkTest = \(tests : AppTarget.Type) ->
      TargetType.AppTarget tests

let mkLib = \(lib : LibraryTarget.Type) ->
      TargetType.LibraryTarget lib

let Spago =
      { Repo = Registry.Repo
      , Prelude = Registry.Prelude
      , Dependencies = Map Text Text
      , Config = { Type = Config, default = configDefaults }
      , Target = { Type = Target, default = targetDefaults }
      , Package
      , TestTarget
      , AppTarget
      , LibraryTarget
      , TargetType
      , Index
      , mkPackageFromGitHub
      , mkLocalPackage
      , mkApp
      , mkTest
      , mkLib
      }

in Spago
{-

We call "Index" of packages the place where Spago looks for the packages to use during a build.
This could either be:
- a package set: that is, a list of package versions that are known to compile together.
  A set is compatible with a certain compiler version, and is a mapping from package names to `Address`es.
  Package sets published on the Registry will only contain an `Address` pointing at the Registry, but users
  can override packages with local and/or external ones.
- the Registry: this means any package version published on the Registry.
  This source can be extended with a mapping between package names and a list of `Address`es, so that
  users can import local packages or things from outside the registry in general.

-}
let Registry = ./Registry.dhall

let Package = ./Package.dhall

let Map = Registry.Prelude.Map.Type

let Index =
      < -- RegistryPackages : Packages |
      PackageSet : { compiler : Text, packages : Map Text Package }
      >

in  Index

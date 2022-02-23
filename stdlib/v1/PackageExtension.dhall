{-

We define here the type of packages that Spago can deal with, additionally to
the packages from the Registry

-}
let Registry = ./Registry.dhall

let PackageExtension =
      < RemotePkg : { repo : Registry.Repo, ref : Text }
      | LocalPkg : Registry.Prelude.Location.Type
      >

in  PackageExtension

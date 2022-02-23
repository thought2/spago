{-

Parametrize the Address type from the Registry, so we're able to add other types
of packages to the package set.

-}
let Registry = ./Registry.dhall

let Package = Registry.Address ./PackageExtension.dhall

in  Package

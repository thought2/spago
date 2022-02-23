let Registry = ./Registry.dhall
let Index = ./Index.dhall
let Map = Registry.Prelude.Map.Type
let Target = ./Target.dhall

let Config =
      {
      -- The source for the package versions listed in the `packages`
      , packages : Index
      -- Command for the alternate backend. Example values: `purerl`, `psgo`, etc.
      , backend : Optional Text
      -- Output folder where the compiler will put its results
      , output : Text
      , targets : Map Text Target
      -- The SPDX code for the license under which the code is released
      , license : Text
      }

in Config
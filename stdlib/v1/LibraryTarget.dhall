let Registry = ./Registry.dhall

-- a compilation target, i.e. "how to build a bunch of files from the project"
let LibraryTarget = {
      -- The git repo the package is published at
      , repository : Registry.Repo
      }


in  { Type = LibraryTarget
    , default = {=}
    }

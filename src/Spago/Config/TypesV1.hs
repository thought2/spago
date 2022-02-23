{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell    #-}
module Spago.Config.TypesV1 where

import Prelude

import qualified Dhall.TH
import Dhall.TH (HaskellType(..))

Dhall.TH.makeHaskellTypes
    [ SingleConstructor "Config" "Config" "./stdlib/v1/Config.dhall"
    , MultipleConstructors "Index" "./stdlib/v1/Index.dhall"
    , MultipleConstructors "Package" "./stdlib/v1/Package.dhall"
    , MultipleConstructors "PackageExtension" "./stdlib/v1/PackageExtension.dhall"
    , SingleConstructor "PackageKV" "PackageKV" "{ mapKey : Text, mapValue : ./stdlib/v1/Package.dhall }"
    , SingleConstructor "Dependency" "Dependency" "{ mapKey : Text, mapValue : Text }"
    , SingleConstructor "TargetKV" "TargetKV" "{ mapKey : Text, mapValue : ./stdlib/v1/Target.dhall }"
    , SingleConstructor "Target" "Target" "./stdlib/v1/Target.dhall"
    , MultipleConstructors "TargetType" "./stdlib/v1/TargetType.dhall"
    , MultipleConstructors "Repo" "(./stdlib/v1/Registry.dhall).Repo"
    , MultipleConstructors "Location" "(./stdlib/v1/Registry.dhall).Prelude.Location.Type"
    ]

deriving instance Show Config
deriving instance Show Index
deriving instance Show TargetKV
deriving instance Show Target
deriving instance Show TargetType
deriving instance Show PackageKV
deriving instance Show Package
deriving instance Show PackageExtension
deriving instance Show Dependency
deriving instance Show Repo
deriving instance Show Location

deriving instance Eq Config
deriving instance Eq Index
deriving instance Eq TargetKV
deriving instance Eq Target
deriving instance Eq TargetType
deriving instance Eq PackageKV
deriving instance Eq Package
deriving instance Eq PackageExtension
deriving instance Eq Dependency
deriving instance Eq Repo
deriving instance Eq Location
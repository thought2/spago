-- A majority of this code was copied from
-- - https://github.com/natefaubion/purescript-psa-utils
-- 
-- To fullfil license requirements
--   Copyright © Nathan Faubion
--   https://opensource.org/license/mit/
module Spago.Psa.Types
  ( ErrorCode
  , ModuleName
  , Filename
  , PsaOutputOptions
  , PsaResult
  , PsaError
  , PsaAnnotedError
  , PsaPath(..)
  , Position
  , Suggestion
  , Lines
  , psaResultCodec
  , psaErrorCodec
  , compareByLocation
  ) where

import Prelude

import Data.Codec.Argonaut.Record as CAR
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Compat as CACompat
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Tuple (Tuple(..))
import Spago.Core.Config as Core

type ErrorCode = String
type ModuleName = String
type Filename = String
type Lines = Array String

-- | Relative files paths from the cwd, tagged as either being part of the
-- | source files or library files of a project. The `Unknown` variant exists
-- | because some psc errors are inter-module and aren't reported with a
-- | canonical file.
data PsaPath
  = Unknown
  | Lib String
  | Src String

derive instance Eq PsaPath

derive instance Ord PsaPath

type PsaOutputOptions =
  { color :: Boolean
  , censorBuildWarnings :: Core.CensorBuildWarnings
  , censorCodes :: Set ErrorCode
  , filterCodes :: Set ErrorCode
  , statVerbosity :: Core.StatVerbosity
  , libraryDirs :: Array String
  , strict :: Boolean
  }

type PsaResult =
  { warnings :: Array PsaError
  , errors :: Array PsaError
  }

type PsaError =
  { moduleName :: Maybe ModuleName
  , errorCode :: ErrorCode
  , errorLink :: String
  , suggestion :: Maybe Suggestion
  , message :: String
  , filename :: Maybe Filename
  , position :: Maybe Position
  }

type PsaAnnotedError =
  { error :: PsaError
  , path :: PsaPath
  , source :: Maybe Lines
  , position :: Maybe Position
  , message :: String
  }

type Position =
  { startLine :: Int
  , startColumn :: Int
  , endLine :: Int
  , endColumn :: Int
  }

type Suggestion =
  { replacement :: String
  , replaceRange :: Maybe Position
  }

compareByLocation :: PsaAnnotedError -> PsaAnnotedError -> Ordering
compareByLocation err1 err2 =
  case compare err1.path err2.path of
    EQ ->
      case err1.position, err2.position of
        Nothing, Nothing -> EQ
        Nothing, _ -> LT
        _, Nothing -> GT
        Just a, Just b ->
          compare (Tuple a.startLine a.startColumn)
            (Tuple b.startLine b.startColumn)
    x -> x

psaResultCodec :: CA.JsonCodec PsaResult
psaResultCodec = CAR.object "PsaResult"
  { warnings: CA.array psaErrorCodec
  , errors: CA.array psaErrorCodec
  }

psaErrorCodec :: CA.JsonCodec PsaError
psaErrorCodec = CAR.object "PsaError"
  { moduleName: CACompat.maybe CA.string
  , errorCode: CA.string
  , errorLink: CA.string
  , message: CA.string
  , filename: CACompat.maybe CA.string
  , position: CACompat.maybe positionCodec
  , suggestion: CACompat.maybe suggestionCodec
  }

positionCodec :: CA.JsonCodec Position
positionCodec = CAR.object "Position"
  { startLine: CA.int
  , startColumn: CA.int
  , endLine: CA.int
  , endColumn: CA.int
  }

suggestionCodec :: CA.JsonCodec Suggestion
suggestionCodec = CAR.object "Suggestion"
  { replacement: CA.string
  , replaceRange: CACompat.maybe positionCodec
  }

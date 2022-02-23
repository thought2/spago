{-# LANGUAGE DuplicateRecordFields #-}
module Spago.Types (
  module Spago.Types,
  ConfigV1.Repo
) where

import           Spago.Prelude

import qualified Data.Versions  as Version

import qualified GHC.IO

import qualified Spago.Dhall as Dhall
import qualified Spago.Config.TypesV1 as ConfigV1

newtype PackageName = PackageName { packageName :: Text }
  deriving (Show, Read, Data)
  deriving newtype (Eq, Ord, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Dhall.FromDhall)

-- | Whether to force an action
data Force = Force | NoForce
  deriving (Eq)

data IncludeTransitive = IncludeTransitive | NoIncludeTransitive

newtype ModuleName = ModuleName { unModuleName :: Text }
  deriving newtype (Eq, FromJSON, FromJSONKey, Ord)
newtype TargetPath = TargetPath { unTargetPath :: Text }
newtype SourcePath = SourcePath { unSourcePath :: Text }
  deriving newtype (Eq, Ord, Show, Dhall.FromDhall)
newtype PursArg = PursArg { unPursArg :: Text }
  deriving newtype (Eq, Show)
newtype BackendArg = BackendArg { unBackendArg :: Text }
  deriving newtype (Eq)

data WithMain = WithMain | WithoutMain

data WithSrcMap = WithSrcMap | WithoutSrcMap

data CacheFlag = SkipCache | NewCache
  deriving (Eq)

data CheckModulesUnique = DoCheckModulesUnique | NoCheckModulesUnique

data JsonFlag = JsonOutputNo | JsonOutputYes

-- | A flag to skip patching the docs using @purescript-docs-search@.
data NoSearch = NoSearch | AddSearch
  deriving (Eq)

-- | Flag to open generated HTML documentation in browser
data OpenDocs = NoOpenDocs | DoOpenDocs
  deriving (Eq)

-- | Flag to disable the automatic use of `psa`
data UsePsa = UsePsa | NoPsa

-- | The output path that can be obtained via `ls`
data PathType
  = PathOutput
  | PathGlobalCache

-- | Only build deps and ignore project paths
data DepsOnly = DepsOnly | AllSources
  deriving (Eq)

data Watch = Watch | BuildOnce

-- | Flag to go through with the build step
--   or skip it, in the case of 'bundleApp' and 'bundleModule'.
data NoBuild = NoBuild | DoBuild

-- | Flag to skip the automatic installation of libraries on build
data NoInstall = NoInstall | DoInstall
  deriving Eq

-- Should we clear the screen on rebuild?
data ClearScreen = DoClear | NoClear
  deriving Eq

-- | Flag to allow files ignored via `.gitignore` to trigger a rebuild
data AllowIgnored = DoAllowIgnored | NoAllowIgnored

data ShowVersion = DoShowVersion | NoShowVersion


data BuildOptions = BuildOptions
  { shouldWatch    :: Watch
  , shouldClear    :: ClearScreen
  , allowIgnored   :: AllowIgnored
  , sourcePaths    :: [SourcePath]
  , withSourceMap  :: WithSrcMap
  , noInstall      :: NoInstall
  , pursArgs       :: [PursArg]
  , depsOnly       :: DepsOnly
  , beforeCommands :: [Text]
  , thenCommands   :: [Text]
  , elseCommands   :: [Text]
  }

defaultBuildOptions :: BuildOptions
defaultBuildOptions = BuildOptions
  { shouldClear = NoClear
  , shouldWatch = BuildOnce
  , allowIgnored = DoAllowIgnored
  , sourcePaths = []
  , withSourceMap = WithoutSrcMap
  , noInstall = DoInstall
  , depsOnly = AllSources
  , pursArgs = []
  , beforeCommands = []
  , thenCommands = []
  , elseCommands = []
  }

fromScriptOptions :: BuildOptions -> ScriptBuildOptions -> BuildOptions
fromScriptOptions opts ScriptBuildOptions{..} = opts
  { pursArgs = pursArgs
  , beforeCommands = beforeCommands
  , thenCommands = thenCommands
  , elseCommands = elseCommands
  }

-- TODO: Figure out how `Watch` would work for `spago script` and include it
data ScriptBuildOptions = ScriptBuildOptions
  { pursArgs       :: [PursArg]
  , beforeCommands :: [Text]
  , thenCommands   :: [Text]
  , elseCommands   :: [Text]
  } deriving (Eq, Generic, Show)

data PursCmd = PursCmd
  { purs :: Text
  , psa :: Maybe Text
  , compilerVersion :: Version.SemVer
  } deriving (Generic)

newtype Jobs = Jobs Int
newtype ConfigPath = ConfigPath Text
newtype GitCmd = GitCmd Text
newtype BowerCmd = BowerCmd Text

data GlobalCache = GlobalCache !GHC.IO.FilePath !(Maybe CacheFlag)

newtype ModuleGraph = ModuleGraph { unModuleGraph :: Map ModuleName ModuleGraphNode }
  deriving newtype (FromJSON)

data ModuleGraphNode = ModuleGraphNode
  { graphNodePath :: Text
  , graphNodeDepends :: [ModuleName]
  } deriving (Generic)

instance FromJSON ModuleGraphNode where
  parseJSON = withObject "ModuleGraphNode" $ \o ->
    ModuleGraphNode
      <$> o .: "path"
      <*> o .: "depends"

type Graph = Maybe ModuleGraph

-- | Spago configuration file type
data Config = Config
  { alternateBackend :: !(Maybe Text)
  , license :: !(Maybe Text)
  , packageSet :: !PackageSet
  , packagesIndex :: !PackagesIndex
  , outputFolder :: !Text
  , targets :: !(Map PackageName Target)
  } deriving (Show, Generic)

-- TODO: merge this type into the config once we also support the solver
data PackageSet = PackageSet
  { packagesDB             :: !(Map PackageName Package)
  , packagesMinPursVersion :: !(Maybe Version.SemVer)
  }
  deriving (Show, Generic)

data PackagesIndex = PackageSetIndex | RegistryIndex
  deriving (Show, Generic)

type Packages = Map PackageName Package

-- TODO: get a proper type for this range
type Range = Text

-- FIXME: rename to TargetData
data Target = Target
  { description :: !Text
  , targetDependencies :: !(Map PackageName Range)
  -- ^ list of dependency package names
  , targetSourcePaths :: !(Set SourcePath)
  , targetType :: !TargetType
  , targetVersion :: !Version.SemVer
  }
  deriving (Eq, Show, Generic)

type BuildTarget = (PackageName, Target)
type Targets = Map PackageName Target
type MaybeTargetName = Maybe PackageName

data TargetType
  = AppTarget { entrypoint :: !Text }
  | LibraryTarget { repository :: !ConfigV1.Repo }
  deriving (Eq, Show, Generic)

-- | A package-set package.
--   Matches the packages definition in Package.dhall from package-sets
data Package = Package
  { dependencies :: ![PackageName]   -- ^ list of dependency package names
  , location     :: !PackageLocation -- ^ info about where the package is located
  }
  deriving (Eq, Show, Generic)

data PackageLocation
  = Remote
      { repo :: !ConfigV1.Repo -- ^ the remote git repository
      , ref :: !Text          -- ^ version string (also functions as a git ref)
      }
  | Local
      { localPath :: !Text        -- ^ local path of the package
      }
  | Registry { registryPackageName :: !PackageName , registryVersion :: !Text }
  deriving (Eq, Show, Generic)

-- | This instance is to make `spago ls packages --json` work
instance ToJSON PackageLocation where
  toJSON Remote{..} = object
    [ "tag" .= ("Remote" :: Text)
    , "contents" .= repoToUrl repo
    ]
  toJSON Local{..} = object
    [ "tag" .= ("Local" :: Text)
    , "contents" .= localPath
    ]

-- TODO: we should get rid of this, it's broken
repoToUrl :: ConfigV1.Repo -> Text
repoToUrl = \case
  ConfigV1.Git subdir gitUrl -> gitUrl
  ConfigV1.GitHub owner repo subdir -> "https://github.com/" <> owner <> "/" <> repo <> ".git"
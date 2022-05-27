{-# LANGUAGE OverloadedLists #-}
module Spago.Config where

import           Spago.Prelude
import           Spago.Env

import qualified Data.Map              as Map
import qualified Data.Set              as Set
import qualified Data.Text             as Text
import qualified Data.Versions         as Version

import qualified Spago.Dhall           as Dhall
import qualified Spago.Messages        as Messages
import qualified Spago.PackageSet      as PackageSet
import qualified Spago.Templates       as Templates
import qualified Spago.Config.Legacy as Legacy
import qualified Spago.Config.TypesV1 as ConfigV1

fromV1 :: ConfigV1.Config -> Config
fromV1 c@ConfigV1.Config { packages } = Config{..}
  where
    alternateBackend = ConfigV1.backend c
    license = Just $ ConfigV1.license c
    outputFolder = ConfigV1.output c
    targets = Map.fromList $ map convertTargetKV $ ConfigV1.targets c

    convertTargetKV (ConfigV1.TargetKV targetNameString target) = (PackageName targetNameString, Target{..})
      where
        -- FIXME: parse version!
        -- version = ConfigV1.version target
        targetVersion = undefined
        description = ConfigV1.description target
        targetDependencies = Map.fromList
          $ map (\(ConfigV1.Dependency name range) -> (PackageName name, range))
          $ ConfigV1.dependencies target
        targetSourcePaths = Set.fromList $ map SourcePath $ ConfigV1.sources target
        targetType = case ConfigV1.targetType target of
          ConfigV1.AppTarget entrypoint -> AppTarget{..}
          ConfigV1.LibraryTarget repository -> LibraryTarget{..}

    (packageSet, packagesIndex) = case packages of
      ConfigV1.PackageSet compiler packages ->
        ( PackageSet (Map.fromList $ map convertPackageKV packages) (hush $ Version.semver compiler)
        , PackageSetIndex
        )
      -- ConfigV1.Registry packages -> error "RegistryIndex is not supported yet" -- FIXME!!

    convertPackageKV (ConfigV1.PackageKV packageNameString package) =
      let
        packageName = PackageName packageNameString
        convertedPackage = case package of
          ConfigV1.Registry v -> Package [] (Registry packageName v)
          ConfigV1.External extension -> case extension of
            --FIXME: figure out dependencies!!
            ConfigV1.RemotePkg ref repo -> Package [] (Remote repo ref)
      in (packageName, convertedPackage)


fromLegacy :: Legacy.Config -> Config
fromLegacy Legacy.Config{..} = Config{..}
  where
    packagesIndex = PackageSetIndex
    outputFolder = "output"
    license = case publishConfig of
      Left _ -> Nothing
      Right Legacy.PublishConfig{ publishLicense } -> Just publishLicense
    targets = Map.singleton (PackageName "app") Target{..}
      where
        targetVersion = Version.SemVer 0 0 0 [] Nothing
        description = ""
        targetDependencies = Map.fromList $ map (, "*") $ Set.toList dependencies
        targetSourcePaths = configSourcePaths
        targetType = case publishConfig of
          Left _ -> AppTarget { entrypoint = "Main" }
          Right Legacy.PublishConfig{ publishRepository }
            -> LibraryTarget { repository = ConfigV1.Git Nothing publishRepository }




-- | Default path for the Spago Config
defaultPath :: IsString t => t
defaultPath = "spago.dhall"

-- | Checks that the Spago config is there and readable
ensureConfig
  :: (HasLogFunc env, HasConfigPath env)
  => RIO env (Either Utf8Builder Config)
ensureConfig = do
  ConfigPath path <- view (the @ConfigPath)
  -- TODO: make path absolute?
  -- TODO use inputWithSettings to pass the parent directory
  let readConfigV1 = liftIO $ Dhall.input Dhall.auto path
  exists <- testfile path
  if not exists
    then pure $ Left $ display $ Messages.cannotFindConfig path
    else try readConfigV1 >>= \case
      Right config -> pure $ Right $ fromV1 config
      Left (err :: SomeException) -> do
        logDebug $ "Failed to parse new config format: " <> displayShow err
        try Legacy.parseConfig >>= \case
          Right config -> do
            PackageSet.ensureFrozen $ Text.unpack path
            pure $ Right $ fromLegacy config
          Left (err' :: Dhall.ReadError Void) -> pure $ Left $ displayShow err'

-- TODO: implement spago migrate, or spago init --migrate

-- TODO: add a config template for new format

-- | Copies over `spago.dhall` to set up a Spago project.
makeConfig
  :: (HasConfigPath env, HasLogFunc env)
  => Force -> Dhall.TemplateComments
  -> RIO env Config
makeConfig force comments = do
  ConfigPath path <- view (the @ConfigPath)
  when (force == NoForce) $ do
    hasSpagoDhall <- testfile path
    when hasSpagoDhall $ die [ display $ Messages.foundExistingProject path ]
  writeTextFile path $ Dhall.processComments comments Templates.spagoDhall
  Dhall.format path
  eitherConfig <- ensureConfig
  -- at last we return the new config
  case eitherConfig of
    Right c -> pure c
    Left err -> die [err]

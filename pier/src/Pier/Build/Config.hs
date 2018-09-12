{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
module Pier.Build.Config
    ( configRules
    , askConfig
    , Config(..)
    , Resolved(..)
    , resolvePackage
    ) where

import Control.Exception (throw)
import Control.Monad (void)
import Data.Maybe (fromMaybe)
import Data.Yaml
import Development.Shake
import Development.Shake.Classes
import Distribution.Package
import Distribution.Text (display)
import Distribution.Version
import GHC.Generics hiding (packageName)

import qualified Data.HashMap.Strict as HM

import Pier.Build.Package
import Pier.Build.Stackage
import Pier.Core.Artifact
import Pier.Core.Persistent

data PierYamlPath = PierYamlPath
    deriving (Show, Eq, Typeable, Generic)
instance Hashable PierYamlPath
instance Binary PierYamlPath
instance NFData PierYamlPath

type instance RuleResult PierYamlPath = FilePath

configRules :: FilePath -> Rules ()
configRules f = do
    void $ addOracle $ \PierYamlPath -> return f
    void $ addPersistent $ \PierYamlQ -> do
        path <- askOracle PierYamlPath
        need [path]
        yamlE <- liftIO $ decodeFileEither path
        either (liftIO . throw) return yamlE

-- TODO: rename; maybe ConfigSpec and ConfigEnv?  Or Config and Env?
data PierYaml = PierYaml
    { resolver :: PlanName
    , packages :: [FilePath]
    , extraDeps :: [PackageIdentifier]
    , systemGhc :: Bool
    , yamlGhcOptions :: [String]
    } deriving (Show, Eq, Typeable, Generic)
instance Hashable PierYaml
instance Binary PierYaml
instance NFData PierYaml

instance FromJSON PierYaml where
    parseJSON = withObject "PierYaml" $ \o -> do
        r <- o .: "resolver"
        pkgs <- o .:? "packages"
        ed <- o .:? "extra-deps"
        sysGhc <- o .:? "system-ghc"
        opts <- o .:? "ghc-options"
        return PierYaml
            { resolver = r
            , packages = fromMaybe [] pkgs
            , extraDeps = fromMaybe [] ed
            , systemGhc = fromMaybe False sysGhc
            , yamlGhcOptions = fromMaybe [] opts
            }

data PierYamlQ = PierYamlQ
    deriving (Eq, Typeable, Generic)
instance Hashable PierYamlQ
instance Binary PierYamlQ
instance NFData PierYamlQ

type instance RuleResult PierYamlQ = PierYaml

instance Show PierYamlQ where
    show _ = "Pier YAML configuration"

data Config = Config
    { plan :: BuildPlan
    , configExtraDeps :: HM.HashMap PackageName Version
    , localPackages :: HM.HashMap PackageName (Artifact, Version)
    , configGhc :: InstalledGhc
    , ghcOptions :: [String]
    } deriving Show

-- TODO: cache?
askConfig :: Action Config
askConfig = do
    yaml <- askPersistent PierYamlQ
    p <- askBuildPlan (resolver yaml)
    ghc <- askInstalledGhc p (if systemGhc yaml then SystemGhc else StackageGhc)
    -- TODO: don't parse local package defs twice.
    -- We do it again later so the full PackageDescription
    -- doesn't need to get saved in the cache.
    pkgDescs <- mapM (\f -> do
                        let a = externalFile f
                        pkg <- parseCabalFileInDir a
                        return (packageName pkg, (a, packageVersion pkg)))
                    $ packages yaml
    return Config
        { plan = p
        , configGhc = ghc
        , localPackages = HM.fromList pkgDescs
        , configExtraDeps = HM.fromList [ (packageName pkg, packageVersion pkg)
                                        | pkg <- extraDeps yaml
                                        ]
        , ghcOptions = yamlGhcOptions yaml
        }

data Resolved
    = Builtin PackageId
    | Hackage PackageId Flags
    -- TODO: flags for local packages as well
    | Local Artifact PackageId
    deriving (Show,Typeable,Eq,Generic)
instance Hashable Resolved
instance Binary Resolved
instance NFData Resolved

resolvePackage :: Config -> PackageName -> Resolved
resolvePackage conf n
    -- TODO: nicer syntax
    -- core packages can't be overridden.  (TODO: is this right?)
    | Just v <- HM.lookup n (corePackageVersions $ plan conf)
                = Builtin $ PackageIdentifier n v
    | Just (a, v) <- HM.lookup n (localPackages conf)
                = Local a $ PackageIdentifier n v
    -- Extra-deps override packages in the build plan:
    | Just v <- HM.lookup n (configExtraDeps conf)
                = Hackage (PackageIdentifier n v) HM.empty
    | Just p <- HM.lookup n (planPackages $ plan conf)
                = Hackage (PackageIdentifier n $ planPackageVersion p)
                          (planPackageFlags p)
    | otherwise = error $ "Couldn't find package " ++ show (display n)

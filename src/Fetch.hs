module Fetch where

import           Data.HashMap.Lazy             as M
import           Data.List                      ( intersect )
import           Data.Text                      ( splitOn
                                                , pack
                                                , unpack
                                                )
import           Data.Yaml
import           Lens.Micro.Aeson
import           System.Directory
import           System.FilePath
import           System.Posix.Files
import           System.Process.Typed
import           Universum


fetch :: String -> String -> IO ()
fetch workdir resolver = do
  packages <- getSnapshotPackages resolver
  deps     <- getProjectDependencies

  cleanup workdir -- not efficient, but works for now
  downloadPackages workdir $ intersect packages deps
  symlinkExtraPackages workdir

getSnapshotPackages :: String -> IO [String]
getSnapshotPackages resolver = do
  homePath <- getHomeDirectory
  let indexPath =
        homePath </> ".stack" </> "build-plan" </> resolver <.> "yaml"

  index :: Value <- decodeFileEither indexPath >>= either (fail . show) pure
  let packages = M.keys <$> index ^? key "packages" . _Object

  maybe (fail $ "Couldn't read packages from " <> indexPath)
        (pure . fmap unpack)
        packages

getProjectDependencies :: IO [String]
getProjectDependencies = do
  out <- readProcessStdout_ (proc "stack" ["ls", "dependencies"])
  return . fmap stripVersion . lines $ decodeUtf8 out
 where
  stripVersion :: Text -> String
  stripVersion package = takeWhile (/= ' ') $ unpack package

cleanup :: String -> IO ()
cleanup dest =
  ifM (doesDirectoryExist dest) (removePathForcibly dest) (pure ())

downloadPackages :: String -> [String] -> IO ()
downloadPackages dest packages =
  runProcess_ $ proc "stack" $ ["unpack"] <> packages <> ["--to", dest]

symlinkExtraPackages :: String -> IO ()
symlinkExtraPackages dest = do
  let downloadedPath = ".stack-work" </> "downloaded"
  ifM (doesDirectoryExist downloadedPath) (symlink downloadedPath) (pure ())
  createSymbolicLink ".." (dest </> "__package__")
 where
  symlink :: FilePath -> IO ()
  symlink path = do
    packages <- listDirectory path
    traverse_ (\p -> createSymbolicLink (".." </> path </> p) (dest </> p))
              packages

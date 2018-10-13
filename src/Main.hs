module Main where

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
import           System.Process.Typed
import           Universum

main :: IO ()
main = do
  resolver <- getResolver
  packages <- getSnapshotPackages resolver
  deps     <- getProjectDependencies

  let dest = ".hs-src-tool"

  cleanup dest -- not efficient, but works for now
  downloadPackages dest $ intersect packages deps
  symlinkExtraPackages dest


getResolver :: IO Text
getResolver = do
  cfg :: Value <- decodeFileEither "stack.yaml" >>= either (fail . show) pure
  let resolver = cfg ^? key "resolver" . _String
  maybe (fail "Couldn't read resolver from stack.yaml") pure resolver

getSnapshotPackages :: Text -> IO [Text]
getSnapshotPackages resolver = do
  homePath <- getHomeDirectory
  let indexPath =
        homePath </> ".stack" </> "build-plan" </> unpack resolver <.> "yaml"

  index :: Value <- decodeFileEither indexPath >>= either (fail . show) pure
  let packages = M.keys <$> index ^? key "packages" . _Object

  maybe (fail $ "Couldn't read packages from " <> indexPath) pure packages

getProjectDependencies :: IO [Text]
getProjectDependencies = do
  out <- readProcessStdout_ (proc "stack" ["ls", "dependencies"])
  return . fmap stripVersion . lines $ decodeUtf8 out
 where
  stripVersion :: Text -> Text
  stripVersion package = pack . takeWhile (/= ' ') $ unpack package

cleanup :: Text -> IO ()
cleanup dest = ifM (doesDirectoryExist $ unpack dest)
                   (removePathForcibly $ unpack dest)
                   (pure ())

downloadPackages :: Text -> [Text] -> IO ()
downloadPackages dest packages =
  runProcess_
    $  proc "stack"
    $  ["unpack"]
    <> fmap unpack packages
    <> ["--to", unpack dest]

symlinkExtraPackages :: Text -> IO ()
symlinkExtraPackages dest = do
  let downloadedPath = ".stack-work" </> "downloaded"
  ifM (doesDirectoryExist downloadedPath) (symlink downloadedPath) (pure ())
 where
  symlink :: FilePath -> IO ()
  symlink path = do
    packages <- listDirectory path
    traverse_
      (\p -> createDirectoryLink (".." </> path </> p) (unpack dest </> p))
      packages

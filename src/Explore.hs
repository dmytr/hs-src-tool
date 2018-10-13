module Explore where

import           System.Directory
import           System.FilePath
import           System.Process.Typed
import           Universum


explore :: String -> String -> String -> IO ()
explore workdir port ui = do
  packages <- ("--package" :) . intersperse "--package" <$> listPackages
  runProcess_
    $  proc "haskell-code-server"
    $  packages
    <> ["--port", port, "--js-path", ui]
 where

  listPackages :: IO [String]
  listPackages = do
    packages <- fmap (workdir </>) <$> listDirectory workdir
    filterM doesDirectoryExist packages

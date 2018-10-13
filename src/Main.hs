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

import           Fetch
import           Index
import           Explore


main :: IO ()
main = do
  args     <- getArgs
  resolver <- getResolver

  let workdir = ".hs-src-tool"

  when ("fetch" `elem` args) $ fetch workdir resolver
  when ("index" `elem` args) $ index workdir resolver
  when ("explore" `elem` args) $ do
    port <- findFlag "port" "port" args
    ui   <- findFlag "ui" "path to ui dir" args
    explore workdir port ui

findFlag :: String -> String -> [String] -> IO String
findFlag flag descr args =
  maybe (fail $ "Flag --" <> flag <> "=<" <> descr <> "> is required") pure
    $   drop (length flag + 3)
    <$> find (isPrefixOf $ "--" <> flag <> "=") args

getResolver :: IO String
getResolver = do
  cfg :: Value <- decodeFileEither "stack.yaml" >>= either (fail . show) pure
  let resolver = cfg ^? key "resolver" . _String
  maybe (fail "Couldn't read resolver from stack.yaml") (pure . unpack) resolver

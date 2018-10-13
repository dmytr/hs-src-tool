module Explore where

import           System.Process.Typed
import           Universum

explore :: String -> String -> String -> IO ()
explore workdir port ui = runProcess_ $ proc
  "haskell-code-server"
  ["--packages", workdir, "--port", port, "--js-path", ui]

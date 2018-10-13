module Index where

import           Data.Text                      ( unpack
                                                , strip
                                                )
import           System.Directory
import           System.FilePath
import           System.Process.Typed
import           Universum


index :: String -> String -> IO ()
index workdir resolver = do
  packages <- listPackages
  forM_ packages $ \p -> do
    putStrLn $ "Building and indexing package " <> p
    ifM
      (isStackPackage p)
      (catchAny (buildStackPackage p >> indexStackPackage p) (reportError p))
      (catchAny (buildCabalPackage p >> indexCabalPackage p) (reportError p))
 where
  reportError :: String -> SomeException -> IO ()
  reportError package _ =
    putStrLn $ "Failed to build or index package " <> package

  listPackages :: IO [String]
  listPackages = do
    packages <- fmap (workdir </>) <$> listDirectory workdir
    filterM doesDirectoryExist packages

  isStackPackage :: String -> IO Bool
  isStackPackage path = doesFileExist $ path </> "stack.yaml"

  buildCabalPackage :: String -> IO ()
  buildCabalPackage path = runProcess_ $ setWorkingDir path $ proc
    "stack"
    [ "--resolver"
    , resolver
    , "exec"
    , "--no-ghc-package-path"
    , "cabal"
    , "--"
    , "new-build"
    ]

  indexCabalPackage :: String -> IO ()
  indexCabalPackage path = do
    dist <- findDist path
    runProcess_ $ setWorkingDir path $ proc
      "stack"
      [ "--resolver"
      , resolver
      , "exec"
      , "--no-ghc-package-path"
      , "haskell-code-indexer"
      , "--"
      , "-p"
      , "."
      , "--dist"
      , dist
      ]

  buildStackPackage :: String -> IO ()
  buildStackPackage path = runProcess_ $ setWorkingDir path $ proc
    "stack"
    ["--resolver", resolver, "build"]

  indexStackPackage :: String -> IO ()
  indexStackPackage path = do
    (dist, _) <- readProcess_ $ proc "stack" ["path", "--dist-dir"]
    runProcess_ $ setWorkingDir path $ proc
      "stack"
      [ "--resolver"
      , resolver
      , "exec"
      , "--no-ghc-package-path"
      , "haskell-code-indexer"
      , "--"
      , "-p"
      , "."
      , "--dist"
      , unpack $ strip (decodeUtf8 dist)
      ]

  findDist :: String -> IO String
  findDist path =
    firstSubdir (path </> "dist-newstyle" </> "build")
      >>= firstSubdir
      >>= firstSubdir
      >>= pure
      .   drop (length path + 1)

  firstSubdir :: String -> IO String
  firstSubdir path = do
    dirs <- listDirectory path
    case dirs of
      []      -> fail $ "Directory " <> path <> "seems to be empty"
      sub : _ -> return $ path </> sub

  catchAny :: IO a -> (SomeException -> IO a) -> IO a
  catchAny = catch

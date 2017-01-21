{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (
    main
)   where

import ReadArgs
import System.FilePath
import System.Directory
import Data.Tagged

import Citeproc (run)

main :: IO ()
main = do
    (identifier :: String, filenames' :: [FilePath]) <- readArgs
    filenames <- case filenames' of
        [] -> do
            home <- getHomeDirectory
            pure [home </> "Dropbox" </> "General" </> "library" <.> "yaml"]
        xs -> pure xs
    run (Tagged @"identifier" identifier) $ Tagged @"filename" <$> filenames

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, OverloadedStrings, RecordWildCards #-}

module Main (
    main
)   where

import WithCli
import System.FilePath
import System.Directory
import Data.Tagged

import Citeproc (run)

data Options = Options
    { databases :: [FilePath]
    } deriving (Show, Generic, HasArguments)

main :: IO ()
main = withCli $ \identifier Options{..} -> do
    filenames <- case databases of
        [] -> do
            home <- getHomeDirectory
            pure [home </> "Dropbox" </> "General" </> "library" <.> "yaml"]
        xs -> pure xs
    run (Tagged @"identifier" identifier) $ Tagged @"filename" <$> filenames

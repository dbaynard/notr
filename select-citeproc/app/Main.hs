{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, OverloadedStrings, RecordWildCards #-}

module Main (
    main
)   where

import Options.Generic
import System.FilePath
import System.Directory
import Data.Tagged

import Citeproc (run)

data Arguments = Arguments
    { identifier :: String <?> "The identifier to search for"
    , databases :: [FilePath] <?> "Library files to search"
    } deriving (Show, Generic, ParseRecord)

main :: IO ()
main = do
    Arguments{..} <- getRecord "Extract entries from citeproc library.yaml"
    filenames <- case unHelpful databases of
        [] -> do
            home <- getHomeDirectory
            pure [home </> "Dropbox" </> "General" </> "library" <.> "yaml"]
        xs -> pure xs
    run (Tagged @"identifier" $ unHelpful identifier) $ Tagged @"filename" <$> filenames

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, OverloadedStrings, RecordWildCards #-}

module Main (
    main
)   where

import WithCli
import System.Environment
import Data.Tagged
import Data.Maybe

import Citeproc

data Options = Options
    { libraries :: [FilePath]
    , doi :: Bool
    , write :: Bool
    } deriving (Show, Generic, HasArguments)

mods :: [Modifier]
mods = [ AddShortOption "libraries" 'l'
       , AddShortOption "write" 'w'
       , AddShortOption "doi" 'd'
       ]

main :: IO ()
main = mods `withCliModified` \identifier Options{..} -> do
    filenames <- case libraries of
        [] -> fmap (fromMaybe []) . runMaybeT $ do
            lib_yaml <- MaybeT . lookupEnv $ "NOTR_LIBRARY_YAML"
            pure $ [Tagged @"filename" lib_yaml]
        xs -> pure $ Tagged @"filename" <$> xs
    let searchTerm = case doi of
            True -> Left (Tagged @"doi" identifier)
            False -> Right (Tagged @"identifier" identifier)
        fileDir = case write of
            True -> Just . Tagged @"write-to-file" $ "doi"
            False -> Nothing
    run searchTerm fileDir filenames

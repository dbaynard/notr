{-# LANGUAGE OverloadedStrings, ApplicativeDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Citeproc (
    run
)   where

import           System.Exit        (exitFailure)
import           System.IO          (stderr, hPutStrLn)
import qualified Data.ByteString.Char8 as BS
import           Control.Monad      (forM_)

import Data.Semigroup

import Control.Error
import Control.Monad.IO.Class

import Data.Tagged

import Data.Yaml
import Data.Yaml.Pretty
import Data.Text (pack)

import Citeproc.Auto

outputYaml :: [ReferencesElt] -> TopLevel
outputYaml topLevelReferences = TopLevel{..}

parseYaml :: forall m . MonadIO m => FilePath -> m TopLevel
parseYaml filename = do
        input <- liftIO . BS.readFile $ filename
        case decode input of
            Nothing -> fatal $ case (decode input :: Maybe Value) of
                Nothing -> "Invalid JSON file: "     ++ filename
                Just _  -> "Mismatched JSON value from file: " ++ filename
            Just r  -> return (r :: TopLevel)
    where
        fatal :: String -> m a
        fatal msg = liftIO $ do
            hPutStrLn stderr msg
            exitFailure

run :: MonadIO m
    => Either (Tagged "doi" String) (Tagged "identifier" String)
    -> [Tagged "filename" FilePath]
    -> m ()
run ident filenames =
        forM_ filenames $ \(Tagged f) -> runMaybeT $ do
            library <- parseYaml f
            match <- hoistMaybe . headMay . filter searchFilter . topLevelReferences $ library
            liftIO . BS.putStr . ("---\n" <>) . (<> "---\n") . encodePretty (orderingReferencesElt `setConfCompare` defConfig) . outputYaml $ [match]
    where
        searchFilter = case ident of
            Left (Tagged search)  -> maybe False (pack search ==) . getDOI
            Right (Tagged search) -> (pack search ==) . referencesEltId


